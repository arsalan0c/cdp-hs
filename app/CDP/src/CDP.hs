{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module CDP (module CDP) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
import Data.Functor.Identity
import Data.String
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import Data.Aeson.Types (Parser(..))
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))
import qualified Data.Aeson           as A
import qualified Network.HTTP.Simple as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets as WS
import Control.Concurrent
import qualified Text.Casing as C
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


import Utils

defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222)

hostPortToEndpoint :: (String, Int) -> Http.Request
hostPortToEndpoint (host, port) = Http.parseRequest_ . 
    ("GET " <>) . 
    mconcat $ [host, ":", show port, "/json"]

type ClientApp a = Session (MVar (Map.Map EventName Handler)) -> IO a

runClient :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort app = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi   <- getPageInfo endpoint
    eventsM <- newMVar Map.empty
    let (host, port, path) = parseUri . debuggerUrl $ pi
    
    WS.runClient host port path $ \conn -> do
        listenThread <- forkIO $ do
            WS.runClient host port path $ \conn -> forever $ do
                res <- WS.fromDataMessage <$> WS.receiveDataMessage conn


                let eventResponse = fromMaybe (error "could not parse event") (A.decode res)
                handler <- withMVar eventsM $ \evs -> do
                    let ev = erEvent eventResponse
                    pure . fromMaybe (putStrLn . const ("received event: " <> show ev)) . Map.lookup ev $ evs 
                
                let eventResponseResult = fromMaybe (error "event does not have params") (A.decode res)
                (\h -> h . errParams $ eventResponseResult) handler                    
        
        let session = MkSession eventsM conn listenThread
        result <- app session
        killThread listenThread
        pure result

data PageInfo = PageInfo
    { debuggerUrl :: String
    } deriving Show
instance FromJSON PageInfo where
    parseJSON = A.withObject "PageInfo" $ \v ->
        PageInfo <$> v .: "webSocketDebuggerUrl"

parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of
            (':':str)   -> read str
            _           -> 80
    pure (Uri.uriRegName auth, port, Uri.uriPath u)

getPageInfo :: Http.Request -> IO PageInfo
getPageInfo request = do
    response <- Http.httpLBS request
    let body = Http.getResponseBody response
    case A.decode body of
        Just mpis -> pure $ head . catMaybes $ mpis
        Nothing   -> error "getPageInfo: Parse error"


type Handler = EventReturn -> IO ()

updateEvents :: (Map.Map EventName Handler -> Map.Map EventName Handler) -> Session (MVar (Map.Map EventName Handler)) -> IO ()
updateEvents f = ($ pure . f) . modifyMVar_ . events

eventSubscribe :: EventName -> Handler -> Session (MVar (Map.Map EventName Handler)) -> IO ()
eventSubscribe ev newHandler session = updateEvents
    (Map.alter (const . Just $ newHandler) ev)
    session

eventUnsubscribe :: EventName -> Session (MVar (Map.Map EventName Handler)) -> IO ()
eventUnsubscribe ev = updateEvents (Map.delete ev)


data EventResponseResult = EventResponseResult { errEvent :: EventName, errParams :: EventReturn }

data EventResponse = EventResponse { erEvent :: EventName }
instance FromJSON (EventResponse) where
    parseJSON = A.withObject "EventResponse" $ \obj -> do
        erEvent <- obj .: "method"
        pure EventResponse{..}


data EventName = EventNameDOMAttributeModified | EventNameDOMAttributeRemoved | EventNameDOMCharacterDataModified | EventNameDOMChildNodeCountUpdated | EventNameDOMChildNodeInserted | EventNameDOMChildNodeRemoved | EventNameDOMDocumentUpdated | EventNameDOMSetChildNodes | EventNameLogEntryAdded | EventNameNetworkDataReceived | EventNameNetworkEventSourceMessageReceived | EventNameNetworkLoadingFailed | EventNameNetworkLoadingFinished | EventNameNetworkRequestServedFromCache | EventNameNetworkRequestWillBeSent | EventNameNetworkResponseReceived | EventNameNetworkWebSocketClosed | EventNameNetworkWebSocketCreated | EventNameNetworkWebSocketFrameError | EventNameNetworkWebSocketFrameReceived | EventNameNetworkWebSocketFrameSent | EventNameNetworkWebSocketHandshakeResponseReceived | EventNameNetworkWebSocketWillSendHandshakeRequest | EventNameNetworkWebTransportCreated | EventNameNetworkWebTransportConnectionEstablished | EventNameNetworkWebTransportClosed | EventNamePageDomContentEventFired | EventNamePageFileChooserOpened | EventNamePageFrameAttached | EventNamePageFrameDetached | EventNamePageFrameNavigated | EventNamePageInterstitialHidden | EventNamePageInterstitialShown | EventNamePageJavascriptDialogClosed | EventNamePageJavascriptDialogOpening | EventNamePageLifecycleEvent | EventNamePagePrerenderAttemptCompleted | EventNamePageLoadEventFired | EventNamePageWindowOpen | EventNamePerformanceMetrics | EventNameTargetReceivedMessageFromTarget | EventNameTargetTargetCreated | EventNameTargetTargetDestroyed | EventNameTargetTargetCrashed | EventNameTargetTargetInfoChanged | EventNameFetchRequestPaused | EventNameFetchAuthRequired | EventNameConsoleMessageAdded | EventNameDebuggerBreakpointResolved | EventNameDebuggerPaused | EventNameDebuggerResumed | EventNameDebuggerScriptFailedToParse | EventNameDebuggerScriptParsed | EventNameProfilerConsoleProfileFinished | EventNameProfilerConsoleProfileStarted | EventNameRuntimeConsoleApiCalled | EventNameRuntimeExceptionRevoked | EventNameRuntimeExceptionThrown | EventNameRuntimeExecutionContextCreated | EventNameRuntimeExecutionContextDestroyed | EventNameRuntimeExecutionContextsCleared | EventNameRuntimeInspectRequested
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON EventName where
    parseJSON = A.withText  "EventName"  $ \v -> do
        pure $ case v of
                "DOMAttributeModified" -> EventNameDOMAttributeModified
                "DOMAttributeRemoved" -> EventNameDOMAttributeRemoved
                "DOMCharacterDataModified" -> EventNameDOMCharacterDataModified
                "DOMChildNodeCountUpdated" -> EventNameDOMChildNodeCountUpdated
                "DOMChildNodeInserted" -> EventNameDOMChildNodeInserted
                "DOMChildNodeRemoved" -> EventNameDOMChildNodeRemoved
                "DOMDocumentUpdated" -> EventNameDOMDocumentUpdated
                "DOMSetChildNodes" -> EventNameDOMSetChildNodes
                "LogEntryAdded" -> EventNameLogEntryAdded
                "NetworkDataReceived" -> EventNameNetworkDataReceived
                "NetworkEventSourceMessageReceived" -> EventNameNetworkEventSourceMessageReceived
                "NetworkLoadingFailed" -> EventNameNetworkLoadingFailed
                "NetworkLoadingFinished" -> EventNameNetworkLoadingFinished
                "NetworkRequestServedFromCache" -> EventNameNetworkRequestServedFromCache
                "NetworkRequestWillBeSent" -> EventNameNetworkRequestWillBeSent
                "NetworkResponseReceived" -> EventNameNetworkResponseReceived
                "NetworkWebSocketClosed" -> EventNameNetworkWebSocketClosed
                "NetworkWebSocketCreated" -> EventNameNetworkWebSocketCreated
                "NetworkWebSocketFrameError" -> EventNameNetworkWebSocketFrameError
                "NetworkWebSocketFrameReceived" -> EventNameNetworkWebSocketFrameReceived
                "NetworkWebSocketFrameSent" -> EventNameNetworkWebSocketFrameSent
                "NetworkWebSocketHandshakeResponseReceived" -> EventNameNetworkWebSocketHandshakeResponseReceived
                "NetworkWebSocketWillSendHandshakeRequest" -> EventNameNetworkWebSocketWillSendHandshakeRequest
                "NetworkWebTransportCreated" -> EventNameNetworkWebTransportCreated
                "NetworkWebTransportConnectionEstablished" -> EventNameNetworkWebTransportConnectionEstablished
                "NetworkWebTransportClosed" -> EventNameNetworkWebTransportClosed
                "PageDomContentEventFired" -> EventNamePageDomContentEventFired
                "PageFileChooserOpened" -> EventNamePageFileChooserOpened
                "PageFrameAttached" -> EventNamePageFrameAttached
                "PageFrameDetached" -> EventNamePageFrameDetached
                "PageFrameNavigated" -> EventNamePageFrameNavigated
                "PageInterstitialHidden" -> EventNamePageInterstitialHidden
                "PageInterstitialShown" -> EventNamePageInterstitialShown
                "PageJavascriptDialogClosed" -> EventNamePageJavascriptDialogClosed
                "PageJavascriptDialogOpening" -> EventNamePageJavascriptDialogOpening
                "PageLifecycleEvent" -> EventNamePageLifecycleEvent
                "PagePrerenderAttemptCompleted" -> EventNamePagePrerenderAttemptCompleted
                "PageLoadEventFired" -> EventNamePageLoadEventFired
                "PageWindowOpen" -> EventNamePageWindowOpen
                "PerformanceMetrics" -> EventNamePerformanceMetrics
                "TargetReceivedMessageFromTarget" -> EventNameTargetReceivedMessageFromTarget
                "TargetTargetCreated" -> EventNameTargetTargetCreated
                "TargetTargetDestroyed" -> EventNameTargetTargetDestroyed
                "TargetTargetCrashed" -> EventNameTargetTargetCrashed
                "TargetTargetInfoChanged" -> EventNameTargetTargetInfoChanged
                "FetchRequestPaused" -> EventNameFetchRequestPaused
                "FetchAuthRequired" -> EventNameFetchAuthRequired
                "ConsoleMessageAdded" -> EventNameConsoleMessageAdded
                "DebuggerBreakpointResolved" -> EventNameDebuggerBreakpointResolved
                "DebuggerPaused" -> EventNameDebuggerPaused
                "DebuggerResumed" -> EventNameDebuggerResumed
                "DebuggerScriptFailedToParse" -> EventNameDebuggerScriptFailedToParse
                "DebuggerScriptParsed" -> EventNameDebuggerScriptParsed
                "ProfilerConsoleProfileFinished" -> EventNameProfilerConsoleProfileFinished
                "ProfilerConsoleProfileStarted" -> EventNameProfilerConsoleProfileStarted
                "RuntimeConsoleApiCalled" -> EventNameRuntimeConsoleApiCalled
                "RuntimeExceptionRevoked" -> EventNameRuntimeExceptionRevoked
                "RuntimeExceptionThrown" -> EventNameRuntimeExceptionThrown
                "RuntimeExecutionContextCreated" -> EventNameRuntimeExecutionContextCreated
                "RuntimeExecutionContextDestroyed" -> EventNameRuntimeExecutionContextDestroyed
                "RuntimeExecutionContextsCleared" -> EventNameRuntimeExecutionContextsCleared
                "RuntimeInspectRequested" -> EventNameRuntimeInspectRequested
                _ -> error "failed to parse EventName"

data EventReturn = EventReturnDOMAttributeModified DOM.AttributeModified | EventReturnDOMAttributeRemoved DOM.AttributeRemoved | EventReturnDOMCharacterDataModified DOM.CharacterDataModified | EventReturnDOMChildNodeCountUpdated DOM.ChildNodeCountUpdated | EventReturnDOMChildNodeInserted DOM.ChildNodeInserted | EventReturnDOMChildNodeRemoved DOM.ChildNodeRemoved | EventReturnDOMDocumentUpdated DOM.DocumentUpdated | EventReturnDOMSetChildNodes DOM.SetChildNodes | EventReturnLogEntryAdded Log.EntryAdded | EventReturnNetworkDataReceived Network.DataReceived | EventReturnNetworkEventSourceMessageReceived Network.EventSourceMessageReceived | EventReturnNetworkLoadingFailed Network.LoadingFailed | EventReturnNetworkLoadingFinished Network.LoadingFinished | EventReturnNetworkRequestServedFromCache Network.RequestServedFromCache | EventReturnNetworkRequestWillBeSent Network.RequestWillBeSent | EventReturnNetworkResponseReceived Network.ResponseReceived | EventReturnNetworkWebSocketClosed Network.WebSocketClosed | EventReturnNetworkWebSocketCreated Network.WebSocketCreated | EventReturnNetworkWebSocketFrameError Network.WebSocketFrameError | EventReturnNetworkWebSocketFrameReceived Network.WebSocketFrameReceived | EventReturnNetworkWebSocketFrameSent Network.WebSocketFrameSent | EventReturnNetworkWebSocketHandshakeResponseReceived Network.WebSocketHandshakeResponseReceived | EventReturnNetworkWebSocketWillSendHandshakeRequest Network.WebSocketWillSendHandshakeRequest | EventReturnNetworkWebTransportCreated Network.WebTransportCreated | EventReturnNetworkWebTransportConnectionEstablished Network.WebTransportConnectionEstablished | EventReturnNetworkWebTransportClosed Network.WebTransportClosed | EventReturnPageDomContentEventFired Page.DomContentEventFired | EventReturnPageFileChooserOpened Page.FileChooserOpened | EventReturnPageFrameAttached Page.FrameAttached | EventReturnPageFrameDetached Page.FrameDetached | EventReturnPageFrameNavigated Page.FrameNavigated | EventReturnPageInterstitialHidden Page.InterstitialHidden | EventReturnPageInterstitialShown Page.InterstitialShown | EventReturnPageJavascriptDialogClosed Page.JavascriptDialogClosed | EventReturnPageJavascriptDialogOpening Page.JavascriptDialogOpening | EventReturnPageLifecycleEvent Page.LifecycleEvent | EventReturnPagePrerenderAttemptCompleted Page.PrerenderAttemptCompleted | EventReturnPageLoadEventFired Page.LoadEventFired | EventReturnPageWindowOpen Page.WindowOpen | EventReturnPerformanceMetrics Performance.Metrics | EventReturnTargetReceivedMessageFromTarget Target.ReceivedMessageFromTarget | EventReturnTargetTargetCreated Target.TargetCreated | EventReturnTargetTargetDestroyed Target.TargetDestroyed | EventReturnTargetTargetCrashed Target.TargetCrashed | EventReturnTargetTargetInfoChanged Target.TargetInfoChanged | EventReturnFetchRequestPaused Fetch.RequestPaused | EventReturnFetchAuthRequired Fetch.AuthRequired | EventReturnConsoleMessageAdded Console.MessageAdded | EventReturnDebuggerBreakpointResolved Debugger.BreakpointResolved | EventReturnDebuggerPaused Debugger.Paused | EventReturnDebuggerResumed Debugger.Resumed | EventReturnDebuggerScriptFailedToParse Debugger.ScriptFailedToParse | EventReturnDebuggerScriptParsed Debugger.ScriptParsed | EventReturnProfilerConsoleProfileFinished Profiler.ConsoleProfileFinished | EventReturnProfilerConsoleProfileStarted Profiler.ConsoleProfileStarted | EventReturnRuntimeConsoleApiCalled Runtime.ConsoleApiCalled | EventReturnRuntimeExceptionRevoked Runtime.ExceptionRevoked | EventReturnRuntimeExceptionThrown Runtime.ExceptionThrown | EventReturnRuntimeExecutionContextCreated Runtime.ExecutionContextCreated | EventReturnRuntimeExecutionContextDestroyed Runtime.ExecutionContextDestroyed | EventReturnRuntimeExecutionContextsCleared Runtime.ExecutionContextsCleared | EventReturnRuntimeInspectRequested Runtime.InspectRequested
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON EventResponseResult where
    parseJSON = A.withObject  "EventResponseResult"  $ \obj -> do
        errEvent <- obj .: "method"
        evn <- methodToName <$> obj .: "method"
        errParams <- case evn of
                "DOMAttributeModified" -> EventReturnDOMAttributeModified <$> obj .: "params"
                "DOMAttributeRemoved" -> EventReturnDOMAttributeRemoved <$> obj .: "params"
                "DOMCharacterDataModified" -> EventReturnDOMCharacterDataModified <$> obj .: "params"
                "DOMChildNodeCountUpdated" -> EventReturnDOMChildNodeCountUpdated <$> obj .: "params"
                "DOMChildNodeInserted" -> EventReturnDOMChildNodeInserted <$> obj .: "params"
                "DOMChildNodeRemoved" -> EventReturnDOMChildNodeRemoved <$> obj .: "params"
                "DOMDocumentUpdated" -> EventReturnDOMDocumentUpdated <$> obj .: "params"
                "DOMSetChildNodes" -> EventReturnDOMSetChildNodes <$> obj .: "params"
                "LogEntryAdded" -> EventReturnLogEntryAdded <$> obj .: "params"
                "NetworkDataReceived" -> EventReturnNetworkDataReceived <$> obj .: "params"
                "NetworkEventSourceMessageReceived" -> EventReturnNetworkEventSourceMessageReceived <$> obj .: "params"
                "NetworkLoadingFailed" -> EventReturnNetworkLoadingFailed <$> obj .: "params"
                "NetworkLoadingFinished" -> EventReturnNetworkLoadingFinished <$> obj .: "params"
                "NetworkRequestServedFromCache" -> EventReturnNetworkRequestServedFromCache <$> obj .: "params"
                "NetworkRequestWillBeSent" -> EventReturnNetworkRequestWillBeSent <$> obj .: "params"
                "NetworkResponseReceived" -> EventReturnNetworkResponseReceived <$> obj .: "params"
                "NetworkWebSocketClosed" -> EventReturnNetworkWebSocketClosed <$> obj .: "params"
                "NetworkWebSocketCreated" -> EventReturnNetworkWebSocketCreated <$> obj .: "params"
                "NetworkWebSocketFrameError" -> EventReturnNetworkWebSocketFrameError <$> obj .: "params"
                "NetworkWebSocketFrameReceived" -> EventReturnNetworkWebSocketFrameReceived <$> obj .: "params"
                "NetworkWebSocketFrameSent" -> EventReturnNetworkWebSocketFrameSent <$> obj .: "params"
                "NetworkWebSocketHandshakeResponseReceived" -> EventReturnNetworkWebSocketHandshakeResponseReceived <$> obj .: "params"
                "NetworkWebSocketWillSendHandshakeRequest" -> EventReturnNetworkWebSocketWillSendHandshakeRequest <$> obj .: "params"
                "NetworkWebTransportCreated" -> EventReturnNetworkWebTransportCreated <$> obj .: "params"
                "NetworkWebTransportConnectionEstablished" -> EventReturnNetworkWebTransportConnectionEstablished <$> obj .: "params"
                "NetworkWebTransportClosed" -> EventReturnNetworkWebTransportClosed <$> obj .: "params"
                "PageDomContentEventFired" -> EventReturnPageDomContentEventFired <$> obj .: "params"
                "PageFileChooserOpened" -> EventReturnPageFileChooserOpened <$> obj .: "params"
                "PageFrameAttached" -> EventReturnPageFrameAttached <$> obj .: "params"
                "PageFrameDetached" -> EventReturnPageFrameDetached <$> obj .: "params"
                "PageFrameNavigated" -> EventReturnPageFrameNavigated <$> obj .: "params"
                "PageInterstitialHidden" -> EventReturnPageInterstitialHidden <$> obj .: "params"
                "PageInterstitialShown" -> EventReturnPageInterstitialShown <$> obj .: "params"
                "PageJavascriptDialogClosed" -> EventReturnPageJavascriptDialogClosed <$> obj .: "params"
                "PageJavascriptDialogOpening" -> EventReturnPageJavascriptDialogOpening <$> obj .: "params"
                "PageLifecycleEvent" -> EventReturnPageLifecycleEvent <$> obj .: "params"
                "PagePrerenderAttemptCompleted" -> EventReturnPagePrerenderAttemptCompleted <$> obj .: "params"
                "PageLoadEventFired" -> EventReturnPageLoadEventFired <$> obj .: "params"
                "PageWindowOpen" -> EventReturnPageWindowOpen <$> obj .: "params"
                "PerformanceMetrics" -> EventReturnPerformanceMetrics <$> obj .: "params"
                "TargetReceivedMessageFromTarget" -> EventReturnTargetReceivedMessageFromTarget <$> obj .: "params"
                "TargetTargetCreated" -> EventReturnTargetTargetCreated <$> obj .: "params"
                "TargetTargetDestroyed" -> EventReturnTargetTargetDestroyed <$> obj .: "params"
                "TargetTargetCrashed" -> EventReturnTargetTargetCrashed <$> obj .: "params"
                "TargetTargetInfoChanged" -> EventReturnTargetTargetInfoChanged <$> obj .: "params"
                "FetchRequestPaused" -> EventReturnFetchRequestPaused <$> obj .: "params"
                "FetchAuthRequired" -> EventReturnFetchAuthRequired <$> obj .: "params"
                "ConsoleMessageAdded" -> EventReturnConsoleMessageAdded <$> obj .: "params"
                "DebuggerBreakpointResolved" -> EventReturnDebuggerBreakpointResolved <$> obj .: "params"
                "DebuggerPaused" -> EventReturnDebuggerPaused <$> obj .: "params"
                "DebuggerResumed" -> EventReturnDebuggerResumed <$> obj .: "params"
                "DebuggerScriptFailedToParse" -> EventReturnDebuggerScriptFailedToParse <$> obj .: "params"
                "DebuggerScriptParsed" -> EventReturnDebuggerScriptParsed <$> obj .: "params"
                "ProfilerConsoleProfileFinished" -> EventReturnProfilerConsoleProfileFinished <$> obj .: "params"
                "ProfilerConsoleProfileStarted" -> EventReturnProfilerConsoleProfileStarted <$> obj .: "params"
                "RuntimeConsoleApiCalled" -> EventReturnRuntimeConsoleApiCalled <$> obj .: "params"
                "RuntimeExceptionRevoked" -> EventReturnRuntimeExceptionRevoked <$> obj .: "params"
                "RuntimeExceptionThrown" -> EventReturnRuntimeExceptionThrown <$> obj .: "params"
                "RuntimeExecutionContextCreated" -> EventReturnRuntimeExecutionContextCreated <$> obj .: "params"
                "RuntimeExecutionContextDestroyed" -> EventReturnRuntimeExecutionContextDestroyed <$> obj .: "params"
                "RuntimeExecutionContextsCleared" -> EventReturnRuntimeExecutionContextsCleared <$> obj .: "params"
                "RuntimeInspectRequested" -> EventReturnRuntimeInspectRequested <$> obj .: "params"
                _ -> error "failed to parse EventResponseResult"
        pure EventResponseResult{..}

