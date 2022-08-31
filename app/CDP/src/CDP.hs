{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module CDP (module CDP) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe          
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
import Data.Proxy
import System.Random

import Utils
import Runtime

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


type ClientApp b = Session -> IO b
data Session = Session { unSession :: Session' Event }  

runClient  :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort app = runClient' hostPort (app . Session)

subscribe :: forall a. FromEvent Event a => Session -> (a -> IO ()) -> IO ()
subscribe session h = subscribe' (unSession session) h

unsubscribe :: forall a. FromEvent Event a => Session -> Proxy a -> IO ()
unsubscribe session p = unsubscribe' (unSession session) p

sendReceiveCommand :: (ToJSON a) => Session -> String -> Maybe a -> IO (Maybe Error)
sendReceiveCommand session = sendReceiveCommand' (unSession session)

sendReceiveCommandResult :: forall a b s. (ToJSON a, Command b) => Session -> String -> Maybe a -> IO (Either Error b)
sendReceiveCommandResult session = sendReceiveCommandResult' (unSession session)


data Event = EVDOMAttributeModified DOMAttributeModified | EVDOMAttributeRemoved DOMAttributeRemoved | EVDOMCharacterDataModified DOMCharacterDataModified | EVDOMChildNodeCountUpdated DOMChildNodeCountUpdated | EVDOMChildNodeInserted DOMChildNodeInserted | EVDOMChildNodeRemoved DOMChildNodeRemoved | EVDOMDocumentUpdated DOMDocumentUpdated | EVDOMSetChildNodes DOMSetChildNodes | EVLogEntryAdded LogEntryAdded | EVNetworkDataReceived NetworkDataReceived | EVNetworkEventSourceMessageReceived NetworkEventSourceMessageReceived | EVNetworkLoadingFailed NetworkLoadingFailed | EVNetworkLoadingFinished NetworkLoadingFinished | EVNetworkRequestServedFromCache NetworkRequestServedFromCache | EVNetworkRequestWillBeSent NetworkRequestWillBeSent | EVNetworkResponseReceived NetworkResponseReceived | EVNetworkWebSocketClosed NetworkWebSocketClosed | EVNetworkWebSocketCreated NetworkWebSocketCreated | EVNetworkWebSocketFrameError NetworkWebSocketFrameError | EVNetworkWebSocketFrameReceived NetworkWebSocketFrameReceived | EVNetworkWebSocketFrameSent NetworkWebSocketFrameSent | EVNetworkWebSocketHandshakeResponseReceived NetworkWebSocketHandshakeResponseReceived | EVNetworkWebSocketWillSendHandshakeRequest NetworkWebSocketWillSendHandshakeRequest | EVNetworkWebTransportCreated NetworkWebTransportCreated | EVNetworkWebTransportConnectionEstablished NetworkWebTransportConnectionEstablished | EVNetworkWebTransportClosed NetworkWebTransportClosed | EVPageDomContentEventFired PageDomContentEventFired | EVPageFileChooserOpened PageFileChooserOpened | EVPageFrameAttached PageFrameAttached | EVPageFrameDetached PageFrameDetached | EVPageFrameNavigated PageFrameNavigated | EVPageInterstitialHidden PageInterstitialHidden | EVPageInterstitialShown PageInterstitialShown | EVPageJavascriptDialogClosed PageJavascriptDialogClosed | EVPageJavascriptDialogOpening PageJavascriptDialogOpening | EVPageLifecycleEvent PageLifecycleEvent | EVPagePrerenderAttemptCompleted PagePrerenderAttemptCompleted | EVPageLoadEventFired PageLoadEventFired | EVPageWindowOpen PageWindowOpen | EVPerformanceMetrics PerformanceMetrics | EVTargetReceivedMessageFromTarget TargetReceivedMessageFromTarget | EVTargetTargetCreated TargetTargetCreated | EVTargetTargetDestroyed TargetTargetDestroyed | EVTargetTargetCrashed TargetTargetCrashed | EVTargetTargetInfoChanged TargetTargetInfoChanged | EVFetchRequestPaused FetchRequestPaused | EVFetchAuthRequired FetchAuthRequired | EVConsoleMessageAdded ConsoleMessageAdded | EVDebuggerBreakpointResolved DebuggerBreakpointResolved | EVDebuggerPaused DebuggerPaused | EVDebuggerResumed DebuggerResumed | EVDebuggerScriptFailedToParse DebuggerScriptFailedToParse | EVDebuggerScriptParsed DebuggerScriptParsed | EVProfilerConsoleProfileFinished ProfilerConsoleProfileFinished | EVProfilerConsoleProfileStarted ProfilerConsoleProfileStarted | EVRuntimeConsoleApiCalled RuntimeConsoleApiCalled | EVRuntimeExceptionRevoked RuntimeExceptionRevoked | EVRuntimeExceptionThrown RuntimeExceptionThrown | EVRuntimeExecutionContextCreated RuntimeExecutionContextCreated | EVRuntimeExecutionContextDestroyed RuntimeExecutionContextDestroyed | EVRuntimeExecutionContextsCleared RuntimeExecutionContextsCleared | EVRuntimeInspectRequested RuntimeInspectRequested
    deriving (Eq, Show, Read)
instance FromJSON (EventResponse Event ) where
    parseJSON = A.withObject  "EventResponse"  $ \obj -> do
        name <- obj .: "method"
        case (name :: String) of
                "DOM.attributeModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMAttributeModified) . fmap EVDOMAttributeModified <$> obj .:? "params"
                "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMAttributeRemoved) . fmap EVDOMAttributeRemoved <$> obj .:? "params"
                "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMCharacterDataModified) . fmap EVDOMCharacterDataModified <$> obj .:? "params"
                "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMChildNodeCountUpdated) . fmap EVDOMChildNodeCountUpdated <$> obj .:? "params"
                "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMChildNodeInserted) . fmap EVDOMChildNodeInserted <$> obj .:? "params"
                "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMChildNodeRemoved) . fmap EVDOMChildNodeRemoved <$> obj .:? "params"
                "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMDocumentUpdated) . fmap EVDOMDocumentUpdated <$> obj .:? "params"
                "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMSetChildNodes) . fmap EVDOMSetChildNodes <$> obj .:? "params"
                "Log.entryAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy LogEntryAdded) . fmap EVLogEntryAdded <$> obj .:? "params"
                "Network.dataReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkDataReceived) . fmap EVNetworkDataReceived <$> obj .:? "params"
                "Network.eventSourceMessageReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkEventSourceMessageReceived) . fmap EVNetworkEventSourceMessageReceived <$> obj .:? "params"
                "Network.loadingFailed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkLoadingFailed) . fmap EVNetworkLoadingFailed <$> obj .:? "params"
                "Network.loadingFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkLoadingFinished) . fmap EVNetworkLoadingFinished <$> obj .:? "params"
                "Network.requestServedFromCache" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkRequestServedFromCache) . fmap EVNetworkRequestServedFromCache <$> obj .:? "params"
                "Network.requestWillBeSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkRequestWillBeSent) . fmap EVNetworkRequestWillBeSent <$> obj .:? "params"
                "Network.responseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkResponseReceived) . fmap EVNetworkResponseReceived <$> obj .:? "params"
                "Network.webSocketClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketClosed) . fmap EVNetworkWebSocketClosed <$> obj .:? "params"
                "Network.webSocketCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketCreated) . fmap EVNetworkWebSocketCreated <$> obj .:? "params"
                "Network.webSocketFrameError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketFrameError) . fmap EVNetworkWebSocketFrameError <$> obj .:? "params"
                "Network.webSocketFrameReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketFrameReceived) . fmap EVNetworkWebSocketFrameReceived <$> obj .:? "params"
                "Network.webSocketFrameSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketFrameSent) . fmap EVNetworkWebSocketFrameSent <$> obj .:? "params"
                "Network.webSocketHandshakeResponseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketHandshakeResponseReceived) . fmap EVNetworkWebSocketHandshakeResponseReceived <$> obj .:? "params"
                "Network.webSocketWillSendHandshakeRequest" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebSocketWillSendHandshakeRequest) . fmap EVNetworkWebSocketWillSendHandshakeRequest <$> obj .:? "params"
                "Network.webTransportCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebTransportCreated) . fmap EVNetworkWebTransportCreated <$> obj .:? "params"
                "Network.webTransportConnectionEstablished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebTransportConnectionEstablished) . fmap EVNetworkWebTransportConnectionEstablished <$> obj .:? "params"
                "Network.webTransportClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy NetworkWebTransportClosed) . fmap EVNetworkWebTransportClosed <$> obj .:? "params"
                "Page.domContentEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageDomContentEventFired) . fmap EVPageDomContentEventFired <$> obj .:? "params"
                "Page.fileChooserOpened" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageFileChooserOpened) . fmap EVPageFileChooserOpened <$> obj .:? "params"
                "Page.frameAttached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageFrameAttached) . fmap EVPageFrameAttached <$> obj .:? "params"
                "Page.frameDetached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageFrameDetached) . fmap EVPageFrameDetached <$> obj .:? "params"
                "Page.frameNavigated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageFrameNavigated) . fmap EVPageFrameNavigated <$> obj .:? "params"
                "Page.interstitialHidden" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageInterstitialHidden) . fmap EVPageInterstitialHidden <$> obj .:? "params"
                "Page.interstitialShown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageInterstitialShown) . fmap EVPageInterstitialShown <$> obj .:? "params"
                "Page.javascriptDialogClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageJavascriptDialogClosed) . fmap EVPageJavascriptDialogClosed <$> obj .:? "params"
                "Page.javascriptDialogOpening" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageJavascriptDialogOpening) . fmap EVPageJavascriptDialogOpening <$> obj .:? "params"
                "Page.lifecycleEvent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageLifecycleEvent) . fmap EVPageLifecycleEvent <$> obj .:? "params"
                "Page.prerenderAttemptCompleted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PagePrerenderAttemptCompleted) . fmap EVPagePrerenderAttemptCompleted <$> obj .:? "params"
                "Page.loadEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageLoadEventFired) . fmap EVPageLoadEventFired <$> obj .:? "params"
                "Page.windowOpen" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PageWindowOpen) . fmap EVPageWindowOpen <$> obj .:? "params"
                "Performance.metrics" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PerformanceMetrics) . fmap EVPerformanceMetrics <$> obj .:? "params"
                "Target.receivedMessageFromTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy TargetReceivedMessageFromTarget) . fmap EVTargetReceivedMessageFromTarget <$> obj .:? "params"
                "Target.targetCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy TargetTargetCreated) . fmap EVTargetTargetCreated <$> obj .:? "params"
                "Target.targetDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy TargetTargetDestroyed) . fmap EVTargetTargetDestroyed <$> obj .:? "params"
                "Target.targetCrashed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy TargetTargetCrashed) . fmap EVTargetTargetCrashed <$> obj .:? "params"
                "Target.targetInfoChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy TargetTargetInfoChanged) . fmap EVTargetTargetInfoChanged <$> obj .:? "params"
                "Fetch.requestPaused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy FetchRequestPaused) . fmap EVFetchRequestPaused <$> obj .:? "params"
                "Fetch.authRequired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy FetchAuthRequired) . fmap EVFetchAuthRequired <$> obj .:? "params"
                "Console.messageAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ConsoleMessageAdded) . fmap EVConsoleMessageAdded <$> obj .:? "params"
                "Debugger.breakpointResolved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DebuggerBreakpointResolved) . fmap EVDebuggerBreakpointResolved <$> obj .:? "params"
                "Debugger.paused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DebuggerPaused) . fmap EVDebuggerPaused <$> obj .:? "params"
                "Debugger.resumed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DebuggerResumed) . fmap EVDebuggerResumed <$> obj .:? "params"
                "Debugger.scriptFailedToParse" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DebuggerScriptFailedToParse) . fmap EVDebuggerScriptFailedToParse <$> obj .:? "params"
                "Debugger.scriptParsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DebuggerScriptParsed) . fmap EVDebuggerScriptParsed <$> obj .:? "params"
                "Profiler.consoleProfileFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ProfilerConsoleProfileFinished) . fmap EVProfilerConsoleProfileFinished <$> obj .:? "params"
                "Profiler.consoleProfileStarted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ProfilerConsoleProfileStarted) . fmap EVProfilerConsoleProfileStarted <$> obj .:? "params"
                "Runtime.consoleAPICalled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeConsoleApiCalled) . fmap EVRuntimeConsoleApiCalled <$> obj .:? "params"
                "Runtime.exceptionRevoked" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeExceptionRevoked) . fmap EVRuntimeExceptionRevoked <$> obj .:? "params"
                "Runtime.exceptionThrown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeExceptionThrown) . fmap EVRuntimeExceptionThrown <$> obj .:? "params"
                "Runtime.executionContextCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeExecutionContextCreated) . fmap EVRuntimeExecutionContextCreated <$> obj .:? "params"
                "Runtime.executionContextDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeExecutionContextDestroyed) . fmap EVRuntimeExecutionContextDestroyed <$> obj .:? "params"
                "Runtime.executionContextsCleared" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeExecutionContextsCleared) . fmap EVRuntimeExecutionContextsCleared <$> obj .:? "params"
                "Runtime.inspectRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy RuntimeInspectRequested) . fmap EVRuntimeInspectRequested <$> obj .:? "params"
                _ -> fail "failed to parse EventResponse"


