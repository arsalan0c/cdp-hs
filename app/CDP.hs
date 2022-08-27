{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections  #-}
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
import Data.Proxy


defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222)

hostPortToEndpoint :: (String, Int) -> Http.Request
hostPortToEndpoint (host, port) = Http.parseRequest_ . 
    ("GET " <>) . 
    mconcat $ [host, ":", show port, "/json"]

type ClientApp a = Session -> IO a

data Session = MkSession 
    { events       :: MVar (Map.Map String Handler)
    , conn         :: WS.Connection
    , listenThread :: ThreadId
    }

runClient :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort app = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi   <- getPageInfo endpoint
    eventsM <- newMVar Map.empty
    let (host, port, path) = parseUri . debuggerUrl $ pi
    
    WS.runClient host port path $ \conn -> do
        listenThread <- forkIO $ forever $ do
            res <- WS.fromDataMessage <$> WS.receiveDataMessage conn
            dispatchEventResponse eventsM res
                
        
        let session = MkSession eventsM conn listenThread
        result <- app session
        killThread listenThread
        pure result

decodeEvent :: BS.ByteString -> Maybe (EventResponse)
decodeEvent = A.decode

dispatchEventResponse :: MVar (Map.Map String Handler) -> BS.ByteString -> IO ()
dispatchEventResponse handlers res = do
    void $ go handlers . decodeEvent $ res
  where
    go :: MVar (Map.Map String Handler) -> Maybe EventResponse -> IO ()
    go handlers evr = maybe (pure ()) f evr
      where
        f (EventResponse p v) = do
            evs <- readMVar handlers
            let handler = maybe print id $ Map.lookup (eventName p) evs >>= handlerToF p
            maybe (pure ()) handler v
    
class (FromJSON a, Show a, Eq a) => Event a where
    eventName   :: Proxy a  -> String
    fToHandler  :: Proxy a  -> ((a -> IO ()) -> Handler)
    handlerToF  :: Proxy a  -> Handler -> Maybe (a -> IO ())

updateEvents :: Session -> (Map.Map String Handler -> Map.Map String Handler) -> IO ()
updateEvents session f = ($ pure . f) . modifyMVar_ . events $ session

subscribe :: forall a. Event a => Session -> (a -> IO ()) -> IO ()
subscribe session h = updateEvents session $ Map.insert (eventName p) $ fToHandler p h
  where
    p = (Proxy :: Proxy a)

unsubscribe :: Event a => Session -> Proxy a -> IO ()
unsubscribe session p = updateEvents session (Map.delete (eventName p))


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

data ToJSONEx where
   ToJSONEx :: (ToJSON a, Show a) => a -> ToJSONEx
instance ToJSON ToJSONEx where
    toJSON (ToJSONEx v) = toJSON v
instance Show ToJSONEx where
    show (ToJSONEx v) = show v
data Command = Command {
      commandId :: Int
    , commandMethod :: String
    , commandParams :: [(String, ToJSONEx)]
    } deriving Show
instance ToJSON Command where
   toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= commandParams cmd
        ]



methodToName :: T.Text -> String
methodToName md = let [domain, method] = T.splitOn "." md in
    mconcat [T.unpack domain, C.pascal . T.unpack $ method]



data EventResponse where
    EventResponse :: Event a => Proxy a -> Maybe a -> EventResponse

data CommandResponseResult a = CommandResponseResult { crrId :: Int, crrResult :: a }
data CommandResponse = CommandResponse { crId :: Int }

instance (Show a) => Show (CommandResponseResult a) where
    show CommandResponseResult{..} = "\nid: " <> show crrId <> "\nresult: " <> show crrResult
instance Show CommandResponse where
    show CommandResponse{..} = "\nid: " <> show crId

instance (FromJSON a) => FromJSON (CommandResponseResult a) where
    parseJSON = A.withObject "CommandResponseResult" $ \obj -> do
        crrId <- obj .: "id"
        crrResult <- obj .: "result"
        pure CommandResponseResult{..}

instance FromJSON CommandResponse where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId <- obj .: "id"
        pure CommandResponse{..}

newtype Error = Error String
    deriving Show

indent :: Int -> String -> String
indent = (<>) . flip replicate ' '

commandToStr :: Command -> String
commandToStr Command{..} = unlines 
    [ "command: " <> commandMethod
    , if (not . null) commandParams 
        then "arguments: " <> (unlines . map (indent 2 . (\(f,s) -> f <> ":" <> s) . fmap show) $ commandParams)
        else ""
    ]

sendCommand :: WS.Connection -> (String, String) -> [(String, ToJSONEx)] -> IO Command
sendCommand conn (domain,method) paramArgs = do
    putStrLn . show $ (domain, method)
    id <- pure 1  -- TODO: randomly generate
    let c = command id
    WS.sendTextData conn . A.encode $ c
    pure c
  where
    command id = Command id 
        (domain <> "." <> method)
        paramArgs
   
receiveResponse :: (FromJSON a) => WS.Connection -> IO (Maybe a)
receiveResponse conn = A.decode <$> do
    dm <- WS.receiveDataMessage conn
    putStrLn . show $ dm
    pure $ WS.fromDataMessage dm

sendReceiveCommandResult :: FromJSON b =>
    WS.Connection ->
         (String, String) -> [(String, ToJSONEx)]
         -> IO (Either Error b)
sendReceiveCommandResult conn (domain,method) paramArgs = do
    command <- sendCommand conn (domain,method) paramArgs
    res     <- receiveResponse conn
    pure $ maybe (Left . responseParseError $ command) 
        (maybe (Left . responseParseError $ command) Right . crrResult) res 

sendReceiveCommand ::
    WS.Connection ->
         (String, String) -> [(String, ToJSONEx)]
         -> IO (Maybe Error)
sendReceiveCommand conn (domain,method) paramArgs = do
    command <- sendCommand conn (domain, method) paramArgs
    res     <- receiveResponse conn
    pure $ maybe (Just . responseParseError $ command) 
        (const Nothing . crId) res

responseParseError :: Command -> Error
responseParseError c = Error . unlines $
    ["unable to parse response", commandToStr c]

data Handler = HDOMAttributeModified (DOMAttributeModified -> IO ()) | HDOMAttributeRemoved (DOMAttributeRemoved -> IO ()) | HDOMCharacterDataModified (DOMCharacterDataModified -> IO ()) | HDOMChildNodeCountUpdated (DOMChildNodeCountUpdated -> IO ()) | HDOMChildNodeInserted (DOMChildNodeInserted -> IO ()) | HDOMChildNodeRemoved (DOMChildNodeRemoved -> IO ()) | HDOMDocumentUpdated (DOMDocumentUpdated -> IO ()) | HDOMSetChildNodes (DOMSetChildNodes -> IO ()) | HLogEntryAdded (LogEntryAdded -> IO ()) | HNetworkDataReceived (NetworkDataReceived -> IO ()) | HNetworkEventSourceMessageReceived (NetworkEventSourceMessageReceived -> IO ()) | HNetworkLoadingFailed (NetworkLoadingFailed -> IO ()) | HNetworkLoadingFinished (NetworkLoadingFinished -> IO ()) | HNetworkRequestServedFromCache (NetworkRequestServedFromCache -> IO ()) | HNetworkRequestWillBeSent (NetworkRequestWillBeSent -> IO ()) | HNetworkResponseReceived (NetworkResponseReceived -> IO ()) | HNetworkWebSocketClosed (NetworkWebSocketClosed -> IO ()) | HNetworkWebSocketCreated (NetworkWebSocketCreated -> IO ()) | HNetworkWebSocketFrameError (NetworkWebSocketFrameError -> IO ()) | HNetworkWebSocketFrameReceived (NetworkWebSocketFrameReceived -> IO ()) | HNetworkWebSocketFrameSent (NetworkWebSocketFrameSent -> IO ()) | HNetworkWebSocketHandshakeResponseReceived (NetworkWebSocketHandshakeResponseReceived -> IO ()) | HNetworkWebSocketWillSendHandshakeRequest (NetworkWebSocketWillSendHandshakeRequest -> IO ()) | HNetworkWebTransportCreated (NetworkWebTransportCreated -> IO ()) | HNetworkWebTransportConnectionEstablished (NetworkWebTransportConnectionEstablished -> IO ()) | HNetworkWebTransportClosed (NetworkWebTransportClosed -> IO ()) | HPageDomContentEventFired (PageDomContentEventFired -> IO ()) | HPageFileChooserOpened (PageFileChooserOpened -> IO ()) | HPageFrameAttached (PageFrameAttached -> IO ()) | HPageFrameDetached (PageFrameDetached -> IO ()) | HPageFrameNavigated (PageFrameNavigated -> IO ()) | HPageInterstitialHidden (PageInterstitialHidden -> IO ()) | HPageInterstitialShown (PageInterstitialShown -> IO ()) | HPageJavascriptDialogClosed (PageJavascriptDialogClosed -> IO ()) | HPageJavascriptDialogOpening (PageJavascriptDialogOpening -> IO ()) | HPageLifecycleEvent (PageLifecycleEvent -> IO ()) | HPagePrerenderAttemptCompleted (PagePrerenderAttemptCompleted -> IO ()) | HPageLoadEventFired (PageLoadEventFired -> IO ()) | HPageWindowOpen (PageWindowOpen -> IO ()) | HPerformanceMetrics (PerformanceMetrics -> IO ()) | HTargetReceivedMessageFromTarget (TargetReceivedMessageFromTarget -> IO ()) | HTargetTargetCreated (TargetTargetCreated -> IO ()) | HTargetTargetDestroyed (TargetTargetDestroyed -> IO ()) | HTargetTargetCrashed (TargetTargetCrashed -> IO ()) | HTargetTargetInfoChanged (TargetTargetInfoChanged -> IO ()) | HFetchRequestPaused (FetchRequestPaused -> IO ()) | HFetchAuthRequired (FetchAuthRequired -> IO ()) | HConsoleMessageAdded (ConsoleMessageAdded -> IO ()) | HDebuggerBreakpointResolved (DebuggerBreakpointResolved -> IO ()) | HDebuggerPaused (DebuggerPaused -> IO ()) | HDebuggerResumed (DebuggerResumed -> IO ()) | HDebuggerScriptFailedToParse (DebuggerScriptFailedToParse -> IO ()) | HDebuggerScriptParsed (DebuggerScriptParsed -> IO ()) | HProfilerConsoleProfileFinished (ProfilerConsoleProfileFinished -> IO ()) | HProfilerConsoleProfileStarted (ProfilerConsoleProfileStarted -> IO ()) | HRuntimeConsoleApiCalled (RuntimeConsoleApiCalled -> IO ()) | HRuntimeExceptionRevoked (RuntimeExceptionRevoked -> IO ()) | HRuntimeExceptionThrown (RuntimeExceptionThrown -> IO ()) | HRuntimeExecutionContextCreated (RuntimeExecutionContextCreated -> IO ()) | HRuntimeExecutionContextDestroyed (RuntimeExecutionContextDestroyed -> IO ()) | HRuntimeExecutionContextsCleared (RuntimeExecutionContextsCleared -> IO ()) | HRuntimeInspectRequested (RuntimeInspectRequested -> IO ())
instance FromJSON EventResponse where
    parseJSON = A.withObject  "EventResponse"  $ \obj -> do
        name <- obj .: "method"
        case (name :: String) of
                "DOM.attributeModified" -> EventResponse (Proxy :: Proxy DOMAttributeModified) <$> obj .:? "params"
                "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy DOMAttributeRemoved) <$> obj .:? "params"
                "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy DOMCharacterDataModified) <$> obj .:? "params"
                "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy DOMChildNodeCountUpdated) <$> obj .:? "params"
                "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy DOMChildNodeInserted) <$> obj .:? "params"
                "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy DOMChildNodeRemoved) <$> obj .:? "params"
                "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy DOMDocumentUpdated) <$> obj .:? "params"
                "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy DOMSetChildNodes) <$> obj .:? "params"
                "Log.entryAdded" -> EventResponse (Proxy :: Proxy LogEntryAdded) <$> obj .:? "params"
                "Network.dataReceived" -> EventResponse (Proxy :: Proxy NetworkDataReceived) <$> obj .:? "params"
                "Network.eventSourceMessageReceived" -> EventResponse (Proxy :: Proxy NetworkEventSourceMessageReceived) <$> obj .:? "params"
                "Network.loadingFailed" -> EventResponse (Proxy :: Proxy NetworkLoadingFailed) <$> obj .:? "params"
                "Network.loadingFinished" -> EventResponse (Proxy :: Proxy NetworkLoadingFinished) <$> obj .:? "params"
                "Network.requestServedFromCache" -> EventResponse (Proxy :: Proxy NetworkRequestServedFromCache) <$> obj .:? "params"
                "Network.requestWillBeSent" -> EventResponse (Proxy :: Proxy NetworkRequestWillBeSent) <$> obj .:? "params"
                "Network.responseReceived" -> EventResponse (Proxy :: Proxy NetworkResponseReceived) <$> obj .:? "params"
                "Network.webSocketClosed" -> EventResponse (Proxy :: Proxy NetworkWebSocketClosed) <$> obj .:? "params"
                "Network.webSocketCreated" -> EventResponse (Proxy :: Proxy NetworkWebSocketCreated) <$> obj .:? "params"
                "Network.webSocketFrameError" -> EventResponse (Proxy :: Proxy NetworkWebSocketFrameError) <$> obj .:? "params"
                "Network.webSocketFrameReceived" -> EventResponse (Proxy :: Proxy NetworkWebSocketFrameReceived) <$> obj .:? "params"
                "Network.webSocketFrameSent" -> EventResponse (Proxy :: Proxy NetworkWebSocketFrameSent) <$> obj .:? "params"
                "Network.webSocketHandshakeResponseReceived" -> EventResponse (Proxy :: Proxy NetworkWebSocketHandshakeResponseReceived) <$> obj .:? "params"
                "Network.webSocketWillSendHandshakeRequest" -> EventResponse (Proxy :: Proxy NetworkWebSocketWillSendHandshakeRequest) <$> obj .:? "params"
                "Network.webTransportCreated" -> EventResponse (Proxy :: Proxy NetworkWebTransportCreated) <$> obj .:? "params"
                "Network.webTransportConnectionEstablished" -> EventResponse (Proxy :: Proxy NetworkWebTransportConnectionEstablished) <$> obj .:? "params"
                "Network.webTransportClosed" -> EventResponse (Proxy :: Proxy NetworkWebTransportClosed) <$> obj .:? "params"
                "Page.domContentEventFired" -> EventResponse (Proxy :: Proxy PageDomContentEventFired) <$> obj .:? "params"
                "Page.fileChooserOpened" -> EventResponse (Proxy :: Proxy PageFileChooserOpened) <$> obj .:? "params"
                "Page.frameAttached" -> EventResponse (Proxy :: Proxy PageFrameAttached) <$> obj .:? "params"
                "Page.frameDetached" -> EventResponse (Proxy :: Proxy PageFrameDetached) <$> obj .:? "params"
                "Page.frameNavigated" -> EventResponse (Proxy :: Proxy PageFrameNavigated) <$> obj .:? "params"
                "Page.interstitialHidden" -> EventResponse (Proxy :: Proxy PageInterstitialHidden) <$> obj .:? "params"
                "Page.interstitialShown" -> EventResponse (Proxy :: Proxy PageInterstitialShown) <$> obj .:? "params"
                "Page.javascriptDialogClosed" -> EventResponse (Proxy :: Proxy PageJavascriptDialogClosed) <$> obj .:? "params"
                "Page.javascriptDialogOpening" -> EventResponse (Proxy :: Proxy PageJavascriptDialogOpening) <$> obj .:? "params"
                "Page.lifecycleEvent" -> EventResponse (Proxy :: Proxy PageLifecycleEvent) <$> obj .:? "params"
                "Page.prerenderAttemptCompleted" -> EventResponse (Proxy :: Proxy PagePrerenderAttemptCompleted) <$> obj .:? "params"
                "Page.loadEventFired" -> EventResponse (Proxy :: Proxy PageLoadEventFired) <$> obj .:? "params"
                "Page.windowOpen" -> EventResponse (Proxy :: Proxy PageWindowOpen) <$> obj .:? "params"
                "Performance.metrics" -> EventResponse (Proxy :: Proxy PerformanceMetrics) <$> obj .:? "params"
                "Target.receivedMessageFromTarget" -> EventResponse (Proxy :: Proxy TargetReceivedMessageFromTarget) <$> obj .:? "params"
                "Target.targetCreated" -> EventResponse (Proxy :: Proxy TargetTargetCreated) <$> obj .:? "params"
                "Target.targetDestroyed" -> EventResponse (Proxy :: Proxy TargetTargetDestroyed) <$> obj .:? "params"
                "Target.targetCrashed" -> EventResponse (Proxy :: Proxy TargetTargetCrashed) <$> obj .:? "params"
                "Target.targetInfoChanged" -> EventResponse (Proxy :: Proxy TargetTargetInfoChanged) <$> obj .:? "params"
                "Fetch.requestPaused" -> EventResponse (Proxy :: Proxy FetchRequestPaused) <$> obj .:? "params"
                "Fetch.authRequired" -> EventResponse (Proxy :: Proxy FetchAuthRequired) <$> obj .:? "params"
                "Console.messageAdded" -> EventResponse (Proxy :: Proxy ConsoleMessageAdded) <$> obj .:? "params"
                "Debugger.breakpointResolved" -> EventResponse (Proxy :: Proxy DebuggerBreakpointResolved) <$> obj .:? "params"
                "Debugger.paused" -> EventResponse (Proxy :: Proxy DebuggerPaused) <$> obj .:? "params"
                "Debugger.resumed" -> EventResponse (Proxy :: Proxy DebuggerResumed) <$> obj .:? "params"
                "Debugger.scriptFailedToParse" -> EventResponse (Proxy :: Proxy DebuggerScriptFailedToParse) <$> obj .:? "params"
                "Debugger.scriptParsed" -> EventResponse (Proxy :: Proxy DebuggerScriptParsed) <$> obj .:? "params"
                "Profiler.consoleProfileFinished" -> EventResponse (Proxy :: Proxy ProfilerConsoleProfileFinished) <$> obj .:? "params"
                "Profiler.consoleProfileStarted" -> EventResponse (Proxy :: Proxy ProfilerConsoleProfileStarted) <$> obj .:? "params"
                "Runtime.consoleAPICalled" -> EventResponse (Proxy :: Proxy RuntimeConsoleApiCalled) <$> obj .:? "params"
                "Runtime.exceptionRevoked" -> EventResponse (Proxy :: Proxy RuntimeExceptionRevoked) <$> obj .:? "params"
                "Runtime.exceptionThrown" -> EventResponse (Proxy :: Proxy RuntimeExceptionThrown) <$> obj .:? "params"
                "Runtime.executionContextCreated" -> EventResponse (Proxy :: Proxy RuntimeExecutionContextCreated) <$> obj .:? "params"
                "Runtime.executionContextDestroyed" -> EventResponse (Proxy :: Proxy RuntimeExecutionContextDestroyed) <$> obj .:? "params"
                "Runtime.executionContextsCleared" -> EventResponse (Proxy :: Proxy RuntimeExecutionContextsCleared) <$> obj .:? "params"
                "Runtime.inspectRequested" -> EventResponse (Proxy :: Proxy RuntimeInspectRequested) <$> obj .:? "params"
                _ -> error "failed to parse EventResponse"




browserClose :: Session -> IO (Maybe Error)
browserClose session  = sendReceiveCommand (conn session) ("Browser","close") ([] ++ (catMaybes []))

data BrowserGetVersion = BrowserGetVersion {
    browserGetVersionProtocolVersion :: String,
    browserGetVersionProduct :: String,
    browserGetVersionRevision :: String,
    browserGetVersionUserAgent :: String,
    browserGetVersionJsVersion :: String
} deriving (Eq, Show, Read)
instance FromJSON  BrowserGetVersion where
    parseJSON = A.withObject "BrowserGetVersion" $ \v ->
         BrowserGetVersion <$> v .:  "protocolVersion"
            <*> v  .:  "product"
            <*> v  .:  "revision"
            <*> v  .:  "userAgent"
            <*> v  .:  "jsVersion"



browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session  = sendReceiveCommandResult (conn session) ("Browser","getVersion") ([] ++ (catMaybes []))



data DOMAttributeModified = DOMAttributeModified {
    domAttributeModifiedNodeId :: DOMNodeId,
    domAttributeModifiedName :: String,
    domAttributeModifiedValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMAttributeModified where
    parseJSON = A.withObject "DOMAttributeModified" $ \v ->
         DOMAttributeModified <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"


instance ToJSON DOMAttributeModified  where
    toJSON v = A.object
        [ "nodeId" .= domAttributeModifiedNodeId v
        , "name" .= domAttributeModifiedName v
        , "value" .= domAttributeModifiedValue v
        ]


instance Event  DOMAttributeModified where
    eventName  _   =  "DOM.attributeModified"
    fToHandler _   =  HDOMAttributeModified
    handlerToF _ h =  case h of HDOMAttributeModified f -> Just f; _ -> Nothing

data DOMAttributeRemoved = DOMAttributeRemoved {
    domAttributeRemovedNodeId :: DOMNodeId,
    domAttributeRemovedName :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMAttributeRemoved where
    parseJSON = A.withObject "DOMAttributeRemoved" $ \v ->
         DOMAttributeRemoved <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON DOMAttributeRemoved  where
    toJSON v = A.object
        [ "nodeId" .= domAttributeRemovedNodeId v
        , "name" .= domAttributeRemovedName v
        ]


instance Event  DOMAttributeRemoved where
    eventName  _   =  "DOM.attributeRemoved"
    fToHandler _   =  HDOMAttributeRemoved
    handlerToF _ h =  case h of HDOMAttributeRemoved f -> Just f; _ -> Nothing

data DOMCharacterDataModified = DOMCharacterDataModified {
    domCharacterDataModifiedNodeId :: DOMNodeId,
    domCharacterDataModifiedCharacterData :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMCharacterDataModified where
    parseJSON = A.withObject "DOMCharacterDataModified" $ \v ->
         DOMCharacterDataModified <$> v .:  "nodeId"
            <*> v  .:  "characterData"


instance ToJSON DOMCharacterDataModified  where
    toJSON v = A.object
        [ "nodeId" .= domCharacterDataModifiedNodeId v
        , "characterData" .= domCharacterDataModifiedCharacterData v
        ]


instance Event  DOMCharacterDataModified where
    eventName  _   =  "DOM.characterDataModified"
    fToHandler _   =  HDOMCharacterDataModified
    handlerToF _ h =  case h of HDOMCharacterDataModified f -> Just f; _ -> Nothing

data DOMChildNodeCountUpdated = DOMChildNodeCountUpdated {
    domChildNodeCountUpdatedNodeId :: DOMNodeId,
    domChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeCountUpdated where
    parseJSON = A.withObject "DOMChildNodeCountUpdated" $ \v ->
         DOMChildNodeCountUpdated <$> v .:  "nodeId"
            <*> v  .:  "childNodeCount"


instance ToJSON DOMChildNodeCountUpdated  where
    toJSON v = A.object
        [ "nodeId" .= domChildNodeCountUpdatedNodeId v
        , "childNodeCount" .= domChildNodeCountUpdatedChildNodeCount v
        ]


instance Event  DOMChildNodeCountUpdated where
    eventName  _   =  "DOM.childNodeCountUpdated"
    fToHandler _   =  HDOMChildNodeCountUpdated
    handlerToF _ h =  case h of HDOMChildNodeCountUpdated f -> Just f; _ -> Nothing

data DOMChildNodeInserted = DOMChildNodeInserted {
    domChildNodeInsertedParentNodeId :: DOMNodeId,
    domChildNodeInsertedPreviousNodeId :: DOMNodeId,
    domChildNodeInsertedNode :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeInserted where
    parseJSON = A.withObject "DOMChildNodeInserted" $ \v ->
         DOMChildNodeInserted <$> v .:  "parentNodeId"
            <*> v  .:  "previousNodeId"
            <*> v  .:  "node"


instance ToJSON DOMChildNodeInserted  where
    toJSON v = A.object
        [ "parentNodeId" .= domChildNodeInsertedParentNodeId v
        , "previousNodeId" .= domChildNodeInsertedPreviousNodeId v
        , "node" .= domChildNodeInsertedNode v
        ]


instance Event  DOMChildNodeInserted where
    eventName  _   =  "DOM.childNodeInserted"
    fToHandler _   =  HDOMChildNodeInserted
    handlerToF _ h =  case h of HDOMChildNodeInserted f -> Just f; _ -> Nothing

data DOMChildNodeRemoved = DOMChildNodeRemoved {
    domChildNodeRemovedParentNodeId :: DOMNodeId,
    domChildNodeRemovedNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeRemoved where
    parseJSON = A.withObject "DOMChildNodeRemoved" $ \v ->
         DOMChildNodeRemoved <$> v .:  "parentNodeId"
            <*> v  .:  "nodeId"


instance ToJSON DOMChildNodeRemoved  where
    toJSON v = A.object
        [ "parentNodeId" .= domChildNodeRemovedParentNodeId v
        , "nodeId" .= domChildNodeRemovedNodeId v
        ]


instance Event  DOMChildNodeRemoved where
    eventName  _   =  "DOM.childNodeRemoved"
    fToHandler _   =  HDOMChildNodeRemoved
    handlerToF _ h =  case h of HDOMChildNodeRemoved f -> Just f; _ -> Nothing

data DOMDocumentUpdated = DOMDocumentUpdated
    deriving (Eq, Show, Read)
instance FromJSON DOMDocumentUpdated where
    parseJSON = A.withText  "DOMDocumentUpdated"  $ \v -> do
        pure $ case v of
                "DOMDocumentUpdated" -> DOMDocumentUpdated
                _ -> error "failed to parse DOMDocumentUpdated"

instance Event  DOMDocumentUpdated where
    eventName  _   =  "DOM.documentUpdated"
    fToHandler _   =  HDOMDocumentUpdated
    handlerToF _ h =  case h of HDOMDocumentUpdated f -> Just f; _ -> Nothing

data DOMSetChildNodes = DOMSetChildNodes {
    domSetChildNodesParentId :: DOMNodeId,
    domSetChildNodesNodes :: [DOMNode]
} deriving (Eq, Show, Read)
instance FromJSON  DOMSetChildNodes where
    parseJSON = A.withObject "DOMSetChildNodes" $ \v ->
         DOMSetChildNodes <$> v .:  "parentId"
            <*> v  .:  "nodes"


instance ToJSON DOMSetChildNodes  where
    toJSON v = A.object
        [ "parentId" .= domSetChildNodesParentId v
        , "nodes" .= domSetChildNodesNodes v
        ]


instance Event  DOMSetChildNodes where
    eventName  _   =  "DOM.setChildNodes"
    fToHandler _   =  HDOMSetChildNodes
    handlerToF _ h =  case h of HDOMSetChildNodes f -> Just f; _ -> Nothing


type DOMNodeId = Int

type DOMBackendNodeId = Int

data DOMBackendNode = DOMBackendNode {
    domBackendNodeNodeType :: Int,
    domBackendNodeNodeName :: String,
    domBackendNodeBackendNodeId :: DOMBackendNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMBackendNode where
    parseJSON = A.withObject "DOMBackendNode" $ \v ->
         DOMBackendNode <$> v .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "backendNodeId"


instance ToJSON DOMBackendNode  where
    toJSON v = A.object
        [ "nodeType" .= domBackendNodeNodeType v
        , "nodeName" .= domBackendNodeNodeName v
        , "backendNodeId" .= domBackendNodeBackendNodeId v
        ]



data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
    deriving (Eq, Show, Read)
instance FromJSON DOMPseudoType where
    parseJSON = A.withText  "DOMPseudoType"  $ \v -> do
        pure $ case v of
                "first-line" -> DOMPseudoTypeFirstLine
                "first-letter" -> DOMPseudoTypeFirstLetter
                "before" -> DOMPseudoTypeBefore
                "after" -> DOMPseudoTypeAfter
                "marker" -> DOMPseudoTypeMarker
                "backdrop" -> DOMPseudoTypeBackdrop
                "selection" -> DOMPseudoTypeSelection
                "target-text" -> DOMPseudoTypeTargetText
                "spelling-error" -> DOMPseudoTypeSpellingError
                "grammar-error" -> DOMPseudoTypeGrammarError
                "highlight" -> DOMPseudoTypeHighlight
                "first-line-inherited" -> DOMPseudoTypeFirstLineInherited
                "scrollbar" -> DOMPseudoTypeScrollbar
                "scrollbar-thumb" -> DOMPseudoTypeScrollbarThumb
                "scrollbar-button" -> DOMPseudoTypeScrollbarButton
                "scrollbar-track" -> DOMPseudoTypeScrollbarTrack
                "scrollbar-track-piece" -> DOMPseudoTypeScrollbarTrackPiece
                "scrollbar-corner" -> DOMPseudoTypeScrollbarCorner
                "resizer" -> DOMPseudoTypeResizer
                "input-list-button" -> DOMPseudoTypeInputListButton
                "page-transition" -> DOMPseudoTypePageTransition
                "page-transition-container" -> DOMPseudoTypePageTransitionContainer
                "page-transition-image-wrapper" -> DOMPseudoTypePageTransitionImageWrapper
                "page-transition-outgoing-image" -> DOMPseudoTypePageTransitionOutgoingImage
                "page-transition-incoming-image" -> DOMPseudoTypePageTransitionIncomingImage
                _ -> error "failed to parse DOMPseudoType"

instance ToJSON DOMPseudoType where
    toJSON v = A.String $
        case v of
                DOMPseudoTypeFirstLine -> "first-line"
                DOMPseudoTypeFirstLetter -> "first-letter"
                DOMPseudoTypeBefore -> "before"
                DOMPseudoTypeAfter -> "after"
                DOMPseudoTypeMarker -> "marker"
                DOMPseudoTypeBackdrop -> "backdrop"
                DOMPseudoTypeSelection -> "selection"
                DOMPseudoTypeTargetText -> "target-text"
                DOMPseudoTypeSpellingError -> "spelling-error"
                DOMPseudoTypeGrammarError -> "grammar-error"
                DOMPseudoTypeHighlight -> "highlight"
                DOMPseudoTypeFirstLineInherited -> "first-line-inherited"
                DOMPseudoTypeScrollbar -> "scrollbar"
                DOMPseudoTypeScrollbarThumb -> "scrollbar-thumb"
                DOMPseudoTypeScrollbarButton -> "scrollbar-button"
                DOMPseudoTypeScrollbarTrack -> "scrollbar-track"
                DOMPseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
                DOMPseudoTypeScrollbarCorner -> "scrollbar-corner"
                DOMPseudoTypeResizer -> "resizer"
                DOMPseudoTypeInputListButton -> "input-list-button"
                DOMPseudoTypePageTransition -> "page-transition"
                DOMPseudoTypePageTransitionContainer -> "page-transition-container"
                DOMPseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
                DOMPseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
                DOMPseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"



data DOMShadowRootType = DOMShadowRootTypeUserAgent | DOMShadowRootTypeOpen | DOMShadowRootTypeClosed
    deriving (Eq, Show, Read)
instance FromJSON DOMShadowRootType where
    parseJSON = A.withText  "DOMShadowRootType"  $ \v -> do
        pure $ case v of
                "user-agent" -> DOMShadowRootTypeUserAgent
                "open" -> DOMShadowRootTypeOpen
                "closed" -> DOMShadowRootTypeClosed
                _ -> error "failed to parse DOMShadowRootType"

instance ToJSON DOMShadowRootType where
    toJSON v = A.String $
        case v of
                DOMShadowRootTypeUserAgent -> "user-agent"
                DOMShadowRootTypeOpen -> "open"
                DOMShadowRootTypeClosed -> "closed"



data DOMCompatibilityMode = DOMCompatibilityModeQuirksMode | DOMCompatibilityModeLimitedQuirksMode | DOMCompatibilityModeNoQuirksMode
    deriving (Eq, Show, Read)
instance FromJSON DOMCompatibilityMode where
    parseJSON = A.withText  "DOMCompatibilityMode"  $ \v -> do
        pure $ case v of
                "QuirksMode" -> DOMCompatibilityModeQuirksMode
                "LimitedQuirksMode" -> DOMCompatibilityModeLimitedQuirksMode
                "NoQuirksMode" -> DOMCompatibilityModeNoQuirksMode
                _ -> error "failed to parse DOMCompatibilityMode"

instance ToJSON DOMCompatibilityMode where
    toJSON v = A.String $
        case v of
                DOMCompatibilityModeQuirksMode -> "QuirksMode"
                DOMCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
                DOMCompatibilityModeNoQuirksMode -> "NoQuirksMode"



data DOMNode = DOMNode {
    domNodeNodeId :: DOMNodeId,
    domNodeBackendNodeId :: DOMBackendNodeId,
    domNodeNodeType :: Int,
    domNodeNodeName :: String,
    domNodeLocalName :: String,
    domNodeNodeValue :: String,
    domNodeParentId :: Maybe DOMNodeId,
    domNodeChildNodeCount :: Maybe Int,
    domNodeChildren :: Maybe [DOMNode],
    domNodeAttributes :: Maybe [String],
    domNodeDocumentUrl :: Maybe String,
    domNodeBaseUrl :: Maybe String,
    domNodePublicId :: Maybe String,
    domNodeSystemId :: Maybe String,
    domNodeInternalSubset :: Maybe String,
    domNodeXmlVersion :: Maybe String,
    domNodeName :: Maybe String,
    domNodeValue :: Maybe String,
    domNodePseudoType :: Maybe DOMPseudoType,
    domNodeShadowRootType :: Maybe DOMShadowRootType,
    domNodeFrameId :: Maybe PageFrameId,
    domNodeContentDocument :: Maybe DOMNode,
    domNodeShadowRoots :: Maybe [DOMNode],
    domNodeTemplateContent :: Maybe DOMNode,
    domNodePseudoElements :: Maybe [DOMNode],
    domNodeDistributedNodes :: Maybe [DOMBackendNode],
    domNodeIsSvg :: Maybe Bool,
    domNodeCompatibilityMode :: Maybe DOMCompatibilityMode,
    domNodeAssignedSlot :: Maybe DOMBackendNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMNode where
    parseJSON = A.withObject "DOMNode" $ \v ->
         DOMNode <$> v .:  "nodeId"
            <*> v  .:  "backendNodeId"
            <*> v  .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "localName"
            <*> v  .:  "nodeValue"
            <*> v  .:?  "parentId"
            <*> v  .:?  "childNodeCount"
            <*> v  .:?  "children"
            <*> v  .:?  "attributes"
            <*> v  .:?  "documentURL"
            <*> v  .:?  "baseURL"
            <*> v  .:?  "publicId"
            <*> v  .:?  "systemId"
            <*> v  .:?  "internalSubset"
            <*> v  .:?  "xmlVersion"
            <*> v  .:?  "name"
            <*> v  .:?  "value"
            <*> v  .:?  "pseudoType"
            <*> v  .:?  "shadowRootType"
            <*> v  .:?  "frameId"
            <*> v  .:?  "contentDocument"
            <*> v  .:?  "shadowRoots"
            <*> v  .:?  "templateContent"
            <*> v  .:?  "pseudoElements"
            <*> v  .:?  "distributedNodes"
            <*> v  .:?  "isSVG"
            <*> v  .:?  "compatibilityMode"
            <*> v  .:?  "assignedSlot"


instance ToJSON DOMNode  where
    toJSON v = A.object
        [ "nodeId" .= domNodeNodeId v
        , "backendNodeId" .= domNodeBackendNodeId v
        , "nodeType" .= domNodeNodeType v
        , "nodeName" .= domNodeNodeName v
        , "localName" .= domNodeLocalName v
        , "nodeValue" .= domNodeNodeValue v
        , "parentId" .= domNodeParentId v
        , "childNodeCount" .= domNodeChildNodeCount v
        , "children" .= domNodeChildren v
        , "attributes" .= domNodeAttributes v
        , "documentURL" .= domNodeDocumentUrl v
        , "baseURL" .= domNodeBaseUrl v
        , "publicId" .= domNodePublicId v
        , "systemId" .= domNodeSystemId v
        , "internalSubset" .= domNodeInternalSubset v
        , "xmlVersion" .= domNodeXmlVersion v
        , "name" .= domNodeName v
        , "value" .= domNodeValue v
        , "pseudoType" .= domNodePseudoType v
        , "shadowRootType" .= domNodeShadowRootType v
        , "frameId" .= domNodeFrameId v
        , "contentDocument" .= domNodeContentDocument v
        , "shadowRoots" .= domNodeShadowRoots v
        , "templateContent" .= domNodeTemplateContent v
        , "pseudoElements" .= domNodePseudoElements v
        , "distributedNodes" .= domNodeDistributedNodes v
        , "isSVG" .= domNodeIsSvg v
        , "compatibilityMode" .= domNodeCompatibilityMode v
        , "assignedSlot" .= domNodeAssignedSlot v
        ]



data DOMRGBA = DOMRGBA {
    domrgbaR :: Int,
    domrgbaG :: Int,
    domrgbaB :: Int,
    domrgbaA :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMRGBA where
    parseJSON = A.withObject "DOMRGBA" $ \v ->
         DOMRGBA <$> v .:  "r"
            <*> v  .:  "g"
            <*> v  .:  "b"
            <*> v  .:?  "a"


instance ToJSON DOMRGBA  where
    toJSON v = A.object
        [ "r" .= domrgbaR v
        , "g" .= domrgbaG v
        , "b" .= domrgbaB v
        , "a" .= domrgbaA v
        ]



type DOMQuad = [Int]

data DOMBoxModel = DOMBoxModel {
    domBoxModelContent :: DOMQuad,
    domBoxModelPadding :: DOMQuad,
    domBoxModelBorder :: DOMQuad,
    domBoxModelMargin :: DOMQuad,
    domBoxModelWidth :: Int,
    domBoxModelHeight :: Int,
    domBoxModelShapeOutside :: Maybe DOMShapeOutsideInfo
} deriving (Eq, Show, Read)
instance FromJSON  DOMBoxModel where
    parseJSON = A.withObject "DOMBoxModel" $ \v ->
         DOMBoxModel <$> v .:  "content"
            <*> v  .:  "padding"
            <*> v  .:  "border"
            <*> v  .:  "margin"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:?  "shapeOutside"


instance ToJSON DOMBoxModel  where
    toJSON v = A.object
        [ "content" .= domBoxModelContent v
        , "padding" .= domBoxModelPadding v
        , "border" .= domBoxModelBorder v
        , "margin" .= domBoxModelMargin v
        , "width" .= domBoxModelWidth v
        , "height" .= domBoxModelHeight v
        , "shapeOutside" .= domBoxModelShapeOutside v
        ]



data DOMShapeOutsideInfo = DOMShapeOutsideInfo {
    domShapeOutsideInfoBounds :: DOMQuad,
    domShapeOutsideInfoShape :: [Int],
    domShapeOutsideInfoMarginShape :: [Int]
} deriving (Eq, Show, Read)
instance FromJSON  DOMShapeOutsideInfo where
    parseJSON = A.withObject "DOMShapeOutsideInfo" $ \v ->
         DOMShapeOutsideInfo <$> v .:  "bounds"
            <*> v  .:  "shape"
            <*> v  .:  "marginShape"


instance ToJSON DOMShapeOutsideInfo  where
    toJSON v = A.object
        [ "bounds" .= domShapeOutsideInfoBounds v
        , "shape" .= domShapeOutsideInfoShape v
        , "marginShape" .= domShapeOutsideInfoMarginShape v
        ]



data DOMRect = DOMRect {
    domRectX :: Int,
    domRectY :: Int,
    domRectWidth :: Int,
    domRectHeight :: Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMRect where
    parseJSON = A.withObject "DOMRect" $ \v ->
         DOMRect <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"


instance ToJSON DOMRect  where
    toJSON v = A.object
        [ "x" .= domRectX v
        , "y" .= domRectY v
        , "width" .= domRectWidth v
        , "height" .= domRectHeight v
        ]



data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty {
    domcssComputedStylePropertyName :: String,
    domcssComputedStylePropertyValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMCSSComputedStyleProperty where
    parseJSON = A.withObject "DOMCSSComputedStyleProperty" $ \v ->
         DOMCSSComputedStyleProperty <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON DOMCSSComputedStyleProperty  where
    toJSON v = A.object
        [ "name" .= domcssComputedStylePropertyName v
        , "value" .= domcssComputedStylePropertyValue v
        ]


data DomDescribeNode = DomDescribeNode {
    domDescribeNodeNode :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DomDescribeNode where
    parseJSON = A.withObject "DomDescribeNode" $ \v ->
         DomDescribeNode <$> v .:  "node"



domDescribeNode :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error DomDescribeNode)
domDescribeNode session domDescribeNodeNodeId domDescribeNodeBackendNodeId domDescribeNodeObjectId domDescribeNodeDepth domDescribeNodePierce = sendReceiveCommandResult (conn session) ("DOM","describeNode") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domDescribeNodeNodeId, fmap (("backendNodeId",) . ToJSONEx) domDescribeNodeBackendNodeId, fmap (("objectId",) . ToJSONEx) domDescribeNodeObjectId, fmap (("depth",) . ToJSONEx) domDescribeNodeDepth, fmap (("pierce",) . ToJSONEx) domDescribeNodePierce]))


domDisable :: Session -> IO (Maybe Error)
domDisable session  = sendReceiveCommand (conn session) ("DOM","disable") ([] ++ (catMaybes []))


domEnable :: Session -> IO (Maybe Error)
domEnable session  = sendReceiveCommand (conn session) ("DOM","enable") ([] ++ (catMaybes []))


domFocus :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Maybe Error)
domFocus session domFocusNodeId domFocusBackendNodeId domFocusObjectId = sendReceiveCommand (conn session) ("DOM","focus") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domFocusNodeId, fmap (("backendNodeId",) . ToJSONEx) domFocusBackendNodeId, fmap (("objectId",) . ToJSONEx) domFocusObjectId]))

data DomGetAttributes = DomGetAttributes {
    domGetAttributesAttributes :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  DomGetAttributes where
    parseJSON = A.withObject "DomGetAttributes" $ \v ->
         DomGetAttributes <$> v .:  "attributes"



domGetAttributes :: Session -> DOMNodeId -> IO (Either Error DomGetAttributes)
domGetAttributes session domGetAttributesNodeId = sendReceiveCommandResult (conn session) ("DOM","getAttributes") ([("nodeId", ToJSONEx domGetAttributesNodeId)] ++ (catMaybes []))

data DomGetBoxModel = DomGetBoxModel {
    domGetBoxModelModel :: DOMBoxModel
} deriving (Eq, Show, Read)
instance FromJSON  DomGetBoxModel where
    parseJSON = A.withObject "DomGetBoxModel" $ \v ->
         DomGetBoxModel <$> v .:  "model"



domGetBoxModel :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Either Error DomGetBoxModel)
domGetBoxModel session domGetBoxModelNodeId domGetBoxModelBackendNodeId domGetBoxModelObjectId = sendReceiveCommandResult (conn session) ("DOM","getBoxModel") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domGetBoxModelNodeId, fmap (("backendNodeId",) . ToJSONEx) domGetBoxModelBackendNodeId, fmap (("objectId",) . ToJSONEx) domGetBoxModelObjectId]))

data DomGetDocument = DomGetDocument {
    domGetDocumentRoot :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DomGetDocument where
    parseJSON = A.withObject "DomGetDocument" $ \v ->
         DomGetDocument <$> v .:  "root"



domGetDocument :: Session -> Maybe Int -> Maybe Bool -> IO (Either Error DomGetDocument)
domGetDocument session domGetDocumentDepth domGetDocumentPierce = sendReceiveCommandResult (conn session) ("DOM","getDocument") ([] ++ (catMaybes [fmap (("depth",) . ToJSONEx) domGetDocumentDepth, fmap (("pierce",) . ToJSONEx) domGetDocumentPierce]))

data DomGetNodeForLocation = DomGetNodeForLocation {
    domGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    domGetNodeForLocationFrameId :: PageFrameId,
    domGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DomGetNodeForLocation where
    parseJSON = A.withObject "DomGetNodeForLocation" $ \v ->
         DomGetNodeForLocation <$> v .:  "backendNodeId"
            <*> v  .:  "frameId"
            <*> v  .:?  "nodeId"



domGetNodeForLocation :: Session -> Int -> Int -> Maybe Bool -> Maybe Bool -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation session domGetNodeForLocationX domGetNodeForLocationY domGetNodeForLocationIncludeUserAgentShadowDom domGetNodeForLocationIgnorePointerEventsNone = sendReceiveCommandResult (conn session) ("DOM","getNodeForLocation") ([("x", ToJSONEx domGetNodeForLocationX), ("y", ToJSONEx domGetNodeForLocationY)] ++ (catMaybes [fmap (("includeUserAgentShadowDOM",) . ToJSONEx) domGetNodeForLocationIncludeUserAgentShadowDom, fmap (("ignorePointerEventsNone",) . ToJSONEx) domGetNodeForLocationIgnorePointerEventsNone]))

data DomGetOuterHTML = DomGetOuterHTML {
    domGetOuterHTMLOuterHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  DomGetOuterHTML where
    parseJSON = A.withObject "DomGetOuterHTML" $ \v ->
         DomGetOuterHTML <$> v .:  "outerHTML"



domGetOuterHTML :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Either Error DomGetOuterHTML)
domGetOuterHTML session domGetOuterHtmlNodeId domGetOuterHtmlBackendNodeId domGetOuterHtmlObjectId = sendReceiveCommandResult (conn session) ("DOM","getOuterHTML") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domGetOuterHtmlNodeId, fmap (("backendNodeId",) . ToJSONEx) domGetOuterHtmlBackendNodeId, fmap (("objectId",) . ToJSONEx) domGetOuterHtmlObjectId]))


domHideHighlight :: Session -> IO (Maybe Error)
domHideHighlight session  = sendReceiveCommand (conn session) ("DOM","hideHighlight") ([] ++ (catMaybes []))


domHighlightNode :: Session -> IO (Maybe Error)
domHighlightNode session  = sendReceiveCommand (conn session) ("DOM","highlightNode") ([] ++ (catMaybes []))


domHighlightRect :: Session -> IO (Maybe Error)
domHighlightRect session  = sendReceiveCommand (conn session) ("DOM","highlightRect") ([] ++ (catMaybes []))

data DomMoveTo = DomMoveTo {
    domMoveToNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DomMoveTo where
    parseJSON = A.withObject "DomMoveTo" $ \v ->
         DomMoveTo <$> v .:  "nodeId"



domMoveTo :: Session -> DOMNodeId -> DOMNodeId -> Maybe DOMNodeId -> IO (Either Error DomMoveTo)
domMoveTo session domMoveToNodeId domMoveToTargetNodeId domMoveToInsertBeforeNodeId = sendReceiveCommandResult (conn session) ("DOM","moveTo") ([("nodeId", ToJSONEx domMoveToNodeId), ("targetNodeId", ToJSONEx domMoveToTargetNodeId)] ++ (catMaybes [fmap (("insertBeforeNodeId",) . ToJSONEx) domMoveToInsertBeforeNodeId]))

data DomQuerySelector = DomQuerySelector {
    domQuerySelectorNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DomQuerySelector where
    parseJSON = A.withObject "DomQuerySelector" $ \v ->
         DomQuerySelector <$> v .:  "nodeId"



domQuerySelector :: Session -> DOMNodeId -> String -> IO (Either Error DomQuerySelector)
domQuerySelector session domQuerySelectorNodeId domQuerySelectorSelector = sendReceiveCommandResult (conn session) ("DOM","querySelector") ([("nodeId", ToJSONEx domQuerySelectorNodeId), ("selector", ToJSONEx domQuerySelectorSelector)] ++ (catMaybes []))

data DomQuerySelectorAll = DomQuerySelectorAll {
    domQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving (Eq, Show, Read)
instance FromJSON  DomQuerySelectorAll where
    parseJSON = A.withObject "DomQuerySelectorAll" $ \v ->
         DomQuerySelectorAll <$> v .:  "nodeIds"



domQuerySelectorAll :: Session -> DOMNodeId -> String -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll session domQuerySelectorAllNodeId domQuerySelectorAllSelector = sendReceiveCommandResult (conn session) ("DOM","querySelectorAll") ([("nodeId", ToJSONEx domQuerySelectorAllNodeId), ("selector", ToJSONEx domQuerySelectorAllSelector)] ++ (catMaybes []))


domRemoveAttribute :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domRemoveAttribute session domRemoveAttributeNodeId domRemoveAttributeName = sendReceiveCommand (conn session) ("DOM","removeAttribute") ([("nodeId", ToJSONEx domRemoveAttributeNodeId), ("name", ToJSONEx domRemoveAttributeName)] ++ (catMaybes []))


domRemoveNode :: Session -> DOMNodeId -> IO (Maybe Error)
domRemoveNode session domRemoveNodeNodeId = sendReceiveCommand (conn session) ("DOM","removeNode") ([("nodeId", ToJSONEx domRemoveNodeNodeId)] ++ (catMaybes []))


domRequestChildNodes :: Session -> DOMNodeId -> Maybe Int -> Maybe Bool -> IO (Maybe Error)
domRequestChildNodes session domRequestChildNodesNodeId domRequestChildNodesDepth domRequestChildNodesPierce = sendReceiveCommand (conn session) ("DOM","requestChildNodes") ([("nodeId", ToJSONEx domRequestChildNodesNodeId)] ++ (catMaybes [fmap (("depth",) . ToJSONEx) domRequestChildNodesDepth, fmap (("pierce",) . ToJSONEx) domRequestChildNodesPierce]))

data DomRequestNode = DomRequestNode {
    domRequestNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DomRequestNode where
    parseJSON = A.withObject "DomRequestNode" $ \v ->
         DomRequestNode <$> v .:  "nodeId"



domRequestNode :: Session -> RuntimeRemoteObjectId -> IO (Either Error DomRequestNode)
domRequestNode session domRequestNodeObjectId = sendReceiveCommandResult (conn session) ("DOM","requestNode") ([("objectId", ToJSONEx domRequestNodeObjectId)] ++ (catMaybes []))

data DomResolveNode = DomResolveNode {
    domResolveNodeObject :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  DomResolveNode where
    parseJSON = A.withObject "DomResolveNode" $ \v ->
         DomResolveNode <$> v .:  "object"



domResolveNode :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe String -> Maybe RuntimeExecutionContextId -> IO (Either Error DomResolveNode)
domResolveNode session domResolveNodeNodeId domResolveNodeBackendNodeId domResolveNodeObjectGroup domResolveNodeExecutionContextId = sendReceiveCommandResult (conn session) ("DOM","resolveNode") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domResolveNodeNodeId, fmap (("backendNodeId",) . ToJSONEx) domResolveNodeBackendNodeId, fmap (("objectGroup",) . ToJSONEx) domResolveNodeObjectGroup, fmap (("executionContextId",) . ToJSONEx) domResolveNodeExecutionContextId]))


domSetAttributeValue :: Session -> DOMNodeId -> String -> String -> IO (Maybe Error)
domSetAttributeValue session domSetAttributeValueNodeId domSetAttributeValueName domSetAttributeValueValue = sendReceiveCommand (conn session) ("DOM","setAttributeValue") ([("nodeId", ToJSONEx domSetAttributeValueNodeId), ("name", ToJSONEx domSetAttributeValueName), ("value", ToJSONEx domSetAttributeValueValue)] ++ (catMaybes []))


domSetAttributesAsText :: Session -> DOMNodeId -> String -> Maybe String -> IO (Maybe Error)
domSetAttributesAsText session domSetAttributesAsTextNodeId domSetAttributesAsTextText domSetAttributesAsTextName = sendReceiveCommand (conn session) ("DOM","setAttributesAsText") ([("nodeId", ToJSONEx domSetAttributesAsTextNodeId), ("text", ToJSONEx domSetAttributesAsTextText)] ++ (catMaybes [fmap (("name",) . ToJSONEx) domSetAttributesAsTextName]))


domSetFileInputFiles :: Session -> [String] -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Maybe Error)
domSetFileInputFiles session domSetFileInputFilesFiles domSetFileInputFilesNodeId domSetFileInputFilesBackendNodeId domSetFileInputFilesObjectId = sendReceiveCommand (conn session) ("DOM","setFileInputFiles") ([("files", ToJSONEx domSetFileInputFilesFiles)] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) domSetFileInputFilesNodeId, fmap (("backendNodeId",) . ToJSONEx) domSetFileInputFilesBackendNodeId, fmap (("objectId",) . ToJSONEx) domSetFileInputFilesObjectId]))

data DomSetNodeName = DomSetNodeName {
    domSetNodeNameNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DomSetNodeName where
    parseJSON = A.withObject "DomSetNodeName" $ \v ->
         DomSetNodeName <$> v .:  "nodeId"



domSetNodeName :: Session -> DOMNodeId -> String -> IO (Either Error DomSetNodeName)
domSetNodeName session domSetNodeNameNodeId domSetNodeNameName = sendReceiveCommandResult (conn session) ("DOM","setNodeName") ([("nodeId", ToJSONEx domSetNodeNameNodeId), ("name", ToJSONEx domSetNodeNameName)] ++ (catMaybes []))


domSetNodeValue :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domSetNodeValue session domSetNodeValueNodeId domSetNodeValueValue = sendReceiveCommand (conn session) ("DOM","setNodeValue") ([("nodeId", ToJSONEx domSetNodeValueNodeId), ("value", ToJSONEx domSetNodeValueValue)] ++ (catMaybes []))


domSetOuterHTML :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domSetOuterHTML session domSetOuterHtmlNodeId domSetOuterHtmlOuterHtml = sendReceiveCommand (conn session) ("DOM","setOuterHTML") ([("nodeId", ToJSONEx domSetOuterHtmlNodeId), ("outerHTML", ToJSONEx domSetOuterHtmlOuterHtml)] ++ (catMaybes []))




data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
    deriving (Eq, Show, Read)
instance FromJSON DOMDebuggerDOMBreakpointType where
    parseJSON = A.withText  "DOMDebuggerDOMBreakpointType"  $ \v -> do
        pure $ case v of
                "subtree-modified" -> DOMDebuggerDOMBreakpointTypeSubtreeModified
                "attribute-modified" -> DOMDebuggerDOMBreakpointTypeAttributeModified
                "node-removed" -> DOMDebuggerDOMBreakpointTypeNodeRemoved
                _ -> error "failed to parse DOMDebuggerDOMBreakpointType"

instance ToJSON DOMDebuggerDOMBreakpointType where
    toJSON v = A.String $
        case v of
                DOMDebuggerDOMBreakpointTypeSubtreeModified -> "subtree-modified"
                DOMDebuggerDOMBreakpointTypeAttributeModified -> "attribute-modified"
                DOMDebuggerDOMBreakpointTypeNodeRemoved -> "node-removed"



data DOMDebuggerEventListener = DOMDebuggerEventListener {
    domDebuggerEventListenerType :: String,
    domDebuggerEventListenerUseCapture :: Bool,
    domDebuggerEventListenerPassive :: Bool,
    domDebuggerEventListenerOnce :: Bool,
    domDebuggerEventListenerScriptId :: RuntimeScriptId,
    domDebuggerEventListenerLineNumber :: Int,
    domDebuggerEventListenerColumnNumber :: Int,
    domDebuggerEventListenerHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerOriginalHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerBackendNodeId :: Maybe DOMBackendNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMDebuggerEventListener where
    parseJSON = A.withObject "DOMDebuggerEventListener" $ \v ->
         DOMDebuggerEventListener <$> v .:  "type"
            <*> v  .:  "useCapture"
            <*> v  .:  "passive"
            <*> v  .:  "once"
            <*> v  .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "handler"
            <*> v  .:?  "originalHandler"
            <*> v  .:?  "backendNodeId"


instance ToJSON DOMDebuggerEventListener  where
    toJSON v = A.object
        [ "type" .= domDebuggerEventListenerType v
        , "useCapture" .= domDebuggerEventListenerUseCapture v
        , "passive" .= domDebuggerEventListenerPassive v
        , "once" .= domDebuggerEventListenerOnce v
        , "scriptId" .= domDebuggerEventListenerScriptId v
        , "lineNumber" .= domDebuggerEventListenerLineNumber v
        , "columnNumber" .= domDebuggerEventListenerColumnNumber v
        , "handler" .= domDebuggerEventListenerHandler v
        , "originalHandler" .= domDebuggerEventListenerOriginalHandler v
        , "backendNodeId" .= domDebuggerEventListenerBackendNodeId v
        ]


data DomDebuggerGetEventListeners = DomDebuggerGetEventListeners {
    domDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Eq, Show, Read)
instance FromJSON  DomDebuggerGetEventListeners where
    parseJSON = A.withObject "DomDebuggerGetEventListeners" $ \v ->
         DomDebuggerGetEventListeners <$> v .:  "listeners"



domDebuggerGetEventListeners :: Session -> RuntimeRemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error DomDebuggerGetEventListeners)
domDebuggerGetEventListeners session domDebuggerGetEventListenersObjectId domDebuggerGetEventListenersDepth domDebuggerGetEventListenersPierce = sendReceiveCommandResult (conn session) ("DOMDebugger","getEventListeners") ([("objectId", ToJSONEx domDebuggerGetEventListenersObjectId)] ++ (catMaybes [fmap (("depth",) . ToJSONEx) domDebuggerGetEventListenersDepth, fmap (("pierce",) . ToJSONEx) domDebuggerGetEventListenersPierce]))


domDebuggerRemoveDOMBreakpoint :: Session -> DOMNodeId -> DOMDebuggerDOMBreakpointType -> IO (Maybe Error)
domDebuggerRemoveDOMBreakpoint session domDebuggerRemoveDomBreakpointNodeId domDebuggerRemoveDomBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","removeDOMBreakpoint") ([("nodeId", ToJSONEx domDebuggerRemoveDomBreakpointNodeId), ("type", ToJSONEx domDebuggerRemoveDomBreakpointType)] ++ (catMaybes []))


domDebuggerRemoveEventListenerBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerRemoveEventListenerBreakpoint session domDebuggerRemoveEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","removeEventListenerBreakpoint") ([("eventName", ToJSONEx domDebuggerRemoveEventListenerBreakpointEventName)] ++ (catMaybes []))


domDebuggerRemoveXHRBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerRemoveXHRBreakpoint session domDebuggerRemoveXhrBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","removeXHRBreakpoint") ([("url", ToJSONEx domDebuggerRemoveXhrBreakpointUrl)] ++ (catMaybes []))


domDebuggerSetDOMBreakpoint :: Session -> DOMNodeId -> DOMDebuggerDOMBreakpointType -> IO (Maybe Error)
domDebuggerSetDOMBreakpoint session domDebuggerSetDomBreakpointNodeId domDebuggerSetDomBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","setDOMBreakpoint") ([("nodeId", ToJSONEx domDebuggerSetDomBreakpointNodeId), ("type", ToJSONEx domDebuggerSetDomBreakpointType)] ++ (catMaybes []))


domDebuggerSetEventListenerBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerSetEventListenerBreakpoint session domDebuggerSetEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","setEventListenerBreakpoint") ([("eventName", ToJSONEx domDebuggerSetEventListenerBreakpointEventName)] ++ (catMaybes []))


domDebuggerSetXHRBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerSetXHRBreakpoint session domDebuggerSetXhrBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","setXHRBreakpoint") ([("url", ToJSONEx domDebuggerSetXhrBreakpointUrl)] ++ (catMaybes []))




data EmulationScreenOrientation = EmulationScreenOrientation {
    emulationScreenOrientationType :: String,
    emulationScreenOrientationAngle :: Int
} deriving (Eq, Show, Read)
instance FromJSON  EmulationScreenOrientation where
    parseJSON = A.withObject "EmulationScreenOrientation" $ \v ->
         EmulationScreenOrientation <$> v .:  "type"
            <*> v  .:  "angle"


instance ToJSON EmulationScreenOrientation  where
    toJSON v = A.object
        [ "type" .= emulationScreenOrientationType v
        , "angle" .= emulationScreenOrientationAngle v
        ]



data EmulationDisplayFeature = EmulationDisplayFeature {
    emulationDisplayFeatureOrientation :: String,
    emulationDisplayFeatureOffset :: Int,
    emulationDisplayFeatureMaskLength :: Int
} deriving (Eq, Show, Read)
instance FromJSON  EmulationDisplayFeature where
    parseJSON = A.withObject "EmulationDisplayFeature" $ \v ->
         EmulationDisplayFeature <$> v .:  "orientation"
            <*> v  .:  "offset"
            <*> v  .:  "maskLength"


instance ToJSON EmulationDisplayFeature  where
    toJSON v = A.object
        [ "orientation" .= emulationDisplayFeatureOrientation v
        , "offset" .= emulationDisplayFeatureOffset v
        , "maskLength" .= emulationDisplayFeatureMaskLength v
        ]



data EmulationMediaFeature = EmulationMediaFeature {
    emulationMediaFeatureName :: String,
    emulationMediaFeatureValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  EmulationMediaFeature where
    parseJSON = A.withObject "EmulationMediaFeature" $ \v ->
         EmulationMediaFeature <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON EmulationMediaFeature  where
    toJSON v = A.object
        [ "name" .= emulationMediaFeatureName v
        , "value" .= emulationMediaFeatureValue v
        ]


data EmulationCanEmulate = EmulationCanEmulate {
    emulationCanEmulateResult :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  EmulationCanEmulate where
    parseJSON = A.withObject "EmulationCanEmulate" $ \v ->
         EmulationCanEmulate <$> v .:  "result"



emulationCanEmulate :: Session -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate session  = sendReceiveCommandResult (conn session) ("Emulation","canEmulate") ([] ++ (catMaybes []))


emulationClearDeviceMetricsOverride :: Session -> IO (Maybe Error)
emulationClearDeviceMetricsOverride session  = sendReceiveCommand (conn session) ("Emulation","clearDeviceMetricsOverride") ([] ++ (catMaybes []))


emulationClearGeolocationOverride :: Session -> IO (Maybe Error)
emulationClearGeolocationOverride session  = sendReceiveCommand (conn session) ("Emulation","clearGeolocationOverride") ([] ++ (catMaybes []))


emulationSetDefaultBackgroundColorOverride :: Session -> Maybe DOMRGBA -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride session emulationSetDefaultBackgroundColorOverrideColor = sendReceiveCommand (conn session) ("Emulation","setDefaultBackgroundColorOverride") ([] ++ (catMaybes [fmap (("color",) . ToJSONEx) emulationSetDefaultBackgroundColorOverrideColor]))


emulationSetDeviceMetricsOverride :: Session -> Int -> Int -> Int -> Bool -> Maybe EmulationScreenOrientation -> IO (Maybe Error)
emulationSetDeviceMetricsOverride session emulationSetDeviceMetricsOverrideWidth emulationSetDeviceMetricsOverrideHeight emulationSetDeviceMetricsOverrideDeviceScaleFactor emulationSetDeviceMetricsOverrideMobile emulationSetDeviceMetricsOverrideScreenOrientation = sendReceiveCommand (conn session) ("Emulation","setDeviceMetricsOverride") ([("width", ToJSONEx emulationSetDeviceMetricsOverrideWidth), ("height", ToJSONEx emulationSetDeviceMetricsOverrideHeight), ("deviceScaleFactor", ToJSONEx emulationSetDeviceMetricsOverrideDeviceScaleFactor), ("mobile", ToJSONEx emulationSetDeviceMetricsOverrideMobile)] ++ (catMaybes [fmap (("screenOrientation",) . ToJSONEx) emulationSetDeviceMetricsOverrideScreenOrientation]))


emulationSetEmulatedMedia :: Session -> Maybe String -> Maybe [EmulationMediaFeature] -> IO (Maybe Error)
emulationSetEmulatedMedia session emulationSetEmulatedMediaMedia emulationSetEmulatedMediaFeatures = sendReceiveCommand (conn session) ("Emulation","setEmulatedMedia") ([] ++ (catMaybes [fmap (("media",) . ToJSONEx) emulationSetEmulatedMediaMedia, fmap (("features",) . ToJSONEx) emulationSetEmulatedMediaFeatures]))


emulationSetGeolocationOverride :: Session -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Maybe Error)
emulationSetGeolocationOverride session emulationSetGeolocationOverrideLatitude emulationSetGeolocationOverrideLongitude emulationSetGeolocationOverrideAccuracy = sendReceiveCommand (conn session) ("Emulation","setGeolocationOverride") ([] ++ (catMaybes [fmap (("latitude",) . ToJSONEx) emulationSetGeolocationOverrideLatitude, fmap (("longitude",) . ToJSONEx) emulationSetGeolocationOverrideLongitude, fmap (("accuracy",) . ToJSONEx) emulationSetGeolocationOverrideAccuracy]))


emulationSetScriptExecutionDisabled :: Session -> Bool -> IO (Maybe Error)
emulationSetScriptExecutionDisabled session emulationSetScriptExecutionDisabledValue = sendReceiveCommand (conn session) ("Emulation","setScriptExecutionDisabled") ([("value", ToJSONEx emulationSetScriptExecutionDisabledValue)] ++ (catMaybes []))


emulationSetTouchEmulationEnabled :: Session -> Bool -> Maybe Int -> IO (Maybe Error)
emulationSetTouchEmulationEnabled session emulationSetTouchEmulationEnabledEnabled emulationSetTouchEmulationEnabledMaxTouchPoints = sendReceiveCommand (conn session) ("Emulation","setTouchEmulationEnabled") ([("enabled", ToJSONEx emulationSetTouchEmulationEnabledEnabled)] ++ (catMaybes [fmap (("maxTouchPoints",) . ToJSONEx) emulationSetTouchEmulationEnabledMaxTouchPoints]))


emulationSetUserAgentOverride :: Session -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
emulationSetUserAgentOverride session emulationSetUserAgentOverrideUserAgent emulationSetUserAgentOverrideAcceptLanguage emulationSetUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Emulation","setUserAgentOverride") ([("userAgent", ToJSONEx emulationSetUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("acceptLanguage",) . ToJSONEx) emulationSetUserAgentOverrideAcceptLanguage, fmap (("platform",) . ToJSONEx) emulationSetUserAgentOverridePlatform]))




type IOStreamHandle = String

ioClose :: Session -> IOStreamHandle -> IO (Maybe Error)
ioClose session ioCloseHandle = sendReceiveCommand (conn session) ("IO","close") ([("handle", ToJSONEx ioCloseHandle)] ++ (catMaybes []))

data IoRead = IoRead {
    ioReadData :: String,
    ioReadEof :: Bool,
    ioReadBase64Encoded :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  IoRead where
    parseJSON = A.withObject "IoRead" $ \v ->
         IoRead <$> v .:  "data"
            <*> v  .:  "eof"
            <*> v  .:?  "base64Encoded"



ioRead :: Session -> IOStreamHandle -> Maybe Int -> Maybe Int -> IO (Either Error IoRead)
ioRead session ioReadHandle ioReadOffset ioReadSize = sendReceiveCommandResult (conn session) ("IO","read") ([("handle", ToJSONEx ioReadHandle)] ++ (catMaybes [fmap (("offset",) . ToJSONEx) ioReadOffset, fmap (("size",) . ToJSONEx) ioReadSize]))

data IoResolveBlob = IoResolveBlob {
    ioResolveBlobUuid :: String
} deriving (Eq, Show, Read)
instance FromJSON  IoResolveBlob where
    parseJSON = A.withObject "IoResolveBlob" $ \v ->
         IoResolveBlob <$> v .:  "uuid"



ioResolveBlob :: Session -> RuntimeRemoteObjectId -> IO (Either Error IoResolveBlob)
ioResolveBlob session ioResolveBlobObjectId = sendReceiveCommandResult (conn session) ("IO","resolveBlob") ([("objectId", ToJSONEx ioResolveBlobObjectId)] ++ (catMaybes []))




data InputTouchPoint = InputTouchPoint {
    inputTouchPointX :: Int,
    inputTouchPointY :: Int,
    inputTouchPointRadiusX :: Maybe Int,
    inputTouchPointRadiusY :: Maybe Int,
    inputTouchPointRotationAngle :: Maybe Int,
    inputTouchPointForce :: Maybe Int,
    inputTouchPointId :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  InputTouchPoint where
    parseJSON = A.withObject "InputTouchPoint" $ \v ->
         InputTouchPoint <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "radiusX"
            <*> v  .:?  "radiusY"
            <*> v  .:?  "rotationAngle"
            <*> v  .:?  "force"
            <*> v  .:?  "id"


instance ToJSON InputTouchPoint  where
    toJSON v = A.object
        [ "x" .= inputTouchPointX v
        , "y" .= inputTouchPointY v
        , "radiusX" .= inputTouchPointRadiusX v
        , "radiusY" .= inputTouchPointRadiusY v
        , "rotationAngle" .= inputTouchPointRotationAngle v
        , "force" .= inputTouchPointForce v
        , "id" .= inputTouchPointId v
        ]



data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
    deriving (Eq, Show, Read)
instance FromJSON InputMouseButton where
    parseJSON = A.withText  "InputMouseButton"  $ \v -> do
        pure $ case v of
                "none" -> InputMouseButtonNone
                "left" -> InputMouseButtonLeft
                "middle" -> InputMouseButtonMiddle
                "right" -> InputMouseButtonRight
                "back" -> InputMouseButtonBack
                "forward" -> InputMouseButtonForward
                _ -> error "failed to parse InputMouseButton"

instance ToJSON InputMouseButton where
    toJSON v = A.String $
        case v of
                InputMouseButtonNone -> "none"
                InputMouseButtonLeft -> "left"
                InputMouseButtonMiddle -> "middle"
                InputMouseButtonRight -> "right"
                InputMouseButtonBack -> "back"
                InputMouseButtonForward -> "forward"



type InputTimeSinceEpoch = Int

inputDispatchKeyEvent :: Session -> String -> Maybe Int -> Maybe InputTimeSinceEpoch -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> IO (Maybe Error)
inputDispatchKeyEvent session inputDispatchKeyEventType inputDispatchKeyEventModifiers inputDispatchKeyEventTimestamp inputDispatchKeyEventText inputDispatchKeyEventUnmodifiedText inputDispatchKeyEventKeyIdentifier inputDispatchKeyEventCode inputDispatchKeyEventKey inputDispatchKeyEventWindowsVirtualKeyCode inputDispatchKeyEventNativeVirtualKeyCode inputDispatchKeyEventAutoRepeat inputDispatchKeyEventIsKeypad inputDispatchKeyEventIsSystemKey inputDispatchKeyEventLocation = sendReceiveCommand (conn session) ("Input","dispatchKeyEvent") ([("type", ToJSONEx inputDispatchKeyEventType)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) inputDispatchKeyEventModifiers, fmap (("timestamp",) . ToJSONEx) inputDispatchKeyEventTimestamp, fmap (("text",) . ToJSONEx) inputDispatchKeyEventText, fmap (("unmodifiedText",) . ToJSONEx) inputDispatchKeyEventUnmodifiedText, fmap (("keyIdentifier",) . ToJSONEx) inputDispatchKeyEventKeyIdentifier, fmap (("code",) . ToJSONEx) inputDispatchKeyEventCode, fmap (("key",) . ToJSONEx) inputDispatchKeyEventKey, fmap (("windowsVirtualKeyCode",) . ToJSONEx) inputDispatchKeyEventWindowsVirtualKeyCode, fmap (("nativeVirtualKeyCode",) . ToJSONEx) inputDispatchKeyEventNativeVirtualKeyCode, fmap (("autoRepeat",) . ToJSONEx) inputDispatchKeyEventAutoRepeat, fmap (("isKeypad",) . ToJSONEx) inputDispatchKeyEventIsKeypad, fmap (("isSystemKey",) . ToJSONEx) inputDispatchKeyEventIsSystemKey, fmap (("location",) . ToJSONEx) inputDispatchKeyEventLocation]))


inputDispatchMouseEvent :: Session -> String -> Int -> Int -> Maybe Int -> Maybe InputTimeSinceEpoch -> Maybe InputMouseButton -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> IO (Maybe Error)
inputDispatchMouseEvent session inputDispatchMouseEventType inputDispatchMouseEventX inputDispatchMouseEventY inputDispatchMouseEventModifiers inputDispatchMouseEventTimestamp inputDispatchMouseEventButton inputDispatchMouseEventButtons inputDispatchMouseEventClickCount inputDispatchMouseEventDeltaX inputDispatchMouseEventDeltaY inputDispatchMouseEventPointerType = sendReceiveCommand (conn session) ("Input","dispatchMouseEvent") ([("type", ToJSONEx inputDispatchMouseEventType), ("x", ToJSONEx inputDispatchMouseEventX), ("y", ToJSONEx inputDispatchMouseEventY)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) inputDispatchMouseEventModifiers, fmap (("timestamp",) . ToJSONEx) inputDispatchMouseEventTimestamp, fmap (("button",) . ToJSONEx) inputDispatchMouseEventButton, fmap (("buttons",) . ToJSONEx) inputDispatchMouseEventButtons, fmap (("clickCount",) . ToJSONEx) inputDispatchMouseEventClickCount, fmap (("deltaX",) . ToJSONEx) inputDispatchMouseEventDeltaX, fmap (("deltaY",) . ToJSONEx) inputDispatchMouseEventDeltaY, fmap (("pointerType",) . ToJSONEx) inputDispatchMouseEventPointerType]))


inputDispatchTouchEvent :: Session -> String -> [InputTouchPoint] -> Maybe Int -> Maybe InputTimeSinceEpoch -> IO (Maybe Error)
inputDispatchTouchEvent session inputDispatchTouchEventType inputDispatchTouchEventTouchPoints inputDispatchTouchEventModifiers inputDispatchTouchEventTimestamp = sendReceiveCommand (conn session) ("Input","dispatchTouchEvent") ([("type", ToJSONEx inputDispatchTouchEventType), ("touchPoints", ToJSONEx inputDispatchTouchEventTouchPoints)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) inputDispatchTouchEventModifiers, fmap (("timestamp",) . ToJSONEx) inputDispatchTouchEventTimestamp]))


inputSetIgnoreInputEvents :: Session -> Bool -> IO (Maybe Error)
inputSetIgnoreInputEvents session inputSetIgnoreInputEventsIgnore = sendReceiveCommand (conn session) ("Input","setIgnoreInputEvents") ([("ignore", ToJSONEx inputSetIgnoreInputEventsIgnore)] ++ (catMaybes []))



data LogEntryAdded = LogEntryAdded {
    logEntryAddedEntry :: LogLogEntry
} deriving (Eq, Show, Read)
instance FromJSON  LogEntryAdded where
    parseJSON = A.withObject "LogEntryAdded" $ \v ->
         LogEntryAdded <$> v .:  "entry"


instance ToJSON LogEntryAdded  where
    toJSON v = A.object
        [ "entry" .= logEntryAddedEntry v
        ]


instance Event  LogEntryAdded where
    eventName  _   =  "Log.entryAdded"
    fToHandler _   =  HLogEntryAdded
    handlerToF _ h =  case h of HLogEntryAdded f -> Just f; _ -> Nothing


data LogLogEntry = LogLogEntry {
    logLogEntrySource :: String,
    logLogEntryLevel :: String,
    logLogEntryText :: String,
    logLogEntryTimestamp :: RuntimeTimestamp,
    logLogEntryCategory :: Maybe String,
    logLogEntryUrl :: Maybe String,
    logLogEntryLineNumber :: Maybe Int,
    logLogEntryStackTrace :: Maybe RuntimeStackTrace,
    logLogEntryNetworkRequestId :: Maybe NetworkRequestId,
    logLogEntryWorkerId :: Maybe String,
    logLogEntryArgs :: Maybe [RuntimeRemoteObject]
} deriving (Eq, Show, Read)
instance FromJSON  LogLogEntry where
    parseJSON = A.withObject "LogLogEntry" $ \v ->
         LogLogEntry <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:  "timestamp"
            <*> v  .:?  "category"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "networkRequestId"
            <*> v  .:?  "workerId"
            <*> v  .:?  "args"


instance ToJSON LogLogEntry  where
    toJSON v = A.object
        [ "source" .= logLogEntrySource v
        , "level" .= logLogEntryLevel v
        , "text" .= logLogEntryText v
        , "timestamp" .= logLogEntryTimestamp v
        , "category" .= logLogEntryCategory v
        , "url" .= logLogEntryUrl v
        , "lineNumber" .= logLogEntryLineNumber v
        , "stackTrace" .= logLogEntryStackTrace v
        , "networkRequestId" .= logLogEntryNetworkRequestId v
        , "workerId" .= logLogEntryWorkerId v
        , "args" .= logLogEntryArgs v
        ]



data LogViolationSetting = LogViolationSetting {
    logViolationSettingName :: String,
    logViolationSettingThreshold :: Int
} deriving (Eq, Show, Read)
instance FromJSON  LogViolationSetting where
    parseJSON = A.withObject "LogViolationSetting" $ \v ->
         LogViolationSetting <$> v .:  "name"
            <*> v  .:  "threshold"


instance ToJSON LogViolationSetting  where
    toJSON v = A.object
        [ "name" .= logViolationSettingName v
        , "threshold" .= logViolationSettingThreshold v
        ]



logClear :: Session -> IO (Maybe Error)
logClear session  = sendReceiveCommand (conn session) ("Log","clear") ([] ++ (catMaybes []))


logDisable :: Session -> IO (Maybe Error)
logDisable session  = sendReceiveCommand (conn session) ("Log","disable") ([] ++ (catMaybes []))


logEnable :: Session -> IO (Maybe Error)
logEnable session  = sendReceiveCommand (conn session) ("Log","enable") ([] ++ (catMaybes []))


logStartViolationsReport :: Session -> [LogViolationSetting] -> IO (Maybe Error)
logStartViolationsReport session logStartViolationsReportConfig = sendReceiveCommand (conn session) ("Log","startViolationsReport") ([("config", ToJSONEx logStartViolationsReportConfig)] ++ (catMaybes []))


logStopViolationsReport :: Session -> IO (Maybe Error)
logStopViolationsReport session  = sendReceiveCommand (conn session) ("Log","stopViolationsReport") ([] ++ (catMaybes []))



data NetworkDataReceived = NetworkDataReceived {
    networkDataReceivedRequestId :: NetworkRequestId,
    networkDataReceivedTimestamp :: NetworkMonotonicTime,
    networkDataReceivedDataLength :: Int,
    networkDataReceivedEncodedDataLength :: Int
} deriving (Eq, Show, Read)
instance FromJSON  NetworkDataReceived where
    parseJSON = A.withObject "NetworkDataReceived" $ \v ->
         NetworkDataReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "dataLength"
            <*> v  .:  "encodedDataLength"


instance ToJSON NetworkDataReceived  where
    toJSON v = A.object
        [ "requestId" .= networkDataReceivedRequestId v
        , "timestamp" .= networkDataReceivedTimestamp v
        , "dataLength" .= networkDataReceivedDataLength v
        , "encodedDataLength" .= networkDataReceivedEncodedDataLength v
        ]


instance Event  NetworkDataReceived where
    eventName  _   =  "Network.dataReceived"
    fToHandler _   =  HNetworkDataReceived
    handlerToF _ h =  case h of HNetworkDataReceived f -> Just f; _ -> Nothing

data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
    networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
    networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
    networkEventSourceMessageReceivedEventName :: String,
    networkEventSourceMessageReceivedEventId :: String,
    networkEventSourceMessageReceivedData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkEventSourceMessageReceived where
    parseJSON = A.withObject "NetworkEventSourceMessageReceived" $ \v ->
         NetworkEventSourceMessageReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "eventName"
            <*> v  .:  "eventId"
            <*> v  .:  "data"


instance ToJSON NetworkEventSourceMessageReceived  where
    toJSON v = A.object
        [ "requestId" .= networkEventSourceMessageReceivedRequestId v
        , "timestamp" .= networkEventSourceMessageReceivedTimestamp v
        , "eventName" .= networkEventSourceMessageReceivedEventName v
        , "eventId" .= networkEventSourceMessageReceivedEventId v
        , "data" .= networkEventSourceMessageReceivedData v
        ]


instance Event  NetworkEventSourceMessageReceived where
    eventName  _   =  "Network.eventSourceMessageReceived"
    fToHandler _   =  HNetworkEventSourceMessageReceived
    handlerToF _ h =  case h of HNetworkEventSourceMessageReceived f -> Just f; _ -> Nothing

data NetworkLoadingFailed = NetworkLoadingFailed {
    networkLoadingFailedRequestId :: NetworkRequestId,
    networkLoadingFailedTimestamp :: NetworkMonotonicTime,
    networkLoadingFailedType :: NetworkResourceType,
    networkLoadingFailedErrorText :: String,
    networkLoadingFailedCanceled :: Maybe Bool,
    networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
    networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
} deriving (Eq, Show, Read)
instance FromJSON  NetworkLoadingFailed where
    parseJSON = A.withObject "NetworkLoadingFailed" $ \v ->
         NetworkLoadingFailed <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "errorText"
            <*> v  .:?  "canceled"
            <*> v  .:?  "blockedReason"
            <*> v  .:?  "corsErrorStatus"


instance ToJSON NetworkLoadingFailed  where
    toJSON v = A.object
        [ "requestId" .= networkLoadingFailedRequestId v
        , "timestamp" .= networkLoadingFailedTimestamp v
        , "type" .= networkLoadingFailedType v
        , "errorText" .= networkLoadingFailedErrorText v
        , "canceled" .= networkLoadingFailedCanceled v
        , "blockedReason" .= networkLoadingFailedBlockedReason v
        , "corsErrorStatus" .= networkLoadingFailedCorsErrorStatus v
        ]


instance Event  NetworkLoadingFailed where
    eventName  _   =  "Network.loadingFailed"
    fToHandler _   =  HNetworkLoadingFailed
    handlerToF _ h =  case h of HNetworkLoadingFailed f -> Just f; _ -> Nothing

data NetworkLoadingFinished = NetworkLoadingFinished {
    networkLoadingFinishedRequestId :: NetworkRequestId,
    networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
    networkLoadingFinishedEncodedDataLength :: Int,
    networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkLoadingFinished where
    parseJSON = A.withObject "NetworkLoadingFinished" $ \v ->
         NetworkLoadingFinished <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "encodedDataLength"
            <*> v  .:?  "shouldReportCorbBlocking"


instance ToJSON NetworkLoadingFinished  where
    toJSON v = A.object
        [ "requestId" .= networkLoadingFinishedRequestId v
        , "timestamp" .= networkLoadingFinishedTimestamp v
        , "encodedDataLength" .= networkLoadingFinishedEncodedDataLength v
        , "shouldReportCorbBlocking" .= networkLoadingFinishedShouldReportCorbBlocking v
        ]


instance Event  NetworkLoadingFinished where
    eventName  _   =  "Network.loadingFinished"
    fToHandler _   =  HNetworkLoadingFinished
    handlerToF _ h =  case h of HNetworkLoadingFinished f -> Just f; _ -> Nothing

data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
    networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequestServedFromCache where
    parseJSON = A.withObject "NetworkRequestServedFromCache" $ \v ->
         NetworkRequestServedFromCache <$> v .:  "requestId"


instance ToJSON NetworkRequestServedFromCache  where
    toJSON v = A.object
        [ "requestId" .= networkRequestServedFromCacheRequestId v
        ]


instance Event  NetworkRequestServedFromCache where
    eventName  _   =  "Network.requestServedFromCache"
    fToHandler _   =  HNetworkRequestServedFromCache
    handlerToF _ h =  case h of HNetworkRequestServedFromCache f -> Just f; _ -> Nothing

data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
    networkRequestWillBeSentRequestId :: NetworkRequestId,
    networkRequestWillBeSentLoaderId :: NetworkLoaderId,
    networkRequestWillBeSentDocumentUrl :: String,
    networkRequestWillBeSentRequest :: NetworkRequest,
    networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
    networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
    networkRequestWillBeSentInitiator :: NetworkInitiator,
    networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
    networkRequestWillBeSentType :: Maybe NetworkResourceType,
    networkRequestWillBeSentFrameId :: Maybe PageFrameId,
    networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequestWillBeSent where
    parseJSON = A.withObject "NetworkRequestWillBeSent" $ \v ->
         NetworkRequestWillBeSent <$> v .:  "requestId"
            <*> v  .:  "loaderId"
            <*> v  .:  "documentURL"
            <*> v  .:  "request"
            <*> v  .:  "timestamp"
            <*> v  .:  "wallTime"
            <*> v  .:  "initiator"
            <*> v  .:?  "redirectResponse"
            <*> v  .:?  "type"
            <*> v  .:?  "frameId"
            <*> v  .:?  "hasUserGesture"


instance ToJSON NetworkRequestWillBeSent  where
    toJSON v = A.object
        [ "requestId" .= networkRequestWillBeSentRequestId v
        , "loaderId" .= networkRequestWillBeSentLoaderId v
        , "documentURL" .= networkRequestWillBeSentDocumentUrl v
        , "request" .= networkRequestWillBeSentRequest v
        , "timestamp" .= networkRequestWillBeSentTimestamp v
        , "wallTime" .= networkRequestWillBeSentWallTime v
        , "initiator" .= networkRequestWillBeSentInitiator v
        , "redirectResponse" .= networkRequestWillBeSentRedirectResponse v
        , "type" .= networkRequestWillBeSentType v
        , "frameId" .= networkRequestWillBeSentFrameId v
        , "hasUserGesture" .= networkRequestWillBeSentHasUserGesture v
        ]


instance Event  NetworkRequestWillBeSent where
    eventName  _   =  "Network.requestWillBeSent"
    fToHandler _   =  HNetworkRequestWillBeSent
    handlerToF _ h =  case h of HNetworkRequestWillBeSent f -> Just f; _ -> Nothing

data NetworkResponseReceived = NetworkResponseReceived {
    networkResponseReceivedRequestId :: NetworkRequestId,
    networkResponseReceivedLoaderId :: NetworkLoaderId,
    networkResponseReceivedTimestamp :: NetworkMonotonicTime,
    networkResponseReceivedType :: NetworkResourceType,
    networkResponseReceivedResponse :: NetworkResponse,
    networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResponseReceived where
    parseJSON = A.withObject "NetworkResponseReceived" $ \v ->
         NetworkResponseReceived <$> v .:  "requestId"
            <*> v  .:  "loaderId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "response"
            <*> v  .:?  "frameId"


instance ToJSON NetworkResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= networkResponseReceivedRequestId v
        , "loaderId" .= networkResponseReceivedLoaderId v
        , "timestamp" .= networkResponseReceivedTimestamp v
        , "type" .= networkResponseReceivedType v
        , "response" .= networkResponseReceivedResponse v
        , "frameId" .= networkResponseReceivedFrameId v
        ]


instance Event  NetworkResponseReceived where
    eventName  _   =  "Network.responseReceived"
    fToHandler _   =  HNetworkResponseReceived
    handlerToF _ h =  case h of HNetworkResponseReceived f -> Just f; _ -> Nothing

data NetworkWebSocketClosed = NetworkWebSocketClosed {
    networkWebSocketClosedRequestId :: NetworkRequestId,
    networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketClosed where
    parseJSON = A.withObject "NetworkWebSocketClosed" $ \v ->
         NetworkWebSocketClosed <$> v .:  "requestId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebSocketClosed  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketClosedRequestId v
        , "timestamp" .= networkWebSocketClosedTimestamp v
        ]


instance Event  NetworkWebSocketClosed where
    eventName  _   =  "Network.webSocketClosed"
    fToHandler _   =  HNetworkWebSocketClosed
    handlerToF _ h =  case h of HNetworkWebSocketClosed f -> Just f; _ -> Nothing

data NetworkWebSocketCreated = NetworkWebSocketCreated {
    networkWebSocketCreatedRequestId :: NetworkRequestId,
    networkWebSocketCreatedUrl :: String,
    networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketCreated where
    parseJSON = A.withObject "NetworkWebSocketCreated" $ \v ->
         NetworkWebSocketCreated <$> v .:  "requestId"
            <*> v  .:  "url"
            <*> v  .:?  "initiator"


instance ToJSON NetworkWebSocketCreated  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketCreatedRequestId v
        , "url" .= networkWebSocketCreatedUrl v
        , "initiator" .= networkWebSocketCreatedInitiator v
        ]


instance Event  NetworkWebSocketCreated where
    eventName  _   =  "Network.webSocketCreated"
    fToHandler _   =  HNetworkWebSocketCreated
    handlerToF _ h =  case h of HNetworkWebSocketCreated f -> Just f; _ -> Nothing

data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
    networkWebSocketFrameErrorRequestId :: NetworkRequestId,
    networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameErrorErrorMessage :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameError where
    parseJSON = A.withObject "NetworkWebSocketFrameError" $ \v ->
         NetworkWebSocketFrameError <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "errorMessage"


instance ToJSON NetworkWebSocketFrameError  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameErrorRequestId v
        , "timestamp" .= networkWebSocketFrameErrorTimestamp v
        , "errorMessage" .= networkWebSocketFrameErrorErrorMessage v
        ]


instance Event  NetworkWebSocketFrameError where
    eventName  _   =  "Network.webSocketFrameError"
    fToHandler _   =  HNetworkWebSocketFrameError
    handlerToF _ h =  case h of HNetworkWebSocketFrameError f -> Just f; _ -> Nothing

data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
    networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
    networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameReceived where
    parseJSON = A.withObject "NetworkWebSocketFrameReceived" $ \v ->
         NetworkWebSocketFrameReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketFrameReceived  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameReceivedRequestId v
        , "timestamp" .= networkWebSocketFrameReceivedTimestamp v
        , "response" .= networkWebSocketFrameReceivedResponse v
        ]


instance Event  NetworkWebSocketFrameReceived where
    eventName  _   =  "Network.webSocketFrameReceived"
    fToHandler _   =  HNetworkWebSocketFrameReceived
    handlerToF _ h =  case h of HNetworkWebSocketFrameReceived f -> Just f; _ -> Nothing

data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
    networkWebSocketFrameSentRequestId :: NetworkRequestId,
    networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameSent where
    parseJSON = A.withObject "NetworkWebSocketFrameSent" $ \v ->
         NetworkWebSocketFrameSent <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketFrameSent  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameSentRequestId v
        , "timestamp" .= networkWebSocketFrameSentTimestamp v
        , "response" .= networkWebSocketFrameSentResponse v
        ]


instance Event  NetworkWebSocketFrameSent where
    eventName  _   =  "Network.webSocketFrameSent"
    fToHandler _   =  HNetworkWebSocketFrameSent
    handlerToF _ h =  case h of HNetworkWebSocketFrameSent f -> Just f; _ -> Nothing

data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
    networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
    networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
    parseJSON = A.withObject "NetworkWebSocketHandshakeResponseReceived" $ \v ->
         NetworkWebSocketHandshakeResponseReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketHandshakeResponseReceivedRequestId v
        , "timestamp" .= networkWebSocketHandshakeResponseReceivedTimestamp v
        , "response" .= networkWebSocketHandshakeResponseReceivedResponse v
        ]


instance Event  NetworkWebSocketHandshakeResponseReceived where
    eventName  _   =  "Network.webSocketHandshakeResponseReceived"
    fToHandler _   =  HNetworkWebSocketHandshakeResponseReceived
    handlerToF _ h =  case h of HNetworkWebSocketHandshakeResponseReceived f -> Just f; _ -> Nothing

data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
    networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
    networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
    networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
    networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
    parseJSON = A.withObject "NetworkWebSocketWillSendHandshakeRequest" $ \v ->
         NetworkWebSocketWillSendHandshakeRequest <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "wallTime"
            <*> v  .:  "request"


instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketWillSendHandshakeRequestRequestId v
        , "timestamp" .= networkWebSocketWillSendHandshakeRequestTimestamp v
        , "wallTime" .= networkWebSocketWillSendHandshakeRequestWallTime v
        , "request" .= networkWebSocketWillSendHandshakeRequestRequest v
        ]


instance Event  NetworkWebSocketWillSendHandshakeRequest where
    eventName  _   =  "Network.webSocketWillSendHandshakeRequest"
    fToHandler _   =  HNetworkWebSocketWillSendHandshakeRequest
    handlerToF _ h =  case h of HNetworkWebSocketWillSendHandshakeRequest f -> Just f; _ -> Nothing

data NetworkWebTransportCreated = NetworkWebTransportCreated {
    networkWebTransportCreatedTransportId :: NetworkRequestId,
    networkWebTransportCreatedUrl :: String,
    networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
    networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportCreated where
    parseJSON = A.withObject "NetworkWebTransportCreated" $ \v ->
         NetworkWebTransportCreated <$> v .:  "transportId"
            <*> v  .:  "url"
            <*> v  .:  "timestamp"
            <*> v  .:?  "initiator"


instance ToJSON NetworkWebTransportCreated  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportCreatedTransportId v
        , "url" .= networkWebTransportCreatedUrl v
        , "timestamp" .= networkWebTransportCreatedTimestamp v
        , "initiator" .= networkWebTransportCreatedInitiator v
        ]


instance Event  NetworkWebTransportCreated where
    eventName  _   =  "Network.webTransportCreated"
    fToHandler _   =  HNetworkWebTransportCreated
    handlerToF _ h =  case h of HNetworkWebTransportCreated f -> Just f; _ -> Nothing

data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
    networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
    networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportConnectionEstablished where
    parseJSON = A.withObject "NetworkWebTransportConnectionEstablished" $ \v ->
         NetworkWebTransportConnectionEstablished <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebTransportConnectionEstablished  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportConnectionEstablishedTransportId v
        , "timestamp" .= networkWebTransportConnectionEstablishedTimestamp v
        ]


instance Event  NetworkWebTransportConnectionEstablished where
    eventName  _   =  "Network.webTransportConnectionEstablished"
    fToHandler _   =  HNetworkWebTransportConnectionEstablished
    handlerToF _ h =  case h of HNetworkWebTransportConnectionEstablished f -> Just f; _ -> Nothing

data NetworkWebTransportClosed = NetworkWebTransportClosed {
    networkWebTransportClosedTransportId :: NetworkRequestId,
    networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportClosed where
    parseJSON = A.withObject "NetworkWebTransportClosed" $ \v ->
         NetworkWebTransportClosed <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebTransportClosed  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportClosedTransportId v
        , "timestamp" .= networkWebTransportClosedTimestamp v
        ]


instance Event  NetworkWebTransportClosed where
    eventName  _   =  "Network.webTransportClosed"
    fToHandler _   =  HNetworkWebTransportClosed
    handlerToF _ h =  case h of HNetworkWebTransportClosed f -> Just f; _ -> Nothing


data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
    deriving (Eq, Show, Read)
instance FromJSON NetworkResourceType where
    parseJSON = A.withText  "NetworkResourceType"  $ \v -> do
        pure $ case v of
                "Document" -> NetworkResourceTypeDocument
                "Stylesheet" -> NetworkResourceTypeStylesheet
                "Image" -> NetworkResourceTypeImage
                "Media" -> NetworkResourceTypeMedia
                "Font" -> NetworkResourceTypeFont
                "Script" -> NetworkResourceTypeScript
                "TextTrack" -> NetworkResourceTypeTextTrack
                "XHR" -> NetworkResourceTypeXhr
                "Fetch" -> NetworkResourceTypeFetch
                "EventSource" -> NetworkResourceTypeEventSource
                "WebSocket" -> NetworkResourceTypeWebSocket
                "Manifest" -> NetworkResourceTypeManifest
                "SignedExchange" -> NetworkResourceTypeSignedExchange
                "Ping" -> NetworkResourceTypePing
                "CSPViolationReport" -> NetworkResourceTypeCspViolationReport
                "Preflight" -> NetworkResourceTypePreflight
                "Other" -> NetworkResourceTypeOther
                _ -> error "failed to parse NetworkResourceType"

instance ToJSON NetworkResourceType where
    toJSON v = A.String $
        case v of
                NetworkResourceTypeDocument -> "Document"
                NetworkResourceTypeStylesheet -> "Stylesheet"
                NetworkResourceTypeImage -> "Image"
                NetworkResourceTypeMedia -> "Media"
                NetworkResourceTypeFont -> "Font"
                NetworkResourceTypeScript -> "Script"
                NetworkResourceTypeTextTrack -> "TextTrack"
                NetworkResourceTypeXhr -> "XHR"
                NetworkResourceTypeFetch -> "Fetch"
                NetworkResourceTypeEventSource -> "EventSource"
                NetworkResourceTypeWebSocket -> "WebSocket"
                NetworkResourceTypeManifest -> "Manifest"
                NetworkResourceTypeSignedExchange -> "SignedExchange"
                NetworkResourceTypePing -> "Ping"
                NetworkResourceTypeCspViolationReport -> "CSPViolationReport"
                NetworkResourceTypePreflight -> "Preflight"
                NetworkResourceTypeOther -> "Other"



type NetworkLoaderId = String

type NetworkRequestId = String

type NetworkInterceptionId = String

data NetworkErrorReason = NetworkErrorReasonFailed | NetworkErrorReasonAborted | NetworkErrorReasonTimedOut | NetworkErrorReasonAccessDenied | NetworkErrorReasonConnectionClosed | NetworkErrorReasonConnectionReset | NetworkErrorReasonConnectionRefused | NetworkErrorReasonConnectionAborted | NetworkErrorReasonConnectionFailed | NetworkErrorReasonNameNotResolved | NetworkErrorReasonInternetDisconnected | NetworkErrorReasonAddressUnreachable | NetworkErrorReasonBlockedByClient | NetworkErrorReasonBlockedByResponse
    deriving (Eq, Show, Read)
instance FromJSON NetworkErrorReason where
    parseJSON = A.withText  "NetworkErrorReason"  $ \v -> do
        pure $ case v of
                "Failed" -> NetworkErrorReasonFailed
                "Aborted" -> NetworkErrorReasonAborted
                "TimedOut" -> NetworkErrorReasonTimedOut
                "AccessDenied" -> NetworkErrorReasonAccessDenied
                "ConnectionClosed" -> NetworkErrorReasonConnectionClosed
                "ConnectionReset" -> NetworkErrorReasonConnectionReset
                "ConnectionRefused" -> NetworkErrorReasonConnectionRefused
                "ConnectionAborted" -> NetworkErrorReasonConnectionAborted
                "ConnectionFailed" -> NetworkErrorReasonConnectionFailed
                "NameNotResolved" -> NetworkErrorReasonNameNotResolved
                "InternetDisconnected" -> NetworkErrorReasonInternetDisconnected
                "AddressUnreachable" -> NetworkErrorReasonAddressUnreachable
                "BlockedByClient" -> NetworkErrorReasonBlockedByClient
                "BlockedByResponse" -> NetworkErrorReasonBlockedByResponse
                _ -> error "failed to parse NetworkErrorReason"

instance ToJSON NetworkErrorReason where
    toJSON v = A.String $
        case v of
                NetworkErrorReasonFailed -> "Failed"
                NetworkErrorReasonAborted -> "Aborted"
                NetworkErrorReasonTimedOut -> "TimedOut"
                NetworkErrorReasonAccessDenied -> "AccessDenied"
                NetworkErrorReasonConnectionClosed -> "ConnectionClosed"
                NetworkErrorReasonConnectionReset -> "ConnectionReset"
                NetworkErrorReasonConnectionRefused -> "ConnectionRefused"
                NetworkErrorReasonConnectionAborted -> "ConnectionAborted"
                NetworkErrorReasonConnectionFailed -> "ConnectionFailed"
                NetworkErrorReasonNameNotResolved -> "NameNotResolved"
                NetworkErrorReasonInternetDisconnected -> "InternetDisconnected"
                NetworkErrorReasonAddressUnreachable -> "AddressUnreachable"
                NetworkErrorReasonBlockedByClient -> "BlockedByClient"
                NetworkErrorReasonBlockedByResponse -> "BlockedByResponse"



type NetworkTimeSinceEpoch = Int

type NetworkMonotonicTime = Int

type NetworkHeaders = [(String, String)]

data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
    deriving (Eq, Show, Read)
instance FromJSON NetworkConnectionType where
    parseJSON = A.withText  "NetworkConnectionType"  $ \v -> do
        pure $ case v of
                "none" -> NetworkConnectionTypeNone
                "cellular2g" -> NetworkConnectionTypeCellular2g
                "cellular3g" -> NetworkConnectionTypeCellular3g
                "cellular4g" -> NetworkConnectionTypeCellular4g
                "bluetooth" -> NetworkConnectionTypeBluetooth
                "ethernet" -> NetworkConnectionTypeEthernet
                "wifi" -> NetworkConnectionTypeWifi
                "wimax" -> NetworkConnectionTypeWimax
                "other" -> NetworkConnectionTypeOther
                _ -> error "failed to parse NetworkConnectionType"

instance ToJSON NetworkConnectionType where
    toJSON v = A.String $
        case v of
                NetworkConnectionTypeNone -> "none"
                NetworkConnectionTypeCellular2g -> "cellular2g"
                NetworkConnectionTypeCellular3g -> "cellular3g"
                NetworkConnectionTypeCellular4g -> "cellular4g"
                NetworkConnectionTypeBluetooth -> "bluetooth"
                NetworkConnectionTypeEthernet -> "ethernet"
                NetworkConnectionTypeWifi -> "wifi"
                NetworkConnectionTypeWimax -> "wimax"
                NetworkConnectionTypeOther -> "other"



data NetworkCookieSameSite = NetworkCookieSameSiteStrict | NetworkCookieSameSiteLax | NetworkCookieSameSiteNone
    deriving (Eq, Show, Read)
instance FromJSON NetworkCookieSameSite where
    parseJSON = A.withText  "NetworkCookieSameSite"  $ \v -> do
        pure $ case v of
                "Strict" -> NetworkCookieSameSiteStrict
                "Lax" -> NetworkCookieSameSiteLax
                "None" -> NetworkCookieSameSiteNone
                _ -> error "failed to parse NetworkCookieSameSite"

instance ToJSON NetworkCookieSameSite where
    toJSON v = A.String $
        case v of
                NetworkCookieSameSiteStrict -> "Strict"
                NetworkCookieSameSiteLax -> "Lax"
                NetworkCookieSameSiteNone -> "None"



data NetworkResourceTiming = NetworkResourceTiming {
    networkResourceTimingRequestTime :: Int,
    networkResourceTimingProxyStart :: Int,
    networkResourceTimingProxyEnd :: Int,
    networkResourceTimingDnsStart :: Int,
    networkResourceTimingDnsEnd :: Int,
    networkResourceTimingConnectStart :: Int,
    networkResourceTimingConnectEnd :: Int,
    networkResourceTimingSslStart :: Int,
    networkResourceTimingSslEnd :: Int,
    networkResourceTimingSendStart :: Int,
    networkResourceTimingSendEnd :: Int,
    networkResourceTimingReceiveHeadersEnd :: Int
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResourceTiming where
    parseJSON = A.withObject "NetworkResourceTiming" $ \v ->
         NetworkResourceTiming <$> v .:  "requestTime"
            <*> v  .:  "proxyStart"
            <*> v  .:  "proxyEnd"
            <*> v  .:  "dnsStart"
            <*> v  .:  "dnsEnd"
            <*> v  .:  "connectStart"
            <*> v  .:  "connectEnd"
            <*> v  .:  "sslStart"
            <*> v  .:  "sslEnd"
            <*> v  .:  "sendStart"
            <*> v  .:  "sendEnd"
            <*> v  .:  "receiveHeadersEnd"


instance ToJSON NetworkResourceTiming  where
    toJSON v = A.object
        [ "requestTime" .= networkResourceTimingRequestTime v
        , "proxyStart" .= networkResourceTimingProxyStart v
        , "proxyEnd" .= networkResourceTimingProxyEnd v
        , "dnsStart" .= networkResourceTimingDnsStart v
        , "dnsEnd" .= networkResourceTimingDnsEnd v
        , "connectStart" .= networkResourceTimingConnectStart v
        , "connectEnd" .= networkResourceTimingConnectEnd v
        , "sslStart" .= networkResourceTimingSslStart v
        , "sslEnd" .= networkResourceTimingSslEnd v
        , "sendStart" .= networkResourceTimingSendStart v
        , "sendEnd" .= networkResourceTimingSendEnd v
        , "receiveHeadersEnd" .= networkResourceTimingReceiveHeadersEnd v
        ]



data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
    deriving (Eq, Show, Read)
instance FromJSON NetworkResourcePriority where
    parseJSON = A.withText  "NetworkResourcePriority"  $ \v -> do
        pure $ case v of
                "VeryLow" -> NetworkResourcePriorityVeryLow
                "Low" -> NetworkResourcePriorityLow
                "Medium" -> NetworkResourcePriorityMedium
                "High" -> NetworkResourcePriorityHigh
                "VeryHigh" -> NetworkResourcePriorityVeryHigh
                _ -> error "failed to parse NetworkResourcePriority"

instance ToJSON NetworkResourcePriority where
    toJSON v = A.String $
        case v of
                NetworkResourcePriorityVeryLow -> "VeryLow"
                NetworkResourcePriorityLow -> "Low"
                NetworkResourcePriorityMedium -> "Medium"
                NetworkResourcePriorityHigh -> "High"
                NetworkResourcePriorityVeryHigh -> "VeryHigh"



data NetworkPostDataEntry = NetworkPostDataEntry {
    networkPostDataEntryBytes :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkPostDataEntry where
    parseJSON = A.withObject "NetworkPostDataEntry" $ \v ->
         NetworkPostDataEntry <$> v .:?  "bytes"


instance ToJSON NetworkPostDataEntry  where
    toJSON v = A.object
        [ "bytes" .= networkPostDataEntryBytes v
        ]



data NetworkRequest = NetworkRequest {
    networkRequestUrl :: String,
    networkRequestMethod :: String,
    networkRequestHeaders :: NetworkHeaders,
    networkRequestInitialPriority :: NetworkResourcePriority,
    networkRequestReferrerPolicy :: String,
    networkRequestUrlFragment :: Maybe String,
    networkRequestPostData :: Maybe String,
    networkRequestHasPostData :: Maybe Bool,
    networkRequestMixedContentType :: Maybe SecurityMixedContentType,
    networkRequestIsLinkPreload :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequest where
    parseJSON = A.withObject "NetworkRequest" $ \v ->
         NetworkRequest <$> v .:  "url"
            <*> v  .:  "method"
            <*> v  .:  "headers"
            <*> v  .:  "initialPriority"
            <*> v  .:  "referrerPolicy"
            <*> v  .:?  "urlFragment"
            <*> v  .:?  "postData"
            <*> v  .:?  "hasPostData"
            <*> v  .:?  "mixedContentType"
            <*> v  .:?  "isLinkPreload"


instance ToJSON NetworkRequest  where
    toJSON v = A.object
        [ "url" .= networkRequestUrl v
        , "method" .= networkRequestMethod v
        , "headers" .= networkRequestHeaders v
        , "initialPriority" .= networkRequestInitialPriority v
        , "referrerPolicy" .= networkRequestReferrerPolicy v
        , "urlFragment" .= networkRequestUrlFragment v
        , "postData" .= networkRequestPostData v
        , "hasPostData" .= networkRequestHasPostData v
        , "mixedContentType" .= networkRequestMixedContentType v
        , "isLinkPreload" .= networkRequestIsLinkPreload v
        ]



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
    networkSignedCertificateTimestampStatus :: String,
    networkSignedCertificateTimestampOrigin :: String,
    networkSignedCertificateTimestampLogDescription :: String,
    networkSignedCertificateTimestampLogId :: String,
    networkSignedCertificateTimestampTimestamp :: Int,
    networkSignedCertificateTimestampHashAlgorithm :: String,
    networkSignedCertificateTimestampSignatureAlgorithm :: String,
    networkSignedCertificateTimestampSignatureData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkSignedCertificateTimestamp where
    parseJSON = A.withObject "NetworkSignedCertificateTimestamp" $ \v ->
         NetworkSignedCertificateTimestamp <$> v .:  "status"
            <*> v  .:  "origin"
            <*> v  .:  "logDescription"
            <*> v  .:  "logId"
            <*> v  .:  "timestamp"
            <*> v  .:  "hashAlgorithm"
            <*> v  .:  "signatureAlgorithm"
            <*> v  .:  "signatureData"


instance ToJSON NetworkSignedCertificateTimestamp  where
    toJSON v = A.object
        [ "status" .= networkSignedCertificateTimestampStatus v
        , "origin" .= networkSignedCertificateTimestampOrigin v
        , "logDescription" .= networkSignedCertificateTimestampLogDescription v
        , "logId" .= networkSignedCertificateTimestampLogId v
        , "timestamp" .= networkSignedCertificateTimestampTimestamp v
        , "hashAlgorithm" .= networkSignedCertificateTimestampHashAlgorithm v
        , "signatureAlgorithm" .= networkSignedCertificateTimestampSignatureAlgorithm v
        , "signatureData" .= networkSignedCertificateTimestampSignatureData v
        ]



data NetworkSecurityDetails = NetworkSecurityDetails {
    networkSecurityDetailsProtocol :: String,
    networkSecurityDetailsKeyExchange :: String,
    networkSecurityDetailsCipher :: String,
    networkSecurityDetailsCertificateId :: SecurityCertificateId,
    networkSecurityDetailsSubjectName :: String,
    networkSecurityDetailsSanList :: [String],
    networkSecurityDetailsIssuer :: String,
    networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
    networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
    networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
    networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance,
    networkSecurityDetailsKeyExchangeGroup :: Maybe String,
    networkSecurityDetailsMac :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkSecurityDetails where
    parseJSON = A.withObject "NetworkSecurityDetails" $ \v ->
         NetworkSecurityDetails <$> v .:  "protocol"
            <*> v  .:  "keyExchange"
            <*> v  .:  "cipher"
            <*> v  .:  "certificateId"
            <*> v  .:  "subjectName"
            <*> v  .:  "sanList"
            <*> v  .:  "issuer"
            <*> v  .:  "validFrom"
            <*> v  .:  "validTo"
            <*> v  .:  "signedCertificateTimestampList"
            <*> v  .:  "certificateTransparencyCompliance"
            <*> v  .:?  "keyExchangeGroup"
            <*> v  .:?  "mac"


instance ToJSON NetworkSecurityDetails  where
    toJSON v = A.object
        [ "protocol" .= networkSecurityDetailsProtocol v
        , "keyExchange" .= networkSecurityDetailsKeyExchange v
        , "cipher" .= networkSecurityDetailsCipher v
        , "certificateId" .= networkSecurityDetailsCertificateId v
        , "subjectName" .= networkSecurityDetailsSubjectName v
        , "sanList" .= networkSecurityDetailsSanList v
        , "issuer" .= networkSecurityDetailsIssuer v
        , "validFrom" .= networkSecurityDetailsValidFrom v
        , "validTo" .= networkSecurityDetailsValidTo v
        , "signedCertificateTimestampList" .= networkSecurityDetailsSignedCertificateTimestampList v
        , "certificateTransparencyCompliance" .= networkSecurityDetailsCertificateTransparencyCompliance v
        , "keyExchangeGroup" .= networkSecurityDetailsKeyExchangeGroup v
        , "mac" .= networkSecurityDetailsMac v
        ]



data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
    deriving (Eq, Show, Read)
instance FromJSON NetworkCertificateTransparencyCompliance where
    parseJSON = A.withText  "NetworkCertificateTransparencyCompliance"  $ \v -> do
        pure $ case v of
                "unknown" -> NetworkCertificateTransparencyComplianceUnknown
                "not-compliant" -> NetworkCertificateTransparencyComplianceNotCompliant
                "compliant" -> NetworkCertificateTransparencyComplianceCompliant
                _ -> error "failed to parse NetworkCertificateTransparencyCompliance"

instance ToJSON NetworkCertificateTransparencyCompliance where
    toJSON v = A.String $
        case v of
                NetworkCertificateTransparencyComplianceUnknown -> "unknown"
                NetworkCertificateTransparencyComplianceNotCompliant -> "not-compliant"
                NetworkCertificateTransparencyComplianceCompliant -> "compliant"



data NetworkBlockedReason = NetworkBlockedReasonOther | NetworkBlockedReasonCsp | NetworkBlockedReasonMixedContent | NetworkBlockedReasonOrigin | NetworkBlockedReasonInspector | NetworkBlockedReasonSubresourceFilter | NetworkBlockedReasonContentType | NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader | NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | NetworkBlockedReasonCorpNotSameOrigin | NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | NetworkBlockedReasonCorpNotSameSite
    deriving (Eq, Show, Read)
instance FromJSON NetworkBlockedReason where
    parseJSON = A.withText  "NetworkBlockedReason"  $ \v -> do
        pure $ case v of
                "other" -> NetworkBlockedReasonOther
                "csp" -> NetworkBlockedReasonCsp
                "mixed-content" -> NetworkBlockedReasonMixedContent
                "origin" -> NetworkBlockedReasonOrigin
                "inspector" -> NetworkBlockedReasonInspector
                "subresource-filter" -> NetworkBlockedReasonSubresourceFilter
                "content-type" -> NetworkBlockedReasonContentType
                "coep-frame-resource-needs-coep-header" -> NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader
                "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
                "corp-not-same-origin" -> NetworkBlockedReasonCorpNotSameOrigin
                "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
                "corp-not-same-site" -> NetworkBlockedReasonCorpNotSameSite
                _ -> error "failed to parse NetworkBlockedReason"

instance ToJSON NetworkBlockedReason where
    toJSON v = A.String $
        case v of
                NetworkBlockedReasonOther -> "other"
                NetworkBlockedReasonCsp -> "csp"
                NetworkBlockedReasonMixedContent -> "mixed-content"
                NetworkBlockedReasonOrigin -> "origin"
                NetworkBlockedReasonInspector -> "inspector"
                NetworkBlockedReasonSubresourceFilter -> "subresource-filter"
                NetworkBlockedReasonContentType -> "content-type"
                NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader -> "coep-frame-resource-needs-coep-header"
                NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage -> "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
                NetworkBlockedReasonCorpNotSameOrigin -> "corp-not-same-origin"
                NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
                NetworkBlockedReasonCorpNotSameSite -> "corp-not-same-site"



data NetworkCorsError = NetworkCorsErrorDisallowedByMode | NetworkCorsErrorInvalidResponse | NetworkCorsErrorWildcardOriginNotAllowed | NetworkCorsErrorMissingAllowOriginHeader | NetworkCorsErrorMultipleAllowOriginValues | NetworkCorsErrorInvalidAllowOriginValue | NetworkCorsErrorAllowOriginMismatch | NetworkCorsErrorInvalidAllowCredentials | NetworkCorsErrorCorsDisabledScheme | NetworkCorsErrorPreflightInvalidStatus | NetworkCorsErrorPreflightDisallowedRedirect | NetworkCorsErrorPreflightWildcardOriginNotAllowed | NetworkCorsErrorPreflightMissingAllowOriginHeader | NetworkCorsErrorPreflightMultipleAllowOriginValues | NetworkCorsErrorPreflightInvalidAllowOriginValue | NetworkCorsErrorPreflightAllowOriginMismatch | NetworkCorsErrorPreflightInvalidAllowCredentials | NetworkCorsErrorPreflightMissingAllowExternal | NetworkCorsErrorPreflightInvalidAllowExternal | NetworkCorsErrorPreflightMissingAllowPrivateNetwork | NetworkCorsErrorPreflightInvalidAllowPrivateNetwork | NetworkCorsErrorInvalidAllowMethodsPreflightResponse | NetworkCorsErrorInvalidAllowHeadersPreflightResponse | NetworkCorsErrorMethodDisallowedByPreflightResponse | NetworkCorsErrorHeaderDisallowedByPreflightResponse | NetworkCorsErrorRedirectContainsCredentials | NetworkCorsErrorInsecurePrivateNetwork | NetworkCorsErrorInvalidPrivateNetworkAccess | NetworkCorsErrorUnexpectedPrivateNetworkAccess | NetworkCorsErrorNoCorsRedirectModeNotFollow
    deriving (Eq, Show, Read)
instance FromJSON NetworkCorsError where
    parseJSON = A.withText  "NetworkCorsError"  $ \v -> do
        pure $ case v of
                "DisallowedByMode" -> NetworkCorsErrorDisallowedByMode
                "InvalidResponse" -> NetworkCorsErrorInvalidResponse
                "WildcardOriginNotAllowed" -> NetworkCorsErrorWildcardOriginNotAllowed
                "MissingAllowOriginHeader" -> NetworkCorsErrorMissingAllowOriginHeader
                "MultipleAllowOriginValues" -> NetworkCorsErrorMultipleAllowOriginValues
                "InvalidAllowOriginValue" -> NetworkCorsErrorInvalidAllowOriginValue
                "AllowOriginMismatch" -> NetworkCorsErrorAllowOriginMismatch
                "InvalidAllowCredentials" -> NetworkCorsErrorInvalidAllowCredentials
                "CorsDisabledScheme" -> NetworkCorsErrorCorsDisabledScheme
                "PreflightInvalidStatus" -> NetworkCorsErrorPreflightInvalidStatus
                "PreflightDisallowedRedirect" -> NetworkCorsErrorPreflightDisallowedRedirect
                "PreflightWildcardOriginNotAllowed" -> NetworkCorsErrorPreflightWildcardOriginNotAllowed
                "PreflightMissingAllowOriginHeader" -> NetworkCorsErrorPreflightMissingAllowOriginHeader
                "PreflightMultipleAllowOriginValues" -> NetworkCorsErrorPreflightMultipleAllowOriginValues
                "PreflightInvalidAllowOriginValue" -> NetworkCorsErrorPreflightInvalidAllowOriginValue
                "PreflightAllowOriginMismatch" -> NetworkCorsErrorPreflightAllowOriginMismatch
                "PreflightInvalidAllowCredentials" -> NetworkCorsErrorPreflightInvalidAllowCredentials
                "PreflightMissingAllowExternal" -> NetworkCorsErrorPreflightMissingAllowExternal
                "PreflightInvalidAllowExternal" -> NetworkCorsErrorPreflightInvalidAllowExternal
                "PreflightMissingAllowPrivateNetwork" -> NetworkCorsErrorPreflightMissingAllowPrivateNetwork
                "PreflightInvalidAllowPrivateNetwork" -> NetworkCorsErrorPreflightInvalidAllowPrivateNetwork
                "InvalidAllowMethodsPreflightResponse" -> NetworkCorsErrorInvalidAllowMethodsPreflightResponse
                "InvalidAllowHeadersPreflightResponse" -> NetworkCorsErrorInvalidAllowHeadersPreflightResponse
                "MethodDisallowedByPreflightResponse" -> NetworkCorsErrorMethodDisallowedByPreflightResponse
                "HeaderDisallowedByPreflightResponse" -> NetworkCorsErrorHeaderDisallowedByPreflightResponse
                "RedirectContainsCredentials" -> NetworkCorsErrorRedirectContainsCredentials
                "InsecurePrivateNetwork" -> NetworkCorsErrorInsecurePrivateNetwork
                "InvalidPrivateNetworkAccess" -> NetworkCorsErrorInvalidPrivateNetworkAccess
                "UnexpectedPrivateNetworkAccess" -> NetworkCorsErrorUnexpectedPrivateNetworkAccess
                "NoCorsRedirectModeNotFollow" -> NetworkCorsErrorNoCorsRedirectModeNotFollow
                _ -> error "failed to parse NetworkCorsError"

instance ToJSON NetworkCorsError where
    toJSON v = A.String $
        case v of
                NetworkCorsErrorDisallowedByMode -> "DisallowedByMode"
                NetworkCorsErrorInvalidResponse -> "InvalidResponse"
                NetworkCorsErrorWildcardOriginNotAllowed -> "WildcardOriginNotAllowed"
                NetworkCorsErrorMissingAllowOriginHeader -> "MissingAllowOriginHeader"
                NetworkCorsErrorMultipleAllowOriginValues -> "MultipleAllowOriginValues"
                NetworkCorsErrorInvalidAllowOriginValue -> "InvalidAllowOriginValue"
                NetworkCorsErrorAllowOriginMismatch -> "AllowOriginMismatch"
                NetworkCorsErrorInvalidAllowCredentials -> "InvalidAllowCredentials"
                NetworkCorsErrorCorsDisabledScheme -> "CorsDisabledScheme"
                NetworkCorsErrorPreflightInvalidStatus -> "PreflightInvalidStatus"
                NetworkCorsErrorPreflightDisallowedRedirect -> "PreflightDisallowedRedirect"
                NetworkCorsErrorPreflightWildcardOriginNotAllowed -> "PreflightWildcardOriginNotAllowed"
                NetworkCorsErrorPreflightMissingAllowOriginHeader -> "PreflightMissingAllowOriginHeader"
                NetworkCorsErrorPreflightMultipleAllowOriginValues -> "PreflightMultipleAllowOriginValues"
                NetworkCorsErrorPreflightInvalidAllowOriginValue -> "PreflightInvalidAllowOriginValue"
                NetworkCorsErrorPreflightAllowOriginMismatch -> "PreflightAllowOriginMismatch"
                NetworkCorsErrorPreflightInvalidAllowCredentials -> "PreflightInvalidAllowCredentials"
                NetworkCorsErrorPreflightMissingAllowExternal -> "PreflightMissingAllowExternal"
                NetworkCorsErrorPreflightInvalidAllowExternal -> "PreflightInvalidAllowExternal"
                NetworkCorsErrorPreflightMissingAllowPrivateNetwork -> "PreflightMissingAllowPrivateNetwork"
                NetworkCorsErrorPreflightInvalidAllowPrivateNetwork -> "PreflightInvalidAllowPrivateNetwork"
                NetworkCorsErrorInvalidAllowMethodsPreflightResponse -> "InvalidAllowMethodsPreflightResponse"
                NetworkCorsErrorInvalidAllowHeadersPreflightResponse -> "InvalidAllowHeadersPreflightResponse"
                NetworkCorsErrorMethodDisallowedByPreflightResponse -> "MethodDisallowedByPreflightResponse"
                NetworkCorsErrorHeaderDisallowedByPreflightResponse -> "HeaderDisallowedByPreflightResponse"
                NetworkCorsErrorRedirectContainsCredentials -> "RedirectContainsCredentials"
                NetworkCorsErrorInsecurePrivateNetwork -> "InsecurePrivateNetwork"
                NetworkCorsErrorInvalidPrivateNetworkAccess -> "InvalidPrivateNetworkAccess"
                NetworkCorsErrorUnexpectedPrivateNetworkAccess -> "UnexpectedPrivateNetworkAccess"
                NetworkCorsErrorNoCorsRedirectModeNotFollow -> "NoCorsRedirectModeNotFollow"



data NetworkCorsErrorStatus = NetworkCorsErrorStatus {
    networkCorsErrorStatusCorsError :: NetworkCorsError,
    networkCorsErrorStatusFailedParameter :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCorsErrorStatus where
    parseJSON = A.withObject "NetworkCorsErrorStatus" $ \v ->
         NetworkCorsErrorStatus <$> v .:  "corsError"
            <*> v  .:  "failedParameter"


instance ToJSON NetworkCorsErrorStatus  where
    toJSON v = A.object
        [ "corsError" .= networkCorsErrorStatusCorsError v
        , "failedParameter" .= networkCorsErrorStatusFailedParameter v
        ]



data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
    deriving (Eq, Show, Read)
instance FromJSON NetworkServiceWorkerResponseSource where
    parseJSON = A.withText  "NetworkServiceWorkerResponseSource"  $ \v -> do
        pure $ case v of
                "cache-storage" -> NetworkServiceWorkerResponseSourceCacheStorage
                "http-cache" -> NetworkServiceWorkerResponseSourceHttpCache
                "fallback-code" -> NetworkServiceWorkerResponseSourceFallbackCode
                "network" -> NetworkServiceWorkerResponseSourceNetwork
                _ -> error "failed to parse NetworkServiceWorkerResponseSource"

instance ToJSON NetworkServiceWorkerResponseSource where
    toJSON v = A.String $
        case v of
                NetworkServiceWorkerResponseSourceCacheStorage -> "cache-storage"
                NetworkServiceWorkerResponseSourceHttpCache -> "http-cache"
                NetworkServiceWorkerResponseSourceFallbackCode -> "fallback-code"
                NetworkServiceWorkerResponseSourceNetwork -> "network"



data NetworkResponse = NetworkResponse {
    networkResponseUrl :: String,
    networkResponseStatus :: Int,
    networkResponseStatusText :: String,
    networkResponseHeaders :: NetworkHeaders,
    networkResponseMimeType :: String,
    networkResponseConnectionReused :: Bool,
    networkResponseConnectionId :: Int,
    networkResponseEncodedDataLength :: Int,
    networkResponseSecurityState :: SecuritySecurityState,
    networkResponseRequestHeaders :: Maybe NetworkHeaders,
    networkResponseRemoteIpAddress :: Maybe String,
    networkResponseRemotePort :: Maybe Int,
    networkResponseFromDiskCache :: Maybe Bool,
    networkResponseFromServiceWorker :: Maybe Bool,
    networkResponseFromPrefetchCache :: Maybe Bool,
    networkResponseTiming :: Maybe NetworkResourceTiming,
    networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
    networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
    networkResponseCacheStorageCacheName :: Maybe String,
    networkResponseProtocol :: Maybe String,
    networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResponse where
    parseJSON = A.withObject "NetworkResponse" $ \v ->
         NetworkResponse <$> v .:  "url"
            <*> v  .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:  "mimeType"
            <*> v  .:  "connectionReused"
            <*> v  .:  "connectionId"
            <*> v  .:  "encodedDataLength"
            <*> v  .:  "securityState"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "remoteIPAddress"
            <*> v  .:?  "remotePort"
            <*> v  .:?  "fromDiskCache"
            <*> v  .:?  "fromServiceWorker"
            <*> v  .:?  "fromPrefetchCache"
            <*> v  .:?  "timing"
            <*> v  .:?  "serviceWorkerResponseSource"
            <*> v  .:?  "responseTime"
            <*> v  .:?  "cacheStorageCacheName"
            <*> v  .:?  "protocol"
            <*> v  .:?  "securityDetails"


instance ToJSON NetworkResponse  where
    toJSON v = A.object
        [ "url" .= networkResponseUrl v
        , "status" .= networkResponseStatus v
        , "statusText" .= networkResponseStatusText v
        , "headers" .= networkResponseHeaders v
        , "mimeType" .= networkResponseMimeType v
        , "connectionReused" .= networkResponseConnectionReused v
        , "connectionId" .= networkResponseConnectionId v
        , "encodedDataLength" .= networkResponseEncodedDataLength v
        , "securityState" .= networkResponseSecurityState v
        , "requestHeaders" .= networkResponseRequestHeaders v
        , "remoteIPAddress" .= networkResponseRemoteIpAddress v
        , "remotePort" .= networkResponseRemotePort v
        , "fromDiskCache" .= networkResponseFromDiskCache v
        , "fromServiceWorker" .= networkResponseFromServiceWorker v
        , "fromPrefetchCache" .= networkResponseFromPrefetchCache v
        , "timing" .= networkResponseTiming v
        , "serviceWorkerResponseSource" .= networkResponseServiceWorkerResponseSource v
        , "responseTime" .= networkResponseResponseTime v
        , "cacheStorageCacheName" .= networkResponseCacheStorageCacheName v
        , "protocol" .= networkResponseProtocol v
        , "securityDetails" .= networkResponseSecurityDetails v
        ]



data NetworkWebSocketRequest = NetworkWebSocketRequest {
    networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketRequest where
    parseJSON = A.withObject "NetworkWebSocketRequest" $ \v ->
         NetworkWebSocketRequest <$> v .:  "headers"


instance ToJSON NetworkWebSocketRequest  where
    toJSON v = A.object
        [ "headers" .= networkWebSocketRequestHeaders v
        ]



data NetworkWebSocketResponse = NetworkWebSocketResponse {
    networkWebSocketResponseStatus :: Int,
    networkWebSocketResponseStatusText :: String,
    networkWebSocketResponseHeaders :: NetworkHeaders,
    networkWebSocketResponseHeadersText :: Maybe String,
    networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
    networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketResponse where
    parseJSON = A.withObject "NetworkWebSocketResponse" $ \v ->
         NetworkWebSocketResponse <$> v .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:?  "headersText"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "requestHeadersText"


instance ToJSON NetworkWebSocketResponse  where
    toJSON v = A.object
        [ "status" .= networkWebSocketResponseStatus v
        , "statusText" .= networkWebSocketResponseStatusText v
        , "headers" .= networkWebSocketResponseHeaders v
        , "headersText" .= networkWebSocketResponseHeadersText v
        , "requestHeaders" .= networkWebSocketResponseRequestHeaders v
        , "requestHeadersText" .= networkWebSocketResponseRequestHeadersText v
        ]



data NetworkWebSocketFrame = NetworkWebSocketFrame {
    networkWebSocketFrameOpcode :: Int,
    networkWebSocketFrameMask :: Bool,
    networkWebSocketFramePayloadData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrame where
    parseJSON = A.withObject "NetworkWebSocketFrame" $ \v ->
         NetworkWebSocketFrame <$> v .:  "opcode"
            <*> v  .:  "mask"
            <*> v  .:  "payloadData"


instance ToJSON NetworkWebSocketFrame  where
    toJSON v = A.object
        [ "opcode" .= networkWebSocketFrameOpcode v
        , "mask" .= networkWebSocketFrameMask v
        , "payloadData" .= networkWebSocketFramePayloadData v
        ]



data NetworkCachedResource = NetworkCachedResource {
    networkCachedResourceUrl :: String,
    networkCachedResourceType :: NetworkResourceType,
    networkCachedResourceBodySize :: Int,
    networkCachedResourceResponse :: Maybe NetworkResponse
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCachedResource where
    parseJSON = A.withObject "NetworkCachedResource" $ \v ->
         NetworkCachedResource <$> v .:  "url"
            <*> v  .:  "type"
            <*> v  .:  "bodySize"
            <*> v  .:?  "response"


instance ToJSON NetworkCachedResource  where
    toJSON v = A.object
        [ "url" .= networkCachedResourceUrl v
        , "type" .= networkCachedResourceType v
        , "bodySize" .= networkCachedResourceBodySize v
        , "response" .= networkCachedResourceResponse v
        ]



data NetworkInitiator = NetworkInitiator {
    networkInitiatorType :: String,
    networkInitiatorStack :: Maybe RuntimeStackTrace,
    networkInitiatorUrl :: Maybe String,
    networkInitiatorLineNumber :: Maybe Int,
    networkInitiatorColumnNumber :: Maybe Int,
    networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkInitiator where
    parseJSON = A.withObject "NetworkInitiator" $ \v ->
         NetworkInitiator <$> v .:  "type"
            <*> v  .:?  "stack"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "requestId"


instance ToJSON NetworkInitiator  where
    toJSON v = A.object
        [ "type" .= networkInitiatorType v
        , "stack" .= networkInitiatorStack v
        , "url" .= networkInitiatorUrl v
        , "lineNumber" .= networkInitiatorLineNumber v
        , "columnNumber" .= networkInitiatorColumnNumber v
        , "requestId" .= networkInitiatorRequestId v
        ]



data NetworkCookie = NetworkCookie {
    networkCookieName :: String,
    networkCookieValue :: String,
    networkCookieDomain :: String,
    networkCookiePath :: String,
    networkCookieExpires :: Int,
    networkCookieSize :: Int,
    networkCookieHttpOnly :: Bool,
    networkCookieSecure :: Bool,
    networkCookieSession :: Bool,
    networkCookieSameSite :: Maybe NetworkCookieSameSite
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCookie where
    parseJSON = A.withObject "NetworkCookie" $ \v ->
         NetworkCookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:  "domain"
            <*> v  .:  "path"
            <*> v  .:  "expires"
            <*> v  .:  "size"
            <*> v  .:  "httpOnly"
            <*> v  .:  "secure"
            <*> v  .:  "session"
            <*> v  .:?  "sameSite"


instance ToJSON NetworkCookie  where
    toJSON v = A.object
        [ "name" .= networkCookieName v
        , "value" .= networkCookieValue v
        , "domain" .= networkCookieDomain v
        , "path" .= networkCookiePath v
        , "expires" .= networkCookieExpires v
        , "size" .= networkCookieSize v
        , "httpOnly" .= networkCookieHttpOnly v
        , "secure" .= networkCookieSecure v
        , "session" .= networkCookieSession v
        , "sameSite" .= networkCookieSameSite v
        ]



data NetworkCookieParam = NetworkCookieParam {
    networkCookieParamName :: String,
    networkCookieParamValue :: String,
    networkCookieParamUrl :: Maybe String,
    networkCookieParamDomain :: Maybe String,
    networkCookieParamPath :: Maybe String,
    networkCookieParamSecure :: Maybe Bool,
    networkCookieParamHttpOnly :: Maybe Bool,
    networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
    networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCookieParam where
    parseJSON = A.withObject "NetworkCookieParam" $ \v ->
         NetworkCookieParam <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON NetworkCookieParam  where
    toJSON v = A.object
        [ "name" .= networkCookieParamName v
        , "value" .= networkCookieParamValue v
        , "url" .= networkCookieParamUrl v
        , "domain" .= networkCookieParamDomain v
        , "path" .= networkCookieParamPath v
        , "secure" .= networkCookieParamSecure v
        , "httpOnly" .= networkCookieParamHttpOnly v
        , "sameSite" .= networkCookieParamSameSite v
        , "expires" .= networkCookieParamExpires v
        ]



networkClearBrowserCache :: Session -> IO (Maybe Error)
networkClearBrowserCache session  = sendReceiveCommand (conn session) ("Network","clearBrowserCache") ([] ++ (catMaybes []))


networkClearBrowserCookies :: Session -> IO (Maybe Error)
networkClearBrowserCookies session  = sendReceiveCommand (conn session) ("Network","clearBrowserCookies") ([] ++ (catMaybes []))


networkDeleteCookies :: Session -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
networkDeleteCookies session networkDeleteCookiesName networkDeleteCookiesUrl networkDeleteCookiesDomain networkDeleteCookiesPath = sendReceiveCommand (conn session) ("Network","deleteCookies") ([("name", ToJSONEx networkDeleteCookiesName)] ++ (catMaybes [fmap (("url",) . ToJSONEx) networkDeleteCookiesUrl, fmap (("domain",) . ToJSONEx) networkDeleteCookiesDomain, fmap (("path",) . ToJSONEx) networkDeleteCookiesPath]))


networkDisable :: Session -> IO (Maybe Error)
networkDisable session  = sendReceiveCommand (conn session) ("Network","disable") ([] ++ (catMaybes []))


networkEmulateNetworkConditions :: Session -> Bool -> Int -> Int -> Int -> Maybe NetworkConnectionType -> IO (Maybe Error)
networkEmulateNetworkConditions session networkEmulateNetworkConditionsOffline networkEmulateNetworkConditionsLatency networkEmulateNetworkConditionsDownloadThroughput networkEmulateNetworkConditionsUploadThroughput networkEmulateNetworkConditionsConnectionType = sendReceiveCommand (conn session) ("Network","emulateNetworkConditions") ([("offline", ToJSONEx networkEmulateNetworkConditionsOffline), ("latency", ToJSONEx networkEmulateNetworkConditionsLatency), ("downloadThroughput", ToJSONEx networkEmulateNetworkConditionsDownloadThroughput), ("uploadThroughput", ToJSONEx networkEmulateNetworkConditionsUploadThroughput)] ++ (catMaybes [fmap (("connectionType",) . ToJSONEx) networkEmulateNetworkConditionsConnectionType]))


networkEnable :: Session -> Maybe Int -> IO (Maybe Error)
networkEnable session networkEnableMaxPostDataSize = sendReceiveCommand (conn session) ("Network","enable") ([] ++ (catMaybes [fmap (("maxPostDataSize",) . ToJSONEx) networkEnableMaxPostDataSize]))

data NetworkGetAllCookies = NetworkGetAllCookies {
    networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetAllCookies where
    parseJSON = A.withObject "NetworkGetAllCookies" $ \v ->
         NetworkGetAllCookies <$> v .:  "cookies"



networkGetAllCookies :: Session -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies session  = sendReceiveCommandResult (conn session) ("Network","getAllCookies") ([] ++ (catMaybes []))

data NetworkGetCookies = NetworkGetCookies {
    networkGetCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetCookies where
    parseJSON = A.withObject "NetworkGetCookies" $ \v ->
         NetworkGetCookies <$> v .:  "cookies"



networkGetCookies :: Session -> Maybe [String] -> IO (Either Error NetworkGetCookies)
networkGetCookies session networkGetCookiesUrls = sendReceiveCommandResult (conn session) ("Network","getCookies") ([] ++ (catMaybes [fmap (("urls",) . ToJSONEx) networkGetCookiesUrls]))

data NetworkGetResponseBody = NetworkGetResponseBody {
    networkGetResponseBodyBody :: String,
    networkGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetResponseBody where
    parseJSON = A.withObject "NetworkGetResponseBody" $ \v ->
         NetworkGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



networkGetResponseBody :: Session -> NetworkRequestId -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody session networkGetResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Network","getResponseBody") ([("requestId", ToJSONEx networkGetResponseBodyRequestId)] ++ (catMaybes []))

data NetworkGetRequestPostData = NetworkGetRequestPostData {
    networkGetRequestPostDataPostData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetRequestPostData where
    parseJSON = A.withObject "NetworkGetRequestPostData" $ \v ->
         NetworkGetRequestPostData <$> v .:  "postData"



networkGetRequestPostData :: Session -> NetworkRequestId -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData session networkGetRequestPostDataRequestId = sendReceiveCommandResult (conn session) ("Network","getRequestPostData") ([("requestId", ToJSONEx networkGetRequestPostDataRequestId)] ++ (catMaybes []))


networkSetCacheDisabled :: Session -> Bool -> IO (Maybe Error)
networkSetCacheDisabled session networkSetCacheDisabledCacheDisabled = sendReceiveCommand (conn session) ("Network","setCacheDisabled") ([("cacheDisabled", ToJSONEx networkSetCacheDisabledCacheDisabled)] ++ (catMaybes []))


networkSetCookie :: Session -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe NetworkCookieSameSite -> Maybe NetworkTimeSinceEpoch -> IO (Maybe Error)
networkSetCookie session networkSetCookieName networkSetCookieValue networkSetCookieUrl networkSetCookieDomain networkSetCookiePath networkSetCookieSecure networkSetCookieHttpOnly networkSetCookieSameSite networkSetCookieExpires = sendReceiveCommand (conn session) ("Network","setCookie") ([("name", ToJSONEx networkSetCookieName), ("value", ToJSONEx networkSetCookieValue)] ++ (catMaybes [fmap (("url",) . ToJSONEx) networkSetCookieUrl, fmap (("domain",) . ToJSONEx) networkSetCookieDomain, fmap (("path",) . ToJSONEx) networkSetCookiePath, fmap (("secure",) . ToJSONEx) networkSetCookieSecure, fmap (("httpOnly",) . ToJSONEx) networkSetCookieHttpOnly, fmap (("sameSite",) . ToJSONEx) networkSetCookieSameSite, fmap (("expires",) . ToJSONEx) networkSetCookieExpires]))


networkSetCookies :: Session -> [NetworkCookieParam] -> IO (Maybe Error)
networkSetCookies session networkSetCookiesCookies = sendReceiveCommand (conn session) ("Network","setCookies") ([("cookies", ToJSONEx networkSetCookiesCookies)] ++ (catMaybes []))


networkSetExtraHTTPHeaders :: Session -> NetworkHeaders -> IO (Maybe Error)
networkSetExtraHTTPHeaders session networkSetExtraHttpHeadersHeaders = sendReceiveCommand (conn session) ("Network","setExtraHTTPHeaders") ([("headers", ToJSONEx networkSetExtraHttpHeadersHeaders)] ++ (catMaybes []))


networkSetUserAgentOverride :: Session -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
networkSetUserAgentOverride session networkSetUserAgentOverrideUserAgent networkSetUserAgentOverrideAcceptLanguage networkSetUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Network","setUserAgentOverride") ([("userAgent", ToJSONEx networkSetUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("acceptLanguage",) . ToJSONEx) networkSetUserAgentOverrideAcceptLanguage, fmap (("platform",) . ToJSONEx) networkSetUserAgentOverridePlatform]))



data PageDomContentEventFired = PageDomContentEventFired {
    pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  PageDomContentEventFired where
    parseJSON = A.withObject "PageDomContentEventFired" $ \v ->
         PageDomContentEventFired <$> v .:  "timestamp"


instance ToJSON PageDomContentEventFired  where
    toJSON v = A.object
        [ "timestamp" .= pageDomContentEventFiredTimestamp v
        ]


instance Event  PageDomContentEventFired where
    eventName  _   =  "Page.domContentEventFired"
    fToHandler _   =  HPageDomContentEventFired
    handlerToF _ h =  case h of HPageDomContentEventFired f -> Just f; _ -> Nothing

data PageFileChooserOpened = PageFileChooserOpened {
    pageFileChooserOpenedMode :: String
} deriving (Eq, Show, Read)
instance FromJSON  PageFileChooserOpened where
    parseJSON = A.withObject "PageFileChooserOpened" $ \v ->
         PageFileChooserOpened <$> v .:  "mode"


instance ToJSON PageFileChooserOpened  where
    toJSON v = A.object
        [ "mode" .= pageFileChooserOpenedMode v
        ]


instance Event  PageFileChooserOpened where
    eventName  _   =  "Page.fileChooserOpened"
    fToHandler _   =  HPageFileChooserOpened
    handlerToF _ h =  case h of HPageFileChooserOpened f -> Just f; _ -> Nothing

data PageFrameAttached = PageFrameAttached {
    pageFrameAttachedFrameId :: PageFrameId,
    pageFrameAttachedParentFrameId :: PageFrameId,
    pageFrameAttachedStack :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  PageFrameAttached where
    parseJSON = A.withObject "PageFrameAttached" $ \v ->
         PageFrameAttached <$> v .:  "frameId"
            <*> v  .:  "parentFrameId"
            <*> v  .:?  "stack"


instance ToJSON PageFrameAttached  where
    toJSON v = A.object
        [ "frameId" .= pageFrameAttachedFrameId v
        , "parentFrameId" .= pageFrameAttachedParentFrameId v
        , "stack" .= pageFrameAttachedStack v
        ]


instance Event  PageFrameAttached where
    eventName  _   =  "Page.frameAttached"
    fToHandler _   =  HPageFrameAttached
    handlerToF _ h =  case h of HPageFrameAttached f -> Just f; _ -> Nothing

data PageFrameDetached = PageFrameDetached {
    pageFrameDetachedFrameId :: PageFrameId
} deriving (Eq, Show, Read)
instance FromJSON  PageFrameDetached where
    parseJSON = A.withObject "PageFrameDetached" $ \v ->
         PageFrameDetached <$> v .:  "frameId"


instance ToJSON PageFrameDetached  where
    toJSON v = A.object
        [ "frameId" .= pageFrameDetachedFrameId v
        ]


instance Event  PageFrameDetached where
    eventName  _   =  "Page.frameDetached"
    fToHandler _   =  HPageFrameDetached
    handlerToF _ h =  case h of HPageFrameDetached f -> Just f; _ -> Nothing

data PageFrameNavigated = PageFrameNavigated {
    pageFrameNavigatedFrame :: PageFrame
} deriving (Eq, Show, Read)
instance FromJSON  PageFrameNavigated where
    parseJSON = A.withObject "PageFrameNavigated" $ \v ->
         PageFrameNavigated <$> v .:  "frame"


instance ToJSON PageFrameNavigated  where
    toJSON v = A.object
        [ "frame" .= pageFrameNavigatedFrame v
        ]


instance Event  PageFrameNavigated where
    eventName  _   =  "Page.frameNavigated"
    fToHandler _   =  HPageFrameNavigated
    handlerToF _ h =  case h of HPageFrameNavigated f -> Just f; _ -> Nothing

data PageInterstitialHidden = PageInterstitialHidden
    deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
    parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
        pure $ case v of
                "PageInterstitialHidden" -> PageInterstitialHidden
                _ -> error "failed to parse PageInterstitialHidden"

instance Event  PageInterstitialHidden where
    eventName  _   =  "Page.interstitialHidden"
    fToHandler _   =  HPageInterstitialHidden
    handlerToF _ h =  case h of HPageInterstitialHidden f -> Just f; _ -> Nothing

data PageInterstitialShown = PageInterstitialShown
    deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
    parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
        pure $ case v of
                "PageInterstitialShown" -> PageInterstitialShown
                _ -> error "failed to parse PageInterstitialShown"

instance Event  PageInterstitialShown where
    eventName  _   =  "Page.interstitialShown"
    fToHandler _   =  HPageInterstitialShown
    handlerToF _ h =  case h of HPageInterstitialShown f -> Just f; _ -> Nothing

data PageJavascriptDialogClosed = PageJavascriptDialogClosed {
    pageJavascriptDialogClosedResult :: Bool,
    pageJavascriptDialogClosedUserInput :: String
} deriving (Eq, Show, Read)
instance FromJSON  PageJavascriptDialogClosed where
    parseJSON = A.withObject "PageJavascriptDialogClosed" $ \v ->
         PageJavascriptDialogClosed <$> v .:  "result"
            <*> v  .:  "userInput"


instance ToJSON PageJavascriptDialogClosed  where
    toJSON v = A.object
        [ "result" .= pageJavascriptDialogClosedResult v
        , "userInput" .= pageJavascriptDialogClosedUserInput v
        ]


instance Event  PageJavascriptDialogClosed where
    eventName  _   =  "Page.javascriptDialogClosed"
    fToHandler _   =  HPageJavascriptDialogClosed
    handlerToF _ h =  case h of HPageJavascriptDialogClosed f -> Just f; _ -> Nothing

data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
    pageJavascriptDialogOpeningUrl :: String,
    pageJavascriptDialogOpeningMessage :: String,
    pageJavascriptDialogOpeningType :: PageDialogType,
    pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
    pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PageJavascriptDialogOpening where
    parseJSON = A.withObject "PageJavascriptDialogOpening" $ \v ->
         PageJavascriptDialogOpening <$> v .:  "url"
            <*> v  .:  "message"
            <*> v  .:  "type"
            <*> v  .:  "hasBrowserHandler"
            <*> v  .:?  "defaultPrompt"


instance ToJSON PageJavascriptDialogOpening  where
    toJSON v = A.object
        [ "url" .= pageJavascriptDialogOpeningUrl v
        , "message" .= pageJavascriptDialogOpeningMessage v
        , "type" .= pageJavascriptDialogOpeningType v
        , "hasBrowserHandler" .= pageJavascriptDialogOpeningHasBrowserHandler v
        , "defaultPrompt" .= pageJavascriptDialogOpeningDefaultPrompt v
        ]


instance Event  PageJavascriptDialogOpening where
    eventName  _   =  "Page.javascriptDialogOpening"
    fToHandler _   =  HPageJavascriptDialogOpening
    handlerToF _ h =  case h of HPageJavascriptDialogOpening f -> Just f; _ -> Nothing

data PageLifecycleEvent = PageLifecycleEvent {
    pageLifecycleEventFrameId :: PageFrameId,
    pageLifecycleEventLoaderId :: NetworkLoaderId,
    pageLifecycleEventName :: String,
    pageLifecycleEventTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  PageLifecycleEvent where
    parseJSON = A.withObject "PageLifecycleEvent" $ \v ->
         PageLifecycleEvent <$> v .:  "frameId"
            <*> v  .:  "loaderId"
            <*> v  .:  "name"
            <*> v  .:  "timestamp"


instance ToJSON PageLifecycleEvent  where
    toJSON v = A.object
        [ "frameId" .= pageLifecycleEventFrameId v
        , "loaderId" .= pageLifecycleEventLoaderId v
        , "name" .= pageLifecycleEventName v
        , "timestamp" .= pageLifecycleEventTimestamp v
        ]


instance Event  PageLifecycleEvent where
    eventName  _   =  "Page.lifecycleEvent"
    fToHandler _   =  HPageLifecycleEvent
    handlerToF _ h =  case h of HPageLifecycleEvent f -> Just f; _ -> Nothing

data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
    pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
    pagePrerenderAttemptCompletedPrerenderingUrl :: String,
    pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus
} deriving (Eq, Show, Read)
instance FromJSON  PagePrerenderAttemptCompleted where
    parseJSON = A.withObject "PagePrerenderAttemptCompleted" $ \v ->
         PagePrerenderAttemptCompleted <$> v .:  "initiatingFrameId"
            <*> v  .:  "prerenderingUrl"
            <*> v  .:  "finalStatus"


instance ToJSON PagePrerenderAttemptCompleted  where
    toJSON v = A.object
        [ "initiatingFrameId" .= pagePrerenderAttemptCompletedInitiatingFrameId v
        , "prerenderingUrl" .= pagePrerenderAttemptCompletedPrerenderingUrl v
        , "finalStatus" .= pagePrerenderAttemptCompletedFinalStatus v
        ]


instance Event  PagePrerenderAttemptCompleted where
    eventName  _   =  "Page.prerenderAttemptCompleted"
    fToHandler _   =  HPagePrerenderAttemptCompleted
    handlerToF _ h =  case h of HPagePrerenderAttemptCompleted f -> Just f; _ -> Nothing

data PageLoadEventFired = PageLoadEventFired {
    pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  PageLoadEventFired where
    parseJSON = A.withObject "PageLoadEventFired" $ \v ->
         PageLoadEventFired <$> v .:  "timestamp"


instance ToJSON PageLoadEventFired  where
    toJSON v = A.object
        [ "timestamp" .= pageLoadEventFiredTimestamp v
        ]


instance Event  PageLoadEventFired where
    eventName  _   =  "Page.loadEventFired"
    fToHandler _   =  HPageLoadEventFired
    handlerToF _ h =  case h of HPageLoadEventFired f -> Just f; _ -> Nothing

data PageWindowOpen = PageWindowOpen {
    pageWindowOpenUrl :: String,
    pageWindowOpenWindowName :: String,
    pageWindowOpenWindowFeatures :: [String],
    pageWindowOpenUserGesture :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PageWindowOpen where
    parseJSON = A.withObject "PageWindowOpen" $ \v ->
         PageWindowOpen <$> v .:  "url"
            <*> v  .:  "windowName"
            <*> v  .:  "windowFeatures"
            <*> v  .:  "userGesture"


instance ToJSON PageWindowOpen  where
    toJSON v = A.object
        [ "url" .= pageWindowOpenUrl v
        , "windowName" .= pageWindowOpenWindowName v
        , "windowFeatures" .= pageWindowOpenWindowFeatures v
        , "userGesture" .= pageWindowOpenUserGesture v
        ]


instance Event  PageWindowOpen where
    eventName  _   =  "Page.windowOpen"
    fToHandler _   =  HPageWindowOpen
    handlerToF _ h =  case h of HPageWindowOpen f -> Just f; _ -> Nothing


type PageFrameId = String

data PageFrame = PageFrame {
    pageFrameId :: PageFrameId,
    pageFrameLoaderId :: NetworkLoaderId,
    pageFrameUrl :: String,
    pageFrameSecurityOrigin :: String,
    pageFrameMimeType :: String,
    pageFrameParentId :: Maybe PageFrameId,
    pageFrameName :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PageFrame where
    parseJSON = A.withObject "PageFrame" $ \v ->
         PageFrame <$> v .:  "id"
            <*> v  .:  "loaderId"
            <*> v  .:  "url"
            <*> v  .:  "securityOrigin"
            <*> v  .:  "mimeType"
            <*> v  .:?  "parentId"
            <*> v  .:?  "name"


instance ToJSON PageFrame  where
    toJSON v = A.object
        [ "id" .= pageFrameId v
        , "loaderId" .= pageFrameLoaderId v
        , "url" .= pageFrameUrl v
        , "securityOrigin" .= pageFrameSecurityOrigin v
        , "mimeType" .= pageFrameMimeType v
        , "parentId" .= pageFrameParentId v
        , "name" .= pageFrameName v
        ]



data PageFrameTree = PageFrameTree {
    pageFrameTreeFrame :: PageFrame,
    pageFrameTreeChildFrames :: Maybe [PageFrameTree]
} deriving (Eq, Show, Read)
instance FromJSON  PageFrameTree where
    parseJSON = A.withObject "PageFrameTree" $ \v ->
         PageFrameTree <$> v .:  "frame"
            <*> v  .:?  "childFrames"


instance ToJSON PageFrameTree  where
    toJSON v = A.object
        [ "frame" .= pageFrameTreeFrame v
        , "childFrames" .= pageFrameTreeChildFrames v
        ]



type PageScriptIdentifier = String

data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddressBar | PageTransitionTypeAutoBookmark | PageTransitionTypeAutoSubframe | PageTransitionTypeManualSubframe | PageTransitionTypeGenerated | PageTransitionTypeAutoToplevel | PageTransitionTypeFormSubmit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeywordGenerated | PageTransitionTypeOther
    deriving (Eq, Show, Read)
instance FromJSON PageTransitionType where
    parseJSON = A.withText  "PageTransitionType"  $ \v -> do
        pure $ case v of
                "link" -> PageTransitionTypeLink
                "typed" -> PageTransitionTypeTyped
                "address_bar" -> PageTransitionTypeAddressBar
                "auto_bookmark" -> PageTransitionTypeAutoBookmark
                "auto_subframe" -> PageTransitionTypeAutoSubframe
                "manual_subframe" -> PageTransitionTypeManualSubframe
                "generated" -> PageTransitionTypeGenerated
                "auto_toplevel" -> PageTransitionTypeAutoToplevel
                "form_submit" -> PageTransitionTypeFormSubmit
                "reload" -> PageTransitionTypeReload
                "keyword" -> PageTransitionTypeKeyword
                "keyword_generated" -> PageTransitionTypeKeywordGenerated
                "other" -> PageTransitionTypeOther
                _ -> error "failed to parse PageTransitionType"

instance ToJSON PageTransitionType where
    toJSON v = A.String $
        case v of
                PageTransitionTypeLink -> "link"
                PageTransitionTypeTyped -> "typed"
                PageTransitionTypeAddressBar -> "address_bar"
                PageTransitionTypeAutoBookmark -> "auto_bookmark"
                PageTransitionTypeAutoSubframe -> "auto_subframe"
                PageTransitionTypeManualSubframe -> "manual_subframe"
                PageTransitionTypeGenerated -> "generated"
                PageTransitionTypeAutoToplevel -> "auto_toplevel"
                PageTransitionTypeFormSubmit -> "form_submit"
                PageTransitionTypeReload -> "reload"
                PageTransitionTypeKeyword -> "keyword"
                PageTransitionTypeKeywordGenerated -> "keyword_generated"
                PageTransitionTypeOther -> "other"



data PageNavigationEntry = PageNavigationEntry {
    pageNavigationEntryId :: Int,
    pageNavigationEntryUrl :: String,
    pageNavigationEntryUserTypedUrl :: String,
    pageNavigationEntryTitle :: String,
    pageNavigationEntryTransitionType :: PageTransitionType
} deriving (Eq, Show, Read)
instance FromJSON  PageNavigationEntry where
    parseJSON = A.withObject "PageNavigationEntry" $ \v ->
         PageNavigationEntry <$> v .:  "id"
            <*> v  .:  "url"
            <*> v  .:  "userTypedURL"
            <*> v  .:  "title"
            <*> v  .:  "transitionType"


instance ToJSON PageNavigationEntry  where
    toJSON v = A.object
        [ "id" .= pageNavigationEntryId v
        , "url" .= pageNavigationEntryUrl v
        , "userTypedURL" .= pageNavigationEntryUserTypedUrl v
        , "title" .= pageNavigationEntryTitle v
        , "transitionType" .= pageNavigationEntryTransitionType v
        ]



data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
    deriving (Eq, Show, Read)
instance FromJSON PageDialogType where
    parseJSON = A.withText  "PageDialogType"  $ \v -> do
        pure $ case v of
                "alert" -> PageDialogTypeAlert
                "confirm" -> PageDialogTypeConfirm
                "prompt" -> PageDialogTypePrompt
                "beforeunload" -> PageDialogTypeBeforeunload
                _ -> error "failed to parse PageDialogType"

instance ToJSON PageDialogType where
    toJSON v = A.String $
        case v of
                PageDialogTypeAlert -> "alert"
                PageDialogTypeConfirm -> "confirm"
                PageDialogTypePrompt -> "prompt"
                PageDialogTypeBeforeunload -> "beforeunload"



data PageAppManifestError = PageAppManifestError {
    pageAppManifestErrorMessage :: String,
    pageAppManifestErrorCritical :: Int,
    pageAppManifestErrorLine :: Int,
    pageAppManifestErrorColumn :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PageAppManifestError where
    parseJSON = A.withObject "PageAppManifestError" $ \v ->
         PageAppManifestError <$> v .:  "message"
            <*> v  .:  "critical"
            <*> v  .:  "line"
            <*> v  .:  "column"


instance ToJSON PageAppManifestError  where
    toJSON v = A.object
        [ "message" .= pageAppManifestErrorMessage v
        , "critical" .= pageAppManifestErrorCritical v
        , "line" .= pageAppManifestErrorLine v
        , "column" .= pageAppManifestErrorColumn v
        ]



data PageLayoutViewport = PageLayoutViewport {
    pageLayoutViewportPageX :: Int,
    pageLayoutViewportPageY :: Int,
    pageLayoutViewportClientWidth :: Int,
    pageLayoutViewportClientHeight :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PageLayoutViewport where
    parseJSON = A.withObject "PageLayoutViewport" $ \v ->
         PageLayoutViewport <$> v .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"


instance ToJSON PageLayoutViewport  where
    toJSON v = A.object
        [ "pageX" .= pageLayoutViewportPageX v
        , "pageY" .= pageLayoutViewportPageY v
        , "clientWidth" .= pageLayoutViewportClientWidth v
        , "clientHeight" .= pageLayoutViewportClientHeight v
        ]



data PageVisualViewport = PageVisualViewport {
    pageVisualViewportOffsetX :: Int,
    pageVisualViewportOffsetY :: Int,
    pageVisualViewportPageX :: Int,
    pageVisualViewportPageY :: Int,
    pageVisualViewportClientWidth :: Int,
    pageVisualViewportClientHeight :: Int,
    pageVisualViewportScale :: Int,
    pageVisualViewportZoom :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PageVisualViewport where
    parseJSON = A.withObject "PageVisualViewport" $ \v ->
         PageVisualViewport <$> v .:  "offsetX"
            <*> v  .:  "offsetY"
            <*> v  .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"
            <*> v  .:  "scale"
            <*> v  .:?  "zoom"


instance ToJSON PageVisualViewport  where
    toJSON v = A.object
        [ "offsetX" .= pageVisualViewportOffsetX v
        , "offsetY" .= pageVisualViewportOffsetY v
        , "pageX" .= pageVisualViewportPageX v
        , "pageY" .= pageVisualViewportPageY v
        , "clientWidth" .= pageVisualViewportClientWidth v
        , "clientHeight" .= pageVisualViewportClientHeight v
        , "scale" .= pageVisualViewportScale v
        , "zoom" .= pageVisualViewportZoom v
        ]



data PageViewport = PageViewport {
    pageViewportX :: Int,
    pageViewportY :: Int,
    pageViewportWidth :: Int,
    pageViewportHeight :: Int,
    pageViewportScale :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PageViewport where
    parseJSON = A.withObject "PageViewport" $ \v ->
         PageViewport <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:  "scale"


instance ToJSON PageViewport  where
    toJSON v = A.object
        [ "x" .= pageViewportX v
        , "y" .= pageViewportY v
        , "width" .= pageViewportWidth v
        , "height" .= pageViewportHeight v
        , "scale" .= pageViewportScale v
        ]



data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
    deriving (Eq, Show, Read)
instance FromJSON PagePrerenderFinalStatus where
    parseJSON = A.withText  "PagePrerenderFinalStatus"  $ \v -> do
        pure $ case v of
                "Activated" -> PagePrerenderFinalStatusActivated
                "Destroyed" -> PagePrerenderFinalStatusDestroyed
                "LowEndDevice" -> PagePrerenderFinalStatusLowEndDevice
                "CrossOriginRedirect" -> PagePrerenderFinalStatusCrossOriginRedirect
                "CrossOriginNavigation" -> PagePrerenderFinalStatusCrossOriginNavigation
                "InvalidSchemeRedirect" -> PagePrerenderFinalStatusInvalidSchemeRedirect
                "InvalidSchemeNavigation" -> PagePrerenderFinalStatusInvalidSchemeNavigation
                "InProgressNavigation" -> PagePrerenderFinalStatusInProgressNavigation
                "NavigationRequestBlockedByCsp" -> PagePrerenderFinalStatusNavigationRequestBlockedByCsp
                "MainFrameNavigation" -> PagePrerenderFinalStatusMainFrameNavigation
                "MojoBinderPolicy" -> PagePrerenderFinalStatusMojoBinderPolicy
                "RendererProcessCrashed" -> PagePrerenderFinalStatusRendererProcessCrashed
                "RendererProcessKilled" -> PagePrerenderFinalStatusRendererProcessKilled
                "Download" -> PagePrerenderFinalStatusDownload
                "TriggerDestroyed" -> PagePrerenderFinalStatusTriggerDestroyed
                "NavigationNotCommitted" -> PagePrerenderFinalStatusNavigationNotCommitted
                "NavigationBadHttpStatus" -> PagePrerenderFinalStatusNavigationBadHttpStatus
                "ClientCertRequested" -> PagePrerenderFinalStatusClientCertRequested
                "NavigationRequestNetworkError" -> PagePrerenderFinalStatusNavigationRequestNetworkError
                "MaxNumOfRunningPrerendersExceeded" -> PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded
                "CancelAllHostsForTesting" -> PagePrerenderFinalStatusCancelAllHostsForTesting
                "DidFailLoad" -> PagePrerenderFinalStatusDidFailLoad
                "Stop" -> PagePrerenderFinalStatusStop
                "SslCertificateError" -> PagePrerenderFinalStatusSslCertificateError
                "LoginAuthRequested" -> PagePrerenderFinalStatusLoginAuthRequested
                "UaChangeRequiresReload" -> PagePrerenderFinalStatusUaChangeRequiresReload
                "BlockedByClient" -> PagePrerenderFinalStatusBlockedByClient
                "AudioOutputDeviceRequested" -> PagePrerenderFinalStatusAudioOutputDeviceRequested
                "MixedContent" -> PagePrerenderFinalStatusMixedContent
                "TriggerBackgrounded" -> PagePrerenderFinalStatusTriggerBackgrounded
                "EmbedderTriggeredAndSameOriginRedirected" -> PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected
                "EmbedderTriggeredAndCrossOriginRedirected" -> PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
                "EmbedderTriggeredAndDestroyed" -> PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
                _ -> error "failed to parse PagePrerenderFinalStatus"

instance ToJSON PagePrerenderFinalStatus where
    toJSON v = A.String $
        case v of
                PagePrerenderFinalStatusActivated -> "Activated"
                PagePrerenderFinalStatusDestroyed -> "Destroyed"
                PagePrerenderFinalStatusLowEndDevice -> "LowEndDevice"
                PagePrerenderFinalStatusCrossOriginRedirect -> "CrossOriginRedirect"
                PagePrerenderFinalStatusCrossOriginNavigation -> "CrossOriginNavigation"
                PagePrerenderFinalStatusInvalidSchemeRedirect -> "InvalidSchemeRedirect"
                PagePrerenderFinalStatusInvalidSchemeNavigation -> "InvalidSchemeNavigation"
                PagePrerenderFinalStatusInProgressNavigation -> "InProgressNavigation"
                PagePrerenderFinalStatusNavigationRequestBlockedByCsp -> "NavigationRequestBlockedByCsp"
                PagePrerenderFinalStatusMainFrameNavigation -> "MainFrameNavigation"
                PagePrerenderFinalStatusMojoBinderPolicy -> "MojoBinderPolicy"
                PagePrerenderFinalStatusRendererProcessCrashed -> "RendererProcessCrashed"
                PagePrerenderFinalStatusRendererProcessKilled -> "RendererProcessKilled"
                PagePrerenderFinalStatusDownload -> "Download"
                PagePrerenderFinalStatusTriggerDestroyed -> "TriggerDestroyed"
                PagePrerenderFinalStatusNavigationNotCommitted -> "NavigationNotCommitted"
                PagePrerenderFinalStatusNavigationBadHttpStatus -> "NavigationBadHttpStatus"
                PagePrerenderFinalStatusClientCertRequested -> "ClientCertRequested"
                PagePrerenderFinalStatusNavigationRequestNetworkError -> "NavigationRequestNetworkError"
                PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded -> "MaxNumOfRunningPrerendersExceeded"
                PagePrerenderFinalStatusCancelAllHostsForTesting -> "CancelAllHostsForTesting"
                PagePrerenderFinalStatusDidFailLoad -> "DidFailLoad"
                PagePrerenderFinalStatusStop -> "Stop"
                PagePrerenderFinalStatusSslCertificateError -> "SslCertificateError"
                PagePrerenderFinalStatusLoginAuthRequested -> "LoginAuthRequested"
                PagePrerenderFinalStatusUaChangeRequiresReload -> "UaChangeRequiresReload"
                PagePrerenderFinalStatusBlockedByClient -> "BlockedByClient"
                PagePrerenderFinalStatusAudioOutputDeviceRequested -> "AudioOutputDeviceRequested"
                PagePrerenderFinalStatusMixedContent -> "MixedContent"
                PagePrerenderFinalStatusTriggerBackgrounded -> "TriggerBackgrounded"
                PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected -> "EmbedderTriggeredAndSameOriginRedirected"
                PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected -> "EmbedderTriggeredAndCrossOriginRedirected"
                PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed -> "EmbedderTriggeredAndDestroyed"


data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
    pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Eq, Show, Read)
instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "PageAddScriptToEvaluateOnNewDocument" $ \v ->
         PageAddScriptToEvaluateOnNewDocument <$> v .:  "identifier"



pageAddScriptToEvaluateOnNewDocument :: Session -> String -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument session pageAddScriptToEvaluateOnNewDocumentSource = sendReceiveCommandResult (conn session) ("Page","addScriptToEvaluateOnNewDocument") ([("source", ToJSONEx pageAddScriptToEvaluateOnNewDocumentSource)] ++ (catMaybes []))


pageBringToFront :: Session -> IO (Maybe Error)
pageBringToFront session  = sendReceiveCommand (conn session) ("Page","bringToFront") ([] ++ (catMaybes []))

data PageCaptureScreenshot = PageCaptureScreenshot {
    pageCaptureScreenshotData :: String
} deriving (Eq, Show, Read)
instance FromJSON  PageCaptureScreenshot where
    parseJSON = A.withObject "PageCaptureScreenshot" $ \v ->
         PageCaptureScreenshot <$> v .:  "data"



pageCaptureScreenshot :: Session -> Maybe String -> Maybe Int -> Maybe PageViewport -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot session pageCaptureScreenshotFormat pageCaptureScreenshotQuality pageCaptureScreenshotClip = sendReceiveCommandResult (conn session) ("Page","captureScreenshot") ([] ++ (catMaybes [fmap (("format",) . ToJSONEx) pageCaptureScreenshotFormat, fmap (("quality",) . ToJSONEx) pageCaptureScreenshotQuality, fmap (("clip",) . ToJSONEx) pageCaptureScreenshotClip]))

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
    pageCreateIsolatedWorldExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PageCreateIsolatedWorld where
    parseJSON = A.withObject "PageCreateIsolatedWorld" $ \v ->
         PageCreateIsolatedWorld <$> v .:  "executionContextId"



pageCreateIsolatedWorld :: Session -> PageFrameId -> Maybe String -> Maybe Bool -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld session pageCreateIsolatedWorldFrameId pageCreateIsolatedWorldWorldName pageCreateIsolatedWorldGrantUniveralAccess = sendReceiveCommandResult (conn session) ("Page","createIsolatedWorld") ([("frameId", ToJSONEx pageCreateIsolatedWorldFrameId)] ++ (catMaybes [fmap (("worldName",) . ToJSONEx) pageCreateIsolatedWorldWorldName, fmap (("grantUniveralAccess",) . ToJSONEx) pageCreateIsolatedWorldGrantUniveralAccess]))


pageDisable :: Session -> IO (Maybe Error)
pageDisable session  = sendReceiveCommand (conn session) ("Page","disable") ([] ++ (catMaybes []))


pageEnable :: Session -> IO (Maybe Error)
pageEnable session  = sendReceiveCommand (conn session) ("Page","enable") ([] ++ (catMaybes []))

data PageGetAppManifest = PageGetAppManifest {
    pageGetAppManifestUrl :: String,
    pageGetAppManifestErrors :: [PageAppManifestError],
    pageGetAppManifestData :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PageGetAppManifest where
    parseJSON = A.withObject "PageGetAppManifest" $ \v ->
         PageGetAppManifest <$> v .:  "url"
            <*> v  .:  "errors"
            <*> v  .:?  "data"



pageGetAppManifest :: Session -> IO (Either Error PageGetAppManifest)
pageGetAppManifest session  = sendReceiveCommandResult (conn session) ("Page","getAppManifest") ([] ++ (catMaybes []))

data PageGetFrameTree = PageGetFrameTree {
    pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Eq, Show, Read)
instance FromJSON  PageGetFrameTree where
    parseJSON = A.withObject "PageGetFrameTree" $ \v ->
         PageGetFrameTree <$> v .:  "frameTree"



pageGetFrameTree :: Session -> IO (Either Error PageGetFrameTree)
pageGetFrameTree session  = sendReceiveCommandResult (conn session) ("Page","getFrameTree") ([] ++ (catMaybes []))

data PageGetLayoutMetrics = PageGetLayoutMetrics {
    pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
    pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
    pageGetLayoutMetricsCssContentSize :: DOMRect
} deriving (Eq, Show, Read)
instance FromJSON  PageGetLayoutMetrics where
    parseJSON = A.withObject "PageGetLayoutMetrics" $ \v ->
         PageGetLayoutMetrics <$> v .:  "cssLayoutViewport"
            <*> v  .:  "cssVisualViewport"
            <*> v  .:  "cssContentSize"



pageGetLayoutMetrics :: Session -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics session  = sendReceiveCommandResult (conn session) ("Page","getLayoutMetrics") ([] ++ (catMaybes []))

data PageGetNavigationHistory = PageGetNavigationHistory {
    pageGetNavigationHistoryCurrentIndex :: Int,
    pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Eq, Show, Read)
instance FromJSON  PageGetNavigationHistory where
    parseJSON = A.withObject "PageGetNavigationHistory" $ \v ->
         PageGetNavigationHistory <$> v .:  "currentIndex"
            <*> v  .:  "entries"



pageGetNavigationHistory :: Session -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory session  = sendReceiveCommandResult (conn session) ("Page","getNavigationHistory") ([] ++ (catMaybes []))


pageResetNavigationHistory :: Session -> IO (Maybe Error)
pageResetNavigationHistory session  = sendReceiveCommand (conn session) ("Page","resetNavigationHistory") ([] ++ (catMaybes []))


pageHandleJavaScriptDialog :: Session -> Bool -> Maybe String -> IO (Maybe Error)
pageHandleJavaScriptDialog session pageHandleJavaScriptDialogAccept pageHandleJavaScriptDialogPromptText = sendReceiveCommand (conn session) ("Page","handleJavaScriptDialog") ([("accept", ToJSONEx pageHandleJavaScriptDialogAccept)] ++ (catMaybes [fmap (("promptText",) . ToJSONEx) pageHandleJavaScriptDialogPromptText]))

data PageNavigate = PageNavigate {
    pageNavigateFrameId :: PageFrameId,
    pageNavigateLoaderId :: Maybe NetworkLoaderId,
    pageNavigateErrorText :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PageNavigate where
    parseJSON = A.withObject "PageNavigate" $ \v ->
         PageNavigate <$> v .:  "frameId"
            <*> v  .:?  "loaderId"
            <*> v  .:?  "errorText"



pageNavigate :: Session -> String -> Maybe String -> Maybe PageTransitionType -> Maybe PageFrameId -> IO (Either Error PageNavigate)
pageNavigate session pageNavigateUrl pageNavigateReferrer pageNavigateTransitionType pageNavigateFrameId = sendReceiveCommandResult (conn session) ("Page","navigate") ([("url", ToJSONEx pageNavigateUrl)] ++ (catMaybes [fmap (("referrer",) . ToJSONEx) pageNavigateReferrer, fmap (("transitionType",) . ToJSONEx) pageNavigateTransitionType, fmap (("frameId",) . ToJSONEx) pageNavigateFrameId]))


pageNavigateToHistoryEntry :: Session -> Int -> IO (Maybe Error)
pageNavigateToHistoryEntry session pageNavigateToHistoryEntryEntryId = sendReceiveCommand (conn session) ("Page","navigateToHistoryEntry") ([("entryId", ToJSONEx pageNavigateToHistoryEntryEntryId)] ++ (catMaybes []))

data PagePrintToPDF = PagePrintToPDF {
    pagePrintToPDFData :: String
} deriving (Eq, Show, Read)
instance FromJSON  PagePrintToPDF where
    parseJSON = A.withObject "PagePrintToPDF" $ \v ->
         PagePrintToPDF <$> v .:  "data"



pagePrintToPDF :: Session -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> IO (Either Error PagePrintToPDF)
pagePrintToPDF session pagePrintToPdfLandscape pagePrintToPdfDisplayHeaderFooter pagePrintToPdfPrintBackground pagePrintToPdfScale pagePrintToPdfPaperWidth pagePrintToPdfPaperHeight pagePrintToPdfMarginTop pagePrintToPdfMarginBottom pagePrintToPdfMarginLeft pagePrintToPdfMarginRight pagePrintToPdfPageRanges pagePrintToPdfHeaderTemplate pagePrintToPdfFooterTemplate pagePrintToPdfPreferCssPageSize = sendReceiveCommandResult (conn session) ("Page","printToPDF") ([] ++ (catMaybes [fmap (("landscape",) . ToJSONEx) pagePrintToPdfLandscape, fmap (("displayHeaderFooter",) . ToJSONEx) pagePrintToPdfDisplayHeaderFooter, fmap (("printBackground",) . ToJSONEx) pagePrintToPdfPrintBackground, fmap (("scale",) . ToJSONEx) pagePrintToPdfScale, fmap (("paperWidth",) . ToJSONEx) pagePrintToPdfPaperWidth, fmap (("paperHeight",) . ToJSONEx) pagePrintToPdfPaperHeight, fmap (("marginTop",) . ToJSONEx) pagePrintToPdfMarginTop, fmap (("marginBottom",) . ToJSONEx) pagePrintToPdfMarginBottom, fmap (("marginLeft",) . ToJSONEx) pagePrintToPdfMarginLeft, fmap (("marginRight",) . ToJSONEx) pagePrintToPdfMarginRight, fmap (("pageRanges",) . ToJSONEx) pagePrintToPdfPageRanges, fmap (("headerTemplate",) . ToJSONEx) pagePrintToPdfHeaderTemplate, fmap (("footerTemplate",) . ToJSONEx) pagePrintToPdfFooterTemplate, fmap (("preferCSSPageSize",) . ToJSONEx) pagePrintToPdfPreferCssPageSize]))


pageReload :: Session -> Maybe Bool -> Maybe String -> IO (Maybe Error)
pageReload session pageReloadIgnoreCache pageReloadScriptToEvaluateOnLoad = sendReceiveCommand (conn session) ("Page","reload") ([] ++ (catMaybes [fmap (("ignoreCache",) . ToJSONEx) pageReloadIgnoreCache, fmap (("scriptToEvaluateOnLoad",) . ToJSONEx) pageReloadScriptToEvaluateOnLoad]))


pageRemoveScriptToEvaluateOnNewDocument :: Session -> PageScriptIdentifier -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument session pageRemoveScriptToEvaluateOnNewDocumentIdentifier = sendReceiveCommand (conn session) ("Page","removeScriptToEvaluateOnNewDocument") ([("identifier", ToJSONEx pageRemoveScriptToEvaluateOnNewDocumentIdentifier)] ++ (catMaybes []))


pageSetDocumentContent :: Session -> PageFrameId -> String -> IO (Maybe Error)
pageSetDocumentContent session pageSetDocumentContentFrameId pageSetDocumentContentHtml = sendReceiveCommand (conn session) ("Page","setDocumentContent") ([("frameId", ToJSONEx pageSetDocumentContentFrameId), ("html", ToJSONEx pageSetDocumentContentHtml)] ++ (catMaybes []))


pageStopLoading :: Session -> IO (Maybe Error)
pageStopLoading session  = sendReceiveCommand (conn session) ("Page","stopLoading") ([] ++ (catMaybes []))



data PerformanceMetrics = PerformanceMetrics {
    performanceMetricsMetrics :: [PerformanceMetric],
    performanceMetricsTitle :: String
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceMetrics where
    parseJSON = A.withObject "PerformanceMetrics" $ \v ->
         PerformanceMetrics <$> v .:  "metrics"
            <*> v  .:  "title"


instance ToJSON PerformanceMetrics  where
    toJSON v = A.object
        [ "metrics" .= performanceMetricsMetrics v
        , "title" .= performanceMetricsTitle v
        ]


instance Event  PerformanceMetrics where
    eventName  _   =  "Performance.metrics"
    fToHandler _   =  HPerformanceMetrics
    handlerToF _ h =  case h of HPerformanceMetrics f -> Just f; _ -> Nothing


data PerformanceMetric = PerformanceMetric {
    performanceMetricName :: String,
    performanceMetricValue :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceMetric where
    parseJSON = A.withObject "PerformanceMetric" $ \v ->
         PerformanceMetric <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON PerformanceMetric  where
    toJSON v = A.object
        [ "name" .= performanceMetricName v
        , "value" .= performanceMetricValue v
        ]



performanceDisable :: Session -> IO (Maybe Error)
performanceDisable session  = sendReceiveCommand (conn session) ("Performance","disable") ([] ++ (catMaybes []))


performanceEnable :: Session -> Maybe String -> IO (Maybe Error)
performanceEnable session performanceEnableTimeDomain = sendReceiveCommand (conn session) ("Performance","enable") ([] ++ (catMaybes [fmap (("timeDomain",) . ToJSONEx) performanceEnableTimeDomain]))

data PerformanceGetMetrics = PerformanceGetMetrics {
    performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceGetMetrics where
    parseJSON = A.withObject "PerformanceGetMetrics" $ \v ->
         PerformanceGetMetrics <$> v .:  "metrics"



performanceGetMetrics :: Session -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics session  = sendReceiveCommandResult (conn session) ("Performance","getMetrics") ([] ++ (catMaybes []))




type SecurityCertificateId = Int

data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
    deriving (Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
    parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> do
        pure $ case v of
                "blockable" -> SecurityMixedContentTypeBlockable
                "optionally-blockable" -> SecurityMixedContentTypeOptionallyBlockable
                "none" -> SecurityMixedContentTypeNone
                _ -> error "failed to parse SecurityMixedContentType"

instance ToJSON SecurityMixedContentType where
    toJSON v = A.String $
        case v of
                SecurityMixedContentTypeBlockable -> "blockable"
                SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
                SecurityMixedContentTypeNone -> "none"



data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
    deriving (Eq, Show, Read)
instance FromJSON SecuritySecurityState where
    parseJSON = A.withText  "SecuritySecurityState"  $ \v -> do
        pure $ case v of
                "unknown" -> SecuritySecurityStateUnknown
                "neutral" -> SecuritySecurityStateNeutral
                "insecure" -> SecuritySecurityStateInsecure
                "secure" -> SecuritySecurityStateSecure
                "info" -> SecuritySecurityStateInfo
                "insecure-broken" -> SecuritySecurityStateInsecureBroken
                _ -> error "failed to parse SecuritySecurityState"

instance ToJSON SecuritySecurityState where
    toJSON v = A.String $
        case v of
                SecuritySecurityStateUnknown -> "unknown"
                SecuritySecurityStateNeutral -> "neutral"
                SecuritySecurityStateInsecure -> "insecure"
                SecuritySecurityStateSecure -> "secure"
                SecuritySecurityStateInfo -> "info"
                SecuritySecurityStateInsecureBroken -> "insecure-broken"



data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
    securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
    securitySecurityStateExplanationTitle :: String,
    securitySecurityStateExplanationSummary :: String,
    securitySecurityStateExplanationDescription :: String,
    securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
    securitySecurityStateExplanationCertificate :: [String],
    securitySecurityStateExplanationRecommendations :: Maybe [String]
} deriving (Eq, Show, Read)
instance FromJSON  SecuritySecurityStateExplanation where
    parseJSON = A.withObject "SecuritySecurityStateExplanation" $ \v ->
         SecuritySecurityStateExplanation <$> v .:  "securityState"
            <*> v  .:  "title"
            <*> v  .:  "summary"
            <*> v  .:  "description"
            <*> v  .:  "mixedContentType"
            <*> v  .:  "certificate"
            <*> v  .:?  "recommendations"


instance ToJSON SecuritySecurityStateExplanation  where
    toJSON v = A.object
        [ "securityState" .= securitySecurityStateExplanationSecurityState v
        , "title" .= securitySecurityStateExplanationTitle v
        , "summary" .= securitySecurityStateExplanationSummary v
        , "description" .= securitySecurityStateExplanationDescription v
        , "mixedContentType" .= securitySecurityStateExplanationMixedContentType v
        , "certificate" .= securitySecurityStateExplanationCertificate v
        , "recommendations" .= securitySecurityStateExplanationRecommendations v
        ]



data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
    deriving (Eq, Show, Read)
instance FromJSON SecurityCertificateErrorAction where
    parseJSON = A.withText  "SecurityCertificateErrorAction"  $ \v -> do
        pure $ case v of
                "continue" -> SecurityCertificateErrorActionContinue
                "cancel" -> SecurityCertificateErrorActionCancel
                _ -> error "failed to parse SecurityCertificateErrorAction"

instance ToJSON SecurityCertificateErrorAction where
    toJSON v = A.String $
        case v of
                SecurityCertificateErrorActionContinue -> "continue"
                SecurityCertificateErrorActionCancel -> "cancel"



securityDisable :: Session -> IO (Maybe Error)
securityDisable session  = sendReceiveCommand (conn session) ("Security","disable") ([] ++ (catMaybes []))


securityEnable :: Session -> IO (Maybe Error)
securityEnable session  = sendReceiveCommand (conn session) ("Security","enable") ([] ++ (catMaybes []))



data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
    targetReceivedMessageFromTargetSessionId :: TargetSessionID,
    targetReceivedMessageFromTargetMessage :: String
} deriving (Eq, Show, Read)
instance FromJSON  TargetReceivedMessageFromTarget where
    parseJSON = A.withObject "TargetReceivedMessageFromTarget" $ \v ->
         TargetReceivedMessageFromTarget <$> v .:  "sessionId"
            <*> v  .:  "message"


instance ToJSON TargetReceivedMessageFromTarget  where
    toJSON v = A.object
        [ "sessionId" .= targetReceivedMessageFromTargetSessionId v
        , "message" .= targetReceivedMessageFromTargetMessage v
        ]


instance Event  TargetReceivedMessageFromTarget where
    eventName  _   =  "Target.receivedMessageFromTarget"
    fToHandler _   =  HTargetReceivedMessageFromTarget
    handlerToF _ h =  case h of HTargetReceivedMessageFromTarget f -> Just f; _ -> Nothing

data TargetTargetCreated = TargetTargetCreated {
    targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetCreated where
    parseJSON = A.withObject "TargetTargetCreated" $ \v ->
         TargetTargetCreated <$> v .:  "targetInfo"


instance ToJSON TargetTargetCreated  where
    toJSON v = A.object
        [ "targetInfo" .= targetTargetCreatedTargetInfo v
        ]


instance Event  TargetTargetCreated where
    eventName  _   =  "Target.targetCreated"
    fToHandler _   =  HTargetTargetCreated
    handlerToF _ h =  case h of HTargetTargetCreated f -> Just f; _ -> Nothing

data TargetTargetDestroyed = TargetTargetDestroyed {
    targetTargetDestroyedTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetDestroyed where
    parseJSON = A.withObject "TargetTargetDestroyed" $ \v ->
         TargetTargetDestroyed <$> v .:  "targetId"


instance ToJSON TargetTargetDestroyed  where
    toJSON v = A.object
        [ "targetId" .= targetTargetDestroyedTargetId v
        ]


instance Event  TargetTargetDestroyed where
    eventName  _   =  "Target.targetDestroyed"
    fToHandler _   =  HTargetTargetDestroyed
    handlerToF _ h =  case h of HTargetTargetDestroyed f -> Just f; _ -> Nothing

data TargetTargetCrashed = TargetTargetCrashed {
    targetTargetCrashedTargetId :: TargetTargetID,
    targetTargetCrashedStatus :: String,
    targetTargetCrashedErrorCode :: Int
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetCrashed where
    parseJSON = A.withObject "TargetTargetCrashed" $ \v ->
         TargetTargetCrashed <$> v .:  "targetId"
            <*> v  .:  "status"
            <*> v  .:  "errorCode"


instance ToJSON TargetTargetCrashed  where
    toJSON v = A.object
        [ "targetId" .= targetTargetCrashedTargetId v
        , "status" .= targetTargetCrashedStatus v
        , "errorCode" .= targetTargetCrashedErrorCode v
        ]


instance Event  TargetTargetCrashed where
    eventName  _   =  "Target.targetCrashed"
    fToHandler _   =  HTargetTargetCrashed
    handlerToF _ h =  case h of HTargetTargetCrashed f -> Just f; _ -> Nothing

data TargetTargetInfoChanged = TargetTargetInfoChanged {
    targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetInfoChanged where
    parseJSON = A.withObject "TargetTargetInfoChanged" $ \v ->
         TargetTargetInfoChanged <$> v .:  "targetInfo"


instance ToJSON TargetTargetInfoChanged  where
    toJSON v = A.object
        [ "targetInfo" .= targetTargetInfoChangedTargetInfo v
        ]


instance Event  TargetTargetInfoChanged where
    eventName  _   =  "Target.targetInfoChanged"
    fToHandler _   =  HTargetTargetInfoChanged
    handlerToF _ h =  case h of HTargetTargetInfoChanged f -> Just f; _ -> Nothing


type TargetTargetID = String

type TargetSessionID = String

data TargetTargetInfo = TargetTargetInfo {
    targetTargetInfoTargetId :: TargetTargetID,
    targetTargetInfoType :: String,
    targetTargetInfoTitle :: String,
    targetTargetInfoUrl :: String,
    targetTargetInfoAttached :: Bool,
    targetTargetInfoOpenerId :: Maybe TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetInfo where
    parseJSON = A.withObject "TargetTargetInfo" $ \v ->
         TargetTargetInfo <$> v .:  "targetId"
            <*> v  .:  "type"
            <*> v  .:  "title"
            <*> v  .:  "url"
            <*> v  .:  "attached"
            <*> v  .:?  "openerId"


instance ToJSON TargetTargetInfo  where
    toJSON v = A.object
        [ "targetId" .= targetTargetInfoTargetId v
        , "type" .= targetTargetInfoType v
        , "title" .= targetTargetInfoTitle v
        , "url" .= targetTargetInfoUrl v
        , "attached" .= targetTargetInfoAttached v
        , "openerId" .= targetTargetInfoOpenerId v
        ]



targetActivateTarget :: Session -> TargetTargetID -> IO (Maybe Error)
targetActivateTarget session targetActivateTargetTargetId = sendReceiveCommand (conn session) ("Target","activateTarget") ([("targetId", ToJSONEx targetActivateTargetTargetId)] ++ (catMaybes []))

data TargetAttachToTarget = TargetAttachToTarget {
    targetAttachToTargetSessionId :: TargetSessionID
} deriving (Eq, Show, Read)
instance FromJSON  TargetAttachToTarget where
    parseJSON = A.withObject "TargetAttachToTarget" $ \v ->
         TargetAttachToTarget <$> v .:  "sessionId"



targetAttachToTarget :: Session -> TargetTargetID -> Maybe Bool -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget session targetAttachToTargetTargetId targetAttachToTargetFlatten = sendReceiveCommandResult (conn session) ("Target","attachToTarget") ([("targetId", ToJSONEx targetAttachToTargetTargetId)] ++ (catMaybes [fmap (("flatten",) . ToJSONEx) targetAttachToTargetFlatten]))


targetCloseTarget :: Session -> TargetTargetID -> IO (Maybe Error)
targetCloseTarget session targetCloseTargetTargetId = sendReceiveCommand (conn session) ("Target","closeTarget") ([("targetId", ToJSONEx targetCloseTargetTargetId)] ++ (catMaybes []))

data TargetCreateTarget = TargetCreateTarget {
    targetCreateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetCreateTarget where
    parseJSON = A.withObject "TargetCreateTarget" $ \v ->
         TargetCreateTarget <$> v .:  "targetId"



targetCreateTarget :: Session -> String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> IO (Either Error TargetCreateTarget)
targetCreateTarget session targetCreateTargetUrl targetCreateTargetWidth targetCreateTargetHeight targetCreateTargetNewWindow targetCreateTargetBackground = sendReceiveCommandResult (conn session) ("Target","createTarget") ([("url", ToJSONEx targetCreateTargetUrl)] ++ (catMaybes [fmap (("width",) . ToJSONEx) targetCreateTargetWidth, fmap (("height",) . ToJSONEx) targetCreateTargetHeight, fmap (("newWindow",) . ToJSONEx) targetCreateTargetNewWindow, fmap (("background",) . ToJSONEx) targetCreateTargetBackground]))


targetDetachFromTarget :: Session -> Maybe TargetSessionID -> IO (Maybe Error)
targetDetachFromTarget session targetDetachFromTargetSessionId = sendReceiveCommand (conn session) ("Target","detachFromTarget") ([] ++ (catMaybes [fmap (("sessionId",) . ToJSONEx) targetDetachFromTargetSessionId]))

data TargetGetTargets = TargetGetTargets {
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Eq, Show, Read)
instance FromJSON  TargetGetTargets where
    parseJSON = A.withObject "TargetGetTargets" $ \v ->
         TargetGetTargets <$> v .:  "targetInfos"



targetGetTargets :: Session -> IO (Either Error TargetGetTargets)
targetGetTargets session  = sendReceiveCommandResult (conn session) ("Target","getTargets") ([] ++ (catMaybes []))


targetSetDiscoverTargets :: Session -> Bool -> IO (Maybe Error)
targetSetDiscoverTargets session targetSetDiscoverTargetsDiscover = sendReceiveCommand (conn session) ("Target","setDiscoverTargets") ([("discover", ToJSONEx targetSetDiscoverTargetsDiscover)] ++ (catMaybes []))



data FetchRequestPaused = FetchRequestPaused {
    fetchRequestPausedRequestId :: FetchRequestId,
    fetchRequestPausedRequest :: NetworkRequest,
    fetchRequestPausedFrameId :: PageFrameId,
    fetchRequestPausedResourceType :: NetworkResourceType,
    fetchRequestPausedResponseErrorReason :: Maybe NetworkErrorReason,
    fetchRequestPausedResponseStatusCode :: Maybe Int,
    fetchRequestPausedResponseStatusText :: Maybe String,
    fetchRequestPausedResponseHeaders :: Maybe [FetchHeaderEntry],
    fetchRequestPausedNetworkId :: Maybe FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  FetchRequestPaused where
    parseJSON = A.withObject "FetchRequestPaused" $ \v ->
         FetchRequestPaused <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:?  "responseErrorReason"
            <*> v  .:?  "responseStatusCode"
            <*> v  .:?  "responseStatusText"
            <*> v  .:?  "responseHeaders"
            <*> v  .:?  "networkId"


instance ToJSON FetchRequestPaused  where
    toJSON v = A.object
        [ "requestId" .= fetchRequestPausedRequestId v
        , "request" .= fetchRequestPausedRequest v
        , "frameId" .= fetchRequestPausedFrameId v
        , "resourceType" .= fetchRequestPausedResourceType v
        , "responseErrorReason" .= fetchRequestPausedResponseErrorReason v
        , "responseStatusCode" .= fetchRequestPausedResponseStatusCode v
        , "responseStatusText" .= fetchRequestPausedResponseStatusText v
        , "responseHeaders" .= fetchRequestPausedResponseHeaders v
        , "networkId" .= fetchRequestPausedNetworkId v
        ]


instance Event  FetchRequestPaused where
    eventName  _   =  "Fetch.requestPaused"
    fToHandler _   =  HFetchRequestPaused
    handlerToF _ h =  case h of HFetchRequestPaused f -> Just f; _ -> Nothing

data FetchAuthRequired = FetchAuthRequired {
    fetchAuthRequiredRequestId :: FetchRequestId,
    fetchAuthRequiredRequest :: NetworkRequest,
    fetchAuthRequiredFrameId :: PageFrameId,
    fetchAuthRequiredResourceType :: NetworkResourceType,
    fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthRequired where
    parseJSON = A.withObject "FetchAuthRequired" $ \v ->
         FetchAuthRequired <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:  "authChallenge"


instance ToJSON FetchAuthRequired  where
    toJSON v = A.object
        [ "requestId" .= fetchAuthRequiredRequestId v
        , "request" .= fetchAuthRequiredRequest v
        , "frameId" .= fetchAuthRequiredFrameId v
        , "resourceType" .= fetchAuthRequiredResourceType v
        , "authChallenge" .= fetchAuthRequiredAuthChallenge v
        ]


instance Event  FetchAuthRequired where
    eventName  _   =  "Fetch.authRequired"
    fToHandler _   =  HFetchAuthRequired
    handlerToF _ h =  case h of HFetchAuthRequired f -> Just f; _ -> Nothing


type FetchRequestId = String

data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
    deriving (Eq, Show, Read)
instance FromJSON FetchRequestStage where
    parseJSON = A.withText  "FetchRequestStage"  $ \v -> do
        pure $ case v of
                "Request" -> FetchRequestStageRequest
                "Response" -> FetchRequestStageResponse
                _ -> error "failed to parse FetchRequestStage"

instance ToJSON FetchRequestStage where
    toJSON v = A.String $
        case v of
                FetchRequestStageRequest -> "Request"
                FetchRequestStageResponse -> "Response"



data FetchRequestPattern = FetchRequestPattern {
    fetchRequestPatternUrlPattern :: Maybe String,
    fetchRequestPatternResourceType :: Maybe NetworkResourceType,
    fetchRequestPatternRequestStage :: Maybe FetchRequestStage
} deriving (Eq, Show, Read)
instance FromJSON  FetchRequestPattern where
    parseJSON = A.withObject "FetchRequestPattern" $ \v ->
         FetchRequestPattern <$> v .:?  "urlPattern"
            <*> v  .:?  "resourceType"
            <*> v  .:?  "requestStage"


instance ToJSON FetchRequestPattern  where
    toJSON v = A.object
        [ "urlPattern" .= fetchRequestPatternUrlPattern v
        , "resourceType" .= fetchRequestPatternResourceType v
        , "requestStage" .= fetchRequestPatternRequestStage v
        ]



data FetchHeaderEntry = FetchHeaderEntry {
    fetchHeaderEntryName :: String,
    fetchHeaderEntryValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  FetchHeaderEntry where
    parseJSON = A.withObject "FetchHeaderEntry" $ \v ->
         FetchHeaderEntry <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON FetchHeaderEntry  where
    toJSON v = A.object
        [ "name" .= fetchHeaderEntryName v
        , "value" .= fetchHeaderEntryValue v
        ]



data FetchAuthChallenge = FetchAuthChallenge {
    fetchAuthChallengeOrigin :: String,
    fetchAuthChallengeScheme :: String,
    fetchAuthChallengeRealm :: String,
    fetchAuthChallengeSource :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthChallenge where
    parseJSON = A.withObject "FetchAuthChallenge" $ \v ->
         FetchAuthChallenge <$> v .:  "origin"
            <*> v  .:  "scheme"
            <*> v  .:  "realm"
            <*> v  .:?  "source"


instance ToJSON FetchAuthChallenge  where
    toJSON v = A.object
        [ "origin" .= fetchAuthChallengeOrigin v
        , "scheme" .= fetchAuthChallengeScheme v
        , "realm" .= fetchAuthChallengeRealm v
        , "source" .= fetchAuthChallengeSource v
        ]



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
    fetchAuthChallengeResponseResponse :: String,
    fetchAuthChallengeResponseUsername :: Maybe String,
    fetchAuthChallengeResponsePassword :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthChallengeResponse where
    parseJSON = A.withObject "FetchAuthChallengeResponse" $ \v ->
         FetchAuthChallengeResponse <$> v .:  "response"
            <*> v  .:?  "username"
            <*> v  .:?  "password"


instance ToJSON FetchAuthChallengeResponse  where
    toJSON v = A.object
        [ "response" .= fetchAuthChallengeResponseResponse v
        , "username" .= fetchAuthChallengeResponseUsername v
        , "password" .= fetchAuthChallengeResponsePassword v
        ]



fetchDisable :: Session -> IO (Maybe Error)
fetchDisable session  = sendReceiveCommand (conn session) ("Fetch","disable") ([] ++ (catMaybes []))


fetchEnable :: Session -> Maybe [FetchRequestPattern] -> Maybe Bool -> IO (Maybe Error)
fetchEnable session fetchEnablePatterns fetchEnableHandleAuthRequests = sendReceiveCommand (conn session) ("Fetch","enable") ([] ++ (catMaybes [fmap (("patterns",) . ToJSONEx) fetchEnablePatterns, fmap (("handleAuthRequests",) . ToJSONEx) fetchEnableHandleAuthRequests]))


fetchFailRequest :: Session -> FetchRequestId -> NetworkErrorReason -> IO (Maybe Error)
fetchFailRequest session fetchFailRequestRequestId fetchFailRequestErrorReason = sendReceiveCommand (conn session) ("Fetch","failRequest") ([("requestId", ToJSONEx fetchFailRequestRequestId), ("errorReason", ToJSONEx fetchFailRequestErrorReason)] ++ (catMaybes []))


fetchFulfillRequest :: Session -> FetchRequestId -> Int -> Maybe [FetchHeaderEntry] -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
fetchFulfillRequest session fetchFulfillRequestRequestId fetchFulfillRequestResponseCode fetchFulfillRequestResponseHeaders fetchFulfillRequestBinaryResponseHeaders fetchFulfillRequestBody fetchFulfillRequestResponsePhrase = sendReceiveCommand (conn session) ("Fetch","fulfillRequest") ([("requestId", ToJSONEx fetchFulfillRequestRequestId), ("responseCode", ToJSONEx fetchFulfillRequestResponseCode)] ++ (catMaybes [fmap (("responseHeaders",) . ToJSONEx) fetchFulfillRequestResponseHeaders, fmap (("binaryResponseHeaders",) . ToJSONEx) fetchFulfillRequestBinaryResponseHeaders, fmap (("body",) . ToJSONEx) fetchFulfillRequestBody, fmap (("responsePhrase",) . ToJSONEx) fetchFulfillRequestResponsePhrase]))


fetchContinueRequest :: Session -> FetchRequestId -> Maybe String -> Maybe String -> Maybe String -> Maybe [FetchHeaderEntry] -> IO (Maybe Error)
fetchContinueRequest session fetchContinueRequestRequestId fetchContinueRequestUrl fetchContinueRequestMethod fetchContinueRequestPostData fetchContinueRequestHeaders = sendReceiveCommand (conn session) ("Fetch","continueRequest") ([("requestId", ToJSONEx fetchContinueRequestRequestId)] ++ (catMaybes [fmap (("url",) . ToJSONEx) fetchContinueRequestUrl, fmap (("method",) . ToJSONEx) fetchContinueRequestMethod, fmap (("postData",) . ToJSONEx) fetchContinueRequestPostData, fmap (("headers",) . ToJSONEx) fetchContinueRequestHeaders]))


fetchContinueWithAuth :: Session -> FetchRequestId -> FetchAuthChallengeResponse -> IO (Maybe Error)
fetchContinueWithAuth session fetchContinueWithAuthRequestId fetchContinueWithAuthAuthChallengeResponse = sendReceiveCommand (conn session) ("Fetch","continueWithAuth") ([("requestId", ToJSONEx fetchContinueWithAuthRequestId), ("authChallengeResponse", ToJSONEx fetchContinueWithAuthAuthChallengeResponse)] ++ (catMaybes []))

data FetchGetResponseBody = FetchGetResponseBody {
    fetchGetResponseBodyBody :: String,
    fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  FetchGetResponseBody where
    parseJSON = A.withObject "FetchGetResponseBody" $ \v ->
         FetchGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



fetchGetResponseBody :: Session -> FetchRequestId -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody session fetchGetResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Fetch","getResponseBody") ([("requestId", ToJSONEx fetchGetResponseBodyRequestId)] ++ (catMaybes []))

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
    fetchTakeResponseBodyAsStreamStream :: IOStreamHandle
} deriving (Eq, Show, Read)
instance FromJSON  FetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "FetchTakeResponseBodyAsStream" $ \v ->
         FetchTakeResponseBodyAsStream <$> v .:  "stream"



fetchTakeResponseBodyAsStream :: Session -> FetchRequestId -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream session fetchTakeResponseBodyAsStreamRequestId = sendReceiveCommandResult (conn session) ("Fetch","takeResponseBodyAsStream") ([("requestId", ToJSONEx fetchTakeResponseBodyAsStreamRequestId)] ++ (catMaybes []))



data ConsoleMessageAdded = ConsoleMessageAdded {
    consoleMessageAddedMessage :: ConsoleConsoleMessage
} deriving (Eq, Show, Read)
instance FromJSON  ConsoleMessageAdded where
    parseJSON = A.withObject "ConsoleMessageAdded" $ \v ->
         ConsoleMessageAdded <$> v .:  "message"


instance ToJSON ConsoleMessageAdded  where
    toJSON v = A.object
        [ "message" .= consoleMessageAddedMessage v
        ]


instance Event  ConsoleMessageAdded where
    eventName  _   =  "Console.messageAdded"
    fToHandler _   =  HConsoleMessageAdded
    handlerToF _ h =  case h of HConsoleMessageAdded f -> Just f; _ -> Nothing


data ConsoleConsoleMessage = ConsoleConsoleMessage {
    consoleConsoleMessageSource :: String,
    consoleConsoleMessageLevel :: String,
    consoleConsoleMessageText :: String,
    consoleConsoleMessageUrl :: Maybe String,
    consoleConsoleMessageLine :: Maybe Int,
    consoleConsoleMessageColumn :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  ConsoleConsoleMessage where
    parseJSON = A.withObject "ConsoleConsoleMessage" $ \v ->
         ConsoleConsoleMessage <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:?  "url"
            <*> v  .:?  "line"
            <*> v  .:?  "column"


instance ToJSON ConsoleConsoleMessage  where
    toJSON v = A.object
        [ "source" .= consoleConsoleMessageSource v
        , "level" .= consoleConsoleMessageLevel v
        , "text" .= consoleConsoleMessageText v
        , "url" .= consoleConsoleMessageUrl v
        , "line" .= consoleConsoleMessageLine v
        , "column" .= consoleConsoleMessageColumn v
        ]



consoleClearMessages :: Session -> IO (Maybe Error)
consoleClearMessages session  = sendReceiveCommand (conn session) ("Console","clearMessages") ([] ++ (catMaybes []))


consoleDisable :: Session -> IO (Maybe Error)
consoleDisable session  = sendReceiveCommand (conn session) ("Console","disable") ([] ++ (catMaybes []))


consoleEnable :: Session -> IO (Maybe Error)
consoleEnable session  = sendReceiveCommand (conn session) ("Console","enable") ([] ++ (catMaybes []))



data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
    debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
    debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerBreakpointResolved where
    parseJSON = A.withObject "DebuggerBreakpointResolved" $ \v ->
         DebuggerBreakpointResolved <$> v .:  "breakpointId"
            <*> v  .:  "location"


instance ToJSON DebuggerBreakpointResolved  where
    toJSON v = A.object
        [ "breakpointId" .= debuggerBreakpointResolvedBreakpointId v
        , "location" .= debuggerBreakpointResolvedLocation v
        ]


instance Event  DebuggerBreakpointResolved where
    eventName  _   =  "Debugger.breakpointResolved"
    fToHandler _   =  HDebuggerBreakpointResolved
    handlerToF _ h =  case h of HDebuggerBreakpointResolved f -> Just f; _ -> Nothing

data DebuggerPaused = DebuggerPaused {
    debuggerPausedCallFrames :: [DebuggerCallFrame],
    debuggerPausedReason :: String,
    debuggerPausedData :: Maybe [(String, String)],
    debuggerPausedHitBreakpoints :: Maybe [String],
    debuggerPausedAsyncStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerPaused where
    parseJSON = A.withObject "DebuggerPaused" $ \v ->
         DebuggerPaused <$> v .:  "callFrames"
            <*> v  .:  "reason"
            <*> v  .:?  "data"
            <*> v  .:?  "hitBreakpoints"
            <*> v  .:?  "asyncStackTrace"


instance ToJSON DebuggerPaused  where
    toJSON v = A.object
        [ "callFrames" .= debuggerPausedCallFrames v
        , "reason" .= debuggerPausedReason v
        , "data" .= debuggerPausedData v
        , "hitBreakpoints" .= debuggerPausedHitBreakpoints v
        , "asyncStackTrace" .= debuggerPausedAsyncStackTrace v
        ]


instance Event  DebuggerPaused where
    eventName  _   =  "Debugger.paused"
    fToHandler _   =  HDebuggerPaused
    handlerToF _ h =  case h of HDebuggerPaused f -> Just f; _ -> Nothing

data DebuggerResumed = DebuggerResumed
    deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
    parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
        pure $ case v of
                "DebuggerResumed" -> DebuggerResumed
                _ -> error "failed to parse DebuggerResumed"

instance Event  DebuggerResumed where
    eventName  _   =  "Debugger.resumed"
    fToHandler _   =  HDebuggerResumed
    handlerToF _ h =  case h of HDebuggerResumed f -> Just f; _ -> Nothing

data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
    debuggerScriptFailedToParseScriptId :: RuntimeScriptId,
    debuggerScriptFailedToParseUrl :: String,
    debuggerScriptFailedToParseStartLine :: Int,
    debuggerScriptFailedToParseStartColumn :: Int,
    debuggerScriptFailedToParseEndLine :: Int,
    debuggerScriptFailedToParseEndColumn :: Int,
    debuggerScriptFailedToParseExecutionContextId :: RuntimeExecutionContextId,
    debuggerScriptFailedToParseHash :: String,
    debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(String, String)],
    debuggerScriptFailedToParseSourceMapUrl :: Maybe String,
    debuggerScriptFailedToParseHasSourceUrl :: Maybe Bool,
    debuggerScriptFailedToParseIsModule :: Maybe Bool,
    debuggerScriptFailedToParseLength :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScriptFailedToParse where
    parseJSON = A.withObject "DebuggerScriptFailedToParse" $ \v ->
         DebuggerScriptFailedToParse <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "startLine"
            <*> v  .:  "startColumn"
            <*> v  .:  "endLine"
            <*> v  .:  "endColumn"
            <*> v  .:  "executionContextId"
            <*> v  .:  "hash"
            <*> v  .:?  "executionContextAuxData"
            <*> v  .:?  "sourceMapURL"
            <*> v  .:?  "hasSourceURL"
            <*> v  .:?  "isModule"
            <*> v  .:?  "length"


instance ToJSON DebuggerScriptFailedToParse  where
    toJSON v = A.object
        [ "scriptId" .= debuggerScriptFailedToParseScriptId v
        , "url" .= debuggerScriptFailedToParseUrl v
        , "startLine" .= debuggerScriptFailedToParseStartLine v
        , "startColumn" .= debuggerScriptFailedToParseStartColumn v
        , "endLine" .= debuggerScriptFailedToParseEndLine v
        , "endColumn" .= debuggerScriptFailedToParseEndColumn v
        , "executionContextId" .= debuggerScriptFailedToParseExecutionContextId v
        , "hash" .= debuggerScriptFailedToParseHash v
        , "executionContextAuxData" .= debuggerScriptFailedToParseExecutionContextAuxData v
        , "sourceMapURL" .= debuggerScriptFailedToParseSourceMapUrl v
        , "hasSourceURL" .= debuggerScriptFailedToParseHasSourceUrl v
        , "isModule" .= debuggerScriptFailedToParseIsModule v
        , "length" .= debuggerScriptFailedToParseLength v
        ]


instance Event  DebuggerScriptFailedToParse where
    eventName  _   =  "Debugger.scriptFailedToParse"
    fToHandler _   =  HDebuggerScriptFailedToParse
    handlerToF _ h =  case h of HDebuggerScriptFailedToParse f -> Just f; _ -> Nothing

data DebuggerScriptParsed = DebuggerScriptParsed {
    debuggerScriptParsedScriptId :: RuntimeScriptId,
    debuggerScriptParsedUrl :: String,
    debuggerScriptParsedStartLine :: Int,
    debuggerScriptParsedStartColumn :: Int,
    debuggerScriptParsedEndLine :: Int,
    debuggerScriptParsedEndColumn :: Int,
    debuggerScriptParsedExecutionContextId :: RuntimeExecutionContextId,
    debuggerScriptParsedHash :: String,
    debuggerScriptParsedExecutionContextAuxData :: Maybe [(String, String)],
    debuggerScriptParsedSourceMapUrl :: Maybe String,
    debuggerScriptParsedHasSourceUrl :: Maybe Bool,
    debuggerScriptParsedIsModule :: Maybe Bool,
    debuggerScriptParsedLength :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScriptParsed where
    parseJSON = A.withObject "DebuggerScriptParsed" $ \v ->
         DebuggerScriptParsed <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "startLine"
            <*> v  .:  "startColumn"
            <*> v  .:  "endLine"
            <*> v  .:  "endColumn"
            <*> v  .:  "executionContextId"
            <*> v  .:  "hash"
            <*> v  .:?  "executionContextAuxData"
            <*> v  .:?  "sourceMapURL"
            <*> v  .:?  "hasSourceURL"
            <*> v  .:?  "isModule"
            <*> v  .:?  "length"


instance ToJSON DebuggerScriptParsed  where
    toJSON v = A.object
        [ "scriptId" .= debuggerScriptParsedScriptId v
        , "url" .= debuggerScriptParsedUrl v
        , "startLine" .= debuggerScriptParsedStartLine v
        , "startColumn" .= debuggerScriptParsedStartColumn v
        , "endLine" .= debuggerScriptParsedEndLine v
        , "endColumn" .= debuggerScriptParsedEndColumn v
        , "executionContextId" .= debuggerScriptParsedExecutionContextId v
        , "hash" .= debuggerScriptParsedHash v
        , "executionContextAuxData" .= debuggerScriptParsedExecutionContextAuxData v
        , "sourceMapURL" .= debuggerScriptParsedSourceMapUrl v
        , "hasSourceURL" .= debuggerScriptParsedHasSourceUrl v
        , "isModule" .= debuggerScriptParsedIsModule v
        , "length" .= debuggerScriptParsedLength v
        ]


instance Event  DebuggerScriptParsed where
    eventName  _   =  "Debugger.scriptParsed"
    fToHandler _   =  HDebuggerScriptParsed
    handlerToF _ h =  case h of HDebuggerScriptParsed f -> Just f; _ -> Nothing


type DebuggerBreakpointId = String

type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
    debuggerLocationScriptId :: RuntimeScriptId,
    debuggerLocationLineNumber :: Int,
    debuggerLocationColumnNumber :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerLocation where
    parseJSON = A.withObject "DebuggerLocation" $ \v ->
         DebuggerLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"


instance ToJSON DebuggerLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerLocationScriptId v
        , "lineNumber" .= debuggerLocationLineNumber v
        , "columnNumber" .= debuggerLocationColumnNumber v
        ]



data DebuggerCallFrame = DebuggerCallFrame {
    debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
    debuggerCallFrameFunctionName :: String,
    debuggerCallFrameLocation :: DebuggerLocation,
    debuggerCallFrameScopeChain :: [DebuggerScope],
    debuggerCallFrameThis :: RuntimeRemoteObject,
    debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
    debuggerCallFrameReturnValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerCallFrame where
    parseJSON = A.withObject "DebuggerCallFrame" $ \v ->
         DebuggerCallFrame <$> v .:  "callFrameId"
            <*> v  .:  "functionName"
            <*> v  .:  "location"
            <*> v  .:  "scopeChain"
            <*> v  .:  "this"
            <*> v  .:?  "functionLocation"
            <*> v  .:?  "returnValue"


instance ToJSON DebuggerCallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= debuggerCallFrameCallFrameId v
        , "functionName" .= debuggerCallFrameFunctionName v
        , "location" .= debuggerCallFrameLocation v
        , "scopeChain" .= debuggerCallFrameScopeChain v
        , "this" .= debuggerCallFrameThis v
        , "functionLocation" .= debuggerCallFrameFunctionLocation v
        , "returnValue" .= debuggerCallFrameReturnValue v
        ]



data DebuggerScope = DebuggerScope {
    debuggerScopeType :: String,
    debuggerScopeObject :: RuntimeRemoteObject,
    debuggerScopeName :: Maybe String,
    debuggerScopeStartLocation :: Maybe DebuggerLocation,
    debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScope where
    parseJSON = A.withObject "DebuggerScope" $ \v ->
         DebuggerScope <$> v .:  "type"
            <*> v  .:  "object"
            <*> v  .:?  "name"
            <*> v  .:?  "startLocation"
            <*> v  .:?  "endLocation"


instance ToJSON DebuggerScope  where
    toJSON v = A.object
        [ "type" .= debuggerScopeType v
        , "object" .= debuggerScopeObject v
        , "name" .= debuggerScopeName v
        , "startLocation" .= debuggerScopeStartLocation v
        , "endLocation" .= debuggerScopeEndLocation v
        ]



data DebuggerSearchMatch = DebuggerSearchMatch {
    debuggerSearchMatchLineNumber :: Int,
    debuggerSearchMatchLineContent :: String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSearchMatch where
    parseJSON = A.withObject "DebuggerSearchMatch" $ \v ->
         DebuggerSearchMatch <$> v .:  "lineNumber"
            <*> v  .:  "lineContent"


instance ToJSON DebuggerSearchMatch  where
    toJSON v = A.object
        [ "lineNumber" .= debuggerSearchMatchLineNumber v
        , "lineContent" .= debuggerSearchMatchLineContent v
        ]



data DebuggerBreakLocation = DebuggerBreakLocation {
    debuggerBreakLocationScriptId :: RuntimeScriptId,
    debuggerBreakLocationLineNumber :: Int,
    debuggerBreakLocationColumnNumber :: Maybe Int,
    debuggerBreakLocationType :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerBreakLocation where
    parseJSON = A.withObject "DebuggerBreakLocation" $ \v ->
         DebuggerBreakLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "type"


instance ToJSON DebuggerBreakLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerBreakLocationScriptId v
        , "lineNumber" .= debuggerBreakLocationLineNumber v
        , "columnNumber" .= debuggerBreakLocationColumnNumber v
        , "type" .= debuggerBreakLocationType v
        ]



data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
    deriving (Eq, Show, Read)
instance FromJSON DebuggerScriptLanguage where
    parseJSON = A.withText  "DebuggerScriptLanguage"  $ \v -> do
        pure $ case v of
                "JavaScript" -> DebuggerScriptLanguageJavaScript
                "WebAssembly" -> DebuggerScriptLanguageWebAssembly
                _ -> error "failed to parse DebuggerScriptLanguage"

instance ToJSON DebuggerScriptLanguage where
    toJSON v = A.String $
        case v of
                DebuggerScriptLanguageJavaScript -> "JavaScript"
                DebuggerScriptLanguageWebAssembly -> "WebAssembly"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
    debuggerDebugSymbolsType :: String,
    debuggerDebugSymbolsExternalUrl :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerDebugSymbols where
    parseJSON = A.withObject "DebuggerDebugSymbols" $ \v ->
         DebuggerDebugSymbols <$> v .:  "type"
            <*> v  .:?  "externalURL"


instance ToJSON DebuggerDebugSymbols  where
    toJSON v = A.object
        [ "type" .= debuggerDebugSymbolsType v
        , "externalURL" .= debuggerDebugSymbolsExternalUrl v
        ]



debuggerContinueToLocation :: Session -> DebuggerLocation -> Maybe String -> IO (Maybe Error)
debuggerContinueToLocation session debuggerContinueToLocationLocation debuggerContinueToLocationTargetCallFrames = sendReceiveCommand (conn session) ("Debugger","continueToLocation") ([("location", ToJSONEx debuggerContinueToLocationLocation)] ++ (catMaybes [fmap (("targetCallFrames",) . ToJSONEx) debuggerContinueToLocationTargetCallFrames]))


debuggerDisable :: Session -> IO (Maybe Error)
debuggerDisable session  = sendReceiveCommand (conn session) ("Debugger","disable") ([] ++ (catMaybes []))


debuggerEnable :: Session -> IO (Maybe Error)
debuggerEnable session  = sendReceiveCommand (conn session) ("Debugger","enable") ([] ++ (catMaybes []))

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
    debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "DebuggerEvaluateOnCallFrame" $ \v ->
         DebuggerEvaluateOnCallFrame <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



debuggerEvaluateOnCallFrame :: Session -> DebuggerCallFrameId -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame session debuggerEvaluateOnCallFrameCallFrameId debuggerEvaluateOnCallFrameExpression debuggerEvaluateOnCallFrameObjectGroup debuggerEvaluateOnCallFrameIncludeCommandLineApi debuggerEvaluateOnCallFrameSilent debuggerEvaluateOnCallFrameReturnByValue debuggerEvaluateOnCallFrameThrowOnSideEffect = sendReceiveCommandResult (conn session) ("Debugger","evaluateOnCallFrame") ([("callFrameId", ToJSONEx debuggerEvaluateOnCallFrameCallFrameId), ("expression", ToJSONEx debuggerEvaluateOnCallFrameExpression)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) debuggerEvaluateOnCallFrameObjectGroup, fmap (("includeCommandLineAPI",) . ToJSONEx) debuggerEvaluateOnCallFrameIncludeCommandLineApi, fmap (("silent",) . ToJSONEx) debuggerEvaluateOnCallFrameSilent, fmap (("returnByValue",) . ToJSONEx) debuggerEvaluateOnCallFrameReturnByValue, fmap (("throwOnSideEffect",) . ToJSONEx) debuggerEvaluateOnCallFrameThrowOnSideEffect]))

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "DebuggerGetPossibleBreakpoints" $ \v ->
         DebuggerGetPossibleBreakpoints <$> v .:  "locations"



debuggerGetPossibleBreakpoints :: Session -> DebuggerLocation -> Maybe DebuggerLocation -> Maybe Bool -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints session debuggerGetPossibleBreakpointsStart debuggerGetPossibleBreakpointsEnd debuggerGetPossibleBreakpointsRestrictToFunction = sendReceiveCommandResult (conn session) ("Debugger","getPossibleBreakpoints") ([("start", ToJSONEx debuggerGetPossibleBreakpointsStart)] ++ (catMaybes [fmap (("end",) . ToJSONEx) debuggerGetPossibleBreakpointsEnd, fmap (("restrictToFunction",) . ToJSONEx) debuggerGetPossibleBreakpointsRestrictToFunction]))

data DebuggerGetScriptSource = DebuggerGetScriptSource {
    debuggerGetScriptSourceScriptSource :: String,
    debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetScriptSource where
    parseJSON = A.withObject "DebuggerGetScriptSource" $ \v ->
         DebuggerGetScriptSource <$> v .:  "scriptSource"
            <*> v  .:?  "bytecode"



debuggerGetScriptSource :: Session -> RuntimeScriptId -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource session debuggerGetScriptSourceScriptId = sendReceiveCommandResult (conn session) ("Debugger","getScriptSource") ([("scriptId", ToJSONEx debuggerGetScriptSourceScriptId)] ++ (catMaybes []))


debuggerPause :: Session -> IO (Maybe Error)
debuggerPause session  = sendReceiveCommand (conn session) ("Debugger","pause") ([] ++ (catMaybes []))


debuggerRemoveBreakpoint :: Session -> DebuggerBreakpointId -> IO (Maybe Error)
debuggerRemoveBreakpoint session debuggerRemoveBreakpointBreakpointId = sendReceiveCommand (conn session) ("Debugger","removeBreakpoint") ([("breakpointId", ToJSONEx debuggerRemoveBreakpointBreakpointId)] ++ (catMaybes []))


debuggerResume :: Session -> Maybe Bool -> IO (Maybe Error)
debuggerResume session debuggerResumeTerminateOnResume = sendReceiveCommand (conn session) ("Debugger","resume") ([] ++ (catMaybes [fmap (("terminateOnResume",) . ToJSONEx) debuggerResumeTerminateOnResume]))

data DebuggerSearchInContent = DebuggerSearchInContent {
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSearchInContent where
    parseJSON = A.withObject "DebuggerSearchInContent" $ \v ->
         DebuggerSearchInContent <$> v .:  "result"



debuggerSearchInContent :: Session -> RuntimeScriptId -> String -> Maybe Bool -> Maybe Bool -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent session debuggerSearchInContentScriptId debuggerSearchInContentQuery debuggerSearchInContentCaseSensitive debuggerSearchInContentIsRegex = sendReceiveCommandResult (conn session) ("Debugger","searchInContent") ([("scriptId", ToJSONEx debuggerSearchInContentScriptId), ("query", ToJSONEx debuggerSearchInContentQuery)] ++ (catMaybes [fmap (("caseSensitive",) . ToJSONEx) debuggerSearchInContentCaseSensitive, fmap (("isRegex",) . ToJSONEx) debuggerSearchInContentIsRegex]))


debuggerSetAsyncCallStackDepth :: Session -> Int -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth session debuggerSetAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Debugger","setAsyncCallStackDepth") ([("maxDepth", ToJSONEx debuggerSetAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpoint where
    parseJSON = A.withObject "DebuggerSetBreakpoint" $ \v ->
         DebuggerSetBreakpoint <$> v .:  "breakpointId"
            <*> v  .:  "actualLocation"



debuggerSetBreakpoint :: Session -> DebuggerLocation -> Maybe String -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint session debuggerSetBreakpointLocation debuggerSetBreakpointCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpoint") ([("location", ToJSONEx debuggerSetBreakpointLocation)] ++ (catMaybes [fmap (("condition",) . ToJSONEx) debuggerSetBreakpointCondition]))

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "DebuggerSetInstrumentationBreakpoint" $ \v ->
         DebuggerSetInstrumentationBreakpoint <$> v .:  "breakpointId"



debuggerSetInstrumentationBreakpoint :: Session -> String -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint session debuggerSetInstrumentationBreakpointInstrumentation = sendReceiveCommandResult (conn session) ("Debugger","setInstrumentationBreakpoint") ([("instrumentation", ToJSONEx debuggerSetInstrumentationBreakpointInstrumentation)] ++ (catMaybes []))

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "DebuggerSetBreakpointByUrl" $ \v ->
         DebuggerSetBreakpointByUrl <$> v .:  "breakpointId"
            <*> v  .:  "locations"



debuggerSetBreakpointByUrl :: Session -> Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe String -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl session debuggerSetBreakpointByUrlLineNumber debuggerSetBreakpointByUrlUrl debuggerSetBreakpointByUrlUrlRegex debuggerSetBreakpointByUrlScriptHash debuggerSetBreakpointByUrlColumnNumber debuggerSetBreakpointByUrlCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpointByUrl") ([("lineNumber", ToJSONEx debuggerSetBreakpointByUrlLineNumber)] ++ (catMaybes [fmap (("url",) . ToJSONEx) debuggerSetBreakpointByUrlUrl, fmap (("urlRegex",) . ToJSONEx) debuggerSetBreakpointByUrlUrlRegex, fmap (("scriptHash",) . ToJSONEx) debuggerSetBreakpointByUrlScriptHash, fmap (("columnNumber",) . ToJSONEx) debuggerSetBreakpointByUrlColumnNumber, fmap (("condition",) . ToJSONEx) debuggerSetBreakpointByUrlCondition]))


debuggerSetBreakpointsActive :: Session -> Bool -> IO (Maybe Error)
debuggerSetBreakpointsActive session debuggerSetBreakpointsActiveActive = sendReceiveCommand (conn session) ("Debugger","setBreakpointsActive") ([("active", ToJSONEx debuggerSetBreakpointsActiveActive)] ++ (catMaybes []))


debuggerSetPauseOnExceptions :: Session -> String -> IO (Maybe Error)
debuggerSetPauseOnExceptions session debuggerSetPauseOnExceptionsState = sendReceiveCommand (conn session) ("Debugger","setPauseOnExceptions") ([("state", ToJSONEx debuggerSetPauseOnExceptionsState)] ++ (catMaybes []))

data DebuggerSetScriptSource = DebuggerSetScriptSource {
    debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
    debuggerSetScriptSourceStackChanged :: Maybe Bool,
    debuggerSetScriptSourceAsyncStackTrace :: Maybe RuntimeStackTrace,
    debuggerSetScriptSourceExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetScriptSource where
    parseJSON = A.withObject "DebuggerSetScriptSource" $ \v ->
         DebuggerSetScriptSource <$> v .:?  "callFrames"
            <*> v  .:?  "stackChanged"
            <*> v  .:?  "asyncStackTrace"
            <*> v  .:?  "exceptionDetails"



debuggerSetScriptSource :: Session -> RuntimeScriptId -> String -> Maybe Bool -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource session debuggerSetScriptSourceScriptId debuggerSetScriptSourceScriptSource debuggerSetScriptSourceDryRun = sendReceiveCommandResult (conn session) ("Debugger","setScriptSource") ([("scriptId", ToJSONEx debuggerSetScriptSourceScriptId), ("scriptSource", ToJSONEx debuggerSetScriptSourceScriptSource)] ++ (catMaybes [fmap (("dryRun",) . ToJSONEx) debuggerSetScriptSourceDryRun]))


debuggerSetSkipAllPauses :: Session -> Bool -> IO (Maybe Error)
debuggerSetSkipAllPauses session debuggerSetSkipAllPausesSkip = sendReceiveCommand (conn session) ("Debugger","setSkipAllPauses") ([("skip", ToJSONEx debuggerSetSkipAllPausesSkip)] ++ (catMaybes []))


debuggerSetVariableValue :: Session -> Int -> String -> RuntimeCallArgument -> DebuggerCallFrameId -> IO (Maybe Error)
debuggerSetVariableValue session debuggerSetVariableValueScopeNumber debuggerSetVariableValueVariableName debuggerSetVariableValueNewValue debuggerSetVariableValueCallFrameId = sendReceiveCommand (conn session) ("Debugger","setVariableValue") ([("scopeNumber", ToJSONEx debuggerSetVariableValueScopeNumber), ("variableName", ToJSONEx debuggerSetVariableValueVariableName), ("newValue", ToJSONEx debuggerSetVariableValueNewValue), ("callFrameId", ToJSONEx debuggerSetVariableValueCallFrameId)] ++ (catMaybes []))


debuggerStepInto :: Session -> IO (Maybe Error)
debuggerStepInto session  = sendReceiveCommand (conn session) ("Debugger","stepInto") ([] ++ (catMaybes []))


debuggerStepOut :: Session -> IO (Maybe Error)
debuggerStepOut session  = sendReceiveCommand (conn session) ("Debugger","stepOut") ([] ++ (catMaybes []))


debuggerStepOver :: Session -> IO (Maybe Error)
debuggerStepOver session  = sendReceiveCommand (conn session) ("Debugger","stepOver") ([] ++ (catMaybes []))



data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
    profilerConsoleProfileFinishedId :: String,
    profilerConsoleProfileFinishedLocation :: DebuggerLocation,
    profilerConsoleProfileFinishedProfile :: ProfilerProfile,
    profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerConsoleProfileFinished where
    parseJSON = A.withObject "ProfilerConsoleProfileFinished" $ \v ->
         ProfilerConsoleProfileFinished <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:  "profile"
            <*> v  .:?  "title"


instance ToJSON ProfilerConsoleProfileFinished  where
    toJSON v = A.object
        [ "id" .= profilerConsoleProfileFinishedId v
        , "location" .= profilerConsoleProfileFinishedLocation v
        , "profile" .= profilerConsoleProfileFinishedProfile v
        , "title" .= profilerConsoleProfileFinishedTitle v
        ]


instance Event  ProfilerConsoleProfileFinished where
    eventName  _   =  "Profiler.consoleProfileFinished"
    fToHandler _   =  HProfilerConsoleProfileFinished
    handlerToF _ h =  case h of HProfilerConsoleProfileFinished f -> Just f; _ -> Nothing

data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
    profilerConsoleProfileStartedId :: String,
    profilerConsoleProfileStartedLocation :: DebuggerLocation,
    profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerConsoleProfileStarted where
    parseJSON = A.withObject "ProfilerConsoleProfileStarted" $ \v ->
         ProfilerConsoleProfileStarted <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:?  "title"


instance ToJSON ProfilerConsoleProfileStarted  where
    toJSON v = A.object
        [ "id" .= profilerConsoleProfileStartedId v
        , "location" .= profilerConsoleProfileStartedLocation v
        , "title" .= profilerConsoleProfileStartedTitle v
        ]


instance Event  ProfilerConsoleProfileStarted where
    eventName  _   =  "Profiler.consoleProfileStarted"
    fToHandler _   =  HProfilerConsoleProfileStarted
    handlerToF _ h =  case h of HProfilerConsoleProfileStarted f -> Just f; _ -> Nothing


data ProfilerProfileNode = ProfilerProfileNode {
    profilerProfileNodeId :: Int,
    profilerProfileNodeCallFrame :: RuntimeCallFrame,
    profilerProfileNodeHitCount :: Maybe Int,
    profilerProfileNodeChildren :: Maybe [Int],
    profilerProfileNodeDeoptReason :: Maybe String,
    profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerProfileNode where
    parseJSON = A.withObject "ProfilerProfileNode" $ \v ->
         ProfilerProfileNode <$> v .:  "id"
            <*> v  .:  "callFrame"
            <*> v  .:?  "hitCount"
            <*> v  .:?  "children"
            <*> v  .:?  "deoptReason"
            <*> v  .:?  "positionTicks"


instance ToJSON ProfilerProfileNode  where
    toJSON v = A.object
        [ "id" .= profilerProfileNodeId v
        , "callFrame" .= profilerProfileNodeCallFrame v
        , "hitCount" .= profilerProfileNodeHitCount v
        , "children" .= profilerProfileNodeChildren v
        , "deoptReason" .= profilerProfileNodeDeoptReason v
        , "positionTicks" .= profilerProfileNodePositionTicks v
        ]



data ProfilerProfile = ProfilerProfile {
    profilerProfileNodes :: [ProfilerProfileNode],
    profilerProfileStartTime :: Int,
    profilerProfileEndTime :: Int,
    profilerProfileSamples :: Maybe [Int],
    profilerProfileTimeDeltas :: Maybe [Int]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerProfile where
    parseJSON = A.withObject "ProfilerProfile" $ \v ->
         ProfilerProfile <$> v .:  "nodes"
            <*> v  .:  "startTime"
            <*> v  .:  "endTime"
            <*> v  .:?  "samples"
            <*> v  .:?  "timeDeltas"


instance ToJSON ProfilerProfile  where
    toJSON v = A.object
        [ "nodes" .= profilerProfileNodes v
        , "startTime" .= profilerProfileStartTime v
        , "endTime" .= profilerProfileEndTime v
        , "samples" .= profilerProfileSamples v
        , "timeDeltas" .= profilerProfileTimeDeltas v
        ]



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
    profilerPositionTickInfoLine :: Int,
    profilerPositionTickInfoTicks :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerPositionTickInfo where
    parseJSON = A.withObject "ProfilerPositionTickInfo" $ \v ->
         ProfilerPositionTickInfo <$> v .:  "line"
            <*> v  .:  "ticks"


instance ToJSON ProfilerPositionTickInfo  where
    toJSON v = A.object
        [ "line" .= profilerPositionTickInfoLine v
        , "ticks" .= profilerPositionTickInfoTicks v
        ]



data ProfilerCoverageRange = ProfilerCoverageRange {
    profilerCoverageRangeStartOffset :: Int,
    profilerCoverageRangeEndOffset :: Int,
    profilerCoverageRangeCount :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerCoverageRange where
    parseJSON = A.withObject "ProfilerCoverageRange" $ \v ->
         ProfilerCoverageRange <$> v .:  "startOffset"
            <*> v  .:  "endOffset"
            <*> v  .:  "count"


instance ToJSON ProfilerCoverageRange  where
    toJSON v = A.object
        [ "startOffset" .= profilerCoverageRangeStartOffset v
        , "endOffset" .= profilerCoverageRangeEndOffset v
        , "count" .= profilerCoverageRangeCount v
        ]



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
    profilerFunctionCoverageFunctionName :: String,
    profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
    profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerFunctionCoverage where
    parseJSON = A.withObject "ProfilerFunctionCoverage" $ \v ->
         ProfilerFunctionCoverage <$> v .:  "functionName"
            <*> v  .:  "ranges"
            <*> v  .:  "isBlockCoverage"


instance ToJSON ProfilerFunctionCoverage  where
    toJSON v = A.object
        [ "functionName" .= profilerFunctionCoverageFunctionName v
        , "ranges" .= profilerFunctionCoverageRanges v
        , "isBlockCoverage" .= profilerFunctionCoverageIsBlockCoverage v
        ]



data ProfilerScriptCoverage = ProfilerScriptCoverage {
    profilerScriptCoverageScriptId :: RuntimeScriptId,
    profilerScriptCoverageUrl :: String,
    profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerScriptCoverage where
    parseJSON = A.withObject "ProfilerScriptCoverage" $ \v ->
         ProfilerScriptCoverage <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "functions"


instance ToJSON ProfilerScriptCoverage  where
    toJSON v = A.object
        [ "scriptId" .= profilerScriptCoverageScriptId v
        , "url" .= profilerScriptCoverageUrl v
        , "functions" .= profilerScriptCoverageFunctions v
        ]



profilerDisable :: Session -> IO (Maybe Error)
profilerDisable session  = sendReceiveCommand (conn session) ("Profiler","disable") ([] ++ (catMaybes []))


profilerEnable :: Session -> IO (Maybe Error)
profilerEnable session  = sendReceiveCommand (conn session) ("Profiler","enable") ([] ++ (catMaybes []))

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerGetBestEffortCoverage where
    parseJSON = A.withObject "ProfilerGetBestEffortCoverage" $ \v ->
         ProfilerGetBestEffortCoverage <$> v .:  "result"



profilerGetBestEffortCoverage :: Session -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","getBestEffortCoverage") ([] ++ (catMaybes []))


profilerSetSamplingInterval :: Session -> Int -> IO (Maybe Error)
profilerSetSamplingInterval session profilerSetSamplingIntervalInterval = sendReceiveCommand (conn session) ("Profiler","setSamplingInterval") ([("interval", ToJSONEx profilerSetSamplingIntervalInterval)] ++ (catMaybes []))


profilerStart :: Session -> IO (Maybe Error)
profilerStart session  = sendReceiveCommand (conn session) ("Profiler","start") ([] ++ (catMaybes []))

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
    profilerStartPreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStartPreciseCoverage where
    parseJSON = A.withObject "ProfilerStartPreciseCoverage" $ \v ->
         ProfilerStartPreciseCoverage <$> v .:  "timestamp"



profilerStartPreciseCoverage :: Session -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage session profilerStartPreciseCoverageCallCount profilerStartPreciseCoverageDetailed profilerStartPreciseCoverageAllowTriggeredUpdates = sendReceiveCommandResult (conn session) ("Profiler","startPreciseCoverage") ([] ++ (catMaybes [fmap (("callCount",) . ToJSONEx) profilerStartPreciseCoverageCallCount, fmap (("detailed",) . ToJSONEx) profilerStartPreciseCoverageDetailed, fmap (("allowTriggeredUpdates",) . ToJSONEx) profilerStartPreciseCoverageAllowTriggeredUpdates]))

data ProfilerStop = ProfilerStop {
    profilerStopProfile :: ProfilerProfile
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStop where
    parseJSON = A.withObject "ProfilerStop" $ \v ->
         ProfilerStop <$> v .:  "profile"



profilerStop :: Session -> IO (Either Error ProfilerStop)
profilerStop session  = sendReceiveCommandResult (conn session) ("Profiler","stop") ([] ++ (catMaybes []))


profilerStopPreciseCoverage :: Session -> IO (Maybe Error)
profilerStopPreciseCoverage session  = sendReceiveCommand (conn session) ("Profiler","stopPreciseCoverage") ([] ++ (catMaybes []))

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    profilerTakePreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerTakePreciseCoverage where
    parseJSON = A.withObject "ProfilerTakePreciseCoverage" $ \v ->
         ProfilerTakePreciseCoverage <$> v .:  "result"
            <*> v  .:  "timestamp"



profilerTakePreciseCoverage :: Session -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","takePreciseCoverage") ([] ++ (catMaybes []))



data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
    runtimeConsoleApiCalledType :: String,
    runtimeConsoleApiCalledArgs :: [RuntimeRemoteObject],
    runtimeConsoleApiCalledExecutionContextId :: RuntimeExecutionContextId,
    runtimeConsoleApiCalledTimestamp :: RuntimeTimestamp,
    runtimeConsoleApiCalledStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeConsoleApiCalled where
    parseJSON = A.withObject "RuntimeConsoleApiCalled" $ \v ->
         RuntimeConsoleApiCalled <$> v .:  "type"
            <*> v  .:  "args"
            <*> v  .:  "executionContextId"
            <*> v  .:  "timestamp"
            <*> v  .:?  "stackTrace"


instance ToJSON RuntimeConsoleApiCalled  where
    toJSON v = A.object
        [ "type" .= runtimeConsoleApiCalledType v
        , "args" .= runtimeConsoleApiCalledArgs v
        , "executionContextId" .= runtimeConsoleApiCalledExecutionContextId v
        , "timestamp" .= runtimeConsoleApiCalledTimestamp v
        , "stackTrace" .= runtimeConsoleApiCalledStackTrace v
        ]


instance Event  RuntimeConsoleApiCalled where
    eventName  _   =  "Runtime.consoleAPICalled"
    fToHandler _   =  HRuntimeConsoleApiCalled
    handlerToF _ h =  case h of HRuntimeConsoleApiCalled f -> Just f; _ -> Nothing

data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
    runtimeExceptionRevokedReason :: String,
    runtimeExceptionRevokedExceptionId :: Int
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionRevoked where
    parseJSON = A.withObject "RuntimeExceptionRevoked" $ \v ->
         RuntimeExceptionRevoked <$> v .:  "reason"
            <*> v  .:  "exceptionId"


instance ToJSON RuntimeExceptionRevoked  where
    toJSON v = A.object
        [ "reason" .= runtimeExceptionRevokedReason v
        , "exceptionId" .= runtimeExceptionRevokedExceptionId v
        ]


instance Event  RuntimeExceptionRevoked where
    eventName  _   =  "Runtime.exceptionRevoked"
    fToHandler _   =  HRuntimeExceptionRevoked
    handlerToF _ h =  case h of HRuntimeExceptionRevoked f -> Just f; _ -> Nothing

data RuntimeExceptionThrown = RuntimeExceptionThrown {
    runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
    runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionThrown where
    parseJSON = A.withObject "RuntimeExceptionThrown" $ \v ->
         RuntimeExceptionThrown <$> v .:  "timestamp"
            <*> v  .:  "exceptionDetails"


instance ToJSON RuntimeExceptionThrown  where
    toJSON v = A.object
        [ "timestamp" .= runtimeExceptionThrownTimestamp v
        , "exceptionDetails" .= runtimeExceptionThrownExceptionDetails v
        ]


instance Event  RuntimeExceptionThrown where
    eventName  _   =  "Runtime.exceptionThrown"
    fToHandler _   =  HRuntimeExceptionThrown
    handlerToF _ h =  case h of HRuntimeExceptionThrown f -> Just f; _ -> Nothing

data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
    runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextCreated where
    parseJSON = A.withObject "RuntimeExecutionContextCreated" $ \v ->
         RuntimeExecutionContextCreated <$> v .:  "context"


instance ToJSON RuntimeExecutionContextCreated  where
    toJSON v = A.object
        [ "context" .= runtimeExecutionContextCreatedContext v
        ]


instance Event  RuntimeExecutionContextCreated where
    eventName  _   =  "Runtime.executionContextCreated"
    fToHandler _   =  HRuntimeExecutionContextCreated
    handlerToF _ h =  case h of HRuntimeExecutionContextCreated f -> Just f; _ -> Nothing

data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
    runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextDestroyed where
    parseJSON = A.withObject "RuntimeExecutionContextDestroyed" $ \v ->
         RuntimeExecutionContextDestroyed <$> v .:  "executionContextId"


instance ToJSON RuntimeExecutionContextDestroyed  where
    toJSON v = A.object
        [ "executionContextId" .= runtimeExecutionContextDestroyedExecutionContextId v
        ]


instance Event  RuntimeExecutionContextDestroyed where
    eventName  _   =  "Runtime.executionContextDestroyed"
    fToHandler _   =  HRuntimeExecutionContextDestroyed
    handlerToF _ h =  case h of HRuntimeExecutionContextDestroyed f -> Just f; _ -> Nothing

data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
    deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
    parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
        pure $ case v of
                "RuntimeExecutionContextsCleared" -> RuntimeExecutionContextsCleared
                _ -> error "failed to parse RuntimeExecutionContextsCleared"

instance Event  RuntimeExecutionContextsCleared where
    eventName  _   =  "Runtime.executionContextsCleared"
    fToHandler _   =  HRuntimeExecutionContextsCleared
    handlerToF _ h =  case h of HRuntimeExecutionContextsCleared f -> Just f; _ -> Nothing

data RuntimeInspectRequested = RuntimeInspectRequested {
    runtimeInspectRequestedObject :: RuntimeRemoteObject,
    runtimeInspectRequestedHints :: [(String, String)]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeInspectRequested where
    parseJSON = A.withObject "RuntimeInspectRequested" $ \v ->
         RuntimeInspectRequested <$> v .:  "object"
            <*> v  .:  "hints"


instance ToJSON RuntimeInspectRequested  where
    toJSON v = A.object
        [ "object" .= runtimeInspectRequestedObject v
        , "hints" .= runtimeInspectRequestedHints v
        ]


instance Event  RuntimeInspectRequested where
    eventName  _   =  "Runtime.inspectRequested"
    fToHandler _   =  HRuntimeInspectRequested
    handlerToF _ h =  case h of HRuntimeInspectRequested f -> Just f; _ -> Nothing


type RuntimeScriptId = String

data RuntimeWebDriverValue = RuntimeWebDriverValue {
    runtimeWebDriverValueType :: String,
    runtimeWebDriverValueValue :: Maybe Int,
    runtimeWebDriverValueObjectId :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeWebDriverValue where
    parseJSON = A.withObject "RuntimeWebDriverValue" $ \v ->
         RuntimeWebDriverValue <$> v .:  "type"
            <*> v  .:?  "value"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeWebDriverValue  where
    toJSON v = A.object
        [ "type" .= runtimeWebDriverValueType v
        , "value" .= runtimeWebDriverValueValue v
        , "objectId" .= runtimeWebDriverValueObjectId v
        ]



type RuntimeRemoteObjectId = String

type RuntimeUnserializableValue = String

data RuntimeRemoteObject = RuntimeRemoteObject {
    runtimeRemoteObjectType :: String,
    runtimeRemoteObjectSubtype :: Maybe String,
    runtimeRemoteObjectClassName :: Maybe String,
    runtimeRemoteObjectValue :: Maybe Int,
    runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeRemoteObjectDescription :: Maybe String,
    runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeRemoteObject where
    parseJSON = A.withObject "RuntimeRemoteObject" $ \v ->
         RuntimeRemoteObject <$> v .:  "type"
            <*> v  .:?  "subtype"
            <*> v  .:?  "className"
            <*> v  .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "description"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeRemoteObject  where
    toJSON v = A.object
        [ "type" .= runtimeRemoteObjectType v
        , "subtype" .= runtimeRemoteObjectSubtype v
        , "className" .= runtimeRemoteObjectClassName v
        , "value" .= runtimeRemoteObjectValue v
        , "unserializableValue" .= runtimeRemoteObjectUnserializableValue v
        , "description" .= runtimeRemoteObjectDescription v
        , "objectId" .= runtimeRemoteObjectObjectId v
        ]



data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
    runtimePropertyDescriptorName :: String,
    runtimePropertyDescriptorConfigurable :: Bool,
    runtimePropertyDescriptorEnumerable :: Bool,
    runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWritable :: Maybe Bool,
    runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWasThrown :: Maybe Bool,
    runtimePropertyDescriptorIsOwn :: Maybe Bool,
    runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimePropertyDescriptor where
    parseJSON = A.withObject "RuntimePropertyDescriptor" $ \v ->
         RuntimePropertyDescriptor <$> v .:  "name"
            <*> v  .:  "configurable"
            <*> v  .:  "enumerable"
            <*> v  .:?  "value"
            <*> v  .:?  "writable"
            <*> v  .:?  "get"
            <*> v  .:?  "set"
            <*> v  .:?  "wasThrown"
            <*> v  .:?  "isOwn"
            <*> v  .:?  "symbol"


instance ToJSON RuntimePropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimePropertyDescriptorName v
        , "configurable" .= runtimePropertyDescriptorConfigurable v
        , "enumerable" .= runtimePropertyDescriptorEnumerable v
        , "value" .= runtimePropertyDescriptorValue v
        , "writable" .= runtimePropertyDescriptorWritable v
        , "get" .= runtimePropertyDescriptorGet v
        , "set" .= runtimePropertyDescriptorSet v
        , "wasThrown" .= runtimePropertyDescriptorWasThrown v
        , "isOwn" .= runtimePropertyDescriptorIsOwn v
        , "symbol" .= runtimePropertyDescriptorSymbol v
        ]



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
    runtimeInternalPropertyDescriptorName :: String,
    runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeInternalPropertyDescriptor where
    parseJSON = A.withObject "RuntimeInternalPropertyDescriptor" $ \v ->
         RuntimeInternalPropertyDescriptor <$> v .:  "name"
            <*> v  .:?  "value"


instance ToJSON RuntimeInternalPropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimeInternalPropertyDescriptorName v
        , "value" .= runtimeInternalPropertyDescriptorValue v
        ]



data RuntimeCallArgument = RuntimeCallArgument {
    runtimeCallArgumentValue :: Maybe Int,
    runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallArgument where
    parseJSON = A.withObject "RuntimeCallArgument" $ \v ->
         RuntimeCallArgument <$> v .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeCallArgument  where
    toJSON v = A.object
        [ "value" .= runtimeCallArgumentValue v
        , "unserializableValue" .= runtimeCallArgumentUnserializableValue v
        , "objectId" .= runtimeCallArgumentObjectId v
        ]



type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
    runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
    runtimeExecutionContextDescriptionOrigin :: String,
    runtimeExecutionContextDescriptionName :: String,
    runtimeExecutionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextDescription where
    parseJSON = A.withObject "RuntimeExecutionContextDescription" $ \v ->
         RuntimeExecutionContextDescription <$> v .:  "id"
            <*> v  .:  "origin"
            <*> v  .:  "name"
            <*> v  .:?  "auxData"


instance ToJSON RuntimeExecutionContextDescription  where
    toJSON v = A.object
        [ "id" .= runtimeExecutionContextDescriptionId v
        , "origin" .= runtimeExecutionContextDescriptionOrigin v
        , "name" .= runtimeExecutionContextDescriptionName v
        , "auxData" .= runtimeExecutionContextDescriptionAuxData v
        ]



data RuntimeExceptionDetails = RuntimeExceptionDetails {
    runtimeExceptionDetailsExceptionId :: Int,
    runtimeExceptionDetailsText :: String,
    runtimeExceptionDetailsLineNumber :: Int,
    runtimeExceptionDetailsColumnNumber :: Int,
    runtimeExceptionDetailsScriptId :: Maybe RuntimeScriptId,
    runtimeExceptionDetailsUrl :: Maybe String,
    runtimeExceptionDetailsStackTrace :: Maybe RuntimeStackTrace,
    runtimeExceptionDetailsException :: Maybe RuntimeRemoteObject,
    runtimeExceptionDetailsExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionDetails where
    parseJSON = A.withObject "RuntimeExceptionDetails" $ \v ->
         RuntimeExceptionDetails <$> v .:  "exceptionId"
            <*> v  .:  "text"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "scriptId"
            <*> v  .:?  "url"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "exception"
            <*> v  .:?  "executionContextId"


instance ToJSON RuntimeExceptionDetails  where
    toJSON v = A.object
        [ "exceptionId" .= runtimeExceptionDetailsExceptionId v
        , "text" .= runtimeExceptionDetailsText v
        , "lineNumber" .= runtimeExceptionDetailsLineNumber v
        , "columnNumber" .= runtimeExceptionDetailsColumnNumber v
        , "scriptId" .= runtimeExceptionDetailsScriptId v
        , "url" .= runtimeExceptionDetailsUrl v
        , "stackTrace" .= runtimeExceptionDetailsStackTrace v
        , "exception" .= runtimeExceptionDetailsException v
        , "executionContextId" .= runtimeExceptionDetailsExecutionContextId v
        ]



type RuntimeTimestamp = Int

type RuntimeTimeDelta = Int

data RuntimeCallFrame = RuntimeCallFrame {
    runtimeCallFrameFunctionName :: String,
    runtimeCallFrameScriptId :: RuntimeScriptId,
    runtimeCallFrameUrl :: String,
    runtimeCallFrameLineNumber :: Int,
    runtimeCallFrameColumnNumber :: Int
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallFrame where
    parseJSON = A.withObject "RuntimeCallFrame" $ \v ->
         RuntimeCallFrame <$> v .:  "functionName"
            <*> v  .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"


instance ToJSON RuntimeCallFrame  where
    toJSON v = A.object
        [ "functionName" .= runtimeCallFrameFunctionName v
        , "scriptId" .= runtimeCallFrameScriptId v
        , "url" .= runtimeCallFrameUrl v
        , "lineNumber" .= runtimeCallFrameLineNumber v
        , "columnNumber" .= runtimeCallFrameColumnNumber v
        ]



data RuntimeStackTrace = RuntimeStackTrace {
    runtimeStackTraceCallFrames :: [RuntimeCallFrame],
    runtimeStackTraceDescription :: Maybe String,
    runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeStackTrace where
    parseJSON = A.withObject "RuntimeStackTrace" $ \v ->
         RuntimeStackTrace <$> v .:  "callFrames"
            <*> v  .:?  "description"
            <*> v  .:?  "parent"


instance ToJSON RuntimeStackTrace  where
    toJSON v = A.object
        [ "callFrames" .= runtimeStackTraceCallFrames v
        , "description" .= runtimeStackTraceDescription v
        , "parent" .= runtimeStackTraceParent v
        ]


data RuntimeAwaitPromise = RuntimeAwaitPromise {
    runtimeAwaitPromiseResult :: RuntimeRemoteObject,
    runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeAwaitPromise where
    parseJSON = A.withObject "RuntimeAwaitPromise" $ \v ->
         RuntimeAwaitPromise <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeAwaitPromise :: Session -> RuntimeRemoteObjectId -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise session runtimeAwaitPromisePromiseObjectId runtimeAwaitPromiseReturnByValue runtimeAwaitPromiseGeneratePreview = sendReceiveCommandResult (conn session) ("Runtime","awaitPromise") ([("promiseObjectId", ToJSONEx runtimeAwaitPromisePromiseObjectId)] ++ (catMaybes [fmap (("returnByValue",) . ToJSONEx) runtimeAwaitPromiseReturnByValue, fmap (("generatePreview",) . ToJSONEx) runtimeAwaitPromiseGeneratePreview]))

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallFunctionOn where
    parseJSON = A.withObject "RuntimeCallFunctionOn" $ \v ->
         RuntimeCallFunctionOn <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeCallFunctionOn :: Session -> String -> Maybe RuntimeRemoteObjectId -> Maybe [RuntimeCallArgument] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe RuntimeExecutionContextId -> Maybe String -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn session runtimeCallFunctionOnFunctionDeclaration runtimeCallFunctionOnObjectId runtimeCallFunctionOnArguments runtimeCallFunctionOnSilent runtimeCallFunctionOnReturnByValue runtimeCallFunctionOnUserGesture runtimeCallFunctionOnAwaitPromise runtimeCallFunctionOnExecutionContextId runtimeCallFunctionOnObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","callFunctionOn") ([("functionDeclaration", ToJSONEx runtimeCallFunctionOnFunctionDeclaration)] ++ (catMaybes [fmap (("objectId",) . ToJSONEx) runtimeCallFunctionOnObjectId, fmap (("arguments",) . ToJSONEx) runtimeCallFunctionOnArguments, fmap (("silent",) . ToJSONEx) runtimeCallFunctionOnSilent, fmap (("returnByValue",) . ToJSONEx) runtimeCallFunctionOnReturnByValue, fmap (("userGesture",) . ToJSONEx) runtimeCallFunctionOnUserGesture, fmap (("awaitPromise",) . ToJSONEx) runtimeCallFunctionOnAwaitPromise, fmap (("executionContextId",) . ToJSONEx) runtimeCallFunctionOnExecutionContextId, fmap (("objectGroup",) . ToJSONEx) runtimeCallFunctionOnObjectGroup]))

data RuntimeCompileScript = RuntimeCompileScript {
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCompileScript where
    parseJSON = A.withObject "RuntimeCompileScript" $ \v ->
         RuntimeCompileScript <$> v .:?  "scriptId"
            <*> v  .:?  "exceptionDetails"



runtimeCompileScript :: Session -> String -> String -> Bool -> Maybe RuntimeExecutionContextId -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript session runtimeCompileScriptExpression runtimeCompileScriptSourceUrl runtimeCompileScriptPersistScript runtimeCompileScriptExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","compileScript") ([("expression", ToJSONEx runtimeCompileScriptExpression), ("sourceURL", ToJSONEx runtimeCompileScriptSourceUrl), ("persistScript", ToJSONEx runtimeCompileScriptPersistScript)] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) runtimeCompileScriptExecutionContextId]))


runtimeDisable :: Session -> IO (Maybe Error)
runtimeDisable session  = sendReceiveCommand (conn session) ("Runtime","disable") ([] ++ (catMaybes []))


runtimeDiscardConsoleEntries :: Session -> IO (Maybe Error)
runtimeDiscardConsoleEntries session  = sendReceiveCommand (conn session) ("Runtime","discardConsoleEntries") ([] ++ (catMaybes []))


runtimeEnable :: Session -> IO (Maybe Error)
runtimeEnable session  = sendReceiveCommand (conn session) ("Runtime","enable") ([] ++ (catMaybes []))

data RuntimeEvaluate = RuntimeEvaluate {
    runtimeEvaluateResult :: RuntimeRemoteObject,
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeEvaluate where
    parseJSON = A.withObject "RuntimeEvaluate" $ \v ->
         RuntimeEvaluate <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeEvaluate :: Session -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe RuntimeExecutionContextId -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate session runtimeEvaluateExpression runtimeEvaluateObjectGroup runtimeEvaluateIncludeCommandLineApi runtimeEvaluateSilent runtimeEvaluateContextId runtimeEvaluateReturnByValue runtimeEvaluateUserGesture runtimeEvaluateAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","evaluate") ([("expression", ToJSONEx runtimeEvaluateExpression)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) runtimeEvaluateObjectGroup, fmap (("includeCommandLineAPI",) . ToJSONEx) runtimeEvaluateIncludeCommandLineApi, fmap (("silent",) . ToJSONEx) runtimeEvaluateSilent, fmap (("contextId",) . ToJSONEx) runtimeEvaluateContextId, fmap (("returnByValue",) . ToJSONEx) runtimeEvaluateReturnByValue, fmap (("userGesture",) . ToJSONEx) runtimeEvaluateUserGesture, fmap (("awaitPromise",) . ToJSONEx) runtimeEvaluateAwaitPromise]))

data RuntimeGetProperties = RuntimeGetProperties {
    runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
    runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
    runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeGetProperties where
    parseJSON = A.withObject "RuntimeGetProperties" $ \v ->
         RuntimeGetProperties <$> v .:  "result"
            <*> v  .:?  "internalProperties"
            <*> v  .:?  "exceptionDetails"



runtimeGetProperties :: Session -> RuntimeRemoteObjectId -> Maybe Bool -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties session runtimeGetPropertiesObjectId runtimeGetPropertiesOwnProperties = sendReceiveCommandResult (conn session) ("Runtime","getProperties") ([("objectId", ToJSONEx runtimeGetPropertiesObjectId)] ++ (catMaybes [fmap (("ownProperties",) . ToJSONEx) runtimeGetPropertiesOwnProperties]))

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
    runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "RuntimeGlobalLexicalScopeNames" $ \v ->
         RuntimeGlobalLexicalScopeNames <$> v .:  "names"



runtimeGlobalLexicalScopeNames :: Session -> Maybe RuntimeExecutionContextId -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames session runtimeGlobalLexicalScopeNamesExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","globalLexicalScopeNames") ([] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) runtimeGlobalLexicalScopeNamesExecutionContextId]))

data RuntimeQueryObjects = RuntimeQueryObjects {
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeQueryObjects where
    parseJSON = A.withObject "RuntimeQueryObjects" $ \v ->
         RuntimeQueryObjects <$> v .:  "objects"



runtimeQueryObjects :: Session -> RuntimeRemoteObjectId -> Maybe String -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects session runtimeQueryObjectsPrototypeObjectId runtimeQueryObjectsObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","queryObjects") ([("prototypeObjectId", ToJSONEx runtimeQueryObjectsPrototypeObjectId)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) runtimeQueryObjectsObjectGroup]))


runtimeReleaseObject :: Session -> RuntimeRemoteObjectId -> IO (Maybe Error)
runtimeReleaseObject session runtimeReleaseObjectObjectId = sendReceiveCommand (conn session) ("Runtime","releaseObject") ([("objectId", ToJSONEx runtimeReleaseObjectObjectId)] ++ (catMaybes []))


runtimeReleaseObjectGroup :: Session -> String -> IO (Maybe Error)
runtimeReleaseObjectGroup session runtimeReleaseObjectGroupObjectGroup = sendReceiveCommand (conn session) ("Runtime","releaseObjectGroup") ([("objectGroup", ToJSONEx runtimeReleaseObjectGroupObjectGroup)] ++ (catMaybes []))


runtimeRunIfWaitingForDebugger :: Session -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger session  = sendReceiveCommand (conn session) ("Runtime","runIfWaitingForDebugger") ([] ++ (catMaybes []))

data RuntimeRunScript = RuntimeRunScript {
    runtimeRunScriptResult :: RuntimeRemoteObject,
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeRunScript where
    parseJSON = A.withObject "RuntimeRunScript" $ \v ->
         RuntimeRunScript <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeRunScript :: Session -> RuntimeScriptId -> Maybe RuntimeExecutionContextId -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeRunScript)
runtimeRunScript session runtimeRunScriptScriptId runtimeRunScriptExecutionContextId runtimeRunScriptObjectGroup runtimeRunScriptSilent runtimeRunScriptIncludeCommandLineApi runtimeRunScriptReturnByValue runtimeRunScriptGeneratePreview runtimeRunScriptAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","runScript") ([("scriptId", ToJSONEx runtimeRunScriptScriptId)] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) runtimeRunScriptExecutionContextId, fmap (("objectGroup",) . ToJSONEx) runtimeRunScriptObjectGroup, fmap (("silent",) . ToJSONEx) runtimeRunScriptSilent, fmap (("includeCommandLineAPI",) . ToJSONEx) runtimeRunScriptIncludeCommandLineApi, fmap (("returnByValue",) . ToJSONEx) runtimeRunScriptReturnByValue, fmap (("generatePreview",) . ToJSONEx) runtimeRunScriptGeneratePreview, fmap (("awaitPromise",) . ToJSONEx) runtimeRunScriptAwaitPromise]))


runtimeSetAsyncCallStackDepth :: Session -> Int -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth session runtimeSetAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Runtime","setAsyncCallStackDepth") ([("maxDepth", ToJSONEx runtimeSetAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))




data SchemaDomain = SchemaDomain {
    schemaDomainName :: String,
    schemaDomainVersion :: String
} deriving (Eq, Show, Read)
instance FromJSON  SchemaDomain where
    parseJSON = A.withObject "SchemaDomain" $ \v ->
         SchemaDomain <$> v .:  "name"
            <*> v  .:  "version"


instance ToJSON SchemaDomain  where
    toJSON v = A.object
        [ "name" .= schemaDomainName v
        , "version" .= schemaDomainVersion v
        ]


data SchemaGetDomains = SchemaGetDomains {
    schemaGetDomainsDomains :: [SchemaDomain]
} deriving (Eq, Show, Read)
instance FromJSON  SchemaGetDomains where
    parseJSON = A.withObject "SchemaGetDomains" $ \v ->
         SchemaGetDomains <$> v .:  "domains"



schemaGetDomains :: Session -> IO (Either Error SchemaGetDomains)
schemaGetDomains session  = sendReceiveCommandResult (conn session) ("Schema","getDomains") ([] ++ (catMaybes []))




