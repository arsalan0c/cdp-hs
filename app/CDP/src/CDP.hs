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

import CDPPrelude


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






browserClose :: Session -> IO (Maybe Error)
browserClose session = sendReceiveCommand session "Browser.close" (Nothing :: Maybe ())

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



instance Command  BrowserGetVersion where
    commandName _ = "Browser.getVersion"


browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session = sendReceiveCommandResult session "Browser.getVersion" (Nothing :: Maybe ())



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


instance FromEvent Event DOMAttributeModified where
    eventName  _ _    =  "DOM.attributeModified"
    fromEvent ev =  case ev of EVDOMAttributeModified v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMAttributeRemoved where
    eventName  _ _    =  "DOM.attributeRemoved"
    fromEvent ev =  case ev of EVDOMAttributeRemoved v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMCharacterDataModified where
    eventName  _ _    =  "DOM.characterDataModified"
    fromEvent ev =  case ev of EVDOMCharacterDataModified v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMChildNodeCountUpdated where
    eventName  _ _    =  "DOM.childNodeCountUpdated"
    fromEvent ev =  case ev of EVDOMChildNodeCountUpdated v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMChildNodeInserted where
    eventName  _ _    =  "DOM.childNodeInserted"
    fromEvent ev =  case ev of EVDOMChildNodeInserted v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMChildNodeRemoved where
    eventName  _ _    =  "DOM.childNodeRemoved"
    fromEvent ev =  case ev of EVDOMChildNodeRemoved v -> Just v; _ -> Nothing

data DOMDocumentUpdated = DOMDocumentUpdated
    deriving (Eq, Show, Read)
instance FromJSON DOMDocumentUpdated where
    parseJSON = A.withText  "DOMDocumentUpdated"  $ \v -> do
        case v of
                "DOMDocumentUpdated" -> pure $ DOMDocumentUpdated
                _ -> fail "failed to parse DOMDocumentUpdated"

instance FromEvent Event DOMDocumentUpdated where
    eventName  _ _    =  "DOM.documentUpdated"
    fromEvent ev =  case ev of EVDOMDocumentUpdated v -> Just v; _ -> Nothing

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


instance FromEvent Event DOMSetChildNodes where
    eventName  _ _    =  "DOM.setChildNodes"
    fromEvent ev =  case ev of EVDOMSetChildNodes v -> Just v; _ -> Nothing


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
        case v of
                "first-line" -> pure $ DOMPseudoTypeFirstLine
                "first-letter" -> pure $ DOMPseudoTypeFirstLetter
                "before" -> pure $ DOMPseudoTypeBefore
                "after" -> pure $ DOMPseudoTypeAfter
                "marker" -> pure $ DOMPseudoTypeMarker
                "backdrop" -> pure $ DOMPseudoTypeBackdrop
                "selection" -> pure $ DOMPseudoTypeSelection
                "target-text" -> pure $ DOMPseudoTypeTargetText
                "spelling-error" -> pure $ DOMPseudoTypeSpellingError
                "grammar-error" -> pure $ DOMPseudoTypeGrammarError
                "highlight" -> pure $ DOMPseudoTypeHighlight
                "first-line-inherited" -> pure $ DOMPseudoTypeFirstLineInherited
                "scrollbar" -> pure $ DOMPseudoTypeScrollbar
                "scrollbar-thumb" -> pure $ DOMPseudoTypeScrollbarThumb
                "scrollbar-button" -> pure $ DOMPseudoTypeScrollbarButton
                "scrollbar-track" -> pure $ DOMPseudoTypeScrollbarTrack
                "scrollbar-track-piece" -> pure $ DOMPseudoTypeScrollbarTrackPiece
                "scrollbar-corner" -> pure $ DOMPseudoTypeScrollbarCorner
                "resizer" -> pure $ DOMPseudoTypeResizer
                "input-list-button" -> pure $ DOMPseudoTypeInputListButton
                "page-transition" -> pure $ DOMPseudoTypePageTransition
                "page-transition-container" -> pure $ DOMPseudoTypePageTransitionContainer
                "page-transition-image-wrapper" -> pure $ DOMPseudoTypePageTransitionImageWrapper
                "page-transition-outgoing-image" -> pure $ DOMPseudoTypePageTransitionOutgoingImage
                "page-transition-incoming-image" -> pure $ DOMPseudoTypePageTransitionIncomingImage
                _ -> fail "failed to parse DOMPseudoType"

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
        case v of
                "user-agent" -> pure $ DOMShadowRootTypeUserAgent
                "open" -> pure $ DOMShadowRootTypeOpen
                "closed" -> pure $ DOMShadowRootTypeClosed
                _ -> fail "failed to parse DOMShadowRootType"

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
        case v of
                "QuirksMode" -> pure $ DOMCompatibilityModeQuirksMode
                "LimitedQuirksMode" -> pure $ DOMCompatibilityModeLimitedQuirksMode
                "NoQuirksMode" -> pure $ DOMCompatibilityModeNoQuirksMode
                _ -> fail "failed to parse DOMCompatibilityMode"

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
    domrgbar :: Int,
    domrgbag :: Int,
    domrgbab :: Int,
    domrgbaa :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMRGBA where
    parseJSON = A.withObject "DOMRGBA" $ \v ->
         DOMRGBA <$> v .:  "r"
            <*> v  .:  "g"
            <*> v  .:  "b"
            <*> v  .:?  "a"


instance ToJSON DOMRGBA  where
    toJSON v = A.object
        [ "r" .= domrgbar v
        , "g" .= domrgbag v
        , "b" .= domrgbab v
        , "a" .= domrgbaa v
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


data DOMDescribeNode = DOMDescribeNode {
    dOMDescribeNodeNode :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMDescribeNode where
    parseJSON = A.withObject "DOMDescribeNode" $ \v ->
         DOMDescribeNode <$> v .:  "node"



instance Command  DOMDescribeNode where
    commandName _ = "DOM.describeNode"

data PDOMDescribeNode = PDOMDescribeNode {
    pdomDescribeNodeNodeId :: Maybe DOMNodeId,
    pdomDescribeNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pdomDescribeNodeObjectId :: Maybe RuntimeRemoteObjectId,
    pdomDescribeNodeDepth :: Maybe Int,
    pdomDescribeNodePierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDescribeNode where
    parseJSON = A.withObject "PDOMDescribeNode" $ \v ->
         PDOMDescribeNode <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMDescribeNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomDescribeNodeNodeId v
        , "backendNodeId" .= pdomDescribeNodeBackendNodeId v
        , "objectId" .= pdomDescribeNodeObjectId v
        , "depth" .= pdomDescribeNodeDepth v
        , "pierce" .= pdomDescribeNodePierce v
        ]


dOMDescribeNode :: Session -> PDOMDescribeNode -> IO (Either Error DOMDescribeNode)
dOMDescribeNode session params = sendReceiveCommandResult session "DOM.describeNode" (Just params)




dOMDisable :: Session -> IO (Maybe Error)
dOMDisable session = sendReceiveCommand session "DOM.disable" (Nothing :: Maybe ())




dOMEnable :: Session -> IO (Maybe Error)
dOMEnable session = sendReceiveCommand session "DOM.enable" (Nothing :: Maybe ())



data PDOMFocus = PDOMFocus {
    pdomFocusNodeId :: Maybe DOMNodeId,
    pdomFocusBackendNodeId :: Maybe DOMBackendNodeId,
    pdomFocusObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMFocus where
    parseJSON = A.withObject "PDOMFocus" $ \v ->
         PDOMFocus <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMFocus  where
    toJSON v = A.object
        [ "nodeId" .= pdomFocusNodeId v
        , "backendNodeId" .= pdomFocusBackendNodeId v
        , "objectId" .= pdomFocusObjectId v
        ]


dOMFocus :: Session -> PDOMFocus -> IO (Maybe Error)
dOMFocus session params = sendReceiveCommand session "DOM.focus" (Just params)

data DOMGetAttributes = DOMGetAttributes {
    dOMGetAttributesAttributes :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetAttributes where
    parseJSON = A.withObject "DOMGetAttributes" $ \v ->
         DOMGetAttributes <$> v .:  "attributes"



instance Command  DOMGetAttributes where
    commandName _ = "DOM.getAttributes"

data PDOMGetAttributes = PDOMGetAttributes {
    pdomGetAttributesNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetAttributes where
    parseJSON = A.withObject "PDOMGetAttributes" $ \v ->
         PDOMGetAttributes <$> v .:  "nodeId"


instance ToJSON PDOMGetAttributes  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetAttributesNodeId v
        ]


dOMGetAttributes :: Session -> PDOMGetAttributes -> IO (Either Error DOMGetAttributes)
dOMGetAttributes session params = sendReceiveCommandResult session "DOM.getAttributes" (Just params)

data DOMGetBoxModel = DOMGetBoxModel {
    dOMGetBoxModelModel :: DOMBoxModel
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetBoxModel where
    parseJSON = A.withObject "DOMGetBoxModel" $ \v ->
         DOMGetBoxModel <$> v .:  "model"



instance Command  DOMGetBoxModel where
    commandName _ = "DOM.getBoxModel"

data PDOMGetBoxModel = PDOMGetBoxModel {
    pdomGetBoxModelNodeId :: Maybe DOMNodeId,
    pdomGetBoxModelBackendNodeId :: Maybe DOMBackendNodeId,
    pdomGetBoxModelObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetBoxModel where
    parseJSON = A.withObject "PDOMGetBoxModel" $ \v ->
         PDOMGetBoxModel <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMGetBoxModel  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetBoxModelNodeId v
        , "backendNodeId" .= pdomGetBoxModelBackendNodeId v
        , "objectId" .= pdomGetBoxModelObjectId v
        ]


dOMGetBoxModel :: Session -> PDOMGetBoxModel -> IO (Either Error DOMGetBoxModel)
dOMGetBoxModel session params = sendReceiveCommandResult session "DOM.getBoxModel" (Just params)

data DOMGetDocument = DOMGetDocument {
    dOMGetDocumentRoot :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetDocument where
    parseJSON = A.withObject "DOMGetDocument" $ \v ->
         DOMGetDocument <$> v .:  "root"



instance Command  DOMGetDocument where
    commandName _ = "DOM.getDocument"

data PDOMGetDocument = PDOMGetDocument {
    pdomGetDocumentDepth :: Maybe Int,
    pdomGetDocumentPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetDocument where
    parseJSON = A.withObject "PDOMGetDocument" $ \v ->
         PDOMGetDocument <$> v .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMGetDocument  where
    toJSON v = A.object
        [ "depth" .= pdomGetDocumentDepth v
        , "pierce" .= pdomGetDocumentPierce v
        ]


dOMGetDocument :: Session -> PDOMGetDocument -> IO (Either Error DOMGetDocument)
dOMGetDocument session params = sendReceiveCommandResult session "DOM.getDocument" (Just params)

data DOMGetNodeForLocation = DOMGetNodeForLocation {
    dOMGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    dOMGetNodeForLocationFrameId :: PageFrameId,
    dOMGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetNodeForLocation where
    parseJSON = A.withObject "DOMGetNodeForLocation" $ \v ->
         DOMGetNodeForLocation <$> v .:  "backendNodeId"
            <*> v  .:  "frameId"
            <*> v  .:?  "nodeId"



instance Command  DOMGetNodeForLocation where
    commandName _ = "DOM.getNodeForLocation"

data PDOMGetNodeForLocation = PDOMGetNodeForLocation {
    pdomGetNodeForLocationX :: Int,
    pdomGetNodeForLocationY :: Int,
    pdomGetNodeForLocationIncludeUserAgentShadowDom :: Maybe Bool,
    pdomGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetNodeForLocation where
    parseJSON = A.withObject "PDOMGetNodeForLocation" $ \v ->
         PDOMGetNodeForLocation <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "includeUserAgentShadowDOM"
            <*> v  .:?  "ignorePointerEventsNone"


instance ToJSON PDOMGetNodeForLocation  where
    toJSON v = A.object
        [ "x" .= pdomGetNodeForLocationX v
        , "y" .= pdomGetNodeForLocationY v
        , "includeUserAgentShadowDOM" .= pdomGetNodeForLocationIncludeUserAgentShadowDom v
        , "ignorePointerEventsNone" .= pdomGetNodeForLocationIgnorePointerEventsNone v
        ]


dOMGetNodeForLocation :: Session -> PDOMGetNodeForLocation -> IO (Either Error DOMGetNodeForLocation)
dOMGetNodeForLocation session params = sendReceiveCommandResult session "DOM.getNodeForLocation" (Just params)

data DOMGetOuterHtml = DOMGetOuterHtml {
    dOMGetOuterHtmlOuterHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetOuterHtml where
    parseJSON = A.withObject "DOMGetOuterHtml" $ \v ->
         DOMGetOuterHtml <$> v .:  "outerHTML"



instance Command  DOMGetOuterHtml where
    commandName _ = "DOM.getOuterHTML"

data PDOMGetOuterHtml = PDOMGetOuterHtml {
    pdomGetOuterHtmlNodeId :: Maybe DOMNodeId,
    pdomGetOuterHtmlBackendNodeId :: Maybe DOMBackendNodeId,
    pdomGetOuterHtmlObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetOuterHtml where
    parseJSON = A.withObject "PDOMGetOuterHtml" $ \v ->
         PDOMGetOuterHtml <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMGetOuterHtml  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetOuterHtmlNodeId v
        , "backendNodeId" .= pdomGetOuterHtmlBackendNodeId v
        , "objectId" .= pdomGetOuterHtmlObjectId v
        ]


dOMGetOuterHtml :: Session -> PDOMGetOuterHtml -> IO (Either Error DOMGetOuterHtml)
dOMGetOuterHtml session params = sendReceiveCommandResult session "DOM.getOuterHTML" (Just params)




dOMHideHighlight :: Session -> IO (Maybe Error)
dOMHideHighlight session = sendReceiveCommand session "DOM.hideHighlight" (Nothing :: Maybe ())




dOMHighlightNode :: Session -> IO (Maybe Error)
dOMHighlightNode session = sendReceiveCommand session "DOM.highlightNode" (Nothing :: Maybe ())




dOMHighlightRect :: Session -> IO (Maybe Error)
dOMHighlightRect session = sendReceiveCommand session "DOM.highlightRect" (Nothing :: Maybe ())

data DOMMoveTo = DOMMoveTo {
    dOMMoveToNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMMoveTo where
    parseJSON = A.withObject "DOMMoveTo" $ \v ->
         DOMMoveTo <$> v .:  "nodeId"



instance Command  DOMMoveTo where
    commandName _ = "DOM.moveTo"

data PDOMMoveTo = PDOMMoveTo {
    pdomMoveToNodeId :: DOMNodeId,
    pdomMoveToTargetNodeId :: DOMNodeId,
    pdomMoveToInsertBeforeNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMMoveTo where
    parseJSON = A.withObject "PDOMMoveTo" $ \v ->
         PDOMMoveTo <$> v .:  "nodeId"
            <*> v  .:  "targetNodeId"
            <*> v  .:?  "insertBeforeNodeId"


instance ToJSON PDOMMoveTo  where
    toJSON v = A.object
        [ "nodeId" .= pdomMoveToNodeId v
        , "targetNodeId" .= pdomMoveToTargetNodeId v
        , "insertBeforeNodeId" .= pdomMoveToInsertBeforeNodeId v
        ]


dOMMoveTo :: Session -> PDOMMoveTo -> IO (Either Error DOMMoveTo)
dOMMoveTo session params = sendReceiveCommandResult session "DOM.moveTo" (Just params)

data DOMQuerySelector = DOMQuerySelector {
    dOMQuerySelectorNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMQuerySelector where
    parseJSON = A.withObject "DOMQuerySelector" $ \v ->
         DOMQuerySelector <$> v .:  "nodeId"



instance Command  DOMQuerySelector where
    commandName _ = "DOM.querySelector"

data PDOMQuerySelector = PDOMQuerySelector {
    pdomQuerySelectorNodeId :: DOMNodeId,
    pdomQuerySelectorSelector :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMQuerySelector where
    parseJSON = A.withObject "PDOMQuerySelector" $ \v ->
         PDOMQuerySelector <$> v .:  "nodeId"
            <*> v  .:  "selector"


instance ToJSON PDOMQuerySelector  where
    toJSON v = A.object
        [ "nodeId" .= pdomQuerySelectorNodeId v
        , "selector" .= pdomQuerySelectorSelector v
        ]


dOMQuerySelector :: Session -> PDOMQuerySelector -> IO (Either Error DOMQuerySelector)
dOMQuerySelector session params = sendReceiveCommandResult session "DOM.querySelector" (Just params)

data DOMQuerySelectorAll = DOMQuerySelectorAll {
    dOMQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving (Eq, Show, Read)
instance FromJSON  DOMQuerySelectorAll where
    parseJSON = A.withObject "DOMQuerySelectorAll" $ \v ->
         DOMQuerySelectorAll <$> v .:  "nodeIds"



instance Command  DOMQuerySelectorAll where
    commandName _ = "DOM.querySelectorAll"

data PDOMQuerySelectorAll = PDOMQuerySelectorAll {
    pdomQuerySelectorAllNodeId :: DOMNodeId,
    pdomQuerySelectorAllSelector :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMQuerySelectorAll where
    parseJSON = A.withObject "PDOMQuerySelectorAll" $ \v ->
         PDOMQuerySelectorAll <$> v .:  "nodeId"
            <*> v  .:  "selector"


instance ToJSON PDOMQuerySelectorAll  where
    toJSON v = A.object
        [ "nodeId" .= pdomQuerySelectorAllNodeId v
        , "selector" .= pdomQuerySelectorAllSelector v
        ]


dOMQuerySelectorAll :: Session -> PDOMQuerySelectorAll -> IO (Either Error DOMQuerySelectorAll)
dOMQuerySelectorAll session params = sendReceiveCommandResult session "DOM.querySelectorAll" (Just params)



data PDOMRemoveAttribute = PDOMRemoveAttribute {
    pdomRemoveAttributeNodeId :: DOMNodeId,
    pdomRemoveAttributeName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRemoveAttribute where
    parseJSON = A.withObject "PDOMRemoveAttribute" $ \v ->
         PDOMRemoveAttribute <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON PDOMRemoveAttribute  where
    toJSON v = A.object
        [ "nodeId" .= pdomRemoveAttributeNodeId v
        , "name" .= pdomRemoveAttributeName v
        ]


dOMRemoveAttribute :: Session -> PDOMRemoveAttribute -> IO (Maybe Error)
dOMRemoveAttribute session params = sendReceiveCommand session "DOM.removeAttribute" (Just params)



data PDOMRemoveNode = PDOMRemoveNode {
    pdomRemoveNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRemoveNode where
    parseJSON = A.withObject "PDOMRemoveNode" $ \v ->
         PDOMRemoveNode <$> v .:  "nodeId"


instance ToJSON PDOMRemoveNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomRemoveNodeNodeId v
        ]


dOMRemoveNode :: Session -> PDOMRemoveNode -> IO (Maybe Error)
dOMRemoveNode session params = sendReceiveCommand session "DOM.removeNode" (Just params)



data PDOMRequestChildNodes = PDOMRequestChildNodes {
    pdomRequestChildNodesNodeId :: DOMNodeId,
    pdomRequestChildNodesDepth :: Maybe Int,
    pdomRequestChildNodesPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRequestChildNodes where
    parseJSON = A.withObject "PDOMRequestChildNodes" $ \v ->
         PDOMRequestChildNodes <$> v .:  "nodeId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMRequestChildNodes  where
    toJSON v = A.object
        [ "nodeId" .= pdomRequestChildNodesNodeId v
        , "depth" .= pdomRequestChildNodesDepth v
        , "pierce" .= pdomRequestChildNodesPierce v
        ]


dOMRequestChildNodes :: Session -> PDOMRequestChildNodes -> IO (Maybe Error)
dOMRequestChildNodes session params = sendReceiveCommand session "DOM.requestChildNodes" (Just params)

data DOMRequestNode = DOMRequestNode {
    dOMRequestNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMRequestNode where
    parseJSON = A.withObject "DOMRequestNode" $ \v ->
         DOMRequestNode <$> v .:  "nodeId"



instance Command  DOMRequestNode where
    commandName _ = "DOM.requestNode"

data PDOMRequestNode = PDOMRequestNode {
    pdomRequestNodeObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRequestNode where
    parseJSON = A.withObject "PDOMRequestNode" $ \v ->
         PDOMRequestNode <$> v .:  "objectId"


instance ToJSON PDOMRequestNode  where
    toJSON v = A.object
        [ "objectId" .= pdomRequestNodeObjectId v
        ]


dOMRequestNode :: Session -> PDOMRequestNode -> IO (Either Error DOMRequestNode)
dOMRequestNode session params = sendReceiveCommandResult session "DOM.requestNode" (Just params)

data DOMResolveNode = DOMResolveNode {
    dOMResolveNodeObject :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  DOMResolveNode where
    parseJSON = A.withObject "DOMResolveNode" $ \v ->
         DOMResolveNode <$> v .:  "object"



instance Command  DOMResolveNode where
    commandName _ = "DOM.resolveNode"

data PDOMResolveNode = PDOMResolveNode {
    pdomResolveNodeNodeId :: Maybe DOMNodeId,
    pdomResolveNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pdomResolveNodeObjectGroup :: Maybe String,
    pdomResolveNodeExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMResolveNode where
    parseJSON = A.withObject "PDOMResolveNode" $ \v ->
         PDOMResolveNode <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "executionContextId"


instance ToJSON PDOMResolveNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomResolveNodeNodeId v
        , "backendNodeId" .= pdomResolveNodeBackendNodeId v
        , "objectGroup" .= pdomResolveNodeObjectGroup v
        , "executionContextId" .= pdomResolveNodeExecutionContextId v
        ]


dOMResolveNode :: Session -> PDOMResolveNode -> IO (Either Error DOMResolveNode)
dOMResolveNode session params = sendReceiveCommandResult session "DOM.resolveNode" (Just params)



data PDOMSetAttributeValue = PDOMSetAttributeValue {
    pdomSetAttributeValueNodeId :: DOMNodeId,
    pdomSetAttributeValueName :: String,
    pdomSetAttributeValueValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetAttributeValue where
    parseJSON = A.withObject "PDOMSetAttributeValue" $ \v ->
         PDOMSetAttributeValue <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"


instance ToJSON PDOMSetAttributeValue  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetAttributeValueNodeId v
        , "name" .= pdomSetAttributeValueName v
        , "value" .= pdomSetAttributeValueValue v
        ]


dOMSetAttributeValue :: Session -> PDOMSetAttributeValue -> IO (Maybe Error)
dOMSetAttributeValue session params = sendReceiveCommand session "DOM.setAttributeValue" (Just params)



data PDOMSetAttributesAsText = PDOMSetAttributesAsText {
    pdomSetAttributesAsTextNodeId :: DOMNodeId,
    pdomSetAttributesAsTextText :: String,
    pdomSetAttributesAsTextName :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetAttributesAsText where
    parseJSON = A.withObject "PDOMSetAttributesAsText" $ \v ->
         PDOMSetAttributesAsText <$> v .:  "nodeId"
            <*> v  .:  "text"
            <*> v  .:?  "name"


instance ToJSON PDOMSetAttributesAsText  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetAttributesAsTextNodeId v
        , "text" .= pdomSetAttributesAsTextText v
        , "name" .= pdomSetAttributesAsTextName v
        ]


dOMSetAttributesAsText :: Session -> PDOMSetAttributesAsText -> IO (Maybe Error)
dOMSetAttributesAsText session params = sendReceiveCommand session "DOM.setAttributesAsText" (Just params)



data PDOMSetFileInputFiles = PDOMSetFileInputFiles {
    pdomSetFileInputFilesFiles :: [String],
    pdomSetFileInputFilesNodeId :: Maybe DOMNodeId,
    pdomSetFileInputFilesBackendNodeId :: Maybe DOMBackendNodeId,
    pdomSetFileInputFilesObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetFileInputFiles where
    parseJSON = A.withObject "PDOMSetFileInputFiles" $ \v ->
         PDOMSetFileInputFiles <$> v .:  "files"
            <*> v  .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMSetFileInputFiles  where
    toJSON v = A.object
        [ "files" .= pdomSetFileInputFilesFiles v
        , "nodeId" .= pdomSetFileInputFilesNodeId v
        , "backendNodeId" .= pdomSetFileInputFilesBackendNodeId v
        , "objectId" .= pdomSetFileInputFilesObjectId v
        ]


dOMSetFileInputFiles :: Session -> PDOMSetFileInputFiles -> IO (Maybe Error)
dOMSetFileInputFiles session params = sendReceiveCommand session "DOM.setFileInputFiles" (Just params)

data DOMSetNodeName = DOMSetNodeName {
    dOMSetNodeNameNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMSetNodeName where
    parseJSON = A.withObject "DOMSetNodeName" $ \v ->
         DOMSetNodeName <$> v .:  "nodeId"



instance Command  DOMSetNodeName where
    commandName _ = "DOM.setNodeName"

data PDOMSetNodeName = PDOMSetNodeName {
    pdomSetNodeNameNodeId :: DOMNodeId,
    pdomSetNodeNameName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetNodeName where
    parseJSON = A.withObject "PDOMSetNodeName" $ \v ->
         PDOMSetNodeName <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON PDOMSetNodeName  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetNodeNameNodeId v
        , "name" .= pdomSetNodeNameName v
        ]


dOMSetNodeName :: Session -> PDOMSetNodeName -> IO (Either Error DOMSetNodeName)
dOMSetNodeName session params = sendReceiveCommandResult session "DOM.setNodeName" (Just params)



data PDOMSetNodeValue = PDOMSetNodeValue {
    pdomSetNodeValueNodeId :: DOMNodeId,
    pdomSetNodeValueValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetNodeValue where
    parseJSON = A.withObject "PDOMSetNodeValue" $ \v ->
         PDOMSetNodeValue <$> v .:  "nodeId"
            <*> v  .:  "value"


instance ToJSON PDOMSetNodeValue  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetNodeValueNodeId v
        , "value" .= pdomSetNodeValueValue v
        ]


dOMSetNodeValue :: Session -> PDOMSetNodeValue -> IO (Maybe Error)
dOMSetNodeValue session params = sendReceiveCommand session "DOM.setNodeValue" (Just params)



data PDOMSetOuterHtml = PDOMSetOuterHtml {
    pdomSetOuterHtmlNodeId :: DOMNodeId,
    pdomSetOuterHtmlOuterHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetOuterHtml where
    parseJSON = A.withObject "PDOMSetOuterHtml" $ \v ->
         PDOMSetOuterHtml <$> v .:  "nodeId"
            <*> v  .:  "outerHTML"


instance ToJSON PDOMSetOuterHtml  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetOuterHtmlNodeId v
        , "outerHTML" .= pdomSetOuterHtmlOuterHtml v
        ]


dOMSetOuterHtml :: Session -> PDOMSetOuterHtml -> IO (Maybe Error)
dOMSetOuterHtml session params = sendReceiveCommand session "DOM.setOuterHTML" (Just params)




data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
    deriving (Eq, Show, Read)
instance FromJSON DOMDebuggerDOMBreakpointType where
    parseJSON = A.withText  "DOMDebuggerDOMBreakpointType"  $ \v -> do
        case v of
                "subtree-modified" -> pure $ DOMDebuggerDOMBreakpointTypeSubtreeModified
                "attribute-modified" -> pure $ DOMDebuggerDOMBreakpointTypeAttributeModified
                "node-removed" -> pure $ DOMDebuggerDOMBreakpointTypeNodeRemoved
                _ -> fail "failed to parse DOMDebuggerDOMBreakpointType"

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


data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners {
    dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Eq, Show, Read)
instance FromJSON  DOMDebuggerGetEventListeners where
    parseJSON = A.withObject "DOMDebuggerGetEventListeners" $ \v ->
         DOMDebuggerGetEventListeners <$> v .:  "listeners"



instance Command  DOMDebuggerGetEventListeners where
    commandName _ = "DOMDebugger.getEventListeners"

data PDOMDebuggerGetEventListeners = PDOMDebuggerGetEventListeners {
    pdomDebuggerGetEventListenersObjectId :: RuntimeRemoteObjectId,
    pdomDebuggerGetEventListenersDepth :: Maybe Int,
    pdomDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerGetEventListeners where
    parseJSON = A.withObject "PDOMDebuggerGetEventListeners" $ \v ->
         PDOMDebuggerGetEventListeners <$> v .:  "objectId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMDebuggerGetEventListeners  where
    toJSON v = A.object
        [ "objectId" .= pdomDebuggerGetEventListenersObjectId v
        , "depth" .= pdomDebuggerGetEventListenersDepth v
        , "pierce" .= pdomDebuggerGetEventListenersPierce v
        ]


dOMDebuggerGetEventListeners :: Session -> PDOMDebuggerGetEventListeners -> IO (Either Error DOMDebuggerGetEventListeners)
dOMDebuggerGetEventListeners session params = sendReceiveCommandResult session "DOMDebugger.getEventListeners" (Just params)



data PDOMDebuggerRemoveDomBreakpoint = PDOMDebuggerRemoveDomBreakpoint {
    pdomDebuggerRemoveDomBreakpointNodeId :: DOMNodeId,
    pdomDebuggerRemoveDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveDomBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveDomBreakpoint" $ \v ->
         PDOMDebuggerRemoveDomBreakpoint <$> v .:  "nodeId"
            <*> v  .:  "type"


instance ToJSON PDOMDebuggerRemoveDomBreakpoint  where
    toJSON v = A.object
        [ "nodeId" .= pdomDebuggerRemoveDomBreakpointNodeId v
        , "type" .= pdomDebuggerRemoveDomBreakpointType v
        ]


dOMDebuggerRemoveDomBreakpoint :: Session -> PDOMDebuggerRemoveDomBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveDomBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeDOMBreakpoint" (Just params)



data PDOMDebuggerRemoveEventListenerBreakpoint = PDOMDebuggerRemoveEventListenerBreakpoint {
    pdomDebuggerRemoveEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveEventListenerBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveEventListenerBreakpoint" $ \v ->
         PDOMDebuggerRemoveEventListenerBreakpoint <$> v .:  "eventName"


instance ToJSON PDOMDebuggerRemoveEventListenerBreakpoint  where
    toJSON v = A.object
        [ "eventName" .= pdomDebuggerRemoveEventListenerBreakpointEventName v
        ]


dOMDebuggerRemoveEventListenerBreakpoint :: Session -> PDOMDebuggerRemoveEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveEventListenerBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeEventListenerBreakpoint" (Just params)



data PDOMDebuggerRemoveXhrBreakpoint = PDOMDebuggerRemoveXhrBreakpoint {
    pdomDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveXhrBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveXhrBreakpoint" $ \v ->
         PDOMDebuggerRemoveXhrBreakpoint <$> v .:  "url"


instance ToJSON PDOMDebuggerRemoveXhrBreakpoint  where
    toJSON v = A.object
        [ "url" .= pdomDebuggerRemoveXhrBreakpointUrl v
        ]


dOMDebuggerRemoveXhrBreakpoint :: Session -> PDOMDebuggerRemoveXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveXhrBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeXHRBreakpoint" (Just params)



data PDOMDebuggerSetDomBreakpoint = PDOMDebuggerSetDomBreakpoint {
    pdomDebuggerSetDomBreakpointNodeId :: DOMNodeId,
    pdomDebuggerSetDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetDomBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetDomBreakpoint" $ \v ->
         PDOMDebuggerSetDomBreakpoint <$> v .:  "nodeId"
            <*> v  .:  "type"


instance ToJSON PDOMDebuggerSetDomBreakpoint  where
    toJSON v = A.object
        [ "nodeId" .= pdomDebuggerSetDomBreakpointNodeId v
        , "type" .= pdomDebuggerSetDomBreakpointType v
        ]


dOMDebuggerSetDomBreakpoint :: Session -> PDOMDebuggerSetDomBreakpoint -> IO (Maybe Error)
dOMDebuggerSetDomBreakpoint session params = sendReceiveCommand session "DOMDebugger.setDOMBreakpoint" (Just params)



data PDOMDebuggerSetEventListenerBreakpoint = PDOMDebuggerSetEventListenerBreakpoint {
    pdomDebuggerSetEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetEventListenerBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetEventListenerBreakpoint" $ \v ->
         PDOMDebuggerSetEventListenerBreakpoint <$> v .:  "eventName"


instance ToJSON PDOMDebuggerSetEventListenerBreakpoint  where
    toJSON v = A.object
        [ "eventName" .= pdomDebuggerSetEventListenerBreakpointEventName v
        ]


dOMDebuggerSetEventListenerBreakpoint :: Session -> PDOMDebuggerSetEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerSetEventListenerBreakpoint session params = sendReceiveCommand session "DOMDebugger.setEventListenerBreakpoint" (Just params)



data PDOMDebuggerSetXhrBreakpoint = PDOMDebuggerSetXhrBreakpoint {
    pdomDebuggerSetXhrBreakpointUrl :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetXhrBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetXhrBreakpoint" $ \v ->
         PDOMDebuggerSetXhrBreakpoint <$> v .:  "url"


instance ToJSON PDOMDebuggerSetXhrBreakpoint  where
    toJSON v = A.object
        [ "url" .= pdomDebuggerSetXhrBreakpointUrl v
        ]


dOMDebuggerSetXhrBreakpoint :: Session -> PDOMDebuggerSetXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerSetXhrBreakpoint session params = sendReceiveCommand session "DOMDebugger.setXHRBreakpoint" (Just params)




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



instance Command  EmulationCanEmulate where
    commandName _ = "Emulation.canEmulate"


emulationCanEmulate :: Session -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate session = sendReceiveCommandResult session "Emulation.canEmulate" (Nothing :: Maybe ())




emulationClearDeviceMetricsOverride :: Session -> IO (Maybe Error)
emulationClearDeviceMetricsOverride session = sendReceiveCommand session "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())




emulationClearGeolocationOverride :: Session -> IO (Maybe Error)
emulationClearGeolocationOverride session = sendReceiveCommand session "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())



data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
    pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMRGBA
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
    parseJSON = A.withObject "PEmulationSetDefaultBackgroundColorOverride" $ \v ->
         PEmulationSetDefaultBackgroundColorOverride <$> v .:?  "color"


instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
    toJSON v = A.object
        [ "color" .= pEmulationSetDefaultBackgroundColorOverrideColor v
        ]


emulationSetDefaultBackgroundColorOverride :: Session -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride session params = sendReceiveCommand session "Emulation.setDefaultBackgroundColorOverride" (Just params)



data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
    pEmulationSetDeviceMetricsOverrideWidth :: Int,
    pEmulationSetDeviceMetricsOverrideHeight :: Int,
    pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Int,
    pEmulationSetDeviceMetricsOverrideMobile :: Bool,
    pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetDeviceMetricsOverride where
    parseJSON = A.withObject "PEmulationSetDeviceMetricsOverride" $ \v ->
         PEmulationSetDeviceMetricsOverride <$> v .:  "width"
            <*> v  .:  "height"
            <*> v  .:  "deviceScaleFactor"
            <*> v  .:  "mobile"
            <*> v  .:?  "screenOrientation"


instance ToJSON PEmulationSetDeviceMetricsOverride  where
    toJSON v = A.object
        [ "width" .= pEmulationSetDeviceMetricsOverrideWidth v
        , "height" .= pEmulationSetDeviceMetricsOverrideHeight v
        , "deviceScaleFactor" .= pEmulationSetDeviceMetricsOverrideDeviceScaleFactor v
        , "mobile" .= pEmulationSetDeviceMetricsOverrideMobile v
        , "screenOrientation" .= pEmulationSetDeviceMetricsOverrideScreenOrientation v
        ]


emulationSetDeviceMetricsOverride :: Session -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride session params = sendReceiveCommand session "Emulation.setDeviceMetricsOverride" (Just params)



data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
    pEmulationSetEmulatedMediaMedia :: Maybe String,
    pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetEmulatedMedia where
    parseJSON = A.withObject "PEmulationSetEmulatedMedia" $ \v ->
         PEmulationSetEmulatedMedia <$> v .:?  "media"
            <*> v  .:?  "features"


instance ToJSON PEmulationSetEmulatedMedia  where
    toJSON v = A.object
        [ "media" .= pEmulationSetEmulatedMediaMedia v
        , "features" .= pEmulationSetEmulatedMediaFeatures v
        ]


emulationSetEmulatedMedia :: Session -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia session params = sendReceiveCommand session "Emulation.setEmulatedMedia" (Just params)



data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
    pEmulationSetGeolocationOverrideLatitude :: Maybe Int,
    pEmulationSetGeolocationOverrideLongitude :: Maybe Int,
    pEmulationSetGeolocationOverrideAccuracy :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetGeolocationOverride where
    parseJSON = A.withObject "PEmulationSetGeolocationOverride" $ \v ->
         PEmulationSetGeolocationOverride <$> v .:?  "latitude"
            <*> v  .:?  "longitude"
            <*> v  .:?  "accuracy"


instance ToJSON PEmulationSetGeolocationOverride  where
    toJSON v = A.object
        [ "latitude" .= pEmulationSetGeolocationOverrideLatitude v
        , "longitude" .= pEmulationSetGeolocationOverrideLongitude v
        , "accuracy" .= pEmulationSetGeolocationOverrideAccuracy v
        ]


emulationSetGeolocationOverride :: Session -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride session params = sendReceiveCommand session "Emulation.setGeolocationOverride" (Just params)



data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
    pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetScriptExecutionDisabled where
    parseJSON = A.withObject "PEmulationSetScriptExecutionDisabled" $ \v ->
         PEmulationSetScriptExecutionDisabled <$> v .:  "value"


instance ToJSON PEmulationSetScriptExecutionDisabled  where
    toJSON v = A.object
        [ "value" .= pEmulationSetScriptExecutionDisabledValue v
        ]


emulationSetScriptExecutionDisabled :: Session -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled session params = sendReceiveCommand session "Emulation.setScriptExecutionDisabled" (Just params)



data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
    pEmulationSetTouchEmulationEnabledEnabled :: Bool,
    pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetTouchEmulationEnabled where
    parseJSON = A.withObject "PEmulationSetTouchEmulationEnabled" $ \v ->
         PEmulationSetTouchEmulationEnabled <$> v .:  "enabled"
            <*> v  .:?  "maxTouchPoints"


instance ToJSON PEmulationSetTouchEmulationEnabled  where
    toJSON v = A.object
        [ "enabled" .= pEmulationSetTouchEmulationEnabledEnabled v
        , "maxTouchPoints" .= pEmulationSetTouchEmulationEnabledMaxTouchPoints v
        ]


emulationSetTouchEmulationEnabled :: Session -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled session params = sendReceiveCommand session "Emulation.setTouchEmulationEnabled" (Just params)



data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
    pEmulationSetUserAgentOverrideUserAgent :: String,
    pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pEmulationSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetUserAgentOverride where
    parseJSON = A.withObject "PEmulationSetUserAgentOverride" $ \v ->
         PEmulationSetUserAgentOverride <$> v .:  "userAgent"
            <*> v  .:?  "acceptLanguage"
            <*> v  .:?  "platform"


instance ToJSON PEmulationSetUserAgentOverride  where
    toJSON v = A.object
        [ "userAgent" .= pEmulationSetUserAgentOverrideUserAgent v
        , "acceptLanguage" .= pEmulationSetUserAgentOverrideAcceptLanguage v
        , "platform" .= pEmulationSetUserAgentOverridePlatform v
        ]


emulationSetUserAgentOverride :: Session -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride session params = sendReceiveCommand session "Emulation.setUserAgentOverride" (Just params)




type IOStreamHandle = String


data PIOClose = PIOClose {
    pioCloseHandle :: IOStreamHandle
} deriving (Eq, Show, Read)
instance FromJSON  PIOClose where
    parseJSON = A.withObject "PIOClose" $ \v ->
         PIOClose <$> v .:  "handle"


instance ToJSON PIOClose  where
    toJSON v = A.object
        [ "handle" .= pioCloseHandle v
        ]


iOClose :: Session -> PIOClose -> IO (Maybe Error)
iOClose session params = sendReceiveCommand session "IO.close" (Just params)

data IORead = IORead {
    iOReadData :: String,
    iOReadEof :: Bool,
    iOReadBase64Encoded :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  IORead where
    parseJSON = A.withObject "IORead" $ \v ->
         IORead <$> v .:  "data"
            <*> v  .:  "eof"
            <*> v  .:?  "base64Encoded"



instance Command  IORead where
    commandName _ = "IO.read"

data PIORead = PIORead {
    pioReadHandle :: IOStreamHandle,
    pioReadOffset :: Maybe Int,
    pioReadSize :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PIORead where
    parseJSON = A.withObject "PIORead" $ \v ->
         PIORead <$> v .:  "handle"
            <*> v  .:?  "offset"
            <*> v  .:?  "size"


instance ToJSON PIORead  where
    toJSON v = A.object
        [ "handle" .= pioReadHandle v
        , "offset" .= pioReadOffset v
        , "size" .= pioReadSize v
        ]


iORead :: Session -> PIORead -> IO (Either Error IORead)
iORead session params = sendReceiveCommandResult session "IO.read" (Just params)

data IOResolveBlob = IOResolveBlob {
    iOResolveBlobUuid :: String
} deriving (Eq, Show, Read)
instance FromJSON  IOResolveBlob where
    parseJSON = A.withObject "IOResolveBlob" $ \v ->
         IOResolveBlob <$> v .:  "uuid"



instance Command  IOResolveBlob where
    commandName _ = "IO.resolveBlob"

data PIOResolveBlob = PIOResolveBlob {
    pioResolveBlobObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PIOResolveBlob where
    parseJSON = A.withObject "PIOResolveBlob" $ \v ->
         PIOResolveBlob <$> v .:  "objectId"


instance ToJSON PIOResolveBlob  where
    toJSON v = A.object
        [ "objectId" .= pioResolveBlobObjectId v
        ]


iOResolveBlob :: Session -> PIOResolveBlob -> IO (Either Error IOResolveBlob)
iOResolveBlob session params = sendReceiveCommandResult session "IO.resolveBlob" (Just params)




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
        case v of
                "none" -> pure $ InputMouseButtonNone
                "left" -> pure $ InputMouseButtonLeft
                "middle" -> pure $ InputMouseButtonMiddle
                "right" -> pure $ InputMouseButtonRight
                "back" -> pure $ InputMouseButtonBack
                "forward" -> pure $ InputMouseButtonForward
                _ -> fail "failed to parse InputMouseButton"

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


data PInputDispatchKeyEvent = PInputDispatchKeyEvent {
    pInputDispatchKeyEventType :: String,
    pInputDispatchKeyEventModifiers :: Maybe Int,
    pInputDispatchKeyEventTimestamp :: Maybe InputTimeSinceEpoch,
    pInputDispatchKeyEventText :: Maybe String,
    pInputDispatchKeyEventUnmodifiedText :: Maybe String,
    pInputDispatchKeyEventKeyIdentifier :: Maybe String,
    pInputDispatchKeyEventCode :: Maybe String,
    pInputDispatchKeyEventKey :: Maybe String,
    pInputDispatchKeyEventWindowsVirtualKeyCode :: Maybe Int,
    pInputDispatchKeyEventNativeVirtualKeyCode :: Maybe Int,
    pInputDispatchKeyEventAutoRepeat :: Maybe Bool,
    pInputDispatchKeyEventIsKeypad :: Maybe Bool,
    pInputDispatchKeyEventIsSystemKey :: Maybe Bool,
    pInputDispatchKeyEventLocation :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchKeyEvent where
    parseJSON = A.withObject "PInputDispatchKeyEvent" $ \v ->
         PInputDispatchKeyEvent <$> v .:  "type"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"
            <*> v  .:?  "text"
            <*> v  .:?  "unmodifiedText"
            <*> v  .:?  "keyIdentifier"
            <*> v  .:?  "code"
            <*> v  .:?  "key"
            <*> v  .:?  "windowsVirtualKeyCode"
            <*> v  .:?  "nativeVirtualKeyCode"
            <*> v  .:?  "autoRepeat"
            <*> v  .:?  "isKeypad"
            <*> v  .:?  "isSystemKey"
            <*> v  .:?  "location"


instance ToJSON PInputDispatchKeyEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchKeyEventType v
        , "modifiers" .= pInputDispatchKeyEventModifiers v
        , "timestamp" .= pInputDispatchKeyEventTimestamp v
        , "text" .= pInputDispatchKeyEventText v
        , "unmodifiedText" .= pInputDispatchKeyEventUnmodifiedText v
        , "keyIdentifier" .= pInputDispatchKeyEventKeyIdentifier v
        , "code" .= pInputDispatchKeyEventCode v
        , "key" .= pInputDispatchKeyEventKey v
        , "windowsVirtualKeyCode" .= pInputDispatchKeyEventWindowsVirtualKeyCode v
        , "nativeVirtualKeyCode" .= pInputDispatchKeyEventNativeVirtualKeyCode v
        , "autoRepeat" .= pInputDispatchKeyEventAutoRepeat v
        , "isKeypad" .= pInputDispatchKeyEventIsKeypad v
        , "isSystemKey" .= pInputDispatchKeyEventIsSystemKey v
        , "location" .= pInputDispatchKeyEventLocation v
        ]


inputDispatchKeyEvent :: Session -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent session params = sendReceiveCommand session "Input.dispatchKeyEvent" (Just params)



data PInputDispatchMouseEvent = PInputDispatchMouseEvent {
    pInputDispatchMouseEventType :: String,
    pInputDispatchMouseEventX :: Int,
    pInputDispatchMouseEventY :: Int,
    pInputDispatchMouseEventModifiers :: Maybe Int,
    pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
    pInputDispatchMouseEventButton :: Maybe InputMouseButton,
    pInputDispatchMouseEventButtons :: Maybe Int,
    pInputDispatchMouseEventClickCount :: Maybe Int,
    pInputDispatchMouseEventDeltaX :: Maybe Int,
    pInputDispatchMouseEventDeltaY :: Maybe Int,
    pInputDispatchMouseEventPointerType :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchMouseEvent where
    parseJSON = A.withObject "PInputDispatchMouseEvent" $ \v ->
         PInputDispatchMouseEvent <$> v .:  "type"
            <*> v  .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"
            <*> v  .:?  "button"
            <*> v  .:?  "buttons"
            <*> v  .:?  "clickCount"
            <*> v  .:?  "deltaX"
            <*> v  .:?  "deltaY"
            <*> v  .:?  "pointerType"


instance ToJSON PInputDispatchMouseEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchMouseEventType v
        , "x" .= pInputDispatchMouseEventX v
        , "y" .= pInputDispatchMouseEventY v
        , "modifiers" .= pInputDispatchMouseEventModifiers v
        , "timestamp" .= pInputDispatchMouseEventTimestamp v
        , "button" .= pInputDispatchMouseEventButton v
        , "buttons" .= pInputDispatchMouseEventButtons v
        , "clickCount" .= pInputDispatchMouseEventClickCount v
        , "deltaX" .= pInputDispatchMouseEventDeltaX v
        , "deltaY" .= pInputDispatchMouseEventDeltaY v
        , "pointerType" .= pInputDispatchMouseEventPointerType v
        ]


inputDispatchMouseEvent :: Session -> PInputDispatchMouseEvent -> IO (Maybe Error)
inputDispatchMouseEvent session params = sendReceiveCommand session "Input.dispatchMouseEvent" (Just params)



data PInputDispatchTouchEvent = PInputDispatchTouchEvent {
    pInputDispatchTouchEventType :: String,
    pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
    pInputDispatchTouchEventModifiers :: Maybe Int,
    pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchTouchEvent where
    parseJSON = A.withObject "PInputDispatchTouchEvent" $ \v ->
         PInputDispatchTouchEvent <$> v .:  "type"
            <*> v  .:  "touchPoints"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"


instance ToJSON PInputDispatchTouchEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchTouchEventType v
        , "touchPoints" .= pInputDispatchTouchEventTouchPoints v
        , "modifiers" .= pInputDispatchTouchEventModifiers v
        , "timestamp" .= pInputDispatchTouchEventTimestamp v
        ]


inputDispatchTouchEvent :: Session -> PInputDispatchTouchEvent -> IO (Maybe Error)
inputDispatchTouchEvent session params = sendReceiveCommand session "Input.dispatchTouchEvent" (Just params)



data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
    pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PInputSetIgnoreInputEvents where
    parseJSON = A.withObject "PInputSetIgnoreInputEvents" $ \v ->
         PInputSetIgnoreInputEvents <$> v .:  "ignore"


instance ToJSON PInputSetIgnoreInputEvents  where
    toJSON v = A.object
        [ "ignore" .= pInputSetIgnoreInputEventsIgnore v
        ]


inputSetIgnoreInputEvents :: Session -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents session params = sendReceiveCommand session "Input.setIgnoreInputEvents" (Just params)



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


instance FromEvent Event LogEntryAdded where
    eventName  _ _    =  "Log.entryAdded"
    fromEvent ev =  case ev of EVLogEntryAdded v -> Just v; _ -> Nothing


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
logClear session = sendReceiveCommand session "Log.clear" (Nothing :: Maybe ())




logDisable :: Session -> IO (Maybe Error)
logDisable session = sendReceiveCommand session "Log.disable" (Nothing :: Maybe ())




logEnable :: Session -> IO (Maybe Error)
logEnable session = sendReceiveCommand session "Log.enable" (Nothing :: Maybe ())



data PLogStartViolationsReport = PLogStartViolationsReport {
    pLogStartViolationsReportConfig :: [LogViolationSetting]
} deriving (Eq, Show, Read)
instance FromJSON  PLogStartViolationsReport where
    parseJSON = A.withObject "PLogStartViolationsReport" $ \v ->
         PLogStartViolationsReport <$> v .:  "config"


instance ToJSON PLogStartViolationsReport  where
    toJSON v = A.object
        [ "config" .= pLogStartViolationsReportConfig v
        ]


logStartViolationsReport :: Session -> PLogStartViolationsReport -> IO (Maybe Error)
logStartViolationsReport session params = sendReceiveCommand session "Log.startViolationsReport" (Just params)




logStopViolationsReport :: Session -> IO (Maybe Error)
logStopViolationsReport session = sendReceiveCommand session "Log.stopViolationsReport" (Nothing :: Maybe ())



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


instance FromEvent Event NetworkDataReceived where
    eventName  _ _    =  "Network.dataReceived"
    fromEvent ev =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkEventSourceMessageReceived where
    eventName  _ _    =  "Network.eventSourceMessageReceived"
    fromEvent ev =  case ev of EVNetworkEventSourceMessageReceived v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkLoadingFailed where
    eventName  _ _    =  "Network.loadingFailed"
    fromEvent ev =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkLoadingFinished where
    eventName  _ _    =  "Network.loadingFinished"
    fromEvent ev =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkRequestServedFromCache where
    eventName  _ _    =  "Network.requestServedFromCache"
    fromEvent ev =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkRequestWillBeSent where
    eventName  _ _    =  "Network.requestWillBeSent"
    fromEvent ev =  case ev of EVNetworkRequestWillBeSent v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkResponseReceived where
    eventName  _ _    =  "Network.responseReceived"
    fromEvent ev =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketClosed where
    eventName  _ _    =  "Network.webSocketClosed"
    fromEvent ev =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketCreated where
    eventName  _ _    =  "Network.webSocketCreated"
    fromEvent ev =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketFrameError where
    eventName  _ _    =  "Network.webSocketFrameError"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketFrameReceived where
    eventName  _ _    =  "Network.webSocketFrameReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketFrameSent where
    eventName  _ _    =  "Network.webSocketFrameSent"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketHandshakeResponseReceived where
    eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebSocketWillSendHandshakeRequest where
    eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
    fromEvent ev =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebTransportCreated where
    eventName  _ _    =  "Network.webTransportCreated"
    fromEvent ev =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebTransportConnectionEstablished where
    eventName  _ _    =  "Network.webTransportConnectionEstablished"
    fromEvent ev =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing

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


instance FromEvent Event NetworkWebTransportClosed where
    eventName  _ _    =  "Network.webTransportClosed"
    fromEvent ev =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing


data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
    deriving (Eq, Show, Read)
instance FromJSON NetworkResourceType where
    parseJSON = A.withText  "NetworkResourceType"  $ \v -> do
        case v of
                "Document" -> pure $ NetworkResourceTypeDocument
                "Stylesheet" -> pure $ NetworkResourceTypeStylesheet
                "Image" -> pure $ NetworkResourceTypeImage
                "Media" -> pure $ NetworkResourceTypeMedia
                "Font" -> pure $ NetworkResourceTypeFont
                "Script" -> pure $ NetworkResourceTypeScript
                "TextTrack" -> pure $ NetworkResourceTypeTextTrack
                "XHR" -> pure $ NetworkResourceTypeXhr
                "Fetch" -> pure $ NetworkResourceTypeFetch
                "EventSource" -> pure $ NetworkResourceTypeEventSource
                "WebSocket" -> pure $ NetworkResourceTypeWebSocket
                "Manifest" -> pure $ NetworkResourceTypeManifest
                "SignedExchange" -> pure $ NetworkResourceTypeSignedExchange
                "Ping" -> pure $ NetworkResourceTypePing
                "CSPViolationReport" -> pure $ NetworkResourceTypeCspViolationReport
                "Preflight" -> pure $ NetworkResourceTypePreflight
                "Other" -> pure $ NetworkResourceTypeOther
                _ -> fail "failed to parse NetworkResourceType"

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
        case v of
                "Failed" -> pure $ NetworkErrorReasonFailed
                "Aborted" -> pure $ NetworkErrorReasonAborted
                "TimedOut" -> pure $ NetworkErrorReasonTimedOut
                "AccessDenied" -> pure $ NetworkErrorReasonAccessDenied
                "ConnectionClosed" -> pure $ NetworkErrorReasonConnectionClosed
                "ConnectionReset" -> pure $ NetworkErrorReasonConnectionReset
                "ConnectionRefused" -> pure $ NetworkErrorReasonConnectionRefused
                "ConnectionAborted" -> pure $ NetworkErrorReasonConnectionAborted
                "ConnectionFailed" -> pure $ NetworkErrorReasonConnectionFailed
                "NameNotResolved" -> pure $ NetworkErrorReasonNameNotResolved
                "InternetDisconnected" -> pure $ NetworkErrorReasonInternetDisconnected
                "AddressUnreachable" -> pure $ NetworkErrorReasonAddressUnreachable
                "BlockedByClient" -> pure $ NetworkErrorReasonBlockedByClient
                "BlockedByResponse" -> pure $ NetworkErrorReasonBlockedByResponse
                _ -> fail "failed to parse NetworkErrorReason"

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
        case v of
                "none" -> pure $ NetworkConnectionTypeNone
                "cellular2g" -> pure $ NetworkConnectionTypeCellular2g
                "cellular3g" -> pure $ NetworkConnectionTypeCellular3g
                "cellular4g" -> pure $ NetworkConnectionTypeCellular4g
                "bluetooth" -> pure $ NetworkConnectionTypeBluetooth
                "ethernet" -> pure $ NetworkConnectionTypeEthernet
                "wifi" -> pure $ NetworkConnectionTypeWifi
                "wimax" -> pure $ NetworkConnectionTypeWimax
                "other" -> pure $ NetworkConnectionTypeOther
                _ -> fail "failed to parse NetworkConnectionType"

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
        case v of
                "Strict" -> pure $ NetworkCookieSameSiteStrict
                "Lax" -> pure $ NetworkCookieSameSiteLax
                "None" -> pure $ NetworkCookieSameSiteNone
                _ -> fail "failed to parse NetworkCookieSameSite"

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
        case v of
                "VeryLow" -> pure $ NetworkResourcePriorityVeryLow
                "Low" -> pure $ NetworkResourcePriorityLow
                "Medium" -> pure $ NetworkResourcePriorityMedium
                "High" -> pure $ NetworkResourcePriorityHigh
                "VeryHigh" -> pure $ NetworkResourcePriorityVeryHigh
                _ -> fail "failed to parse NetworkResourcePriority"

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
        case v of
                "unknown" -> pure $ NetworkCertificateTransparencyComplianceUnknown
                "not-compliant" -> pure $ NetworkCertificateTransparencyComplianceNotCompliant
                "compliant" -> pure $ NetworkCertificateTransparencyComplianceCompliant
                _ -> fail "failed to parse NetworkCertificateTransparencyCompliance"

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
        case v of
                "other" -> pure $ NetworkBlockedReasonOther
                "csp" -> pure $ NetworkBlockedReasonCsp
                "mixed-content" -> pure $ NetworkBlockedReasonMixedContent
                "origin" -> pure $ NetworkBlockedReasonOrigin
                "inspector" -> pure $ NetworkBlockedReasonInspector
                "subresource-filter" -> pure $ NetworkBlockedReasonSubresourceFilter
                "content-type" -> pure $ NetworkBlockedReasonContentType
                "coep-frame-resource-needs-coep-header" -> pure $ NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader
                "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> pure $ NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
                "corp-not-same-origin" -> pure $ NetworkBlockedReasonCorpNotSameOrigin
                "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> pure $ NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
                "corp-not-same-site" -> pure $ NetworkBlockedReasonCorpNotSameSite
                _ -> fail "failed to parse NetworkBlockedReason"

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
        case v of
                "DisallowedByMode" -> pure $ NetworkCorsErrorDisallowedByMode
                "InvalidResponse" -> pure $ NetworkCorsErrorInvalidResponse
                "WildcardOriginNotAllowed" -> pure $ NetworkCorsErrorWildcardOriginNotAllowed
                "MissingAllowOriginHeader" -> pure $ NetworkCorsErrorMissingAllowOriginHeader
                "MultipleAllowOriginValues" -> pure $ NetworkCorsErrorMultipleAllowOriginValues
                "InvalidAllowOriginValue" -> pure $ NetworkCorsErrorInvalidAllowOriginValue
                "AllowOriginMismatch" -> pure $ NetworkCorsErrorAllowOriginMismatch
                "InvalidAllowCredentials" -> pure $ NetworkCorsErrorInvalidAllowCredentials
                "CorsDisabledScheme" -> pure $ NetworkCorsErrorCorsDisabledScheme
                "PreflightInvalidStatus" -> pure $ NetworkCorsErrorPreflightInvalidStatus
                "PreflightDisallowedRedirect" -> pure $ NetworkCorsErrorPreflightDisallowedRedirect
                "PreflightWildcardOriginNotAllowed" -> pure $ NetworkCorsErrorPreflightWildcardOriginNotAllowed
                "PreflightMissingAllowOriginHeader" -> pure $ NetworkCorsErrorPreflightMissingAllowOriginHeader
                "PreflightMultipleAllowOriginValues" -> pure $ NetworkCorsErrorPreflightMultipleAllowOriginValues
                "PreflightInvalidAllowOriginValue" -> pure $ NetworkCorsErrorPreflightInvalidAllowOriginValue
                "PreflightAllowOriginMismatch" -> pure $ NetworkCorsErrorPreflightAllowOriginMismatch
                "PreflightInvalidAllowCredentials" -> pure $ NetworkCorsErrorPreflightInvalidAllowCredentials
                "PreflightMissingAllowExternal" -> pure $ NetworkCorsErrorPreflightMissingAllowExternal
                "PreflightInvalidAllowExternal" -> pure $ NetworkCorsErrorPreflightInvalidAllowExternal
                "PreflightMissingAllowPrivateNetwork" -> pure $ NetworkCorsErrorPreflightMissingAllowPrivateNetwork
                "PreflightInvalidAllowPrivateNetwork" -> pure $ NetworkCorsErrorPreflightInvalidAllowPrivateNetwork
                "InvalidAllowMethodsPreflightResponse" -> pure $ NetworkCorsErrorInvalidAllowMethodsPreflightResponse
                "InvalidAllowHeadersPreflightResponse" -> pure $ NetworkCorsErrorInvalidAllowHeadersPreflightResponse
                "MethodDisallowedByPreflightResponse" -> pure $ NetworkCorsErrorMethodDisallowedByPreflightResponse
                "HeaderDisallowedByPreflightResponse" -> pure $ NetworkCorsErrorHeaderDisallowedByPreflightResponse
                "RedirectContainsCredentials" -> pure $ NetworkCorsErrorRedirectContainsCredentials
                "InsecurePrivateNetwork" -> pure $ NetworkCorsErrorInsecurePrivateNetwork
                "InvalidPrivateNetworkAccess" -> pure $ NetworkCorsErrorInvalidPrivateNetworkAccess
                "UnexpectedPrivateNetworkAccess" -> pure $ NetworkCorsErrorUnexpectedPrivateNetworkAccess
                "NoCorsRedirectModeNotFollow" -> pure $ NetworkCorsErrorNoCorsRedirectModeNotFollow
                _ -> fail "failed to parse NetworkCorsError"

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
        case v of
                "cache-storage" -> pure $ NetworkServiceWorkerResponseSourceCacheStorage
                "http-cache" -> pure $ NetworkServiceWorkerResponseSourceHttpCache
                "fallback-code" -> pure $ NetworkServiceWorkerResponseSourceFallbackCode
                "network" -> pure $ NetworkServiceWorkerResponseSourceNetwork
                _ -> fail "failed to parse NetworkServiceWorkerResponseSource"

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
networkClearBrowserCache session = sendReceiveCommand session "Network.clearBrowserCache" (Nothing :: Maybe ())




networkClearBrowserCookies :: Session -> IO (Maybe Error)
networkClearBrowserCookies session = sendReceiveCommand session "Network.clearBrowserCookies" (Nothing :: Maybe ())



data PNetworkDeleteCookies = PNetworkDeleteCookies {
    pNetworkDeleteCookiesName :: String,
    pNetworkDeleteCookiesUrl :: Maybe String,
    pNetworkDeleteCookiesDomain :: Maybe String,
    pNetworkDeleteCookiesPath :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkDeleteCookies where
    parseJSON = A.withObject "PNetworkDeleteCookies" $ \v ->
         PNetworkDeleteCookies <$> v .:  "name"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"


instance ToJSON PNetworkDeleteCookies  where
    toJSON v = A.object
        [ "name" .= pNetworkDeleteCookiesName v
        , "url" .= pNetworkDeleteCookiesUrl v
        , "domain" .= pNetworkDeleteCookiesDomain v
        , "path" .= pNetworkDeleteCookiesPath v
        ]


networkDeleteCookies :: Session -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies session params = sendReceiveCommand session "Network.deleteCookies" (Just params)




networkDisable :: Session -> IO (Maybe Error)
networkDisable session = sendReceiveCommand session "Network.disable" (Nothing :: Maybe ())



data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
    pNetworkEmulateNetworkConditionsOffline :: Bool,
    pNetworkEmulateNetworkConditionsLatency :: Int,
    pNetworkEmulateNetworkConditionsDownloadThroughput :: Int,
    pNetworkEmulateNetworkConditionsUploadThroughput :: Int,
    pNetworkEmulateNetworkConditionsConnectionType :: Maybe NetworkConnectionType
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkEmulateNetworkConditions where
    parseJSON = A.withObject "PNetworkEmulateNetworkConditions" $ \v ->
         PNetworkEmulateNetworkConditions <$> v .:  "offline"
            <*> v  .:  "latency"
            <*> v  .:  "downloadThroughput"
            <*> v  .:  "uploadThroughput"
            <*> v  .:?  "connectionType"


instance ToJSON PNetworkEmulateNetworkConditions  where
    toJSON v = A.object
        [ "offline" .= pNetworkEmulateNetworkConditionsOffline v
        , "latency" .= pNetworkEmulateNetworkConditionsLatency v
        , "downloadThroughput" .= pNetworkEmulateNetworkConditionsDownloadThroughput v
        , "uploadThroughput" .= pNetworkEmulateNetworkConditionsUploadThroughput v
        , "connectionType" .= pNetworkEmulateNetworkConditionsConnectionType v
        ]


networkEmulateNetworkConditions :: Session -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions session params = sendReceiveCommand session "Network.emulateNetworkConditions" (Just params)



data PNetworkEnable = PNetworkEnable {
    pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkEnable where
    parseJSON = A.withObject "PNetworkEnable" $ \v ->
         PNetworkEnable <$> v .:?  "maxPostDataSize"


instance ToJSON PNetworkEnable  where
    toJSON v = A.object
        [ "maxPostDataSize" .= pNetworkEnableMaxPostDataSize v
        ]


networkEnable :: Session -> PNetworkEnable -> IO (Maybe Error)
networkEnable session params = sendReceiveCommand session "Network.enable" (Just params)

data NetworkGetAllCookies = NetworkGetAllCookies {
    networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetAllCookies where
    parseJSON = A.withObject "NetworkGetAllCookies" $ \v ->
         NetworkGetAllCookies <$> v .:  "cookies"



instance Command  NetworkGetAllCookies where
    commandName _ = "Network.getAllCookies"


networkGetAllCookies :: Session -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies session = sendReceiveCommandResult session "Network.getAllCookies" (Nothing :: Maybe ())

data NetworkGetCookies = NetworkGetCookies {
    networkGetCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetCookies where
    parseJSON = A.withObject "NetworkGetCookies" $ \v ->
         NetworkGetCookies <$> v .:  "cookies"



instance Command  NetworkGetCookies where
    commandName _ = "Network.getCookies"

data PNetworkGetCookies = PNetworkGetCookies {
    pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetCookies where
    parseJSON = A.withObject "PNetworkGetCookies" $ \v ->
         PNetworkGetCookies <$> v .:?  "urls"


instance ToJSON PNetworkGetCookies  where
    toJSON v = A.object
        [ "urls" .= pNetworkGetCookiesUrls v
        ]


networkGetCookies :: Session -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies session params = sendReceiveCommandResult session "Network.getCookies" (Just params)

data NetworkGetResponseBody = NetworkGetResponseBody {
    networkGetResponseBodyBody :: String,
    networkGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetResponseBody where
    parseJSON = A.withObject "NetworkGetResponseBody" $ \v ->
         NetworkGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



instance Command  NetworkGetResponseBody where
    commandName _ = "Network.getResponseBody"

data PNetworkGetResponseBody = PNetworkGetResponseBody {
    pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetResponseBody where
    parseJSON = A.withObject "PNetworkGetResponseBody" $ \v ->
         PNetworkGetResponseBody <$> v .:  "requestId"


instance ToJSON PNetworkGetResponseBody  where
    toJSON v = A.object
        [ "requestId" .= pNetworkGetResponseBodyRequestId v
        ]


networkGetResponseBody :: Session -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody session params = sendReceiveCommandResult session "Network.getResponseBody" (Just params)

data NetworkGetRequestPostData = NetworkGetRequestPostData {
    networkGetRequestPostDataPostData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetRequestPostData where
    parseJSON = A.withObject "NetworkGetRequestPostData" $ \v ->
         NetworkGetRequestPostData <$> v .:  "postData"



instance Command  NetworkGetRequestPostData where
    commandName _ = "Network.getRequestPostData"

data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
    pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetRequestPostData where
    parseJSON = A.withObject "PNetworkGetRequestPostData" $ \v ->
         PNetworkGetRequestPostData <$> v .:  "requestId"


instance ToJSON PNetworkGetRequestPostData  where
    toJSON v = A.object
        [ "requestId" .= pNetworkGetRequestPostDataRequestId v
        ]


networkGetRequestPostData :: Session -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData session params = sendReceiveCommandResult session "Network.getRequestPostData" (Just params)



data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
    pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCacheDisabled where
    parseJSON = A.withObject "PNetworkSetCacheDisabled" $ \v ->
         PNetworkSetCacheDisabled <$> v .:  "cacheDisabled"


instance ToJSON PNetworkSetCacheDisabled  where
    toJSON v = A.object
        [ "cacheDisabled" .= pNetworkSetCacheDisabledCacheDisabled v
        ]


networkSetCacheDisabled :: Session -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled session params = sendReceiveCommand session "Network.setCacheDisabled" (Just params)



data PNetworkSetCookie = PNetworkSetCookie {
    pNetworkSetCookieName :: String,
    pNetworkSetCookieValue :: String,
    pNetworkSetCookieUrl :: Maybe String,
    pNetworkSetCookieDomain :: Maybe String,
    pNetworkSetCookiePath :: Maybe String,
    pNetworkSetCookieSecure :: Maybe Bool,
    pNetworkSetCookieHttpOnly :: Maybe Bool,
    pNetworkSetCookieSameSite :: Maybe NetworkCookieSameSite,
    pNetworkSetCookieExpires :: Maybe NetworkTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCookie where
    parseJSON = A.withObject "PNetworkSetCookie" $ \v ->
         PNetworkSetCookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON PNetworkSetCookie  where
    toJSON v = A.object
        [ "name" .= pNetworkSetCookieName v
        , "value" .= pNetworkSetCookieValue v
        , "url" .= pNetworkSetCookieUrl v
        , "domain" .= pNetworkSetCookieDomain v
        , "path" .= pNetworkSetCookiePath v
        , "secure" .= pNetworkSetCookieSecure v
        , "httpOnly" .= pNetworkSetCookieHttpOnly v
        , "sameSite" .= pNetworkSetCookieSameSite v
        , "expires" .= pNetworkSetCookieExpires v
        ]


networkSetCookie :: Session -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie session params = sendReceiveCommand session "Network.setCookie" (Just params)



data PNetworkSetCookies = PNetworkSetCookies {
    pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCookies where
    parseJSON = A.withObject "PNetworkSetCookies" $ \v ->
         PNetworkSetCookies <$> v .:  "cookies"


instance ToJSON PNetworkSetCookies  where
    toJSON v = A.object
        [ "cookies" .= pNetworkSetCookiesCookies v
        ]


networkSetCookies :: Session -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies session params = sendReceiveCommand session "Network.setCookies" (Just params)



data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
    pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetExtraHttpHeaders where
    parseJSON = A.withObject "PNetworkSetExtraHttpHeaders" $ \v ->
         PNetworkSetExtraHttpHeaders <$> v .:  "headers"


instance ToJSON PNetworkSetExtraHttpHeaders  where
    toJSON v = A.object
        [ "headers" .= pNetworkSetExtraHttpHeadersHeaders v
        ]


networkSetExtraHttpHeaders :: Session -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders session params = sendReceiveCommand session "Network.setExtraHTTPHeaders" (Just params)



data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
    pNetworkSetUserAgentOverrideUserAgent :: String,
    pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pNetworkSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetUserAgentOverride where
    parseJSON = A.withObject "PNetworkSetUserAgentOverride" $ \v ->
         PNetworkSetUserAgentOverride <$> v .:  "userAgent"
            <*> v  .:?  "acceptLanguage"
            <*> v  .:?  "platform"


instance ToJSON PNetworkSetUserAgentOverride  where
    toJSON v = A.object
        [ "userAgent" .= pNetworkSetUserAgentOverrideUserAgent v
        , "acceptLanguage" .= pNetworkSetUserAgentOverrideAcceptLanguage v
        , "platform" .= pNetworkSetUserAgentOverridePlatform v
        ]


networkSetUserAgentOverride :: Session -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride session params = sendReceiveCommand session "Network.setUserAgentOverride" (Just params)



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


instance FromEvent Event PageDomContentEventFired where
    eventName  _ _    =  "Page.domContentEventFired"
    fromEvent ev =  case ev of EVPageDomContentEventFired v -> Just v; _ -> Nothing

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


instance FromEvent Event PageFileChooserOpened where
    eventName  _ _    =  "Page.fileChooserOpened"
    fromEvent ev =  case ev of EVPageFileChooserOpened v -> Just v; _ -> Nothing

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


instance FromEvent Event PageFrameAttached where
    eventName  _ _    =  "Page.frameAttached"
    fromEvent ev =  case ev of EVPageFrameAttached v -> Just v; _ -> Nothing

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


instance FromEvent Event PageFrameDetached where
    eventName  _ _    =  "Page.frameDetached"
    fromEvent ev =  case ev of EVPageFrameDetached v -> Just v; _ -> Nothing

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


instance FromEvent Event PageFrameNavigated where
    eventName  _ _    =  "Page.frameNavigated"
    fromEvent ev =  case ev of EVPageFrameNavigated v -> Just v; _ -> Nothing

data PageInterstitialHidden = PageInterstitialHidden
    deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
    parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
        case v of
                "PageInterstitialHidden" -> pure $ PageInterstitialHidden
                _ -> fail "failed to parse PageInterstitialHidden"

instance FromEvent Event PageInterstitialHidden where
    eventName  _ _    =  "Page.interstitialHidden"
    fromEvent ev =  case ev of EVPageInterstitialHidden v -> Just v; _ -> Nothing

data PageInterstitialShown = PageInterstitialShown
    deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
    parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
        case v of
                "PageInterstitialShown" -> pure $ PageInterstitialShown
                _ -> fail "failed to parse PageInterstitialShown"

instance FromEvent Event PageInterstitialShown where
    eventName  _ _    =  "Page.interstitialShown"
    fromEvent ev =  case ev of EVPageInterstitialShown v -> Just v; _ -> Nothing

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


instance FromEvent Event PageJavascriptDialogClosed where
    eventName  _ _    =  "Page.javascriptDialogClosed"
    fromEvent ev =  case ev of EVPageJavascriptDialogClosed v -> Just v; _ -> Nothing

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


instance FromEvent Event PageJavascriptDialogOpening where
    eventName  _ _    =  "Page.javascriptDialogOpening"
    fromEvent ev =  case ev of EVPageJavascriptDialogOpening v -> Just v; _ -> Nothing

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


instance FromEvent Event PageLifecycleEvent where
    eventName  _ _    =  "Page.lifecycleEvent"
    fromEvent ev =  case ev of EVPageLifecycleEvent v -> Just v; _ -> Nothing

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


instance FromEvent Event PagePrerenderAttemptCompleted where
    eventName  _ _    =  "Page.prerenderAttemptCompleted"
    fromEvent ev =  case ev of EVPagePrerenderAttemptCompleted v -> Just v; _ -> Nothing

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


instance FromEvent Event PageLoadEventFired where
    eventName  _ _    =  "Page.loadEventFired"
    fromEvent ev =  case ev of EVPageLoadEventFired v -> Just v; _ -> Nothing

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


instance FromEvent Event PageWindowOpen where
    eventName  _ _    =  "Page.windowOpen"
    fromEvent ev =  case ev of EVPageWindowOpen v -> Just v; _ -> Nothing


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
        case v of
                "link" -> pure $ PageTransitionTypeLink
                "typed" -> pure $ PageTransitionTypeTyped
                "address_bar" -> pure $ PageTransitionTypeAddressBar
                "auto_bookmark" -> pure $ PageTransitionTypeAutoBookmark
                "auto_subframe" -> pure $ PageTransitionTypeAutoSubframe
                "manual_subframe" -> pure $ PageTransitionTypeManualSubframe
                "generated" -> pure $ PageTransitionTypeGenerated
                "auto_toplevel" -> pure $ PageTransitionTypeAutoToplevel
                "form_submit" -> pure $ PageTransitionTypeFormSubmit
                "reload" -> pure $ PageTransitionTypeReload
                "keyword" -> pure $ PageTransitionTypeKeyword
                "keyword_generated" -> pure $ PageTransitionTypeKeywordGenerated
                "other" -> pure $ PageTransitionTypeOther
                _ -> fail "failed to parse PageTransitionType"

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
        case v of
                "alert" -> pure $ PageDialogTypeAlert
                "confirm" -> pure $ PageDialogTypeConfirm
                "prompt" -> pure $ PageDialogTypePrompt
                "beforeunload" -> pure $ PageDialogTypeBeforeunload
                _ -> fail "failed to parse PageDialogType"

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
        case v of
                "Activated" -> pure $ PagePrerenderFinalStatusActivated
                "Destroyed" -> pure $ PagePrerenderFinalStatusDestroyed
                "LowEndDevice" -> pure $ PagePrerenderFinalStatusLowEndDevice
                "CrossOriginRedirect" -> pure $ PagePrerenderFinalStatusCrossOriginRedirect
                "CrossOriginNavigation" -> pure $ PagePrerenderFinalStatusCrossOriginNavigation
                "InvalidSchemeRedirect" -> pure $ PagePrerenderFinalStatusInvalidSchemeRedirect
                "InvalidSchemeNavigation" -> pure $ PagePrerenderFinalStatusInvalidSchemeNavigation
                "InProgressNavigation" -> pure $ PagePrerenderFinalStatusInProgressNavigation
                "NavigationRequestBlockedByCsp" -> pure $ PagePrerenderFinalStatusNavigationRequestBlockedByCsp
                "MainFrameNavigation" -> pure $ PagePrerenderFinalStatusMainFrameNavigation
                "MojoBinderPolicy" -> pure $ PagePrerenderFinalStatusMojoBinderPolicy
                "RendererProcessCrashed" -> pure $ PagePrerenderFinalStatusRendererProcessCrashed
                "RendererProcessKilled" -> pure $ PagePrerenderFinalStatusRendererProcessKilled
                "Download" -> pure $ PagePrerenderFinalStatusDownload
                "TriggerDestroyed" -> pure $ PagePrerenderFinalStatusTriggerDestroyed
                "NavigationNotCommitted" -> pure $ PagePrerenderFinalStatusNavigationNotCommitted
                "NavigationBadHttpStatus" -> pure $ PagePrerenderFinalStatusNavigationBadHttpStatus
                "ClientCertRequested" -> pure $ PagePrerenderFinalStatusClientCertRequested
                "NavigationRequestNetworkError" -> pure $ PagePrerenderFinalStatusNavigationRequestNetworkError
                "MaxNumOfRunningPrerendersExceeded" -> pure $ PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded
                "CancelAllHostsForTesting" -> pure $ PagePrerenderFinalStatusCancelAllHostsForTesting
                "DidFailLoad" -> pure $ PagePrerenderFinalStatusDidFailLoad
                "Stop" -> pure $ PagePrerenderFinalStatusStop
                "SslCertificateError" -> pure $ PagePrerenderFinalStatusSslCertificateError
                "LoginAuthRequested" -> pure $ PagePrerenderFinalStatusLoginAuthRequested
                "UaChangeRequiresReload" -> pure $ PagePrerenderFinalStatusUaChangeRequiresReload
                "BlockedByClient" -> pure $ PagePrerenderFinalStatusBlockedByClient
                "AudioOutputDeviceRequested" -> pure $ PagePrerenderFinalStatusAudioOutputDeviceRequested
                "MixedContent" -> pure $ PagePrerenderFinalStatusMixedContent
                "TriggerBackgrounded" -> pure $ PagePrerenderFinalStatusTriggerBackgrounded
                "EmbedderTriggeredAndSameOriginRedirected" -> pure $ PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected
                "EmbedderTriggeredAndCrossOriginRedirected" -> pure $ PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
                "EmbedderTriggeredAndDestroyed" -> pure $ PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
                _ -> fail "failed to parse PagePrerenderFinalStatus"

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



instance Command  PageAddScriptToEvaluateOnNewDocument where
    commandName _ = "Page.addScriptToEvaluateOnNewDocument"

data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
    pPageAddScriptToEvaluateOnNewDocumentSource :: String
} deriving (Eq, Show, Read)
instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "PPageAddScriptToEvaluateOnNewDocument" $ \v ->
         PPageAddScriptToEvaluateOnNewDocument <$> v .:  "source"


instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
    toJSON v = A.object
        [ "source" .= pPageAddScriptToEvaluateOnNewDocumentSource v
        ]


pageAddScriptToEvaluateOnNewDocument :: Session -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument session params = sendReceiveCommandResult session "Page.addScriptToEvaluateOnNewDocument" (Just params)




pageBringToFront :: Session -> IO (Maybe Error)
pageBringToFront session = sendReceiveCommand session "Page.bringToFront" (Nothing :: Maybe ())

data PageCaptureScreenshot = PageCaptureScreenshot {
    pageCaptureScreenshotData :: String
} deriving (Eq, Show, Read)
instance FromJSON  PageCaptureScreenshot where
    parseJSON = A.withObject "PageCaptureScreenshot" $ \v ->
         PageCaptureScreenshot <$> v .:  "data"



instance Command  PageCaptureScreenshot where
    commandName _ = "Page.captureScreenshot"

data PPageCaptureScreenshot = PPageCaptureScreenshot {
    pPageCaptureScreenshotFormat :: Maybe String,
    pPageCaptureScreenshotQuality :: Maybe Int,
    pPageCaptureScreenshotClip :: Maybe PageViewport
} deriving (Eq, Show, Read)
instance FromJSON  PPageCaptureScreenshot where
    parseJSON = A.withObject "PPageCaptureScreenshot" $ \v ->
         PPageCaptureScreenshot <$> v .:?  "format"
            <*> v  .:?  "quality"
            <*> v  .:?  "clip"


instance ToJSON PPageCaptureScreenshot  where
    toJSON v = A.object
        [ "format" .= pPageCaptureScreenshotFormat v
        , "quality" .= pPageCaptureScreenshotQuality v
        , "clip" .= pPageCaptureScreenshotClip v
        ]


pageCaptureScreenshot :: Session -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot session params = sendReceiveCommandResult session "Page.captureScreenshot" (Just params)

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
    pageCreateIsolatedWorldExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PageCreateIsolatedWorld where
    parseJSON = A.withObject "PageCreateIsolatedWorld" $ \v ->
         PageCreateIsolatedWorld <$> v .:  "executionContextId"



instance Command  PageCreateIsolatedWorld where
    commandName _ = "Page.createIsolatedWorld"

data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
    pPageCreateIsolatedWorldFrameId :: PageFrameId,
    pPageCreateIsolatedWorldWorldName :: Maybe String,
    pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PPageCreateIsolatedWorld where
    parseJSON = A.withObject "PPageCreateIsolatedWorld" $ \v ->
         PPageCreateIsolatedWorld <$> v .:  "frameId"
            <*> v  .:?  "worldName"
            <*> v  .:?  "grantUniveralAccess"


instance ToJSON PPageCreateIsolatedWorld  where
    toJSON v = A.object
        [ "frameId" .= pPageCreateIsolatedWorldFrameId v
        , "worldName" .= pPageCreateIsolatedWorldWorldName v
        , "grantUniveralAccess" .= pPageCreateIsolatedWorldGrantUniveralAccess v
        ]


pageCreateIsolatedWorld :: Session -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld session params = sendReceiveCommandResult session "Page.createIsolatedWorld" (Just params)




pageDisable :: Session -> IO (Maybe Error)
pageDisable session = sendReceiveCommand session "Page.disable" (Nothing :: Maybe ())




pageEnable :: Session -> IO (Maybe Error)
pageEnable session = sendReceiveCommand session "Page.enable" (Nothing :: Maybe ())

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



instance Command  PageGetAppManifest where
    commandName _ = "Page.getAppManifest"


pageGetAppManifest :: Session -> IO (Either Error PageGetAppManifest)
pageGetAppManifest session = sendReceiveCommandResult session "Page.getAppManifest" (Nothing :: Maybe ())

data PageGetFrameTree = PageGetFrameTree {
    pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Eq, Show, Read)
instance FromJSON  PageGetFrameTree where
    parseJSON = A.withObject "PageGetFrameTree" $ \v ->
         PageGetFrameTree <$> v .:  "frameTree"



instance Command  PageGetFrameTree where
    commandName _ = "Page.getFrameTree"


pageGetFrameTree :: Session -> IO (Either Error PageGetFrameTree)
pageGetFrameTree session = sendReceiveCommandResult session "Page.getFrameTree" (Nothing :: Maybe ())

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



instance Command  PageGetLayoutMetrics where
    commandName _ = "Page.getLayoutMetrics"


pageGetLayoutMetrics :: Session -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics session = sendReceiveCommandResult session "Page.getLayoutMetrics" (Nothing :: Maybe ())

data PageGetNavigationHistory = PageGetNavigationHistory {
    pageGetNavigationHistoryCurrentIndex :: Int,
    pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Eq, Show, Read)
instance FromJSON  PageGetNavigationHistory where
    parseJSON = A.withObject "PageGetNavigationHistory" $ \v ->
         PageGetNavigationHistory <$> v .:  "currentIndex"
            <*> v  .:  "entries"



instance Command  PageGetNavigationHistory where
    commandName _ = "Page.getNavigationHistory"


pageGetNavigationHistory :: Session -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory session = sendReceiveCommandResult session "Page.getNavigationHistory" (Nothing :: Maybe ())




pageResetNavigationHistory :: Session -> IO (Maybe Error)
pageResetNavigationHistory session = sendReceiveCommand session "Page.resetNavigationHistory" (Nothing :: Maybe ())



data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
    pPageHandleJavaScriptDialogAccept :: Bool,
    pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PPageHandleJavaScriptDialog where
    parseJSON = A.withObject "PPageHandleJavaScriptDialog" $ \v ->
         PPageHandleJavaScriptDialog <$> v .:  "accept"
            <*> v  .:?  "promptText"


instance ToJSON PPageHandleJavaScriptDialog  where
    toJSON v = A.object
        [ "accept" .= pPageHandleJavaScriptDialogAccept v
        , "promptText" .= pPageHandleJavaScriptDialogPromptText v
        ]


pageHandleJavaScriptDialog :: Session -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog session params = sendReceiveCommand session "Page.handleJavaScriptDialog" (Just params)

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



instance Command  PageNavigate where
    commandName _ = "Page.navigate"

data PPageNavigate = PPageNavigate {
    pPageNavigateUrl :: String,
    pPageNavigateReferrer :: Maybe String,
    pPageNavigateTransitionType :: Maybe PageTransitionType,
    pPageNavigateFrameId :: Maybe PageFrameId
} deriving (Eq, Show, Read)
instance FromJSON  PPageNavigate where
    parseJSON = A.withObject "PPageNavigate" $ \v ->
         PPageNavigate <$> v .:  "url"
            <*> v  .:?  "referrer"
            <*> v  .:?  "transitionType"
            <*> v  .:?  "frameId"


instance ToJSON PPageNavigate  where
    toJSON v = A.object
        [ "url" .= pPageNavigateUrl v
        , "referrer" .= pPageNavigateReferrer v
        , "transitionType" .= pPageNavigateTransitionType v
        , "frameId" .= pPageNavigateFrameId v
        ]


pageNavigate :: Session -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate session params = sendReceiveCommandResult session "Page.navigate" (Just params)



data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
    pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PPageNavigateToHistoryEntry where
    parseJSON = A.withObject "PPageNavigateToHistoryEntry" $ \v ->
         PPageNavigateToHistoryEntry <$> v .:  "entryId"


instance ToJSON PPageNavigateToHistoryEntry  where
    toJSON v = A.object
        [ "entryId" .= pPageNavigateToHistoryEntryEntryId v
        ]


pageNavigateToHistoryEntry :: Session -> PPageNavigateToHistoryEntry -> IO (Maybe Error)
pageNavigateToHistoryEntry session params = sendReceiveCommand session "Page.navigateToHistoryEntry" (Just params)

data PagePrintToPdf = PagePrintToPdf {
    pagePrintToPdfData :: String
} deriving (Eq, Show, Read)
instance FromJSON  PagePrintToPdf where
    parseJSON = A.withObject "PagePrintToPdf" $ \v ->
         PagePrintToPdf <$> v .:  "data"



instance Command  PagePrintToPdf where
    commandName _ = "Page.printToPDF"

data PPagePrintToPdf = PPagePrintToPdf {
    pPagePrintToPdfLandscape :: Maybe Bool,
    pPagePrintToPdfDisplayHeaderFooter :: Maybe Bool,
    pPagePrintToPdfPrintBackground :: Maybe Bool,
    pPagePrintToPdfScale :: Maybe Int,
    pPagePrintToPdfPaperWidth :: Maybe Int,
    pPagePrintToPdfPaperHeight :: Maybe Int,
    pPagePrintToPdfMarginTop :: Maybe Int,
    pPagePrintToPdfMarginBottom :: Maybe Int,
    pPagePrintToPdfMarginLeft :: Maybe Int,
    pPagePrintToPdfMarginRight :: Maybe Int,
    pPagePrintToPdfPageRanges :: Maybe String,
    pPagePrintToPdfHeaderTemplate :: Maybe String,
    pPagePrintToPdfFooterTemplate :: Maybe String,
    pPagePrintToPdfPreferCssPageSize :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PPagePrintToPdf where
    parseJSON = A.withObject "PPagePrintToPdf" $ \v ->
         PPagePrintToPdf <$> v .:?  "landscape"
            <*> v  .:?  "displayHeaderFooter"
            <*> v  .:?  "printBackground"
            <*> v  .:?  "scale"
            <*> v  .:?  "paperWidth"
            <*> v  .:?  "paperHeight"
            <*> v  .:?  "marginTop"
            <*> v  .:?  "marginBottom"
            <*> v  .:?  "marginLeft"
            <*> v  .:?  "marginRight"
            <*> v  .:?  "pageRanges"
            <*> v  .:?  "headerTemplate"
            <*> v  .:?  "footerTemplate"
            <*> v  .:?  "preferCSSPageSize"


instance ToJSON PPagePrintToPdf  where
    toJSON v = A.object
        [ "landscape" .= pPagePrintToPdfLandscape v
        , "displayHeaderFooter" .= pPagePrintToPdfDisplayHeaderFooter v
        , "printBackground" .= pPagePrintToPdfPrintBackground v
        , "scale" .= pPagePrintToPdfScale v
        , "paperWidth" .= pPagePrintToPdfPaperWidth v
        , "paperHeight" .= pPagePrintToPdfPaperHeight v
        , "marginTop" .= pPagePrintToPdfMarginTop v
        , "marginBottom" .= pPagePrintToPdfMarginBottom v
        , "marginLeft" .= pPagePrintToPdfMarginLeft v
        , "marginRight" .= pPagePrintToPdfMarginRight v
        , "pageRanges" .= pPagePrintToPdfPageRanges v
        , "headerTemplate" .= pPagePrintToPdfHeaderTemplate v
        , "footerTemplate" .= pPagePrintToPdfFooterTemplate v
        , "preferCSSPageSize" .= pPagePrintToPdfPreferCssPageSize v
        ]


pagePrintToPdf :: Session -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf session params = sendReceiveCommandResult session "Page.printToPDF" (Just params)



data PPageReload = PPageReload {
    pPageReloadIgnoreCache :: Maybe Bool,
    pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PPageReload where
    parseJSON = A.withObject "PPageReload" $ \v ->
         PPageReload <$> v .:?  "ignoreCache"
            <*> v  .:?  "scriptToEvaluateOnLoad"


instance ToJSON PPageReload  where
    toJSON v = A.object
        [ "ignoreCache" .= pPageReloadIgnoreCache v
        , "scriptToEvaluateOnLoad" .= pPageReloadScriptToEvaluateOnLoad v
        ]


pageReload :: Session -> PPageReload -> IO (Maybe Error)
pageReload session params = sendReceiveCommand session "Page.reload" (Just params)



data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
    pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Eq, Show, Read)
instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "PPageRemoveScriptToEvaluateOnNewDocument" $ \v ->
         PPageRemoveScriptToEvaluateOnNewDocument <$> v .:  "identifier"


instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
    toJSON v = A.object
        [ "identifier" .= pPageRemoveScriptToEvaluateOnNewDocumentIdentifier v
        ]


pageRemoveScriptToEvaluateOnNewDocument :: Session -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument session params = sendReceiveCommand session "Page.removeScriptToEvaluateOnNewDocument" (Just params)



data PPageSetDocumentContent = PPageSetDocumentContent {
    pPageSetDocumentContentFrameId :: PageFrameId,
    pPageSetDocumentContentHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  PPageSetDocumentContent where
    parseJSON = A.withObject "PPageSetDocumentContent" $ \v ->
         PPageSetDocumentContent <$> v .:  "frameId"
            <*> v  .:  "html"


instance ToJSON PPageSetDocumentContent  where
    toJSON v = A.object
        [ "frameId" .= pPageSetDocumentContentFrameId v
        , "html" .= pPageSetDocumentContentHtml v
        ]


pageSetDocumentContent :: Session -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent session params = sendReceiveCommand session "Page.setDocumentContent" (Just params)




pageStopLoading :: Session -> IO (Maybe Error)
pageStopLoading session = sendReceiveCommand session "Page.stopLoading" (Nothing :: Maybe ())



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


instance FromEvent Event PerformanceMetrics where
    eventName  _ _    =  "Performance.metrics"
    fromEvent ev =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing


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
performanceDisable session = sendReceiveCommand session "Performance.disable" (Nothing :: Maybe ())



data PPerformanceEnable = PPerformanceEnable {
    pPerformanceEnableTimeDomain :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PPerformanceEnable where
    parseJSON = A.withObject "PPerformanceEnable" $ \v ->
         PPerformanceEnable <$> v .:?  "timeDomain"


instance ToJSON PPerformanceEnable  where
    toJSON v = A.object
        [ "timeDomain" .= pPerformanceEnableTimeDomain v
        ]


performanceEnable :: Session -> PPerformanceEnable -> IO (Maybe Error)
performanceEnable session params = sendReceiveCommand session "Performance.enable" (Just params)

data PerformanceGetMetrics = PerformanceGetMetrics {
    performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceGetMetrics where
    parseJSON = A.withObject "PerformanceGetMetrics" $ \v ->
         PerformanceGetMetrics <$> v .:  "metrics"



instance Command  PerformanceGetMetrics where
    commandName _ = "Performance.getMetrics"


performanceGetMetrics :: Session -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics session = sendReceiveCommandResult session "Performance.getMetrics" (Nothing :: Maybe ())




type SecurityCertificateId = Int

data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
    deriving (Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
    parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> do
        case v of
                "blockable" -> pure $ SecurityMixedContentTypeBlockable
                "optionally-blockable" -> pure $ SecurityMixedContentTypeOptionallyBlockable
                "none" -> pure $ SecurityMixedContentTypeNone
                _ -> fail "failed to parse SecurityMixedContentType"

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
        case v of
                "unknown" -> pure $ SecuritySecurityStateUnknown
                "neutral" -> pure $ SecuritySecurityStateNeutral
                "insecure" -> pure $ SecuritySecurityStateInsecure
                "secure" -> pure $ SecuritySecurityStateSecure
                "info" -> pure $ SecuritySecurityStateInfo
                "insecure-broken" -> pure $ SecuritySecurityStateInsecureBroken
                _ -> fail "failed to parse SecuritySecurityState"

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
        case v of
                "continue" -> pure $ SecurityCertificateErrorActionContinue
                "cancel" -> pure $ SecurityCertificateErrorActionCancel
                _ -> fail "failed to parse SecurityCertificateErrorAction"

instance ToJSON SecurityCertificateErrorAction where
    toJSON v = A.String $
        case v of
                SecurityCertificateErrorActionContinue -> "continue"
                SecurityCertificateErrorActionCancel -> "cancel"





securityDisable :: Session -> IO (Maybe Error)
securityDisable session = sendReceiveCommand session "Security.disable" (Nothing :: Maybe ())




securityEnable :: Session -> IO (Maybe Error)
securityEnable session = sendReceiveCommand session "Security.enable" (Nothing :: Maybe ())



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


instance FromEvent Event TargetReceivedMessageFromTarget where
    eventName  _ _    =  "Target.receivedMessageFromTarget"
    fromEvent ev =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing

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


instance FromEvent Event TargetTargetCreated where
    eventName  _ _    =  "Target.targetCreated"
    fromEvent ev =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing

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


instance FromEvent Event TargetTargetDestroyed where
    eventName  _ _    =  "Target.targetDestroyed"
    fromEvent ev =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing

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


instance FromEvent Event TargetTargetCrashed where
    eventName  _ _    =  "Target.targetCrashed"
    fromEvent ev =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing

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


instance FromEvent Event TargetTargetInfoChanged where
    eventName  _ _    =  "Target.targetInfoChanged"
    fromEvent ev =  case ev of EVTargetTargetInfoChanged v -> Just v; _ -> Nothing


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




data PTargetActivateTarget = PTargetActivateTarget {
    pTargetActivateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetActivateTarget where
    parseJSON = A.withObject "PTargetActivateTarget" $ \v ->
         PTargetActivateTarget <$> v .:  "targetId"


instance ToJSON PTargetActivateTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetActivateTargetTargetId v
        ]


targetActivateTarget :: Session -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget session params = sendReceiveCommand session "Target.activateTarget" (Just params)

data TargetAttachToTarget = TargetAttachToTarget {
    targetAttachToTargetSessionId :: TargetSessionID
} deriving (Eq, Show, Read)
instance FromJSON  TargetAttachToTarget where
    parseJSON = A.withObject "TargetAttachToTarget" $ \v ->
         TargetAttachToTarget <$> v .:  "sessionId"



instance Command  TargetAttachToTarget where
    commandName _ = "Target.attachToTarget"

data PTargetAttachToTarget = PTargetAttachToTarget {
    pTargetAttachToTargetTargetId :: TargetTargetID,
    pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetAttachToTarget where
    parseJSON = A.withObject "PTargetAttachToTarget" $ \v ->
         PTargetAttachToTarget <$> v .:  "targetId"
            <*> v  .:?  "flatten"


instance ToJSON PTargetAttachToTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetAttachToTargetTargetId v
        , "flatten" .= pTargetAttachToTargetFlatten v
        ]


targetAttachToTarget :: Session -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget session params = sendReceiveCommandResult session "Target.attachToTarget" (Just params)



data PTargetCloseTarget = PTargetCloseTarget {
    pTargetCloseTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetCloseTarget where
    parseJSON = A.withObject "PTargetCloseTarget" $ \v ->
         PTargetCloseTarget <$> v .:  "targetId"


instance ToJSON PTargetCloseTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetCloseTargetTargetId v
        ]


targetCloseTarget :: Session -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget session params = sendReceiveCommand session "Target.closeTarget" (Just params)

data TargetCreateTarget = TargetCreateTarget {
    targetCreateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetCreateTarget where
    parseJSON = A.withObject "TargetCreateTarget" $ \v ->
         TargetCreateTarget <$> v .:  "targetId"



instance Command  TargetCreateTarget where
    commandName _ = "Target.createTarget"

data PTargetCreateTarget = PTargetCreateTarget {
    pTargetCreateTargetUrl :: String,
    pTargetCreateTargetWidth :: Maybe Int,
    pTargetCreateTargetHeight :: Maybe Int,
    pTargetCreateTargetNewWindow :: Maybe Bool,
    pTargetCreateTargetBackground :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetCreateTarget where
    parseJSON = A.withObject "PTargetCreateTarget" $ \v ->
         PTargetCreateTarget <$> v .:  "url"
            <*> v  .:?  "width"
            <*> v  .:?  "height"
            <*> v  .:?  "newWindow"
            <*> v  .:?  "background"


instance ToJSON PTargetCreateTarget  where
    toJSON v = A.object
        [ "url" .= pTargetCreateTargetUrl v
        , "width" .= pTargetCreateTargetWidth v
        , "height" .= pTargetCreateTargetHeight v
        , "newWindow" .= pTargetCreateTargetNewWindow v
        , "background" .= pTargetCreateTargetBackground v
        ]


targetCreateTarget :: Session -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget session params = sendReceiveCommandResult session "Target.createTarget" (Just params)



data PTargetDetachFromTarget = PTargetDetachFromTarget {
    pTargetDetachFromTargetSessionId :: Maybe TargetSessionID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetDetachFromTarget where
    parseJSON = A.withObject "PTargetDetachFromTarget" $ \v ->
         PTargetDetachFromTarget <$> v .:?  "sessionId"


instance ToJSON PTargetDetachFromTarget  where
    toJSON v = A.object
        [ "sessionId" .= pTargetDetachFromTargetSessionId v
        ]


targetDetachFromTarget :: Session -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget session params = sendReceiveCommand session "Target.detachFromTarget" (Just params)

data TargetGetTargets = TargetGetTargets {
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Eq, Show, Read)
instance FromJSON  TargetGetTargets where
    parseJSON = A.withObject "TargetGetTargets" $ \v ->
         TargetGetTargets <$> v .:  "targetInfos"



instance Command  TargetGetTargets where
    commandName _ = "Target.getTargets"


targetGetTargets :: Session -> IO (Either Error TargetGetTargets)
targetGetTargets session = sendReceiveCommandResult session "Target.getTargets" (Nothing :: Maybe ())



data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
    pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetSetDiscoverTargets where
    parseJSON = A.withObject "PTargetSetDiscoverTargets" $ \v ->
         PTargetSetDiscoverTargets <$> v .:  "discover"


instance ToJSON PTargetSetDiscoverTargets  where
    toJSON v = A.object
        [ "discover" .= pTargetSetDiscoverTargetsDiscover v
        ]


targetSetDiscoverTargets :: Session -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets session params = sendReceiveCommand session "Target.setDiscoverTargets" (Just params)



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


instance FromEvent Event FetchRequestPaused where
    eventName  _ _    =  "Fetch.requestPaused"
    fromEvent ev =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing

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


instance FromEvent Event FetchAuthRequired where
    eventName  _ _    =  "Fetch.authRequired"
    fromEvent ev =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing


type FetchRequestId = String

data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
    deriving (Eq, Show, Read)
instance FromJSON FetchRequestStage where
    parseJSON = A.withText  "FetchRequestStage"  $ \v -> do
        case v of
                "Request" -> pure $ FetchRequestStageRequest
                "Response" -> pure $ FetchRequestStageResponse
                _ -> fail "failed to parse FetchRequestStage"

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
fetchDisable session = sendReceiveCommand session "Fetch.disable" (Nothing :: Maybe ())



data PFetchEnable = PFetchEnable {
    pFetchEnablePatterns :: Maybe [FetchRequestPattern],
    pFetchEnableHandleAuthRequests :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PFetchEnable where
    parseJSON = A.withObject "PFetchEnable" $ \v ->
         PFetchEnable <$> v .:?  "patterns"
            <*> v  .:?  "handleAuthRequests"


instance ToJSON PFetchEnable  where
    toJSON v = A.object
        [ "patterns" .= pFetchEnablePatterns v
        , "handleAuthRequests" .= pFetchEnableHandleAuthRequests v
        ]


fetchEnable :: Session -> PFetchEnable -> IO (Maybe Error)
fetchEnable session params = sendReceiveCommand session "Fetch.enable" (Just params)



data PFetchFailRequest = PFetchFailRequest {
    pFetchFailRequestRequestId :: FetchRequestId,
    pFetchFailRequestErrorReason :: NetworkErrorReason
} deriving (Eq, Show, Read)
instance FromJSON  PFetchFailRequest where
    parseJSON = A.withObject "PFetchFailRequest" $ \v ->
         PFetchFailRequest <$> v .:  "requestId"
            <*> v  .:  "errorReason"


instance ToJSON PFetchFailRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchFailRequestRequestId v
        , "errorReason" .= pFetchFailRequestErrorReason v
        ]


fetchFailRequest :: Session -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest session params = sendReceiveCommand session "Fetch.failRequest" (Just params)



data PFetchFulfillRequest = PFetchFulfillRequest {
    pFetchFulfillRequestRequestId :: FetchRequestId,
    pFetchFulfillRequestResponseCode :: Int,
    pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
    pFetchFulfillRequestBinaryResponseHeaders :: Maybe String,
    pFetchFulfillRequestBody :: Maybe String,
    pFetchFulfillRequestResponsePhrase :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PFetchFulfillRequest where
    parseJSON = A.withObject "PFetchFulfillRequest" $ \v ->
         PFetchFulfillRequest <$> v .:  "requestId"
            <*> v  .:  "responseCode"
            <*> v  .:?  "responseHeaders"
            <*> v  .:?  "binaryResponseHeaders"
            <*> v  .:?  "body"
            <*> v  .:?  "responsePhrase"


instance ToJSON PFetchFulfillRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchFulfillRequestRequestId v
        , "responseCode" .= pFetchFulfillRequestResponseCode v
        , "responseHeaders" .= pFetchFulfillRequestResponseHeaders v
        , "binaryResponseHeaders" .= pFetchFulfillRequestBinaryResponseHeaders v
        , "body" .= pFetchFulfillRequestBody v
        , "responsePhrase" .= pFetchFulfillRequestResponsePhrase v
        ]


fetchFulfillRequest :: Session -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest session params = sendReceiveCommand session "Fetch.fulfillRequest" (Just params)



data PFetchContinueRequest = PFetchContinueRequest {
    pFetchContinueRequestRequestId :: FetchRequestId,
    pFetchContinueRequestUrl :: Maybe String,
    pFetchContinueRequestMethod :: Maybe String,
    pFetchContinueRequestPostData :: Maybe String,
    pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry]
} deriving (Eq, Show, Read)
instance FromJSON  PFetchContinueRequest where
    parseJSON = A.withObject "PFetchContinueRequest" $ \v ->
         PFetchContinueRequest <$> v .:  "requestId"
            <*> v  .:?  "url"
            <*> v  .:?  "method"
            <*> v  .:?  "postData"
            <*> v  .:?  "headers"


instance ToJSON PFetchContinueRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchContinueRequestRequestId v
        , "url" .= pFetchContinueRequestUrl v
        , "method" .= pFetchContinueRequestMethod v
        , "postData" .= pFetchContinueRequestPostData v
        , "headers" .= pFetchContinueRequestHeaders v
        ]


fetchContinueRequest :: Session -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest session params = sendReceiveCommand session "Fetch.continueRequest" (Just params)



data PFetchContinueWithAuth = PFetchContinueWithAuth {
    pFetchContinueWithAuthRequestId :: FetchRequestId,
    pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
} deriving (Eq, Show, Read)
instance FromJSON  PFetchContinueWithAuth where
    parseJSON = A.withObject "PFetchContinueWithAuth" $ \v ->
         PFetchContinueWithAuth <$> v .:  "requestId"
            <*> v  .:  "authChallengeResponse"


instance ToJSON PFetchContinueWithAuth  where
    toJSON v = A.object
        [ "requestId" .= pFetchContinueWithAuthRequestId v
        , "authChallengeResponse" .= pFetchContinueWithAuthAuthChallengeResponse v
        ]


fetchContinueWithAuth :: Session -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth session params = sendReceiveCommand session "Fetch.continueWithAuth" (Just params)

data FetchGetResponseBody = FetchGetResponseBody {
    fetchGetResponseBodyBody :: String,
    fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  FetchGetResponseBody where
    parseJSON = A.withObject "FetchGetResponseBody" $ \v ->
         FetchGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



instance Command  FetchGetResponseBody where
    commandName _ = "Fetch.getResponseBody"

data PFetchGetResponseBody = PFetchGetResponseBody {
    pFetchGetResponseBodyRequestId :: FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PFetchGetResponseBody where
    parseJSON = A.withObject "PFetchGetResponseBody" $ \v ->
         PFetchGetResponseBody <$> v .:  "requestId"


instance ToJSON PFetchGetResponseBody  where
    toJSON v = A.object
        [ "requestId" .= pFetchGetResponseBodyRequestId v
        ]


fetchGetResponseBody :: Session -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody session params = sendReceiveCommandResult session "Fetch.getResponseBody" (Just params)

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
    fetchTakeResponseBodyAsStreamStream :: IOStreamHandle
} deriving (Eq, Show, Read)
instance FromJSON  FetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "FetchTakeResponseBodyAsStream" $ \v ->
         FetchTakeResponseBodyAsStream <$> v .:  "stream"



instance Command  FetchTakeResponseBodyAsStream where
    commandName _ = "Fetch.takeResponseBodyAsStream"

data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
    pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PFetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "PFetchTakeResponseBodyAsStream" $ \v ->
         PFetchTakeResponseBodyAsStream <$> v .:  "requestId"


instance ToJSON PFetchTakeResponseBodyAsStream  where
    toJSON v = A.object
        [ "requestId" .= pFetchTakeResponseBodyAsStreamRequestId v
        ]


fetchTakeResponseBodyAsStream :: Session -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream session params = sendReceiveCommandResult session "Fetch.takeResponseBodyAsStream" (Just params)



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


instance FromEvent Event ConsoleMessageAdded where
    eventName  _ _    =  "Console.messageAdded"
    fromEvent ev =  case ev of EVConsoleMessageAdded v -> Just v; _ -> Nothing


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
consoleClearMessages session = sendReceiveCommand session "Console.clearMessages" (Nothing :: Maybe ())




consoleDisable :: Session -> IO (Maybe Error)
consoleDisable session = sendReceiveCommand session "Console.disable" (Nothing :: Maybe ())




consoleEnable :: Session -> IO (Maybe Error)
consoleEnable session = sendReceiveCommand session "Console.enable" (Nothing :: Maybe ())



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


instance FromEvent Event DebuggerBreakpointResolved where
    eventName  _ _    =  "Debugger.breakpointResolved"
    fromEvent ev =  case ev of EVDebuggerBreakpointResolved v -> Just v; _ -> Nothing

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


instance FromEvent Event DebuggerPaused where
    eventName  _ _    =  "Debugger.paused"
    fromEvent ev =  case ev of EVDebuggerPaused v -> Just v; _ -> Nothing

data DebuggerResumed = DebuggerResumed
    deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
    parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
        case v of
                "DebuggerResumed" -> pure $ DebuggerResumed
                _ -> fail "failed to parse DebuggerResumed"

instance FromEvent Event DebuggerResumed where
    eventName  _ _    =  "Debugger.resumed"
    fromEvent ev =  case ev of EVDebuggerResumed v -> Just v; _ -> Nothing

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


instance FromEvent Event DebuggerScriptFailedToParse where
    eventName  _ _    =  "Debugger.scriptFailedToParse"
    fromEvent ev =  case ev of EVDebuggerScriptFailedToParse v -> Just v; _ -> Nothing

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


instance FromEvent Event DebuggerScriptParsed where
    eventName  _ _    =  "Debugger.scriptParsed"
    fromEvent ev =  case ev of EVDebuggerScriptParsed v -> Just v; _ -> Nothing


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
        case v of
                "JavaScript" -> pure $ DebuggerScriptLanguageJavaScript
                "WebAssembly" -> pure $ DebuggerScriptLanguageWebAssembly
                _ -> fail "failed to parse DebuggerScriptLanguage"

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




data PDebuggerContinueToLocation = PDebuggerContinueToLocation {
    pDebuggerContinueToLocationLocation :: DebuggerLocation,
    pDebuggerContinueToLocationTargetCallFrames :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerContinueToLocation where
    parseJSON = A.withObject "PDebuggerContinueToLocation" $ \v ->
         PDebuggerContinueToLocation <$> v .:  "location"
            <*> v  .:?  "targetCallFrames"


instance ToJSON PDebuggerContinueToLocation  where
    toJSON v = A.object
        [ "location" .= pDebuggerContinueToLocationLocation v
        , "targetCallFrames" .= pDebuggerContinueToLocationTargetCallFrames v
        ]


debuggerContinueToLocation :: Session -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation session params = sendReceiveCommand session "Debugger.continueToLocation" (Just params)




debuggerDisable :: Session -> IO (Maybe Error)
debuggerDisable session = sendReceiveCommand session "Debugger.disable" (Nothing :: Maybe ())




debuggerEnable :: Session -> IO (Maybe Error)
debuggerEnable session = sendReceiveCommand session "Debugger.enable" (Nothing :: Maybe ())

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
    debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "DebuggerEvaluateOnCallFrame" $ \v ->
         DebuggerEvaluateOnCallFrame <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  DebuggerEvaluateOnCallFrame where
    commandName _ = "Debugger.evaluateOnCallFrame"

data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
    pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
    pDebuggerEvaluateOnCallFrameExpression :: String,
    pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
    pDebuggerEvaluateOnCallFrameIncludeCommandLineApi :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "PDebuggerEvaluateOnCallFrame" $ \v ->
         PDebuggerEvaluateOnCallFrame <$> v .:  "callFrameId"
            <*> v  .:  "expression"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "silent"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "throwOnSideEffect"


instance ToJSON PDebuggerEvaluateOnCallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= pDebuggerEvaluateOnCallFrameCallFrameId v
        , "expression" .= pDebuggerEvaluateOnCallFrameExpression v
        , "objectGroup" .= pDebuggerEvaluateOnCallFrameObjectGroup v
        , "includeCommandLineAPI" .= pDebuggerEvaluateOnCallFrameIncludeCommandLineApi v
        , "silent" .= pDebuggerEvaluateOnCallFrameSilent v
        , "returnByValue" .= pDebuggerEvaluateOnCallFrameReturnByValue v
        , "throwOnSideEffect" .= pDebuggerEvaluateOnCallFrameThrowOnSideEffect v
        ]


debuggerEvaluateOnCallFrame :: Session -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame session params = sendReceiveCommandResult session "Debugger.evaluateOnCallFrame" (Just params)

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "DebuggerGetPossibleBreakpoints" $ \v ->
         DebuggerGetPossibleBreakpoints <$> v .:  "locations"



instance Command  DebuggerGetPossibleBreakpoints where
    commandName _ = "Debugger.getPossibleBreakpoints"

data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
    pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
    pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
    pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "PDebuggerGetPossibleBreakpoints" $ \v ->
         PDebuggerGetPossibleBreakpoints <$> v .:  "start"
            <*> v  .:?  "end"
            <*> v  .:?  "restrictToFunction"


instance ToJSON PDebuggerGetPossibleBreakpoints  where
    toJSON v = A.object
        [ "start" .= pDebuggerGetPossibleBreakpointsStart v
        , "end" .= pDebuggerGetPossibleBreakpointsEnd v
        , "restrictToFunction" .= pDebuggerGetPossibleBreakpointsRestrictToFunction v
        ]


debuggerGetPossibleBreakpoints :: Session -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints session params = sendReceiveCommandResult session "Debugger.getPossibleBreakpoints" (Just params)

data DebuggerGetScriptSource = DebuggerGetScriptSource {
    debuggerGetScriptSourceScriptSource :: String,
    debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetScriptSource where
    parseJSON = A.withObject "DebuggerGetScriptSource" $ \v ->
         DebuggerGetScriptSource <$> v .:  "scriptSource"
            <*> v  .:?  "bytecode"



instance Command  DebuggerGetScriptSource where
    commandName _ = "Debugger.getScriptSource"

data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
    pDebuggerGetScriptSourceScriptId :: RuntimeScriptId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerGetScriptSource where
    parseJSON = A.withObject "PDebuggerGetScriptSource" $ \v ->
         PDebuggerGetScriptSource <$> v .:  "scriptId"


instance ToJSON PDebuggerGetScriptSource  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerGetScriptSourceScriptId v
        ]


debuggerGetScriptSource :: Session -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource session params = sendReceiveCommandResult session "Debugger.getScriptSource" (Just params)




debuggerPause :: Session -> IO (Maybe Error)
debuggerPause session = sendReceiveCommand session "Debugger.pause" (Nothing :: Maybe ())



data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
    pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerRemoveBreakpoint where
    parseJSON = A.withObject "PDebuggerRemoveBreakpoint" $ \v ->
         PDebuggerRemoveBreakpoint <$> v .:  "breakpointId"


instance ToJSON PDebuggerRemoveBreakpoint  where
    toJSON v = A.object
        [ "breakpointId" .= pDebuggerRemoveBreakpointBreakpointId v
        ]


debuggerRemoveBreakpoint :: Session -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint session params = sendReceiveCommand session "Debugger.removeBreakpoint" (Just params)



data PDebuggerResume = PDebuggerResume {
    pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerResume where
    parseJSON = A.withObject "PDebuggerResume" $ \v ->
         PDebuggerResume <$> v .:?  "terminateOnResume"


instance ToJSON PDebuggerResume  where
    toJSON v = A.object
        [ "terminateOnResume" .= pDebuggerResumeTerminateOnResume v
        ]


debuggerResume :: Session -> PDebuggerResume -> IO (Maybe Error)
debuggerResume session params = sendReceiveCommand session "Debugger.resume" (Just params)

data DebuggerSearchInContent = DebuggerSearchInContent {
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSearchInContent where
    parseJSON = A.withObject "DebuggerSearchInContent" $ \v ->
         DebuggerSearchInContent <$> v .:  "result"



instance Command  DebuggerSearchInContent where
    commandName _ = "Debugger.searchInContent"

data PDebuggerSearchInContent = PDebuggerSearchInContent {
    pDebuggerSearchInContentScriptId :: RuntimeScriptId,
    pDebuggerSearchInContentQuery :: String,
    pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
    pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSearchInContent where
    parseJSON = A.withObject "PDebuggerSearchInContent" $ \v ->
         PDebuggerSearchInContent <$> v .:  "scriptId"
            <*> v  .:  "query"
            <*> v  .:?  "caseSensitive"
            <*> v  .:?  "isRegex"


instance ToJSON PDebuggerSearchInContent  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerSearchInContentScriptId v
        , "query" .= pDebuggerSearchInContentQuery v
        , "caseSensitive" .= pDebuggerSearchInContentCaseSensitive v
        , "isRegex" .= pDebuggerSearchInContentIsRegex v
        ]


debuggerSearchInContent :: Session -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent session params = sendReceiveCommandResult session "Debugger.searchInContent" (Just params)



data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
    pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetAsyncCallStackDepth where
    parseJSON = A.withObject "PDebuggerSetAsyncCallStackDepth" $ \v ->
         PDebuggerSetAsyncCallStackDepth <$> v .:  "maxDepth"


instance ToJSON PDebuggerSetAsyncCallStackDepth  where
    toJSON v = A.object
        [ "maxDepth" .= pDebuggerSetAsyncCallStackDepthMaxDepth v
        ]


debuggerSetAsyncCallStackDepth :: Session -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth session params = sendReceiveCommand session "Debugger.setAsyncCallStackDepth" (Just params)

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpoint where
    parseJSON = A.withObject "DebuggerSetBreakpoint" $ \v ->
         DebuggerSetBreakpoint <$> v .:  "breakpointId"
            <*> v  .:  "actualLocation"



instance Command  DebuggerSetBreakpoint where
    commandName _ = "Debugger.setBreakpoint"

data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
    pDebuggerSetBreakpointLocation :: DebuggerLocation,
    pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpoint where
    parseJSON = A.withObject "PDebuggerSetBreakpoint" $ \v ->
         PDebuggerSetBreakpoint <$> v .:  "location"
            <*> v  .:?  "condition"


instance ToJSON PDebuggerSetBreakpoint  where
    toJSON v = A.object
        [ "location" .= pDebuggerSetBreakpointLocation v
        , "condition" .= pDebuggerSetBreakpointCondition v
        ]


debuggerSetBreakpoint :: Session -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint session params = sendReceiveCommandResult session "Debugger.setBreakpoint" (Just params)

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "DebuggerSetInstrumentationBreakpoint" $ \v ->
         DebuggerSetInstrumentationBreakpoint <$> v .:  "breakpointId"



instance Command  DebuggerSetInstrumentationBreakpoint where
    commandName _ = "Debugger.setInstrumentationBreakpoint"

data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint {
    pDebuggerSetInstrumentationBreakpointInstrumentation :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "PDebuggerSetInstrumentationBreakpoint" $ \v ->
         PDebuggerSetInstrumentationBreakpoint <$> v .:  "instrumentation"


instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
    toJSON v = A.object
        [ "instrumentation" .= pDebuggerSetInstrumentationBreakpointInstrumentation v
        ]


debuggerSetInstrumentationBreakpoint :: Session -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint session params = sendReceiveCommandResult session "Debugger.setInstrumentationBreakpoint" (Just params)

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "DebuggerSetBreakpointByUrl" $ \v ->
         DebuggerSetBreakpointByUrl <$> v .:  "breakpointId"
            <*> v  .:  "locations"



instance Command  DebuggerSetBreakpointByUrl where
    commandName _ = "Debugger.setBreakpointByUrl"

data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
    pDebuggerSetBreakpointByUrlLineNumber :: Int,
    pDebuggerSetBreakpointByUrlUrl :: Maybe String,
    pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
    pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
    pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
    pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "PDebuggerSetBreakpointByUrl" $ \v ->
         PDebuggerSetBreakpointByUrl <$> v .:  "lineNumber"
            <*> v  .:?  "url"
            <*> v  .:?  "urlRegex"
            <*> v  .:?  "scriptHash"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "condition"


instance ToJSON PDebuggerSetBreakpointByUrl  where
    toJSON v = A.object
        [ "lineNumber" .= pDebuggerSetBreakpointByUrlLineNumber v
        , "url" .= pDebuggerSetBreakpointByUrlUrl v
        , "urlRegex" .= pDebuggerSetBreakpointByUrlUrlRegex v
        , "scriptHash" .= pDebuggerSetBreakpointByUrlScriptHash v
        , "columnNumber" .= pDebuggerSetBreakpointByUrlColumnNumber v
        , "condition" .= pDebuggerSetBreakpointByUrlCondition v
        ]


debuggerSetBreakpointByUrl :: Session -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl session params = sendReceiveCommandResult session "Debugger.setBreakpointByUrl" (Just params)



data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
    pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpointsActive where
    parseJSON = A.withObject "PDebuggerSetBreakpointsActive" $ \v ->
         PDebuggerSetBreakpointsActive <$> v .:  "active"


instance ToJSON PDebuggerSetBreakpointsActive  where
    toJSON v = A.object
        [ "active" .= pDebuggerSetBreakpointsActiveActive v
        ]


debuggerSetBreakpointsActive :: Session -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive session params = sendReceiveCommand session "Debugger.setBreakpointsActive" (Just params)



data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions {
    pDebuggerSetPauseOnExceptionsState :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetPauseOnExceptions where
    parseJSON = A.withObject "PDebuggerSetPauseOnExceptions" $ \v ->
         PDebuggerSetPauseOnExceptions <$> v .:  "state"


instance ToJSON PDebuggerSetPauseOnExceptions  where
    toJSON v = A.object
        [ "state" .= pDebuggerSetPauseOnExceptionsState v
        ]


debuggerSetPauseOnExceptions :: Session -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions session params = sendReceiveCommand session "Debugger.setPauseOnExceptions" (Just params)

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



instance Command  DebuggerSetScriptSource where
    commandName _ = "Debugger.setScriptSource"

data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
    pDebuggerSetScriptSourceScriptId :: RuntimeScriptId,
    pDebuggerSetScriptSourceScriptSource :: String,
    pDebuggerSetScriptSourceDryRun :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetScriptSource where
    parseJSON = A.withObject "PDebuggerSetScriptSource" $ \v ->
         PDebuggerSetScriptSource <$> v .:  "scriptId"
            <*> v  .:  "scriptSource"
            <*> v  .:?  "dryRun"


instance ToJSON PDebuggerSetScriptSource  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerSetScriptSourceScriptId v
        , "scriptSource" .= pDebuggerSetScriptSourceScriptSource v
        , "dryRun" .= pDebuggerSetScriptSourceDryRun v
        ]


debuggerSetScriptSource :: Session -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource session params = sendReceiveCommandResult session "Debugger.setScriptSource" (Just params)



data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
    pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetSkipAllPauses where
    parseJSON = A.withObject "PDebuggerSetSkipAllPauses" $ \v ->
         PDebuggerSetSkipAllPauses <$> v .:  "skip"


instance ToJSON PDebuggerSetSkipAllPauses  where
    toJSON v = A.object
        [ "skip" .= pDebuggerSetSkipAllPausesSkip v
        ]


debuggerSetSkipAllPauses :: Session -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses session params = sendReceiveCommand session "Debugger.setSkipAllPauses" (Just params)



data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
    pDebuggerSetVariableValueScopeNumber :: Int,
    pDebuggerSetVariableValueVariableName :: String,
    pDebuggerSetVariableValueNewValue :: RuntimeCallArgument,
    pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetVariableValue where
    parseJSON = A.withObject "PDebuggerSetVariableValue" $ \v ->
         PDebuggerSetVariableValue <$> v .:  "scopeNumber"
            <*> v  .:  "variableName"
            <*> v  .:  "newValue"
            <*> v  .:  "callFrameId"


instance ToJSON PDebuggerSetVariableValue  where
    toJSON v = A.object
        [ "scopeNumber" .= pDebuggerSetVariableValueScopeNumber v
        , "variableName" .= pDebuggerSetVariableValueVariableName v
        , "newValue" .= pDebuggerSetVariableValueNewValue v
        , "callFrameId" .= pDebuggerSetVariableValueCallFrameId v
        ]


debuggerSetVariableValue :: Session -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue session params = sendReceiveCommand session "Debugger.setVariableValue" (Just params)




debuggerStepInto :: Session -> IO (Maybe Error)
debuggerStepInto session = sendReceiveCommand session "Debugger.stepInto" (Nothing :: Maybe ())




debuggerStepOut :: Session -> IO (Maybe Error)
debuggerStepOut session = sendReceiveCommand session "Debugger.stepOut" (Nothing :: Maybe ())




debuggerStepOver :: Session -> IO (Maybe Error)
debuggerStepOver session = sendReceiveCommand session "Debugger.stepOver" (Nothing :: Maybe ())



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


instance FromEvent Event ProfilerConsoleProfileFinished where
    eventName  _ _    =  "Profiler.consoleProfileFinished"
    fromEvent ev =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing

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


instance FromEvent Event ProfilerConsoleProfileStarted where
    eventName  _ _    =  "Profiler.consoleProfileStarted"
    fromEvent ev =  case ev of EVProfilerConsoleProfileStarted v -> Just v; _ -> Nothing


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
profilerDisable session = sendReceiveCommand session "Profiler.disable" (Nothing :: Maybe ())




profilerEnable :: Session -> IO (Maybe Error)
profilerEnable session = sendReceiveCommand session "Profiler.enable" (Nothing :: Maybe ())

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerGetBestEffortCoverage where
    parseJSON = A.withObject "ProfilerGetBestEffortCoverage" $ \v ->
         ProfilerGetBestEffortCoverage <$> v .:  "result"



instance Command  ProfilerGetBestEffortCoverage where
    commandName _ = "Profiler.getBestEffortCoverage"


profilerGetBestEffortCoverage :: Session -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage session = sendReceiveCommandResult session "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())



data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
    pProfilerSetSamplingIntervalInterval :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PProfilerSetSamplingInterval where
    parseJSON = A.withObject "PProfilerSetSamplingInterval" $ \v ->
         PProfilerSetSamplingInterval <$> v .:  "interval"


instance ToJSON PProfilerSetSamplingInterval  where
    toJSON v = A.object
        [ "interval" .= pProfilerSetSamplingIntervalInterval v
        ]


profilerSetSamplingInterval :: Session -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval session params = sendReceiveCommand session "Profiler.setSamplingInterval" (Just params)




profilerStart :: Session -> IO (Maybe Error)
profilerStart session = sendReceiveCommand session "Profiler.start" (Nothing :: Maybe ())

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
    profilerStartPreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStartPreciseCoverage where
    parseJSON = A.withObject "ProfilerStartPreciseCoverage" $ \v ->
         ProfilerStartPreciseCoverage <$> v .:  "timestamp"



instance Command  ProfilerStartPreciseCoverage where
    commandName _ = "Profiler.startPreciseCoverage"

data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
    pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
    pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
    pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PProfilerStartPreciseCoverage where
    parseJSON = A.withObject "PProfilerStartPreciseCoverage" $ \v ->
         PProfilerStartPreciseCoverage <$> v .:?  "callCount"
            <*> v  .:?  "detailed"
            <*> v  .:?  "allowTriggeredUpdates"


instance ToJSON PProfilerStartPreciseCoverage  where
    toJSON v = A.object
        [ "callCount" .= pProfilerStartPreciseCoverageCallCount v
        , "detailed" .= pProfilerStartPreciseCoverageDetailed v
        , "allowTriggeredUpdates" .= pProfilerStartPreciseCoverageAllowTriggeredUpdates v
        ]


profilerStartPreciseCoverage :: Session -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage session params = sendReceiveCommandResult session "Profiler.startPreciseCoverage" (Just params)

data ProfilerStop = ProfilerStop {
    profilerStopProfile :: ProfilerProfile
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStop where
    parseJSON = A.withObject "ProfilerStop" $ \v ->
         ProfilerStop <$> v .:  "profile"



instance Command  ProfilerStop where
    commandName _ = "Profiler.stop"


profilerStop :: Session -> IO (Either Error ProfilerStop)
profilerStop session = sendReceiveCommandResult session "Profiler.stop" (Nothing :: Maybe ())




profilerStopPreciseCoverage :: Session -> IO (Maybe Error)
profilerStopPreciseCoverage session = sendReceiveCommand session "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    profilerTakePreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerTakePreciseCoverage where
    parseJSON = A.withObject "ProfilerTakePreciseCoverage" $ \v ->
         ProfilerTakePreciseCoverage <$> v .:  "result"
            <*> v  .:  "timestamp"



instance Command  ProfilerTakePreciseCoverage where
    commandName _ = "Profiler.takePreciseCoverage"


profilerTakePreciseCoverage :: Session -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage session = sendReceiveCommandResult session "Profiler.takePreciseCoverage" (Nothing :: Maybe ())



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


instance FromEvent Event RuntimeConsoleApiCalled where
    eventName  _ _    =  "Runtime.consoleAPICalled"
    fromEvent ev =  case ev of EVRuntimeConsoleApiCalled v -> Just v; _ -> Nothing

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


instance FromEvent Event RuntimeExceptionRevoked where
    eventName  _ _    =  "Runtime.exceptionRevoked"
    fromEvent ev =  case ev of EVRuntimeExceptionRevoked v -> Just v; _ -> Nothing

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


instance FromEvent Event RuntimeExceptionThrown where
    eventName  _ _    =  "Runtime.exceptionThrown"
    fromEvent ev =  case ev of EVRuntimeExceptionThrown v -> Just v; _ -> Nothing

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


instance FromEvent Event RuntimeExecutionContextCreated where
    eventName  _ _    =  "Runtime.executionContextCreated"
    fromEvent ev =  case ev of EVRuntimeExecutionContextCreated v -> Just v; _ -> Nothing

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


instance FromEvent Event RuntimeExecutionContextDestroyed where
    eventName  _ _    =  "Runtime.executionContextDestroyed"
    fromEvent ev =  case ev of EVRuntimeExecutionContextDestroyed v -> Just v; _ -> Nothing

data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
    deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
    parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
        case v of
                "RuntimeExecutionContextsCleared" -> pure $ RuntimeExecutionContextsCleared
                _ -> fail "failed to parse RuntimeExecutionContextsCleared"

instance FromEvent Event RuntimeExecutionContextsCleared where
    eventName  _ _    =  "Runtime.executionContextsCleared"
    fromEvent ev =  case ev of EVRuntimeExecutionContextsCleared v -> Just v; _ -> Nothing

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


instance FromEvent Event RuntimeInspectRequested where
    eventName  _ _    =  "Runtime.inspectRequested"
    fromEvent ev =  case ev of EVRuntimeInspectRequested v -> Just v; _ -> Nothing


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



instance Command  RuntimeAwaitPromise where
    commandName _ = "Runtime.awaitPromise"

data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
    pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
    pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
    pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeAwaitPromise where
    parseJSON = A.withObject "PRuntimeAwaitPromise" $ \v ->
         PRuntimeAwaitPromise <$> v .:  "promiseObjectId"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "generatePreview"


instance ToJSON PRuntimeAwaitPromise  where
    toJSON v = A.object
        [ "promiseObjectId" .= pRuntimeAwaitPromisePromiseObjectId v
        , "returnByValue" .= pRuntimeAwaitPromiseReturnByValue v
        , "generatePreview" .= pRuntimeAwaitPromiseGeneratePreview v
        ]


runtimeAwaitPromise :: Session -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise session params = sendReceiveCommandResult session "Runtime.awaitPromise" (Just params)

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallFunctionOn where
    parseJSON = A.withObject "RuntimeCallFunctionOn" $ \v ->
         RuntimeCallFunctionOn <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeCallFunctionOn where
    commandName _ = "Runtime.callFunctionOn"

data PRuntimeCallFunctionOn = PRuntimeCallFunctionOn {
    pRuntimeCallFunctionOnFunctionDeclaration :: String,
    pRuntimeCallFunctionOnObjectId :: Maybe RuntimeRemoteObjectId,
    pRuntimeCallFunctionOnArguments :: Maybe [RuntimeCallArgument],
    pRuntimeCallFunctionOnSilent :: Maybe Bool,
    pRuntimeCallFunctionOnReturnByValue :: Maybe Bool,
    pRuntimeCallFunctionOnUserGesture :: Maybe Bool,
    pRuntimeCallFunctionOnAwaitPromise :: Maybe Bool,
    pRuntimeCallFunctionOnExecutionContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeCallFunctionOnObjectGroup :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeCallFunctionOn where
    parseJSON = A.withObject "PRuntimeCallFunctionOn" $ \v ->
         PRuntimeCallFunctionOn <$> v .:  "functionDeclaration"
            <*> v  .:?  "objectId"
            <*> v  .:?  "arguments"
            <*> v  .:?  "silent"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "userGesture"
            <*> v  .:?  "awaitPromise"
            <*> v  .:?  "executionContextId"
            <*> v  .:?  "objectGroup"


instance ToJSON PRuntimeCallFunctionOn  where
    toJSON v = A.object
        [ "functionDeclaration" .= pRuntimeCallFunctionOnFunctionDeclaration v
        , "objectId" .= pRuntimeCallFunctionOnObjectId v
        , "arguments" .= pRuntimeCallFunctionOnArguments v
        , "silent" .= pRuntimeCallFunctionOnSilent v
        , "returnByValue" .= pRuntimeCallFunctionOnReturnByValue v
        , "userGesture" .= pRuntimeCallFunctionOnUserGesture v
        , "awaitPromise" .= pRuntimeCallFunctionOnAwaitPromise v
        , "executionContextId" .= pRuntimeCallFunctionOnExecutionContextId v
        , "objectGroup" .= pRuntimeCallFunctionOnObjectGroup v
        ]


runtimeCallFunctionOn :: Session -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn session params = sendReceiveCommandResult session "Runtime.callFunctionOn" (Just params)

data RuntimeCompileScript = RuntimeCompileScript {
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCompileScript where
    parseJSON = A.withObject "RuntimeCompileScript" $ \v ->
         RuntimeCompileScript <$> v .:?  "scriptId"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeCompileScript where
    commandName _ = "Runtime.compileScript"

data PRuntimeCompileScript = PRuntimeCompileScript {
    pRuntimeCompileScriptExpression :: String,
    pRuntimeCompileScriptSourceUrl :: String,
    pRuntimeCompileScriptPersistScript :: Bool,
    pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeCompileScript where
    parseJSON = A.withObject "PRuntimeCompileScript" $ \v ->
         PRuntimeCompileScript <$> v .:  "expression"
            <*> v  .:  "sourceURL"
            <*> v  .:  "persistScript"
            <*> v  .:?  "executionContextId"


instance ToJSON PRuntimeCompileScript  where
    toJSON v = A.object
        [ "expression" .= pRuntimeCompileScriptExpression v
        , "sourceURL" .= pRuntimeCompileScriptSourceUrl v
        , "persistScript" .= pRuntimeCompileScriptPersistScript v
        , "executionContextId" .= pRuntimeCompileScriptExecutionContextId v
        ]


runtimeCompileScript :: Session -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript session params = sendReceiveCommandResult session "Runtime.compileScript" (Just params)




runtimeDisable :: Session -> IO (Maybe Error)
runtimeDisable session = sendReceiveCommand session "Runtime.disable" (Nothing :: Maybe ())




runtimeDiscardConsoleEntries :: Session -> IO (Maybe Error)
runtimeDiscardConsoleEntries session = sendReceiveCommand session "Runtime.discardConsoleEntries" (Nothing :: Maybe ())




runtimeEnable :: Session -> IO (Maybe Error)
runtimeEnable session = sendReceiveCommand session "Runtime.enable" (Nothing :: Maybe ())

data RuntimeEvaluate = RuntimeEvaluate {
    runtimeEvaluateResult :: RuntimeRemoteObject,
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeEvaluate where
    parseJSON = A.withObject "RuntimeEvaluate" $ \v ->
         RuntimeEvaluate <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeEvaluate where
    commandName _ = "Runtime.evaluate"

data PRuntimeEvaluate = PRuntimeEvaluate {
    pRuntimeEvaluateExpression :: String,
    pRuntimeEvaluateObjectGroup :: Maybe String,
    pRuntimeEvaluateIncludeCommandLineApi :: Maybe Bool,
    pRuntimeEvaluateSilent :: Maybe Bool,
    pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeEvaluateReturnByValue :: Maybe Bool,
    pRuntimeEvaluateUserGesture :: Maybe Bool,
    pRuntimeEvaluateAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeEvaluate where
    parseJSON = A.withObject "PRuntimeEvaluate" $ \v ->
         PRuntimeEvaluate <$> v .:  "expression"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "silent"
            <*> v  .:?  "contextId"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "userGesture"
            <*> v  .:?  "awaitPromise"


instance ToJSON PRuntimeEvaluate  where
    toJSON v = A.object
        [ "expression" .= pRuntimeEvaluateExpression v
        , "objectGroup" .= pRuntimeEvaluateObjectGroup v
        , "includeCommandLineAPI" .= pRuntimeEvaluateIncludeCommandLineApi v
        , "silent" .= pRuntimeEvaluateSilent v
        , "contextId" .= pRuntimeEvaluateContextId v
        , "returnByValue" .= pRuntimeEvaluateReturnByValue v
        , "userGesture" .= pRuntimeEvaluateUserGesture v
        , "awaitPromise" .= pRuntimeEvaluateAwaitPromise v
        ]


runtimeEvaluate :: Session -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate session params = sendReceiveCommandResult session "Runtime.evaluate" (Just params)

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



instance Command  RuntimeGetProperties where
    commandName _ = "Runtime.getProperties"

data PRuntimeGetProperties = PRuntimeGetProperties {
    pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
    pRuntimeGetPropertiesOwnProperties :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeGetProperties where
    parseJSON = A.withObject "PRuntimeGetProperties" $ \v ->
         PRuntimeGetProperties <$> v .:  "objectId"
            <*> v  .:?  "ownProperties"


instance ToJSON PRuntimeGetProperties  where
    toJSON v = A.object
        [ "objectId" .= pRuntimeGetPropertiesObjectId v
        , "ownProperties" .= pRuntimeGetPropertiesOwnProperties v
        ]


runtimeGetProperties :: Session -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties session params = sendReceiveCommandResult session "Runtime.getProperties" (Just params)

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
    runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "RuntimeGlobalLexicalScopeNames" $ \v ->
         RuntimeGlobalLexicalScopeNames <$> v .:  "names"



instance Command  RuntimeGlobalLexicalScopeNames where
    commandName _ = "Runtime.globalLexicalScopeNames"

data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
    pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "PRuntimeGlobalLexicalScopeNames" $ \v ->
         PRuntimeGlobalLexicalScopeNames <$> v .:?  "executionContextId"


instance ToJSON PRuntimeGlobalLexicalScopeNames  where
    toJSON v = A.object
        [ "executionContextId" .= pRuntimeGlobalLexicalScopeNamesExecutionContextId v
        ]


runtimeGlobalLexicalScopeNames :: Session -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames session params = sendReceiveCommandResult session "Runtime.globalLexicalScopeNames" (Just params)

data RuntimeQueryObjects = RuntimeQueryObjects {
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeQueryObjects where
    parseJSON = A.withObject "RuntimeQueryObjects" $ \v ->
         RuntimeQueryObjects <$> v .:  "objects"



instance Command  RuntimeQueryObjects where
    commandName _ = "Runtime.queryObjects"

data PRuntimeQueryObjects = PRuntimeQueryObjects {
    pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
    pRuntimeQueryObjectsObjectGroup :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeQueryObjects where
    parseJSON = A.withObject "PRuntimeQueryObjects" $ \v ->
         PRuntimeQueryObjects <$> v .:  "prototypeObjectId"
            <*> v  .:?  "objectGroup"


instance ToJSON PRuntimeQueryObjects  where
    toJSON v = A.object
        [ "prototypeObjectId" .= pRuntimeQueryObjectsPrototypeObjectId v
        , "objectGroup" .= pRuntimeQueryObjectsObjectGroup v
        ]


runtimeQueryObjects :: Session -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects session params = sendReceiveCommandResult session "Runtime.queryObjects" (Just params)



data PRuntimeReleaseObject = PRuntimeReleaseObject {
    pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeReleaseObject where
    parseJSON = A.withObject "PRuntimeReleaseObject" $ \v ->
         PRuntimeReleaseObject <$> v .:  "objectId"


instance ToJSON PRuntimeReleaseObject  where
    toJSON v = A.object
        [ "objectId" .= pRuntimeReleaseObjectObjectId v
        ]


runtimeReleaseObject :: Session -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject session params = sendReceiveCommand session "Runtime.releaseObject" (Just params)



data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
    pRuntimeReleaseObjectGroupObjectGroup :: String
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeReleaseObjectGroup where
    parseJSON = A.withObject "PRuntimeReleaseObjectGroup" $ \v ->
         PRuntimeReleaseObjectGroup <$> v .:  "objectGroup"


instance ToJSON PRuntimeReleaseObjectGroup  where
    toJSON v = A.object
        [ "objectGroup" .= pRuntimeReleaseObjectGroupObjectGroup v
        ]


runtimeReleaseObjectGroup :: Session -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup session params = sendReceiveCommand session "Runtime.releaseObjectGroup" (Just params)




runtimeRunIfWaitingForDebugger :: Session -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger session = sendReceiveCommand session "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())

data RuntimeRunScript = RuntimeRunScript {
    runtimeRunScriptResult :: RuntimeRemoteObject,
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeRunScript where
    parseJSON = A.withObject "RuntimeRunScript" $ \v ->
         RuntimeRunScript <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeRunScript where
    commandName _ = "Runtime.runScript"

data PRuntimeRunScript = PRuntimeRunScript {
    pRuntimeRunScriptScriptId :: RuntimeScriptId,
    pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeRunScriptObjectGroup :: Maybe String,
    pRuntimeRunScriptSilent :: Maybe Bool,
    pRuntimeRunScriptIncludeCommandLineApi :: Maybe Bool,
    pRuntimeRunScriptReturnByValue :: Maybe Bool,
    pRuntimeRunScriptGeneratePreview :: Maybe Bool,
    pRuntimeRunScriptAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeRunScript where
    parseJSON = A.withObject "PRuntimeRunScript" $ \v ->
         PRuntimeRunScript <$> v .:  "scriptId"
            <*> v  .:?  "executionContextId"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "silent"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "generatePreview"
            <*> v  .:?  "awaitPromise"


instance ToJSON PRuntimeRunScript  where
    toJSON v = A.object
        [ "scriptId" .= pRuntimeRunScriptScriptId v
        , "executionContextId" .= pRuntimeRunScriptExecutionContextId v
        , "objectGroup" .= pRuntimeRunScriptObjectGroup v
        , "silent" .= pRuntimeRunScriptSilent v
        , "includeCommandLineAPI" .= pRuntimeRunScriptIncludeCommandLineApi v
        , "returnByValue" .= pRuntimeRunScriptReturnByValue v
        , "generatePreview" .= pRuntimeRunScriptGeneratePreview v
        , "awaitPromise" .= pRuntimeRunScriptAwaitPromise v
        ]


runtimeRunScript :: Session -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript session params = sendReceiveCommandResult session "Runtime.runScript" (Just params)



data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
    pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeSetAsyncCallStackDepth where
    parseJSON = A.withObject "PRuntimeSetAsyncCallStackDepth" $ \v ->
         PRuntimeSetAsyncCallStackDepth <$> v .:  "maxDepth"


instance ToJSON PRuntimeSetAsyncCallStackDepth  where
    toJSON v = A.object
        [ "maxDepth" .= pRuntimeSetAsyncCallStackDepthMaxDepth v
        ]


runtimeSetAsyncCallStackDepth :: Session -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth session params = sendReceiveCommand session "Runtime.setAsyncCallStackDepth" (Just params)




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



instance Command  SchemaGetDomains where
    commandName _ = "Schema.getDomains"


schemaGetDomains :: Session -> IO (Either Error SchemaGetDomains)
schemaGetDomains session = sendReceiveCommandResult session "Schema.getDomains" (Nothing :: Maybe ())





