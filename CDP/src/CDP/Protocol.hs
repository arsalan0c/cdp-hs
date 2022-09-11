{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Protocol (module CDP.Protocol) where


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
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Runtime

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






browserClose :: Handle Event -> IO (Maybe Error)
browserClose handle = sendReceiveCommand handle "Browser.close" (Nothing :: Maybe ())

data BrowserGetVersion = BrowserGetVersion {
    browserGetVersionProtocolVersion :: String,
    browserGetVersionProduct :: String,
    browserGetVersionRevision :: String,
    browserGetVersionUserAgent :: String,
    browserGetVersionJsVersion :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  BrowserGetVersion where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command  BrowserGetVersion where
    commandName _ = "Browser.getVersion"


browserGetVersion :: Handle Event -> IO (Either Error BrowserGetVersion)
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())



data DOMAttributeModified = DOMAttributeModified {
    dOMAttributeModifiedNodeId :: DOMNodeId,
    dOMAttributeModifiedName :: String,
    dOMAttributeModifiedValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMAttributeModified where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON DOMAttributeModified  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


instance FromEvent Event DOMAttributeModified where
    eventName  _ _    =  "DOM.attributeModified"
    fromEvent ev =  case ev of EVDOMAttributeModified v -> Just v; _ -> Nothing

data DOMAttributeRemoved = DOMAttributeRemoved {
    dOMAttributeRemovedNodeId :: DOMNodeId,
    dOMAttributeRemovedName :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMAttributeRemoved where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON DOMAttributeRemoved  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


instance FromEvent Event DOMAttributeRemoved where
    eventName  _ _    =  "DOM.attributeRemoved"
    fromEvent ev =  case ev of EVDOMAttributeRemoved v -> Just v; _ -> Nothing

data DOMCharacterDataModified = DOMCharacterDataModified {
    dOMCharacterDataModifiedNodeId :: DOMNodeId,
    dOMCharacterDataModifiedCharacterData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMCharacterDataModified where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON DOMCharacterDataModified  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


instance FromEvent Event DOMCharacterDataModified where
    eventName  _ _    =  "DOM.characterDataModified"
    fromEvent ev =  case ev of EVDOMCharacterDataModified v -> Just v; _ -> Nothing

data DOMChildNodeCountUpdated = DOMChildNodeCountUpdated {
    dOMChildNodeCountUpdatedNodeId :: DOMNodeId,
    dOMChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMChildNodeCountUpdated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON DOMChildNodeCountUpdated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


instance FromEvent Event DOMChildNodeCountUpdated where
    eventName  _ _    =  "DOM.childNodeCountUpdated"
    fromEvent ev =  case ev of EVDOMChildNodeCountUpdated v -> Just v; _ -> Nothing

data DOMChildNodeInserted = DOMChildNodeInserted {
    dOMChildNodeInsertedParentNodeId :: DOMNodeId,
    dOMChildNodeInsertedPreviousNodeId :: DOMNodeId,
    dOMChildNodeInsertedNode :: DOMNode
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMChildNodeInserted where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON DOMChildNodeInserted  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


instance FromEvent Event DOMChildNodeInserted where
    eventName  _ _    =  "DOM.childNodeInserted"
    fromEvent ev =  case ev of EVDOMChildNodeInserted v -> Just v; _ -> Nothing

data DOMChildNodeRemoved = DOMChildNodeRemoved {
    dOMChildNodeRemovedParentNodeId :: DOMNodeId,
    dOMChildNodeRemovedNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMChildNodeRemoved where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON DOMChildNodeRemoved  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


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
    dOMSetChildNodesParentId :: DOMNodeId,
    dOMSetChildNodesNodes :: [DOMNode]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMSetChildNodes where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON DOMSetChildNodes  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


instance FromEvent Event DOMSetChildNodes where
    eventName  _ _    =  "DOM.setChildNodes"
    fromEvent ev =  case ev of EVDOMSetChildNodes v -> Just v; _ -> Nothing


type DOMNodeId = Int

type DOMBackendNodeId = Int

data DOMBackendNode = DOMBackendNode {
    dOMBackendNodeNodeType :: Int,
    dOMBackendNodeNodeName :: String,
    dOMBackendNodeBackendNodeId :: DOMBackendNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMBackendNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON DOMBackendNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}



data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
    dOMNodeNodeId :: DOMNodeId,
    dOMNodeBackendNodeId :: DOMBackendNodeId,
    dOMNodeNodeType :: Int,
    dOMNodeNodeName :: String,
    dOMNodeLocalName :: String,
    dOMNodeNodeValue :: String,
    dOMNodeParentId :: Maybe DOMNodeId,
    dOMNodeChildNodeCount :: Maybe Int,
    dOMNodeChildren :: Maybe [DOMNode],
    dOMNodeAttributes :: Maybe [String],
    dOMNodeDocumentURL :: Maybe String,
    dOMNodeBaseURL :: Maybe String,
    dOMNodePublicId :: Maybe String,
    dOMNodeSystemId :: Maybe String,
    dOMNodeInternalSubset :: Maybe String,
    dOMNodeXmlVersion :: Maybe String,
    dOMNodeName :: Maybe String,
    dOMNodeValue :: Maybe String,
    dOMNodePseudoType :: Maybe DOMPseudoType,
    dOMNodeShadowRootType :: Maybe DOMShadowRootType,
    dOMNodeFrameId :: Maybe PageFrameId,
    dOMNodeContentDocument :: Maybe DOMNode,
    dOMNodeShadowRoots :: Maybe [DOMNode],
    dOMNodeTemplateContent :: Maybe DOMNode,
    dOMNodePseudoElements :: Maybe [DOMNode],
    dOMNodeDistributedNodes :: Maybe [DOMBackendNode],
    dOMNodeIsSVG :: Maybe Bool,
    dOMNodeCompatibilityMode :: Maybe DOMCompatibilityMode,
    dOMNodeAssignedSlot :: Maybe DOMBackendNode
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }

instance ToJSON DOMNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}



data DOMRGBA = DOMRGBA {
    dOMRGBAR :: Int,
    dOMRGBAG :: Int,
    dOMRGBAB :: Int,
    dOMRGBAA :: Maybe Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMRGBA where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }

instance ToJSON DOMRGBA  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}



type DOMQuad = [Double]

data DOMBoxModel = DOMBoxModel {
    dOMBoxModelContent :: DOMQuad,
    dOMBoxModelPadding :: DOMQuad,
    dOMBoxModelBorder :: DOMQuad,
    dOMBoxModelMargin :: DOMQuad,
    dOMBoxModelWidth :: Int,
    dOMBoxModelHeight :: Int,
    dOMBoxModelShapeOutside :: Maybe DOMShapeOutsideInfo
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMBoxModel where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }

instance ToJSON DOMBoxModel  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}



data DOMShapeOutsideInfo = DOMShapeOutsideInfo {
    dOMShapeOutsideInfoBounds :: DOMQuad,
    dOMShapeOutsideInfoShape :: [Int],
    dOMShapeOutsideInfoMarginShape :: [Int]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMShapeOutsideInfo where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON DOMShapeOutsideInfo  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



data DOMRect = DOMRect {
    dOMRectX :: Double,
    dOMRectY :: Double,
    dOMRectWidth :: Double,
    dOMRectHeight :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMRect where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }

instance ToJSON DOMRect  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}



data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty {
    dOMCSSComputedStylePropertyName :: String,
    dOMCSSComputedStylePropertyValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMCSSComputedStyleProperty where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON DOMCSSComputedStyleProperty  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


data DOMDescribeNode = DOMDescribeNode {
    dOMDescribeNodeNode :: DOMNode
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMDescribeNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Command  DOMDescribeNode where
    commandName _ = "DOM.describeNode"

data PDOMDescribeNode = PDOMDescribeNode {
    pDOMDescribeNodeNodeId :: Maybe DOMNodeId,
    pDOMDescribeNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMDescribeNodeObjectId :: Maybe RuntimeRemoteObjectId,
    pDOMDescribeNodeDepth :: Maybe Int,
    pDOMDescribeNodePierce :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDescribeNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON PDOMDescribeNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


dOMDescribeNode :: Handle Event -> PDOMDescribeNode -> IO (Either Error DOMDescribeNode)
dOMDescribeNode handle params = sendReceiveCommandResult handle "DOM.describeNode" (Just params)




dOMDisable :: Handle Event -> IO (Maybe Error)
dOMDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())




dOMEnable :: Handle Event -> IO (Maybe Error)
dOMEnable handle = sendReceiveCommand handle "DOM.enable" (Nothing :: Maybe ())



data PDOMFocus = PDOMFocus {
    pDOMFocusNodeId :: Maybe DOMNodeId,
    pDOMFocusBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMFocusObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMFocus where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance ToJSON PDOMFocus  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}


dOMFocus :: Handle Event -> PDOMFocus -> IO (Maybe Error)
dOMFocus handle params = sendReceiveCommand handle "DOM.focus" (Just params)

data DOMGetAttributes = DOMGetAttributes {
    dOMGetAttributesAttributes :: [String]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMGetAttributes where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  DOMGetAttributes where
    commandName _ = "DOM.getAttributes"

data PDOMGetAttributes = PDOMGetAttributes {
    pDOMGetAttributesNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMGetAttributes where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PDOMGetAttributes  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


dOMGetAttributes :: Handle Event -> PDOMGetAttributes -> IO (Either Error DOMGetAttributes)
dOMGetAttributes handle params = sendReceiveCommandResult handle "DOM.getAttributes" (Just params)

data DOMGetBoxModel = DOMGetBoxModel {
    dOMGetBoxModelModel :: DOMBoxModel
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMGetBoxModel where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  DOMGetBoxModel where
    commandName _ = "DOM.getBoxModel"

data PDOMGetBoxModel = PDOMGetBoxModel {
    pDOMGetBoxModelNodeId :: Maybe DOMNodeId,
    pDOMGetBoxModelBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMGetBoxModelObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMGetBoxModel where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDOMGetBoxModel  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


dOMGetBoxModel :: Handle Event -> PDOMGetBoxModel -> IO (Either Error DOMGetBoxModel)
dOMGetBoxModel handle params = sendReceiveCommandResult handle "DOM.getBoxModel" (Just params)

data DOMGetDocument = DOMGetDocument {
    dOMGetDocumentRoot :: DOMNode
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMGetDocument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  DOMGetDocument where
    commandName _ = "DOM.getDocument"

data PDOMGetDocument = PDOMGetDocument {
    pDOMGetDocumentDepth :: Maybe Int,
    pDOMGetDocumentPierce :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMGetDocument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDOMGetDocument  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


dOMGetDocument :: Handle Event -> PDOMGetDocument -> IO (Either Error DOMGetDocument)
dOMGetDocument handle params = sendReceiveCommandResult handle "DOM.getDocument" (Just params)

data DOMGetNodeForLocation = DOMGetNodeForLocation {
    dOMGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    dOMGetNodeForLocationFrameId :: PageFrameId,
    dOMGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMGetNodeForLocation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command  DOMGetNodeForLocation where
    commandName _ = "DOM.getNodeForLocation"

data PDOMGetNodeForLocation = PDOMGetNodeForLocation {
    pDOMGetNodeForLocationX :: Int,
    pDOMGetNodeForLocationY :: Int,
    pDOMGetNodeForLocationIncludeUserAgentShadowDOM :: Maybe Bool,
    pDOMGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMGetNodeForLocation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PDOMGetNodeForLocation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


dOMGetNodeForLocation :: Handle Event -> PDOMGetNodeForLocation -> IO (Either Error DOMGetNodeForLocation)
dOMGetNodeForLocation handle params = sendReceiveCommandResult handle "DOM.getNodeForLocation" (Just params)

data DOMGetOuterHtml = DOMGetOuterHtml {
    dOMGetOuterHtmlOuterHtml :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMGetOuterHtml where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Command  DOMGetOuterHtml where
    commandName _ = "DOM.getOuterHTML"

data PDOMGetOuterHtml = PDOMGetOuterHtml {
    pDOMGetOuterHtmlNodeId :: Maybe DOMNodeId,
    pDOMGetOuterHtmlBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMGetOuterHtmlObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMGetOuterHtml where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON PDOMGetOuterHtml  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


dOMGetOuterHtml :: Handle Event -> PDOMGetOuterHtml -> IO (Either Error DOMGetOuterHtml)
dOMGetOuterHtml handle params = sendReceiveCommandResult handle "DOM.getOuterHTML" (Just params)




dOMHideHighlight :: Handle Event -> IO (Maybe Error)
dOMHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())




dOMHighlightNode :: Handle Event -> IO (Maybe Error)
dOMHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())




dOMHighlightRect :: Handle Event -> IO (Maybe Error)
dOMHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())

data DOMMoveTo = DOMMoveTo {
    dOMMoveToNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMMoveTo where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


instance Command  DOMMoveTo where
    commandName _ = "DOM.moveTo"

data PDOMMoveTo = PDOMMoveTo {
    pDOMMoveToNodeId :: DOMNodeId,
    pDOMMoveToTargetNodeId :: DOMNodeId,
    pDOMMoveToInsertBeforeNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMMoveTo where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }

instance ToJSON PDOMMoveTo  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}


dOMMoveTo :: Handle Event -> PDOMMoveTo -> IO (Either Error DOMMoveTo)
dOMMoveTo handle params = sendReceiveCommandResult handle "DOM.moveTo" (Just params)

data DOMQuerySelector = DOMQuerySelector {
    dOMQuerySelectorNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMQuerySelector where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  DOMQuerySelector where
    commandName _ = "DOM.querySelector"

data PDOMQuerySelector = PDOMQuerySelector {
    pDOMQuerySelectorNodeId :: DOMNodeId,
    pDOMQuerySelectorSelector :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMQuerySelector where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PDOMQuerySelector  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


dOMQuerySelector :: Handle Event -> PDOMQuerySelector -> IO (Either Error DOMQuerySelector)
dOMQuerySelector handle params = sendReceiveCommandResult handle "DOM.querySelector" (Just params)

data DOMQuerySelectorAll = DOMQuerySelectorAll {
    dOMQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMQuerySelectorAll where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command  DOMQuerySelectorAll where
    commandName _ = "DOM.querySelectorAll"

data PDOMQuerySelectorAll = PDOMQuerySelectorAll {
    pDOMQuerySelectorAllNodeId :: DOMNodeId,
    pDOMQuerySelectorAllSelector :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMQuerySelectorAll where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON PDOMQuerySelectorAll  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


dOMQuerySelectorAll :: Handle Event -> PDOMQuerySelectorAll -> IO (Either Error DOMQuerySelectorAll)
dOMQuerySelectorAll handle params = sendReceiveCommandResult handle "DOM.querySelectorAll" (Just params)



data PDOMRemoveAttribute = PDOMRemoveAttribute {
    pDOMRemoveAttributeNodeId :: DOMNodeId,
    pDOMRemoveAttributeName :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMRemoveAttribute where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON PDOMRemoveAttribute  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


dOMRemoveAttribute :: Handle Event -> PDOMRemoveAttribute -> IO (Maybe Error)
dOMRemoveAttribute handle params = sendReceiveCommand handle "DOM.removeAttribute" (Just params)



data PDOMRemoveNode = PDOMRemoveNode {
    pDOMRemoveNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMRemoveNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON PDOMRemoveNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}


dOMRemoveNode :: Handle Event -> PDOMRemoveNode -> IO (Maybe Error)
dOMRemoveNode handle params = sendReceiveCommand handle "DOM.removeNode" (Just params)



data PDOMRequestChildNodes = PDOMRequestChildNodes {
    pDOMRequestChildNodesNodeId :: DOMNodeId,
    pDOMRequestChildNodesDepth :: Maybe Int,
    pDOMRequestChildNodesPierce :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMRequestChildNodes where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PDOMRequestChildNodes  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


dOMRequestChildNodes :: Handle Event -> PDOMRequestChildNodes -> IO (Maybe Error)
dOMRequestChildNodes handle params = sendReceiveCommand handle "DOM.requestChildNodes" (Just params)

data DOMRequestNode = DOMRequestNode {
    dOMRequestNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMRequestNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  DOMRequestNode where
    commandName _ = "DOM.requestNode"

data PDOMRequestNode = PDOMRequestNode {
    pDOMRequestNodeObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMRequestNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDOMRequestNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


dOMRequestNode :: Handle Event -> PDOMRequestNode -> IO (Either Error DOMRequestNode)
dOMRequestNode handle params = sendReceiveCommandResult handle "DOM.requestNode" (Just params)

data DOMResolveNode = DOMResolveNode {
    dOMResolveNodeObject :: RuntimeRemoteObject
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMResolveNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  DOMResolveNode where
    commandName _ = "DOM.resolveNode"

data PDOMResolveNode = PDOMResolveNode {
    pDOMResolveNodeNodeId :: Maybe DOMNodeId,
    pDOMResolveNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMResolveNodeObjectGroup :: Maybe String,
    pDOMResolveNodeExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMResolveNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDOMResolveNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


dOMResolveNode :: Handle Event -> PDOMResolveNode -> IO (Either Error DOMResolveNode)
dOMResolveNode handle params = sendReceiveCommandResult handle "DOM.resolveNode" (Just params)



data PDOMSetAttributeValue = PDOMSetAttributeValue {
    pDOMSetAttributeValueNodeId :: DOMNodeId,
    pDOMSetAttributeValueName :: String,
    pDOMSetAttributeValueValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetAttributeValue where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PDOMSetAttributeValue  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


dOMSetAttributeValue :: Handle Event -> PDOMSetAttributeValue -> IO (Maybe Error)
dOMSetAttributeValue handle params = sendReceiveCommand handle "DOM.setAttributeValue" (Just params)



data PDOMSetAttributesAsText = PDOMSetAttributesAsText {
    pDOMSetAttributesAsTextNodeId :: DOMNodeId,
    pDOMSetAttributesAsTextText :: String,
    pDOMSetAttributesAsTextName :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetAttributesAsText where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON PDOMSetAttributesAsText  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


dOMSetAttributesAsText :: Handle Event -> PDOMSetAttributesAsText -> IO (Maybe Error)
dOMSetAttributesAsText handle params = sendReceiveCommand handle "DOM.setAttributesAsText" (Just params)



data PDOMSetFileInputFiles = PDOMSetFileInputFiles {
    pDOMSetFileInputFilesFiles :: [String],
    pDOMSetFileInputFilesNodeId :: Maybe DOMNodeId,
    pDOMSetFileInputFilesBackendNodeId :: Maybe DOMBackendNodeId,
    pDOMSetFileInputFilesObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetFileInputFiles where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PDOMSetFileInputFiles  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


dOMSetFileInputFiles :: Handle Event -> PDOMSetFileInputFiles -> IO (Maybe Error)
dOMSetFileInputFiles handle params = sendReceiveCommand handle "DOM.setFileInputFiles" (Just params)

data DOMSetNodeName = DOMSetNodeName {
    dOMSetNodeNameNodeId :: DOMNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMSetNodeName where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  DOMSetNodeName where
    commandName _ = "DOM.setNodeName"

data PDOMSetNodeName = PDOMSetNodeName {
    pDOMSetNodeNameNodeId :: DOMNodeId,
    pDOMSetNodeNameName :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetNodeName where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDOMSetNodeName  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


dOMSetNodeName :: Handle Event -> PDOMSetNodeName -> IO (Either Error DOMSetNodeName)
dOMSetNodeName handle params = sendReceiveCommandResult handle "DOM.setNodeName" (Just params)



data PDOMSetNodeValue = PDOMSetNodeValue {
    pDOMSetNodeValueNodeId :: DOMNodeId,
    pDOMSetNodeValueValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetNodeValue where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON PDOMSetNodeValue  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


dOMSetNodeValue :: Handle Event -> PDOMSetNodeValue -> IO (Maybe Error)
dOMSetNodeValue handle params = sendReceiveCommand handle "DOM.setNodeValue" (Just params)



data PDOMSetOuterHtml = PDOMSetOuterHtml {
    pDOMSetOuterHtmlNodeId :: DOMNodeId,
    pDOMSetOuterHtmlOuterHTML :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMSetOuterHtml where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON PDOMSetOuterHtml  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


dOMSetOuterHtml :: Handle Event -> PDOMSetOuterHtml -> IO (Maybe Error)
dOMSetOuterHtml handle params = sendReceiveCommand handle "DOM.setOuterHTML" (Just params)




data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
    deriving (Eq, Show, Read, Generic)
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
    dOMDebuggerEventListenerType :: String,
    dOMDebuggerEventListenerUseCapture :: Bool,
    dOMDebuggerEventListenerPassive :: Bool,
    dOMDebuggerEventListenerOnce :: Bool,
    dOMDebuggerEventListenerScriptId :: RuntimeScriptId,
    dOMDebuggerEventListenerLineNumber :: Int,
    dOMDebuggerEventListenerColumnNumber :: Int,
    dOMDebuggerEventListenerHandler :: Maybe RuntimeRemoteObject,
    dOMDebuggerEventListenerOriginalHandler :: Maybe RuntimeRemoteObject,
    dOMDebuggerEventListenerBackendNodeId :: Maybe DOMBackendNodeId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMDebuggerEventListener where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON DOMDebuggerEventListener  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners {
    dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DOMDebuggerGetEventListeners where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command  DOMDebuggerGetEventListeners where
    commandName _ = "DOMDebugger.getEventListeners"

data PDOMDebuggerGetEventListeners = PDOMDebuggerGetEventListeners {
    pDOMDebuggerGetEventListenersObjectId :: RuntimeRemoteObjectId,
    pDOMDebuggerGetEventListenersDepth :: Maybe Int,
    pDOMDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerGetEventListeners where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON PDOMDebuggerGetEventListeners  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


dOMDebuggerGetEventListeners :: Handle Event -> PDOMDebuggerGetEventListeners -> IO (Either Error DOMDebuggerGetEventListeners)
dOMDebuggerGetEventListeners handle params = sendReceiveCommandResult handle "DOMDebugger.getEventListeners" (Just params)



data PDOMDebuggerRemoveDomBreakpoint = PDOMDebuggerRemoveDomBreakpoint {
    pDOMDebuggerRemoveDomBreakpointNodeId :: DOMNodeId,
    pDOMDebuggerRemoveDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerRemoveDomBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON PDOMDebuggerRemoveDomBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


dOMDebuggerRemoveDomBreakpoint :: Handle Event -> PDOMDebuggerRemoveDomBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeDOMBreakpoint" (Just params)



data PDOMDebuggerRemoveEventListenerBreakpoint = PDOMDebuggerRemoveEventListenerBreakpoint {
    pDOMDebuggerRemoveEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerRemoveEventListenerBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }

instance ToJSON PDOMDebuggerRemoveEventListenerBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}


dOMDebuggerRemoveEventListenerBreakpoint :: Handle Event -> PDOMDebuggerRemoveEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeEventListenerBreakpoint" (Just params)



data PDOMDebuggerRemoveXhrBreakpoint = PDOMDebuggerRemoveXhrBreakpoint {
    pDOMDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerRemoveXhrBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON PDOMDebuggerRemoveXhrBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


dOMDebuggerRemoveXhrBreakpoint :: Handle Event -> PDOMDebuggerRemoveXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeXHRBreakpoint" (Just params)



data PDOMDebuggerSetDomBreakpoint = PDOMDebuggerSetDomBreakpoint {
    pDOMDebuggerSetDomBreakpointNodeId :: DOMNodeId,
    pDOMDebuggerSetDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerSetDomBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance ToJSON PDOMDebuggerSetDomBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}


dOMDebuggerSetDomBreakpoint :: Handle Event -> PDOMDebuggerSetDomBreakpoint -> IO (Maybe Error)
dOMDebuggerSetDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setDOMBreakpoint" (Just params)



data PDOMDebuggerSetEventListenerBreakpoint = PDOMDebuggerSetEventListenerBreakpoint {
    pDOMDebuggerSetEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerSetEventListenerBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }

instance ToJSON PDOMDebuggerSetEventListenerBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}


dOMDebuggerSetEventListenerBreakpoint :: Handle Event -> PDOMDebuggerSetEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerSetEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setEventListenerBreakpoint" (Just params)



data PDOMDebuggerSetXhrBreakpoint = PDOMDebuggerSetXhrBreakpoint {
    pDOMDebuggerSetXhrBreakpointUrl :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDOMDebuggerSetXhrBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance ToJSON PDOMDebuggerSetXhrBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}


dOMDebuggerSetXhrBreakpoint :: Handle Event -> PDOMDebuggerSetXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerSetXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setXHRBreakpoint" (Just params)




data EmulationScreenOrientation = EmulationScreenOrientation {
    emulationScreenOrientationType :: String,
    emulationScreenOrientationAngle :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  EmulationScreenOrientation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON EmulationScreenOrientation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}



data EmulationDisplayFeature = EmulationDisplayFeature {
    emulationDisplayFeatureOrientation :: String,
    emulationDisplayFeatureOffset :: Int,
    emulationDisplayFeatureMaskLength :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  EmulationDisplayFeature where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON EmulationDisplayFeature  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}



data EmulationMediaFeature = EmulationMediaFeature {
    emulationMediaFeatureName :: String,
    emulationMediaFeatureValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  EmulationMediaFeature where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON EmulationMediaFeature  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


data EmulationCanEmulate = EmulationCanEmulate {
    emulationCanEmulateResult :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  EmulationCanEmulate where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command  EmulationCanEmulate where
    commandName _ = "Emulation.canEmulate"


emulationCanEmulate :: Handle Event -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())




emulationClearDeviceMetricsOverride :: Handle Event -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())




emulationClearGeolocationOverride :: Handle Event -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())



data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
    pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMRGBA
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }

instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}


emulationSetDefaultBackgroundColorOverride :: Handle Event -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)



data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
    pEmulationSetDeviceMetricsOverrideWidth :: Int,
    pEmulationSetDeviceMetricsOverrideHeight :: Int,
    pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
    pEmulationSetDeviceMetricsOverrideMobile :: Bool,
    pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetDeviceMetricsOverride where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance ToJSON PEmulationSetDeviceMetricsOverride  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}


emulationSetDeviceMetricsOverride :: Handle Event -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)



data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
    pEmulationSetEmulatedMediaMedia :: Maybe String,
    pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetEmulatedMedia where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON PEmulationSetEmulatedMedia  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


emulationSetEmulatedMedia :: Handle Event -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia handle params = sendReceiveCommand handle "Emulation.setEmulatedMedia" (Just params)



data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
    pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
    pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
    pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetGeolocationOverride where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance ToJSON PEmulationSetGeolocationOverride  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}


emulationSetGeolocationOverride :: Handle Event -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)



data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
    pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetScriptExecutionDisabled where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance ToJSON PEmulationSetScriptExecutionDisabled  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}


emulationSetScriptExecutionDisabled :: Handle Event -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)



data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
    pEmulationSetTouchEmulationEnabledEnabled :: Bool,
    pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetTouchEmulationEnabled where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance ToJSON PEmulationSetTouchEmulationEnabled  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}


emulationSetTouchEmulationEnabled :: Handle Event -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)



data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
    pEmulationSetUserAgentOverrideUserAgent :: String,
    pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pEmulationSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PEmulationSetUserAgentOverride where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance ToJSON PEmulationSetUserAgentOverride  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}


emulationSetUserAgentOverride :: Handle Event -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)




type IOStreamHandle = String


data PIOClose = PIOClose {
    pIOCloseHandle :: IOStreamHandle
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PIOClose where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }

instance ToJSON PIOClose  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}


iOClose :: Handle Event -> PIOClose -> IO (Maybe Error)
iOClose handle params = sendReceiveCommand handle "IO.close" (Just params)

data IORead = IORead {
    iOReadData :: String,
    iOReadEof :: Bool,
    iOReadBase64Encoded :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  IORead where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 6 }


instance Command  IORead where
    commandName _ = "IO.read"

data PIORead = PIORead {
    pIOReadHandle :: IOStreamHandle,
    pIOReadOffset :: Maybe Int,
    pIOReadSize :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PIORead where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }

instance ToJSON PIORead  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}


iORead :: Handle Event -> PIORead -> IO (Either Error IORead)
iORead handle params = sendReceiveCommandResult handle "IO.read" (Just params)

data IOResolveBlob = IOResolveBlob {
    iOResolveBlobUuid :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  IOResolveBlob where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


instance Command  IOResolveBlob where
    commandName _ = "IO.resolveBlob"

data PIOResolveBlob = PIOResolveBlob {
    pIOResolveBlobObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PIOResolveBlob where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON PIOResolveBlob  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}


iOResolveBlob :: Handle Event -> PIOResolveBlob -> IO (Either Error IOResolveBlob)
iOResolveBlob handle params = sendReceiveCommandResult handle "IO.resolveBlob" (Just params)




data InputTouchPoint = InputTouchPoint {
    inputTouchPointX :: Double,
    inputTouchPointY :: Double,
    inputTouchPointRadiusX :: Maybe Double,
    inputTouchPointRadiusY :: Maybe Double,
    inputTouchPointRotationAngle :: Maybe Double,
    inputTouchPointForce :: Maybe Double,
    inputTouchPointId :: Maybe Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  InputTouchPoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON InputTouchPoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}



data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
    deriving (Eq, Show, Read, Generic)
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



type InputTimeSinceEpoch = Double


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PInputDispatchKeyEvent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PInputDispatchKeyEvent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


inputDispatchKeyEvent :: Handle Event -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent handle params = sendReceiveCommand handle "Input.dispatchKeyEvent" (Just params)



data PInputDispatchMouseEvent = PInputDispatchMouseEvent {
    pInputDispatchMouseEventType :: String,
    pInputDispatchMouseEventX :: Double,
    pInputDispatchMouseEventY :: Double,
    pInputDispatchMouseEventModifiers :: Maybe Int,
    pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
    pInputDispatchMouseEventButton :: Maybe InputMouseButton,
    pInputDispatchMouseEventButtons :: Maybe Int,
    pInputDispatchMouseEventClickCount :: Maybe Int,
    pInputDispatchMouseEventDeltaX :: Maybe Double,
    pInputDispatchMouseEventDeltaY :: Maybe Double,
    pInputDispatchMouseEventPointerType :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PInputDispatchMouseEvent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PInputDispatchMouseEvent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


inputDispatchMouseEvent :: Handle Event -> PInputDispatchMouseEvent -> IO (Maybe Error)
inputDispatchMouseEvent handle params = sendReceiveCommand handle "Input.dispatchMouseEvent" (Just params)



data PInputDispatchTouchEvent = PInputDispatchTouchEvent {
    pInputDispatchTouchEventType :: String,
    pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
    pInputDispatchTouchEventModifiers :: Maybe Int,
    pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PInputDispatchTouchEvent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PInputDispatchTouchEvent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


inputDispatchTouchEvent :: Handle Event -> PInputDispatchTouchEvent -> IO (Maybe Error)
inputDispatchTouchEvent handle params = sendReceiveCommand handle "Input.dispatchTouchEvent" (Just params)



data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
    pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PInputSetIgnoreInputEvents where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON PInputSetIgnoreInputEvents  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


inputSetIgnoreInputEvents :: Handle Event -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents handle params = sendReceiveCommand handle "Input.setIgnoreInputEvents" (Just params)



data LogEntryAdded = LogEntryAdded {
    logEntryAddedEntry :: LogLogEntry
} deriving (Eq, Show, Read, Generic)
instance FromJSON  LogEntryAdded where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance ToJSON LogEntryAdded  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  LogLogEntry where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }

instance ToJSON LogLogEntry  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}



data LogViolationSetting = LogViolationSetting {
    logViolationSettingName :: String,
    logViolationSettingThreshold :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  LogViolationSetting where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON LogViolationSetting  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}





logClear :: Handle Event -> IO (Maybe Error)
logClear handle = sendReceiveCommand handle "Log.clear" (Nothing :: Maybe ())




logDisable :: Handle Event -> IO (Maybe Error)
logDisable handle = sendReceiveCommand handle "Log.disable" (Nothing :: Maybe ())




logEnable :: Handle Event -> IO (Maybe Error)
logEnable handle = sendReceiveCommand handle "Log.enable" (Nothing :: Maybe ())



data PLogStartViolationsReport = PLogStartViolationsReport {
    pLogStartViolationsReportConfig :: [LogViolationSetting]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PLogStartViolationsReport where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON PLogStartViolationsReport  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


logStartViolationsReport :: Handle Event -> PLogStartViolationsReport -> IO (Maybe Error)
logStartViolationsReport handle params = sendReceiveCommand handle "Log.startViolationsReport" (Just params)




logStopViolationsReport :: Handle Event -> IO (Maybe Error)
logStopViolationsReport handle = sendReceiveCommand handle "Log.stopViolationsReport" (Nothing :: Maybe ())



data NetworkDataReceived = NetworkDataReceived {
    networkDataReceivedRequestId :: NetworkRequestId,
    networkDataReceivedTimestamp :: NetworkMonotonicTime,
    networkDataReceivedDataLength :: Int,
    networkDataReceivedEncodedDataLength :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkDataReceived where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON NetworkDataReceived  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


instance FromEvent Event NetworkDataReceived where
    eventName  _ _    =  "Network.dataReceived"
    fromEvent ev =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing

data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
    networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
    networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
    networkEventSourceMessageReceivedEventName :: String,
    networkEventSourceMessageReceivedEventId :: String,
    networkEventSourceMessageReceivedData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkEventSourceMessageReceived where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance ToJSON NetworkEventSourceMessageReceived  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkLoadingFailed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON NetworkLoadingFailed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


instance FromEvent Event NetworkLoadingFailed where
    eventName  _ _    =  "Network.loadingFailed"
    fromEvent ev =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing

data NetworkLoadingFinished = NetworkLoadingFinished {
    networkLoadingFinishedRequestId :: NetworkRequestId,
    networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
    networkLoadingFinishedEncodedDataLength :: Double,
    networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkLoadingFinished where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON NetworkLoadingFinished  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


instance FromEvent Event NetworkLoadingFinished where
    eventName  _ _    =  "Network.loadingFinished"
    fromEvent ev =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing

data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
    networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkRequestServedFromCache where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON NetworkRequestServedFromCache  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


instance FromEvent Event NetworkRequestServedFromCache where
    eventName  _ _    =  "Network.requestServedFromCache"
    fromEvent ev =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing

data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
    networkRequestWillBeSentRequestId :: NetworkRequestId,
    networkRequestWillBeSentLoaderId :: NetworkLoaderId,
    networkRequestWillBeSentDocumentURL :: String,
    networkRequestWillBeSentRequest :: NetworkRequest,
    networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
    networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
    networkRequestWillBeSentInitiator :: NetworkInitiator,
    networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
    networkRequestWillBeSentType :: Maybe NetworkResourceType,
    networkRequestWillBeSentFrameId :: Maybe PageFrameId,
    networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkRequestWillBeSent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON NetworkRequestWillBeSent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkResponseReceived where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON NetworkResponseReceived  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


instance FromEvent Event NetworkResponseReceived where
    eventName  _ _    =  "Network.responseReceived"
    fromEvent ev =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing

data NetworkWebSocketClosed = NetworkWebSocketClosed {
    networkWebSocketClosedRequestId :: NetworkRequestId,
    networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketClosed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON NetworkWebSocketClosed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketClosed where
    eventName  _ _    =  "Network.webSocketClosed"
    fromEvent ev =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing

data NetworkWebSocketCreated = NetworkWebSocketCreated {
    networkWebSocketCreatedRequestId :: NetworkRequestId,
    networkWebSocketCreatedUrl :: String,
    networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketCreated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON NetworkWebSocketCreated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketCreated where
    eventName  _ _    =  "Network.webSocketCreated"
    fromEvent ev =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing

data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
    networkWebSocketFrameErrorRequestId :: NetworkRequestId,
    networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameErrorErrorMessage :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketFrameError where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON NetworkWebSocketFrameError  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketFrameError where
    eventName  _ _    =  "Network.webSocketFrameError"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing

data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
    networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
    networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketFrameReceived where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON NetworkWebSocketFrameReceived  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketFrameReceived where
    eventName  _ _    =  "Network.webSocketFrameReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing

data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
    networkWebSocketFrameSentRequestId :: NetworkRequestId,
    networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketFrameSent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON NetworkWebSocketFrameSent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketFrameSent where
    eventName  _ _    =  "Network.webSocketFrameSent"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing

data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
    networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
    networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }

instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketHandshakeResponseReceived where
    eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing

data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
    networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
    networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
    networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
    networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }

instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebSocketWillSendHandshakeRequest where
    eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
    fromEvent ev =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing

data NetworkWebTransportCreated = NetworkWebTransportCreated {
    networkWebTransportCreatedTransportId :: NetworkRequestId,
    networkWebTransportCreatedUrl :: String,
    networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
    networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebTransportCreated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON NetworkWebTransportCreated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebTransportCreated where
    eventName  _ _    =  "Network.webTransportCreated"
    fromEvent ev =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing

data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
    networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
    networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebTransportConnectionEstablished where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }

instance ToJSON NetworkWebTransportConnectionEstablished  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebTransportConnectionEstablished where
    eventName  _ _    =  "Network.webTransportConnectionEstablished"
    fromEvent ev =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing

data NetworkWebTransportClosed = NetworkWebTransportClosed {
    networkWebTransportClosedTransportId :: NetworkRequestId,
    networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebTransportClosed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON NetworkWebTransportClosed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


instance FromEvent Event NetworkWebTransportClosed where
    eventName  _ _    =  "Network.webTransportClosed"
    fromEvent ev =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing


data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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



type NetworkTimeSinceEpoch = Double

type NetworkMonotonicTime = Double

type NetworkHeaders = [(String, String)]

data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
    networkResourceTimingRequestTime :: Double,
    networkResourceTimingProxyStart :: Double,
    networkResourceTimingProxyEnd :: Double,
    networkResourceTimingDnsStart :: Double,
    networkResourceTimingDnsEnd :: Double,
    networkResourceTimingConnectStart :: Double,
    networkResourceTimingConnectEnd :: Double,
    networkResourceTimingSslStart :: Double,
    networkResourceTimingSslEnd :: Double,
    networkResourceTimingSendStart :: Double,
    networkResourceTimingSendEnd :: Double,
    networkResourceTimingReceiveHeadersEnd :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkResourceTiming where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON NetworkResourceTiming  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkPostDataEntry where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON NetworkPostDataEntry  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON NetworkRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
    networkSignedCertificateTimestampStatus :: String,
    networkSignedCertificateTimestampOrigin :: String,
    networkSignedCertificateTimestampLogDescription :: String,
    networkSignedCertificateTimestampLogId :: String,
    networkSignedCertificateTimestampTimestamp :: Double,
    networkSignedCertificateTimestampHashAlgorithm :: String,
    networkSignedCertificateTimestampSignatureAlgorithm :: String,
    networkSignedCertificateTimestampSignatureData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkSignedCertificateTimestamp where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance ToJSON NetworkSignedCertificateTimestamp  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkSecurityDetails where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON NetworkSecurityDetails  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}



data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkCorsErrorStatus where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON NetworkCorsErrorStatus  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}



data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
    deriving (Eq, Show, Read, Generic)
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
    networkResponseConnectionId :: Double,
    networkResponseEncodedDataLength :: Double,
    networkResponseSecurityState :: SecuritySecurityState,
    networkResponseRequestHeaders :: Maybe NetworkHeaders,
    networkResponseRemoteIPAddress :: Maybe String,
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkResponse where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON NetworkResponse  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}



data NetworkWebSocketRequest = NetworkWebSocketRequest {
    networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON NetworkWebSocketRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}



data NetworkWebSocketResponse = NetworkWebSocketResponse {
    networkWebSocketResponseStatus :: Int,
    networkWebSocketResponseStatusText :: String,
    networkWebSocketResponseHeaders :: NetworkHeaders,
    networkWebSocketResponseHeadersText :: Maybe String,
    networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
    networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketResponse where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON NetworkWebSocketResponse  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}



data NetworkWebSocketFrame = NetworkWebSocketFrame {
    networkWebSocketFrameOpcode :: Double,
    networkWebSocketFrameMask :: Bool,
    networkWebSocketFramePayloadData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkWebSocketFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON NetworkWebSocketFrame  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



data NetworkCachedResource = NetworkCachedResource {
    networkCachedResourceUrl :: String,
    networkCachedResourceType :: NetworkResourceType,
    networkCachedResourceBodySize :: Double,
    networkCachedResourceResponse :: Maybe NetworkResponse
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkCachedResource where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON NetworkCachedResource  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



data NetworkInitiator = NetworkInitiator {
    networkInitiatorType :: String,
    networkInitiatorStack :: Maybe RuntimeStackTrace,
    networkInitiatorUrl :: Maybe String,
    networkInitiatorLineNumber :: Maybe Double,
    networkInitiatorColumnNumber :: Maybe Double,
    networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkInitiator where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON NetworkInitiator  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}



data NetworkCookie = NetworkCookie {
    networkCookieName :: String,
    networkCookieValue :: String,
    networkCookieDomain :: String,
    networkCookiePath :: String,
    networkCookieExpires :: Double,
    networkCookieSize :: Int,
    networkCookieHttpOnly :: Bool,
    networkCookieSecure :: Bool,
    networkCookieSession :: Bool,
    networkCookieSameSite :: Maybe NetworkCookieSameSite
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkCookie where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance ToJSON NetworkCookie  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkCookieParam where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON NetworkCookieParam  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}





networkClearBrowserCache :: Handle Event -> IO (Maybe Error)
networkClearBrowserCache handle = sendReceiveCommand handle "Network.clearBrowserCache" (Nothing :: Maybe ())




networkClearBrowserCookies :: Handle Event -> IO (Maybe Error)
networkClearBrowserCookies handle = sendReceiveCommand handle "Network.clearBrowserCookies" (Nothing :: Maybe ())



data PNetworkDeleteCookies = PNetworkDeleteCookies {
    pNetworkDeleteCookiesName :: String,
    pNetworkDeleteCookiesUrl :: Maybe String,
    pNetworkDeleteCookiesDomain :: Maybe String,
    pNetworkDeleteCookiesPath :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkDeleteCookies where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PNetworkDeleteCookies  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


networkDeleteCookies :: Handle Event -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies handle params = sendReceiveCommand handle "Network.deleteCookies" (Just params)




networkDisable :: Handle Event -> IO (Maybe Error)
networkDisable handle = sendReceiveCommand handle "Network.disable" (Nothing :: Maybe ())



data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
    pNetworkEmulateNetworkConditionsOffline :: Bool,
    pNetworkEmulateNetworkConditionsLatency :: Double,
    pNetworkEmulateNetworkConditionsDownloadThroughput :: Double,
    pNetworkEmulateNetworkConditionsUploadThroughput :: Double,
    pNetworkEmulateNetworkConditionsConnectionType :: Maybe NetworkConnectionType
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkEmulateNetworkConditions where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance ToJSON PNetworkEmulateNetworkConditions  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}


networkEmulateNetworkConditions :: Handle Event -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions handle params = sendReceiveCommand handle "Network.emulateNetworkConditions" (Just params)



data PNetworkEnable = PNetworkEnable {
    pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkEnable where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON PNetworkEnable  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}


networkEnable :: Handle Event -> PNetworkEnable -> IO (Maybe Error)
networkEnable handle params = sendReceiveCommand handle "Network.enable" (Just params)

data NetworkGetAllCookies = NetworkGetAllCookies {
    networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkGetAllCookies where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  NetworkGetAllCookies where
    commandName _ = "Network.getAllCookies"


networkGetAllCookies :: Handle Event -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies handle = sendReceiveCommandResult handle "Network.getAllCookies" (Nothing :: Maybe ())

data NetworkGetCookies = NetworkGetCookies {
    networkGetCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkGetCookies where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command  NetworkGetCookies where
    commandName _ = "Network.getCookies"

data PNetworkGetCookies = PNetworkGetCookies {
    pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkGetCookies where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PNetworkGetCookies  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


networkGetCookies :: Handle Event -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies handle params = sendReceiveCommandResult handle "Network.getCookies" (Just params)

data NetworkGetResponseBody = NetworkGetResponseBody {
    networkGetResponseBodyBody :: String,
    networkGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkGetResponseBody where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Command  NetworkGetResponseBody where
    commandName _ = "Network.getResponseBody"

data PNetworkGetResponseBody = PNetworkGetResponseBody {
    pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkGetResponseBody where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON PNetworkGetResponseBody  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


networkGetResponseBody :: Handle Event -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody handle params = sendReceiveCommandResult handle "Network.getResponseBody" (Just params)

data NetworkGetRequestPostData = NetworkGetRequestPostData {
    networkGetRequestPostDataPostData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  NetworkGetRequestPostData where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command  NetworkGetRequestPostData where
    commandName _ = "Network.getRequestPostData"

data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
    pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkGetRequestPostData where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON PNetworkGetRequestPostData  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


networkGetRequestPostData :: Handle Event -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData handle params = sendReceiveCommandResult handle "Network.getRequestPostData" (Just params)



data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
    pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkSetCacheDisabled where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PNetworkSetCacheDisabled  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


networkSetCacheDisabled :: Handle Event -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled handle params = sendReceiveCommand handle "Network.setCacheDisabled" (Just params)



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkSetCookie where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PNetworkSetCookie  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


networkSetCookie :: Handle Event -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie handle params = sendReceiveCommand handle "Network.setCookie" (Just params)



data PNetworkSetCookies = PNetworkSetCookies {
    pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkSetCookies where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PNetworkSetCookies  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


networkSetCookies :: Handle Event -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies handle params = sendReceiveCommand handle "Network.setCookies" (Just params)



data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
    pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkSetExtraHttpHeaders where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PNetworkSetExtraHttpHeaders  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


networkSetExtraHttpHeaders :: Handle Event -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders handle params = sendReceiveCommand handle "Network.setExtraHTTPHeaders" (Just params)



data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
    pNetworkSetUserAgentOverrideUserAgent :: String,
    pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pNetworkSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PNetworkSetUserAgentOverride where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance ToJSON PNetworkSetUserAgentOverride  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}


networkSetUserAgentOverride :: Handle Event -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)



data PageDomContentEventFired = PageDomContentEventFired {
    pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageDomContentEventFired where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PageDomContentEventFired  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


instance FromEvent Event PageDomContentEventFired where
    eventName  _ _    =  "Page.domContentEventFired"
    fromEvent ev =  case ev of EVPageDomContentEventFired v -> Just v; _ -> Nothing

data PageFileChooserOpened = PageFileChooserOpened {
    pageFileChooserOpenedMode :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFileChooserOpened where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PageFileChooserOpened  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


instance FromEvent Event PageFileChooserOpened where
    eventName  _ _    =  "Page.fileChooserOpened"
    fromEvent ev =  case ev of EVPageFileChooserOpened v -> Just v; _ -> Nothing

data PageFrameAttached = PageFrameAttached {
    pageFrameAttachedFrameId :: PageFrameId,
    pageFrameAttachedParentFrameId :: PageFrameId,
    pageFrameAttachedStack :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFrameAttached where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PageFrameAttached  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


instance FromEvent Event PageFrameAttached where
    eventName  _ _    =  "Page.frameAttached"
    fromEvent ev =  case ev of EVPageFrameAttached v -> Just v; _ -> Nothing

data PageFrameDetached = PageFrameDetached {
    pageFrameDetachedFrameId :: PageFrameId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFrameDetached where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PageFrameDetached  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


instance FromEvent Event PageFrameDetached where
    eventName  _ _    =  "Page.frameDetached"
    fromEvent ev =  case ev of EVPageFrameDetached v -> Just v; _ -> Nothing

data PageFrameNavigated = PageFrameNavigated {
    pageFrameNavigatedFrame :: PageFrame
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFrameNavigated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PageFrameNavigated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageJavascriptDialogClosed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON PageJavascriptDialogClosed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


instance FromEvent Event PageJavascriptDialogClosed where
    eventName  _ _    =  "Page.javascriptDialogClosed"
    fromEvent ev =  case ev of EVPageJavascriptDialogClosed v -> Just v; _ -> Nothing

data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
    pageJavascriptDialogOpeningUrl :: String,
    pageJavascriptDialogOpeningMessage :: String,
    pageJavascriptDialogOpeningType :: PageDialogType,
    pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
    pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageJavascriptDialogOpening where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PageJavascriptDialogOpening  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


instance FromEvent Event PageJavascriptDialogOpening where
    eventName  _ _    =  "Page.javascriptDialogOpening"
    fromEvent ev =  case ev of EVPageJavascriptDialogOpening v -> Just v; _ -> Nothing

data PageLifecycleEvent = PageLifecycleEvent {
    pageLifecycleEventFrameId :: PageFrameId,
    pageLifecycleEventLoaderId :: NetworkLoaderId,
    pageLifecycleEventName :: String,
    pageLifecycleEventTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageLifecycleEvent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PageLifecycleEvent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


instance FromEvent Event PageLifecycleEvent where
    eventName  _ _    =  "Page.lifecycleEvent"
    fromEvent ev =  case ev of EVPageLifecycleEvent v -> Just v; _ -> Nothing

data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
    pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
    pagePrerenderAttemptCompletedPrerenderingUrl :: String,
    pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PagePrerenderAttemptCompleted where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON PagePrerenderAttemptCompleted  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


instance FromEvent Event PagePrerenderAttemptCompleted where
    eventName  _ _    =  "Page.prerenderAttemptCompleted"
    fromEvent ev =  case ev of EVPagePrerenderAttemptCompleted v -> Just v; _ -> Nothing

data PageLoadEventFired = PageLoadEventFired {
    pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageLoadEventFired where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PageLoadEventFired  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


instance FromEvent Event PageLoadEventFired where
    eventName  _ _    =  "Page.loadEventFired"
    fromEvent ev =  case ev of EVPageLoadEventFired v -> Just v; _ -> Nothing

data PageWindowOpen = PageWindowOpen {
    pageWindowOpenUrl :: String,
    pageWindowOpenWindowName :: String,
    pageWindowOpenWindowFeatures :: [String],
    pageWindowOpenUserGesture :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageWindowOpen where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON PageWindowOpen  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance ToJSON PageFrame  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}



data PageFrameTree = PageFrameTree {
    pageFrameTreeFrame :: PageFrame,
    pageFrameTreeChildFrames :: Maybe [PageFrameTree]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageFrameTree where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance ToJSON PageFrameTree  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}



type PageScriptIdentifier = String

data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddressBar | PageTransitionTypeAutoBookmark | PageTransitionTypeAutoSubframe | PageTransitionTypeManualSubframe | PageTransitionTypeGenerated | PageTransitionTypeAutoToplevel | PageTransitionTypeFormSubmit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeywordGenerated | PageTransitionTypeOther
    deriving (Eq, Show, Read, Generic)
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
    pageNavigationEntryUserTypedURL :: String,
    pageNavigationEntryTitle :: String,
    pageNavigationEntryTransitionType :: PageTransitionType
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageNavigationEntry where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON PageNavigationEntry  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageAppManifestError where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON PageAppManifestError  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}



data PageLayoutViewport = PageLayoutViewport {
    pageLayoutViewportPageX :: Int,
    pageLayoutViewportPageY :: Int,
    pageLayoutViewportClientWidth :: Int,
    pageLayoutViewportClientHeight :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageLayoutViewport where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PageLayoutViewport  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}



data PageVisualViewport = PageVisualViewport {
    pageVisualViewportOffsetX :: Double,
    pageVisualViewportOffsetY :: Double,
    pageVisualViewportPageX :: Double,
    pageVisualViewportPageY :: Double,
    pageVisualViewportClientWidth :: Double,
    pageVisualViewportClientHeight :: Double,
    pageVisualViewportScale :: Double,
    pageVisualViewportZoom :: Maybe Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageVisualViewport where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PageVisualViewport  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}



data PageViewport = PageViewport {
    pageViewportX :: Double,
    pageViewportY :: Double,
    pageViewportWidth :: Double,
    pageViewportHeight :: Double,
    pageViewportScale :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageViewport where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance ToJSON PageViewport  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}



data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Command  PageAddScriptToEvaluateOnNewDocument where
    commandName _ = "Page.addScriptToEvaluateOnNewDocument"

data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
    pPageAddScriptToEvaluateOnNewDocumentSource :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}


pageAddScriptToEvaluateOnNewDocument :: Handle Event -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument handle params = sendReceiveCommandResult handle "Page.addScriptToEvaluateOnNewDocument" (Just params)




pageBringToFront :: Handle Event -> IO (Maybe Error)
pageBringToFront handle = sendReceiveCommand handle "Page.bringToFront" (Nothing :: Maybe ())

data PageCaptureScreenshot = PageCaptureScreenshot {
    pageCaptureScreenshotData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageCaptureScreenshot where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command  PageCaptureScreenshot where
    commandName _ = "Page.captureScreenshot"

data PPageCaptureScreenshot = PPageCaptureScreenshot {
    pPageCaptureScreenshotFormat :: Maybe String,
    pPageCaptureScreenshotQuality :: Maybe Int,
    pPageCaptureScreenshotClip :: Maybe PageViewport
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageCaptureScreenshot where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PPageCaptureScreenshot  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


pageCaptureScreenshot :: Handle Event -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot handle params = sendReceiveCommandResult handle "Page.captureScreenshot" (Just params)

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
    pageCreateIsolatedWorldExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageCreateIsolatedWorld where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command  PageCreateIsolatedWorld where
    commandName _ = "Page.createIsolatedWorld"

data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
    pPageCreateIsolatedWorldFrameId :: PageFrameId,
    pPageCreateIsolatedWorldWorldName :: Maybe String,
    pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageCreateIsolatedWorld where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PPageCreateIsolatedWorld  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


pageCreateIsolatedWorld :: Handle Event -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld handle params = sendReceiveCommandResult handle "Page.createIsolatedWorld" (Just params)




pageDisable :: Handle Event -> IO (Maybe Error)
pageDisable handle = sendReceiveCommand handle "Page.disable" (Nothing :: Maybe ())




pageEnable :: Handle Event -> IO (Maybe Error)
pageEnable handle = sendReceiveCommand handle "Page.enable" (Nothing :: Maybe ())

data PageGetAppManifest = PageGetAppManifest {
    pageGetAppManifestUrl :: String,
    pageGetAppManifestErrors :: [PageAppManifestError],
    pageGetAppManifestData :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageGetAppManifest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Command  PageGetAppManifest where
    commandName _ = "Page.getAppManifest"


pageGetAppManifest :: Handle Event -> IO (Either Error PageGetAppManifest)
pageGetAppManifest handle = sendReceiveCommandResult handle "Page.getAppManifest" (Nothing :: Maybe ())

data PageGetFrameTree = PageGetFrameTree {
    pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageGetFrameTree where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  PageGetFrameTree where
    commandName _ = "Page.getFrameTree"


pageGetFrameTree :: Handle Event -> IO (Either Error PageGetFrameTree)
pageGetFrameTree handle = sendReceiveCommandResult handle "Page.getFrameTree" (Nothing :: Maybe ())

data PageGetLayoutMetrics = PageGetLayoutMetrics {
    pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
    pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
    pageGetLayoutMetricsCssContentSize :: DOMRect
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageGetLayoutMetrics where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  PageGetLayoutMetrics where
    commandName _ = "Page.getLayoutMetrics"


pageGetLayoutMetrics :: Handle Event -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics handle = sendReceiveCommandResult handle "Page.getLayoutMetrics" (Nothing :: Maybe ())

data PageGetNavigationHistory = PageGetNavigationHistory {
    pageGetNavigationHistoryCurrentIndex :: Int,
    pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageGetNavigationHistory where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command  PageGetNavigationHistory where
    commandName _ = "Page.getNavigationHistory"


pageGetNavigationHistory :: Handle Event -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory handle = sendReceiveCommandResult handle "Page.getNavigationHistory" (Nothing :: Maybe ())




pageResetNavigationHistory :: Handle Event -> IO (Maybe Error)
pageResetNavigationHistory handle = sendReceiveCommand handle "Page.resetNavigationHistory" (Nothing :: Maybe ())



data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
    pPageHandleJavaScriptDialogAccept :: Bool,
    pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageHandleJavaScriptDialog where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PPageHandleJavaScriptDialog  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


pageHandleJavaScriptDialog :: Handle Event -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog handle params = sendReceiveCommand handle "Page.handleJavaScriptDialog" (Just params)

data PageNavigate = PageNavigate {
    pageNavigateFrameId :: PageFrameId,
    pageNavigateLoaderId :: Maybe NetworkLoaderId,
    pageNavigateErrorText :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PageNavigate where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


instance Command  PageNavigate where
    commandName _ = "Page.navigate"

data PPageNavigate = PPageNavigate {
    pPageNavigateUrl :: String,
    pPageNavigateReferrer :: Maybe String,
    pPageNavigateTransitionType :: Maybe PageTransitionType,
    pPageNavigateFrameId :: Maybe PageFrameId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageNavigate where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance ToJSON PPageNavigate  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}


pageNavigate :: Handle Event -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate handle params = sendReceiveCommandResult handle "Page.navigate" (Just params)



data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
    pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageNavigateToHistoryEntry where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PPageNavigateToHistoryEntry  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


pageNavigateToHistoryEntry :: Handle Event -> PPageNavigateToHistoryEntry -> IO (Maybe Error)
pageNavigateToHistoryEntry handle params = sendReceiveCommand handle "Page.navigateToHistoryEntry" (Just params)

data PagePrintToPdf = PagePrintToPdf {
    pagePrintToPdfData :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PagePrintToPdf where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command  PagePrintToPdf where
    commandName _ = "Page.printToPDF"

data PPagePrintToPdf = PPagePrintToPdf {
    pPagePrintToPdfLandscape :: Maybe Bool,
    pPagePrintToPdfDisplayHeaderFooter :: Maybe Bool,
    pPagePrintToPdfPrintBackground :: Maybe Bool,
    pPagePrintToPdfScale :: Maybe Double,
    pPagePrintToPdfPaperWidth :: Maybe Double,
    pPagePrintToPdfPaperHeight :: Maybe Double,
    pPagePrintToPdfMarginTop :: Maybe Double,
    pPagePrintToPdfMarginBottom :: Maybe Double,
    pPagePrintToPdfMarginLeft :: Maybe Double,
    pPagePrintToPdfMarginRight :: Maybe Double,
    pPagePrintToPdfPageRanges :: Maybe String,
    pPagePrintToPdfHeaderTemplate :: Maybe String,
    pPagePrintToPdfFooterTemplate :: Maybe String,
    pPagePrintToPdfPreferCSSPageSize :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPagePrintToPdf where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PPagePrintToPdf  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


pagePrintToPdf :: Handle Event -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)



data PPageReload = PPageReload {
    pPageReloadIgnoreCache :: Maybe Bool,
    pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageReload where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }

instance ToJSON PPageReload  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}


pageReload :: Handle Event -> PPageReload -> IO (Maybe Error)
pageReload handle params = sendReceiveCommand handle "Page.reload" (Just params)



data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
    pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }

instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}


pageRemoveScriptToEvaluateOnNewDocument :: Handle Event -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument handle params = sendReceiveCommand handle "Page.removeScriptToEvaluateOnNewDocument" (Just params)



data PPageSetDocumentContent = PPageSetDocumentContent {
    pPageSetDocumentContentFrameId :: PageFrameId,
    pPageSetDocumentContentHtml :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPageSetDocumentContent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON PPageSetDocumentContent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


pageSetDocumentContent :: Handle Event -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent handle params = sendReceiveCommand handle "Page.setDocumentContent" (Just params)




pageStopLoading :: Handle Event -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())



data PerformanceMetrics = PerformanceMetrics {
    performanceMetricsMetrics :: [PerformanceMetric],
    performanceMetricsTitle :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PerformanceMetrics where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PerformanceMetrics  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


instance FromEvent Event PerformanceMetrics where
    eventName  _ _    =  "Performance.metrics"
    fromEvent ev =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing


data PerformanceMetric = PerformanceMetric {
    performanceMetricName :: String,
    performanceMetricValue :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PerformanceMetric where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PerformanceMetric  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}





performanceDisable :: Handle Event -> IO (Maybe Error)
performanceDisable handle = sendReceiveCommand handle "Performance.disable" (Nothing :: Maybe ())



data PPerformanceEnable = PPerformanceEnable {
    pPerformanceEnableTimeDomain :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PPerformanceEnable where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PPerformanceEnable  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


performanceEnable :: Handle Event -> PPerformanceEnable -> IO (Maybe Error)
performanceEnable handle params = sendReceiveCommand handle "Performance.enable" (Just params)

data PerformanceGetMetrics = PerformanceGetMetrics {
    performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PerformanceGetMetrics where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command  PerformanceGetMetrics where
    commandName _ = "Performance.getMetrics"


performanceGetMetrics :: Handle Event -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics handle = sendReceiveCommandResult handle "Performance.getMetrics" (Nothing :: Maybe ())




type SecurityCertificateId = Int

data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
    deriving (Eq, Show, Read, Generic)
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
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  SecuritySecurityStateExplanation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance ToJSON SecuritySecurityStateExplanation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}



data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
    deriving (Eq, Show, Read, Generic)
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





securityDisable :: Handle Event -> IO (Maybe Error)
securityDisable handle = sendReceiveCommand handle "Security.disable" (Nothing :: Maybe ())




securityEnable :: Handle Event -> IO (Maybe Error)
securityEnable handle = sendReceiveCommand handle "Security.enable" (Nothing :: Maybe ())



data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
    targetReceivedMessageFromTargetSessionId :: TargetSessionID,
    targetReceivedMessageFromTargetMessage :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetReceivedMessageFromTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON TargetReceivedMessageFromTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


instance FromEvent Event TargetReceivedMessageFromTarget where
    eventName  _ _    =  "Target.receivedMessageFromTarget"
    fromEvent ev =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing

data TargetTargetCreated = TargetTargetCreated {
    targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetTargetCreated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON TargetTargetCreated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


instance FromEvent Event TargetTargetCreated where
    eventName  _ _    =  "Target.targetCreated"
    fromEvent ev =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing

data TargetTargetDestroyed = TargetTargetDestroyed {
    targetTargetDestroyedTargetId :: TargetTargetID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetTargetDestroyed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON TargetTargetDestroyed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


instance FromEvent Event TargetTargetDestroyed where
    eventName  _ _    =  "Target.targetDestroyed"
    fromEvent ev =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing

data TargetTargetCrashed = TargetTargetCrashed {
    targetTargetCrashedTargetId :: TargetTargetID,
    targetTargetCrashedStatus :: String,
    targetTargetCrashedErrorCode :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetTargetCrashed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON TargetTargetCrashed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


instance FromEvent Event TargetTargetCrashed where
    eventName  _ _    =  "Target.targetCrashed"
    fromEvent ev =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing

data TargetTargetInfoChanged = TargetTargetInfoChanged {
    targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetTargetInfoChanged where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON TargetTargetInfoChanged  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetTargetInfo where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON TargetTargetInfo  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}




data PTargetActivateTarget = PTargetActivateTarget {
    pTargetActivateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetActivateTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PTargetActivateTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


targetActivateTarget :: Handle Event -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget handle params = sendReceiveCommand handle "Target.activateTarget" (Just params)

data TargetAttachToTarget = TargetAttachToTarget {
    targetAttachToTargetSessionId :: TargetSessionID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetAttachToTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  TargetAttachToTarget where
    commandName _ = "Target.attachToTarget"

data PTargetAttachToTarget = PTargetAttachToTarget {
    pTargetAttachToTargetTargetId :: TargetTargetID,
    pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetAttachToTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PTargetAttachToTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


targetAttachToTarget :: Handle Event -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget handle params = sendReceiveCommandResult handle "Target.attachToTarget" (Just params)



data PTargetCloseTarget = PTargetCloseTarget {
    pTargetCloseTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetCloseTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON PTargetCloseTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


targetCloseTarget :: Handle Event -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget handle params = sendReceiveCommand handle "Target.closeTarget" (Just params)

data TargetCreateTarget = TargetCreateTarget {
    targetCreateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetCreateTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Command  TargetCreateTarget where
    commandName _ = "Target.createTarget"

data PTargetCreateTarget = PTargetCreateTarget {
    pTargetCreateTargetUrl :: String,
    pTargetCreateTargetWidth :: Maybe Int,
    pTargetCreateTargetHeight :: Maybe Int,
    pTargetCreateTargetNewWindow :: Maybe Bool,
    pTargetCreateTargetBackground :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetCreateTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON PTargetCreateTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


targetCreateTarget :: Handle Event -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget handle params = sendReceiveCommandResult handle "Target.createTarget" (Just params)



data PTargetDetachFromTarget = PTargetDetachFromTarget {
    pTargetDetachFromTargetSessionId :: Maybe TargetSessionID
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetDetachFromTarget where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON PTargetDetachFromTarget  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


targetDetachFromTarget :: Handle Event -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget handle params = sendReceiveCommand handle "Target.detachFromTarget" (Just params)

data TargetGetTargets = TargetGetTargets {
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  TargetGetTargets where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  TargetGetTargets where
    commandName _ = "Target.getTargets"


targetGetTargets :: Handle Event -> IO (Either Error TargetGetTargets)
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())



data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
    pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PTargetSetDiscoverTargets where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON PTargetSetDiscoverTargets  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


targetSetDiscoverTargets :: Handle Event -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchRequestPaused where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON FetchRequestPaused  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}


instance FromEvent Event FetchRequestPaused where
    eventName  _ _    =  "Fetch.requestPaused"
    fromEvent ev =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing

data FetchAuthRequired = FetchAuthRequired {
    fetchAuthRequiredRequestId :: FetchRequestId,
    fetchAuthRequiredRequest :: NetworkRequest,
    fetchAuthRequiredFrameId :: PageFrameId,
    fetchAuthRequiredResourceType :: NetworkResourceType,
    fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchAuthRequired where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON FetchAuthRequired  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


instance FromEvent Event FetchAuthRequired where
    eventName  _ _    =  "Fetch.authRequired"
    fromEvent ev =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing


type FetchRequestId = String

data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
    deriving (Eq, Show, Read, Generic)
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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchRequestPattern where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON FetchRequestPattern  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



data FetchHeaderEntry = FetchHeaderEntry {
    fetchHeaderEntryName :: String,
    fetchHeaderEntryValue :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchHeaderEntry where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON FetchHeaderEntry  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}



data FetchAuthChallenge = FetchAuthChallenge {
    fetchAuthChallengeOrigin :: String,
    fetchAuthChallengeScheme :: String,
    fetchAuthChallengeRealm :: String,
    fetchAuthChallengeSource :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchAuthChallenge where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance ToJSON FetchAuthChallenge  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
    fetchAuthChallengeResponseResponse :: String,
    fetchAuthChallengeResponseUsername :: Maybe String,
    fetchAuthChallengeResponsePassword :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchAuthChallengeResponse where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON FetchAuthChallengeResponse  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}





fetchDisable :: Handle Event -> IO (Maybe Error)
fetchDisable handle = sendReceiveCommand handle "Fetch.disable" (Nothing :: Maybe ())



data PFetchEnable = PFetchEnable {
    pFetchEnablePatterns :: Maybe [FetchRequestPattern],
    pFetchEnableHandleAuthRequests :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchEnable where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance ToJSON PFetchEnable  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}


fetchEnable :: Handle Event -> PFetchEnable -> IO (Maybe Error)
fetchEnable handle params = sendReceiveCommand handle "Fetch.enable" (Just params)



data PFetchFailRequest = PFetchFailRequest {
    pFetchFailRequestRequestId :: FetchRequestId,
    pFetchFailRequestErrorReason :: NetworkErrorReason
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchFailRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PFetchFailRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


fetchFailRequest :: Handle Event -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest handle params = sendReceiveCommand handle "Fetch.failRequest" (Just params)



data PFetchFulfillRequest = PFetchFulfillRequest {
    pFetchFulfillRequestRequestId :: FetchRequestId,
    pFetchFulfillRequestResponseCode :: Int,
    pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
    pFetchFulfillRequestBinaryResponseHeaders :: Maybe String,
    pFetchFulfillRequestBody :: Maybe String,
    pFetchFulfillRequestResponsePhrase :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchFulfillRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON PFetchFulfillRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


fetchFulfillRequest :: Handle Event -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest handle params = sendReceiveCommand handle "Fetch.fulfillRequest" (Just params)



data PFetchContinueRequest = PFetchContinueRequest {
    pFetchContinueRequestRequestId :: FetchRequestId,
    pFetchContinueRequestUrl :: Maybe String,
    pFetchContinueRequestMethod :: Maybe String,
    pFetchContinueRequestPostData :: Maybe String,
    pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchContinueRequest where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PFetchContinueRequest  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


fetchContinueRequest :: Handle Event -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest handle params = sendReceiveCommand handle "Fetch.continueRequest" (Just params)



data PFetchContinueWithAuth = PFetchContinueWithAuth {
    pFetchContinueWithAuthRequestId :: FetchRequestId,
    pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchContinueWithAuth where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PFetchContinueWithAuth  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


fetchContinueWithAuth :: Handle Event -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth handle params = sendReceiveCommand handle "Fetch.continueWithAuth" (Just params)

data FetchGetResponseBody = FetchGetResponseBody {
    fetchGetResponseBodyBody :: String,
    fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchGetResponseBody where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  FetchGetResponseBody where
    commandName _ = "Fetch.getResponseBody"

data PFetchGetResponseBody = PFetchGetResponseBody {
    pFetchGetResponseBodyRequestId :: FetchRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchGetResponseBody where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PFetchGetResponseBody  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


fetchGetResponseBody :: Handle Event -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody handle params = sendReceiveCommandResult handle "Fetch.getResponseBody" (Just params)

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
    fetchTakeResponseBodyAsStreamStream :: IOStreamHandle
} deriving (Eq, Show, Read, Generic)
instance FromJSON  FetchTakeResponseBodyAsStream where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command  FetchTakeResponseBodyAsStream where
    commandName _ = "Fetch.takeResponseBodyAsStream"

data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
    pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PFetchTakeResponseBodyAsStream where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance ToJSON PFetchTakeResponseBodyAsStream  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}


fetchTakeResponseBodyAsStream :: Handle Event -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream handle params = sendReceiveCommandResult handle "Fetch.takeResponseBodyAsStream" (Just params)



data ConsoleMessageAdded = ConsoleMessageAdded {
    consoleMessageAddedMessage :: ConsoleConsoleMessage
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ConsoleMessageAdded where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON ConsoleMessageAdded  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ConsoleConsoleMessage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON ConsoleConsoleMessage  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}





consoleClearMessages :: Handle Event -> IO (Maybe Error)
consoleClearMessages handle = sendReceiveCommand handle "Console.clearMessages" (Nothing :: Maybe ())




consoleDisable :: Handle Event -> IO (Maybe Error)
consoleDisable handle = sendReceiveCommand handle "Console.disable" (Nothing :: Maybe ())




consoleEnable :: Handle Event -> IO (Maybe Error)
consoleEnable handle = sendReceiveCommand handle "Console.enable" (Nothing :: Maybe ())



data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
    debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
    debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerBreakpointResolved where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON DebuggerBreakpointResolved  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


instance FromEvent Event DebuggerBreakpointResolved where
    eventName  _ _    =  "Debugger.breakpointResolved"
    fromEvent ev =  case ev of EVDebuggerBreakpointResolved v -> Just v; _ -> Nothing

data DebuggerPaused = DebuggerPaused {
    debuggerPausedCallFrames :: [DebuggerCallFrame],
    debuggerPausedReason :: String,
    debuggerPausedData :: Maybe [(String, String)],
    debuggerPausedHitBreakpoints :: Maybe [String],
    debuggerPausedAsyncStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerPaused where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance ToJSON DebuggerPaused  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}


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
    debuggerScriptFailedToParseSourceMapURL :: Maybe String,
    debuggerScriptFailedToParseHasSourceURL :: Maybe Bool,
    debuggerScriptFailedToParseIsModule :: Maybe Bool,
    debuggerScriptFailedToParseLength :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerScriptFailedToParse where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON DebuggerScriptFailedToParse  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


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
    debuggerScriptParsedSourceMapURL :: Maybe String,
    debuggerScriptParsedHasSourceURL :: Maybe Bool,
    debuggerScriptParsedIsModule :: Maybe Bool,
    debuggerScriptParsedLength :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerScriptParsed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON DebuggerScriptParsed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


instance FromEvent Event DebuggerScriptParsed where
    eventName  _ _    =  "Debugger.scriptParsed"
    fromEvent ev =  case ev of EVDebuggerScriptParsed v -> Just v; _ -> Nothing


type DebuggerBreakpointId = String

type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
    debuggerLocationScriptId :: RuntimeScriptId,
    debuggerLocationLineNumber :: Int,
    debuggerLocationColumnNumber :: Maybe Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerLocation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON DebuggerLocation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}



data DebuggerCallFrame = DebuggerCallFrame {
    debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
    debuggerCallFrameFunctionName :: String,
    debuggerCallFrameLocation :: DebuggerLocation,
    debuggerCallFrameScopeChain :: [DebuggerScope],
    debuggerCallFrameThis :: RuntimeRemoteObject,
    debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
    debuggerCallFrameReturnValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerCallFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON DebuggerCallFrame  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}



data DebuggerScope = DebuggerScope {
    debuggerScopeType :: String,
    debuggerScopeObject :: RuntimeRemoteObject,
    debuggerScopeName :: Maybe String,
    debuggerScopeStartLocation :: Maybe DebuggerLocation,
    debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerScope where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance ToJSON DebuggerScope  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}



data DebuggerSearchMatch = DebuggerSearchMatch {
    debuggerSearchMatchLineNumber :: Double,
    debuggerSearchMatchLineContent :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSearchMatch where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON DebuggerSearchMatch  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



data DebuggerBreakLocation = DebuggerBreakLocation {
    debuggerBreakLocationScriptId :: RuntimeScriptId,
    debuggerBreakLocationLineNumber :: Int,
    debuggerBreakLocationColumnNumber :: Maybe Int,
    debuggerBreakLocationType :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerBreakLocation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON DebuggerBreakLocation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
    deriving (Eq, Show, Read, Generic)
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
    debuggerDebugSymbolsExternalURL :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerDebugSymbols where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON DebuggerDebugSymbols  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}




data PDebuggerContinueToLocation = PDebuggerContinueToLocation {
    pDebuggerContinueToLocationLocation :: DebuggerLocation,
    pDebuggerContinueToLocationTargetCallFrames :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerContinueToLocation where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PDebuggerContinueToLocation  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


debuggerContinueToLocation :: Handle Event -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation handle params = sendReceiveCommand handle "Debugger.continueToLocation" (Just params)




debuggerDisable :: Handle Event -> IO (Maybe Error)
debuggerDisable handle = sendReceiveCommand handle "Debugger.disable" (Nothing :: Maybe ())




debuggerEnable :: Handle Event -> IO (Maybe Error)
debuggerEnable handle = sendReceiveCommand handle "Debugger.enable" (Nothing :: Maybe ())

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
    debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerEvaluateOnCallFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command  DebuggerEvaluateOnCallFrame where
    commandName _ = "Debugger.evaluateOnCallFrame"

data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
    pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
    pDebuggerEvaluateOnCallFrameExpression :: String,
    pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
    pDebuggerEvaluateOnCallFrameIncludeCommandLineAPI :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerEvaluateOnCallFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance ToJSON PDebuggerEvaluateOnCallFrame  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}


debuggerEvaluateOnCallFrame :: Handle Event -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame handle params = sendReceiveCommandResult handle "Debugger.evaluateOnCallFrame" (Just params)

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerGetPossibleBreakpoints where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command  DebuggerGetPossibleBreakpoints where
    commandName _ = "Debugger.getPossibleBreakpoints"

data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
    pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
    pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
    pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerGetPossibleBreakpoints where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON PDebuggerGetPossibleBreakpoints  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


debuggerGetPossibleBreakpoints :: Handle Event -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints handle params = sendReceiveCommandResult handle "Debugger.getPossibleBreakpoints" (Just params)

data DebuggerGetScriptSource = DebuggerGetScriptSource {
    debuggerGetScriptSourceScriptSource :: String,
    debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerGetScriptSource where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command  DebuggerGetScriptSource where
    commandName _ = "Debugger.getScriptSource"

data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
    pDebuggerGetScriptSourceScriptId :: RuntimeScriptId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerGetScriptSource where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PDebuggerGetScriptSource  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


debuggerGetScriptSource :: Handle Event -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource handle params = sendReceiveCommandResult handle "Debugger.getScriptSource" (Just params)




debuggerPause :: Handle Event -> IO (Maybe Error)
debuggerPause handle = sendReceiveCommand handle "Debugger.pause" (Nothing :: Maybe ())



data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
    pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerRemoveBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON PDebuggerRemoveBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


debuggerRemoveBreakpoint :: Handle Event -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint handle params = sendReceiveCommand handle "Debugger.removeBreakpoint" (Just params)



data PDebuggerResume = PDebuggerResume {
    pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerResume where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON PDebuggerResume  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}


debuggerResume :: Handle Event -> PDebuggerResume -> IO (Maybe Error)
debuggerResume handle params = sendReceiveCommand handle "Debugger.resume" (Just params)

data DebuggerSearchInContent = DebuggerSearchInContent {
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSearchInContent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command  DebuggerSearchInContent where
    commandName _ = "Debugger.searchInContent"

data PDebuggerSearchInContent = PDebuggerSearchInContent {
    pDebuggerSearchInContentScriptId :: RuntimeScriptId,
    pDebuggerSearchInContentQuery :: String,
    pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
    pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSearchInContent where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PDebuggerSearchInContent  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


debuggerSearchInContent :: Handle Event -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent handle params = sendReceiveCommandResult handle "Debugger.searchInContent" (Just params)



data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
    pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetAsyncCallStackDepth where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON PDebuggerSetAsyncCallStackDepth  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


debuggerSetAsyncCallStackDepth :: Handle Event -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Debugger.setAsyncCallStackDepth" (Just params)

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSetBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command  DebuggerSetBreakpoint where
    commandName _ = "Debugger.setBreakpoint"

data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
    pDebuggerSetBreakpointLocation :: DebuggerLocation,
    pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PDebuggerSetBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


debuggerSetBreakpoint :: Handle Event -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setBreakpoint" (Just params)

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSetInstrumentationBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Command  DebuggerSetInstrumentationBreakpoint where
    commandName _ = "Debugger.setInstrumentationBreakpoint"

data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint {
    pDebuggerSetInstrumentationBreakpointInstrumentation :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}


debuggerSetInstrumentationBreakpoint :: Handle Event -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setInstrumentationBreakpoint" (Just params)

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSetBreakpointByUrl where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command  DebuggerSetBreakpointByUrl where
    commandName _ = "Debugger.setBreakpointByUrl"

data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
    pDebuggerSetBreakpointByUrlLineNumber :: Int,
    pDebuggerSetBreakpointByUrlUrl :: Maybe String,
    pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
    pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
    pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
    pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetBreakpointByUrl where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance ToJSON PDebuggerSetBreakpointByUrl  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}


debuggerSetBreakpointByUrl :: Handle Event -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl handle params = sendReceiveCommandResult handle "Debugger.setBreakpointByUrl" (Just params)



data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
    pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetBreakpointsActive where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON PDebuggerSetBreakpointsActive  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


debuggerSetBreakpointsActive :: Handle Event -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive handle params = sendReceiveCommand handle "Debugger.setBreakpointsActive" (Just params)



data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions {
    pDebuggerSetPauseOnExceptionsState :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetPauseOnExceptions where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON PDebuggerSetPauseOnExceptions  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


debuggerSetPauseOnExceptions :: Handle Event -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions handle params = sendReceiveCommand handle "Debugger.setPauseOnExceptions" (Just params)

data DebuggerSetScriptSource = DebuggerSetScriptSource {
    debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
    debuggerSetScriptSourceStackChanged :: Maybe Bool,
    debuggerSetScriptSourceAsyncStackTrace :: Maybe RuntimeStackTrace,
    debuggerSetScriptSourceExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  DebuggerSetScriptSource where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command  DebuggerSetScriptSource where
    commandName _ = "Debugger.setScriptSource"

data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
    pDebuggerSetScriptSourceScriptId :: RuntimeScriptId,
    pDebuggerSetScriptSourceScriptSource :: String,
    pDebuggerSetScriptSourceDryRun :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetScriptSource where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON PDebuggerSetScriptSource  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}


debuggerSetScriptSource :: Handle Event -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource handle params = sendReceiveCommandResult handle "Debugger.setScriptSource" (Just params)



data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
    pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetSkipAllPauses where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON PDebuggerSetSkipAllPauses  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


debuggerSetSkipAllPauses :: Handle Event -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses handle params = sendReceiveCommand handle "Debugger.setSkipAllPauses" (Just params)



data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
    pDebuggerSetVariableValueScopeNumber :: Int,
    pDebuggerSetVariableValueVariableName :: String,
    pDebuggerSetVariableValueNewValue :: RuntimeCallArgument,
    pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PDebuggerSetVariableValue where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON PDebuggerSetVariableValue  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}


debuggerSetVariableValue :: Handle Event -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue handle params = sendReceiveCommand handle "Debugger.setVariableValue" (Just params)




debuggerStepInto :: Handle Event -> IO (Maybe Error)
debuggerStepInto handle = sendReceiveCommand handle "Debugger.stepInto" (Nothing :: Maybe ())




debuggerStepOut :: Handle Event -> IO (Maybe Error)
debuggerStepOut handle = sendReceiveCommand handle "Debugger.stepOut" (Nothing :: Maybe ())




debuggerStepOver :: Handle Event -> IO (Maybe Error)
debuggerStepOver handle = sendReceiveCommand handle "Debugger.stepOver" (Nothing :: Maybe ())



data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
    profilerConsoleProfileFinishedId :: String,
    profilerConsoleProfileFinishedLocation :: DebuggerLocation,
    profilerConsoleProfileFinishedProfile :: ProfilerProfile,
    profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerConsoleProfileFinished where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance ToJSON ProfilerConsoleProfileFinished  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}


instance FromEvent Event ProfilerConsoleProfileFinished where
    eventName  _ _    =  "Profiler.consoleProfileFinished"
    fromEvent ev =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing

data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
    profilerConsoleProfileStartedId :: String,
    profilerConsoleProfileStartedLocation :: DebuggerLocation,
    profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerConsoleProfileStarted where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON ProfilerConsoleProfileStarted  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerProfileNode where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON ProfilerProfileNode  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



data ProfilerProfile = ProfilerProfile {
    profilerProfileNodes :: [ProfilerProfileNode],
    profilerProfileStartTime :: Double,
    profilerProfileEndTime :: Double,
    profilerProfileSamples :: Maybe [Int],
    profilerProfileTimeDeltas :: Maybe [Int]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerProfile where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance ToJSON ProfilerProfile  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
    profilerPositionTickInfoLine :: Int,
    profilerPositionTickInfoTicks :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerPositionTickInfo where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON ProfilerPositionTickInfo  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}



data ProfilerCoverageRange = ProfilerCoverageRange {
    profilerCoverageRangeStartOffset :: Int,
    profilerCoverageRangeEndOffset :: Int,
    profilerCoverageRangeCount :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerCoverageRange where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON ProfilerCoverageRange  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
    profilerFunctionCoverageFunctionName :: String,
    profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
    profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerFunctionCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance ToJSON ProfilerFunctionCoverage  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}



data ProfilerScriptCoverage = ProfilerScriptCoverage {
    profilerScriptCoverageScriptId :: RuntimeScriptId,
    profilerScriptCoverageUrl :: String,
    profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerScriptCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON ProfilerScriptCoverage  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}





profilerDisable :: Handle Event -> IO (Maybe Error)
profilerDisable handle = sendReceiveCommand handle "Profiler.disable" (Nothing :: Maybe ())




profilerEnable :: Handle Event -> IO (Maybe Error)
profilerEnable handle = sendReceiveCommand handle "Profiler.enable" (Nothing :: Maybe ())

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerGetBestEffortCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command  ProfilerGetBestEffortCoverage where
    commandName _ = "Profiler.getBestEffortCoverage"


profilerGetBestEffortCoverage :: Handle Event -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage handle = sendReceiveCommandResult handle "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())



data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
    pProfilerSetSamplingIntervalInterval :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PProfilerSetSamplingInterval where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance ToJSON PProfilerSetSamplingInterval  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}


profilerSetSamplingInterval :: Handle Event -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval handle params = sendReceiveCommand handle "Profiler.setSamplingInterval" (Just params)




profilerStart :: Handle Event -> IO (Maybe Error)
profilerStart handle = sendReceiveCommand handle "Profiler.start" (Nothing :: Maybe ())

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
    profilerStartPreciseCoverageTimestamp :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerStartPreciseCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command  ProfilerStartPreciseCoverage where
    commandName _ = "Profiler.startPreciseCoverage"

data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
    pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
    pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
    pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PProfilerStartPreciseCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance ToJSON PProfilerStartPreciseCoverage  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}


profilerStartPreciseCoverage :: Handle Event -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage handle params = sendReceiveCommandResult handle "Profiler.startPreciseCoverage" (Just params)

data ProfilerStop = ProfilerStop {
    profilerStopProfile :: ProfilerProfile
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerStop where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


instance Command  ProfilerStop where
    commandName _ = "Profiler.stop"


profilerStop :: Handle Event -> IO (Either Error ProfilerStop)
profilerStop handle = sendReceiveCommandResult handle "Profiler.stop" (Nothing :: Maybe ())




profilerStopPreciseCoverage :: Handle Event -> IO (Maybe Error)
profilerStopPreciseCoverage handle = sendReceiveCommand handle "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    profilerTakePreciseCoverageTimestamp :: Double
} deriving (Eq, Show, Read, Generic)
instance FromJSON  ProfilerTakePreciseCoverage where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command  ProfilerTakePreciseCoverage where
    commandName _ = "Profiler.takePreciseCoverage"


profilerTakePreciseCoverage :: Handle Event -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage handle = sendReceiveCommandResult handle "Profiler.takePreciseCoverage" (Nothing :: Maybe ())



data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
    runtimeConsoleApiCalledType :: String,
    runtimeConsoleApiCalledArgs :: [RuntimeRemoteObject],
    runtimeConsoleApiCalledExecutionContextId :: RuntimeExecutionContextId,
    runtimeConsoleApiCalledTimestamp :: RuntimeTimestamp,
    runtimeConsoleApiCalledStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeConsoleApiCalled where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON RuntimeConsoleApiCalled  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


instance FromEvent Event RuntimeConsoleApiCalled where
    eventName  _ _    =  "Runtime.consoleAPICalled"
    fromEvent ev =  case ev of EVRuntimeConsoleApiCalled v -> Just v; _ -> Nothing

data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
    runtimeExceptionRevokedReason :: String,
    runtimeExceptionRevokedExceptionId :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExceptionRevoked where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON RuntimeExceptionRevoked  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


instance FromEvent Event RuntimeExceptionRevoked where
    eventName  _ _    =  "Runtime.exceptionRevoked"
    fromEvent ev =  case ev of EVRuntimeExceptionRevoked v -> Just v; _ -> Nothing

data RuntimeExceptionThrown = RuntimeExceptionThrown {
    runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
    runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExceptionThrown where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON RuntimeExceptionThrown  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


instance FromEvent Event RuntimeExceptionThrown where
    eventName  _ _    =  "Runtime.exceptionThrown"
    fromEvent ev =  case ev of EVRuntimeExceptionThrown v -> Just v; _ -> Nothing

data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
    runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExecutionContextCreated where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance ToJSON RuntimeExecutionContextCreated  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}


instance FromEvent Event RuntimeExecutionContextCreated where
    eventName  _ _    =  "Runtime.executionContextCreated"
    fromEvent ev =  case ev of EVRuntimeExecutionContextCreated v -> Just v; _ -> Nothing

data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
    runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExecutionContextDestroyed where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance ToJSON RuntimeExecutionContextDestroyed  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeInspectRequested where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON RuntimeInspectRequested  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}


instance FromEvent Event RuntimeInspectRequested where
    eventName  _ _    =  "Runtime.inspectRequested"
    fromEvent ev =  case ev of EVRuntimeInspectRequested v -> Just v; _ -> Nothing


type RuntimeScriptId = String

data RuntimeWebDriverValue = RuntimeWebDriverValue {
    runtimeWebDriverValueType :: String,
    runtimeWebDriverValueValue :: Maybe Int,
    runtimeWebDriverValueObjectId :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeWebDriverValue where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON RuntimeWebDriverValue  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeRemoteObject where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON RuntimeRemoteObject  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimePropertyDescriptor where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance ToJSON RuntimePropertyDescriptor  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
    runtimeInternalPropertyDescriptorName :: String,
    runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeInternalPropertyDescriptor where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance ToJSON RuntimeInternalPropertyDescriptor  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}



data RuntimeCallArgument = RuntimeCallArgument {
    runtimeCallArgumentValue :: Maybe Int,
    runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeCallArgument where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance ToJSON RuntimeCallArgument  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}



type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
    runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
    runtimeExecutionContextDescriptionOrigin :: String,
    runtimeExecutionContextDescriptionName :: String,
    runtimeExecutionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExecutionContextDescription where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance ToJSON RuntimeExecutionContextDescription  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}



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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeExceptionDetails where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance ToJSON RuntimeExceptionDetails  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}



type RuntimeTimestamp = Double

type RuntimeTimeDelta = Double

data RuntimeCallFrame = RuntimeCallFrame {
    runtimeCallFrameFunctionName :: String,
    runtimeCallFrameScriptId :: RuntimeScriptId,
    runtimeCallFrameUrl :: String,
    runtimeCallFrameLineNumber :: Int,
    runtimeCallFrameColumnNumber :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeCallFrame where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON RuntimeCallFrame  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}



data RuntimeStackTrace = RuntimeStackTrace {
    runtimeStackTraceCallFrames :: [RuntimeCallFrame],
    runtimeStackTraceDescription :: Maybe String,
    runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeStackTrace where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON RuntimeStackTrace  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


data RuntimeAwaitPromise = RuntimeAwaitPromise {
    runtimeAwaitPromiseResult :: RuntimeRemoteObject,
    runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeAwaitPromise where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command  RuntimeAwaitPromise where
    commandName _ = "Runtime.awaitPromise"

data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
    pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
    pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
    pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeAwaitPromise where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON PRuntimeAwaitPromise  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


runtimeAwaitPromise :: Handle Event -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise handle params = sendReceiveCommandResult handle "Runtime.awaitPromise" (Just params)

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeCallFunctionOn where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeCallFunctionOn where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance ToJSON PRuntimeCallFunctionOn  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}


runtimeCallFunctionOn :: Handle Event -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn handle params = sendReceiveCommandResult handle "Runtime.callFunctionOn" (Just params)

data RuntimeCompileScript = RuntimeCompileScript {
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeCompileScript where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  RuntimeCompileScript where
    commandName _ = "Runtime.compileScript"

data PRuntimeCompileScript = PRuntimeCompileScript {
    pRuntimeCompileScriptExpression :: String,
    pRuntimeCompileScriptSourceURL :: String,
    pRuntimeCompileScriptPersistScript :: Bool,
    pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeCompileScript where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PRuntimeCompileScript  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


runtimeCompileScript :: Handle Event -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript handle params = sendReceiveCommandResult handle "Runtime.compileScript" (Just params)




runtimeDisable :: Handle Event -> IO (Maybe Error)
runtimeDisable handle = sendReceiveCommand handle "Runtime.disable" (Nothing :: Maybe ())




runtimeDiscardConsoleEntries :: Handle Event -> IO (Maybe Error)
runtimeDiscardConsoleEntries handle = sendReceiveCommand handle "Runtime.discardConsoleEntries" (Nothing :: Maybe ())




runtimeEnable :: Handle Event -> IO (Maybe Error)
runtimeEnable handle = sendReceiveCommand handle "Runtime.enable" (Nothing :: Maybe ())

data RuntimeEvaluate = RuntimeEvaluate {
    runtimeEvaluateResult :: RuntimeRemoteObject,
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeEvaluate where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Command  RuntimeEvaluate where
    commandName _ = "Runtime.evaluate"

data PRuntimeEvaluate = PRuntimeEvaluate {
    pRuntimeEvaluateExpression :: String,
    pRuntimeEvaluateObjectGroup :: Maybe String,
    pRuntimeEvaluateIncludeCommandLineAPI :: Maybe Bool,
    pRuntimeEvaluateSilent :: Maybe Bool,
    pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeEvaluateReturnByValue :: Maybe Bool,
    pRuntimeEvaluateUserGesture :: Maybe Bool,
    pRuntimeEvaluateAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeEvaluate where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance ToJSON PRuntimeEvaluate  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}


runtimeEvaluate :: Handle Event -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate handle params = sendReceiveCommandResult handle "Runtime.evaluate" (Just params)

data RuntimeGetProperties = RuntimeGetProperties {
    runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
    runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
    runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeGetProperties where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command  RuntimeGetProperties where
    commandName _ = "Runtime.getProperties"

data PRuntimeGetProperties = PRuntimeGetProperties {
    pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
    pRuntimeGetPropertiesOwnProperties :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeGetProperties where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PRuntimeGetProperties  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


runtimeGetProperties :: Handle Event -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties handle params = sendReceiveCommandResult handle "Runtime.getProperties" (Just params)

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
    runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeGlobalLexicalScopeNames where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command  RuntimeGlobalLexicalScopeNames where
    commandName _ = "Runtime.globalLexicalScopeNames"

data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
    pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeGlobalLexicalScopeNames where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance ToJSON PRuntimeGlobalLexicalScopeNames  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}


runtimeGlobalLexicalScopeNames :: Handle Event -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames handle params = sendReceiveCommandResult handle "Runtime.globalLexicalScopeNames" (Just params)

data RuntimeQueryObjects = RuntimeQueryObjects {
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeQueryObjects where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command  RuntimeQueryObjects where
    commandName _ = "Runtime.queryObjects"

data PRuntimeQueryObjects = PRuntimeQueryObjects {
    pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
    pRuntimeQueryObjectsObjectGroup :: Maybe String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeQueryObjects where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance ToJSON PRuntimeQueryObjects  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}


runtimeQueryObjects :: Handle Event -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects handle params = sendReceiveCommandResult handle "Runtime.queryObjects" (Just params)



data PRuntimeReleaseObject = PRuntimeReleaseObject {
    pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeReleaseObject where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance ToJSON PRuntimeReleaseObject  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}


runtimeReleaseObject :: Handle Event -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject handle params = sendReceiveCommand handle "Runtime.releaseObject" (Just params)



data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
    pRuntimeReleaseObjectGroupObjectGroup :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeReleaseObjectGroup where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance ToJSON PRuntimeReleaseObjectGroup  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}


runtimeReleaseObjectGroup :: Handle Event -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup handle params = sendReceiveCommand handle "Runtime.releaseObjectGroup" (Just params)




runtimeRunIfWaitingForDebugger :: Handle Event -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger handle = sendReceiveCommand handle "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())

data RuntimeRunScript = RuntimeRunScript {
    runtimeRunScriptResult :: RuntimeRemoteObject,
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read, Generic)
instance FromJSON  RuntimeRunScript where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  RuntimeRunScript where
    commandName _ = "Runtime.runScript"

data PRuntimeRunScript = PRuntimeRunScript {
    pRuntimeRunScriptScriptId :: RuntimeScriptId,
    pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeRunScriptObjectGroup :: Maybe String,
    pRuntimeRunScriptSilent :: Maybe Bool,
    pRuntimeRunScriptIncludeCommandLineAPI :: Maybe Bool,
    pRuntimeRunScriptReturnByValue :: Maybe Bool,
    pRuntimeRunScriptGeneratePreview :: Maybe Bool,
    pRuntimeRunScriptAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeRunScript where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance ToJSON PRuntimeRunScript  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}


runtimeRunScript :: Handle Event -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript handle params = sendReceiveCommandResult handle "Runtime.runScript" (Just params)



data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
    pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read, Generic)
instance FromJSON  PRuntimeSetAsyncCallStackDepth where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance ToJSON PRuntimeSetAsyncCallStackDepth  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}


runtimeSetAsyncCallStackDepth :: Handle Event -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Runtime.setAsyncCallStackDepth" (Just params)




data SchemaDomain = SchemaDomain {
    schemaDomainName :: String,
    schemaDomainVersion :: String
} deriving (Eq, Show, Read, Generic)
instance FromJSON  SchemaDomain where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance ToJSON SchemaDomain  where
    toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}


data SchemaGetDomains = SchemaGetDomains {
    schemaGetDomainsDomains :: [SchemaDomain]
} deriving (Eq, Show, Read, Generic)
instance FromJSON  SchemaGetDomains where
    parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command  SchemaGetDomains where
    commandName _ = "Schema.getDomains"


schemaGetDomains :: Handle Event -> IO (Either Error SchemaGetDomains)
schemaGetDomains handle = sendReceiveCommandResult handle "Schema.getDomains" (Nothing :: Maybe ())





