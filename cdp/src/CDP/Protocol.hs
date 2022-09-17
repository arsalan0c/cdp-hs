{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
 {-# LANGUAGE ScopedTypeVariables #-}
 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE DeriveGeneric #-}

 module CDP.Protocol where

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
data Event = EVDomAttributeModified DomAttributeModified | EVDomAttributeRemoved DomAttributeRemoved | EVDomCharacterDataModified DomCharacterDataModified | EVDomChildNodeCountUpdated DomChildNodeCountUpdated | EVDomChildNodeInserted DomChildNodeInserted | EVDomChildNodeRemoved DomChildNodeRemoved | EVDomDocumentUpdated DomDocumentUpdated | EVDomSetChildNodes DomSetChildNodes | EVLogEntryAdded LogEntryAdded | EVNetworkDataReceived NetworkDataReceived | EVNetworkEventSourceMessageReceived NetworkEventSourceMessageReceived | EVNetworkLoadingFailed NetworkLoadingFailed | EVNetworkLoadingFinished NetworkLoadingFinished | EVNetworkRequestServedFromCache NetworkRequestServedFromCache | EVNetworkRequestWillBeSent NetworkRequestWillBeSent | EVNetworkResponseReceived NetworkResponseReceived | EVNetworkWebSocketClosed NetworkWebSocketClosed | EVNetworkWebSocketCreated NetworkWebSocketCreated | EVNetworkWebSocketFrameError NetworkWebSocketFrameError | EVNetworkWebSocketFrameReceived NetworkWebSocketFrameReceived | EVNetworkWebSocketFrameSent NetworkWebSocketFrameSent | EVNetworkWebSocketHandshakeResponseReceived NetworkWebSocketHandshakeResponseReceived | EVNetworkWebSocketWillSendHandshakeRequest NetworkWebSocketWillSendHandshakeRequest | EVNetworkWebTransportCreated NetworkWebTransportCreated | EVNetworkWebTransportConnectionEstablished NetworkWebTransportConnectionEstablished | EVNetworkWebTransportClosed NetworkWebTransportClosed | EVPageDomContentEventFired PageDomContentEventFired | EVPageFileChooserOpened PageFileChooserOpened | EVPageFrameAttached PageFrameAttached | EVPageFrameDetached PageFrameDetached | EVPageFrameNavigated PageFrameNavigated | EVPageInterstitialHidden PageInterstitialHidden | EVPageInterstitialShown PageInterstitialShown | EVPageJavascriptDialogClosed PageJavascriptDialogClosed | EVPageJavascriptDialogOpening PageJavascriptDialogOpening | EVPageLifecycleEvent PageLifecycleEvent | EVPagePrerenderAttemptCompleted PagePrerenderAttemptCompleted | EVPageLoadEventFired PageLoadEventFired | EVPageWindowOpen PageWindowOpen | EVPerformanceMetrics PerformanceMetrics | EVTargetReceivedMessageFromTarget TargetReceivedMessageFromTarget | EVTargetTargetCreated TargetTargetCreated | EVTargetTargetDestroyed TargetTargetDestroyed | EVTargetTargetCrashed TargetTargetCrashed | EVTargetTargetInfoChanged TargetTargetInfoChanged | EVFetchRequestPaused FetchRequestPaused | EVFetchAuthRequired FetchAuthRequired | EVDebuggerBreakpointResolved DebuggerBreakpointResolved | EVDebuggerPaused DebuggerPaused | EVDebuggerResumed DebuggerResumed | EVDebuggerScriptFailedToParse DebuggerScriptFailedToParse | EVDebuggerScriptParsed DebuggerScriptParsed | EVProfilerConsoleProfileFinished ProfilerConsoleProfileFinished | EVProfilerConsoleProfileStarted ProfilerConsoleProfileStarted | EVRuntimeConsoleApiCalled RuntimeConsoleApiCalled | EVRuntimeExceptionRevoked RuntimeExceptionRevoked | EVRuntimeExceptionThrown RuntimeExceptionThrown | EVRuntimeExecutionContextCreated RuntimeExecutionContextCreated | EVRuntimeExecutionContextDestroyed RuntimeExecutionContextDestroyed | EVRuntimeExecutionContextsCleared RuntimeExecutionContextsCleared | EVRuntimeInspectRequested RuntimeInspectRequested
  deriving (Eq, Show, Read)
instance FromJSON (EventResponse Event ) where
   parseJSON = A.withObject  "EventResponse"  $ \obj -> do
       name <- obj .: "method"
       case (name :: String) of
           "DOM.attributeModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomAttributeModified) . fmap EVDomAttributeModified <$> obj .:? "params"
           "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomAttributeRemoved) . fmap EVDomAttributeRemoved <$> obj .:? "params"
           "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomCharacterDataModified) . fmap EVDomCharacterDataModified <$> obj .:? "params"
           "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomChildNodeCountUpdated) . fmap EVDomChildNodeCountUpdated <$> obj .:? "params"
           "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomChildNodeInserted) . fmap EVDomChildNodeInserted <$> obj .:? "params"
           "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomChildNodeRemoved) . fmap EVDomChildNodeRemoved <$> obj .:? "params"
           "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomDocumentUpdated) . fmap EVDomDocumentUpdated <$> obj .:? "params"
           "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DomSetChildNodes) . fmap EVDomSetChildNodes <$> obj .:? "params"
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




browserGetVersion :: Handle Event -> IO (Either Error BrowserGetVersion)
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())

data BrowserGetVersion = BrowserGetVersion {
   browserGetVersionProtocolVersion :: String,
   browserGetVersionProduct :: String,
   browserGetVersionRevision :: String,
   browserGetVersionUserAgent :: String,
   browserGetVersionJsVersion :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command BrowserGetVersion where
   commandName _ = "Browser.getVersion"





type DomNodeId = Int
type DomBackendNodeId = Int

data DomBackendNode = DomBackendNode {
   domBackendNodeNodeType :: Int,
   domBackendNodeNodeName :: String,
   domBackendNodeBackendNodeId :: DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBackendNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DomBackendNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


data DomPseudoType = DomPseudoTypeFirstLine | DomPseudoTypeFirstLetter | DomPseudoTypeBefore | DomPseudoTypeAfter | DomPseudoTypeMarker | DomPseudoTypeBackdrop | DomPseudoTypeSelection | DomPseudoTypeTargetText | DomPseudoTypeSpellingError | DomPseudoTypeGrammarError | DomPseudoTypeHighlight | DomPseudoTypeFirstLineInherited | DomPseudoTypeScrollbar | DomPseudoTypeScrollbarThumb | DomPseudoTypeScrollbarButton | DomPseudoTypeScrollbarTrack | DomPseudoTypeScrollbarTrackPiece | DomPseudoTypeScrollbarCorner | DomPseudoTypeResizer | DomPseudoTypeInputListButton | DomPseudoTypePageTransition | DomPseudoTypePageTransitionContainer | DomPseudoTypePageTransitionImageWrapper | DomPseudoTypePageTransitionOutgoingImage | DomPseudoTypePageTransitionIncomingImage
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomPseudoType where
   parseJSON = A.withText  "DomPseudoType"  $ \v -> do
      case v of
         "first-line" -> pure $ DomPseudoTypeFirstLine
         "first-letter" -> pure $ DomPseudoTypeFirstLetter
         "before" -> pure $ DomPseudoTypeBefore
         "after" -> pure $ DomPseudoTypeAfter
         "marker" -> pure $ DomPseudoTypeMarker
         "backdrop" -> pure $ DomPseudoTypeBackdrop
         "selection" -> pure $ DomPseudoTypeSelection
         "target-text" -> pure $ DomPseudoTypeTargetText
         "spelling-error" -> pure $ DomPseudoTypeSpellingError
         "grammar-error" -> pure $ DomPseudoTypeGrammarError
         "highlight" -> pure $ DomPseudoTypeHighlight
         "first-line-inherited" -> pure $ DomPseudoTypeFirstLineInherited
         "scrollbar" -> pure $ DomPseudoTypeScrollbar
         "scrollbar-thumb" -> pure $ DomPseudoTypeScrollbarThumb
         "scrollbar-button" -> pure $ DomPseudoTypeScrollbarButton
         "scrollbar-track" -> pure $ DomPseudoTypeScrollbarTrack
         "scrollbar-track-piece" -> pure $ DomPseudoTypeScrollbarTrackPiece
         "scrollbar-corner" -> pure $ DomPseudoTypeScrollbarCorner
         "resizer" -> pure $ DomPseudoTypeResizer
         "input-list-button" -> pure $ DomPseudoTypeInputListButton
         "page-transition" -> pure $ DomPseudoTypePageTransition
         "page-transition-container" -> pure $ DomPseudoTypePageTransitionContainer
         "page-transition-image-wrapper" -> pure $ DomPseudoTypePageTransitionImageWrapper
         "page-transition-outgoing-image" -> pure $ DomPseudoTypePageTransitionOutgoingImage
         "page-transition-incoming-image" -> pure $ DomPseudoTypePageTransitionIncomingImage
         _ -> fail "failed to parse DomPseudoType"

instance ToJSON DomPseudoType where
   toJSON v = A.String $
      case v of
         DomPseudoTypeFirstLine -> "first-line"
         DomPseudoTypeFirstLetter -> "first-letter"
         DomPseudoTypeBefore -> "before"
         DomPseudoTypeAfter -> "after"
         DomPseudoTypeMarker -> "marker"
         DomPseudoTypeBackdrop -> "backdrop"
         DomPseudoTypeSelection -> "selection"
         DomPseudoTypeTargetText -> "target-text"
         DomPseudoTypeSpellingError -> "spelling-error"
         DomPseudoTypeGrammarError -> "grammar-error"
         DomPseudoTypeHighlight -> "highlight"
         DomPseudoTypeFirstLineInherited -> "first-line-inherited"
         DomPseudoTypeScrollbar -> "scrollbar"
         DomPseudoTypeScrollbarThumb -> "scrollbar-thumb"
         DomPseudoTypeScrollbarButton -> "scrollbar-button"
         DomPseudoTypeScrollbarTrack -> "scrollbar-track"
         DomPseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
         DomPseudoTypeScrollbarCorner -> "scrollbar-corner"
         DomPseudoTypeResizer -> "resizer"
         DomPseudoTypeInputListButton -> "input-list-button"
         DomPseudoTypePageTransition -> "page-transition"
         DomPseudoTypePageTransitionContainer -> "page-transition-container"
         DomPseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
         DomPseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
         DomPseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"


data DomShadowRootType = DomShadowRootTypeUserAgent | DomShadowRootTypeOpen | DomShadowRootTypeClosed
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomShadowRootType where
   parseJSON = A.withText  "DomShadowRootType"  $ \v -> do
      case v of
         "user-agent" -> pure $ DomShadowRootTypeUserAgent
         "open" -> pure $ DomShadowRootTypeOpen
         "closed" -> pure $ DomShadowRootTypeClosed
         _ -> fail "failed to parse DomShadowRootType"

instance ToJSON DomShadowRootType where
   toJSON v = A.String $
      case v of
         DomShadowRootTypeUserAgent -> "user-agent"
         DomShadowRootTypeOpen -> "open"
         DomShadowRootTypeClosed -> "closed"


data DomCompatibilityMode = DomCompatibilityModeQuirksMode | DomCompatibilityModeLimitedQuirksMode | DomCompatibilityModeNoQuirksMode
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomCompatibilityMode where
   parseJSON = A.withText  "DomCompatibilityMode"  $ \v -> do
      case v of
         "QuirksMode" -> pure $ DomCompatibilityModeQuirksMode
         "LimitedQuirksMode" -> pure $ DomCompatibilityModeLimitedQuirksMode
         "NoQuirksMode" -> pure $ DomCompatibilityModeNoQuirksMode
         _ -> fail "failed to parse DomCompatibilityMode"

instance ToJSON DomCompatibilityMode where
   toJSON v = A.String $
      case v of
         DomCompatibilityModeQuirksMode -> "QuirksMode"
         DomCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
         DomCompatibilityModeNoQuirksMode -> "NoQuirksMode"



data DomNode = DomNode {
   domNodeNodeId :: DomNodeId,
   domNodeParentId :: Maybe DomNodeId,
   domNodeBackendNodeId :: DomBackendNodeId,
   domNodeNodeType :: Int,
   domNodeNodeName :: String,
   domNodeLocalName :: String,
   domNodeNodeValue :: String,
   domNodeChildNodeCount :: Maybe Int,
   domNodeChildren :: Maybe [DomNode],
   domNodeAttributes :: Maybe [String],
   domNodeDocumentUrl :: Maybe String,
   domNodeBaseUrl :: Maybe String,
   domNodePublicId :: Maybe String,
   domNodeSystemId :: Maybe String,
   domNodeInternalSubset :: Maybe String,
   domNodeXmlVersion :: Maybe String,
   domNodeName :: Maybe String,
   domNodeValue :: Maybe String,
   domNodePseudoType :: Maybe DomPseudoType,
   domNodeShadowRootType :: Maybe DomShadowRootType,
   domNodeFrameId :: Maybe PageFrameId,
   domNodeContentDocument :: Maybe DomNode,
   domNodeShadowRoots :: Maybe [DomNode],
   domNodeTemplateContent :: Maybe DomNode,
   domNodePseudoElements :: Maybe [DomNode],
   domNodeDistributedNodes :: Maybe [DomBackendNode],
   domNodeIsSvg :: Maybe Bool,
   domNodeCompatibilityMode :: Maybe DomCompatibilityMode,
   domNodeAssignedSlot :: Maybe DomBackendNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



data DomRgba = DomRgba {
   domRgbaR :: Int,
   domRgbaG :: Int,
   domRgbaB :: Int,
   domRgbaA :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRgba  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRgba where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }


type DomQuad = [Double]

data DomBoxModel = DomBoxModel {
   domBoxModelContent :: DomQuad,
   domBoxModelPadding :: DomQuad,
   domBoxModelBorder :: DomQuad,
   domBoxModelMargin :: DomQuad,
   domBoxModelWidth :: Int,
   domBoxModelHeight :: Int,
   domBoxModelShapeOutside :: Maybe DomShapeOutsideInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  DomBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data DomShapeOutsideInfo = DomShapeOutsideInfo {
   domShapeOutsideInfoBounds :: DomQuad,
   domShapeOutsideInfoShape :: [Int],
   domShapeOutsideInfoMarginShape :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShapeOutsideInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShapeOutsideInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data DomRect = DomRect {
   domRectX :: Double,
   domRectY :: Double,
   domRectWidth :: Double,
   domRectHeight :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



data DomCssComputedStyleProperty = DomCssComputedStyleProperty {
   domCssComputedStylePropertyName :: String,
   domCssComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }




data DomAttributeModified = DomAttributeModified {
   domAttributeModifiedNodeId :: DomNodeId,
   domAttributeModifiedName :: String,
   domAttributeModifiedValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomAttributeModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance FromEvent Event DomAttributeModified where
   eventName  _ _    =  "DOM.attributeModified"
   fromEvent ev      =  case ev of EVDomAttributeModified v -> Just v; _ -> Nothing



data DomAttributeRemoved = DomAttributeRemoved {
   domAttributeRemovedNodeId :: DomNodeId,
   domAttributeRemovedName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomAttributeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance FromEvent Event DomAttributeRemoved where
   eventName  _ _    =  "DOM.attributeRemoved"
   fromEvent ev      =  case ev of EVDomAttributeRemoved v -> Just v; _ -> Nothing



data DomCharacterDataModified = DomCharacterDataModified {
   domCharacterDataModifiedNodeId :: DomNodeId,
   domCharacterDataModifiedCharacterData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCharacterDataModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomCharacterDataModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance FromEvent Event DomCharacterDataModified where
   eventName  _ _    =  "DOM.characterDataModified"
   fromEvent ev      =  case ev of EVDomCharacterDataModified v -> Just v; _ -> Nothing



data DomChildNodeCountUpdated = DomChildNodeCountUpdated {
   domChildNodeCountUpdatedNodeId :: DomNodeId,
   domChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeCountUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeCountUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance FromEvent Event DomChildNodeCountUpdated where
   eventName  _ _    =  "DOM.childNodeCountUpdated"
   fromEvent ev      =  case ev of EVDomChildNodeCountUpdated v -> Just v; _ -> Nothing



data DomChildNodeInserted = DomChildNodeInserted {
   domChildNodeInsertedParentNodeId :: DomNodeId,
   domChildNodeInsertedPreviousNodeId :: DomNodeId,
   domChildNodeInsertedNode :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeInserted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeInserted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance FromEvent Event DomChildNodeInserted where
   eventName  _ _    =  "DOM.childNodeInserted"
   fromEvent ev      =  case ev of EVDomChildNodeInserted v -> Just v; _ -> Nothing



data DomChildNodeRemoved = DomChildNodeRemoved {
   domChildNodeRemovedParentNodeId :: DomNodeId,
   domChildNodeRemovedNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance FromEvent Event DomChildNodeRemoved where
   eventName  _ _    =  "DOM.childNodeRemoved"
   fromEvent ev      =  case ev of EVDomChildNodeRemoved v -> Just v; _ -> Nothing


data DomDocumentUpdated = DomDocumentUpdated
   deriving (Eq, Show, Read)
instance FromJSON DomDocumentUpdated where
   parseJSON = A.withText  "DomDocumentUpdated"  $ \v -> do
      case v of
         "DomDocumentUpdated" -> pure $ DomDocumentUpdated
         _ -> fail "failed to parse DomDocumentUpdated"


instance FromEvent Event DomDocumentUpdated where
   eventName  _ _    =  "DOM.documentUpdated"
   fromEvent ev      =  case ev of EVDomDocumentUpdated v -> Just v; _ -> Nothing



data DomSetChildNodes = DomSetChildNodes {
   domSetChildNodesParentId :: DomNodeId,
   domSetChildNodesNodes :: [DomNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DomSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance FromEvent Event DomSetChildNodes where
   eventName  _ _    =  "DOM.setChildNodes"
   fromEvent ev      =  case ev of EVDomSetChildNodes v -> Just v; _ -> Nothing




data PDomDescribeNode = PDomDescribeNode {
   pDomDescribeNodeNodeId :: Maybe DomNodeId,
   pDomDescribeNodeBackendNodeId :: Maybe DomBackendNodeId,
   pDomDescribeNodeObjectId :: Maybe RuntimeRemoteObjectId,
   pDomDescribeNodeDepth :: Maybe Int,
   pDomDescribeNodePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDescribeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domDescribeNode :: Handle Event -> PDomDescribeNode -> IO (Either Error DomDescribeNode)
domDescribeNode handle params = sendReceiveCommandResult handle "DOM.describeNode" (Just params)

data DomDescribeNode = DomDescribeNode {
   domDescribeNodeNode :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomDescribeNode where
   commandName _ = "DOM.describeNode"




domDisable :: Handle Event -> IO (Maybe Error)
domDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())



type PDomEnable = [(T.Text, T.Text)]
domEnable :: Handle Event -> PDomEnable -> IO (Maybe Error)
domEnable handle params = sendReceiveCommand handle "DOM.enable" (Just params)




data PDomFocus = PDomFocus {
   pDomFocusNodeId :: Maybe DomNodeId,
   pDomFocusBackendNodeId :: Maybe DomBackendNodeId,
   pDomFocusObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomFocus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PDomFocus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


domFocus :: Handle Event -> PDomFocus -> IO (Maybe Error)
domFocus handle params = sendReceiveCommand handle "DOM.focus" (Just params)




data PDomGetAttributes = PDomGetAttributes {
   pDomGetAttributesNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetAttributes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domGetAttributes :: Handle Event -> PDomGetAttributes -> IO (Either Error DomGetAttributes)
domGetAttributes handle params = sendReceiveCommandResult handle "DOM.getAttributes" (Just params)

data DomGetAttributes = DomGetAttributes {
   domGetAttributesAttributes :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetAttributes where
   commandName _ = "DOM.getAttributes"




data PDomGetBoxModel = PDomGetBoxModel {
   pDomGetBoxModelNodeId :: Maybe DomNodeId,
   pDomGetBoxModelBackendNodeId :: Maybe DomBackendNodeId,
   pDomGetBoxModelObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domGetBoxModel :: Handle Event -> PDomGetBoxModel -> IO (Either Error DomGetBoxModel)
domGetBoxModel handle params = sendReceiveCommandResult handle "DOM.getBoxModel" (Just params)

data DomGetBoxModel = DomGetBoxModel {
   domGetBoxModelModel :: DomBoxModel
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetBoxModel where
   commandName _ = "DOM.getBoxModel"




data PDomGetDocument = PDomGetDocument {
   pDomGetDocumentDepth :: Maybe Int,
   pDomGetDocumentPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domGetDocument :: Handle Event -> PDomGetDocument -> IO (Either Error DomGetDocument)
domGetDocument handle params = sendReceiveCommandResult handle "DOM.getDocument" (Just params)

data DomGetDocument = DomGetDocument {
   domGetDocumentRoot :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetDocument where
   commandName _ = "DOM.getDocument"




data PDomGetNodeForLocation = PDomGetNodeForLocation {
   pDomGetNodeForLocationX :: Int,
   pDomGetNodeForLocationY :: Int,
   pDomGetNodeForLocationIncludeUserAgentShadowDom :: Maybe Bool,
   pDomGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeForLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


domGetNodeForLocation :: Handle Event -> PDomGetNodeForLocation -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation handle params = sendReceiveCommandResult handle "DOM.getNodeForLocation" (Just params)

data DomGetNodeForLocation = DomGetNodeForLocation {
   domGetNodeForLocationBackendNodeId :: DomBackendNodeId,
   domGetNodeForLocationFrameId :: PageFrameId,
   domGetNodeForLocationNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeForLocation where
   commandName _ = "DOM.getNodeForLocation"




data PDomGetOuterHtml = PDomGetOuterHtml {
   pDomGetOuterHtmlNodeId :: Maybe DomNodeId,
   pDomGetOuterHtmlBackendNodeId :: Maybe DomBackendNodeId,
   pDomGetOuterHtmlObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domGetOuterHtml :: Handle Event -> PDomGetOuterHtml -> IO (Either Error DomGetOuterHtml)
domGetOuterHtml handle params = sendReceiveCommandResult handle "DOM.getOuterHTML" (Just params)

data DomGetOuterHtml = DomGetOuterHtml {
   domGetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomGetOuterHtml where
   commandName _ = "DOM.getOuterHTML"




domHideHighlight :: Handle Event -> IO (Maybe Error)
domHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())




domHighlightNode :: Handle Event -> IO (Maybe Error)
domHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())




domHighlightRect :: Handle Event -> IO (Maybe Error)
domHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())




data PDomMoveTo = PDomMoveTo {
   pDomMoveToNodeId :: DomNodeId,
   pDomMoveToTargetNodeId :: DomNodeId,
   pDomMoveToInsertBeforeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomMoveTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


domMoveTo :: Handle Event -> PDomMoveTo -> IO (Either Error DomMoveTo)
domMoveTo handle params = sendReceiveCommandResult handle "DOM.moveTo" (Just params)

data DomMoveTo = DomMoveTo {
   domMoveToNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomMoveTo where
   commandName _ = "DOM.moveTo"




data PDomQuerySelector = PDomQuerySelector {
   pDomQuerySelectorNodeId :: DomNodeId,
   pDomQuerySelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domQuerySelector :: Handle Event -> PDomQuerySelector -> IO (Either Error DomQuerySelector)
domQuerySelector handle params = sendReceiveCommandResult handle "DOM.querySelector" (Just params)

data DomQuerySelector = DomQuerySelector {
   domQuerySelectorNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomQuerySelector where
   commandName _ = "DOM.querySelector"




data PDomQuerySelectorAll = PDomQuerySelectorAll {
   pDomQuerySelectorAllNodeId :: DomNodeId,
   pDomQuerySelectorAllSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelectorAll  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


domQuerySelectorAll :: Handle Event -> PDomQuerySelectorAll -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll handle params = sendReceiveCommandResult handle "DOM.querySelectorAll" (Just params)

data DomQuerySelectorAll = DomQuerySelectorAll {
   domQuerySelectorAllNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomQuerySelectorAll where
   commandName _ = "DOM.querySelectorAll"




data PDomRemoveAttribute = PDomRemoveAttribute {
   pDomRemoveAttributeNodeId :: DomNodeId,
   pDomRemoveAttributeName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveAttribute  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveAttribute where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


domRemoveAttribute :: Handle Event -> PDomRemoveAttribute -> IO (Maybe Error)
domRemoveAttribute handle params = sendReceiveCommand handle "DOM.removeAttribute" (Just params)




data PDomRemoveNode = PDomRemoveNode {
   pDomRemoveNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


domRemoveNode :: Handle Event -> PDomRemoveNode -> IO (Maybe Error)
domRemoveNode handle params = sendReceiveCommand handle "DOM.removeNode" (Just params)




data PDomRequestChildNodes = PDomRequestChildNodes {
   pDomRequestChildNodesNodeId :: DomNodeId,
   pDomRequestChildNodesDepth :: Maybe Int,
   pDomRequestChildNodesPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomRequestChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domRequestChildNodes :: Handle Event -> PDomRequestChildNodes -> IO (Maybe Error)
domRequestChildNodes handle params = sendReceiveCommand handle "DOM.requestChildNodes" (Just params)




data PDomRequestNode = PDomRequestNode {
   pDomRequestNodeObjectId :: RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domRequestNode :: Handle Event -> PDomRequestNode -> IO (Either Error DomRequestNode)
domRequestNode handle params = sendReceiveCommandResult handle "DOM.requestNode" (Just params)

data DomRequestNode = DomRequestNode {
   domRequestNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomRequestNode where
   commandName _ = "DOM.requestNode"




data PDomResolveNode = PDomResolveNode {
   pDomResolveNodeNodeId :: Maybe DomNodeId,
   pDomResolveNodeBackendNodeId :: Maybe DomBackendNodeId,
   pDomResolveNodeObjectGroup :: Maybe String,
   pDomResolveNodeExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomResolveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domResolveNode :: Handle Event -> PDomResolveNode -> IO (Either Error DomResolveNode)
domResolveNode handle params = sendReceiveCommandResult handle "DOM.resolveNode" (Just params)

data DomResolveNode = DomResolveNode {
   domResolveNodeObject :: RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomResolveNode where
   commandName _ = "DOM.resolveNode"




data PDomSetAttributeValue = PDomSetAttributeValue {
   pDomSetAttributeValueNodeId :: DomNodeId,
   pDomSetAttributeValueName :: String,
   pDomSetAttributeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domSetAttributeValue :: Handle Event -> PDomSetAttributeValue -> IO (Maybe Error)
domSetAttributeValue handle params = sendReceiveCommand handle "DOM.setAttributeValue" (Just params)




data PDomSetAttributesAsText = PDomSetAttributesAsText {
   pDomSetAttributesAsTextNodeId :: DomNodeId,
   pDomSetAttributesAsTextText :: String,
   pDomSetAttributesAsTextName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributesAsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributesAsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


domSetAttributesAsText :: Handle Event -> PDomSetAttributesAsText -> IO (Maybe Error)
domSetAttributesAsText handle params = sendReceiveCommand handle "DOM.setAttributesAsText" (Just params)




data PDomSetFileInputFiles = PDomSetFileInputFiles {
   pDomSetFileInputFilesFiles :: [String],
   pDomSetFileInputFilesNodeId :: Maybe DomNodeId,
   pDomSetFileInputFilesBackendNodeId :: Maybe DomBackendNodeId,
   pDomSetFileInputFilesObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetFileInputFiles  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetFileInputFiles where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domSetFileInputFiles :: Handle Event -> PDomSetFileInputFiles -> IO (Maybe Error)
domSetFileInputFiles handle params = sendReceiveCommand handle "DOM.setFileInputFiles" (Just params)




data PDomSetNodeName = PDomSetNodeName {
   pDomSetNodeNameNodeId :: DomNodeId,
   pDomSetNodeNameName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeName  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domSetNodeName :: Handle Event -> PDomSetNodeName -> IO (Either Error DomSetNodeName)
domSetNodeName handle params = sendReceiveCommandResult handle "DOM.setNodeName" (Just params)

data DomSetNodeName = DomSetNodeName {
   domSetNodeNameNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomSetNodeName where
   commandName _ = "DOM.setNodeName"




data PDomSetNodeValue = PDomSetNodeValue {
   pDomSetNodeValueNodeId :: DomNodeId,
   pDomSetNodeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domSetNodeValue :: Handle Event -> PDomSetNodeValue -> IO (Maybe Error)
domSetNodeValue handle params = sendReceiveCommand handle "DOM.setNodeValue" (Just params)




data PDomSetOuterHtml = PDomSetOuterHtml {
   pDomSetOuterHtmlNodeId :: DomNodeId,
   pDomSetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domSetOuterHtml :: Handle Event -> PDomSetOuterHtml -> IO (Maybe Error)
domSetOuterHtml handle params = sendReceiveCommand handle "DOM.setOuterHTML" (Just params)





data DomDebuggerDomBreakpointType = DomDebuggerDomBreakpointTypeSubtreeModified | DomDebuggerDomBreakpointTypeAttributeModified | DomDebuggerDomBreakpointTypeNodeRemoved
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomDebuggerDomBreakpointType where
   parseJSON = A.withText  "DomDebuggerDomBreakpointType"  $ \v -> do
      case v of
         "subtree-modified" -> pure $ DomDebuggerDomBreakpointTypeSubtreeModified
         "attribute-modified" -> pure $ DomDebuggerDomBreakpointTypeAttributeModified
         "node-removed" -> pure $ DomDebuggerDomBreakpointTypeNodeRemoved
         _ -> fail "failed to parse DomDebuggerDomBreakpointType"

instance ToJSON DomDebuggerDomBreakpointType where
   toJSON v = A.String $
      case v of
         DomDebuggerDomBreakpointTypeSubtreeModified -> "subtree-modified"
         DomDebuggerDomBreakpointTypeAttributeModified -> "attribute-modified"
         DomDebuggerDomBreakpointTypeNodeRemoved -> "node-removed"



data DomDebuggerEventListener = DomDebuggerEventListener {
   domDebuggerEventListenerType :: String,
   domDebuggerEventListenerUseCapture :: Bool,
   domDebuggerEventListenerPassive :: Bool,
   domDebuggerEventListenerOnce :: Bool,
   domDebuggerEventListenerScriptId :: RuntimeScriptId,
   domDebuggerEventListenerLineNumber :: Int,
   domDebuggerEventListenerColumnNumber :: Int,
   domDebuggerEventListenerHandler :: Maybe RuntimeRemoteObject,
   domDebuggerEventListenerOriginalHandler :: Maybe RuntimeRemoteObject,
   domDebuggerEventListenerBackendNodeId :: Maybe DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomDebuggerEventListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomDebuggerEventListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }





data PDomDebuggerGetEventListeners = PDomDebuggerGetEventListeners {
   pDomDebuggerGetEventListenersObjectId :: RuntimeRemoteObjectId,
   pDomDebuggerGetEventListenersDepth :: Maybe Int,
   pDomDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerGetEventListeners  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


domDebuggerGetEventListeners :: Handle Event -> PDomDebuggerGetEventListeners -> IO (Either Error DomDebuggerGetEventListeners)
domDebuggerGetEventListeners handle params = sendReceiveCommandResult handle "DOMDebugger.getEventListeners" (Just params)

data DomDebuggerGetEventListeners = DomDebuggerGetEventListeners {
   domDebuggerGetEventListenersListeners :: [DomDebuggerEventListener]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomDebuggerGetEventListeners where
   commandName _ = "DOMDebugger.getEventListeners"




data PDomDebuggerRemoveDomBreakpoint = PDomDebuggerRemoveDomBreakpoint {
   pDomDebuggerRemoveDomBreakpointNodeId :: DomNodeId,
   pDomDebuggerRemoveDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


domDebuggerRemoveDomBreakpoint :: Handle Event -> PDomDebuggerRemoveDomBreakpoint -> IO (Maybe Error)
domDebuggerRemoveDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeDOMBreakpoint" (Just params)




data PDomDebuggerRemoveEventListenerBreakpoint = PDomDebuggerRemoveEventListenerBreakpoint {
   pDomDebuggerRemoveEventListenerBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


domDebuggerRemoveEventListenerBreakpoint :: Handle Event -> PDomDebuggerRemoveEventListenerBreakpoint -> IO (Maybe Error)
domDebuggerRemoveEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeEventListenerBreakpoint" (Just params)




data PDomDebuggerRemoveXhrBreakpoint = PDomDebuggerRemoveXhrBreakpoint {
   pDomDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


domDebuggerRemoveXhrBreakpoint :: Handle Event -> PDomDebuggerRemoveXhrBreakpoint -> IO (Maybe Error)
domDebuggerRemoveXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeXHRBreakpoint" (Just params)




data PDomDebuggerSetDomBreakpoint = PDomDebuggerSetDomBreakpoint {
   pDomDebuggerSetDomBreakpointNodeId :: DomNodeId,
   pDomDebuggerSetDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domDebuggerSetDomBreakpoint :: Handle Event -> PDomDebuggerSetDomBreakpoint -> IO (Maybe Error)
domDebuggerSetDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setDOMBreakpoint" (Just params)




data PDomDebuggerSetEventListenerBreakpoint = PDomDebuggerSetEventListenerBreakpoint {
   pDomDebuggerSetEventListenerBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


domDebuggerSetEventListenerBreakpoint :: Handle Event -> PDomDebuggerSetEventListenerBreakpoint -> IO (Maybe Error)
domDebuggerSetEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setEventListenerBreakpoint" (Just params)




data PDomDebuggerSetXhrBreakpoint = PDomDebuggerSetXhrBreakpoint {
   pDomDebuggerSetXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domDebuggerSetXhrBreakpoint :: Handle Event -> PDomDebuggerSetXhrBreakpoint -> IO (Maybe Error)
domDebuggerSetXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setXHRBreakpoint" (Just params)





data EmulationScreenOrientationType = EmulationScreenOrientationTypePortraitPrimary | EmulationScreenOrientationTypePortraitSecondary | EmulationScreenOrientationTypeLandscapePrimary | EmulationScreenOrientationTypeLandscapeSecondary
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationScreenOrientationType where
   parseJSON = A.withText  "EmulationScreenOrientationType"  $ \v -> do
      case v of
         "portraitPrimary" -> pure $ EmulationScreenOrientationTypePortraitPrimary
         "portraitSecondary" -> pure $ EmulationScreenOrientationTypePortraitSecondary
         "landscapePrimary" -> pure $ EmulationScreenOrientationTypeLandscapePrimary
         "landscapeSecondary" -> pure $ EmulationScreenOrientationTypeLandscapeSecondary
         _ -> fail "failed to parse EmulationScreenOrientationType"

instance ToJSON EmulationScreenOrientationType where
   toJSON v = A.String $
      case v of
         EmulationScreenOrientationTypePortraitPrimary -> "portraitPrimary"
         EmulationScreenOrientationTypePortraitSecondary -> "portraitSecondary"
         EmulationScreenOrientationTypeLandscapePrimary -> "landscapePrimary"
         EmulationScreenOrientationTypeLandscapeSecondary -> "landscapeSecondary"



data EmulationScreenOrientation = EmulationScreenOrientation {
   emulationScreenOrientationType :: EmulationScreenOrientationType,
   emulationScreenOrientationAngle :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationScreenOrientation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  EmulationScreenOrientation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


data EmulationDisplayFeatureOrientation = EmulationDisplayFeatureOrientationVertical | EmulationDisplayFeatureOrientationHorizontal
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisplayFeatureOrientation where
   parseJSON = A.withText  "EmulationDisplayFeatureOrientation"  $ \v -> do
      case v of
         "vertical" -> pure $ EmulationDisplayFeatureOrientationVertical
         "horizontal" -> pure $ EmulationDisplayFeatureOrientationHorizontal
         _ -> fail "failed to parse EmulationDisplayFeatureOrientation"

instance ToJSON EmulationDisplayFeatureOrientation where
   toJSON v = A.String $
      case v of
         EmulationDisplayFeatureOrientationVertical -> "vertical"
         EmulationDisplayFeatureOrientationHorizontal -> "horizontal"



data EmulationDisplayFeature = EmulationDisplayFeature {
   emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation,
   emulationDisplayFeatureOffset :: Int,
   emulationDisplayFeatureMaskLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data EmulationMediaFeature = EmulationMediaFeature {
   emulationMediaFeatureName :: String,
   emulationMediaFeatureValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationMediaFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  EmulationMediaFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }





emulationCanEmulate :: Handle Event -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())

data EmulationCanEmulate = EmulationCanEmulate {
   emulationCanEmulateResult :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command EmulationCanEmulate where
   commandName _ = "Emulation.canEmulate"




emulationClearDeviceMetricsOverride :: Handle Event -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())




emulationClearGeolocationOverride :: Handle Event -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())




data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
   pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


emulationSetDefaultBackgroundColorOverride :: Handle Event -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)




data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
   pEmulationSetDeviceMetricsOverrideWidth :: Int,
   pEmulationSetDeviceMetricsOverrideHeight :: Int,
   pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
   pEmulationSetDeviceMetricsOverrideMobile :: Bool,
   pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetDeviceMetricsOverride :: Handle Event -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)




data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
   pEmulationSetEmulatedMediaMedia :: Maybe String,
   pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


emulationSetEmulatedMedia :: Handle Event -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia handle params = sendReceiveCommand handle "Emulation.setEmulatedMedia" (Just params)




data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
   pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
   pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
   pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetGeolocationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetGeolocationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


emulationSetGeolocationOverride :: Handle Event -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)




data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
   pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


emulationSetScriptExecutionDisabled :: Handle Event -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)




data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
   pEmulationSetTouchEmulationEnabledEnabled :: Bool,
   pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTouchEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTouchEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetTouchEmulationEnabled :: Handle Event -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)




data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
   pEmulationSetUserAgentOverrideUserAgent :: String,
   pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pEmulationSetUserAgentOverridePlatform :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


emulationSetUserAgentOverride :: Handle Event -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)





type IoStreamHandle = String



data PIoClose = PIoClose {
   pIoCloseHandle :: IoStreamHandle
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoClose  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  PIoClose where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }


ioClose :: Handle Event -> PIoClose -> IO (Maybe Error)
ioClose handle params = sendReceiveCommand handle "IO.close" (Just params)




data PIoRead = PIoRead {
   pIoReadHandle :: IoStreamHandle,
   pIoReadOffset :: Maybe Int,
   pIoReadSize :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoRead  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  PIoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }


ioRead :: Handle Event -> PIoRead -> IO (Either Error IoRead)
ioRead handle params = sendReceiveCommandResult handle "IO.read" (Just params)

data IoRead = IoRead {
   ioReadBase64Encoded :: Maybe Bool,
   ioReadData :: String,
   ioReadEof :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 6 }

instance Command IoRead where
   commandName _ = "IO.read"




data PIoResolveBlob = PIoResolveBlob {
   pIoResolveBlobObjectId :: RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoResolveBlob  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PIoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


ioResolveBlob :: Handle Event -> PIoResolveBlob -> IO (Either Error IoResolveBlob)
ioResolveBlob handle params = sendReceiveCommandResult handle "IO.resolveBlob" (Just params)

data IoResolveBlob = IoResolveBlob {
   ioResolveBlobUuid :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance Command IoResolveBlob where
   commandName _ = "IO.resolveBlob"






data InputTouchPoint = InputTouchPoint {
   inputTouchPointX :: Double,
   inputTouchPointY :: Double,
   inputTouchPointRadiusX :: Maybe Double,
   inputTouchPointRadiusY :: Maybe Double,
   inputTouchPointRotationAngle :: Maybe Double,
   inputTouchPointForce :: Maybe Double,
   inputTouchPointId :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputTouchPoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  InputTouchPoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
   deriving (Ord, Eq, Show, Read)
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


data PInputDispatchKeyEventType = PInputDispatchKeyEventTypeKeyDown | PInputDispatchKeyEventTypeKeyUp | PInputDispatchKeyEventTypeRawKeyDown | PInputDispatchKeyEventTypeChar
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchKeyEventType where
   parseJSON = A.withText  "PInputDispatchKeyEventType"  $ \v -> do
      case v of
         "keyDown" -> pure $ PInputDispatchKeyEventTypeKeyDown
         "keyUp" -> pure $ PInputDispatchKeyEventTypeKeyUp
         "rawKeyDown" -> pure $ PInputDispatchKeyEventTypeRawKeyDown
         "char" -> pure $ PInputDispatchKeyEventTypeChar
         _ -> fail "failed to parse PInputDispatchKeyEventType"

instance ToJSON PInputDispatchKeyEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchKeyEventTypeKeyDown -> "keyDown"
         PInputDispatchKeyEventTypeKeyUp -> "keyUp"
         PInputDispatchKeyEventTypeRawKeyDown -> "rawKeyDown"
         PInputDispatchKeyEventTypeChar -> "char"



data PInputDispatchKeyEvent = PInputDispatchKeyEvent {
   pInputDispatchKeyEventType :: PInputDispatchKeyEventType,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchKeyEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchKeyEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


inputDispatchKeyEvent :: Handle Event -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent handle params = sendReceiveCommand handle "Input.dispatchKeyEvent" (Just params)



data PInputDispatchMouseEventType = PInputDispatchMouseEventTypeMousePressed | PInputDispatchMouseEventTypeMouseReleased | PInputDispatchMouseEventTypeMouseMoved | PInputDispatchMouseEventTypeMouseWheel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventType where
   parseJSON = A.withText  "PInputDispatchMouseEventType"  $ \v -> do
      case v of
         "mousePressed" -> pure $ PInputDispatchMouseEventTypeMousePressed
         "mouseReleased" -> pure $ PInputDispatchMouseEventTypeMouseReleased
         "mouseMoved" -> pure $ PInputDispatchMouseEventTypeMouseMoved
         "mouseWheel" -> pure $ PInputDispatchMouseEventTypeMouseWheel
         _ -> fail "failed to parse PInputDispatchMouseEventType"

instance ToJSON PInputDispatchMouseEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchMouseEventTypeMousePressed -> "mousePressed"
         PInputDispatchMouseEventTypeMouseReleased -> "mouseReleased"
         PInputDispatchMouseEventTypeMouseMoved -> "mouseMoved"
         PInputDispatchMouseEventTypeMouseWheel -> "mouseWheel"


data PInputDispatchMouseEventPointerType = PInputDispatchMouseEventPointerTypeMouse | PInputDispatchMouseEventPointerTypePen
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventPointerType where
   parseJSON = A.withText  "PInputDispatchMouseEventPointerType"  $ \v -> do
      case v of
         "mouse" -> pure $ PInputDispatchMouseEventPointerTypeMouse
         "pen" -> pure $ PInputDispatchMouseEventPointerTypePen
         _ -> fail "failed to parse PInputDispatchMouseEventPointerType"

instance ToJSON PInputDispatchMouseEventPointerType where
   toJSON v = A.String $
      case v of
         PInputDispatchMouseEventPointerTypeMouse -> "mouse"
         PInputDispatchMouseEventPointerTypePen -> "pen"



data PInputDispatchMouseEvent = PInputDispatchMouseEvent {
   pInputDispatchMouseEventType :: PInputDispatchMouseEventType,
   pInputDispatchMouseEventX :: Double,
   pInputDispatchMouseEventY :: Double,
   pInputDispatchMouseEventModifiers :: Maybe Int,
   pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
   pInputDispatchMouseEventButton :: Maybe InputMouseButton,
   pInputDispatchMouseEventButtons :: Maybe Int,
   pInputDispatchMouseEventClickCount :: Maybe Int,
   pInputDispatchMouseEventDeltaX :: Maybe Double,
   pInputDispatchMouseEventDeltaY :: Maybe Double,
   pInputDispatchMouseEventPointerType :: PInputDispatchMouseEventPointerType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchMouseEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchMouseEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


inputDispatchMouseEvent :: Handle Event -> PInputDispatchMouseEvent -> IO (Maybe Error)
inputDispatchMouseEvent handle params = sendReceiveCommand handle "Input.dispatchMouseEvent" (Just params)



data PInputDispatchTouchEventType = PInputDispatchTouchEventTypeTouchStart | PInputDispatchTouchEventTypeTouchEnd | PInputDispatchTouchEventTypeTouchMove | PInputDispatchTouchEventTypeTouchCancel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchTouchEventType where
   parseJSON = A.withText  "PInputDispatchTouchEventType"  $ \v -> do
      case v of
         "touchStart" -> pure $ PInputDispatchTouchEventTypeTouchStart
         "touchEnd" -> pure $ PInputDispatchTouchEventTypeTouchEnd
         "touchMove" -> pure $ PInputDispatchTouchEventTypeTouchMove
         "touchCancel" -> pure $ PInputDispatchTouchEventTypeTouchCancel
         _ -> fail "failed to parse PInputDispatchTouchEventType"

instance ToJSON PInputDispatchTouchEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchTouchEventTypeTouchStart -> "touchStart"
         PInputDispatchTouchEventTypeTouchEnd -> "touchEnd"
         PInputDispatchTouchEventTypeTouchMove -> "touchMove"
         PInputDispatchTouchEventTypeTouchCancel -> "touchCancel"



data PInputDispatchTouchEvent = PInputDispatchTouchEvent {
   pInputDispatchTouchEventType :: PInputDispatchTouchEventType,
   pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
   pInputDispatchTouchEventModifiers :: Maybe Int,
   pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchTouchEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchTouchEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


inputDispatchTouchEvent :: Handle Event -> PInputDispatchTouchEvent -> IO (Maybe Error)
inputDispatchTouchEvent handle params = sendReceiveCommand handle "Input.dispatchTouchEvent" (Just params)




data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
   pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetIgnoreInputEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSetIgnoreInputEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


inputSetIgnoreInputEvents :: Handle Event -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents handle params = sendReceiveCommand handle "Input.setIgnoreInputEvents" (Just params)





data LogLogEntrySource = LogLogEntrySourceXml | LogLogEntrySourceJavascript | LogLogEntrySourceNetwork | LogLogEntrySourceStorage | LogLogEntrySourceAppcache | LogLogEntrySourceRendering | LogLogEntrySourceSecurity | LogLogEntrySourceDeprecation | LogLogEntrySourceWorker | LogLogEntrySourceViolation | LogLogEntrySourceIntervention | LogLogEntrySourceRecommendation | LogLogEntrySourceOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntrySource where
   parseJSON = A.withText  "LogLogEntrySource"  $ \v -> do
      case v of
         "xml" -> pure $ LogLogEntrySourceXml
         "javascript" -> pure $ LogLogEntrySourceJavascript
         "network" -> pure $ LogLogEntrySourceNetwork
         "storage" -> pure $ LogLogEntrySourceStorage
         "appcache" -> pure $ LogLogEntrySourceAppcache
         "rendering" -> pure $ LogLogEntrySourceRendering
         "security" -> pure $ LogLogEntrySourceSecurity
         "deprecation" -> pure $ LogLogEntrySourceDeprecation
         "worker" -> pure $ LogLogEntrySourceWorker
         "violation" -> pure $ LogLogEntrySourceViolation
         "intervention" -> pure $ LogLogEntrySourceIntervention
         "recommendation" -> pure $ LogLogEntrySourceRecommendation
         "other" -> pure $ LogLogEntrySourceOther
         _ -> fail "failed to parse LogLogEntrySource"

instance ToJSON LogLogEntrySource where
   toJSON v = A.String $
      case v of
         LogLogEntrySourceXml -> "xml"
         LogLogEntrySourceJavascript -> "javascript"
         LogLogEntrySourceNetwork -> "network"
         LogLogEntrySourceStorage -> "storage"
         LogLogEntrySourceAppcache -> "appcache"
         LogLogEntrySourceRendering -> "rendering"
         LogLogEntrySourceSecurity -> "security"
         LogLogEntrySourceDeprecation -> "deprecation"
         LogLogEntrySourceWorker -> "worker"
         LogLogEntrySourceViolation -> "violation"
         LogLogEntrySourceIntervention -> "intervention"
         LogLogEntrySourceRecommendation -> "recommendation"
         LogLogEntrySourceOther -> "other"


data LogLogEntryLevel = LogLogEntryLevelVerbose | LogLogEntryLevelInfo | LogLogEntryLevelWarning | LogLogEntryLevelError
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntryLevel where
   parseJSON = A.withText  "LogLogEntryLevel"  $ \v -> do
      case v of
         "verbose" -> pure $ LogLogEntryLevelVerbose
         "info" -> pure $ LogLogEntryLevelInfo
         "warning" -> pure $ LogLogEntryLevelWarning
         "error" -> pure $ LogLogEntryLevelError
         _ -> fail "failed to parse LogLogEntryLevel"

instance ToJSON LogLogEntryLevel where
   toJSON v = A.String $
      case v of
         LogLogEntryLevelVerbose -> "verbose"
         LogLogEntryLevelInfo -> "info"
         LogLogEntryLevelWarning -> "warning"
         LogLogEntryLevelError -> "error"


data LogLogEntryCategory = LogLogEntryCategoryCors
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntryCategory where
   parseJSON = A.withText  "LogLogEntryCategory"  $ \v -> do
      case v of
         "cors" -> pure $ LogLogEntryCategoryCors
         _ -> fail "failed to parse LogLogEntryCategory"

instance ToJSON LogLogEntryCategory where
   toJSON v = A.String $
      case v of
         LogLogEntryCategoryCors -> "cors"



data LogLogEntry = LogLogEntry {
   logLogEntrySource :: LogLogEntrySource,
   logLogEntryLevel :: LogLogEntryLevel,
   logLogEntryText :: String,
   logLogEntryCategory :: LogLogEntryCategory,
   logLogEntryTimestamp :: RuntimeTimestamp,
   logLogEntryUrl :: Maybe String,
   logLogEntryLineNumber :: Maybe Int,
   logLogEntryStackTrace :: Maybe RuntimeStackTrace,
   logLogEntryNetworkRequestId :: Maybe NetworkRequestId,
   logLogEntryWorkerId :: Maybe String,
   logLogEntryArgs :: Maybe [RuntimeRemoteObject]
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogLogEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  LogLogEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


data LogViolationSettingName = LogViolationSettingNameLongTask | LogViolationSettingNameLongLayout | LogViolationSettingNameBlockedEvent | LogViolationSettingNameBlockedParser | LogViolationSettingNameDiscouragedApiUse | LogViolationSettingNameHandler | LogViolationSettingNameRecurringHandler
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogViolationSettingName where
   parseJSON = A.withText  "LogViolationSettingName"  $ \v -> do
      case v of
         "longTask" -> pure $ LogViolationSettingNameLongTask
         "longLayout" -> pure $ LogViolationSettingNameLongLayout
         "blockedEvent" -> pure $ LogViolationSettingNameBlockedEvent
         "blockedParser" -> pure $ LogViolationSettingNameBlockedParser
         "discouragedAPIUse" -> pure $ LogViolationSettingNameDiscouragedApiUse
         "handler" -> pure $ LogViolationSettingNameHandler
         "recurringHandler" -> pure $ LogViolationSettingNameRecurringHandler
         _ -> fail "failed to parse LogViolationSettingName"

instance ToJSON LogViolationSettingName where
   toJSON v = A.String $
      case v of
         LogViolationSettingNameLongTask -> "longTask"
         LogViolationSettingNameLongLayout -> "longLayout"
         LogViolationSettingNameBlockedEvent -> "blockedEvent"
         LogViolationSettingNameBlockedParser -> "blockedParser"
         LogViolationSettingNameDiscouragedApiUse -> "discouragedAPIUse"
         LogViolationSettingNameHandler -> "handler"
         LogViolationSettingNameRecurringHandler -> "recurringHandler"



data LogViolationSetting = LogViolationSetting {
   logViolationSettingName :: LogViolationSettingName,
   logViolationSettingThreshold :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogViolationSetting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  LogViolationSetting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }




data LogEntryAdded = LogEntryAdded {
   logEntryAddedEntry :: LogLogEntry
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogEntryAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  LogEntryAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


instance FromEvent Event LogEntryAdded where
   eventName  _ _    =  "Log.entryAdded"
   fromEvent ev      =  case ev of EVLogEntryAdded v -> Just v; _ -> Nothing




logClear :: Handle Event -> IO (Maybe Error)
logClear handle = sendReceiveCommand handle "Log.clear" (Nothing :: Maybe ())




logDisable :: Handle Event -> IO (Maybe Error)
logDisable handle = sendReceiveCommand handle "Log.disable" (Nothing :: Maybe ())




logEnable :: Handle Event -> IO (Maybe Error)
logEnable handle = sendReceiveCommand handle "Log.enable" (Nothing :: Maybe ())




data PLogStartViolationsReport = PLogStartViolationsReport {
   pLogStartViolationsReportConfig :: [LogViolationSetting]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLogStartViolationsReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLogStartViolationsReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


logStartViolationsReport :: Handle Event -> PLogStartViolationsReport -> IO (Maybe Error)
logStartViolationsReport handle params = sendReceiveCommand handle "Log.startViolationsReport" (Just params)




logStopViolationsReport :: Handle Event -> IO (Maybe Error)
logStopViolationsReport handle = sendReceiveCommand handle "Log.stopViolationsReport" (Nothing :: Maybe ())





data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
   deriving (Ord, Eq, Show, Read)
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
   deriving (Ord, Eq, Show, Read)
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
type NetworkHeaders = [(T.Text, T.Text)]
data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
   deriving (Ord, Eq, Show, Read)
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
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkPostDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkPostDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


data NetworkRequestReferrerPolicy = NetworkRequestReferrerPolicyUnsafeUrl | NetworkRequestReferrerPolicyNoReferrerWhenDowngrade | NetworkRequestReferrerPolicyNoReferrer | NetworkRequestReferrerPolicyOrigin | NetworkRequestReferrerPolicyOriginWhenCrossOrigin | NetworkRequestReferrerPolicySameOrigin | NetworkRequestReferrerPolicyStrictOrigin | NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkRequestReferrerPolicy where
   parseJSON = A.withText  "NetworkRequestReferrerPolicy"  $ \v -> do
      case v of
         "unsafe-url" -> pure $ NetworkRequestReferrerPolicyUnsafeUrl
         "no-referrer-when-downgrade" -> pure $ NetworkRequestReferrerPolicyNoReferrerWhenDowngrade
         "no-referrer" -> pure $ NetworkRequestReferrerPolicyNoReferrer
         "origin" -> pure $ NetworkRequestReferrerPolicyOrigin
         "origin-when-cross-origin" -> pure $ NetworkRequestReferrerPolicyOriginWhenCrossOrigin
         "same-origin" -> pure $ NetworkRequestReferrerPolicySameOrigin
         "strict-origin" -> pure $ NetworkRequestReferrerPolicyStrictOrigin
         "strict-origin-when-cross-origin" -> pure $ NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
         _ -> fail "failed to parse NetworkRequestReferrerPolicy"

instance ToJSON NetworkRequestReferrerPolicy where
   toJSON v = A.String $
      case v of
         NetworkRequestReferrerPolicyUnsafeUrl -> "unsafe-url"
         NetworkRequestReferrerPolicyNoReferrerWhenDowngrade -> "no-referrer-when-downgrade"
         NetworkRequestReferrerPolicyNoReferrer -> "no-referrer"
         NetworkRequestReferrerPolicyOrigin -> "origin"
         NetworkRequestReferrerPolicyOriginWhenCrossOrigin -> "origin-when-cross-origin"
         NetworkRequestReferrerPolicySameOrigin -> "same-origin"
         NetworkRequestReferrerPolicyStrictOrigin -> "strict-origin"
         NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"



data NetworkRequest = NetworkRequest {
   networkRequestUrl :: String,
   networkRequestUrlFragment :: Maybe String,
   networkRequestMethod :: String,
   networkRequestHeaders :: NetworkHeaders,
   networkRequestPostData :: Maybe String,
   networkRequestHasPostData :: Maybe Bool,
   networkRequestMixedContentType :: Maybe SecurityMixedContentType,
   networkRequestInitialPriority :: NetworkResourcePriority,
   networkRequestReferrerPolicy :: NetworkRequestReferrerPolicy,
   networkRequestIsLinkPreload :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  NetworkRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
   networkSignedCertificateTimestampStatus :: String,
   networkSignedCertificateTimestampOrigin :: String,
   networkSignedCertificateTimestampLogDescription :: String,
   networkSignedCertificateTimestampLogId :: String,
   networkSignedCertificateTimestampTimestamp :: Double,
   networkSignedCertificateTimestampHashAlgorithm :: String,
   networkSignedCertificateTimestampSignatureAlgorithm :: String,
   networkSignedCertificateTimestampSignatureData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedCertificateTimestamp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedCertificateTimestamp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data NetworkSecurityDetails = NetworkSecurityDetails {
   networkSecurityDetailsProtocol :: String,
   networkSecurityDetailsKeyExchange :: String,
   networkSecurityDetailsKeyExchangeGroup :: Maybe String,
   networkSecurityDetailsCipher :: String,
   networkSecurityDetailsMac :: Maybe String,
   networkSecurityDetailsCertificateId :: SecurityCertificateId,
   networkSecurityDetailsSubjectName :: String,
   networkSecurityDetailsSanList :: [String],
   networkSecurityDetailsIssuer :: String,
   networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
   networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
   networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
   networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
   deriving (Ord, Eq, Show, Read)
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
   deriving (Ord, Eq, Show, Read)
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
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCorsErrorStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkCorsErrorStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
   deriving (Ord, Eq, Show, Read)
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
   networkResponseRequestHeaders :: Maybe NetworkHeaders,
   networkResponseConnectionReused :: Bool,
   networkResponseConnectionId :: Double,
   networkResponseRemoteIpAddress :: Maybe String,
   networkResponseRemotePort :: Maybe Int,
   networkResponseFromDiskCache :: Maybe Bool,
   networkResponseFromServiceWorker :: Maybe Bool,
   networkResponseFromPrefetchCache :: Maybe Bool,
   networkResponseEncodedDataLength :: Double,
   networkResponseTiming :: Maybe NetworkResourceTiming,
   networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
   networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
   networkResponseCacheStorageCacheName :: Maybe String,
   networkResponseProtocol :: Maybe String,
   networkResponseSecurityState :: SecuritySecurityState,
   networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  NetworkResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data NetworkWebSocketRequest = NetworkWebSocketRequest {
   networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data NetworkWebSocketResponse = NetworkWebSocketResponse {
   networkWebSocketResponseStatus :: Int,
   networkWebSocketResponseStatusText :: String,
   networkWebSocketResponseHeaders :: NetworkHeaders,
   networkWebSocketResponseHeadersText :: Maybe String,
   networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
   networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data NetworkWebSocketFrame = NetworkWebSocketFrame {
   networkWebSocketFrameOpcode :: Double,
   networkWebSocketFrameMask :: Bool,
   networkWebSocketFramePayloadData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data NetworkCachedResource = NetworkCachedResource {
   networkCachedResourceUrl :: String,
   networkCachedResourceType :: NetworkResourceType,
   networkCachedResourceResponse :: Maybe NetworkResponse,
   networkCachedResourceBodySize :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCachedResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkCachedResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


data NetworkInitiatorType = NetworkInitiatorTypeParser | NetworkInitiatorTypeScript | NetworkInitiatorTypePreload | NetworkInitiatorTypeSignedExchange | NetworkInitiatorTypePreflight | NetworkInitiatorTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkInitiatorType where
   parseJSON = A.withText  "NetworkInitiatorType"  $ \v -> do
      case v of
         "parser" -> pure $ NetworkInitiatorTypeParser
         "script" -> pure $ NetworkInitiatorTypeScript
         "preload" -> pure $ NetworkInitiatorTypePreload
         "SignedExchange" -> pure $ NetworkInitiatorTypeSignedExchange
         "preflight" -> pure $ NetworkInitiatorTypePreflight
         "other" -> pure $ NetworkInitiatorTypeOther
         _ -> fail "failed to parse NetworkInitiatorType"

instance ToJSON NetworkInitiatorType where
   toJSON v = A.String $
      case v of
         NetworkInitiatorTypeParser -> "parser"
         NetworkInitiatorTypeScript -> "script"
         NetworkInitiatorTypePreload -> "preload"
         NetworkInitiatorTypeSignedExchange -> "SignedExchange"
         NetworkInitiatorTypePreflight -> "preflight"
         NetworkInitiatorTypeOther -> "other"



data NetworkInitiator = NetworkInitiator {
   networkInitiatorType :: NetworkInitiatorType,
   networkInitiatorStack :: Maybe RuntimeStackTrace,
   networkInitiatorUrl :: Maybe String,
   networkInitiatorLineNumber :: Maybe Double,
   networkInitiatorColumnNumber :: Maybe Double,
   networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkInitiator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  NetworkInitiator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  NetworkCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookieParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  NetworkCookieParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }




data NetworkDataReceived = NetworkDataReceived {
   networkDataReceivedRequestId :: NetworkRequestId,
   networkDataReceivedTimestamp :: NetworkMonotonicTime,
   networkDataReceivedDataLength :: Int,
   networkDataReceivedEncodedDataLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkDataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  NetworkDataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance FromEvent Event NetworkDataReceived where
   eventName  _ _    =  "Network.dataReceived"
   fromEvent ev      =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing



data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
   networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
   networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
   networkEventSourceMessageReceivedEventName :: String,
   networkEventSourceMessageReceivedEventId :: String,
   networkEventSourceMessageReceivedData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkEventSourceMessageReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkEventSourceMessageReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance FromEvent Event NetworkEventSourceMessageReceived where
   eventName  _ _    =  "Network.eventSourceMessageReceived"
   fromEvent ev      =  case ev of EVNetworkEventSourceMessageReceived v -> Just v; _ -> Nothing



data NetworkLoadingFailed = NetworkLoadingFailed {
   networkLoadingFailedRequestId :: NetworkRequestId,
   networkLoadingFailedTimestamp :: NetworkMonotonicTime,
   networkLoadingFailedType :: NetworkResourceType,
   networkLoadingFailedErrorText :: String,
   networkLoadingFailedCanceled :: Maybe Bool,
   networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
   networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFailed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFailed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance FromEvent Event NetworkLoadingFailed where
   eventName  _ _    =  "Network.loadingFailed"
   fromEvent ev      =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing



data NetworkLoadingFinished = NetworkLoadingFinished {
   networkLoadingFinishedRequestId :: NetworkRequestId,
   networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
   networkLoadingFinishedEncodedDataLength :: Double,
   networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance FromEvent Event NetworkLoadingFinished where
   eventName  _ _    =  "Network.loadingFinished"
   fromEvent ev      =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing



data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
   networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestServedFromCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestServedFromCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance FromEvent Event NetworkRequestServedFromCache where
   eventName  _ _    =  "Network.requestServedFromCache"
   fromEvent ev      =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing



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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance FromEvent Event NetworkRequestWillBeSent where
   eventName  _ _    =  "Network.requestWillBeSent"
   fromEvent ev      =  case ev of EVNetworkRequestWillBeSent v -> Just v; _ -> Nothing



data NetworkResponseReceived = NetworkResponseReceived {
   networkResponseReceivedRequestId :: NetworkRequestId,
   networkResponseReceivedLoaderId :: NetworkLoaderId,
   networkResponseReceivedTimestamp :: NetworkMonotonicTime,
   networkResponseReceivedType :: NetworkResourceType,
   networkResponseReceivedResponse :: NetworkResponse,
   networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event NetworkResponseReceived where
   eventName  _ _    =  "Network.responseReceived"
   fromEvent ev      =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing



data NetworkWebSocketClosed = NetworkWebSocketClosed {
   networkWebSocketClosedRequestId :: NetworkRequestId,
   networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance FromEvent Event NetworkWebSocketClosed where
   eventName  _ _    =  "Network.webSocketClosed"
   fromEvent ev      =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing



data NetworkWebSocketCreated = NetworkWebSocketCreated {
   networkWebSocketCreatedRequestId :: NetworkRequestId,
   networkWebSocketCreatedUrl :: String,
   networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event NetworkWebSocketCreated where
   eventName  _ _    =  "Network.webSocketCreated"
   fromEvent ev      =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing



data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
   networkWebSocketFrameErrorRequestId :: NetworkRequestId,
   networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameErrorErrorMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance FromEvent Event NetworkWebSocketFrameError where
   eventName  _ _    =  "Network.webSocketFrameError"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing



data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
   networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
   networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance FromEvent Event NetworkWebSocketFrameReceived where
   eventName  _ _    =  "Network.webSocketFrameReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing



data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
   networkWebSocketFrameSentRequestId :: NetworkRequestId,
   networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance FromEvent Event NetworkWebSocketFrameSent where
   eventName  _ _    =  "Network.webSocketFrameSent"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing



data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
   networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
   networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
   networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


instance FromEvent Event NetworkWebSocketHandshakeResponseReceived where
   eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing



data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
   networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
   networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
   networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
   networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


instance FromEvent Event NetworkWebSocketWillSendHandshakeRequest where
   eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
   fromEvent ev      =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing



data NetworkWebTransportCreated = NetworkWebTransportCreated {
   networkWebTransportCreatedTransportId :: NetworkRequestId,
   networkWebTransportCreatedUrl :: String,
   networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
   networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance FromEvent Event NetworkWebTransportCreated where
   eventName  _ _    =  "Network.webTransportCreated"
   fromEvent ev      =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing



data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
   networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
   networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportConnectionEstablished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportConnectionEstablished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


instance FromEvent Event NetworkWebTransportConnectionEstablished where
   eventName  _ _    =  "Network.webTransportConnectionEstablished"
   fromEvent ev      =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing



data NetworkWebTransportClosed = NetworkWebTransportClosed {
   networkWebTransportClosedTransportId :: NetworkRequestId,
   networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance FromEvent Event NetworkWebTransportClosed where
   eventName  _ _    =  "Network.webTransportClosed"
   fromEvent ev      =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing




networkClearBrowserCache :: Handle Event -> IO (Maybe Error)
networkClearBrowserCache handle = sendReceiveCommand handle "Network.clearBrowserCache" (Nothing :: Maybe ())




networkClearBrowserCookies :: Handle Event -> IO (Maybe Error)
networkClearBrowserCookies handle = sendReceiveCommand handle "Network.clearBrowserCookies" (Nothing :: Maybe ())




data PNetworkDeleteCookies = PNetworkDeleteCookies {
   pNetworkDeleteCookiesName :: String,
   pNetworkDeleteCookiesUrl :: Maybe String,
   pNetworkDeleteCookiesDomain :: Maybe String,
   pNetworkDeleteCookiesPath :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkDeleteCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PNetworkDeleteCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEmulateNetworkConditions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PNetworkEmulateNetworkConditions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


networkEmulateNetworkConditions :: Handle Event -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions handle params = sendReceiveCommand handle "Network.emulateNetworkConditions" (Just params)




data PNetworkEnable = PNetworkEnable {
   pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


networkEnable :: Handle Event -> PNetworkEnable -> IO (Maybe Error)
networkEnable handle params = sendReceiveCommand handle "Network.enable" (Just params)




networkGetAllCookies :: Handle Event -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies handle = sendReceiveCommandResult handle "Network.getAllCookies" (Nothing :: Maybe ())

data NetworkGetAllCookies = NetworkGetAllCookies {
   networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetAllCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command NetworkGetAllCookies where
   commandName _ = "Network.getAllCookies"




data PNetworkGetCookies = PNetworkGetCookies {
   pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


networkGetCookies :: Handle Event -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies handle params = sendReceiveCommandResult handle "Network.getCookies" (Just params)

data NetworkGetCookies = NetworkGetCookies {
   networkGetCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command NetworkGetCookies where
   commandName _ = "Network.getCookies"




data PNetworkGetResponseBody = PNetworkGetResponseBody {
   pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


networkGetResponseBody :: Handle Event -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody handle params = sendReceiveCommandResult handle "Network.getResponseBody" (Just params)

data NetworkGetResponseBody = NetworkGetResponseBody {
   networkGetResponseBodyBody :: String,
   networkGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command NetworkGetResponseBody where
   commandName _ = "Network.getResponseBody"




data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
   pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetRequestPostData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


networkGetRequestPostData :: Handle Event -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData handle params = sendReceiveCommandResult handle "Network.getRequestPostData" (Just params)

data NetworkGetRequestPostData = NetworkGetRequestPostData {
   networkGetRequestPostDataPostData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command NetworkGetRequestPostData where
   commandName _ = "Network.getRequestPostData"




data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
   pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCacheDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCacheDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


networkSetCookie :: Handle Event -> PNetworkSetCookie -> IO (Either Error NetworkSetCookie)
networkSetCookie handle params = sendReceiveCommandResult handle "Network.setCookie" (Just params)

data NetworkSetCookie = NetworkSetCookie {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command NetworkSetCookie where
   commandName _ = "Network.setCookie"




data PNetworkSetCookies = PNetworkSetCookies {
   pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


networkSetCookies :: Handle Event -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies handle params = sendReceiveCommand handle "Network.setCookies" (Just params)




data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
   pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetExtraHttpHeaders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetExtraHttpHeaders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


networkSetExtraHttpHeaders :: Handle Event -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders handle params = sendReceiveCommand handle "Network.setExtraHTTPHeaders" (Just params)




data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
   pNetworkSetUserAgentOverrideUserAgent :: String,
   pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pNetworkSetUserAgentOverridePlatform :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


networkSetUserAgentOverride :: Handle Event -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)





type PageFrameId = String

data PageFrame = PageFrame {
   pageFrameId :: PageFrameId,
   pageFrameParentId :: Maybe PageFrameId,
   pageFrameLoaderId :: NetworkLoaderId,
   pageFrameName :: Maybe String,
   pageFrameUrl :: String,
   pageFrameSecurityOrigin :: String,
   pageFrameMimeType :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



data PageFrameTree = PageFrameTree {
   pageFrameTreeFrame :: PageFrame,
   pageFrameTreeChildFrames :: Maybe [PageFrameTree]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PageFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


type PageScriptIdentifier = String
data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddressBar | PageTransitionTypeAutoBookmark | PageTransitionTypeAutoSubframe | PageTransitionTypeManualSubframe | PageTransitionTypeGenerated | PageTransitionTypeAutoToplevel | PageTransitionTypeFormSubmit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeywordGenerated | PageTransitionTypeOther
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigationEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageNavigationEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data PageLayoutViewport = PageLayoutViewport {
   pageLayoutViewportPageX :: Int,
   pageLayoutViewportPageY :: Int,
   pageLayoutViewportClientWidth :: Int,
   pageLayoutViewportClientHeight :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLayoutViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLayoutViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageVisualViewport = PageVisualViewport {
   pageVisualViewportOffsetX :: Double,
   pageVisualViewportOffsetY :: Double,
   pageVisualViewportPageX :: Double,
   pageVisualViewportPageY :: Double,
   pageVisualViewportClientWidth :: Double,
   pageVisualViewportClientHeight :: Double,
   pageVisualViewportScale :: Double,
   pageVisualViewportZoom :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageVisualViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageVisualViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageViewport = PageViewport {
   pageViewportX :: Double,
   pageViewportY :: Double,
   pageViewportWidth :: Double,
   pageViewportHeight :: Double,
   pageViewportScale :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PageViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
   deriving (Ord, Eq, Show, Read)
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




data PageDomContentEventFired = PageDomContentEventFired {
   pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDomContentEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PageDomContentEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance FromEvent Event PageDomContentEventFired where
   eventName  _ _    =  "Page.domContentEventFired"
   fromEvent ev      =  case ev of EVPageDomContentEventFired v -> Just v; _ -> Nothing


data PageFileChooserOpenedMode = PageFileChooserOpenedModeSelectSingle | PageFileChooserOpenedModeSelectMultiple
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageFileChooserOpenedMode where
   parseJSON = A.withText  "PageFileChooserOpenedMode"  $ \v -> do
      case v of
         "selectSingle" -> pure $ PageFileChooserOpenedModeSelectSingle
         "selectMultiple" -> pure $ PageFileChooserOpenedModeSelectMultiple
         _ -> fail "failed to parse PageFileChooserOpenedMode"

instance ToJSON PageFileChooserOpenedMode where
   toJSON v = A.String $
      case v of
         PageFileChooserOpenedModeSelectSingle -> "selectSingle"
         PageFileChooserOpenedModeSelectMultiple -> "selectMultiple"



data PageFileChooserOpened = PageFileChooserOpened {
   pageFileChooserOpenedMode :: PageFileChooserOpenedMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFileChooserOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFileChooserOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance FromEvent Event PageFileChooserOpened where
   eventName  _ _    =  "Page.fileChooserOpened"
   fromEvent ev      =  case ev of EVPageFileChooserOpened v -> Just v; _ -> Nothing



data PageFrameAttached = PageFrameAttached {
   pageFrameAttachedFrameId :: PageFrameId,
   pageFrameAttachedParentFrameId :: PageFrameId,
   pageFrameAttachedStack :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameAttached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameAttached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance FromEvent Event PageFrameAttached where
   eventName  _ _    =  "Page.frameAttached"
   fromEvent ev      =  case ev of EVPageFrameAttached v -> Just v; _ -> Nothing



data PageFrameDetached = PageFrameDetached {
   pageFrameDetachedFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance FromEvent Event PageFrameDetached where
   eventName  _ _    =  "Page.frameDetached"
   fromEvent ev      =  case ev of EVPageFrameDetached v -> Just v; _ -> Nothing



data PageFrameNavigated = PageFrameNavigated {
   pageFrameNavigatedFrame :: PageFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameNavigated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageFrameNavigated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance FromEvent Event PageFrameNavigated where
   eventName  _ _    =  "Page.frameNavigated"
   fromEvent ev      =  case ev of EVPageFrameNavigated v -> Just v; _ -> Nothing


data PageInterstitialHidden = PageInterstitialHidden
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
   parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
      case v of
         "PageInterstitialHidden" -> pure $ PageInterstitialHidden
         _ -> fail "failed to parse PageInterstitialHidden"


instance FromEvent Event PageInterstitialHidden where
   eventName  _ _    =  "Page.interstitialHidden"
   fromEvent ev      =  case ev of EVPageInterstitialHidden v -> Just v; _ -> Nothing


data PageInterstitialShown = PageInterstitialShown
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
   parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
      case v of
         "PageInterstitialShown" -> pure $ PageInterstitialShown
         _ -> fail "failed to parse PageInterstitialShown"


instance FromEvent Event PageInterstitialShown where
   eventName  _ _    =  "Page.interstitialShown"
   fromEvent ev      =  case ev of EVPageInterstitialShown v -> Just v; _ -> Nothing



data PageJavascriptDialogClosed = PageJavascriptDialogClosed {
   pageJavascriptDialogClosedResult :: Bool,
   pageJavascriptDialogClosedUserInput :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance FromEvent Event PageJavascriptDialogClosed where
   eventName  _ _    =  "Page.javascriptDialogClosed"
   fromEvent ev      =  case ev of EVPageJavascriptDialogClosed v -> Just v; _ -> Nothing



data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
   pageJavascriptDialogOpeningUrl :: String,
   pageJavascriptDialogOpeningMessage :: String,
   pageJavascriptDialogOpeningType :: PageDialogType,
   pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
   pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogOpening  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogOpening where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance FromEvent Event PageJavascriptDialogOpening where
   eventName  _ _    =  "Page.javascriptDialogOpening"
   fromEvent ev      =  case ev of EVPageJavascriptDialogOpening v -> Just v; _ -> Nothing



data PageLifecycleEvent = PageLifecycleEvent {
   pageLifecycleEventFrameId :: PageFrameId,
   pageLifecycleEventLoaderId :: NetworkLoaderId,
   pageLifecycleEventName :: String,
   pageLifecycleEventTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLifecycleEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLifecycleEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance FromEvent Event PageLifecycleEvent where
   eventName  _ _    =  "Page.lifecycleEvent"
   fromEvent ev      =  case ev of EVPageLifecycleEvent v -> Just v; _ -> Nothing



data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
   pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
   pagePrerenderAttemptCompletedPrerenderingUrl :: String,
   pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePrerenderAttemptCompleted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PagePrerenderAttemptCompleted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance FromEvent Event PagePrerenderAttemptCompleted where
   eventName  _ _    =  "Page.prerenderAttemptCompleted"
   fromEvent ev      =  case ev of EVPagePrerenderAttemptCompleted v -> Just v; _ -> Nothing



data PageLoadEventFired = PageLoadEventFired {
   pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLoadEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLoadEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance FromEvent Event PageLoadEventFired where
   eventName  _ _    =  "Page.loadEventFired"
   fromEvent ev      =  case ev of EVPageLoadEventFired v -> Just v; _ -> Nothing



data PageWindowOpen = PageWindowOpen {
   pageWindowOpenUrl :: String,
   pageWindowOpenWindowName :: String,
   pageWindowOpenWindowFeatures :: [String],
   pageWindowOpenUserGesture :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageWindowOpen  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PageWindowOpen where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance FromEvent Event PageWindowOpen where
   eventName  _ _    =  "Page.windowOpen"
   fromEvent ev      =  case ev of EVPageWindowOpen v -> Just v; _ -> Nothing




data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
   pPageAddScriptToEvaluateOnNewDocumentSource :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


pageAddScriptToEvaluateOnNewDocument :: Handle Event -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument handle params = sendReceiveCommandResult handle "Page.addScriptToEvaluateOnNewDocument" (Just params)

data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
   pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PageAddScriptToEvaluateOnNewDocument where
   commandName _ = "Page.addScriptToEvaluateOnNewDocument"




pageBringToFront :: Handle Event -> IO (Maybe Error)
pageBringToFront handle = sendReceiveCommand handle "Page.bringToFront" (Nothing :: Maybe ())



data PPageCaptureScreenshotFormat = PPageCaptureScreenshotFormatJpeg | PPageCaptureScreenshotFormatPng | PPageCaptureScreenshotFormatWebp
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageCaptureScreenshotFormat where
   parseJSON = A.withText  "PPageCaptureScreenshotFormat"  $ \v -> do
      case v of
         "jpeg" -> pure $ PPageCaptureScreenshotFormatJpeg
         "png" -> pure $ PPageCaptureScreenshotFormatPng
         "webp" -> pure $ PPageCaptureScreenshotFormatWebp
         _ -> fail "failed to parse PPageCaptureScreenshotFormat"

instance ToJSON PPageCaptureScreenshotFormat where
   toJSON v = A.String $
      case v of
         PPageCaptureScreenshotFormatJpeg -> "jpeg"
         PPageCaptureScreenshotFormatPng -> "png"
         PPageCaptureScreenshotFormatWebp -> "webp"



data PPageCaptureScreenshot = PPageCaptureScreenshot {
   pPageCaptureScreenshotFormat :: PPageCaptureScreenshotFormat,
   pPageCaptureScreenshotQuality :: Maybe Int,
   pPageCaptureScreenshotClip :: Maybe PageViewport
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureScreenshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


pageCaptureScreenshot :: Handle Event -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot handle params = sendReceiveCommandResult handle "Page.captureScreenshot" (Just params)

data PageCaptureScreenshot = PageCaptureScreenshot {
   pageCaptureScreenshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PageCaptureScreenshot where
   commandName _ = "Page.captureScreenshot"




data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
   pPageCreateIsolatedWorldFrameId :: PageFrameId,
   pPageCreateIsolatedWorldWorldName :: Maybe String,
   pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCreateIsolatedWorld  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


pageCreateIsolatedWorld :: Handle Event -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld handle params = sendReceiveCommandResult handle "Page.createIsolatedWorld" (Just params)

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
   pageCreateIsolatedWorldExecutionContextId :: RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PageCreateIsolatedWorld where
   commandName _ = "Page.createIsolatedWorld"




pageDisable :: Handle Event -> IO (Maybe Error)
pageDisable handle = sendReceiveCommand handle "Page.disable" (Nothing :: Maybe ())




pageEnable :: Handle Event -> IO (Maybe Error)
pageEnable handle = sendReceiveCommand handle "Page.enable" (Nothing :: Maybe ())




pageGetAppManifest :: Handle Event -> IO (Either Error PageGetAppManifest)
pageGetAppManifest handle = sendReceiveCommandResult handle "Page.getAppManifest" (Nothing :: Maybe ())

data PageGetAppManifest = PageGetAppManifest {
   pageGetAppManifestUrl :: String,
   pageGetAppManifestErrors :: [PageAppManifestError],
   pageGetAppManifestData :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppManifest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PageGetAppManifest where
   commandName _ = "Page.getAppManifest"




pageGetFrameTree :: Handle Event -> IO (Either Error PageGetFrameTree)
pageGetFrameTree handle = sendReceiveCommandResult handle "Page.getFrameTree" (Nothing :: Maybe ())

data PageGetFrameTree = PageGetFrameTree {
   pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PageGetFrameTree where
   commandName _ = "Page.getFrameTree"




pageGetLayoutMetrics :: Handle Event -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics handle = sendReceiveCommandResult handle "Page.getLayoutMetrics" (Nothing :: Maybe ())

data PageGetLayoutMetrics = PageGetLayoutMetrics {
   pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
   pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
   pageGetLayoutMetricsCssContentSize :: DomRect
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetLayoutMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetLayoutMetrics where
   commandName _ = "Page.getLayoutMetrics"




pageGetNavigationHistory :: Handle Event -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory handle = sendReceiveCommandResult handle "Page.getNavigationHistory" (Nothing :: Maybe ())

data PageGetNavigationHistory = PageGetNavigationHistory {
   pageGetNavigationHistoryCurrentIndex :: Int,
   pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetNavigationHistory where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PageGetNavigationHistory where
   commandName _ = "Page.getNavigationHistory"




pageResetNavigationHistory :: Handle Event -> IO (Maybe Error)
pageResetNavigationHistory handle = sendReceiveCommand handle "Page.resetNavigationHistory" (Nothing :: Maybe ())




data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
   pPageHandleJavaScriptDialogAccept :: Bool,
   pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageHandleJavaScriptDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageHandleJavaScriptDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


pageHandleJavaScriptDialog :: Handle Event -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog handle params = sendReceiveCommand handle "Page.handleJavaScriptDialog" (Just params)




data PPageNavigate = PPageNavigate {
   pPageNavigateUrl :: String,
   pPageNavigateReferrer :: Maybe String,
   pPageNavigateTransitionType :: Maybe PageTransitionType,
   pPageNavigateFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PPageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


pageNavigate :: Handle Event -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate handle params = sendReceiveCommandResult handle "Page.navigate" (Just params)

data PageNavigate = PageNavigate {
   pageNavigateFrameId :: PageFrameId,
   pageNavigateLoaderId :: Maybe NetworkLoaderId,
   pageNavigateErrorText :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageNavigate where
   commandName _ = "Page.navigate"




data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
   pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigateToHistoryEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageNavigateToHistoryEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


pageNavigateToHistoryEntry :: Handle Event -> PPageNavigateToHistoryEntry -> IO (Maybe Error)
pageNavigateToHistoryEntry handle params = sendReceiveCommand handle "Page.navigateToHistoryEntry" (Just params)




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
   pPagePrintToPdfPreferCssPageSize :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPdf  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


pagePrintToPdf :: Handle Event -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)

data PagePrintToPdf = PagePrintToPdf {
   pagePrintToPdfData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PagePrintToPdf where
   commandName _ = "Page.printToPDF"




data PPageReload = PPageReload {
   pPageReloadIgnoreCache :: Maybe Bool,
   pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageReload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PPageReload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


pageReload :: Handle Event -> PPageReload -> IO (Maybe Error)
pageReload handle params = sendReceiveCommand handle "Page.reload" (Just params)




data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
   pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


pageRemoveScriptToEvaluateOnNewDocument :: Handle Event -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument handle params = sendReceiveCommand handle "Page.removeScriptToEvaluateOnNewDocument" (Just params)




data PPageSetDocumentContent = PPageSetDocumentContent {
   pPageSetDocumentContentFrameId :: PageFrameId,
   pPageSetDocumentContentHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetDocumentContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageSetDocumentContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


pageSetDocumentContent :: Handle Event -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent handle params = sendReceiveCommand handle "Page.setDocumentContent" (Just params)




pageStopLoading :: Handle Event -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())






data PerformanceMetric = PerformanceMetric {
   performanceMetricName :: String,
   performanceMetricValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetric  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetric where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }




data PerformanceMetrics = PerformanceMetrics {
   performanceMetricsMetrics :: [PerformanceMetric],
   performanceMetricsTitle :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetrics  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance FromEvent Event PerformanceMetrics where
   eventName  _ _    =  "Performance.metrics"
   fromEvent ev      =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing




performanceDisable :: Handle Event -> IO (Maybe Error)
performanceDisable handle = sendReceiveCommand handle "Performance.disable" (Nothing :: Maybe ())



data PPerformanceEnableTimeDomain = PPerformanceEnableTimeDomainTimeTicks | PPerformanceEnableTimeDomainThreadTicks
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPerformanceEnableTimeDomain where
   parseJSON = A.withText  "PPerformanceEnableTimeDomain"  $ \v -> do
      case v of
         "timeTicks" -> pure $ PPerformanceEnableTimeDomainTimeTicks
         "threadTicks" -> pure $ PPerformanceEnableTimeDomainThreadTicks
         _ -> fail "failed to parse PPerformanceEnableTimeDomain"

instance ToJSON PPerformanceEnableTimeDomain where
   toJSON v = A.String $
      case v of
         PPerformanceEnableTimeDomainTimeTicks -> "timeTicks"
         PPerformanceEnableTimeDomainThreadTicks -> "threadTicks"



data PPerformanceEnable = PPerformanceEnable {
   pPerformanceEnableTimeDomain :: PPerformanceEnableTimeDomain
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPerformanceEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PPerformanceEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


performanceEnable :: Handle Event -> PPerformanceEnable -> IO (Maybe Error)
performanceEnable handle params = sendReceiveCommand handle "Performance.enable" (Just params)




performanceGetMetrics :: Handle Event -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics handle = sendReceiveCommandResult handle "Performance.getMetrics" (Nothing :: Maybe ())

data PerformanceGetMetrics = PerformanceGetMetrics {
   performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PerformanceGetMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PerformanceGetMetrics where
   commandName _ = "Performance.getMetrics"





type SecurityCertificateId = Int
data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
   deriving (Ord, Eq, Show, Read)
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
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySecurityStateExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecuritySecurityStateExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
   deriving (Ord, Eq, Show, Read)
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





type TargetTargetId = String
type TargetSessionId = String

data TargetTargetInfo = TargetTargetInfo {
   targetTargetInfoTargetId :: TargetTargetId,
   targetTargetInfoType :: String,
   targetTargetInfoTitle :: String,
   targetTargetInfoUrl :: String,
   targetTargetInfoAttached :: Bool,
   targetTargetInfoOpenerId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }




data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
   targetReceivedMessageFromTargetSessionId :: TargetSessionId,
   targetReceivedMessageFromTargetMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetReceivedMessageFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  TargetReceivedMessageFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance FromEvent Event TargetReceivedMessageFromTarget where
   eventName  _ _    =  "Target.receivedMessageFromTarget"
   fromEvent ev      =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing



data TargetTargetCreated = TargetTargetCreated {
   targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance FromEvent Event TargetTargetCreated where
   eventName  _ _    =  "Target.targetCreated"
   fromEvent ev      =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing



data TargetTargetDestroyed = TargetTargetDestroyed {
   targetTargetDestroyedTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  TargetTargetDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance FromEvent Event TargetTargetDestroyed where
   eventName  _ _    =  "Target.targetDestroyed"
   fromEvent ev      =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing



data TargetTargetCrashed = TargetTargetCrashed {
   targetTargetCrashedTargetId :: TargetTargetId,
   targetTargetCrashedStatus :: String,
   targetTargetCrashedErrorCode :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCrashed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCrashed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance FromEvent Event TargetTargetCrashed where
   eventName  _ _    =  "Target.targetCrashed"
   fromEvent ev      =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing



data TargetTargetInfoChanged = TargetTargetInfoChanged {
   targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfoChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfoChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event TargetTargetInfoChanged where
   eventName  _ _    =  "Target.targetInfoChanged"
   fromEvent ev      =  case ev of EVTargetTargetInfoChanged v -> Just v; _ -> Nothing




data PTargetActivateTarget = PTargetActivateTarget {
   pTargetActivateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetActivateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetActivateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


targetActivateTarget :: Handle Event -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget handle params = sendReceiveCommand handle "Target.activateTarget" (Just params)




data PTargetAttachToTarget = PTargetAttachToTarget {
   pTargetAttachToTargetTargetId :: TargetTargetId,
   pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAttachToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


targetAttachToTarget :: Handle Event -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget handle params = sendReceiveCommandResult handle "Target.attachToTarget" (Just params)

data TargetAttachToTarget = TargetAttachToTarget {
   targetAttachToTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TargetAttachToTarget where
   commandName _ = "Target.attachToTarget"




data PTargetCloseTarget = PTargetCloseTarget {
   pTargetCloseTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCloseTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PTargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


targetCloseTarget :: Handle Event -> PTargetCloseTarget -> IO (Either Error TargetCloseTarget)
targetCloseTarget handle params = sendReceiveCommandResult handle "Target.closeTarget" (Just params)

data TargetCloseTarget = TargetCloseTarget {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command TargetCloseTarget where
   commandName _ = "Target.closeTarget"




data PTargetCreateTarget = PTargetCreateTarget {
   pTargetCreateTargetUrl :: String,
   pTargetCreateTargetWidth :: Maybe Int,
   pTargetCreateTargetHeight :: Maybe Int,
   pTargetCreateTargetNewWindow :: Maybe Bool,
   pTargetCreateTargetBackground :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


targetCreateTarget :: Handle Event -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget handle params = sendReceiveCommandResult handle "Target.createTarget" (Just params)

data TargetCreateTarget = TargetCreateTarget {
   targetCreateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command TargetCreateTarget where
   commandName _ = "Target.createTarget"




data PTargetDetachFromTarget = PTargetDetachFromTarget {
   pTargetDetachFromTargetSessionId :: Maybe TargetSessionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDetachFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PTargetDetachFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


targetDetachFromTarget :: Handle Event -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget handle params = sendReceiveCommand handle "Target.detachFromTarget" (Just params)




targetGetTargets :: Handle Event -> IO (Either Error TargetGetTargets)
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())

data TargetGetTargets = TargetGetTargets {
   targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command TargetGetTargets where
   commandName _ = "Target.getTargets"




data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
   pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetDiscoverTargets  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetDiscoverTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


targetSetDiscoverTargets :: Handle Event -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)





type FetchRequestId = String
data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
   deriving (Ord, Eq, Show, Read)
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data FetchHeaderEntry = FetchHeaderEntry {
   fetchHeaderEntryName :: String,
   fetchHeaderEntryValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchHeaderEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  FetchHeaderEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


data FetchAuthChallengeSource = FetchAuthChallengeSourceServer | FetchAuthChallengeSourceProxy
   deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeSource where
   parseJSON = A.withText  "FetchAuthChallengeSource"  $ \v -> do
      case v of
         "Server" -> pure $ FetchAuthChallengeSourceServer
         "Proxy" -> pure $ FetchAuthChallengeSourceProxy
         _ -> fail "failed to parse FetchAuthChallengeSource"

instance ToJSON FetchAuthChallengeSource where
   toJSON v = A.String $
      case v of
         FetchAuthChallengeSourceServer -> "Server"
         FetchAuthChallengeSourceProxy -> "Proxy"



data FetchAuthChallenge = FetchAuthChallenge {
   fetchAuthChallengeSource :: FetchAuthChallengeSource,
   fetchAuthChallengeOrigin :: String,
   fetchAuthChallengeScheme :: String,
   fetchAuthChallengeRealm :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallenge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallenge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


data FetchAuthChallengeResponseResponse = FetchAuthChallengeResponseResponseDefault | FetchAuthChallengeResponseResponseCancelAuth | FetchAuthChallengeResponseResponseProvideCredentials
   deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeResponseResponse where
   parseJSON = A.withText  "FetchAuthChallengeResponseResponse"  $ \v -> do
      case v of
         "Default" -> pure $ FetchAuthChallengeResponseResponseDefault
         "CancelAuth" -> pure $ FetchAuthChallengeResponseResponseCancelAuth
         "ProvideCredentials" -> pure $ FetchAuthChallengeResponseResponseProvideCredentials
         _ -> fail "failed to parse FetchAuthChallengeResponseResponse"

instance ToJSON FetchAuthChallengeResponseResponse where
   toJSON v = A.String $
      case v of
         FetchAuthChallengeResponseResponseDefault -> "Default"
         FetchAuthChallengeResponseResponseCancelAuth -> "CancelAuth"
         FetchAuthChallengeResponseResponseProvideCredentials -> "ProvideCredentials"



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
   fetchAuthChallengeResponseResponse :: FetchAuthChallengeResponseResponse,
   fetchAuthChallengeResponseUsername :: Maybe String,
   fetchAuthChallengeResponsePassword :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }




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
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance FromEvent Event FetchRequestPaused where
   eventName  _ _    =  "Fetch.requestPaused"
   fromEvent ev      =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing



data FetchAuthRequired = FetchAuthRequired {
   fetchAuthRequiredRequestId :: FetchRequestId,
   fetchAuthRequiredRequest :: NetworkRequest,
   fetchAuthRequiredFrameId :: PageFrameId,
   fetchAuthRequiredResourceType :: NetworkResourceType,
   fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthRequired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  FetchAuthRequired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance FromEvent Event FetchAuthRequired where
   eventName  _ _    =  "Fetch.authRequired"
   fromEvent ev      =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing




fetchDisable :: Handle Event -> IO (Maybe Error)
fetchDisable handle = sendReceiveCommand handle "Fetch.disable" (Nothing :: Maybe ())




data PFetchEnable = PFetchEnable {
   pFetchEnablePatterns :: Maybe [FetchRequestPattern],
   pFetchEnableHandleAuthRequests :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PFetchEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


fetchEnable :: Handle Event -> PFetchEnable -> IO (Maybe Error)
fetchEnable handle params = sendReceiveCommand handle "Fetch.enable" (Just params)




data PFetchFailRequest = PFetchFailRequest {
   pFetchFailRequestRequestId :: FetchRequestId,
   pFetchFailRequestErrorReason :: NetworkErrorReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFailRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PFetchFailRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


fetchFailRequest :: Handle Event -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest handle params = sendReceiveCommand handle "Fetch.failRequest" (Just params)




data PFetchFulfillRequest = PFetchFulfillRequest {
   pFetchFulfillRequestRequestId :: FetchRequestId,
   pFetchFulfillRequestResponseCode :: Int,
   pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
   pFetchFulfillRequestBinaryResponseHeaders :: Maybe String,
   pFetchFulfillRequestBody :: Maybe String,
   pFetchFulfillRequestResponsePhrase :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFulfillRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PFetchFulfillRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


fetchFulfillRequest :: Handle Event -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest handle params = sendReceiveCommand handle "Fetch.fulfillRequest" (Just params)




data PFetchContinueRequest = PFetchContinueRequest {
   pFetchContinueRequestRequestId :: FetchRequestId,
   pFetchContinueRequestUrl :: Maybe String,
   pFetchContinueRequestMethod :: Maybe String,
   pFetchContinueRequestPostData :: Maybe String,
   pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


fetchContinueRequest :: Handle Event -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest handle params = sendReceiveCommand handle "Fetch.continueRequest" (Just params)




data PFetchContinueWithAuth = PFetchContinueWithAuth {
   pFetchContinueWithAuthRequestId :: FetchRequestId,
   pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueWithAuth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueWithAuth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


fetchContinueWithAuth :: Handle Event -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth handle params = sendReceiveCommand handle "Fetch.continueWithAuth" (Just params)




data PFetchGetResponseBody = PFetchGetResponseBody {
   pFetchGetResponseBodyRequestId :: FetchRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


fetchGetResponseBody :: Handle Event -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody handle params = sendReceiveCommandResult handle "Fetch.getResponseBody" (Just params)

data FetchGetResponseBody = FetchGetResponseBody {
   fetchGetResponseBodyBody :: String,
   fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command FetchGetResponseBody where
   commandName _ = "Fetch.getResponseBody"




data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
   pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchTakeResponseBodyAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PFetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


fetchTakeResponseBodyAsStream :: Handle Event -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream handle params = sendReceiveCommandResult handle "Fetch.takeResponseBodyAsStream" (Just params)

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
   fetchTakeResponseBodyAsStreamStream :: IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command FetchTakeResponseBodyAsStream where
   commandName _ = "Fetch.takeResponseBodyAsStream"





type DebuggerBreakpointId = String
type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
   debuggerLocationScriptId :: RuntimeScriptId,
   debuggerLocationLineNumber :: Int,
   debuggerLocationColumnNumber :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data DebuggerCallFrame = DebuggerCallFrame {
   debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
   debuggerCallFrameFunctionName :: String,
   debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
   debuggerCallFrameLocation :: DebuggerLocation,
   debuggerCallFrameScopeChain :: [DebuggerScope],
   debuggerCallFrameThis :: RuntimeRemoteObject,
   debuggerCallFrameReturnValue :: Maybe RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  DebuggerCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


data DebuggerScopeType = DebuggerScopeTypeGlobal | DebuggerScopeTypeLocal | DebuggerScopeTypeWith | DebuggerScopeTypeClosure | DebuggerScopeTypeCatch | DebuggerScopeTypeBlock | DebuggerScopeTypeScript | DebuggerScopeTypeEval | DebuggerScopeTypeModule | DebuggerScopeTypeWasmExpressionStack
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerScopeType where
   parseJSON = A.withText  "DebuggerScopeType"  $ \v -> do
      case v of
         "global" -> pure $ DebuggerScopeTypeGlobal
         "local" -> pure $ DebuggerScopeTypeLocal
         "with" -> pure $ DebuggerScopeTypeWith
         "closure" -> pure $ DebuggerScopeTypeClosure
         "catch" -> pure $ DebuggerScopeTypeCatch
         "block" -> pure $ DebuggerScopeTypeBlock
         "script" -> pure $ DebuggerScopeTypeScript
         "eval" -> pure $ DebuggerScopeTypeEval
         "module" -> pure $ DebuggerScopeTypeModule
         "wasm-expression-stack" -> pure $ DebuggerScopeTypeWasmExpressionStack
         _ -> fail "failed to parse DebuggerScopeType"

instance ToJSON DebuggerScopeType where
   toJSON v = A.String $
      case v of
         DebuggerScopeTypeGlobal -> "global"
         DebuggerScopeTypeLocal -> "local"
         DebuggerScopeTypeWith -> "with"
         DebuggerScopeTypeClosure -> "closure"
         DebuggerScopeTypeCatch -> "catch"
         DebuggerScopeTypeBlock -> "block"
         DebuggerScopeTypeScript -> "script"
         DebuggerScopeTypeEval -> "eval"
         DebuggerScopeTypeModule -> "module"
         DebuggerScopeTypeWasmExpressionStack -> "wasm-expression-stack"



data DebuggerScope = DebuggerScope {
   debuggerScopeType :: DebuggerScopeType,
   debuggerScopeObject :: RuntimeRemoteObject,
   debuggerScopeName :: Maybe String,
   debuggerScopeStartLocation :: Maybe DebuggerLocation,
   debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScope  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DebuggerScope where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



data DebuggerSearchMatch = DebuggerSearchMatch {
   debuggerSearchMatchLineNumber :: Double,
   debuggerSearchMatchLineContent :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerSearchMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DebuggerSearchMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


data DebuggerBreakLocationType = DebuggerBreakLocationTypeDebuggerStatement | DebuggerBreakLocationTypeCall | DebuggerBreakLocationTypeReturn
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerBreakLocationType where
   parseJSON = A.withText  "DebuggerBreakLocationType"  $ \v -> do
      case v of
         "debuggerStatement" -> pure $ DebuggerBreakLocationTypeDebuggerStatement
         "call" -> pure $ DebuggerBreakLocationTypeCall
         "return" -> pure $ DebuggerBreakLocationTypeReturn
         _ -> fail "failed to parse DebuggerBreakLocationType"

instance ToJSON DebuggerBreakLocationType where
   toJSON v = A.String $
      case v of
         DebuggerBreakLocationTypeDebuggerStatement -> "debuggerStatement"
         DebuggerBreakLocationTypeCall -> "call"
         DebuggerBreakLocationTypeReturn -> "return"



data DebuggerBreakLocation = DebuggerBreakLocation {
   debuggerBreakLocationScriptId :: RuntimeScriptId,
   debuggerBreakLocationLineNumber :: Int,
   debuggerBreakLocationColumnNumber :: Maybe Int,
   debuggerBreakLocationType :: DebuggerBreakLocationType
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
   deriving (Ord, Eq, Show, Read)
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


data DebuggerDebugSymbolsType = DebuggerDebugSymbolsTypeNone | DebuggerDebugSymbolsTypeSourceMap | DebuggerDebugSymbolsTypeEmbeddedDwarf | DebuggerDebugSymbolsTypeExternalDwarf
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerDebugSymbolsType where
   parseJSON = A.withText  "DebuggerDebugSymbolsType"  $ \v -> do
      case v of
         "None" -> pure $ DebuggerDebugSymbolsTypeNone
         "SourceMap" -> pure $ DebuggerDebugSymbolsTypeSourceMap
         "EmbeddedDWARF" -> pure $ DebuggerDebugSymbolsTypeEmbeddedDwarf
         "ExternalDWARF" -> pure $ DebuggerDebugSymbolsTypeExternalDwarf
         _ -> fail "failed to parse DebuggerDebugSymbolsType"

instance ToJSON DebuggerDebugSymbolsType where
   toJSON v = A.String $
      case v of
         DebuggerDebugSymbolsTypeNone -> "None"
         DebuggerDebugSymbolsTypeSourceMap -> "SourceMap"
         DebuggerDebugSymbolsTypeEmbeddedDwarf -> "EmbeddedDWARF"
         DebuggerDebugSymbolsTypeExternalDwarf -> "ExternalDWARF"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
   debuggerDebugSymbolsType :: DebuggerDebugSymbolsType,
   debuggerDebugSymbolsExternalUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerDebugSymbols  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerDebugSymbols where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }




data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
   debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
   debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakpointResolved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakpointResolved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance FromEvent Event DebuggerBreakpointResolved where
   eventName  _ _    =  "Debugger.breakpointResolved"
   fromEvent ev      =  case ev of EVDebuggerBreakpointResolved v -> Just v; _ -> Nothing


data DebuggerPausedReason = DebuggerPausedReasonAmbiguous | DebuggerPausedReasonAssert | DebuggerPausedReasonCspViolation | DebuggerPausedReasonDebugCommand | DebuggerPausedReasonDom | DebuggerPausedReasonEventListener | DebuggerPausedReasonException | DebuggerPausedReasonInstrumentation | DebuggerPausedReasonOom | DebuggerPausedReasonOther | DebuggerPausedReasonPromiseRejection | DebuggerPausedReasonXhr
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerPausedReason where
   parseJSON = A.withText  "DebuggerPausedReason"  $ \v -> do
      case v of
         "ambiguous" -> pure $ DebuggerPausedReasonAmbiguous
         "assert" -> pure $ DebuggerPausedReasonAssert
         "CSPViolation" -> pure $ DebuggerPausedReasonCspViolation
         "debugCommand" -> pure $ DebuggerPausedReasonDebugCommand
         "DOM" -> pure $ DebuggerPausedReasonDom
         "EventListener" -> pure $ DebuggerPausedReasonEventListener
         "exception" -> pure $ DebuggerPausedReasonException
         "instrumentation" -> pure $ DebuggerPausedReasonInstrumentation
         "OOM" -> pure $ DebuggerPausedReasonOom
         "other" -> pure $ DebuggerPausedReasonOther
         "promiseRejection" -> pure $ DebuggerPausedReasonPromiseRejection
         "XHR" -> pure $ DebuggerPausedReasonXhr
         _ -> fail "failed to parse DebuggerPausedReason"

instance ToJSON DebuggerPausedReason where
   toJSON v = A.String $
      case v of
         DebuggerPausedReasonAmbiguous -> "ambiguous"
         DebuggerPausedReasonAssert -> "assert"
         DebuggerPausedReasonCspViolation -> "CSPViolation"
         DebuggerPausedReasonDebugCommand -> "debugCommand"
         DebuggerPausedReasonDom -> "DOM"
         DebuggerPausedReasonEventListener -> "EventListener"
         DebuggerPausedReasonException -> "exception"
         DebuggerPausedReasonInstrumentation -> "instrumentation"
         DebuggerPausedReasonOom -> "OOM"
         DebuggerPausedReasonOther -> "other"
         DebuggerPausedReasonPromiseRejection -> "promiseRejection"
         DebuggerPausedReasonXhr -> "XHR"



data DebuggerPaused = DebuggerPaused {
   debuggerPausedCallFrames :: [DebuggerCallFrame],
   debuggerPausedReason :: DebuggerPausedReason,
   debuggerPausedData :: Maybe [(T.Text, T.Text)],
   debuggerPausedHitBreakpoints :: Maybe [String],
   debuggerPausedAsyncStackTrace :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DebuggerPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance FromEvent Event DebuggerPaused where
   eventName  _ _    =  "Debugger.paused"
   fromEvent ev      =  case ev of EVDebuggerPaused v -> Just v; _ -> Nothing


data DebuggerResumed = DebuggerResumed
   deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
   parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
      case v of
         "DebuggerResumed" -> pure $ DebuggerResumed
         _ -> fail "failed to parse DebuggerResumed"


instance FromEvent Event DebuggerResumed where
   eventName  _ _    =  "Debugger.resumed"
   fromEvent ev      =  case ev of EVDebuggerResumed v -> Just v; _ -> Nothing



data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
   debuggerScriptFailedToParseScriptId :: RuntimeScriptId,
   debuggerScriptFailedToParseUrl :: String,
   debuggerScriptFailedToParseStartLine :: Int,
   debuggerScriptFailedToParseStartColumn :: Int,
   debuggerScriptFailedToParseEndLine :: Int,
   debuggerScriptFailedToParseEndColumn :: Int,
   debuggerScriptFailedToParseExecutionContextId :: RuntimeExecutionContextId,
   debuggerScriptFailedToParseHash :: String,
   debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(T.Text, T.Text)],
   debuggerScriptFailedToParseSourceMapUrl :: Maybe String,
   debuggerScriptFailedToParseHasSourceUrl :: Maybe Bool,
   debuggerScriptFailedToParseIsModule :: Maybe Bool,
   debuggerScriptFailedToParseLength :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptFailedToParse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptFailedToParse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance FromEvent Event DebuggerScriptFailedToParse where
   eventName  _ _    =  "Debugger.scriptFailedToParse"
   fromEvent ev      =  case ev of EVDebuggerScriptFailedToParse v -> Just v; _ -> Nothing



data DebuggerScriptParsed = DebuggerScriptParsed {
   debuggerScriptParsedScriptId :: RuntimeScriptId,
   debuggerScriptParsedUrl :: String,
   debuggerScriptParsedStartLine :: Int,
   debuggerScriptParsedStartColumn :: Int,
   debuggerScriptParsedEndLine :: Int,
   debuggerScriptParsedEndColumn :: Int,
   debuggerScriptParsedExecutionContextId :: RuntimeExecutionContextId,
   debuggerScriptParsedHash :: String,
   debuggerScriptParsedExecutionContextAuxData :: Maybe [(T.Text, T.Text)],
   debuggerScriptParsedSourceMapUrl :: Maybe String,
   debuggerScriptParsedHasSourceUrl :: Maybe Bool,
   debuggerScriptParsedIsModule :: Maybe Bool,
   debuggerScriptParsedLength :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance FromEvent Event DebuggerScriptParsed where
   eventName  _ _    =  "Debugger.scriptParsed"
   fromEvent ev      =  case ev of EVDebuggerScriptParsed v -> Just v; _ -> Nothing



data PDebuggerContinueToLocationTargetCallFrames = PDebuggerContinueToLocationTargetCallFramesAny | PDebuggerContinueToLocationTargetCallFramesCurrent
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerContinueToLocationTargetCallFrames where
   parseJSON = A.withText  "PDebuggerContinueToLocationTargetCallFrames"  $ \v -> do
      case v of
         "any" -> pure $ PDebuggerContinueToLocationTargetCallFramesAny
         "current" -> pure $ PDebuggerContinueToLocationTargetCallFramesCurrent
         _ -> fail "failed to parse PDebuggerContinueToLocationTargetCallFrames"

instance ToJSON PDebuggerContinueToLocationTargetCallFrames where
   toJSON v = A.String $
      case v of
         PDebuggerContinueToLocationTargetCallFramesAny -> "any"
         PDebuggerContinueToLocationTargetCallFramesCurrent -> "current"



data PDebuggerContinueToLocation = PDebuggerContinueToLocation {
   pDebuggerContinueToLocationLocation :: DebuggerLocation,
   pDebuggerContinueToLocationTargetCallFrames :: PDebuggerContinueToLocationTargetCallFrames
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerContinueToLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerContinueToLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


debuggerContinueToLocation :: Handle Event -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation handle params = sendReceiveCommand handle "Debugger.continueToLocation" (Just params)




debuggerDisable :: Handle Event -> IO (Maybe Error)
debuggerDisable handle = sendReceiveCommand handle "Debugger.disable" (Nothing :: Maybe ())



type PDebuggerEnable = [(T.Text, T.Text)]
debuggerEnable :: Handle Event -> PDebuggerEnable -> IO (Either Error DebuggerEnable)
debuggerEnable handle params = sendReceiveCommandResult handle "Debugger.enable" (Just params)

data DebuggerEnable = DebuggerEnable {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DebuggerEnable where
   commandName _ = "Debugger.enable"




data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
   pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
   pDebuggerEvaluateOnCallFrameExpression :: String,
   pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
   pDebuggerEvaluateOnCallFrameIncludeCommandLineApi :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEvaluateOnCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


debuggerEvaluateOnCallFrame :: Handle Event -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame handle params = sendReceiveCommandResult handle "Debugger.evaluateOnCallFrame" (Just params)

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
   debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
   debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DebuggerEvaluateOnCallFrame where
   commandName _ = "Debugger.evaluateOnCallFrame"




data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
   pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
   pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
   pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetPossibleBreakpoints  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


debuggerGetPossibleBreakpoints :: Handle Event -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints handle params = sendReceiveCommandResult handle "Debugger.getPossibleBreakpoints" (Just params)

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
   debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command DebuggerGetPossibleBreakpoints where
   commandName _ = "Debugger.getPossibleBreakpoints"




data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
   pDebuggerGetScriptSourceScriptId :: RuntimeScriptId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerGetScriptSource :: Handle Event -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource handle params = sendReceiveCommandResult handle "Debugger.getScriptSource" (Just params)

data DebuggerGetScriptSource = DebuggerGetScriptSource {
   debuggerGetScriptSourceScriptSource :: String,
   debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerGetScriptSource where
   commandName _ = "Debugger.getScriptSource"




debuggerPause :: Handle Event -> IO (Maybe Error)
debuggerPause handle = sendReceiveCommand handle "Debugger.pause" (Nothing :: Maybe ())




data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
   pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerRemoveBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerRemoveBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerRemoveBreakpoint :: Handle Event -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint handle params = sendReceiveCommand handle "Debugger.removeBreakpoint" (Just params)




data PDebuggerResume = PDebuggerResume {
   pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerResume  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerResume where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


debuggerResume :: Handle Event -> PDebuggerResume -> IO (Maybe Error)
debuggerResume handle params = sendReceiveCommand handle "Debugger.resume" (Just params)




data PDebuggerSearchInContent = PDebuggerSearchInContent {
   pDebuggerSearchInContentScriptId :: RuntimeScriptId,
   pDebuggerSearchInContentQuery :: String,
   pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
   pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSearchInContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerSearchInContent :: Handle Event -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent handle params = sendReceiveCommandResult handle "Debugger.searchInContent" (Just params)

data DebuggerSearchInContent = DebuggerSearchInContent {
   debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSearchInContent where
   commandName _ = "Debugger.searchInContent"




data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
   pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


debuggerSetAsyncCallStackDepth :: Handle Event -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Debugger.setAsyncCallStackDepth" (Just params)




data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
   pDebuggerSetBreakpointLocation :: DebuggerLocation,
   pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


debuggerSetBreakpoint :: Handle Event -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setBreakpoint" (Just params)

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
   debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
   debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DebuggerSetBreakpoint where
   commandName _ = "Debugger.setBreakpoint"



data PDebuggerSetInstrumentationBreakpointInstrumentation = PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution | PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
   parseJSON = A.withText  "PDebuggerSetInstrumentationBreakpointInstrumentation"  $ \v -> do
      case v of
         "beforeScriptExecution" -> pure $ PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution
         "beforeScriptWithSourceMapExecution" -> pure $ PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
         _ -> fail "failed to parse PDebuggerSetInstrumentationBreakpointInstrumentation"

instance ToJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
   toJSON v = A.String $
      case v of
         PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution -> "beforeScriptExecution"
         PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution -> "beforeScriptWithSourceMapExecution"



data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint {
   pDebuggerSetInstrumentationBreakpointInstrumentation :: PDebuggerSetInstrumentationBreakpointInstrumentation
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


debuggerSetInstrumentationBreakpoint :: Handle Event -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setInstrumentationBreakpoint" (Just params)

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
   debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command DebuggerSetInstrumentationBreakpoint where
   commandName _ = "Debugger.setInstrumentationBreakpoint"




data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
   pDebuggerSetBreakpointByUrlLineNumber :: Int,
   pDebuggerSetBreakpointByUrlUrl :: Maybe String,
   pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
   pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
   pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
   pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointByUrl  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


debuggerSetBreakpointByUrl :: Handle Event -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl handle params = sendReceiveCommandResult handle "Debugger.setBreakpointByUrl" (Just params)

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
   debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
   debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DebuggerSetBreakpointByUrl where
   commandName _ = "Debugger.setBreakpointByUrl"




data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
   pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointsActive  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointsActive where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


debuggerSetBreakpointsActive :: Handle Event -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive handle params = sendReceiveCommand handle "Debugger.setBreakpointsActive" (Just params)



data PDebuggerSetPauseOnExceptionsState = PDebuggerSetPauseOnExceptionsStateNone | PDebuggerSetPauseOnExceptionsStateUncaught | PDebuggerSetPauseOnExceptionsStateAll
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetPauseOnExceptionsState where
   parseJSON = A.withText  "PDebuggerSetPauseOnExceptionsState"  $ \v -> do
      case v of
         "none" -> pure $ PDebuggerSetPauseOnExceptionsStateNone
         "uncaught" -> pure $ PDebuggerSetPauseOnExceptionsStateUncaught
         "all" -> pure $ PDebuggerSetPauseOnExceptionsStateAll
         _ -> fail "failed to parse PDebuggerSetPauseOnExceptionsState"

instance ToJSON PDebuggerSetPauseOnExceptionsState where
   toJSON v = A.String $
      case v of
         PDebuggerSetPauseOnExceptionsStateNone -> "none"
         PDebuggerSetPauseOnExceptionsStateUncaught -> "uncaught"
         PDebuggerSetPauseOnExceptionsStateAll -> "all"



data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions {
   pDebuggerSetPauseOnExceptionsState :: PDebuggerSetPauseOnExceptionsState
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetPauseOnExceptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetPauseOnExceptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


debuggerSetPauseOnExceptions :: Handle Event -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions handle params = sendReceiveCommand handle "Debugger.setPauseOnExceptions" (Just params)




data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
   pDebuggerSetScriptSourceScriptId :: RuntimeScriptId,
   pDebuggerSetScriptSourceScriptSource :: String,
   pDebuggerSetScriptSourceDryRun :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerSetScriptSource :: Handle Event -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource handle params = sendReceiveCommandResult handle "Debugger.setScriptSource" (Just params)

data DebuggerSetScriptSource = DebuggerSetScriptSource {
   debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
   debuggerSetScriptSourceStackChanged :: Maybe Bool,
   debuggerSetScriptSourceAsyncStackTrace :: Maybe RuntimeStackTrace,
   debuggerSetScriptSourceExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSetScriptSource where
   commandName _ = "Debugger.setScriptSource"




data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
   pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetSkipAllPauses  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetSkipAllPauses where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerSetSkipAllPauses :: Handle Event -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses handle params = sendReceiveCommand handle "Debugger.setSkipAllPauses" (Just params)




data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
   pDebuggerSetVariableValueScopeNumber :: Int,
   pDebuggerSetVariableValueVariableName :: String,
   pDebuggerSetVariableValueNewValue :: RuntimeCallArgument,
   pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetVariableValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetVariableValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerSetVariableValue :: Handle Event -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue handle params = sendReceiveCommand handle "Debugger.setVariableValue" (Just params)



type PDebuggerStepInto = [(T.Text, T.Text)]
debuggerStepInto :: Handle Event -> PDebuggerStepInto -> IO (Maybe Error)
debuggerStepInto handle params = sendReceiveCommand handle "Debugger.stepInto" (Just params)




debuggerStepOut :: Handle Event -> IO (Maybe Error)
debuggerStepOut handle = sendReceiveCommand handle "Debugger.stepOut" (Nothing :: Maybe ())



type PDebuggerStepOver = [(T.Text, T.Text)]
debuggerStepOver :: Handle Event -> PDebuggerStepOver -> IO (Maybe Error)
debuggerStepOver handle params = sendReceiveCommand handle "Debugger.stepOver" (Just params)






data ProfilerProfileNode = ProfilerProfileNode {
   profilerProfileNodeId :: Int,
   profilerProfileNodeCallFrame :: RuntimeCallFrame,
   profilerProfileNodeHitCount :: Maybe Int,
   profilerProfileNodeChildren :: Maybe [Int],
   profilerProfileNodeDeoptReason :: Maybe String,
   profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data ProfilerProfile = ProfilerProfile {
   profilerProfileNodes :: [ProfilerProfileNode],
   profilerProfileStartTime :: Double,
   profilerProfileEndTime :: Double,
   profilerProfileSamples :: Maybe [Int],
   profilerProfileTimeDeltas :: Maybe [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
   profilerPositionTickInfoLine :: Int,
   profilerPositionTickInfoTicks :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPositionTickInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerPositionTickInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data ProfilerCoverageRange = ProfilerCoverageRange {
   profilerCoverageRangeStartOffset :: Int,
   profilerCoverageRangeEndOffset :: Int,
   profilerCoverageRangeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerCoverageRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  ProfilerCoverageRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
   profilerFunctionCoverageFunctionName :: String,
   profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
   profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerFunctionCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerFunctionCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data ProfilerScriptCoverage = ProfilerScriptCoverage {
   profilerScriptCoverageScriptId :: RuntimeScriptId,
   profilerScriptCoverageUrl :: String,
   profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }




data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
   profilerConsoleProfileFinishedId :: String,
   profilerConsoleProfileFinishedLocation :: DebuggerLocation,
   profilerConsoleProfileFinishedProfile :: ProfilerProfile,
   profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance FromEvent Event ProfilerConsoleProfileFinished where
   eventName  _ _    =  "Profiler.consoleProfileFinished"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing



data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
   profilerConsoleProfileStartedId :: String,
   profilerConsoleProfileStartedLocation :: DebuggerLocation,
   profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance FromEvent Event ProfilerConsoleProfileStarted where
   eventName  _ _    =  "Profiler.consoleProfileStarted"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileStarted v -> Just v; _ -> Nothing




profilerDisable :: Handle Event -> IO (Maybe Error)
profilerDisable handle = sendReceiveCommand handle "Profiler.disable" (Nothing :: Maybe ())




profilerEnable :: Handle Event -> IO (Maybe Error)
profilerEnable handle = sendReceiveCommand handle "Profiler.enable" (Nothing :: Maybe ())




profilerGetBestEffortCoverage :: Handle Event -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage handle = sendReceiveCommandResult handle "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
   profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerGetBestEffortCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command ProfilerGetBestEffortCoverage where
   commandName _ = "Profiler.getBestEffortCoverage"




data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
   pProfilerSetSamplingIntervalInterval :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerSetSamplingInterval  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PProfilerSetSamplingInterval where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


profilerSetSamplingInterval :: Handle Event -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval handle params = sendReceiveCommand handle "Profiler.setSamplingInterval" (Just params)




profilerStart :: Handle Event -> IO (Maybe Error)
profilerStart handle = sendReceiveCommand handle "Profiler.start" (Nothing :: Maybe ())




data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
   pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
   pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
   pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerStartPreciseCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


profilerStartPreciseCoverage :: Handle Event -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage handle params = sendReceiveCommandResult handle "Profiler.startPreciseCoverage" (Just params)

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
   profilerStartPreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command ProfilerStartPreciseCoverage where
   commandName _ = "Profiler.startPreciseCoverage"




profilerStop :: Handle Event -> IO (Either Error ProfilerStop)
profilerStop handle = sendReceiveCommandResult handle "Profiler.stop" (Nothing :: Maybe ())

data ProfilerStop = ProfilerStop {
   profilerStopProfile :: ProfilerProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStop where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command ProfilerStop where
   commandName _ = "Profiler.stop"




profilerStopPreciseCoverage :: Handle Event -> IO (Maybe Error)
profilerStopPreciseCoverage handle = sendReceiveCommand handle "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())




profilerTakePreciseCoverage :: Handle Event -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage handle = sendReceiveCommandResult handle "Profiler.takePreciseCoverage" (Nothing :: Maybe ())

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
   profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
   profilerTakePreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakePreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command ProfilerTakePreciseCoverage where
   commandName _ = "Profiler.takePreciseCoverage"





type RuntimeScriptId = String
data RuntimeWebDriverValueType = RuntimeWebDriverValueTypeUndefined | RuntimeWebDriverValueTypeNull | RuntimeWebDriverValueTypeString | RuntimeWebDriverValueTypeNumber | RuntimeWebDriverValueTypeBoolean | RuntimeWebDriverValueTypeBigint | RuntimeWebDriverValueTypeRegexp | RuntimeWebDriverValueTypeDate | RuntimeWebDriverValueTypeSymbol | RuntimeWebDriverValueTypeArray | RuntimeWebDriverValueTypeObject | RuntimeWebDriverValueTypeFunction | RuntimeWebDriverValueTypeMap | RuntimeWebDriverValueTypeSet | RuntimeWebDriverValueTypeWeakmap | RuntimeWebDriverValueTypeWeakset | RuntimeWebDriverValueTypeError | RuntimeWebDriverValueTypeProxy | RuntimeWebDriverValueTypePromise | RuntimeWebDriverValueTypeTypedarray | RuntimeWebDriverValueTypeArraybuffer | RuntimeWebDriverValueTypeNode | RuntimeWebDriverValueTypeWindow
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeWebDriverValueType where
   parseJSON = A.withText  "RuntimeWebDriverValueType"  $ \v -> do
      case v of
         "undefined" -> pure $ RuntimeWebDriverValueTypeUndefined
         "null" -> pure $ RuntimeWebDriverValueTypeNull
         "string" -> pure $ RuntimeWebDriverValueTypeString
         "number" -> pure $ RuntimeWebDriverValueTypeNumber
         "boolean" -> pure $ RuntimeWebDriverValueTypeBoolean
         "bigint" -> pure $ RuntimeWebDriverValueTypeBigint
         "regexp" -> pure $ RuntimeWebDriverValueTypeRegexp
         "date" -> pure $ RuntimeWebDriverValueTypeDate
         "symbol" -> pure $ RuntimeWebDriverValueTypeSymbol
         "array" -> pure $ RuntimeWebDriverValueTypeArray
         "object" -> pure $ RuntimeWebDriverValueTypeObject
         "function" -> pure $ RuntimeWebDriverValueTypeFunction
         "map" -> pure $ RuntimeWebDriverValueTypeMap
         "set" -> pure $ RuntimeWebDriverValueTypeSet
         "weakmap" -> pure $ RuntimeWebDriverValueTypeWeakmap
         "weakset" -> pure $ RuntimeWebDriverValueTypeWeakset
         "error" -> pure $ RuntimeWebDriverValueTypeError
         "proxy" -> pure $ RuntimeWebDriverValueTypeProxy
         "promise" -> pure $ RuntimeWebDriverValueTypePromise
         "typedarray" -> pure $ RuntimeWebDriverValueTypeTypedarray
         "arraybuffer" -> pure $ RuntimeWebDriverValueTypeArraybuffer
         "node" -> pure $ RuntimeWebDriverValueTypeNode
         "window" -> pure $ RuntimeWebDriverValueTypeWindow
         _ -> fail "failed to parse RuntimeWebDriverValueType"

instance ToJSON RuntimeWebDriverValueType where
   toJSON v = A.String $
      case v of
         RuntimeWebDriverValueTypeUndefined -> "undefined"
         RuntimeWebDriverValueTypeNull -> "null"
         RuntimeWebDriverValueTypeString -> "string"
         RuntimeWebDriverValueTypeNumber -> "number"
         RuntimeWebDriverValueTypeBoolean -> "boolean"
         RuntimeWebDriverValueTypeBigint -> "bigint"
         RuntimeWebDriverValueTypeRegexp -> "regexp"
         RuntimeWebDriverValueTypeDate -> "date"
         RuntimeWebDriverValueTypeSymbol -> "symbol"
         RuntimeWebDriverValueTypeArray -> "array"
         RuntimeWebDriverValueTypeObject -> "object"
         RuntimeWebDriverValueTypeFunction -> "function"
         RuntimeWebDriverValueTypeMap -> "map"
         RuntimeWebDriverValueTypeSet -> "set"
         RuntimeWebDriverValueTypeWeakmap -> "weakmap"
         RuntimeWebDriverValueTypeWeakset -> "weakset"
         RuntimeWebDriverValueTypeError -> "error"
         RuntimeWebDriverValueTypeProxy -> "proxy"
         RuntimeWebDriverValueTypePromise -> "promise"
         RuntimeWebDriverValueTypeTypedarray -> "typedarray"
         RuntimeWebDriverValueTypeArraybuffer -> "arraybuffer"
         RuntimeWebDriverValueTypeNode -> "node"
         RuntimeWebDriverValueTypeWindow -> "window"



data RuntimeWebDriverValue = RuntimeWebDriverValue {
   runtimeWebDriverValueType :: RuntimeWebDriverValueType,
   runtimeWebDriverValueValue :: Maybe Int,
   runtimeWebDriverValueObjectId :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeWebDriverValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  RuntimeWebDriverValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


type RuntimeRemoteObjectId = String
type RuntimeUnserializableValue = String
data RuntimeRemoteObjectType = RuntimeRemoteObjectTypeObject | RuntimeRemoteObjectTypeFunction | RuntimeRemoteObjectTypeUndefined | RuntimeRemoteObjectTypeString | RuntimeRemoteObjectTypeNumber | RuntimeRemoteObjectTypeBoolean | RuntimeRemoteObjectTypeSymbol | RuntimeRemoteObjectTypeBigint
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectType where
   parseJSON = A.withText  "RuntimeRemoteObjectType"  $ \v -> do
      case v of
         "object" -> pure $ RuntimeRemoteObjectTypeObject
         "function" -> pure $ RuntimeRemoteObjectTypeFunction
         "undefined" -> pure $ RuntimeRemoteObjectTypeUndefined
         "string" -> pure $ RuntimeRemoteObjectTypeString
         "number" -> pure $ RuntimeRemoteObjectTypeNumber
         "boolean" -> pure $ RuntimeRemoteObjectTypeBoolean
         "symbol" -> pure $ RuntimeRemoteObjectTypeSymbol
         "bigint" -> pure $ RuntimeRemoteObjectTypeBigint
         _ -> fail "failed to parse RuntimeRemoteObjectType"

instance ToJSON RuntimeRemoteObjectType where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectTypeObject -> "object"
         RuntimeRemoteObjectTypeFunction -> "function"
         RuntimeRemoteObjectTypeUndefined -> "undefined"
         RuntimeRemoteObjectTypeString -> "string"
         RuntimeRemoteObjectTypeNumber -> "number"
         RuntimeRemoteObjectTypeBoolean -> "boolean"
         RuntimeRemoteObjectTypeSymbol -> "symbol"
         RuntimeRemoteObjectTypeBigint -> "bigint"


data RuntimeRemoteObjectSubtype = RuntimeRemoteObjectSubtypeArray | RuntimeRemoteObjectSubtypeNull | RuntimeRemoteObjectSubtypeNode | RuntimeRemoteObjectSubtypeRegexp | RuntimeRemoteObjectSubtypeDate | RuntimeRemoteObjectSubtypeMap | RuntimeRemoteObjectSubtypeSet | RuntimeRemoteObjectSubtypeWeakmap | RuntimeRemoteObjectSubtypeWeakset | RuntimeRemoteObjectSubtypeIterator | RuntimeRemoteObjectSubtypeGenerator | RuntimeRemoteObjectSubtypeError | RuntimeRemoteObjectSubtypeProxy | RuntimeRemoteObjectSubtypePromise | RuntimeRemoteObjectSubtypeTypedarray | RuntimeRemoteObjectSubtypeArraybuffer | RuntimeRemoteObjectSubtypeDataview | RuntimeRemoteObjectSubtypeWebassemblymemory | RuntimeRemoteObjectSubtypeWasmvalue
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectSubtype where
   parseJSON = A.withText  "RuntimeRemoteObjectSubtype"  $ \v -> do
      case v of
         "array" -> pure $ RuntimeRemoteObjectSubtypeArray
         "null" -> pure $ RuntimeRemoteObjectSubtypeNull
         "node" -> pure $ RuntimeRemoteObjectSubtypeNode
         "regexp" -> pure $ RuntimeRemoteObjectSubtypeRegexp
         "date" -> pure $ RuntimeRemoteObjectSubtypeDate
         "map" -> pure $ RuntimeRemoteObjectSubtypeMap
         "set" -> pure $ RuntimeRemoteObjectSubtypeSet
         "weakmap" -> pure $ RuntimeRemoteObjectSubtypeWeakmap
         "weakset" -> pure $ RuntimeRemoteObjectSubtypeWeakset
         "iterator" -> pure $ RuntimeRemoteObjectSubtypeIterator
         "generator" -> pure $ RuntimeRemoteObjectSubtypeGenerator
         "error" -> pure $ RuntimeRemoteObjectSubtypeError
         "proxy" -> pure $ RuntimeRemoteObjectSubtypeProxy
         "promise" -> pure $ RuntimeRemoteObjectSubtypePromise
         "typedarray" -> pure $ RuntimeRemoteObjectSubtypeTypedarray
         "arraybuffer" -> pure $ RuntimeRemoteObjectSubtypeArraybuffer
         "dataview" -> pure $ RuntimeRemoteObjectSubtypeDataview
         "webassemblymemory" -> pure $ RuntimeRemoteObjectSubtypeWebassemblymemory
         "wasmvalue" -> pure $ RuntimeRemoteObjectSubtypeWasmvalue
         _ -> fail "failed to parse RuntimeRemoteObjectSubtype"

instance ToJSON RuntimeRemoteObjectSubtype where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectSubtypeArray -> "array"
         RuntimeRemoteObjectSubtypeNull -> "null"
         RuntimeRemoteObjectSubtypeNode -> "node"
         RuntimeRemoteObjectSubtypeRegexp -> "regexp"
         RuntimeRemoteObjectSubtypeDate -> "date"
         RuntimeRemoteObjectSubtypeMap -> "map"
         RuntimeRemoteObjectSubtypeSet -> "set"
         RuntimeRemoteObjectSubtypeWeakmap -> "weakmap"
         RuntimeRemoteObjectSubtypeWeakset -> "weakset"
         RuntimeRemoteObjectSubtypeIterator -> "iterator"
         RuntimeRemoteObjectSubtypeGenerator -> "generator"
         RuntimeRemoteObjectSubtypeError -> "error"
         RuntimeRemoteObjectSubtypeProxy -> "proxy"
         RuntimeRemoteObjectSubtypePromise -> "promise"
         RuntimeRemoteObjectSubtypeTypedarray -> "typedarray"
         RuntimeRemoteObjectSubtypeArraybuffer -> "arraybuffer"
         RuntimeRemoteObjectSubtypeDataview -> "dataview"
         RuntimeRemoteObjectSubtypeWebassemblymemory -> "webassemblymemory"
         RuntimeRemoteObjectSubtypeWasmvalue -> "wasmvalue"



data RuntimeRemoteObject = RuntimeRemoteObject {
   runtimeRemoteObjectType :: RuntimeRemoteObjectType,
   runtimeRemoteObjectSubtype :: RuntimeRemoteObjectSubtype,
   runtimeRemoteObjectClassName :: Maybe String,
   runtimeRemoteObjectValue :: Maybe Int,
   runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
   runtimeRemoteObjectDescription :: Maybe String,
   runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeRemoteObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeRemoteObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
   runtimePropertyDescriptorName :: String,
   runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorWritable :: Maybe Bool,
   runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorConfigurable :: Bool,
   runtimePropertyDescriptorEnumerable :: Bool,
   runtimePropertyDescriptorWasThrown :: Maybe Bool,
   runtimePropertyDescriptorIsOwn :: Maybe Bool,
   runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimePropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  RuntimePropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
   runtimeInternalPropertyDescriptorName :: String,
   runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInternalPropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  RuntimeInternalPropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data RuntimeCallArgument = RuntimeCallArgument {
   runtimeCallArgumentValue :: Maybe Int,
   runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
   runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
   runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
   runtimeExecutionContextDescriptionOrigin :: String,
   runtimeExecutionContextDescriptionName :: String,
   runtimeExecutionContextDescriptionAuxData :: Maybe [(T.Text, T.Text)]
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDescription  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDescription where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }



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
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


type RuntimeTimestamp = Double
type RuntimeTimeDelta = Double

data RuntimeCallFrame = RuntimeCallFrame {
   runtimeCallFrameFunctionName :: String,
   runtimeCallFrameScriptId :: RuntimeScriptId,
   runtimeCallFrameUrl :: String,
   runtimeCallFrameLineNumber :: Int,
   runtimeCallFrameColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data RuntimeStackTrace = RuntimeStackTrace {
   runtimeStackTraceDescription :: Maybe String,
   runtimeStackTraceCallFrames :: [RuntimeCallFrame],
   runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeStackTrace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  RuntimeStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data RuntimeConsoleApiCalledType = RuntimeConsoleApiCalledTypeLog | RuntimeConsoleApiCalledTypeDebug | RuntimeConsoleApiCalledTypeInfo | RuntimeConsoleApiCalledTypeError | RuntimeConsoleApiCalledTypeWarning | RuntimeConsoleApiCalledTypeDir | RuntimeConsoleApiCalledTypeDirxml | RuntimeConsoleApiCalledTypeTable | RuntimeConsoleApiCalledTypeTrace | RuntimeConsoleApiCalledTypeClear | RuntimeConsoleApiCalledTypeStartGroup | RuntimeConsoleApiCalledTypeStartGroupCollapsed | RuntimeConsoleApiCalledTypeEndGroup | RuntimeConsoleApiCalledTypeAssert | RuntimeConsoleApiCalledTypeProfile | RuntimeConsoleApiCalledTypeProfileEnd | RuntimeConsoleApiCalledTypeCount | RuntimeConsoleApiCalledTypeTimeEnd
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeConsoleApiCalledType where
   parseJSON = A.withText  "RuntimeConsoleApiCalledType"  $ \v -> do
      case v of
         "log" -> pure $ RuntimeConsoleApiCalledTypeLog
         "debug" -> pure $ RuntimeConsoleApiCalledTypeDebug
         "info" -> pure $ RuntimeConsoleApiCalledTypeInfo
         "error" -> pure $ RuntimeConsoleApiCalledTypeError
         "warning" -> pure $ RuntimeConsoleApiCalledTypeWarning
         "dir" -> pure $ RuntimeConsoleApiCalledTypeDir
         "dirxml" -> pure $ RuntimeConsoleApiCalledTypeDirxml
         "table" -> pure $ RuntimeConsoleApiCalledTypeTable
         "trace" -> pure $ RuntimeConsoleApiCalledTypeTrace
         "clear" -> pure $ RuntimeConsoleApiCalledTypeClear
         "startGroup" -> pure $ RuntimeConsoleApiCalledTypeStartGroup
         "startGroupCollapsed" -> pure $ RuntimeConsoleApiCalledTypeStartGroupCollapsed
         "endGroup" -> pure $ RuntimeConsoleApiCalledTypeEndGroup
         "assert" -> pure $ RuntimeConsoleApiCalledTypeAssert
         "profile" -> pure $ RuntimeConsoleApiCalledTypeProfile
         "profileEnd" -> pure $ RuntimeConsoleApiCalledTypeProfileEnd
         "count" -> pure $ RuntimeConsoleApiCalledTypeCount
         "timeEnd" -> pure $ RuntimeConsoleApiCalledTypeTimeEnd
         _ -> fail "failed to parse RuntimeConsoleApiCalledType"

instance ToJSON RuntimeConsoleApiCalledType where
   toJSON v = A.String $
      case v of
         RuntimeConsoleApiCalledTypeLog -> "log"
         RuntimeConsoleApiCalledTypeDebug -> "debug"
         RuntimeConsoleApiCalledTypeInfo -> "info"
         RuntimeConsoleApiCalledTypeError -> "error"
         RuntimeConsoleApiCalledTypeWarning -> "warning"
         RuntimeConsoleApiCalledTypeDir -> "dir"
         RuntimeConsoleApiCalledTypeDirxml -> "dirxml"
         RuntimeConsoleApiCalledTypeTable -> "table"
         RuntimeConsoleApiCalledTypeTrace -> "trace"
         RuntimeConsoleApiCalledTypeClear -> "clear"
         RuntimeConsoleApiCalledTypeStartGroup -> "startGroup"
         RuntimeConsoleApiCalledTypeStartGroupCollapsed -> "startGroupCollapsed"
         RuntimeConsoleApiCalledTypeEndGroup -> "endGroup"
         RuntimeConsoleApiCalledTypeAssert -> "assert"
         RuntimeConsoleApiCalledTypeProfile -> "profile"
         RuntimeConsoleApiCalledTypeProfileEnd -> "profileEnd"
         RuntimeConsoleApiCalledTypeCount -> "count"
         RuntimeConsoleApiCalledTypeTimeEnd -> "timeEnd"



data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
   runtimeConsoleApiCalledType :: RuntimeConsoleApiCalledType,
   runtimeConsoleApiCalledArgs :: [RuntimeRemoteObject],
   runtimeConsoleApiCalledExecutionContextId :: RuntimeExecutionContextId,
   runtimeConsoleApiCalledTimestamp :: RuntimeTimestamp,
   runtimeConsoleApiCalledStackTrace :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeConsoleApiCalled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeConsoleApiCalled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event RuntimeConsoleApiCalled where
   eventName  _ _    =  "Runtime.consoleAPICalled"
   fromEvent ev      =  case ev of EVRuntimeConsoleApiCalled v -> Just v; _ -> Nothing



data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
   runtimeExceptionRevokedReason :: String,
   runtimeExceptionRevokedExceptionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionRevoked  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionRevoked where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event RuntimeExceptionRevoked where
   eventName  _ _    =  "Runtime.exceptionRevoked"
   fromEvent ev      =  case ev of EVRuntimeExceptionRevoked v -> Just v; _ -> Nothing



data RuntimeExceptionThrown = RuntimeExceptionThrown {
   runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
   runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionThrown  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionThrown where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance FromEvent Event RuntimeExceptionThrown where
   eventName  _ _    =  "Runtime.exceptionThrown"
   fromEvent ev      =  case ev of EVRuntimeExceptionThrown v -> Just v; _ -> Nothing



data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
   runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance FromEvent Event RuntimeExecutionContextCreated where
   eventName  _ _    =  "Runtime.executionContextCreated"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextCreated v -> Just v; _ -> Nothing



data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
   runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance FromEvent Event RuntimeExecutionContextDestroyed where
   eventName  _ _    =  "Runtime.executionContextDestroyed"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextDestroyed v -> Just v; _ -> Nothing


data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
   deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
   parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
      case v of
         "RuntimeExecutionContextsCleared" -> pure $ RuntimeExecutionContextsCleared
         _ -> fail "failed to parse RuntimeExecutionContextsCleared"


instance FromEvent Event RuntimeExecutionContextsCleared where
   eventName  _ _    =  "Runtime.executionContextsCleared"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextsCleared v -> Just v; _ -> Nothing



data RuntimeInspectRequested = RuntimeInspectRequested {
   runtimeInspectRequestedObject :: RuntimeRemoteObject,
   runtimeInspectRequestedHints :: [(T.Text, T.Text)]
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInspectRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeInspectRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance FromEvent Event RuntimeInspectRequested where
   eventName  _ _    =  "Runtime.inspectRequested"
   fromEvent ev      =  case ev of EVRuntimeInspectRequested v -> Just v; _ -> Nothing




data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
   pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
   pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
   pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeAwaitPromise  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


runtimeAwaitPromise :: Handle Event -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise handle params = sendReceiveCommandResult handle "Runtime.awaitPromise" (Just params)

data RuntimeAwaitPromise = RuntimeAwaitPromise {
   runtimeAwaitPromiseResult :: RuntimeRemoteObject,
   runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeAwaitPromise where
   commandName _ = "Runtime.awaitPromise"




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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCallFunctionOn  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


runtimeCallFunctionOn :: Handle Event -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn handle params = sendReceiveCommandResult handle "Runtime.callFunctionOn" (Just params)

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
   runtimeCallFunctionOnResult :: RuntimeRemoteObject,
   runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command RuntimeCallFunctionOn where
   commandName _ = "Runtime.callFunctionOn"




data PRuntimeCompileScript = PRuntimeCompileScript {
   pRuntimeCompileScriptExpression :: String,
   pRuntimeCompileScriptSourceUrl :: String,
   pRuntimeCompileScriptPersistScript :: Bool,
   pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCompileScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeCompileScript :: Handle Event -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript handle params = sendReceiveCommandResult handle "Runtime.compileScript" (Just params)

data RuntimeCompileScript = RuntimeCompileScript {
   runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
   runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeCompileScript where
   commandName _ = "Runtime.compileScript"




runtimeDisable :: Handle Event -> IO (Maybe Error)
runtimeDisable handle = sendReceiveCommand handle "Runtime.disable" (Nothing :: Maybe ())




runtimeDiscardConsoleEntries :: Handle Event -> IO (Maybe Error)
runtimeDiscardConsoleEntries handle = sendReceiveCommand handle "Runtime.discardConsoleEntries" (Nothing :: Maybe ())




runtimeEnable :: Handle Event -> IO (Maybe Error)
runtimeEnable handle = sendReceiveCommand handle "Runtime.enable" (Nothing :: Maybe ())




data PRuntimeEvaluate = PRuntimeEvaluate {
   pRuntimeEvaluateExpression :: String,
   pRuntimeEvaluateObjectGroup :: Maybe String,
   pRuntimeEvaluateIncludeCommandLineApi :: Maybe Bool,
   pRuntimeEvaluateSilent :: Maybe Bool,
   pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
   pRuntimeEvaluateReturnByValue :: Maybe Bool,
   pRuntimeEvaluateUserGesture :: Maybe Bool,
   pRuntimeEvaluateAwaitPromise :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeEvaluate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PRuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


runtimeEvaluate :: Handle Event -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate handle params = sendReceiveCommandResult handle "Runtime.evaluate" (Just params)

data RuntimeEvaluate = RuntimeEvaluate {
   runtimeEvaluateResult :: RuntimeRemoteObject,
   runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command RuntimeEvaluate where
   commandName _ = "Runtime.evaluate"




data PRuntimeGetProperties = PRuntimeGetProperties {
   pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
   pRuntimeGetPropertiesOwnProperties :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGetProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeGetProperties :: Handle Event -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties handle params = sendReceiveCommandResult handle "Runtime.getProperties" (Just params)

data RuntimeGetProperties = RuntimeGetProperties {
   runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
   runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
   runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeGetProperties where
   commandName _ = "Runtime.getProperties"




data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
   pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGlobalLexicalScopeNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


runtimeGlobalLexicalScopeNames :: Handle Event -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames handle params = sendReceiveCommandResult handle "Runtime.globalLexicalScopeNames" (Just params)

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
   runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command RuntimeGlobalLexicalScopeNames where
   commandName _ = "Runtime.globalLexicalScopeNames"




data PRuntimeQueryObjects = PRuntimeQueryObjects {
   pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
   pRuntimeQueryObjectsObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeQueryObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


runtimeQueryObjects :: Handle Event -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects handle params = sendReceiveCommandResult handle "Runtime.queryObjects" (Just params)

data RuntimeQueryObjects = RuntimeQueryObjects {
   runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeQueryObjects where
   commandName _ = "Runtime.queryObjects"




data PRuntimeReleaseObject = PRuntimeReleaseObject {
   pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeReleaseObject :: Handle Event -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject handle params = sendReceiveCommand handle "Runtime.releaseObject" (Just params)




data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
   pRuntimeReleaseObjectGroupObjectGroup :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObjectGroup  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObjectGroup where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


runtimeReleaseObjectGroup :: Handle Event -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup handle params = sendReceiveCommand handle "Runtime.releaseObjectGroup" (Just params)




runtimeRunIfWaitingForDebugger :: Handle Event -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger handle = sendReceiveCommand handle "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())




data PRuntimeRunScript = PRuntimeRunScript {
   pRuntimeRunScriptScriptId :: RuntimeScriptId,
   pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
   pRuntimeRunScriptObjectGroup :: Maybe String,
   pRuntimeRunScriptSilent :: Maybe Bool,
   pRuntimeRunScriptIncludeCommandLineApi :: Maybe Bool,
   pRuntimeRunScriptReturnByValue :: Maybe Bool,
   pRuntimeRunScriptGeneratePreview :: Maybe Bool,
   pRuntimeRunScriptAwaitPromise :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeRunScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PRuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


runtimeRunScript :: Handle Event -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript handle params = sendReceiveCommandResult handle "Runtime.runScript" (Just params)

data RuntimeRunScript = RuntimeRunScript {
   runtimeRunScriptResult :: RuntimeRemoteObject,
   runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command RuntimeRunScript where
   commandName _ = "Runtime.runScript"




data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
   pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PRuntimeSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


runtimeSetAsyncCallStackDepth :: Handle Event -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Runtime.setAsyncCallStackDepth" (Just params)







