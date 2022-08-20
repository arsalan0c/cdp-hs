{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Page (module Domains.Page) where
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

import qualified Domains.Debugger as Debugger
import qualified Domains.DOM as DOM
import qualified Domains.IO as IO
import qualified Domains.Network as Network
import qualified Domains.Runtime as Runtime


import Utils

data DomContentEventFired = DomContentEventFired {
    domContentEventFiredTimestamp :: Network.MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  DomContentEventFired where
    parseJSON = A.withObject "DomContentEventFired" $ \v ->
         DomContentEventFired <$> v .:  "timestamp"


instance ToJSON DomContentEventFired  where
    toJSON v = A.object
        [ "timestamp" .= domContentEventFiredTimestamp v
        ]


data FileChooserOpened = FileChooserOpened {
    fileChooserOpenedMode :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FileChooserOpened where
    parseJSON = A.withObject "FileChooserOpened" $ \v ->
         FileChooserOpened <$> v .:  "mode"


instance ToJSON FileChooserOpened  where
    toJSON v = A.object
        [ "mode" .= fileChooserOpenedMode v
        ]


data FrameAttached = FrameAttached {
    frameAttachedFrameId :: FrameId,
    frameAttachedParentFrameId :: FrameId,
    frameAttachedStack :: Maybe Runtime.StackTrace
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FrameAttached where
    parseJSON = A.withObject "FrameAttached" $ \v ->
         FrameAttached <$> v .:  "frameId"
            <*> v  .:  "parentFrameId"
            <*> v  .:?  "stack"


instance ToJSON FrameAttached  where
    toJSON v = A.object
        [ "frameId" .= frameAttachedFrameId v
        , "parentFrameId" .= frameAttachedParentFrameId v
        , "stack" .= frameAttachedStack v
        ]


data FrameDetached = FrameDetached {
    frameDetachedFrameId :: FrameId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FrameDetached where
    parseJSON = A.withObject "FrameDetached" $ \v ->
         FrameDetached <$> v .:  "frameId"


instance ToJSON FrameDetached  where
    toJSON v = A.object
        [ "frameId" .= frameDetachedFrameId v
        ]


data FrameNavigated = FrameNavigated {
    frameNavigatedFrame :: Frame
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FrameNavigated where
    parseJSON = A.withObject "FrameNavigated" $ \v ->
         FrameNavigated <$> v .:  "frame"


instance ToJSON FrameNavigated  where
    toJSON v = A.object
        [ "frame" .= frameNavigatedFrame v
        ]


data InterstitialHidden = InterstitialHidden
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON InterstitialHidden where
    parseJSON = A.withText  "InterstitialHidden"  $ \v -> do
        pure $ case v of
                "InterstitialHidden" -> InterstitialHidden
                _ -> error "failed to parse InterstitialHidden"

data InterstitialShown = InterstitialShown
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON InterstitialShown where
    parseJSON = A.withText  "InterstitialShown"  $ \v -> do
        pure $ case v of
                "InterstitialShown" -> InterstitialShown
                _ -> error "failed to parse InterstitialShown"

data JavascriptDialogClosed = JavascriptDialogClosed {
    javascriptDialogClosedResult :: Bool,
    javascriptDialogClosedUserInput :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  JavascriptDialogClosed where
    parseJSON = A.withObject "JavascriptDialogClosed" $ \v ->
         JavascriptDialogClosed <$> v .:  "result"
            <*> v  .:  "userInput"


instance ToJSON JavascriptDialogClosed  where
    toJSON v = A.object
        [ "result" .= javascriptDialogClosedResult v
        , "userInput" .= javascriptDialogClosedUserInput v
        ]


data JavascriptDialogOpening = JavascriptDialogOpening {
    javascriptDialogOpeningUrl :: String,
    javascriptDialogOpeningMessage :: String,
    javascriptDialogOpeningType :: DialogType,
    javascriptDialogOpeningHasBrowserHandler :: Bool,
    javascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  JavascriptDialogOpening where
    parseJSON = A.withObject "JavascriptDialogOpening" $ \v ->
         JavascriptDialogOpening <$> v .:  "url"
            <*> v  .:  "message"
            <*> v  .:  "type"
            <*> v  .:  "hasBrowserHandler"
            <*> v  .:?  "defaultPrompt"


instance ToJSON JavascriptDialogOpening  where
    toJSON v = A.object
        [ "url" .= javascriptDialogOpeningUrl v
        , "message" .= javascriptDialogOpeningMessage v
        , "type" .= javascriptDialogOpeningType v
        , "hasBrowserHandler" .= javascriptDialogOpeningHasBrowserHandler v
        , "defaultPrompt" .= javascriptDialogOpeningDefaultPrompt v
        ]


data LifecycleEvent = LifecycleEvent {
    lifecycleEventFrameId :: FrameId,
    lifecycleEventLoaderId :: Network.LoaderId,
    lifecycleEventName :: String,
    lifecycleEventTimestamp :: Network.MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LifecycleEvent where
    parseJSON = A.withObject "LifecycleEvent" $ \v ->
         LifecycleEvent <$> v .:  "frameId"
            <*> v  .:  "loaderId"
            <*> v  .:  "name"
            <*> v  .:  "timestamp"


instance ToJSON LifecycleEvent  where
    toJSON v = A.object
        [ "frameId" .= lifecycleEventFrameId v
        , "loaderId" .= lifecycleEventLoaderId v
        , "name" .= lifecycleEventName v
        , "timestamp" .= lifecycleEventTimestamp v
        ]


data PrerenderAttemptCompleted = PrerenderAttemptCompleted {
    prerenderAttemptCompletedInitiatingFrameId :: FrameId,
    prerenderAttemptCompletedPrerenderingUrl :: String,
    prerenderAttemptCompletedFinalStatus :: PrerenderFinalStatus
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  PrerenderAttemptCompleted where
    parseJSON = A.withObject "PrerenderAttemptCompleted" $ \v ->
         PrerenderAttemptCompleted <$> v .:  "initiatingFrameId"
            <*> v  .:  "prerenderingUrl"
            <*> v  .:  "finalStatus"


instance ToJSON PrerenderAttemptCompleted  where
    toJSON v = A.object
        [ "initiatingFrameId" .= prerenderAttemptCompletedInitiatingFrameId v
        , "prerenderingUrl" .= prerenderAttemptCompletedPrerenderingUrl v
        , "finalStatus" .= prerenderAttemptCompletedFinalStatus v
        ]


data LoadEventFired = LoadEventFired {
    loadEventFiredTimestamp :: Network.MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LoadEventFired where
    parseJSON = A.withObject "LoadEventFired" $ \v ->
         LoadEventFired <$> v .:  "timestamp"


instance ToJSON LoadEventFired  where
    toJSON v = A.object
        [ "timestamp" .= loadEventFiredTimestamp v
        ]


data WindowOpen = WindowOpen {
    windowOpenUrl :: String,
    windowOpenWindowName :: String,
    windowOpenWindowFeatures :: [String],
    windowOpenUserGesture :: Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WindowOpen where
    parseJSON = A.withObject "WindowOpen" $ \v ->
         WindowOpen <$> v .:  "url"
            <*> v  .:  "windowName"
            <*> v  .:  "windowFeatures"
            <*> v  .:  "userGesture"


instance ToJSON WindowOpen  where
    toJSON v = A.object
        [ "url" .= windowOpenUrl v
        , "windowName" .= windowOpenWindowName v
        , "windowFeatures" .= windowOpenWindowFeatures v
        , "userGesture" .= windowOpenUserGesture v
        ]



type FrameId = String

data Frame = Frame {
    frameId :: FrameId,
    frameLoaderId :: Network.LoaderId,
    frameUrl :: String,
    frameSecurityOrigin :: String,
    frameMimeType :: String,
    frameParentId :: Maybe FrameId,
    frameName :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Frame where
    parseJSON = A.withObject "Frame" $ \v ->
         Frame <$> v .:  "id"
            <*> v  .:  "loaderId"
            <*> v  .:  "url"
            <*> v  .:  "securityOrigin"
            <*> v  .:  "mimeType"
            <*> v  .:?  "parentId"
            <*> v  .:?  "name"


instance ToJSON Frame  where
    toJSON v = A.object
        [ "id" .= frameId v
        , "loaderId" .= frameLoaderId v
        , "url" .= frameUrl v
        , "securityOrigin" .= frameSecurityOrigin v
        , "mimeType" .= frameMimeType v
        , "parentId" .= frameParentId v
        , "name" .= frameName v
        ]



data FrameTree = FrameTree {
    frameTreeFrame :: Frame,
    frameTreeChildFrames :: Maybe [FrameTree]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FrameTree where
    parseJSON = A.withObject "FrameTree" $ \v ->
         FrameTree <$> v .:  "frame"
            <*> v  .:?  "childFrames"


instance ToJSON FrameTree  where
    toJSON v = A.object
        [ "frame" .= frameTreeFrame v
        , "childFrames" .= frameTreeChildFrames v
        ]



type ScriptIdentifier = String

data TransitionType = TransitionTypeLink | TransitionTypeTyped | TransitionTypeAddressBar | TransitionTypeAutoBookmark | TransitionTypeAutoSubframe | TransitionTypeManualSubframe | TransitionTypeGenerated | TransitionTypeAutoToplevel | TransitionTypeFormSubmit | TransitionTypeReload | TransitionTypeKeyword | TransitionTypeKeywordGenerated | TransitionTypeOther
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON TransitionType where
    parseJSON = A.withText  "TransitionType"  $ \v -> do
        pure $ case v of
                "link" -> TransitionTypeLink
                "typed" -> TransitionTypeTyped
                "address_bar" -> TransitionTypeAddressBar
                "auto_bookmark" -> TransitionTypeAutoBookmark
                "auto_subframe" -> TransitionTypeAutoSubframe
                "manual_subframe" -> TransitionTypeManualSubframe
                "generated" -> TransitionTypeGenerated
                "auto_toplevel" -> TransitionTypeAutoToplevel
                "form_submit" -> TransitionTypeFormSubmit
                "reload" -> TransitionTypeReload
                "keyword" -> TransitionTypeKeyword
                "keyword_generated" -> TransitionTypeKeywordGenerated
                "other" -> TransitionTypeOther
                _ -> error "failed to parse TransitionType"

instance ToJSON TransitionType where
    toJSON v = A.String $
        case v of
                TransitionTypeLink -> "link"
                TransitionTypeTyped -> "typed"
                TransitionTypeAddressBar -> "address_bar"
                TransitionTypeAutoBookmark -> "auto_bookmark"
                TransitionTypeAutoSubframe -> "auto_subframe"
                TransitionTypeManualSubframe -> "manual_subframe"
                TransitionTypeGenerated -> "generated"
                TransitionTypeAutoToplevel -> "auto_toplevel"
                TransitionTypeFormSubmit -> "form_submit"
                TransitionTypeReload -> "reload"
                TransitionTypeKeyword -> "keyword"
                TransitionTypeKeywordGenerated -> "keyword_generated"
                TransitionTypeOther -> "other"



data NavigationEntry = NavigationEntry {
    navigationEntryId :: Int,
    navigationEntryUrl :: String,
    navigationEntryUserTypedUrl :: String,
    navigationEntryTitle :: String,
    navigationEntryTransitionType :: TransitionType
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  NavigationEntry where
    parseJSON = A.withObject "NavigationEntry" $ \v ->
         NavigationEntry <$> v .:  "id"
            <*> v  .:  "url"
            <*> v  .:  "userTypedURL"
            <*> v  .:  "title"
            <*> v  .:  "transitionType"


instance ToJSON NavigationEntry  where
    toJSON v = A.object
        [ "id" .= navigationEntryId v
        , "url" .= navigationEntryUrl v
        , "userTypedURL" .= navigationEntryUserTypedUrl v
        , "title" .= navigationEntryTitle v
        , "transitionType" .= navigationEntryTransitionType v
        ]



data DialogType = DialogTypeAlert | DialogTypeConfirm | DialogTypePrompt | DialogTypeBeforeunload
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON DialogType where
    parseJSON = A.withText  "DialogType"  $ \v -> do
        pure $ case v of
                "alert" -> DialogTypeAlert
                "confirm" -> DialogTypeConfirm
                "prompt" -> DialogTypePrompt
                "beforeunload" -> DialogTypeBeforeunload
                _ -> error "failed to parse DialogType"

instance ToJSON DialogType where
    toJSON v = A.String $
        case v of
                DialogTypeAlert -> "alert"
                DialogTypeConfirm -> "confirm"
                DialogTypePrompt -> "prompt"
                DialogTypeBeforeunload -> "beforeunload"



data AppManifestError = AppManifestError {
    appManifestErrorMessage :: String,
    appManifestErrorCritical :: Int,
    appManifestErrorLine :: Int,
    appManifestErrorColumn :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AppManifestError where
    parseJSON = A.withObject "AppManifestError" $ \v ->
         AppManifestError <$> v .:  "message"
            <*> v  .:  "critical"
            <*> v  .:  "line"
            <*> v  .:  "column"


instance ToJSON AppManifestError  where
    toJSON v = A.object
        [ "message" .= appManifestErrorMessage v
        , "critical" .= appManifestErrorCritical v
        , "line" .= appManifestErrorLine v
        , "column" .= appManifestErrorColumn v
        ]



data LayoutViewport = LayoutViewport {
    layoutViewportPageX :: Int,
    layoutViewportPageY :: Int,
    layoutViewportClientWidth :: Int,
    layoutViewportClientHeight :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LayoutViewport where
    parseJSON = A.withObject "LayoutViewport" $ \v ->
         LayoutViewport <$> v .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"


instance ToJSON LayoutViewport  where
    toJSON v = A.object
        [ "pageX" .= layoutViewportPageX v
        , "pageY" .= layoutViewportPageY v
        , "clientWidth" .= layoutViewportClientWidth v
        , "clientHeight" .= layoutViewportClientHeight v
        ]



data VisualViewport = VisualViewport {
    visualViewportOffsetX :: Int,
    visualViewportOffsetY :: Int,
    visualViewportPageX :: Int,
    visualViewportPageY :: Int,
    visualViewportClientWidth :: Int,
    visualViewportClientHeight :: Int,
    visualViewportScale :: Int,
    visualViewportZoom :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  VisualViewport where
    parseJSON = A.withObject "VisualViewport" $ \v ->
         VisualViewport <$> v .:  "offsetX"
            <*> v  .:  "offsetY"
            <*> v  .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"
            <*> v  .:  "scale"
            <*> v  .:?  "zoom"


instance ToJSON VisualViewport  where
    toJSON v = A.object
        [ "offsetX" .= visualViewportOffsetX v
        , "offsetY" .= visualViewportOffsetY v
        , "pageX" .= visualViewportPageX v
        , "pageY" .= visualViewportPageY v
        , "clientWidth" .= visualViewportClientWidth v
        , "clientHeight" .= visualViewportClientHeight v
        , "scale" .= visualViewportScale v
        , "zoom" .= visualViewportZoom v
        ]



data Viewport = Viewport {
    viewportX :: Int,
    viewportY :: Int,
    viewportWidth :: Int,
    viewportHeight :: Int,
    viewportScale :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Viewport where
    parseJSON = A.withObject "Viewport" $ \v ->
         Viewport <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:  "scale"


instance ToJSON Viewport  where
    toJSON v = A.object
        [ "x" .= viewportX v
        , "y" .= viewportY v
        , "width" .= viewportWidth v
        , "height" .= viewportHeight v
        , "scale" .= viewportScale v
        ]



data PrerenderFinalStatus = PrerenderFinalStatusActivated | PrerenderFinalStatusDestroyed | PrerenderFinalStatusLowEndDevice | PrerenderFinalStatusCrossOriginRedirect | PrerenderFinalStatusCrossOriginNavigation | PrerenderFinalStatusInvalidSchemeRedirect | PrerenderFinalStatusInvalidSchemeNavigation | PrerenderFinalStatusInProgressNavigation | PrerenderFinalStatusNavigationRequestBlockedByCsp | PrerenderFinalStatusMainFrameNavigation | PrerenderFinalStatusMojoBinderPolicy | PrerenderFinalStatusRendererProcessCrashed | PrerenderFinalStatusRendererProcessKilled | PrerenderFinalStatusDownload | PrerenderFinalStatusTriggerDestroyed | PrerenderFinalStatusNavigationNotCommitted | PrerenderFinalStatusNavigationBadHttpStatus | PrerenderFinalStatusClientCertRequested | PrerenderFinalStatusNavigationRequestNetworkError | PrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PrerenderFinalStatusCancelAllHostsForTesting | PrerenderFinalStatusDidFailLoad | PrerenderFinalStatusStop | PrerenderFinalStatusSslCertificateError | PrerenderFinalStatusLoginAuthRequested | PrerenderFinalStatusUaChangeRequiresReload | PrerenderFinalStatusBlockedByClient | PrerenderFinalStatusAudioOutputDeviceRequested | PrerenderFinalStatusMixedContent | PrerenderFinalStatusTriggerBackgrounded | PrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PrerenderFinalStatusEmbedderTriggeredAndDestroyed
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON PrerenderFinalStatus where
    parseJSON = A.withText  "PrerenderFinalStatus"  $ \v -> do
        pure $ case v of
                "Activated" -> PrerenderFinalStatusActivated
                "Destroyed" -> PrerenderFinalStatusDestroyed
                "LowEndDevice" -> PrerenderFinalStatusLowEndDevice
                "CrossOriginRedirect" -> PrerenderFinalStatusCrossOriginRedirect
                "CrossOriginNavigation" -> PrerenderFinalStatusCrossOriginNavigation
                "InvalidSchemeRedirect" -> PrerenderFinalStatusInvalidSchemeRedirect
                "InvalidSchemeNavigation" -> PrerenderFinalStatusInvalidSchemeNavigation
                "InProgressNavigation" -> PrerenderFinalStatusInProgressNavigation
                "NavigationRequestBlockedByCsp" -> PrerenderFinalStatusNavigationRequestBlockedByCsp
                "MainFrameNavigation" -> PrerenderFinalStatusMainFrameNavigation
                "MojoBinderPolicy" -> PrerenderFinalStatusMojoBinderPolicy
                "RendererProcessCrashed" -> PrerenderFinalStatusRendererProcessCrashed
                "RendererProcessKilled" -> PrerenderFinalStatusRendererProcessKilled
                "Download" -> PrerenderFinalStatusDownload
                "TriggerDestroyed" -> PrerenderFinalStatusTriggerDestroyed
                "NavigationNotCommitted" -> PrerenderFinalStatusNavigationNotCommitted
                "NavigationBadHttpStatus" -> PrerenderFinalStatusNavigationBadHttpStatus
                "ClientCertRequested" -> PrerenderFinalStatusClientCertRequested
                "NavigationRequestNetworkError" -> PrerenderFinalStatusNavigationRequestNetworkError
                "MaxNumOfRunningPrerendersExceeded" -> PrerenderFinalStatusMaxNumOfRunningPrerendersExceeded
                "CancelAllHostsForTesting" -> PrerenderFinalStatusCancelAllHostsForTesting
                "DidFailLoad" -> PrerenderFinalStatusDidFailLoad
                "Stop" -> PrerenderFinalStatusStop
                "SslCertificateError" -> PrerenderFinalStatusSslCertificateError
                "LoginAuthRequested" -> PrerenderFinalStatusLoginAuthRequested
                "UaChangeRequiresReload" -> PrerenderFinalStatusUaChangeRequiresReload
                "BlockedByClient" -> PrerenderFinalStatusBlockedByClient
                "AudioOutputDeviceRequested" -> PrerenderFinalStatusAudioOutputDeviceRequested
                "MixedContent" -> PrerenderFinalStatusMixedContent
                "TriggerBackgrounded" -> PrerenderFinalStatusTriggerBackgrounded
                "EmbedderTriggeredAndSameOriginRedirected" -> PrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected
                "EmbedderTriggeredAndCrossOriginRedirected" -> PrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
                "EmbedderTriggeredAndDestroyed" -> PrerenderFinalStatusEmbedderTriggeredAndDestroyed
                _ -> error "failed to parse PrerenderFinalStatus"

instance ToJSON PrerenderFinalStatus where
    toJSON v = A.String $
        case v of
                PrerenderFinalStatusActivated -> "Activated"
                PrerenderFinalStatusDestroyed -> "Destroyed"
                PrerenderFinalStatusLowEndDevice -> "LowEndDevice"
                PrerenderFinalStatusCrossOriginRedirect -> "CrossOriginRedirect"
                PrerenderFinalStatusCrossOriginNavigation -> "CrossOriginNavigation"
                PrerenderFinalStatusInvalidSchemeRedirect -> "InvalidSchemeRedirect"
                PrerenderFinalStatusInvalidSchemeNavigation -> "InvalidSchemeNavigation"
                PrerenderFinalStatusInProgressNavigation -> "InProgressNavigation"
                PrerenderFinalStatusNavigationRequestBlockedByCsp -> "NavigationRequestBlockedByCsp"
                PrerenderFinalStatusMainFrameNavigation -> "MainFrameNavigation"
                PrerenderFinalStatusMojoBinderPolicy -> "MojoBinderPolicy"
                PrerenderFinalStatusRendererProcessCrashed -> "RendererProcessCrashed"
                PrerenderFinalStatusRendererProcessKilled -> "RendererProcessKilled"
                PrerenderFinalStatusDownload -> "Download"
                PrerenderFinalStatusTriggerDestroyed -> "TriggerDestroyed"
                PrerenderFinalStatusNavigationNotCommitted -> "NavigationNotCommitted"
                PrerenderFinalStatusNavigationBadHttpStatus -> "NavigationBadHttpStatus"
                PrerenderFinalStatusClientCertRequested -> "ClientCertRequested"
                PrerenderFinalStatusNavigationRequestNetworkError -> "NavigationRequestNetworkError"
                PrerenderFinalStatusMaxNumOfRunningPrerendersExceeded -> "MaxNumOfRunningPrerendersExceeded"
                PrerenderFinalStatusCancelAllHostsForTesting -> "CancelAllHostsForTesting"
                PrerenderFinalStatusDidFailLoad -> "DidFailLoad"
                PrerenderFinalStatusStop -> "Stop"
                PrerenderFinalStatusSslCertificateError -> "SslCertificateError"
                PrerenderFinalStatusLoginAuthRequested -> "LoginAuthRequested"
                PrerenderFinalStatusUaChangeRequiresReload -> "UaChangeRequiresReload"
                PrerenderFinalStatusBlockedByClient -> "BlockedByClient"
                PrerenderFinalStatusAudioOutputDeviceRequested -> "AudioOutputDeviceRequested"
                PrerenderFinalStatusMixedContent -> "MixedContent"
                PrerenderFinalStatusTriggerBackgrounded -> "TriggerBackgrounded"
                PrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected -> "EmbedderTriggeredAndSameOriginRedirected"
                PrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected -> "EmbedderTriggeredAndCrossOriginRedirected"
                PrerenderFinalStatusEmbedderTriggeredAndDestroyed -> "EmbedderTriggeredAndDestroyed"


data AddScriptToEvaluateOnNewDocument = AddScriptToEvaluateOnNewDocument {
    addScriptToEvaluateOnNewDocumentIdentifier :: ScriptIdentifier
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AddScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "AddScriptToEvaluateOnNewDocument" $ \v ->
         AddScriptToEvaluateOnNewDocument <$> v .:  "identifier"



addScriptToEvaluateOnNewDocument :: Session a -> String -> IO (Either Error AddScriptToEvaluateOnNewDocument)
addScriptToEvaluateOnNewDocument session addScriptToEvaluateOnNewDocumentSource = sendReceiveCommandResult (conn session) ("Page","addScriptToEvaluateOnNewDocument") ([("source", ToJSONEx addScriptToEvaluateOnNewDocumentSource)] ++ (catMaybes []))


bringToFront :: Session a -> IO (Maybe Error)
bringToFront session  = sendReceiveCommand (conn session) ("Page","bringToFront") ([] ++ (catMaybes []))

data CaptureScreenshot = CaptureScreenshot {
    captureScreenshotData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CaptureScreenshot where
    parseJSON = A.withObject "CaptureScreenshot" $ \v ->
         CaptureScreenshot <$> v .:  "data"



captureScreenshot :: Session a -> Maybe String -> Maybe Int -> Maybe Viewport -> IO (Either Error CaptureScreenshot)
captureScreenshot session captureScreenshotFormat captureScreenshotQuality captureScreenshotClip = sendReceiveCommandResult (conn session) ("Page","captureScreenshot") ([] ++ (catMaybes [fmap (("format",) . ToJSONEx) captureScreenshotFormat, fmap (("quality",) . ToJSONEx) captureScreenshotQuality, fmap (("clip",) . ToJSONEx) captureScreenshotClip]))

data CreateIsolatedWorld = CreateIsolatedWorld {
    createIsolatedWorldExecutionContextId :: Runtime.ExecutionContextId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CreateIsolatedWorld where
    parseJSON = A.withObject "CreateIsolatedWorld" $ \v ->
         CreateIsolatedWorld <$> v .:  "executionContextId"



createIsolatedWorld :: Session a -> FrameId -> Maybe String -> Maybe Bool -> IO (Either Error CreateIsolatedWorld)
createIsolatedWorld session createIsolatedWorldFrameId createIsolatedWorldWorldName createIsolatedWorldGrantUniveralAccess = sendReceiveCommandResult (conn session) ("Page","createIsolatedWorld") ([("frameId", ToJSONEx createIsolatedWorldFrameId)] ++ (catMaybes [fmap (("worldName",) . ToJSONEx) createIsolatedWorldWorldName, fmap (("grantUniveralAccess",) . ToJSONEx) createIsolatedWorldGrantUniveralAccess]))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Page","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Page","enable") ([] ++ (catMaybes []))

data GetAppManifest = GetAppManifest {
    getAppManifestUrl :: String,
    getAppManifestErrors :: [AppManifestError],
    getAppManifestData :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetAppManifest where
    parseJSON = A.withObject "GetAppManifest" $ \v ->
         GetAppManifest <$> v .:  "url"
            <*> v  .:  "errors"
            <*> v  .:?  "data"



getAppManifest :: Session a -> IO (Either Error GetAppManifest)
getAppManifest session  = sendReceiveCommandResult (conn session) ("Page","getAppManifest") ([] ++ (catMaybes []))

data GetFrameTree = GetFrameTree {
    getFrameTreeFrameTree :: FrameTree
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetFrameTree where
    parseJSON = A.withObject "GetFrameTree" $ \v ->
         GetFrameTree <$> v .:  "frameTree"



getFrameTree :: Session a -> IO (Either Error GetFrameTree)
getFrameTree session  = sendReceiveCommandResult (conn session) ("Page","getFrameTree") ([] ++ (catMaybes []))

data GetLayoutMetrics = GetLayoutMetrics {
    getLayoutMetricsCssLayoutViewport :: LayoutViewport,
    getLayoutMetricsCssVisualViewport :: VisualViewport,
    getLayoutMetricsCssContentSize :: DOM.Rect
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetLayoutMetrics where
    parseJSON = A.withObject "GetLayoutMetrics" $ \v ->
         GetLayoutMetrics <$> v .:  "cssLayoutViewport"
            <*> v  .:  "cssVisualViewport"
            <*> v  .:  "cssContentSize"



getLayoutMetrics :: Session a -> IO (Either Error GetLayoutMetrics)
getLayoutMetrics session  = sendReceiveCommandResult (conn session) ("Page","getLayoutMetrics") ([] ++ (catMaybes []))

data GetNavigationHistory = GetNavigationHistory {
    getNavigationHistoryCurrentIndex :: Int,
    getNavigationHistoryEntries :: [NavigationEntry]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetNavigationHistory where
    parseJSON = A.withObject "GetNavigationHistory" $ \v ->
         GetNavigationHistory <$> v .:  "currentIndex"
            <*> v  .:  "entries"



getNavigationHistory :: Session a -> IO (Either Error GetNavigationHistory)
getNavigationHistory session  = sendReceiveCommandResult (conn session) ("Page","getNavigationHistory") ([] ++ (catMaybes []))


resetNavigationHistory :: Session a -> IO (Maybe Error)
resetNavigationHistory session  = sendReceiveCommand (conn session) ("Page","resetNavigationHistory") ([] ++ (catMaybes []))


handleJavaScriptDialog :: Session a -> Bool -> Maybe String -> IO (Maybe Error)
handleJavaScriptDialog session handleJavaScriptDialogAccept handleJavaScriptDialogPromptText = sendReceiveCommand (conn session) ("Page","handleJavaScriptDialog") ([("accept", ToJSONEx handleJavaScriptDialogAccept)] ++ (catMaybes [fmap (("promptText",) . ToJSONEx) handleJavaScriptDialogPromptText]))

data Navigate = Navigate {
    navigateFrameId :: FrameId,
    navigateLoaderId :: Maybe Network.LoaderId,
    navigateErrorText :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Navigate where
    parseJSON = A.withObject "Navigate" $ \v ->
         Navigate <$> v .:  "frameId"
            <*> v  .:?  "loaderId"
            <*> v  .:?  "errorText"



navigate :: Session a -> String -> Maybe String -> Maybe TransitionType -> Maybe FrameId -> IO (Either Error Navigate)
navigate session navigateUrl navigateReferrer navigateTransitionType navigateFrameId = sendReceiveCommandResult (conn session) ("Page","navigate") ([("url", ToJSONEx navigateUrl)] ++ (catMaybes [fmap (("referrer",) . ToJSONEx) navigateReferrer, fmap (("transitionType",) . ToJSONEx) navigateTransitionType, fmap (("frameId",) . ToJSONEx) navigateFrameId]))


navigateToHistoryEntry :: Session a -> Int -> IO (Maybe Error)
navigateToHistoryEntry session navigateToHistoryEntryEntryId = sendReceiveCommand (conn session) ("Page","navigateToHistoryEntry") ([("entryId", ToJSONEx navigateToHistoryEntryEntryId)] ++ (catMaybes []))

data PrintToPDF = PrintToPDF {
    printToPDFData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  PrintToPDF where
    parseJSON = A.withObject "PrintToPDF" $ \v ->
         PrintToPDF <$> v .:  "data"



printToPDF :: Session a -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> IO (Either Error PrintToPDF)
printToPDF session printToPdfLandscape printToPdfDisplayHeaderFooter printToPdfPrintBackground printToPdfScale printToPdfPaperWidth printToPdfPaperHeight printToPdfMarginTop printToPdfMarginBottom printToPdfMarginLeft printToPdfMarginRight printToPdfPageRanges printToPdfHeaderTemplate printToPdfFooterTemplate printToPdfPreferCssPageSize = sendReceiveCommandResult (conn session) ("Page","printToPDF") ([] ++ (catMaybes [fmap (("landscape",) . ToJSONEx) printToPdfLandscape, fmap (("displayHeaderFooter",) . ToJSONEx) printToPdfDisplayHeaderFooter, fmap (("printBackground",) . ToJSONEx) printToPdfPrintBackground, fmap (("scale",) . ToJSONEx) printToPdfScale, fmap (("paperWidth",) . ToJSONEx) printToPdfPaperWidth, fmap (("paperHeight",) . ToJSONEx) printToPdfPaperHeight, fmap (("marginTop",) . ToJSONEx) printToPdfMarginTop, fmap (("marginBottom",) . ToJSONEx) printToPdfMarginBottom, fmap (("marginLeft",) . ToJSONEx) printToPdfMarginLeft, fmap (("marginRight",) . ToJSONEx) printToPdfMarginRight, fmap (("pageRanges",) . ToJSONEx) printToPdfPageRanges, fmap (("headerTemplate",) . ToJSONEx) printToPdfHeaderTemplate, fmap (("footerTemplate",) . ToJSONEx) printToPdfFooterTemplate, fmap (("preferCSSPageSize",) . ToJSONEx) printToPdfPreferCssPageSize]))


reload :: Session a -> Maybe Bool -> Maybe String -> IO (Maybe Error)
reload session reloadIgnoreCache reloadScriptToEvaluateOnLoad = sendReceiveCommand (conn session) ("Page","reload") ([] ++ (catMaybes [fmap (("ignoreCache",) . ToJSONEx) reloadIgnoreCache, fmap (("scriptToEvaluateOnLoad",) . ToJSONEx) reloadScriptToEvaluateOnLoad]))


removeScriptToEvaluateOnNewDocument :: Session a -> ScriptIdentifier -> IO (Maybe Error)
removeScriptToEvaluateOnNewDocument session removeScriptToEvaluateOnNewDocumentIdentifier = sendReceiveCommand (conn session) ("Page","removeScriptToEvaluateOnNewDocument") ([("identifier", ToJSONEx removeScriptToEvaluateOnNewDocumentIdentifier)] ++ (catMaybes []))


setDocumentContent :: Session a -> FrameId -> String -> IO (Maybe Error)
setDocumentContent session setDocumentContentFrameId setDocumentContentHtml = sendReceiveCommand (conn session) ("Page","setDocumentContent") ([("frameId", ToJSONEx setDocumentContentFrameId), ("html", ToJSONEx setDocumentContentHtml)] ++ (catMaybes []))


stopLoading :: Session a -> IO (Maybe Error)
stopLoading session  = sendReceiveCommand (conn session) ("Page","stopLoading") ([] ++ (catMaybes []))


