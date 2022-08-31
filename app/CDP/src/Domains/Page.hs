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

module Domains.Page (module Domains.Page) where

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

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


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

