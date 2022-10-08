{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains
( Event
, module CDP.Domains.Accessibility
, module CDP.Domains.Animation
, module CDP.Domains.Audits
, module CDP.Domains.BackgroundService
, module CDP.Domains.BrowserTarget
, module CDP.Domains.CSS
, module CDP.Domains.CacheStorage
, module CDP.Domains.Cast
, module CDP.Domains.DOMDebugger
, module CDP.Domains.DOMPageNetworkEmulationSecurity
, module CDP.Domains.DOMSnapshot
, module CDP.Domains.DOMStorage
, module CDP.Domains.Database
, module CDP.Domains.Debugger
, module CDP.Domains.DeviceOrientation
, module CDP.Domains.EventBreakpoints
, module CDP.Domains.Fetch
, module CDP.Domains.HeadlessExperimental
, module CDP.Domains.HeapProfiler
, module CDP.Domains.IO
, module CDP.Domains.IndexedDB
, module CDP.Domains.Input
, module CDP.Domains.Inspector
, module CDP.Domains.LayerTree
, module CDP.Domains.Log
, module CDP.Domains.Media
, module CDP.Domains.Memory
, module CDP.Domains.Overlay
, module CDP.Domains.Performance
, module CDP.Domains.PerformanceTimeline
, module CDP.Domains.Profiler
, module CDP.Domains.Runtime
, module CDP.Domains.ServiceWorker
, module CDP.Domains.Storage
, module CDP.Domains.SystemInfo
, module CDP.Domains.Tethering
, module CDP.Domains.Tracing
, module CDP.Domains.WebAudio
, module CDP.Domains.WebAuthn
) where

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

import CDP.Internal.Runtime
import CDP.Handle

import CDP.Domains.Accessibility
import CDP.Domains.Accessibility as Accessibility
import CDP.Domains.Animation
import CDP.Domains.Animation as Animation
import CDP.Domains.Audits
import CDP.Domains.Audits as Audits
import CDP.Domains.BackgroundService
import CDP.Domains.BackgroundService as BackgroundService
import CDP.Domains.BrowserTarget
import CDP.Domains.BrowserTarget as BrowserTarget
import CDP.Domains.CSS
import CDP.Domains.CSS as CSS
import CDP.Domains.CacheStorage
import CDP.Domains.CacheStorage as CacheStorage
import CDP.Domains.Cast
import CDP.Domains.Cast as Cast
import CDP.Domains.DOMDebugger
import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetworkEmulationSecurity
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.DOMSnapshot
import CDP.Domains.DOMSnapshot as DOMSnapshot
import CDP.Domains.DOMStorage
import CDP.Domains.DOMStorage as DOMStorage
import CDP.Domains.Database
import CDP.Domains.Database as Database
import CDP.Domains.Debugger
import CDP.Domains.Debugger as Debugger
import CDP.Domains.DeviceOrientation
import CDP.Domains.DeviceOrientation as DeviceOrientation
import CDP.Domains.EventBreakpoints
import CDP.Domains.EventBreakpoints as EventBreakpoints
import CDP.Domains.Fetch
import CDP.Domains.Fetch as Fetch
import CDP.Domains.HeadlessExperimental
import CDP.Domains.HeadlessExperimental as HeadlessExperimental
import CDP.Domains.HeapProfiler
import CDP.Domains.HeapProfiler as HeapProfiler
import CDP.Domains.IO
import CDP.Domains.IO as IO
import CDP.Domains.IndexedDB
import CDP.Domains.IndexedDB as IndexedDB
import CDP.Domains.Input
import CDP.Domains.Input as Input
import CDP.Domains.Inspector
import CDP.Domains.Inspector as Inspector
import CDP.Domains.LayerTree
import CDP.Domains.LayerTree as LayerTree
import CDP.Domains.Log
import CDP.Domains.Log as Log
import CDP.Domains.Media
import CDP.Domains.Media as Media
import CDP.Domains.Memory
import CDP.Domains.Memory as Memory
import CDP.Domains.Overlay
import CDP.Domains.Overlay as Overlay
import CDP.Domains.Performance
import CDP.Domains.Performance as Performance
import CDP.Domains.PerformanceTimeline
import CDP.Domains.PerformanceTimeline as PerformanceTimeline
import CDP.Domains.Profiler
import CDP.Domains.Profiler as Profiler
import CDP.Domains.Runtime
import CDP.Domains.Runtime as Runtime
import CDP.Domains.ServiceWorker
import CDP.Domains.ServiceWorker as ServiceWorker
import CDP.Domains.Storage
import CDP.Domains.Storage as Storage
import CDP.Domains.SystemInfo
import CDP.Domains.SystemInfo as SystemInfo
import CDP.Domains.Tethering
import CDP.Domains.Tethering as Tethering
import CDP.Domains.Tracing
import CDP.Domains.Tracing as Tracing
import CDP.Domains.WebAudio
import CDP.Domains.WebAudio as WebAudio
import CDP.Domains.WebAuthn
import CDP.Domains.WebAuthn as WebAuthn


data Event = EVAccessibilityLoadComplete Accessibility.AccessibilityLoadComplete | EVAccessibilityNodesUpdated Accessibility.AccessibilityNodesUpdated | EVAnimationAnimationCanceled Animation.AnimationAnimationCanceled | EVAnimationAnimationCreated Animation.AnimationAnimationCreated | EVAnimationAnimationStarted Animation.AnimationAnimationStarted | EVAuditsIssueAdded Audits.AuditsIssueAdded | EVBackgroundServiceRecordingStateChanged BackgroundService.BackgroundServiceRecordingStateChanged | EVBackgroundServiceBackgroundServiceEventReceived BackgroundService.BackgroundServiceBackgroundServiceEventReceived | EVBrowserDownloadWillBegin BrowserTarget.BrowserDownloadWillBegin | EVBrowserDownloadProgress BrowserTarget.BrowserDownloadProgress | EVCssFontsUpdated CSS.CssFontsUpdated | EVCssMediaQueryResultChanged CSS.CssMediaQueryResultChanged | EVCssStyleSheetAdded CSS.CssStyleSheetAdded | EVCssStyleSheetChanged CSS.CssStyleSheetChanged | EVCssStyleSheetRemoved CSS.CssStyleSheetRemoved | EVCastSinksUpdated Cast.CastSinksUpdated | EVCastIssueUpdated Cast.CastIssueUpdated | EVDomAttributeModified DOMPageNetworkEmulationSecurity.DomAttributeModified | EVDomAttributeRemoved DOMPageNetworkEmulationSecurity.DomAttributeRemoved | EVDomCharacterDataModified DOMPageNetworkEmulationSecurity.DomCharacterDataModified | EVDomChildNodeCountUpdated DOMPageNetworkEmulationSecurity.DomChildNodeCountUpdated | EVDomChildNodeInserted DOMPageNetworkEmulationSecurity.DomChildNodeInserted | EVDomChildNodeRemoved DOMPageNetworkEmulationSecurity.DomChildNodeRemoved | EVDomDistributedNodesUpdated DOMPageNetworkEmulationSecurity.DomDistributedNodesUpdated | EVDomDocumentUpdated DOMPageNetworkEmulationSecurity.DomDocumentUpdated | EVDomInlineStyleInvalidated DOMPageNetworkEmulationSecurity.DomInlineStyleInvalidated | EVDomPseudoElementAdded DOMPageNetworkEmulationSecurity.DomPseudoElementAdded | EVDomPseudoElementRemoved DOMPageNetworkEmulationSecurity.DomPseudoElementRemoved | EVDomSetChildNodes DOMPageNetworkEmulationSecurity.DomSetChildNodes | EVDomShadowRootPopped DOMPageNetworkEmulationSecurity.DomShadowRootPopped | EVDomShadowRootPushed DOMPageNetworkEmulationSecurity.DomShadowRootPushed | EVDomStorageDomStorageItemAdded DOMStorage.DomStorageDomStorageItemAdded | EVDomStorageDomStorageItemRemoved DOMStorage.DomStorageDomStorageItemRemoved | EVDomStorageDomStorageItemUpdated DOMStorage.DomStorageDomStorageItemUpdated | EVDomStorageDomStorageItemsCleared DOMStorage.DomStorageDomStorageItemsCleared | EVDatabaseAddDatabase Database.DatabaseAddDatabase | EVEmulationVirtualTimeBudgetExpired DOMPageNetworkEmulationSecurity.EmulationVirtualTimeBudgetExpired | EVInputDragIntercepted Input.InputDragIntercepted | EVInspectorDetached Inspector.InspectorDetached | EVInspectorTargetCrashed Inspector.InspectorTargetCrashed | EVInspectorTargetReloadedAfterCrash Inspector.InspectorTargetReloadedAfterCrash | EVLayerTreeLayerPainted LayerTree.LayerTreeLayerPainted | EVLayerTreeLayerTreeDidChange LayerTree.LayerTreeLayerTreeDidChange | EVLogEntryAdded Log.LogEntryAdded | EVNetworkDataReceived DOMPageNetworkEmulationSecurity.NetworkDataReceived | EVNetworkEventSourceMessageReceived DOMPageNetworkEmulationSecurity.NetworkEventSourceMessageReceived | EVNetworkLoadingFailed DOMPageNetworkEmulationSecurity.NetworkLoadingFailed | EVNetworkLoadingFinished DOMPageNetworkEmulationSecurity.NetworkLoadingFinished | EVNetworkRequestServedFromCache DOMPageNetworkEmulationSecurity.NetworkRequestServedFromCache | EVNetworkRequestWillBeSent DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSent | EVNetworkResourceChangedPriority DOMPageNetworkEmulationSecurity.NetworkResourceChangedPriority | EVNetworkSignedExchangeReceived DOMPageNetworkEmulationSecurity.NetworkSignedExchangeReceived | EVNetworkResponseReceived DOMPageNetworkEmulationSecurity.NetworkResponseReceived | EVNetworkWebSocketClosed DOMPageNetworkEmulationSecurity.NetworkWebSocketClosed | EVNetworkWebSocketCreated DOMPageNetworkEmulationSecurity.NetworkWebSocketCreated | EVNetworkWebSocketFrameError DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameError | EVNetworkWebSocketFrameReceived DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameReceived | EVNetworkWebSocketFrameSent DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameSent | EVNetworkWebSocketHandshakeResponseReceived DOMPageNetworkEmulationSecurity.NetworkWebSocketHandshakeResponseReceived | EVNetworkWebSocketWillSendHandshakeRequest DOMPageNetworkEmulationSecurity.NetworkWebSocketWillSendHandshakeRequest | EVNetworkWebTransportCreated DOMPageNetworkEmulationSecurity.NetworkWebTransportCreated | EVNetworkWebTransportConnectionEstablished DOMPageNetworkEmulationSecurity.NetworkWebTransportConnectionEstablished | EVNetworkWebTransportClosed DOMPageNetworkEmulationSecurity.NetworkWebTransportClosed | EVNetworkRequestWillBeSentExtraInfo DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSentExtraInfo | EVNetworkResponseReceivedExtraInfo DOMPageNetworkEmulationSecurity.NetworkResponseReceivedExtraInfo | EVNetworkTrustTokenOperationDone DOMPageNetworkEmulationSecurity.NetworkTrustTokenOperationDone | EVNetworkSubresourceWebBundleMetadataReceived DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataReceived | EVNetworkSubresourceWebBundleMetadataError DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataError | EVNetworkSubresourceWebBundleInnerResponseParsed DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseParsed | EVNetworkSubresourceWebBundleInnerResponseError DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseError | EVNetworkReportingApiReportAdded DOMPageNetworkEmulationSecurity.NetworkReportingApiReportAdded | EVNetworkReportingApiReportUpdated DOMPageNetworkEmulationSecurity.NetworkReportingApiReportUpdated | EVNetworkReportingApiEndpointsChangedForOrigin DOMPageNetworkEmulationSecurity.NetworkReportingApiEndpointsChangedForOrigin | EVOverlayInspectNodeRequested Overlay.OverlayInspectNodeRequested | EVOverlayNodeHighlightRequested Overlay.OverlayNodeHighlightRequested | EVOverlayScreenshotRequested Overlay.OverlayScreenshotRequested | EVOverlayInspectModeCanceled Overlay.OverlayInspectModeCanceled | EVPageDomContentEventFired DOMPageNetworkEmulationSecurity.PageDomContentEventFired | EVPageFileChooserOpened DOMPageNetworkEmulationSecurity.PageFileChooserOpened | EVPageFrameAttached DOMPageNetworkEmulationSecurity.PageFrameAttached | EVPageFrameDetached DOMPageNetworkEmulationSecurity.PageFrameDetached | EVPageFrameNavigated DOMPageNetworkEmulationSecurity.PageFrameNavigated | EVPageDocumentOpened DOMPageNetworkEmulationSecurity.PageDocumentOpened | EVPageFrameResized DOMPageNetworkEmulationSecurity.PageFrameResized | EVPageFrameRequestedNavigation DOMPageNetworkEmulationSecurity.PageFrameRequestedNavigation | EVPageFrameStartedLoading DOMPageNetworkEmulationSecurity.PageFrameStartedLoading | EVPageFrameStoppedLoading DOMPageNetworkEmulationSecurity.PageFrameStoppedLoading | EVPageInterstitialHidden DOMPageNetworkEmulationSecurity.PageInterstitialHidden | EVPageInterstitialShown DOMPageNetworkEmulationSecurity.PageInterstitialShown | EVPageJavascriptDialogClosed DOMPageNetworkEmulationSecurity.PageJavascriptDialogClosed | EVPageJavascriptDialogOpening DOMPageNetworkEmulationSecurity.PageJavascriptDialogOpening | EVPageLifecycleEvent DOMPageNetworkEmulationSecurity.PageLifecycleEvent | EVPageBackForwardCacheNotUsed DOMPageNetworkEmulationSecurity.PageBackForwardCacheNotUsed | EVPagePrerenderAttemptCompleted DOMPageNetworkEmulationSecurity.PagePrerenderAttemptCompleted | EVPageLoadEventFired DOMPageNetworkEmulationSecurity.PageLoadEventFired | EVPageNavigatedWithinDocument DOMPageNetworkEmulationSecurity.PageNavigatedWithinDocument | EVPageScreencastFrame DOMPageNetworkEmulationSecurity.PageScreencastFrame | EVPageScreencastVisibilityChanged DOMPageNetworkEmulationSecurity.PageScreencastVisibilityChanged | EVPageWindowOpen DOMPageNetworkEmulationSecurity.PageWindowOpen | EVPageCompilationCacheProduced DOMPageNetworkEmulationSecurity.PageCompilationCacheProduced | EVPerformanceMetrics Performance.PerformanceMetrics | EVPerformanceTimelineTimelineEventAdded PerformanceTimeline.PerformanceTimelineTimelineEventAdded | EVSecurityVisibleSecurityStateChanged DOMPageNetworkEmulationSecurity.SecurityVisibleSecurityStateChanged | EVServiceWorkerWorkerErrorReported ServiceWorker.ServiceWorkerWorkerErrorReported | EVServiceWorkerWorkerRegistrationUpdated ServiceWorker.ServiceWorkerWorkerRegistrationUpdated | EVServiceWorkerWorkerVersionUpdated ServiceWorker.ServiceWorkerWorkerVersionUpdated | EVStorageCacheStorageContentUpdated Storage.StorageCacheStorageContentUpdated | EVStorageCacheStorageListUpdated Storage.StorageCacheStorageListUpdated | EVStorageIndexedDbContentUpdated Storage.StorageIndexedDbContentUpdated | EVStorageIndexedDbListUpdated Storage.StorageIndexedDbListUpdated | EVStorageInterestGroupAccessed Storage.StorageInterestGroupAccessed | EVTargetAttachedToTarget BrowserTarget.TargetAttachedToTarget | EVTargetDetachedFromTarget BrowserTarget.TargetDetachedFromTarget | EVTargetReceivedMessageFromTarget BrowserTarget.TargetReceivedMessageFromTarget | EVTargetTargetCreated BrowserTarget.TargetTargetCreated | EVTargetTargetDestroyed BrowserTarget.TargetTargetDestroyed | EVTargetTargetCrashed BrowserTarget.TargetTargetCrashed | EVTargetTargetInfoChanged BrowserTarget.TargetTargetInfoChanged | EVTetheringAccepted Tethering.TetheringAccepted | EVTracingBufferUsage Tracing.TracingBufferUsage | EVTracingDataCollected Tracing.TracingDataCollected | EVTracingTracingComplete Tracing.TracingTracingComplete | EVFetchRequestPaused Fetch.FetchRequestPaused | EVFetchAuthRequired Fetch.FetchAuthRequired | EVWebAudioContextCreated WebAudio.WebAudioContextCreated | EVWebAudioContextWillBeDestroyed WebAudio.WebAudioContextWillBeDestroyed | EVWebAudioContextChanged WebAudio.WebAudioContextChanged | EVWebAudioAudioListenerCreated WebAudio.WebAudioAudioListenerCreated | EVWebAudioAudioListenerWillBeDestroyed WebAudio.WebAudioAudioListenerWillBeDestroyed | EVWebAudioAudioNodeCreated WebAudio.WebAudioAudioNodeCreated | EVWebAudioAudioNodeWillBeDestroyed WebAudio.WebAudioAudioNodeWillBeDestroyed | EVWebAudioAudioParamCreated WebAudio.WebAudioAudioParamCreated | EVWebAudioAudioParamWillBeDestroyed WebAudio.WebAudioAudioParamWillBeDestroyed | EVWebAudioNodesConnected WebAudio.WebAudioNodesConnected | EVWebAudioNodesDisconnected WebAudio.WebAudioNodesDisconnected | EVWebAudioNodeParamConnected WebAudio.WebAudioNodeParamConnected | EVWebAudioNodeParamDisconnected WebAudio.WebAudioNodeParamDisconnected | EVMediaPlayerPropertiesChanged Media.MediaPlayerPropertiesChanged | EVMediaPlayerEventsAdded Media.MediaPlayerEventsAdded | EVMediaPlayerMessagesLogged Media.MediaPlayerMessagesLogged | EVMediaPlayerErrorsRaised Media.MediaPlayerErrorsRaised | EVMediaPlayersCreated Media.MediaPlayersCreated | EVDebuggerBreakpointResolved Debugger.DebuggerBreakpointResolved | EVDebuggerPaused Debugger.DebuggerPaused | EVDebuggerResumed Debugger.DebuggerResumed | EVDebuggerScriptFailedToParse Debugger.DebuggerScriptFailedToParse | EVDebuggerScriptParsed Debugger.DebuggerScriptParsed | EVHeapProfilerAddHeapSnapshotChunk HeapProfiler.HeapProfilerAddHeapSnapshotChunk | EVHeapProfilerHeapStatsUpdate HeapProfiler.HeapProfilerHeapStatsUpdate | EVHeapProfilerLastSeenObjectId HeapProfiler.HeapProfilerLastSeenObjectId | EVHeapProfilerReportHeapSnapshotProgress HeapProfiler.HeapProfilerReportHeapSnapshotProgress | EVHeapProfilerResetProfiles HeapProfiler.HeapProfilerResetProfiles | EVProfilerConsoleProfileFinished Profiler.ProfilerConsoleProfileFinished | EVProfilerConsoleProfileStarted Profiler.ProfilerConsoleProfileStarted | EVProfilerPreciseCoverageDeltaUpdate Profiler.ProfilerPreciseCoverageDeltaUpdate | EVRuntimeBindingCalled Runtime.RuntimeBindingCalled | EVRuntimeConsoleApiCalled Runtime.RuntimeConsoleApiCalled | EVRuntimeExceptionRevoked Runtime.RuntimeExceptionRevoked | EVRuntimeExceptionThrown Runtime.RuntimeExceptionThrown | EVRuntimeExecutionContextCreated Runtime.RuntimeExecutionContextCreated | EVRuntimeExecutionContextDestroyed Runtime.RuntimeExecutionContextDestroyed | EVRuntimeExecutionContextsCleared Runtime.RuntimeExecutionContextsCleared | EVRuntimeInspectRequested Runtime.RuntimeInspectRequested
  deriving (Eq, Show, Read)
instance FromJSON (EventResponse Event ) where
   parseJSON = A.withObject  "EventResponse"  $ \obj -> do
       name <- obj .: "method"
       case (name :: String) of
           "Accessibility.loadComplete" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Accessibility.AccessibilityLoadComplete) . fmap EVAccessibilityLoadComplete <$> obj .:? "params"
           "Accessibility.nodesUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Accessibility.AccessibilityNodesUpdated) . fmap EVAccessibilityNodesUpdated <$> obj .:? "params"
           "Animation.animationCanceled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Animation.AnimationAnimationCanceled) . fmap EVAnimationAnimationCanceled <$> obj .:? "params"
           "Animation.animationCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Animation.AnimationAnimationCreated) . fmap EVAnimationAnimationCreated <$> obj .:? "params"
           "Animation.animationStarted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Animation.AnimationAnimationStarted) . fmap EVAnimationAnimationStarted <$> obj .:? "params"
           "Audits.issueAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Audits.AuditsIssueAdded) . fmap EVAuditsIssueAdded <$> obj .:? "params"
           "BackgroundService.recordingStateChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BackgroundService.BackgroundServiceRecordingStateChanged) . fmap EVBackgroundServiceRecordingStateChanged <$> obj .:? "params"
           "BackgroundService.backgroundServiceEventReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BackgroundService.BackgroundServiceBackgroundServiceEventReceived) . fmap EVBackgroundServiceBackgroundServiceEventReceived <$> obj .:? "params"
           "Browser.downloadWillBegin" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.BrowserDownloadWillBegin) . fmap EVBrowserDownloadWillBegin <$> obj .:? "params"
           "Browser.downloadProgress" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.BrowserDownloadProgress) . fmap EVBrowserDownloadProgress <$> obj .:? "params"
           "CSS.fontsUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy CSS.CssFontsUpdated) . fmap EVCssFontsUpdated <$> obj .:? "params"
           "CSS.mediaQueryResultChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy CSS.CssMediaQueryResultChanged) . fmap EVCssMediaQueryResultChanged <$> obj .:? "params"
           "CSS.styleSheetAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy CSS.CssStyleSheetAdded) . fmap EVCssStyleSheetAdded <$> obj .:? "params"
           "CSS.styleSheetChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy CSS.CssStyleSheetChanged) . fmap EVCssStyleSheetChanged <$> obj .:? "params"
           "CSS.styleSheetRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy CSS.CssStyleSheetRemoved) . fmap EVCssStyleSheetRemoved <$> obj .:? "params"
           "Cast.sinksUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Cast.CastSinksUpdated) . fmap EVCastSinksUpdated <$> obj .:? "params"
           "Cast.issueUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Cast.CastIssueUpdated) . fmap EVCastIssueUpdated <$> obj .:? "params"
           "DOM.attributeModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomAttributeModified) . fmap EVDomAttributeModified <$> obj .:? "params"
           "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomAttributeRemoved) . fmap EVDomAttributeRemoved <$> obj .:? "params"
           "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomCharacterDataModified) . fmap EVDomCharacterDataModified <$> obj .:? "params"
           "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomChildNodeCountUpdated) . fmap EVDomChildNodeCountUpdated <$> obj .:? "params"
           "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomChildNodeInserted) . fmap EVDomChildNodeInserted <$> obj .:? "params"
           "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomChildNodeRemoved) . fmap EVDomChildNodeRemoved <$> obj .:? "params"
           "DOM.distributedNodesUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomDistributedNodesUpdated) . fmap EVDomDistributedNodesUpdated <$> obj .:? "params"
           "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomDocumentUpdated) . fmap EVDomDocumentUpdated <$> obj .:? "params"
           "DOM.inlineStyleInvalidated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomInlineStyleInvalidated) . fmap EVDomInlineStyleInvalidated <$> obj .:? "params"
           "DOM.pseudoElementAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomPseudoElementAdded) . fmap EVDomPseudoElementAdded <$> obj .:? "params"
           "DOM.pseudoElementRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomPseudoElementRemoved) . fmap EVDomPseudoElementRemoved <$> obj .:? "params"
           "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomSetChildNodes) . fmap EVDomSetChildNodes <$> obj .:? "params"
           "DOM.shadowRootPopped" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomShadowRootPopped) . fmap EVDomShadowRootPopped <$> obj .:? "params"
           "DOM.shadowRootPushed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.DomShadowRootPushed) . fmap EVDomShadowRootPushed <$> obj .:? "params"
           "DOMStorage.domStorageItemAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMStorage.DomStorageDomStorageItemAdded) . fmap EVDomStorageDomStorageItemAdded <$> obj .:? "params"
           "DOMStorage.domStorageItemRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMStorage.DomStorageDomStorageItemRemoved) . fmap EVDomStorageDomStorageItemRemoved <$> obj .:? "params"
           "DOMStorage.domStorageItemUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMStorage.DomStorageDomStorageItemUpdated) . fmap EVDomStorageDomStorageItemUpdated <$> obj .:? "params"
           "DOMStorage.domStorageItemsCleared" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMStorage.DomStorageDomStorageItemsCleared) . fmap EVDomStorageDomStorageItemsCleared <$> obj .:? "params"
           "Database.addDatabase" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Database.DatabaseAddDatabase) . fmap EVDatabaseAddDatabase <$> obj .:? "params"
           "Emulation.virtualTimeBudgetExpired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.EmulationVirtualTimeBudgetExpired) . fmap EVEmulationVirtualTimeBudgetExpired <$> obj .:? "params"
           "Input.dragIntercepted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Input.InputDragIntercepted) . fmap EVInputDragIntercepted <$> obj .:? "params"
           "Inspector.detached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Inspector.InspectorDetached) . fmap EVInspectorDetached <$> obj .:? "params"
           "Inspector.targetCrashed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Inspector.InspectorTargetCrashed) . fmap EVInspectorTargetCrashed <$> obj .:? "params"
           "Inspector.targetReloadedAfterCrash" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Inspector.InspectorTargetReloadedAfterCrash) . fmap EVInspectorTargetReloadedAfterCrash <$> obj .:? "params"
           "LayerTree.layerPainted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy LayerTree.LayerTreeLayerPainted) . fmap EVLayerTreeLayerPainted <$> obj .:? "params"
           "LayerTree.layerTreeDidChange" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy LayerTree.LayerTreeLayerTreeDidChange) . fmap EVLayerTreeLayerTreeDidChange <$> obj .:? "params"
           "Log.entryAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Log.LogEntryAdded) . fmap EVLogEntryAdded <$> obj .:? "params"
           "Network.dataReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkDataReceived) . fmap EVNetworkDataReceived <$> obj .:? "params"
           "Network.eventSourceMessageReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkEventSourceMessageReceived) . fmap EVNetworkEventSourceMessageReceived <$> obj .:? "params"
           "Network.loadingFailed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkLoadingFailed) . fmap EVNetworkLoadingFailed <$> obj .:? "params"
           "Network.loadingFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkLoadingFinished) . fmap EVNetworkLoadingFinished <$> obj .:? "params"
           "Network.requestServedFromCache" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkRequestServedFromCache) . fmap EVNetworkRequestServedFromCache <$> obj .:? "params"
           "Network.requestWillBeSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSent) . fmap EVNetworkRequestWillBeSent <$> obj .:? "params"
           "Network.resourceChangedPriority" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkResourceChangedPriority) . fmap EVNetworkResourceChangedPriority <$> obj .:? "params"
           "Network.signedExchangeReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkSignedExchangeReceived) . fmap EVNetworkSignedExchangeReceived <$> obj .:? "params"
           "Network.responseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkResponseReceived) . fmap EVNetworkResponseReceived <$> obj .:? "params"
           "Network.webSocketClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketClosed) . fmap EVNetworkWebSocketClosed <$> obj .:? "params"
           "Network.webSocketCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketCreated) . fmap EVNetworkWebSocketCreated <$> obj .:? "params"
           "Network.webSocketFrameError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameError) . fmap EVNetworkWebSocketFrameError <$> obj .:? "params"
           "Network.webSocketFrameReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameReceived) . fmap EVNetworkWebSocketFrameReceived <$> obj .:? "params"
           "Network.webSocketFrameSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameSent) . fmap EVNetworkWebSocketFrameSent <$> obj .:? "params"
           "Network.webSocketHandshakeResponseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketHandshakeResponseReceived) . fmap EVNetworkWebSocketHandshakeResponseReceived <$> obj .:? "params"
           "Network.webSocketWillSendHandshakeRequest" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebSocketWillSendHandshakeRequest) . fmap EVNetworkWebSocketWillSendHandshakeRequest <$> obj .:? "params"
           "Network.webTransportCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebTransportCreated) . fmap EVNetworkWebTransportCreated <$> obj .:? "params"
           "Network.webTransportConnectionEstablished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebTransportConnectionEstablished) . fmap EVNetworkWebTransportConnectionEstablished <$> obj .:? "params"
           "Network.webTransportClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkWebTransportClosed) . fmap EVNetworkWebTransportClosed <$> obj .:? "params"
           "Network.requestWillBeSentExtraInfo" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSentExtraInfo) . fmap EVNetworkRequestWillBeSentExtraInfo <$> obj .:? "params"
           "Network.responseReceivedExtraInfo" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkResponseReceivedExtraInfo) . fmap EVNetworkResponseReceivedExtraInfo <$> obj .:? "params"
           "Network.trustTokenOperationDone" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkTrustTokenOperationDone) . fmap EVNetworkTrustTokenOperationDone <$> obj .:? "params"
           "Network.subresourceWebBundleMetadataReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataReceived) . fmap EVNetworkSubresourceWebBundleMetadataReceived <$> obj .:? "params"
           "Network.subresourceWebBundleMetadataError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataError) . fmap EVNetworkSubresourceWebBundleMetadataError <$> obj .:? "params"
           "Network.subresourceWebBundleInnerResponseParsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseParsed) . fmap EVNetworkSubresourceWebBundleInnerResponseParsed <$> obj .:? "params"
           "Network.subresourceWebBundleInnerResponseError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseError) . fmap EVNetworkSubresourceWebBundleInnerResponseError <$> obj .:? "params"
           "Network.reportingApiReportAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkReportingApiReportAdded) . fmap EVNetworkReportingApiReportAdded <$> obj .:? "params"
           "Network.reportingApiReportUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkReportingApiReportUpdated) . fmap EVNetworkReportingApiReportUpdated <$> obj .:? "params"
           "Network.reportingApiEndpointsChangedForOrigin" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.NetworkReportingApiEndpointsChangedForOrigin) . fmap EVNetworkReportingApiEndpointsChangedForOrigin <$> obj .:? "params"
           "Overlay.inspectNodeRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Overlay.OverlayInspectNodeRequested) . fmap EVOverlayInspectNodeRequested <$> obj .:? "params"
           "Overlay.nodeHighlightRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Overlay.OverlayNodeHighlightRequested) . fmap EVOverlayNodeHighlightRequested <$> obj .:? "params"
           "Overlay.screenshotRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Overlay.OverlayScreenshotRequested) . fmap EVOverlayScreenshotRequested <$> obj .:? "params"
           "Overlay.inspectModeCanceled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Overlay.OverlayInspectModeCanceled) . fmap EVOverlayInspectModeCanceled <$> obj .:? "params"
           "Page.domContentEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageDomContentEventFired) . fmap EVPageDomContentEventFired <$> obj .:? "params"
           "Page.fileChooserOpened" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFileChooserOpened) . fmap EVPageFileChooserOpened <$> obj .:? "params"
           "Page.frameAttached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameAttached) . fmap EVPageFrameAttached <$> obj .:? "params"
           "Page.frameDetached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameDetached) . fmap EVPageFrameDetached <$> obj .:? "params"
           "Page.frameNavigated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameNavigated) . fmap EVPageFrameNavigated <$> obj .:? "params"
           "Page.documentOpened" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageDocumentOpened) . fmap EVPageDocumentOpened <$> obj .:? "params"
           "Page.frameResized" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameResized) . fmap EVPageFrameResized <$> obj .:? "params"
           "Page.frameRequestedNavigation" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameRequestedNavigation) . fmap EVPageFrameRequestedNavigation <$> obj .:? "params"
           "Page.frameStartedLoading" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameStartedLoading) . fmap EVPageFrameStartedLoading <$> obj .:? "params"
           "Page.frameStoppedLoading" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageFrameStoppedLoading) . fmap EVPageFrameStoppedLoading <$> obj .:? "params"
           "Page.interstitialHidden" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageInterstitialHidden) . fmap EVPageInterstitialHidden <$> obj .:? "params"
           "Page.interstitialShown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageInterstitialShown) . fmap EVPageInterstitialShown <$> obj .:? "params"
           "Page.javascriptDialogClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageJavascriptDialogClosed) . fmap EVPageJavascriptDialogClosed <$> obj .:? "params"
           "Page.javascriptDialogOpening" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageJavascriptDialogOpening) . fmap EVPageJavascriptDialogOpening <$> obj .:? "params"
           "Page.lifecycleEvent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageLifecycleEvent) . fmap EVPageLifecycleEvent <$> obj .:? "params"
           "Page.backForwardCacheNotUsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageBackForwardCacheNotUsed) . fmap EVPageBackForwardCacheNotUsed <$> obj .:? "params"
           "Page.prerenderAttemptCompleted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PagePrerenderAttemptCompleted) . fmap EVPagePrerenderAttemptCompleted <$> obj .:? "params"
           "Page.loadEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageLoadEventFired) . fmap EVPageLoadEventFired <$> obj .:? "params"
           "Page.navigatedWithinDocument" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageNavigatedWithinDocument) . fmap EVPageNavigatedWithinDocument <$> obj .:? "params"
           "Page.screencastFrame" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageScreencastFrame) . fmap EVPageScreencastFrame <$> obj .:? "params"
           "Page.screencastVisibilityChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageScreencastVisibilityChanged) . fmap EVPageScreencastVisibilityChanged <$> obj .:? "params"
           "Page.windowOpen" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageWindowOpen) . fmap EVPageWindowOpen <$> obj .:? "params"
           "Page.compilationCacheProduced" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.PageCompilationCacheProduced) . fmap EVPageCompilationCacheProduced <$> obj .:? "params"
           "Performance.metrics" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Performance.PerformanceMetrics) . fmap EVPerformanceMetrics <$> obj .:? "params"
           "PerformanceTimeline.timelineEventAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy PerformanceTimeline.PerformanceTimelineTimelineEventAdded) . fmap EVPerformanceTimelineTimelineEventAdded <$> obj .:? "params"
           "Security.visibleSecurityStateChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetworkEmulationSecurity.SecurityVisibleSecurityStateChanged) . fmap EVSecurityVisibleSecurityStateChanged <$> obj .:? "params"
           "ServiceWorker.workerErrorReported" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ServiceWorker.ServiceWorkerWorkerErrorReported) . fmap EVServiceWorkerWorkerErrorReported <$> obj .:? "params"
           "ServiceWorker.workerRegistrationUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ServiceWorker.ServiceWorkerWorkerRegistrationUpdated) . fmap EVServiceWorkerWorkerRegistrationUpdated <$> obj .:? "params"
           "ServiceWorker.workerVersionUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy ServiceWorker.ServiceWorkerWorkerVersionUpdated) . fmap EVServiceWorkerWorkerVersionUpdated <$> obj .:? "params"
           "Storage.cacheStorageContentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Storage.StorageCacheStorageContentUpdated) . fmap EVStorageCacheStorageContentUpdated <$> obj .:? "params"
           "Storage.cacheStorageListUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Storage.StorageCacheStorageListUpdated) . fmap EVStorageCacheStorageListUpdated <$> obj .:? "params"
           "Storage.indexedDBContentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Storage.StorageIndexedDbContentUpdated) . fmap EVStorageIndexedDbContentUpdated <$> obj .:? "params"
           "Storage.indexedDBListUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Storage.StorageIndexedDbListUpdated) . fmap EVStorageIndexedDbListUpdated <$> obj .:? "params"
           "Storage.interestGroupAccessed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Storage.StorageInterestGroupAccessed) . fmap EVStorageInterestGroupAccessed <$> obj .:? "params"
           "Target.attachedToTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetAttachedToTarget) . fmap EVTargetAttachedToTarget <$> obj .:? "params"
           "Target.detachedFromTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetDetachedFromTarget) . fmap EVTargetDetachedFromTarget <$> obj .:? "params"
           "Target.receivedMessageFromTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetReceivedMessageFromTarget) . fmap EVTargetReceivedMessageFromTarget <$> obj .:? "params"
           "Target.targetCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetTargetCreated) . fmap EVTargetTargetCreated <$> obj .:? "params"
           "Target.targetDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetTargetDestroyed) . fmap EVTargetTargetDestroyed <$> obj .:? "params"
           "Target.targetCrashed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetTargetCrashed) . fmap EVTargetTargetCrashed <$> obj .:? "params"
           "Target.targetInfoChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy BrowserTarget.TargetTargetInfoChanged) . fmap EVTargetTargetInfoChanged <$> obj .:? "params"
           "Tethering.accepted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Tethering.TetheringAccepted) . fmap EVTetheringAccepted <$> obj .:? "params"
           "Tracing.bufferUsage" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Tracing.TracingBufferUsage) . fmap EVTracingBufferUsage <$> obj .:? "params"
           "Tracing.dataCollected" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Tracing.TracingDataCollected) . fmap EVTracingDataCollected <$> obj .:? "params"
           "Tracing.tracingComplete" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Tracing.TracingTracingComplete) . fmap EVTracingTracingComplete <$> obj .:? "params"
           "Fetch.requestPaused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Fetch.FetchRequestPaused) . fmap EVFetchRequestPaused <$> obj .:? "params"
           "Fetch.authRequired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Fetch.FetchAuthRequired) . fmap EVFetchAuthRequired <$> obj .:? "params"
           "WebAudio.contextCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioContextCreated) . fmap EVWebAudioContextCreated <$> obj .:? "params"
           "WebAudio.contextWillBeDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioContextWillBeDestroyed) . fmap EVWebAudioContextWillBeDestroyed <$> obj .:? "params"
           "WebAudio.contextChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioContextChanged) . fmap EVWebAudioContextChanged <$> obj .:? "params"
           "WebAudio.audioListenerCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioListenerCreated) . fmap EVWebAudioAudioListenerCreated <$> obj .:? "params"
           "WebAudio.audioListenerWillBeDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioListenerWillBeDestroyed) . fmap EVWebAudioAudioListenerWillBeDestroyed <$> obj .:? "params"
           "WebAudio.audioNodeCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioNodeCreated) . fmap EVWebAudioAudioNodeCreated <$> obj .:? "params"
           "WebAudio.audioNodeWillBeDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioNodeWillBeDestroyed) . fmap EVWebAudioAudioNodeWillBeDestroyed <$> obj .:? "params"
           "WebAudio.audioParamCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioParamCreated) . fmap EVWebAudioAudioParamCreated <$> obj .:? "params"
           "WebAudio.audioParamWillBeDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioAudioParamWillBeDestroyed) . fmap EVWebAudioAudioParamWillBeDestroyed <$> obj .:? "params"
           "WebAudio.nodesConnected" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioNodesConnected) . fmap EVWebAudioNodesConnected <$> obj .:? "params"
           "WebAudio.nodesDisconnected" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioNodesDisconnected) . fmap EVWebAudioNodesDisconnected <$> obj .:? "params"
           "WebAudio.nodeParamConnected" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioNodeParamConnected) . fmap EVWebAudioNodeParamConnected <$> obj .:? "params"
           "WebAudio.nodeParamDisconnected" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy WebAudio.WebAudioNodeParamDisconnected) . fmap EVWebAudioNodeParamDisconnected <$> obj .:? "params"
           "Media.playerPropertiesChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Media.MediaPlayerPropertiesChanged) . fmap EVMediaPlayerPropertiesChanged <$> obj .:? "params"
           "Media.playerEventsAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Media.MediaPlayerEventsAdded) . fmap EVMediaPlayerEventsAdded <$> obj .:? "params"
           "Media.playerMessagesLogged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Media.MediaPlayerMessagesLogged) . fmap EVMediaPlayerMessagesLogged <$> obj .:? "params"
           "Media.playerErrorsRaised" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Media.MediaPlayerErrorsRaised) . fmap EVMediaPlayerErrorsRaised <$> obj .:? "params"
           "Media.playersCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Media.MediaPlayersCreated) . fmap EVMediaPlayersCreated <$> obj .:? "params"
           "Debugger.breakpointResolved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerBreakpointResolved) . fmap EVDebuggerBreakpointResolved <$> obj .:? "params"
           "Debugger.paused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerPaused) . fmap EVDebuggerPaused <$> obj .:? "params"
           "Debugger.resumed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerResumed) . fmap EVDebuggerResumed <$> obj .:? "params"
           "Debugger.scriptFailedToParse" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerScriptFailedToParse) . fmap EVDebuggerScriptFailedToParse <$> obj .:? "params"
           "Debugger.scriptParsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerScriptParsed) . fmap EVDebuggerScriptParsed <$> obj .:? "params"
           "HeapProfiler.addHeapSnapshotChunk" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy HeapProfiler.HeapProfilerAddHeapSnapshotChunk) . fmap EVHeapProfilerAddHeapSnapshotChunk <$> obj .:? "params"
           "HeapProfiler.heapStatsUpdate" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy HeapProfiler.HeapProfilerHeapStatsUpdate) . fmap EVHeapProfilerHeapStatsUpdate <$> obj .:? "params"
           "HeapProfiler.lastSeenObjectId" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy HeapProfiler.HeapProfilerLastSeenObjectId) . fmap EVHeapProfilerLastSeenObjectId <$> obj .:? "params"
           "HeapProfiler.reportHeapSnapshotProgress" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy HeapProfiler.HeapProfilerReportHeapSnapshotProgress) . fmap EVHeapProfilerReportHeapSnapshotProgress <$> obj .:? "params"
           "HeapProfiler.resetProfiles" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy HeapProfiler.HeapProfilerResetProfiles) . fmap EVHeapProfilerResetProfiles <$> obj .:? "params"
           "Profiler.consoleProfileFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Profiler.ProfilerConsoleProfileFinished) . fmap EVProfilerConsoleProfileFinished <$> obj .:? "params"
           "Profiler.consoleProfileStarted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Profiler.ProfilerConsoleProfileStarted) . fmap EVProfilerConsoleProfileStarted <$> obj .:? "params"
           "Profiler.preciseCoverageDeltaUpdate" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Profiler.ProfilerPreciseCoverageDeltaUpdate) . fmap EVProfilerPreciseCoverageDeltaUpdate <$> obj .:? "params"
           "Runtime.bindingCalled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeBindingCalled) . fmap EVRuntimeBindingCalled <$> obj .:? "params"
           "Runtime.consoleAPICalled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeConsoleApiCalled) . fmap EVRuntimeConsoleApiCalled <$> obj .:? "params"
           "Runtime.exceptionRevoked" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExceptionRevoked) . fmap EVRuntimeExceptionRevoked <$> obj .:? "params"
           "Runtime.exceptionThrown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExceptionThrown) . fmap EVRuntimeExceptionThrown <$> obj .:? "params"
           "Runtime.executionContextCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextCreated) . fmap EVRuntimeExecutionContextCreated <$> obj .:? "params"
           "Runtime.executionContextDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextDestroyed) . fmap EVRuntimeExecutionContextDestroyed <$> obj .:? "params"
           "Runtime.executionContextsCleared" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextsCleared) . fmap EVRuntimeExecutionContextsCleared <$> obj .:? "params"
           "Runtime.inspectRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeInspectRequested) . fmap EVRuntimeInspectRequested <$> obj .:? "params"
           _ -> fail "failed to parse EventResponse"

instance FromEvent Event Accessibility.AccessibilityLoadComplete where
   eventName  _ _    =  "Accessibility.loadComplete"
   fromEvent ev      =  case ev of EVAccessibilityLoadComplete v -> Just v; _ -> Nothing

instance FromEvent Event Accessibility.AccessibilityNodesUpdated where
   eventName  _ _    =  "Accessibility.nodesUpdated"
   fromEvent ev      =  case ev of EVAccessibilityNodesUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Animation.AnimationAnimationCanceled where
   eventName  _ _    =  "Animation.animationCanceled"
   fromEvent ev      =  case ev of EVAnimationAnimationCanceled v -> Just v; _ -> Nothing

instance FromEvent Event Animation.AnimationAnimationCreated where
   eventName  _ _    =  "Animation.animationCreated"
   fromEvent ev      =  case ev of EVAnimationAnimationCreated v -> Just v; _ -> Nothing

instance FromEvent Event Animation.AnimationAnimationStarted where
   eventName  _ _    =  "Animation.animationStarted"
   fromEvent ev      =  case ev of EVAnimationAnimationStarted v -> Just v; _ -> Nothing

instance FromEvent Event Audits.AuditsIssueAdded where
   eventName  _ _    =  "Audits.issueAdded"
   fromEvent ev      =  case ev of EVAuditsIssueAdded v -> Just v; _ -> Nothing

instance FromEvent Event BackgroundService.BackgroundServiceRecordingStateChanged where
   eventName  _ _    =  "BackgroundService.recordingStateChanged"
   fromEvent ev      =  case ev of EVBackgroundServiceRecordingStateChanged v -> Just v; _ -> Nothing

instance FromEvent Event BackgroundService.BackgroundServiceBackgroundServiceEventReceived where
   eventName  _ _    =  "BackgroundService.backgroundServiceEventReceived"
   fromEvent ev      =  case ev of EVBackgroundServiceBackgroundServiceEventReceived v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.BrowserDownloadWillBegin where
   eventName  _ _    =  "Browser.downloadWillBegin"
   fromEvent ev      =  case ev of EVBrowserDownloadWillBegin v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.BrowserDownloadProgress where
   eventName  _ _    =  "Browser.downloadProgress"
   fromEvent ev      =  case ev of EVBrowserDownloadProgress v -> Just v; _ -> Nothing

instance FromEvent Event CSS.CssFontsUpdated where
   eventName  _ _    =  "CSS.fontsUpdated"
   fromEvent ev      =  case ev of EVCssFontsUpdated v -> Just v; _ -> Nothing

instance FromEvent Event CSS.CssMediaQueryResultChanged where
   eventName  _ _    =  "CSS.mediaQueryResultChanged"
   fromEvent ev      =  case ev of EVCssMediaQueryResultChanged v -> Just v; _ -> Nothing

instance FromEvent Event CSS.CssStyleSheetAdded where
   eventName  _ _    =  "CSS.styleSheetAdded"
   fromEvent ev      =  case ev of EVCssStyleSheetAdded v -> Just v; _ -> Nothing

instance FromEvent Event CSS.CssStyleSheetChanged where
   eventName  _ _    =  "CSS.styleSheetChanged"
   fromEvent ev      =  case ev of EVCssStyleSheetChanged v -> Just v; _ -> Nothing

instance FromEvent Event CSS.CssStyleSheetRemoved where
   eventName  _ _    =  "CSS.styleSheetRemoved"
   fromEvent ev      =  case ev of EVCssStyleSheetRemoved v -> Just v; _ -> Nothing

instance FromEvent Event Cast.CastSinksUpdated where
   eventName  _ _    =  "Cast.sinksUpdated"
   fromEvent ev      =  case ev of EVCastSinksUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Cast.CastIssueUpdated where
   eventName  _ _    =  "Cast.issueUpdated"
   fromEvent ev      =  case ev of EVCastIssueUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomAttributeModified where
   eventName  _ _    =  "DOM.attributeModified"
   fromEvent ev      =  case ev of EVDomAttributeModified v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomAttributeRemoved where
   eventName  _ _    =  "DOM.attributeRemoved"
   fromEvent ev      =  case ev of EVDomAttributeRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomCharacterDataModified where
   eventName  _ _    =  "DOM.characterDataModified"
   fromEvent ev      =  case ev of EVDomCharacterDataModified v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomChildNodeCountUpdated where
   eventName  _ _    =  "DOM.childNodeCountUpdated"
   fromEvent ev      =  case ev of EVDomChildNodeCountUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomChildNodeInserted where
   eventName  _ _    =  "DOM.childNodeInserted"
   fromEvent ev      =  case ev of EVDomChildNodeInserted v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomChildNodeRemoved where
   eventName  _ _    =  "DOM.childNodeRemoved"
   fromEvent ev      =  case ev of EVDomChildNodeRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomDistributedNodesUpdated where
   eventName  _ _    =  "DOM.distributedNodesUpdated"
   fromEvent ev      =  case ev of EVDomDistributedNodesUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomDocumentUpdated where
   eventName  _ _    =  "DOM.documentUpdated"
   fromEvent ev      =  case ev of EVDomDocumentUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomInlineStyleInvalidated where
   eventName  _ _    =  "DOM.inlineStyleInvalidated"
   fromEvent ev      =  case ev of EVDomInlineStyleInvalidated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomPseudoElementAdded where
   eventName  _ _    =  "DOM.pseudoElementAdded"
   fromEvent ev      =  case ev of EVDomPseudoElementAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomPseudoElementRemoved where
   eventName  _ _    =  "DOM.pseudoElementRemoved"
   fromEvent ev      =  case ev of EVDomPseudoElementRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomSetChildNodes where
   eventName  _ _    =  "DOM.setChildNodes"
   fromEvent ev      =  case ev of EVDomSetChildNodes v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomShadowRootPopped where
   eventName  _ _    =  "DOM.shadowRootPopped"
   fromEvent ev      =  case ev of EVDomShadowRootPopped v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.DomShadowRootPushed where
   eventName  _ _    =  "DOM.shadowRootPushed"
   fromEvent ev      =  case ev of EVDomShadowRootPushed v -> Just v; _ -> Nothing

instance FromEvent Event DOMStorage.DomStorageDomStorageItemAdded where
   eventName  _ _    =  "DOMStorage.domStorageItemAdded"
   fromEvent ev      =  case ev of EVDomStorageDomStorageItemAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMStorage.DomStorageDomStorageItemRemoved where
   eventName  _ _    =  "DOMStorage.domStorageItemRemoved"
   fromEvent ev      =  case ev of EVDomStorageDomStorageItemRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMStorage.DomStorageDomStorageItemUpdated where
   eventName  _ _    =  "DOMStorage.domStorageItemUpdated"
   fromEvent ev      =  case ev of EVDomStorageDomStorageItemUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMStorage.DomStorageDomStorageItemsCleared where
   eventName  _ _    =  "DOMStorage.domStorageItemsCleared"
   fromEvent ev      =  case ev of EVDomStorageDomStorageItemsCleared v -> Just v; _ -> Nothing

instance FromEvent Event Database.DatabaseAddDatabase where
   eventName  _ _    =  "Database.addDatabase"
   fromEvent ev      =  case ev of EVDatabaseAddDatabase v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.EmulationVirtualTimeBudgetExpired where
   eventName  _ _    =  "Emulation.virtualTimeBudgetExpired"
   fromEvent ev      =  case ev of EVEmulationVirtualTimeBudgetExpired v -> Just v; _ -> Nothing

instance FromEvent Event Input.InputDragIntercepted where
   eventName  _ _    =  "Input.dragIntercepted"
   fromEvent ev      =  case ev of EVInputDragIntercepted v -> Just v; _ -> Nothing

instance FromEvent Event Inspector.InspectorDetached where
   eventName  _ _    =  "Inspector.detached"
   fromEvent ev      =  case ev of EVInspectorDetached v -> Just v; _ -> Nothing

instance FromEvent Event Inspector.InspectorTargetCrashed where
   eventName  _ _    =  "Inspector.targetCrashed"
   fromEvent ev      =  case ev of EVInspectorTargetCrashed v -> Just v; _ -> Nothing

instance FromEvent Event Inspector.InspectorTargetReloadedAfterCrash where
   eventName  _ _    =  "Inspector.targetReloadedAfterCrash"
   fromEvent ev      =  case ev of EVInspectorTargetReloadedAfterCrash v -> Just v; _ -> Nothing

instance FromEvent Event LayerTree.LayerTreeLayerPainted where
   eventName  _ _    =  "LayerTree.layerPainted"
   fromEvent ev      =  case ev of EVLayerTreeLayerPainted v -> Just v; _ -> Nothing

instance FromEvent Event LayerTree.LayerTreeLayerTreeDidChange where
   eventName  _ _    =  "LayerTree.layerTreeDidChange"
   fromEvent ev      =  case ev of EVLayerTreeLayerTreeDidChange v -> Just v; _ -> Nothing

instance FromEvent Event Log.LogEntryAdded where
   eventName  _ _    =  "Log.entryAdded"
   fromEvent ev      =  case ev of EVLogEntryAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkDataReceived where
   eventName  _ _    =  "Network.dataReceived"
   fromEvent ev      =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkEventSourceMessageReceived where
   eventName  _ _    =  "Network.eventSourceMessageReceived"
   fromEvent ev      =  case ev of EVNetworkEventSourceMessageReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkLoadingFailed where
   eventName  _ _    =  "Network.loadingFailed"
   fromEvent ev      =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkLoadingFinished where
   eventName  _ _    =  "Network.loadingFinished"
   fromEvent ev      =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkRequestServedFromCache where
   eventName  _ _    =  "Network.requestServedFromCache"
   fromEvent ev      =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSent where
   eventName  _ _    =  "Network.requestWillBeSent"
   fromEvent ev      =  case ev of EVNetworkRequestWillBeSent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkResourceChangedPriority where
   eventName  _ _    =  "Network.resourceChangedPriority"
   fromEvent ev      =  case ev of EVNetworkResourceChangedPriority v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkSignedExchangeReceived where
   eventName  _ _    =  "Network.signedExchangeReceived"
   fromEvent ev      =  case ev of EVNetworkSignedExchangeReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkResponseReceived where
   eventName  _ _    =  "Network.responseReceived"
   fromEvent ev      =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketClosed where
   eventName  _ _    =  "Network.webSocketClosed"
   fromEvent ev      =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketCreated where
   eventName  _ _    =  "Network.webSocketCreated"
   fromEvent ev      =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameError where
   eventName  _ _    =  "Network.webSocketFrameError"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameReceived where
   eventName  _ _    =  "Network.webSocketFrameReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketFrameSent where
   eventName  _ _    =  "Network.webSocketFrameSent"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketHandshakeResponseReceived where
   eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebSocketWillSendHandshakeRequest where
   eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
   fromEvent ev      =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebTransportCreated where
   eventName  _ _    =  "Network.webTransportCreated"
   fromEvent ev      =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebTransportConnectionEstablished where
   eventName  _ _    =  "Network.webTransportConnectionEstablished"
   fromEvent ev      =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkWebTransportClosed where
   eventName  _ _    =  "Network.webTransportClosed"
   fromEvent ev      =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkRequestWillBeSentExtraInfo where
   eventName  _ _    =  "Network.requestWillBeSentExtraInfo"
   fromEvent ev      =  case ev of EVNetworkRequestWillBeSentExtraInfo v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkResponseReceivedExtraInfo where
   eventName  _ _    =  "Network.responseReceivedExtraInfo"
   fromEvent ev      =  case ev of EVNetworkResponseReceivedExtraInfo v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkTrustTokenOperationDone where
   eventName  _ _    =  "Network.trustTokenOperationDone"
   fromEvent ev      =  case ev of EVNetworkTrustTokenOperationDone v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataReceived where
   eventName  _ _    =  "Network.subresourceWebBundleMetadataReceived"
   fromEvent ev      =  case ev of EVNetworkSubresourceWebBundleMetadataReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleMetadataError where
   eventName  _ _    =  "Network.subresourceWebBundleMetadataError"
   fromEvent ev      =  case ev of EVNetworkSubresourceWebBundleMetadataError v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseParsed where
   eventName  _ _    =  "Network.subresourceWebBundleInnerResponseParsed"
   fromEvent ev      =  case ev of EVNetworkSubresourceWebBundleInnerResponseParsed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkSubresourceWebBundleInnerResponseError where
   eventName  _ _    =  "Network.subresourceWebBundleInnerResponseError"
   fromEvent ev      =  case ev of EVNetworkSubresourceWebBundleInnerResponseError v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkReportingApiReportAdded where
   eventName  _ _    =  "Network.reportingApiReportAdded"
   fromEvent ev      =  case ev of EVNetworkReportingApiReportAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkReportingApiReportUpdated where
   eventName  _ _    =  "Network.reportingApiReportUpdated"
   fromEvent ev      =  case ev of EVNetworkReportingApiReportUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.NetworkReportingApiEndpointsChangedForOrigin where
   eventName  _ _    =  "Network.reportingApiEndpointsChangedForOrigin"
   fromEvent ev      =  case ev of EVNetworkReportingApiEndpointsChangedForOrigin v -> Just v; _ -> Nothing

instance FromEvent Event Overlay.OverlayInspectNodeRequested where
   eventName  _ _    =  "Overlay.inspectNodeRequested"
   fromEvent ev      =  case ev of EVOverlayInspectNodeRequested v -> Just v; _ -> Nothing

instance FromEvent Event Overlay.OverlayNodeHighlightRequested where
   eventName  _ _    =  "Overlay.nodeHighlightRequested"
   fromEvent ev      =  case ev of EVOverlayNodeHighlightRequested v -> Just v; _ -> Nothing

instance FromEvent Event Overlay.OverlayScreenshotRequested where
   eventName  _ _    =  "Overlay.screenshotRequested"
   fromEvent ev      =  case ev of EVOverlayScreenshotRequested v -> Just v; _ -> Nothing

instance FromEvent Event Overlay.OverlayInspectModeCanceled where
   eventName  _ _    =  "Overlay.inspectModeCanceled"
   fromEvent ev      =  case ev of EVOverlayInspectModeCanceled v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageDomContentEventFired where
   eventName  _ _    =  "Page.domContentEventFired"
   fromEvent ev      =  case ev of EVPageDomContentEventFired v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFileChooserOpened where
   eventName  _ _    =  "Page.fileChooserOpened"
   fromEvent ev      =  case ev of EVPageFileChooserOpened v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameAttached where
   eventName  _ _    =  "Page.frameAttached"
   fromEvent ev      =  case ev of EVPageFrameAttached v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameDetached where
   eventName  _ _    =  "Page.frameDetached"
   fromEvent ev      =  case ev of EVPageFrameDetached v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameNavigated where
   eventName  _ _    =  "Page.frameNavigated"
   fromEvent ev      =  case ev of EVPageFrameNavigated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageDocumentOpened where
   eventName  _ _    =  "Page.documentOpened"
   fromEvent ev      =  case ev of EVPageDocumentOpened v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameResized where
   eventName  _ _    =  "Page.frameResized"
   fromEvent ev      =  case ev of EVPageFrameResized v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameRequestedNavigation where
   eventName  _ _    =  "Page.frameRequestedNavigation"
   fromEvent ev      =  case ev of EVPageFrameRequestedNavigation v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameStartedLoading where
   eventName  _ _    =  "Page.frameStartedLoading"
   fromEvent ev      =  case ev of EVPageFrameStartedLoading v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageFrameStoppedLoading where
   eventName  _ _    =  "Page.frameStoppedLoading"
   fromEvent ev      =  case ev of EVPageFrameStoppedLoading v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageInterstitialHidden where
   eventName  _ _    =  "Page.interstitialHidden"
   fromEvent ev      =  case ev of EVPageInterstitialHidden v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageInterstitialShown where
   eventName  _ _    =  "Page.interstitialShown"
   fromEvent ev      =  case ev of EVPageInterstitialShown v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageJavascriptDialogClosed where
   eventName  _ _    =  "Page.javascriptDialogClosed"
   fromEvent ev      =  case ev of EVPageJavascriptDialogClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageJavascriptDialogOpening where
   eventName  _ _    =  "Page.javascriptDialogOpening"
   fromEvent ev      =  case ev of EVPageJavascriptDialogOpening v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageLifecycleEvent where
   eventName  _ _    =  "Page.lifecycleEvent"
   fromEvent ev      =  case ev of EVPageLifecycleEvent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageBackForwardCacheNotUsed where
   eventName  _ _    =  "Page.backForwardCacheNotUsed"
   fromEvent ev      =  case ev of EVPageBackForwardCacheNotUsed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PagePrerenderAttemptCompleted where
   eventName  _ _    =  "Page.prerenderAttemptCompleted"
   fromEvent ev      =  case ev of EVPagePrerenderAttemptCompleted v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageLoadEventFired where
   eventName  _ _    =  "Page.loadEventFired"
   fromEvent ev      =  case ev of EVPageLoadEventFired v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageNavigatedWithinDocument where
   eventName  _ _    =  "Page.navigatedWithinDocument"
   fromEvent ev      =  case ev of EVPageNavigatedWithinDocument v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageScreencastFrame where
   eventName  _ _    =  "Page.screencastFrame"
   fromEvent ev      =  case ev of EVPageScreencastFrame v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageScreencastVisibilityChanged where
   eventName  _ _    =  "Page.screencastVisibilityChanged"
   fromEvent ev      =  case ev of EVPageScreencastVisibilityChanged v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageWindowOpen where
   eventName  _ _    =  "Page.windowOpen"
   fromEvent ev      =  case ev of EVPageWindowOpen v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.PageCompilationCacheProduced where
   eventName  _ _    =  "Page.compilationCacheProduced"
   fromEvent ev      =  case ev of EVPageCompilationCacheProduced v -> Just v; _ -> Nothing

instance FromEvent Event Performance.PerformanceMetrics where
   eventName  _ _    =  "Performance.metrics"
   fromEvent ev      =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing

instance FromEvent Event PerformanceTimeline.PerformanceTimelineTimelineEventAdded where
   eventName  _ _    =  "PerformanceTimeline.timelineEventAdded"
   fromEvent ev      =  case ev of EVPerformanceTimelineTimelineEventAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetworkEmulationSecurity.SecurityVisibleSecurityStateChanged where
   eventName  _ _    =  "Security.visibleSecurityStateChanged"
   fromEvent ev      =  case ev of EVSecurityVisibleSecurityStateChanged v -> Just v; _ -> Nothing

instance FromEvent Event ServiceWorker.ServiceWorkerWorkerErrorReported where
   eventName  _ _    =  "ServiceWorker.workerErrorReported"
   fromEvent ev      =  case ev of EVServiceWorkerWorkerErrorReported v -> Just v; _ -> Nothing

instance FromEvent Event ServiceWorker.ServiceWorkerWorkerRegistrationUpdated where
   eventName  _ _    =  "ServiceWorker.workerRegistrationUpdated"
   fromEvent ev      =  case ev of EVServiceWorkerWorkerRegistrationUpdated v -> Just v; _ -> Nothing

instance FromEvent Event ServiceWorker.ServiceWorkerWorkerVersionUpdated where
   eventName  _ _    =  "ServiceWorker.workerVersionUpdated"
   fromEvent ev      =  case ev of EVServiceWorkerWorkerVersionUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Storage.StorageCacheStorageContentUpdated where
   eventName  _ _    =  "Storage.cacheStorageContentUpdated"
   fromEvent ev      =  case ev of EVStorageCacheStorageContentUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Storage.StorageCacheStorageListUpdated where
   eventName  _ _    =  "Storage.cacheStorageListUpdated"
   fromEvent ev      =  case ev of EVStorageCacheStorageListUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Storage.StorageIndexedDbContentUpdated where
   eventName  _ _    =  "Storage.indexedDBContentUpdated"
   fromEvent ev      =  case ev of EVStorageIndexedDbContentUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Storage.StorageIndexedDbListUpdated where
   eventName  _ _    =  "Storage.indexedDBListUpdated"
   fromEvent ev      =  case ev of EVStorageIndexedDbListUpdated v -> Just v; _ -> Nothing

instance FromEvent Event Storage.StorageInterestGroupAccessed where
   eventName  _ _    =  "Storage.interestGroupAccessed"
   fromEvent ev      =  case ev of EVStorageInterestGroupAccessed v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetAttachedToTarget where
   eventName  _ _    =  "Target.attachedToTarget"
   fromEvent ev      =  case ev of EVTargetAttachedToTarget v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetDetachedFromTarget where
   eventName  _ _    =  "Target.detachedFromTarget"
   fromEvent ev      =  case ev of EVTargetDetachedFromTarget v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetReceivedMessageFromTarget where
   eventName  _ _    =  "Target.receivedMessageFromTarget"
   fromEvent ev      =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetTargetCreated where
   eventName  _ _    =  "Target.targetCreated"
   fromEvent ev      =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetTargetDestroyed where
   eventName  _ _    =  "Target.targetDestroyed"
   fromEvent ev      =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetTargetCrashed where
   eventName  _ _    =  "Target.targetCrashed"
   fromEvent ev      =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing

instance FromEvent Event BrowserTarget.TargetTargetInfoChanged where
   eventName  _ _    =  "Target.targetInfoChanged"
   fromEvent ev      =  case ev of EVTargetTargetInfoChanged v -> Just v; _ -> Nothing

instance FromEvent Event Tethering.TetheringAccepted where
   eventName  _ _    =  "Tethering.accepted"
   fromEvent ev      =  case ev of EVTetheringAccepted v -> Just v; _ -> Nothing

instance FromEvent Event Tracing.TracingBufferUsage where
   eventName  _ _    =  "Tracing.bufferUsage"
   fromEvent ev      =  case ev of EVTracingBufferUsage v -> Just v; _ -> Nothing

instance FromEvent Event Tracing.TracingDataCollected where
   eventName  _ _    =  "Tracing.dataCollected"
   fromEvent ev      =  case ev of EVTracingDataCollected v -> Just v; _ -> Nothing

instance FromEvent Event Tracing.TracingTracingComplete where
   eventName  _ _    =  "Tracing.tracingComplete"
   fromEvent ev      =  case ev of EVTracingTracingComplete v -> Just v; _ -> Nothing

instance FromEvent Event Fetch.FetchRequestPaused where
   eventName  _ _    =  "Fetch.requestPaused"
   fromEvent ev      =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing

instance FromEvent Event Fetch.FetchAuthRequired where
   eventName  _ _    =  "Fetch.authRequired"
   fromEvent ev      =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioContextCreated where
   eventName  _ _    =  "WebAudio.contextCreated"
   fromEvent ev      =  case ev of EVWebAudioContextCreated v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioContextWillBeDestroyed where
   eventName  _ _    =  "WebAudio.contextWillBeDestroyed"
   fromEvent ev      =  case ev of EVWebAudioContextWillBeDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioContextChanged where
   eventName  _ _    =  "WebAudio.contextChanged"
   fromEvent ev      =  case ev of EVWebAudioContextChanged v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioListenerCreated where
   eventName  _ _    =  "WebAudio.audioListenerCreated"
   fromEvent ev      =  case ev of EVWebAudioAudioListenerCreated v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioListenerWillBeDestroyed where
   eventName  _ _    =  "WebAudio.audioListenerWillBeDestroyed"
   fromEvent ev      =  case ev of EVWebAudioAudioListenerWillBeDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioNodeCreated where
   eventName  _ _    =  "WebAudio.audioNodeCreated"
   fromEvent ev      =  case ev of EVWebAudioAudioNodeCreated v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioNodeWillBeDestroyed where
   eventName  _ _    =  "WebAudio.audioNodeWillBeDestroyed"
   fromEvent ev      =  case ev of EVWebAudioAudioNodeWillBeDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioParamCreated where
   eventName  _ _    =  "WebAudio.audioParamCreated"
   fromEvent ev      =  case ev of EVWebAudioAudioParamCreated v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioAudioParamWillBeDestroyed where
   eventName  _ _    =  "WebAudio.audioParamWillBeDestroyed"
   fromEvent ev      =  case ev of EVWebAudioAudioParamWillBeDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioNodesConnected where
   eventName  _ _    =  "WebAudio.nodesConnected"
   fromEvent ev      =  case ev of EVWebAudioNodesConnected v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioNodesDisconnected where
   eventName  _ _    =  "WebAudio.nodesDisconnected"
   fromEvent ev      =  case ev of EVWebAudioNodesDisconnected v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioNodeParamConnected where
   eventName  _ _    =  "WebAudio.nodeParamConnected"
   fromEvent ev      =  case ev of EVWebAudioNodeParamConnected v -> Just v; _ -> Nothing

instance FromEvent Event WebAudio.WebAudioNodeParamDisconnected where
   eventName  _ _    =  "WebAudio.nodeParamDisconnected"
   fromEvent ev      =  case ev of EVWebAudioNodeParamDisconnected v -> Just v; _ -> Nothing

instance FromEvent Event Media.MediaPlayerPropertiesChanged where
   eventName  _ _    =  "Media.playerPropertiesChanged"
   fromEvent ev      =  case ev of EVMediaPlayerPropertiesChanged v -> Just v; _ -> Nothing

instance FromEvent Event Media.MediaPlayerEventsAdded where
   eventName  _ _    =  "Media.playerEventsAdded"
   fromEvent ev      =  case ev of EVMediaPlayerEventsAdded v -> Just v; _ -> Nothing

instance FromEvent Event Media.MediaPlayerMessagesLogged where
   eventName  _ _    =  "Media.playerMessagesLogged"
   fromEvent ev      =  case ev of EVMediaPlayerMessagesLogged v -> Just v; _ -> Nothing

instance FromEvent Event Media.MediaPlayerErrorsRaised where
   eventName  _ _    =  "Media.playerErrorsRaised"
   fromEvent ev      =  case ev of EVMediaPlayerErrorsRaised v -> Just v; _ -> Nothing

instance FromEvent Event Media.MediaPlayersCreated where
   eventName  _ _    =  "Media.playersCreated"
   fromEvent ev      =  case ev of EVMediaPlayersCreated v -> Just v; _ -> Nothing

instance FromEvent Event Debugger.DebuggerBreakpointResolved where
   eventName  _ _    =  "Debugger.breakpointResolved"
   fromEvent ev      =  case ev of EVDebuggerBreakpointResolved v -> Just v; _ -> Nothing

instance FromEvent Event Debugger.DebuggerPaused where
   eventName  _ _    =  "Debugger.paused"
   fromEvent ev      =  case ev of EVDebuggerPaused v -> Just v; _ -> Nothing

instance FromEvent Event Debugger.DebuggerResumed where
   eventName  _ _    =  "Debugger.resumed"
   fromEvent ev      =  case ev of EVDebuggerResumed v -> Just v; _ -> Nothing

instance FromEvent Event Debugger.DebuggerScriptFailedToParse where
   eventName  _ _    =  "Debugger.scriptFailedToParse"
   fromEvent ev      =  case ev of EVDebuggerScriptFailedToParse v -> Just v; _ -> Nothing

instance FromEvent Event Debugger.DebuggerScriptParsed where
   eventName  _ _    =  "Debugger.scriptParsed"
   fromEvent ev      =  case ev of EVDebuggerScriptParsed v -> Just v; _ -> Nothing

instance FromEvent Event HeapProfiler.HeapProfilerAddHeapSnapshotChunk where
   eventName  _ _    =  "HeapProfiler.addHeapSnapshotChunk"
   fromEvent ev      =  case ev of EVHeapProfilerAddHeapSnapshotChunk v -> Just v; _ -> Nothing

instance FromEvent Event HeapProfiler.HeapProfilerHeapStatsUpdate where
   eventName  _ _    =  "HeapProfiler.heapStatsUpdate"
   fromEvent ev      =  case ev of EVHeapProfilerHeapStatsUpdate v -> Just v; _ -> Nothing

instance FromEvent Event HeapProfiler.HeapProfilerLastSeenObjectId where
   eventName  _ _    =  "HeapProfiler.lastSeenObjectId"
   fromEvent ev      =  case ev of EVHeapProfilerLastSeenObjectId v -> Just v; _ -> Nothing

instance FromEvent Event HeapProfiler.HeapProfilerReportHeapSnapshotProgress where
   eventName  _ _    =  "HeapProfiler.reportHeapSnapshotProgress"
   fromEvent ev      =  case ev of EVHeapProfilerReportHeapSnapshotProgress v -> Just v; _ -> Nothing

instance FromEvent Event HeapProfiler.HeapProfilerResetProfiles where
   eventName  _ _    =  "HeapProfiler.resetProfiles"
   fromEvent ev      =  case ev of EVHeapProfilerResetProfiles v -> Just v; _ -> Nothing

instance FromEvent Event Profiler.ProfilerConsoleProfileFinished where
   eventName  _ _    =  "Profiler.consoleProfileFinished"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing

instance FromEvent Event Profiler.ProfilerConsoleProfileStarted where
   eventName  _ _    =  "Profiler.consoleProfileStarted"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileStarted v -> Just v; _ -> Nothing

instance FromEvent Event Profiler.ProfilerPreciseCoverageDeltaUpdate where
   eventName  _ _    =  "Profiler.preciseCoverageDeltaUpdate"
   fromEvent ev      =  case ev of EVProfilerPreciseCoverageDeltaUpdate v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeBindingCalled where
   eventName  _ _    =  "Runtime.bindingCalled"
   fromEvent ev      =  case ev of EVRuntimeBindingCalled v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeConsoleApiCalled where
   eventName  _ _    =  "Runtime.consoleAPICalled"
   fromEvent ev      =  case ev of EVRuntimeConsoleApiCalled v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeExceptionRevoked where
   eventName  _ _    =  "Runtime.exceptionRevoked"
   fromEvent ev      =  case ev of EVRuntimeExceptionRevoked v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeExceptionThrown where
   eventName  _ _    =  "Runtime.exceptionThrown"
   fromEvent ev      =  case ev of EVRuntimeExceptionThrown v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeExecutionContextCreated where
   eventName  _ _    =  "Runtime.executionContextCreated"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextCreated v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeExecutionContextDestroyed where
   eventName  _ _    =  "Runtime.executionContextDestroyed"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeExecutionContextsCleared where
   eventName  _ _    =  "Runtime.executionContextsCleared"
   fromEvent ev      =  case ev of EVRuntimeExecutionContextsCleared v -> Just v; _ -> Nothing

instance FromEvent Event Runtime.RuntimeInspectRequested where
   eventName  _ _    =  "Runtime.inspectRequested"
   fromEvent ev      =  case ev of EVRuntimeInspectRequested v -> Just v; _ -> Nothing



