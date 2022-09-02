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

runClient  :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort app = runClient' hostPort (app . Session)


data Event = EVBrowser BrowserEvent | EVDOM DOMEvent | EVDOMDebugger DOMDebuggerEvent | EVEmulation EmulationEvent | EVIO IOEvent | EVInput InputEvent | EVLog LogEvent | EVNetwork NetworkEvent | EVPage PageEvent | EVPerformance PerformanceEvent | EVSecurity SecurityEvent | EVTarget TargetEvent | EVFetch FetchEvent | EVConsole ConsoleEvent | EVDebugger DebuggerEvent | EVProfiler ProfilerEvent | EVRuntime RuntimeEvent | EVSchema SchemaEvent
    deriving (Eq, Show, Read)
instance FromJSON (EventResponse Event ) where
    parseJSON = A.withObject  "EventResponse"  $ \obj -> do
        name <- obj .: "method"
        case (name :: String) of
                "DOM.attributeModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMAttributeModified) . fmap EVDOM . fmap EVDOMAttributeModified <$> obj .:? "params"
                "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMAttributeRemoved) . fmap EVDOM . fmap EVDOMAttributeRemoved <$> obj .:? "params"
                "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMCharacterDataModified) . fmap EVDOM . fmap EVDOMCharacterDataModified <$> obj .:? "params"
                "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMChildNodeCountUpdated) . fmap EVDOM . fmap EVDOMChildNodeCountUpdated <$> obj .:? "params"
                "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMChildNodeInserted) . fmap EVDOM . fmap EVDOMChildNodeInserted <$> obj .:? "params"
                "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMChildNodeRemoved) . fmap EVDOM . fmap EVDOMChildNodeRemoved <$> obj .:? "params"
                "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMDocumentUpdated) . fmap EVDOM . fmap EVDOMDocumentUpdated <$> obj .:? "params"
                "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDOMSetChildNodes) . fmap EVDOM . fmap EVDOMSetChildNodes <$> obj .:? "params"
                "Log.entryAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVLogEntryAdded) . fmap EVLog . fmap EVLogEntryAdded <$> obj .:? "params"
                "Network.dataReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkDataReceived) . fmap EVNetwork . fmap EVNetworkDataReceived <$> obj .:? "params"
                "Network.eventSourceMessageReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkEventSourceMessageReceived) . fmap EVNetwork . fmap EVNetworkEventSourceMessageReceived <$> obj .:? "params"
                "Network.loadingFailed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkLoadingFailed) . fmap EVNetwork . fmap EVNetworkLoadingFailed <$> obj .:? "params"
                "Network.loadingFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkLoadingFinished) . fmap EVNetwork . fmap EVNetworkLoadingFinished <$> obj .:? "params"
                "Network.requestServedFromCache" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkRequestServedFromCache) . fmap EVNetwork . fmap EVNetworkRequestServedFromCache <$> obj .:? "params"
                "Network.requestWillBeSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkRequestWillBeSent) . fmap EVNetwork . fmap EVNetworkRequestWillBeSent <$> obj .:? "params"
                "Network.responseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkResponseReceived) . fmap EVNetwork . fmap EVNetworkResponseReceived <$> obj .:? "params"
                "Network.webSocketClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketClosed) . fmap EVNetwork . fmap EVNetworkWebSocketClosed <$> obj .:? "params"
                "Network.webSocketCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketCreated) . fmap EVNetwork . fmap EVNetworkWebSocketCreated <$> obj .:? "params"
                "Network.webSocketFrameError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketFrameError) . fmap EVNetwork . fmap EVNetworkWebSocketFrameError <$> obj .:? "params"
                "Network.webSocketFrameReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketFrameReceived) . fmap EVNetwork . fmap EVNetworkWebSocketFrameReceived <$> obj .:? "params"
                "Network.webSocketFrameSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketFrameSent) . fmap EVNetwork . fmap EVNetworkWebSocketFrameSent <$> obj .:? "params"
                "Network.webSocketHandshakeResponseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketHandshakeResponseReceived) . fmap EVNetwork . fmap EVNetworkWebSocketHandshakeResponseReceived <$> obj .:? "params"
                "Network.webSocketWillSendHandshakeRequest" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebSocketWillSendHandshakeRequest) . fmap EVNetwork . fmap EVNetworkWebSocketWillSendHandshakeRequest <$> obj .:? "params"
                "Network.webTransportCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebTransportCreated) . fmap EVNetwork . fmap EVNetworkWebTransportCreated <$> obj .:? "params"
                "Network.webTransportConnectionEstablished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebTransportConnectionEstablished) . fmap EVNetwork . fmap EVNetworkWebTransportConnectionEstablished <$> obj .:? "params"
                "Network.webTransportClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVNetworkWebTransportClosed) . fmap EVNetwork . fmap EVNetworkWebTransportClosed <$> obj .:? "params"
                "Page.domContentEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageDomContentEventFired) . fmap EVPage . fmap EVPageDomContentEventFired <$> obj .:? "params"
                "Page.fileChooserOpened" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageFileChooserOpened) . fmap EVPage . fmap EVPageFileChooserOpened <$> obj .:? "params"
                "Page.frameAttached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageFrameAttached) . fmap EVPage . fmap EVPageFrameAttached <$> obj .:? "params"
                "Page.frameDetached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageFrameDetached) . fmap EVPage . fmap EVPageFrameDetached <$> obj .:? "params"
                "Page.frameNavigated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageFrameNavigated) . fmap EVPage . fmap EVPageFrameNavigated <$> obj .:? "params"
                "Page.interstitialHidden" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageInterstitialHidden) . fmap EVPage . fmap EVPageInterstitialHidden <$> obj .:? "params"
                "Page.interstitialShown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageInterstitialShown) . fmap EVPage . fmap EVPageInterstitialShown <$> obj .:? "params"
                "Page.javascriptDialogClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageJavascriptDialogClosed) . fmap EVPage . fmap EVPageJavascriptDialogClosed <$> obj .:? "params"
                "Page.javascriptDialogOpening" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageJavascriptDialogOpening) . fmap EVPage . fmap EVPageJavascriptDialogOpening <$> obj .:? "params"
                "Page.lifecycleEvent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageLifecycleEvent) . fmap EVPage . fmap EVPageLifecycleEvent <$> obj .:? "params"
                "Page.prerenderAttemptCompleted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPagePrerenderAttemptCompleted) . fmap EVPage . fmap EVPagePrerenderAttemptCompleted <$> obj .:? "params"
                "Page.loadEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageLoadEventFired) . fmap EVPage . fmap EVPageLoadEventFired <$> obj .:? "params"
                "Page.windowOpen" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPageWindowOpen) . fmap EVPage . fmap EVPageWindowOpen <$> obj .:? "params"
                "Performance.metrics" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVPerformanceMetrics) . fmap EVPerformance . fmap EVPerformanceMetrics <$> obj .:? "params"
                "Target.receivedMessageFromTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVTargetReceivedMessageFromTarget) . fmap EVTarget . fmap EVTargetReceivedMessageFromTarget <$> obj .:? "params"
                "Target.targetCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVTargetTargetCreated) . fmap EVTarget . fmap EVTargetTargetCreated <$> obj .:? "params"
                "Target.targetDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVTargetTargetDestroyed) . fmap EVTarget . fmap EVTargetTargetDestroyed <$> obj .:? "params"
                "Target.targetCrashed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVTargetTargetCrashed) . fmap EVTarget . fmap EVTargetTargetCrashed <$> obj .:? "params"
                "Target.targetInfoChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVTargetTargetInfoChanged) . fmap EVTarget . fmap EVTargetTargetInfoChanged <$> obj .:? "params"
                "Fetch.requestPaused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVFetchRequestPaused) . fmap EVFetch . fmap EVFetchRequestPaused <$> obj .:? "params"
                "Fetch.authRequired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVFetchAuthRequired) . fmap EVFetch . fmap EVFetchAuthRequired <$> obj .:? "params"
                "Console.messageAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVConsoleMessageAdded) . fmap EVConsole . fmap EVConsoleMessageAdded <$> obj .:? "params"
                "Debugger.breakpointResolved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDebuggerBreakpointResolved) . fmap EVDebugger . fmap EVDebuggerBreakpointResolved <$> obj .:? "params"
                "Debugger.paused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDebuggerPaused) . fmap EVDebugger . fmap EVDebuggerPaused <$> obj .:? "params"
                "Debugger.resumed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDebuggerResumed) . fmap EVDebugger . fmap EVDebuggerResumed <$> obj .:? "params"
                "Debugger.scriptFailedToParse" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDebuggerScriptFailedToParse) . fmap EVDebugger . fmap EVDebuggerScriptFailedToParse <$> obj .:? "params"
                "Debugger.scriptParsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVDebuggerScriptParsed) . fmap EVDebugger . fmap EVDebuggerScriptParsed <$> obj .:? "params"
                "Profiler.consoleProfileFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVProfilerConsoleProfileFinished) . fmap EVProfiler . fmap EVProfilerConsoleProfileFinished <$> obj .:? "params"
                "Profiler.consoleProfileStarted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVProfilerConsoleProfileStarted) . fmap EVProfiler . fmap EVProfilerConsoleProfileStarted <$> obj .:? "params"
                "Runtime.consoleAPICalled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeConsoleApiCalled) . fmap EVRuntime . fmap EVRuntimeConsoleApiCalled <$> obj .:? "params"
                "Runtime.exceptionRevoked" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeExceptionRevoked) . fmap EVRuntime . fmap EVRuntimeExceptionRevoked <$> obj .:? "params"
                "Runtime.exceptionThrown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeExceptionThrown) . fmap EVRuntime . fmap EVRuntimeExceptionThrown <$> obj .:? "params"
                "Runtime.executionContextCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeExecutionContextCreated) . fmap EVRuntime . fmap EVRuntimeExecutionContextCreated <$> obj .:? "params"
                "Runtime.executionContextDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeExecutionContextDestroyed) . fmap EVRuntime . fmap EVRuntimeExecutionContextDestroyed <$> obj .:? "params"
                "Runtime.executionContextsCleared" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeExecutionContextsCleared) . fmap EVRuntime . fmap EVRuntimeExecutionContextsCleared <$> obj .:? "params"
                "Runtime.inspectRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy EVRuntimeInspectRequested) . fmap EVRuntime . fmap EVRuntimeInspectRequested <$> obj .:? "params"
                _ -> fail "failed to parse EventResponseEvent"


