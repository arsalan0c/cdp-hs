{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP
( Handle
, Config(..)
, module CDP
, module CDP.Domains.Browser
, module CDP.Domains.DOMDebugger
, module CDP.Domains.DOMPageNetwork
, module CDP.Domains.Debugger
, module CDP.Domains.Emulation
, module CDP.Domains.Fetch
, module CDP.Domains.IO
, module CDP.Domains.Input
, module CDP.Domains.Log
, module CDP.Domains.Performance
, module CDP.Domains.Profiler
, module CDP.Domains.Runtime
, module CDP.Domains.Security
, module CDP.Domains.Target
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
import CDP.Domains.Browser
import CDP.Domains.Browser as Browser
import CDP.Domains.DOMDebugger
import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetwork
import CDP.Domains.DOMPageNetwork as DOMPageNetwork
import CDP.Domains.Debugger
import CDP.Domains.Debugger as Debugger
import CDP.Domains.Emulation
import CDP.Domains.Emulation as Emulation
import CDP.Domains.Fetch
import CDP.Domains.Fetch as Fetch
import CDP.Domains.IO
import CDP.Domains.IO as IO
import CDP.Domains.Input
import CDP.Domains.Input as Input
import CDP.Domains.Log
import CDP.Domains.Log as Log
import CDP.Domains.Performance
import CDP.Domains.Performance as Performance
import CDP.Domains.Profiler
import CDP.Domains.Profiler as Profiler
import CDP.Domains.Runtime
import CDP.Domains.Runtime as Runtime
import CDP.Domains.Security
import CDP.Domains.Security as Security
import CDP.Domains.Target
import CDP.Domains.Target as Target


type ClientApp b = Handle Event -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config (app . Handle)

subscribe   :: forall a. FromEvent Event a => Handle Event -> (a -> IO ()) -> IO ()
subscribe (Handle handle) eventHandler = subscribe' handle eventHandler

unsubscribe :: forall a. FromEvent Event a => Handle Event -> Proxy a -> IO ()
unsubscribe (Handle handle) p = unsubscribe' handle p

data Event = EVDomAttributeModified DOMPageNetwork.DomAttributeModified | EVDomAttributeRemoved DOMPageNetwork.DomAttributeRemoved | EVDomCharacterDataModified DOMPageNetwork.DomCharacterDataModified | EVDomChildNodeCountUpdated DOMPageNetwork.DomChildNodeCountUpdated | EVDomChildNodeInserted DOMPageNetwork.DomChildNodeInserted | EVDomChildNodeRemoved DOMPageNetwork.DomChildNodeRemoved | EVDomDocumentUpdated DOMPageNetwork.DomDocumentUpdated | EVDomSetChildNodes DOMPageNetwork.DomSetChildNodes | EVLogEntryAdded Log.LogEntryAdded | EVNetworkDataReceived DOMPageNetwork.NetworkDataReceived | EVNetworkEventSourceMessageReceived DOMPageNetwork.NetworkEventSourceMessageReceived | EVNetworkLoadingFailed DOMPageNetwork.NetworkLoadingFailed | EVNetworkLoadingFinished DOMPageNetwork.NetworkLoadingFinished | EVNetworkRequestServedFromCache DOMPageNetwork.NetworkRequestServedFromCache | EVNetworkRequestWillBeSent DOMPageNetwork.NetworkRequestWillBeSent | EVNetworkResponseReceived DOMPageNetwork.NetworkResponseReceived | EVNetworkWebSocketClosed DOMPageNetwork.NetworkWebSocketClosed | EVNetworkWebSocketCreated DOMPageNetwork.NetworkWebSocketCreated | EVNetworkWebSocketFrameError DOMPageNetwork.NetworkWebSocketFrameError | EVNetworkWebSocketFrameReceived DOMPageNetwork.NetworkWebSocketFrameReceived | EVNetworkWebSocketFrameSent DOMPageNetwork.NetworkWebSocketFrameSent | EVNetworkWebSocketHandshakeResponseReceived DOMPageNetwork.NetworkWebSocketHandshakeResponseReceived | EVNetworkWebSocketWillSendHandshakeRequest DOMPageNetwork.NetworkWebSocketWillSendHandshakeRequest | EVNetworkWebTransportCreated DOMPageNetwork.NetworkWebTransportCreated | EVNetworkWebTransportConnectionEstablished DOMPageNetwork.NetworkWebTransportConnectionEstablished | EVNetworkWebTransportClosed DOMPageNetwork.NetworkWebTransportClosed | EVPageDomContentEventFired DOMPageNetwork.PageDomContentEventFired | EVPageFileChooserOpened DOMPageNetwork.PageFileChooserOpened | EVPageFrameAttached DOMPageNetwork.PageFrameAttached | EVPageFrameDetached DOMPageNetwork.PageFrameDetached | EVPageFrameNavigated DOMPageNetwork.PageFrameNavigated | EVPageInterstitialHidden DOMPageNetwork.PageInterstitialHidden | EVPageInterstitialShown DOMPageNetwork.PageInterstitialShown | EVPageJavascriptDialogClosed DOMPageNetwork.PageJavascriptDialogClosed | EVPageJavascriptDialogOpening DOMPageNetwork.PageJavascriptDialogOpening | EVPageLifecycleEvent DOMPageNetwork.PageLifecycleEvent | EVPagePrerenderAttemptCompleted DOMPageNetwork.PagePrerenderAttemptCompleted | EVPageLoadEventFired DOMPageNetwork.PageLoadEventFired | EVPageWindowOpen DOMPageNetwork.PageWindowOpen | EVPerformanceMetrics Performance.PerformanceMetrics | EVTargetReceivedMessageFromTarget Target.TargetReceivedMessageFromTarget | EVTargetTargetCreated Target.TargetTargetCreated | EVTargetTargetDestroyed Target.TargetTargetDestroyed | EVTargetTargetCrashed Target.TargetTargetCrashed | EVTargetTargetInfoChanged Target.TargetTargetInfoChanged | EVFetchRequestPaused Fetch.FetchRequestPaused | EVFetchAuthRequired Fetch.FetchAuthRequired | EVDebuggerBreakpointResolved Debugger.DebuggerBreakpointResolved | EVDebuggerPaused Debugger.DebuggerPaused | EVDebuggerResumed Debugger.DebuggerResumed | EVDebuggerScriptFailedToParse Debugger.DebuggerScriptFailedToParse | EVDebuggerScriptParsed Debugger.DebuggerScriptParsed | EVProfilerConsoleProfileFinished Profiler.ProfilerConsoleProfileFinished | EVProfilerConsoleProfileStarted Profiler.ProfilerConsoleProfileStarted | EVRuntimeConsoleApiCalled Runtime.RuntimeConsoleApiCalled | EVRuntimeExceptionRevoked Runtime.RuntimeExceptionRevoked | EVRuntimeExceptionThrown Runtime.RuntimeExceptionThrown | EVRuntimeExecutionContextCreated Runtime.RuntimeExecutionContextCreated | EVRuntimeExecutionContextDestroyed Runtime.RuntimeExecutionContextDestroyed | EVRuntimeExecutionContextsCleared Runtime.RuntimeExecutionContextsCleared | EVRuntimeInspectRequested Runtime.RuntimeInspectRequested
  deriving (Eq, Show, Read)
instance FromJSON (EventResponse Event ) where
   parseJSON = A.withObject  "EventResponse"  $ \obj -> do
       name <- obj .: "method"
       case (name :: String) of
           "DOM.attributeModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomAttributeModified) . fmap EVDomAttributeModified <$> obj .:? "params"
           "DOM.attributeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomAttributeRemoved) . fmap EVDomAttributeRemoved <$> obj .:? "params"
           "DOM.characterDataModified" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomCharacterDataModified) . fmap EVDomCharacterDataModified <$> obj .:? "params"
           "DOM.childNodeCountUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomChildNodeCountUpdated) . fmap EVDomChildNodeCountUpdated <$> obj .:? "params"
           "DOM.childNodeInserted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomChildNodeInserted) . fmap EVDomChildNodeInserted <$> obj .:? "params"
           "DOM.childNodeRemoved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomChildNodeRemoved) . fmap EVDomChildNodeRemoved <$> obj .:? "params"
           "DOM.documentUpdated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomDocumentUpdated) . fmap EVDomDocumentUpdated <$> obj .:? "params"
           "DOM.setChildNodes" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.DomSetChildNodes) . fmap EVDomSetChildNodes <$> obj .:? "params"
           "Log.entryAdded" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Log.LogEntryAdded) . fmap EVLogEntryAdded <$> obj .:? "params"
           "Network.dataReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkDataReceived) . fmap EVNetworkDataReceived <$> obj .:? "params"
           "Network.eventSourceMessageReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkEventSourceMessageReceived) . fmap EVNetworkEventSourceMessageReceived <$> obj .:? "params"
           "Network.loadingFailed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkLoadingFailed) . fmap EVNetworkLoadingFailed <$> obj .:? "params"
           "Network.loadingFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkLoadingFinished) . fmap EVNetworkLoadingFinished <$> obj .:? "params"
           "Network.requestServedFromCache" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkRequestServedFromCache) . fmap EVNetworkRequestServedFromCache <$> obj .:? "params"
           "Network.requestWillBeSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkRequestWillBeSent) . fmap EVNetworkRequestWillBeSent <$> obj .:? "params"
           "Network.responseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkResponseReceived) . fmap EVNetworkResponseReceived <$> obj .:? "params"
           "Network.webSocketClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketClosed) . fmap EVNetworkWebSocketClosed <$> obj .:? "params"
           "Network.webSocketCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketCreated) . fmap EVNetworkWebSocketCreated <$> obj .:? "params"
           "Network.webSocketFrameError" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketFrameError) . fmap EVNetworkWebSocketFrameError <$> obj .:? "params"
           "Network.webSocketFrameReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketFrameReceived) . fmap EVNetworkWebSocketFrameReceived <$> obj .:? "params"
           "Network.webSocketFrameSent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketFrameSent) . fmap EVNetworkWebSocketFrameSent <$> obj .:? "params"
           "Network.webSocketHandshakeResponseReceived" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketHandshakeResponseReceived) . fmap EVNetworkWebSocketHandshakeResponseReceived <$> obj .:? "params"
           "Network.webSocketWillSendHandshakeRequest" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebSocketWillSendHandshakeRequest) . fmap EVNetworkWebSocketWillSendHandshakeRequest <$> obj .:? "params"
           "Network.webTransportCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebTransportCreated) . fmap EVNetworkWebTransportCreated <$> obj .:? "params"
           "Network.webTransportConnectionEstablished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebTransportConnectionEstablished) . fmap EVNetworkWebTransportConnectionEstablished <$> obj .:? "params"
           "Network.webTransportClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.NetworkWebTransportClosed) . fmap EVNetworkWebTransportClosed <$> obj .:? "params"
           "Page.domContentEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageDomContentEventFired) . fmap EVPageDomContentEventFired <$> obj .:? "params"
           "Page.fileChooserOpened" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageFileChooserOpened) . fmap EVPageFileChooserOpened <$> obj .:? "params"
           "Page.frameAttached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageFrameAttached) . fmap EVPageFrameAttached <$> obj .:? "params"
           "Page.frameDetached" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageFrameDetached) . fmap EVPageFrameDetached <$> obj .:? "params"
           "Page.frameNavigated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageFrameNavigated) . fmap EVPageFrameNavigated <$> obj .:? "params"
           "Page.interstitialHidden" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageInterstitialHidden) . fmap EVPageInterstitialHidden <$> obj .:? "params"
           "Page.interstitialShown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageInterstitialShown) . fmap EVPageInterstitialShown <$> obj .:? "params"
           "Page.javascriptDialogClosed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageJavascriptDialogClosed) . fmap EVPageJavascriptDialogClosed <$> obj .:? "params"
           "Page.javascriptDialogOpening" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageJavascriptDialogOpening) . fmap EVPageJavascriptDialogOpening <$> obj .:? "params"
           "Page.lifecycleEvent" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageLifecycleEvent) . fmap EVPageLifecycleEvent <$> obj .:? "params"
           "Page.prerenderAttemptCompleted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PagePrerenderAttemptCompleted) . fmap EVPagePrerenderAttemptCompleted <$> obj .:? "params"
           "Page.loadEventFired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageLoadEventFired) . fmap EVPageLoadEventFired <$> obj .:? "params"
           "Page.windowOpen" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy DOMPageNetwork.PageWindowOpen) . fmap EVPageWindowOpen <$> obj .:? "params"
           "Performance.metrics" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Performance.PerformanceMetrics) . fmap EVPerformanceMetrics <$> obj .:? "params"
           "Target.receivedMessageFromTarget" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Target.TargetReceivedMessageFromTarget) . fmap EVTargetReceivedMessageFromTarget <$> obj .:? "params"
           "Target.targetCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Target.TargetTargetCreated) . fmap EVTargetTargetCreated <$> obj .:? "params"
           "Target.targetDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Target.TargetTargetDestroyed) . fmap EVTargetTargetDestroyed <$> obj .:? "params"
           "Target.targetCrashed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Target.TargetTargetCrashed) . fmap EVTargetTargetCrashed <$> obj .:? "params"
           "Target.targetInfoChanged" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Target.TargetTargetInfoChanged) . fmap EVTargetTargetInfoChanged <$> obj .:? "params"
           "Fetch.requestPaused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Fetch.FetchRequestPaused) . fmap EVFetchRequestPaused <$> obj .:? "params"
           "Fetch.authRequired" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Fetch.FetchAuthRequired) . fmap EVFetchAuthRequired <$> obj .:? "params"
           "Debugger.breakpointResolved" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerBreakpointResolved) . fmap EVDebuggerBreakpointResolved <$> obj .:? "params"
           "Debugger.paused" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerPaused) . fmap EVDebuggerPaused <$> obj .:? "params"
           "Debugger.resumed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerResumed) . fmap EVDebuggerResumed <$> obj .:? "params"
           "Debugger.scriptFailedToParse" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerScriptFailedToParse) . fmap EVDebuggerScriptFailedToParse <$> obj .:? "params"
           "Debugger.scriptParsed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Debugger.DebuggerScriptParsed) . fmap EVDebuggerScriptParsed <$> obj .:? "params"
           "Profiler.consoleProfileFinished" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Profiler.ProfilerConsoleProfileFinished) . fmap EVProfilerConsoleProfileFinished <$> obj .:? "params"
           "Profiler.consoleProfileStarted" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Profiler.ProfilerConsoleProfileStarted) . fmap EVProfilerConsoleProfileStarted <$> obj .:? "params"
           "Runtime.consoleAPICalled" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeConsoleApiCalled) . fmap EVRuntimeConsoleApiCalled <$> obj .:? "params"
           "Runtime.exceptionRevoked" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExceptionRevoked) . fmap EVRuntimeExceptionRevoked <$> obj .:? "params"
           "Runtime.exceptionThrown" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExceptionThrown) . fmap EVRuntimeExceptionThrown <$> obj .:? "params"
           "Runtime.executionContextCreated" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextCreated) . fmap EVRuntimeExecutionContextCreated <$> obj .:? "params"
           "Runtime.executionContextDestroyed" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextDestroyed) . fmap EVRuntimeExecutionContextDestroyed <$> obj .:? "params"
           "Runtime.executionContextsCleared" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeExecutionContextsCleared) . fmap EVRuntimeExecutionContextsCleared <$> obj .:? "params"
           "Runtime.inspectRequested" -> EventResponse (Proxy :: Proxy Event) (Proxy :: Proxy Runtime.RuntimeInspectRequested) . fmap EVRuntimeInspectRequested <$> obj .:? "params"
           _ -> fail "failed to parse EventResponse"

instance FromEvent Event DOMPageNetwork.DomAttributeModified where
   eventName  _ _    =  "DOM.attributeModified"
   fromEvent ev      =  case ev of EVDomAttributeModified v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomAttributeRemoved where
   eventName  _ _    =  "DOM.attributeRemoved"
   fromEvent ev      =  case ev of EVDomAttributeRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomCharacterDataModified where
   eventName  _ _    =  "DOM.characterDataModified"
   fromEvent ev      =  case ev of EVDomCharacterDataModified v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomChildNodeCountUpdated where
   eventName  _ _    =  "DOM.childNodeCountUpdated"
   fromEvent ev      =  case ev of EVDomChildNodeCountUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomChildNodeInserted where
   eventName  _ _    =  "DOM.childNodeInserted"
   fromEvent ev      =  case ev of EVDomChildNodeInserted v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomChildNodeRemoved where
   eventName  _ _    =  "DOM.childNodeRemoved"
   fromEvent ev      =  case ev of EVDomChildNodeRemoved v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomDocumentUpdated where
   eventName  _ _    =  "DOM.documentUpdated"
   fromEvent ev      =  case ev of EVDomDocumentUpdated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.DomSetChildNodes where
   eventName  _ _    =  "DOM.setChildNodes"
   fromEvent ev      =  case ev of EVDomSetChildNodes v -> Just v; _ -> Nothing

instance FromEvent Event Log.LogEntryAdded where
   eventName  _ _    =  "Log.entryAdded"
   fromEvent ev      =  case ev of EVLogEntryAdded v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkDataReceived where
   eventName  _ _    =  "Network.dataReceived"
   fromEvent ev      =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkEventSourceMessageReceived where
   eventName  _ _    =  "Network.eventSourceMessageReceived"
   fromEvent ev      =  case ev of EVNetworkEventSourceMessageReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkLoadingFailed where
   eventName  _ _    =  "Network.loadingFailed"
   fromEvent ev      =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkLoadingFinished where
   eventName  _ _    =  "Network.loadingFinished"
   fromEvent ev      =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkRequestServedFromCache where
   eventName  _ _    =  "Network.requestServedFromCache"
   fromEvent ev      =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkRequestWillBeSent where
   eventName  _ _    =  "Network.requestWillBeSent"
   fromEvent ev      =  case ev of EVNetworkRequestWillBeSent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkResponseReceived where
   eventName  _ _    =  "Network.responseReceived"
   fromEvent ev      =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketClosed where
   eventName  _ _    =  "Network.webSocketClosed"
   fromEvent ev      =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketCreated where
   eventName  _ _    =  "Network.webSocketCreated"
   fromEvent ev      =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketFrameError where
   eventName  _ _    =  "Network.webSocketFrameError"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketFrameReceived where
   eventName  _ _    =  "Network.webSocketFrameReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketFrameSent where
   eventName  _ _    =  "Network.webSocketFrameSent"
   fromEvent ev      =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketHandshakeResponseReceived where
   eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
   fromEvent ev      =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebSocketWillSendHandshakeRequest where
   eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
   fromEvent ev      =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebTransportCreated where
   eventName  _ _    =  "Network.webTransportCreated"
   fromEvent ev      =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebTransportConnectionEstablished where
   eventName  _ _    =  "Network.webTransportConnectionEstablished"
   fromEvent ev      =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.NetworkWebTransportClosed where
   eventName  _ _    =  "Network.webTransportClosed"
   fromEvent ev      =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageDomContentEventFired where
   eventName  _ _    =  "Page.domContentEventFired"
   fromEvent ev      =  case ev of EVPageDomContentEventFired v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageFileChooserOpened where
   eventName  _ _    =  "Page.fileChooserOpened"
   fromEvent ev      =  case ev of EVPageFileChooserOpened v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageFrameAttached where
   eventName  _ _    =  "Page.frameAttached"
   fromEvent ev      =  case ev of EVPageFrameAttached v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageFrameDetached where
   eventName  _ _    =  "Page.frameDetached"
   fromEvent ev      =  case ev of EVPageFrameDetached v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageFrameNavigated where
   eventName  _ _    =  "Page.frameNavigated"
   fromEvent ev      =  case ev of EVPageFrameNavigated v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageInterstitialHidden where
   eventName  _ _    =  "Page.interstitialHidden"
   fromEvent ev      =  case ev of EVPageInterstitialHidden v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageInterstitialShown where
   eventName  _ _    =  "Page.interstitialShown"
   fromEvent ev      =  case ev of EVPageInterstitialShown v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageJavascriptDialogClosed where
   eventName  _ _    =  "Page.javascriptDialogClosed"
   fromEvent ev      =  case ev of EVPageJavascriptDialogClosed v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageJavascriptDialogOpening where
   eventName  _ _    =  "Page.javascriptDialogOpening"
   fromEvent ev      =  case ev of EVPageJavascriptDialogOpening v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageLifecycleEvent where
   eventName  _ _    =  "Page.lifecycleEvent"
   fromEvent ev      =  case ev of EVPageLifecycleEvent v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PagePrerenderAttemptCompleted where
   eventName  _ _    =  "Page.prerenderAttemptCompleted"
   fromEvent ev      =  case ev of EVPagePrerenderAttemptCompleted v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageLoadEventFired where
   eventName  _ _    =  "Page.loadEventFired"
   fromEvent ev      =  case ev of EVPageLoadEventFired v -> Just v; _ -> Nothing

instance FromEvent Event DOMPageNetwork.PageWindowOpen where
   eventName  _ _    =  "Page.windowOpen"
   fromEvent ev      =  case ev of EVPageWindowOpen v -> Just v; _ -> Nothing

instance FromEvent Event Performance.PerformanceMetrics where
   eventName  _ _    =  "Performance.metrics"
   fromEvent ev      =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing

instance FromEvent Event Target.TargetReceivedMessageFromTarget where
   eventName  _ _    =  "Target.receivedMessageFromTarget"
   fromEvent ev      =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing

instance FromEvent Event Target.TargetTargetCreated where
   eventName  _ _    =  "Target.targetCreated"
   fromEvent ev      =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing

instance FromEvent Event Target.TargetTargetDestroyed where
   eventName  _ _    =  "Target.targetDestroyed"
   fromEvent ev      =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing

instance FromEvent Event Target.TargetTargetCrashed where
   eventName  _ _    =  "Target.targetCrashed"
   fromEvent ev      =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing

instance FromEvent Event Target.TargetTargetInfoChanged where
   eventName  _ _    =  "Target.targetInfoChanged"
   fromEvent ev      =  case ev of EVTargetTargetInfoChanged v -> Just v; _ -> Nothing

instance FromEvent Event Fetch.FetchRequestPaused where
   eventName  _ _    =  "Fetch.requestPaused"
   fromEvent ev      =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing

instance FromEvent Event Fetch.FetchAuthRequired where
   eventName  _ _    =  "Fetch.authRequired"
   fromEvent ev      =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing

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

instance FromEvent Event Profiler.ProfilerConsoleProfileFinished where
   eventName  _ _    =  "Profiler.consoleProfileFinished"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing

instance FromEvent Event Profiler.ProfilerConsoleProfileStarted where
   eventName  _ _    =  "Profiler.consoleProfileStarted"
   fromEvent ev      =  case ev of EVProfilerConsoleProfileStarted v -> Just v; _ -> Nothing

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



