{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= BackgroundService

Defines events for background web platform features.
-}


module CDP.Domains.BackgroundService (module CDP.Domains.BackgroundService) where

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.ServiceWorker as ServiceWorker


-- | Type 'BackgroundService.ServiceName'.
--   The Background Service that will be associated with the commands/events.
--   Every Background Service operates independently, but they share the same
--   API.
data BackgroundServiceServiceName = BackgroundServiceServiceNameBackgroundFetch | BackgroundServiceServiceNameBackgroundSync | BackgroundServiceServiceNamePushMessaging | BackgroundServiceServiceNameNotifications | BackgroundServiceServiceNamePaymentHandler | BackgroundServiceServiceNamePeriodicBackgroundSync
  deriving (Ord, Eq, Show, Read)
instance FromJSON BackgroundServiceServiceName where
  parseJSON = A.withText "BackgroundServiceServiceName" $ \v -> case v of
    "backgroundFetch" -> pure BackgroundServiceServiceNameBackgroundFetch
    "backgroundSync" -> pure BackgroundServiceServiceNameBackgroundSync
    "pushMessaging" -> pure BackgroundServiceServiceNamePushMessaging
    "notifications" -> pure BackgroundServiceServiceNameNotifications
    "paymentHandler" -> pure BackgroundServiceServiceNamePaymentHandler
    "periodicBackgroundSync" -> pure BackgroundServiceServiceNamePeriodicBackgroundSync
    "_" -> fail "failed to parse BackgroundServiceServiceName"
instance ToJSON BackgroundServiceServiceName where
  toJSON v = A.String $ case v of
    BackgroundServiceServiceNameBackgroundFetch -> "backgroundFetch"
    BackgroundServiceServiceNameBackgroundSync -> "backgroundSync"
    BackgroundServiceServiceNamePushMessaging -> "pushMessaging"
    BackgroundServiceServiceNameNotifications -> "notifications"
    BackgroundServiceServiceNamePaymentHandler -> "paymentHandler"
    BackgroundServiceServiceNamePeriodicBackgroundSync -> "periodicBackgroundSync"

-- | Type 'BackgroundService.EventMetadata'.
--   A key-value pair for additional event information to pass along.
data BackgroundServiceEventMetadata = BackgroundServiceEventMetadata
  {
    backgroundServiceEventMetadataKey :: String,
    backgroundServiceEventMetadataValue :: String
  }
  deriving (Eq, Show)
instance FromJSON BackgroundServiceEventMetadata where
  parseJSON = A.withObject "BackgroundServiceEventMetadata" $ \o -> BackgroundServiceEventMetadata
    <$> o A..: "key"
    <*> o A..: "value"
instance ToJSON BackgroundServiceEventMetadata where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (backgroundServiceEventMetadataKey p),
    ("value" A..=) <$> Just (backgroundServiceEventMetadataValue p)
    ]

-- | Type 'BackgroundService.BackgroundServiceEvent'.
data BackgroundServiceBackgroundServiceEvent = BackgroundServiceBackgroundServiceEvent
  {
    -- | Timestamp of the event (in seconds).
    backgroundServiceBackgroundServiceEventTimestamp :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    -- | The origin this event belongs to.
    backgroundServiceBackgroundServiceEventOrigin :: String,
    -- | The Service Worker ID that initiated the event.
    backgroundServiceBackgroundServiceEventServiceWorkerRegistrationId :: ServiceWorker.ServiceWorkerRegistrationID,
    -- | The Background Service this event belongs to.
    backgroundServiceBackgroundServiceEventService :: BackgroundServiceServiceName,
    -- | A description of the event.
    backgroundServiceBackgroundServiceEventEventName :: String,
    -- | An identifier that groups related events together.
    backgroundServiceBackgroundServiceEventInstanceId :: String,
    -- | A list of event-specific information.
    backgroundServiceBackgroundServiceEventEventMetadata :: [BackgroundServiceEventMetadata]
  }
  deriving (Eq, Show)
instance FromJSON BackgroundServiceBackgroundServiceEvent where
  parseJSON = A.withObject "BackgroundServiceBackgroundServiceEvent" $ \o -> BackgroundServiceBackgroundServiceEvent
    <$> o A..: "timestamp"
    <*> o A..: "origin"
    <*> o A..: "serviceWorkerRegistrationId"
    <*> o A..: "service"
    <*> o A..: "eventName"
    <*> o A..: "instanceId"
    <*> o A..: "eventMetadata"
instance ToJSON BackgroundServiceBackgroundServiceEvent where
  toJSON p = A.object $ catMaybes [
    ("timestamp" A..=) <$> Just (backgroundServiceBackgroundServiceEventTimestamp p),
    ("origin" A..=) <$> Just (backgroundServiceBackgroundServiceEventOrigin p),
    ("serviceWorkerRegistrationId" A..=) <$> Just (backgroundServiceBackgroundServiceEventServiceWorkerRegistrationId p),
    ("service" A..=) <$> Just (backgroundServiceBackgroundServiceEventService p),
    ("eventName" A..=) <$> Just (backgroundServiceBackgroundServiceEventEventName p),
    ("instanceId" A..=) <$> Just (backgroundServiceBackgroundServiceEventInstanceId p),
    ("eventMetadata" A..=) <$> Just (backgroundServiceBackgroundServiceEventEventMetadata p)
    ]

-- | Type of the 'BackgroundService.recordingStateChanged' event.
data BackgroundServiceRecordingStateChanged = BackgroundServiceRecordingStateChanged
  {
    backgroundServiceRecordingStateChangedIsRecording :: Bool,
    backgroundServiceRecordingStateChangedService :: BackgroundServiceServiceName
  }
  deriving (Eq, Show)
instance FromJSON BackgroundServiceRecordingStateChanged where
  parseJSON = A.withObject "BackgroundServiceRecordingStateChanged" $ \o -> BackgroundServiceRecordingStateChanged
    <$> o A..: "isRecording"
    <*> o A..: "service"
instance Event BackgroundServiceRecordingStateChanged where
  eventName _ = "BackgroundService.recordingStateChanged"

-- | Type of the 'BackgroundService.backgroundServiceEventReceived' event.
data BackgroundServiceBackgroundServiceEventReceived = BackgroundServiceBackgroundServiceEventReceived
  {
    backgroundServiceBackgroundServiceEventReceivedBackgroundServiceEvent :: BackgroundServiceBackgroundServiceEvent
  }
  deriving (Eq, Show)
instance FromJSON BackgroundServiceBackgroundServiceEventReceived where
  parseJSON = A.withObject "BackgroundServiceBackgroundServiceEventReceived" $ \o -> BackgroundServiceBackgroundServiceEventReceived
    <$> o A..: "backgroundServiceEvent"
instance Event BackgroundServiceBackgroundServiceEventReceived where
  eventName _ = "BackgroundService.backgroundServiceEventReceived"

-- | Enables event updates for the service.

-- | Parameters of the 'BackgroundService.startObserving' command.
data PBackgroundServiceStartObserving = PBackgroundServiceStartObserving
  {
    pBackgroundServiceStartObservingService :: BackgroundServiceServiceName
  }
  deriving (Eq, Show)
pBackgroundServiceStartObserving
  :: BackgroundServiceServiceName
  -> PBackgroundServiceStartObserving
pBackgroundServiceStartObserving
  arg_pBackgroundServiceStartObservingService
  = PBackgroundServiceStartObserving
    arg_pBackgroundServiceStartObservingService
instance ToJSON PBackgroundServiceStartObserving where
  toJSON p = A.object $ catMaybes [
    ("service" A..=) <$> Just (pBackgroundServiceStartObservingService p)
    ]
instance Command PBackgroundServiceStartObserving where
  type CommandResponse PBackgroundServiceStartObserving = ()
  commandName _ = "BackgroundService.startObserving"
  fromJSON = const . A.Success . const ()

-- | Disables event updates for the service.

-- | Parameters of the 'BackgroundService.stopObserving' command.
data PBackgroundServiceStopObserving = PBackgroundServiceStopObserving
  {
    pBackgroundServiceStopObservingService :: BackgroundServiceServiceName
  }
  deriving (Eq, Show)
pBackgroundServiceStopObserving
  :: BackgroundServiceServiceName
  -> PBackgroundServiceStopObserving
pBackgroundServiceStopObserving
  arg_pBackgroundServiceStopObservingService
  = PBackgroundServiceStopObserving
    arg_pBackgroundServiceStopObservingService
instance ToJSON PBackgroundServiceStopObserving where
  toJSON p = A.object $ catMaybes [
    ("service" A..=) <$> Just (pBackgroundServiceStopObservingService p)
    ]
instance Command PBackgroundServiceStopObserving where
  type CommandResponse PBackgroundServiceStopObserving = ()
  commandName _ = "BackgroundService.stopObserving"
  fromJSON = const . A.Success . const ()

-- | Set the recording state for the service.

-- | Parameters of the 'BackgroundService.setRecording' command.
data PBackgroundServiceSetRecording = PBackgroundServiceSetRecording
  {
    pBackgroundServiceSetRecordingShouldRecord :: Bool,
    pBackgroundServiceSetRecordingService :: BackgroundServiceServiceName
  }
  deriving (Eq, Show)
pBackgroundServiceSetRecording
  :: Bool
  -> BackgroundServiceServiceName
  -> PBackgroundServiceSetRecording
pBackgroundServiceSetRecording
  arg_pBackgroundServiceSetRecordingShouldRecord
  arg_pBackgroundServiceSetRecordingService
  = PBackgroundServiceSetRecording
    arg_pBackgroundServiceSetRecordingShouldRecord
    arg_pBackgroundServiceSetRecordingService
instance ToJSON PBackgroundServiceSetRecording where
  toJSON p = A.object $ catMaybes [
    ("shouldRecord" A..=) <$> Just (pBackgroundServiceSetRecordingShouldRecord p),
    ("service" A..=) <$> Just (pBackgroundServiceSetRecordingService p)
    ]
instance Command PBackgroundServiceSetRecording where
  type CommandResponse PBackgroundServiceSetRecording = ()
  commandName _ = "BackgroundService.setRecording"
  fromJSON = const . A.Success . const ()

-- | Clears all stored data for the service.

-- | Parameters of the 'BackgroundService.clearEvents' command.
data PBackgroundServiceClearEvents = PBackgroundServiceClearEvents
  {
    pBackgroundServiceClearEventsService :: BackgroundServiceServiceName
  }
  deriving (Eq, Show)
pBackgroundServiceClearEvents
  :: BackgroundServiceServiceName
  -> PBackgroundServiceClearEvents
pBackgroundServiceClearEvents
  arg_pBackgroundServiceClearEventsService
  = PBackgroundServiceClearEvents
    arg_pBackgroundServiceClearEventsService
instance ToJSON PBackgroundServiceClearEvents where
  toJSON p = A.object $ catMaybes [
    ("service" A..=) <$> Just (pBackgroundServiceClearEventsService p)
    ]
instance Command PBackgroundServiceClearEvents where
  type CommandResponse PBackgroundServiceClearEvents = ()
  commandName _ = "BackgroundService.clearEvents"
  fromJSON = const . A.Success . const ()

