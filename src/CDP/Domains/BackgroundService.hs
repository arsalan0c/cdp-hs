{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  BackgroundService :
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
   parseJSON = A.withText  "BackgroundServiceServiceName"  $ \v -> do
      case v of
         "backgroundFetch" -> pure BackgroundServiceServiceNameBackgroundFetch
         "backgroundSync" -> pure BackgroundServiceServiceNameBackgroundSync
         "pushMessaging" -> pure BackgroundServiceServiceNamePushMessaging
         "notifications" -> pure BackgroundServiceServiceNameNotifications
         "paymentHandler" -> pure BackgroundServiceServiceNamePaymentHandler
         "periodicBackgroundSync" -> pure BackgroundServiceServiceNamePeriodicBackgroundSync
         _ -> fail "failed to parse BackgroundServiceServiceName"

instance ToJSON BackgroundServiceServiceName where
   toJSON v = A.String $
      case v of
         BackgroundServiceServiceNameBackgroundFetch -> "backgroundFetch"
         BackgroundServiceServiceNameBackgroundSync -> "backgroundSync"
         BackgroundServiceServiceNamePushMessaging -> "pushMessaging"
         BackgroundServiceServiceNameNotifications -> "notifications"
         BackgroundServiceServiceNamePaymentHandler -> "paymentHandler"
         BackgroundServiceServiceNamePeriodicBackgroundSync -> "periodicBackgroundSync"



-- | Type 'BackgroundService.EventMetadata'.
--   A key-value pair for additional event information to pass along.
data BackgroundServiceEventMetadata = BackgroundServiceEventMetadata {
  backgroundServiceEventMetadataKey :: String,
  backgroundServiceEventMetadataValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceEventMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceEventMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'BackgroundService.BackgroundServiceEvent'.
data BackgroundServiceBackgroundServiceEvent = BackgroundServiceBackgroundServiceEvent {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceBackgroundServiceEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceBackgroundServiceEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }





-- | Type of the 'BackgroundService.recordingStateChanged' event.
data BackgroundServiceRecordingStateChanged = BackgroundServiceRecordingStateChanged {
  backgroundServiceRecordingStateChangedIsRecording :: Bool,
  backgroundServiceRecordingStateChangedService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceRecordingStateChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceRecordingStateChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


instance Event BackgroundServiceRecordingStateChanged where
    eventName _ = "BackgroundService.recordingStateChanged"

-- | Type of the 'BackgroundService.backgroundServiceEventReceived' event.
data BackgroundServiceBackgroundServiceEventReceived = BackgroundServiceBackgroundServiceEventReceived {
  backgroundServiceBackgroundServiceEventReceivedBackgroundServiceEvent :: BackgroundServiceBackgroundServiceEvent
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceBackgroundServiceEventReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceBackgroundServiceEventReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }


instance Event BackgroundServiceBackgroundServiceEventReceived where
    eventName _ = "BackgroundService.backgroundServiceEventReceived"



-- | BackgroundService.startObserving
--   Enables event updates for the service.

-- | Parameters of the 'BackgroundService.startObserving' command.
data PBackgroundServiceStartObserving = PBackgroundServiceStartObserving {
  pBackgroundServiceStartObservingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceStartObserving  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceStartObserving where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Command PBackgroundServiceStartObserving where
   type CommandResponse PBackgroundServiceStartObserving = ()
   commandName _ = "BackgroundService.startObserving"
   fromJSON = const . A.Success . const ()


-- | BackgroundService.stopObserving
--   Disables event updates for the service.

-- | Parameters of the 'BackgroundService.stopObserving' command.
data PBackgroundServiceStopObserving = PBackgroundServiceStopObserving {
  pBackgroundServiceStopObservingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceStopObserving  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceStopObserving where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PBackgroundServiceStopObserving where
   type CommandResponse PBackgroundServiceStopObserving = ()
   commandName _ = "BackgroundService.stopObserving"
   fromJSON = const . A.Success . const ()


-- | BackgroundService.setRecording
--   Set the recording state for the service.

-- | Parameters of the 'BackgroundService.setRecording' command.
data PBackgroundServiceSetRecording = PBackgroundServiceSetRecording {
  pBackgroundServiceSetRecordingShouldRecord :: Bool,
  pBackgroundServiceSetRecordingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceSetRecording  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceSetRecording where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PBackgroundServiceSetRecording where
   type CommandResponse PBackgroundServiceSetRecording = ()
   commandName _ = "BackgroundService.setRecording"
   fromJSON = const . A.Success . const ()


-- | BackgroundService.clearEvents
--   Clears all stored data for the service.

-- | Parameters of the 'BackgroundService.clearEvents' command.
data PBackgroundServiceClearEvents = PBackgroundServiceClearEvents {
  pBackgroundServiceClearEventsService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceClearEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceClearEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PBackgroundServiceClearEvents where
   type CommandResponse PBackgroundServiceClearEvents = ()
   commandName _ = "BackgroundService.clearEvents"
   fromJSON = const . A.Success . const ()



