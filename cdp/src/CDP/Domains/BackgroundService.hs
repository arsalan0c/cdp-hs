{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.ServiceWorker as ServiceWorker


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



data BackgroundServiceEventMetadata = BackgroundServiceEventMetadata {
   backgroundServiceEventMetadataKey :: String,
   backgroundServiceEventMetadataValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceEventMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceEventMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data BackgroundServiceBackgroundServiceEvent = BackgroundServiceBackgroundServiceEvent {
   backgroundServiceBackgroundServiceEventTimestamp :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
   backgroundServiceBackgroundServiceEventOrigin :: String,
   backgroundServiceBackgroundServiceEventServiceWorkerRegistrationId :: ServiceWorker.ServiceWorkerRegistrationId,
   backgroundServiceBackgroundServiceEventService :: BackgroundServiceServiceName,
   backgroundServiceBackgroundServiceEventEventName :: String,
   backgroundServiceBackgroundServiceEventInstanceId :: String,
   backgroundServiceBackgroundServiceEventEventMetadata :: [BackgroundServiceEventMetadata]
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceBackgroundServiceEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceBackgroundServiceEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }





data BackgroundServiceRecordingStateChanged = BackgroundServiceRecordingStateChanged {
   backgroundServiceRecordingStateChangedIsRecording :: Bool,
   backgroundServiceRecordingStateChangedService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceRecordingStateChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceRecordingStateChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



data BackgroundServiceBackgroundServiceEventReceived = BackgroundServiceBackgroundServiceEventReceived {
   backgroundServiceBackgroundServiceEventReceivedBackgroundServiceEvent :: BackgroundServiceBackgroundServiceEvent
} deriving (Generic, Eq, Show, Read)
instance ToJSON BackgroundServiceBackgroundServiceEventReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  BackgroundServiceBackgroundServiceEventReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }





data PBackgroundServiceStartObserving = PBackgroundServiceStartObserving {
   pBackgroundServiceStartObservingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceStartObserving  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceStartObserving where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


backgroundServiceStartObserving :: Handle ev -> PBackgroundServiceStartObserving -> IO (Maybe Error)
backgroundServiceStartObserving handle params = sendReceiveCommand handle "BackgroundService.startObserving" (Just params)



data PBackgroundServiceStopObserving = PBackgroundServiceStopObserving {
   pBackgroundServiceStopObservingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceStopObserving  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceStopObserving where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


backgroundServiceStopObserving :: Handle ev -> PBackgroundServiceStopObserving -> IO (Maybe Error)
backgroundServiceStopObserving handle params = sendReceiveCommand handle "BackgroundService.stopObserving" (Just params)



data PBackgroundServiceSetRecording = PBackgroundServiceSetRecording {
   pBackgroundServiceSetRecordingShouldRecord :: Bool,
   pBackgroundServiceSetRecordingService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceSetRecording  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceSetRecording where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


backgroundServiceSetRecording :: Handle ev -> PBackgroundServiceSetRecording -> IO (Maybe Error)
backgroundServiceSetRecording handle params = sendReceiveCommand handle "BackgroundService.setRecording" (Just params)



data PBackgroundServiceClearEvents = PBackgroundServiceClearEvents {
   pBackgroundServiceClearEventsService :: BackgroundServiceServiceName
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBackgroundServiceClearEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PBackgroundServiceClearEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


backgroundServiceClearEvents :: Handle ev -> PBackgroundServiceClearEvents -> IO (Maybe Error)
backgroundServiceClearEvents handle params = sendReceiveCommand handle "BackgroundService.clearEvents" (Just params)



