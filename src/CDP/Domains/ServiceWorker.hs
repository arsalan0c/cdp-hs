{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  ServiceWorker 
-}


module CDP.Domains.ServiceWorker (module CDP.Domains.ServiceWorker) where

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


import CDP.Domains.BrowserTarget as BrowserTarget


-- | Type 'ServiceWorker.RegistrationID'.
type ServiceWorkerRegistrationID = String

-- | Type 'ServiceWorker.ServiceWorkerRegistration'.
--   ServiceWorker registration.
data ServiceWorkerServiceWorkerRegistration = ServiceWorkerServiceWorkerRegistration {
  serviceWorkerServiceWorkerRegistrationRegistrationId :: ServiceWorkerRegistrationID,
  serviceWorkerServiceWorkerRegistrationScopeURL :: String,
  serviceWorkerServiceWorkerRegistrationIsDeleted :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerRegistration  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerRegistration where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'ServiceWorker.ServiceWorkerVersionRunningStatus'.
data ServiceWorkerServiceWorkerVersionRunningStatus = ServiceWorkerServiceWorkerVersionRunningStatusStopped | ServiceWorkerServiceWorkerVersionRunningStatusStarting | ServiceWorkerServiceWorkerVersionRunningStatusRunning | ServiceWorkerServiceWorkerVersionRunningStatusStopping
   deriving (Ord, Eq, Show, Read)
instance FromJSON ServiceWorkerServiceWorkerVersionRunningStatus where
   parseJSON = A.withText  "ServiceWorkerServiceWorkerVersionRunningStatus"  $ \v -> do
      case v of
         "stopped" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStopped
         "starting" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStarting
         "running" -> pure ServiceWorkerServiceWorkerVersionRunningStatusRunning
         "stopping" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStopping
         _ -> fail "failed to parse ServiceWorkerServiceWorkerVersionRunningStatus"

instance ToJSON ServiceWorkerServiceWorkerVersionRunningStatus where
   toJSON v = A.String $
      case v of
         ServiceWorkerServiceWorkerVersionRunningStatusStopped -> "stopped"
         ServiceWorkerServiceWorkerVersionRunningStatusStarting -> "starting"
         ServiceWorkerServiceWorkerVersionRunningStatusRunning -> "running"
         ServiceWorkerServiceWorkerVersionRunningStatusStopping -> "stopping"



-- | Type 'ServiceWorker.ServiceWorkerVersionStatus'.
data ServiceWorkerServiceWorkerVersionStatus = ServiceWorkerServiceWorkerVersionStatusNew | ServiceWorkerServiceWorkerVersionStatusInstalling | ServiceWorkerServiceWorkerVersionStatusInstalled | ServiceWorkerServiceWorkerVersionStatusActivating | ServiceWorkerServiceWorkerVersionStatusActivated | ServiceWorkerServiceWorkerVersionStatusRedundant
   deriving (Ord, Eq, Show, Read)
instance FromJSON ServiceWorkerServiceWorkerVersionStatus where
   parseJSON = A.withText  "ServiceWorkerServiceWorkerVersionStatus"  $ \v -> do
      case v of
         "new" -> pure ServiceWorkerServiceWorkerVersionStatusNew
         "installing" -> pure ServiceWorkerServiceWorkerVersionStatusInstalling
         "installed" -> pure ServiceWorkerServiceWorkerVersionStatusInstalled
         "activating" -> pure ServiceWorkerServiceWorkerVersionStatusActivating
         "activated" -> pure ServiceWorkerServiceWorkerVersionStatusActivated
         "redundant" -> pure ServiceWorkerServiceWorkerVersionStatusRedundant
         _ -> fail "failed to parse ServiceWorkerServiceWorkerVersionStatus"

instance ToJSON ServiceWorkerServiceWorkerVersionStatus where
   toJSON v = A.String $
      case v of
         ServiceWorkerServiceWorkerVersionStatusNew -> "new"
         ServiceWorkerServiceWorkerVersionStatusInstalling -> "installing"
         ServiceWorkerServiceWorkerVersionStatusInstalled -> "installed"
         ServiceWorkerServiceWorkerVersionStatusActivating -> "activating"
         ServiceWorkerServiceWorkerVersionStatusActivated -> "activated"
         ServiceWorkerServiceWorkerVersionStatusRedundant -> "redundant"



-- | Type 'ServiceWorker.ServiceWorkerVersion'.
--   ServiceWorker version.
data ServiceWorkerServiceWorkerVersion = ServiceWorkerServiceWorkerVersion {
  serviceWorkerServiceWorkerVersionVersionId :: String,
  serviceWorkerServiceWorkerVersionRegistrationId :: ServiceWorkerRegistrationID,
  serviceWorkerServiceWorkerVersionScriptURL :: String,
  serviceWorkerServiceWorkerVersionRunningStatus :: ServiceWorkerServiceWorkerVersionRunningStatus,
  serviceWorkerServiceWorkerVersionStatus :: ServiceWorkerServiceWorkerVersionStatus,
  -- | The Last-Modified header value of the main script.
  serviceWorkerServiceWorkerVersionScriptLastModified :: Maybe Double,
  -- | The time at which the response headers of the main script were received from the server.
  --   For cached script it is the last time the cache entry was validated.
  serviceWorkerServiceWorkerVersionScriptResponseTime :: Maybe Double,
  serviceWorkerServiceWorkerVersionControlledClients :: Maybe [BrowserTarget.TargetTargetID],
  serviceWorkerServiceWorkerVersionTargetId :: Maybe BrowserTarget.TargetTargetID
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'ServiceWorker.ServiceWorkerErrorMessage'.
--   ServiceWorker error message.
data ServiceWorkerServiceWorkerErrorMessage = ServiceWorkerServiceWorkerErrorMessage {
  serviceWorkerServiceWorkerErrorMessageErrorMessage :: String,
  serviceWorkerServiceWorkerErrorMessageRegistrationId :: ServiceWorkerRegistrationID,
  serviceWorkerServiceWorkerErrorMessageVersionId :: String,
  serviceWorkerServiceWorkerErrorMessageSourceURL :: String,
  serviceWorkerServiceWorkerErrorMessageLineNumber :: Int,
  serviceWorkerServiceWorkerErrorMessageColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerErrorMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerErrorMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }





-- | Type of the 'ServiceWorker.workerErrorReported' event.
data ServiceWorkerWorkerErrorReported = ServiceWorkerWorkerErrorReported {
  serviceWorkerWorkerErrorReportedErrorMessage :: ServiceWorkerServiceWorkerErrorMessage
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerErrorReported  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerErrorReported where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event ServiceWorkerWorkerErrorReported where
    eventName _ = "ServiceWorker.workerErrorReported"

-- | Type of the 'ServiceWorker.workerRegistrationUpdated' event.
data ServiceWorkerWorkerRegistrationUpdated = ServiceWorkerWorkerRegistrationUpdated {
  serviceWorkerWorkerRegistrationUpdatedRegistrations :: [ServiceWorkerServiceWorkerRegistration]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerRegistrationUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerRegistrationUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


instance Event ServiceWorkerWorkerRegistrationUpdated where
    eventName _ = "ServiceWorker.workerRegistrationUpdated"

-- | Type of the 'ServiceWorker.workerVersionUpdated' event.
data ServiceWorkerWorkerVersionUpdated = ServiceWorkerWorkerVersionUpdated {
  serviceWorkerWorkerVersionUpdatedVersions :: [ServiceWorkerServiceWorkerVersion]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerVersionUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerVersionUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Event ServiceWorkerWorkerVersionUpdated where
    eventName _ = "ServiceWorker.workerVersionUpdated"



-- | ServiceWorker.deliverPushMessage

-- | Parameters of the 'ServiceWorker.deliverPushMessage' command.
data PServiceWorkerDeliverPushMessage = PServiceWorkerDeliverPushMessage {
  pServiceWorkerDeliverPushMessageOrigin :: String,
  pServiceWorkerDeliverPushMessageRegistrationId :: ServiceWorkerRegistrationID,
  pServiceWorkerDeliverPushMessageData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDeliverPushMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDeliverPushMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Command PServiceWorkerDeliverPushMessage where
   type CommandResponse PServiceWorkerDeliverPushMessage = ()
   commandName _ = "ServiceWorker.deliverPushMessage"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.disable

-- | Parameters of the 'ServiceWorker.disable' command.
data PServiceWorkerDisable = PServiceWorkerDisable
instance ToJSON PServiceWorkerDisable where toJSON _ = A.Null

instance Command PServiceWorkerDisable where
   type CommandResponse PServiceWorkerDisable = ()
   commandName _ = "ServiceWorker.disable"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.dispatchSyncEvent

-- | Parameters of the 'ServiceWorker.dispatchSyncEvent' command.
data PServiceWorkerDispatchSyncEvent = PServiceWorkerDispatchSyncEvent {
  pServiceWorkerDispatchSyncEventOrigin :: String,
  pServiceWorkerDispatchSyncEventRegistrationId :: ServiceWorkerRegistrationID,
  pServiceWorkerDispatchSyncEventTag :: String,
  pServiceWorkerDispatchSyncEventLastChance :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PServiceWorkerDispatchSyncEvent where
   type CommandResponse PServiceWorkerDispatchSyncEvent = ()
   commandName _ = "ServiceWorker.dispatchSyncEvent"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.dispatchPeriodicSyncEvent

-- | Parameters of the 'ServiceWorker.dispatchPeriodicSyncEvent' command.
data PServiceWorkerDispatchPeriodicSyncEvent = PServiceWorkerDispatchPeriodicSyncEvent {
  pServiceWorkerDispatchPeriodicSyncEventOrigin :: String,
  pServiceWorkerDispatchPeriodicSyncEventRegistrationId :: ServiceWorkerRegistrationID,
  pServiceWorkerDispatchPeriodicSyncEventTag :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchPeriodicSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchPeriodicSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


instance Command PServiceWorkerDispatchPeriodicSyncEvent where
   type CommandResponse PServiceWorkerDispatchPeriodicSyncEvent = ()
   commandName _ = "ServiceWorker.dispatchPeriodicSyncEvent"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.enable

-- | Parameters of the 'ServiceWorker.enable' command.
data PServiceWorkerEnable = PServiceWorkerEnable
instance ToJSON PServiceWorkerEnable where toJSON _ = A.Null

instance Command PServiceWorkerEnable where
   type CommandResponse PServiceWorkerEnable = ()
   commandName _ = "ServiceWorker.enable"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.inspectWorker

-- | Parameters of the 'ServiceWorker.inspectWorker' command.
data PServiceWorkerInspectWorker = PServiceWorkerInspectWorker {
  pServiceWorkerInspectWorkerVersionId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerInspectWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerInspectWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PServiceWorkerInspectWorker where
   type CommandResponse PServiceWorkerInspectWorker = ()
   commandName _ = "ServiceWorker.inspectWorker"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.setForceUpdateOnPageLoad

-- | Parameters of the 'ServiceWorker.setForceUpdateOnPageLoad' command.
data PServiceWorkerSetForceUpdateOnPageLoad = PServiceWorkerSetForceUpdateOnPageLoad {
  pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSetForceUpdateOnPageLoad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSetForceUpdateOnPageLoad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


instance Command PServiceWorkerSetForceUpdateOnPageLoad where
   type CommandResponse PServiceWorkerSetForceUpdateOnPageLoad = ()
   commandName _ = "ServiceWorker.setForceUpdateOnPageLoad"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.skipWaiting

-- | Parameters of the 'ServiceWorker.skipWaiting' command.
data PServiceWorkerSkipWaiting = PServiceWorkerSkipWaiting {
  pServiceWorkerSkipWaitingScopeURL :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSkipWaiting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSkipWaiting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PServiceWorkerSkipWaiting where
   type CommandResponse PServiceWorkerSkipWaiting = ()
   commandName _ = "ServiceWorker.skipWaiting"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.startWorker

-- | Parameters of the 'ServiceWorker.startWorker' command.
data PServiceWorkerStartWorker = PServiceWorkerStartWorker {
  pServiceWorkerStartWorkerScopeURL :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStartWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStartWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PServiceWorkerStartWorker where
   type CommandResponse PServiceWorkerStartWorker = ()
   commandName _ = "ServiceWorker.startWorker"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.stopAllWorkers

-- | Parameters of the 'ServiceWorker.stopAllWorkers' command.
data PServiceWorkerStopAllWorkers = PServiceWorkerStopAllWorkers
instance ToJSON PServiceWorkerStopAllWorkers where toJSON _ = A.Null

instance Command PServiceWorkerStopAllWorkers where
   type CommandResponse PServiceWorkerStopAllWorkers = ()
   commandName _ = "ServiceWorker.stopAllWorkers"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.stopWorker

-- | Parameters of the 'ServiceWorker.stopWorker' command.
data PServiceWorkerStopWorker = PServiceWorkerStopWorker {
  pServiceWorkerStopWorkerVersionId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStopWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStopWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PServiceWorkerStopWorker where
   type CommandResponse PServiceWorkerStopWorker = ()
   commandName _ = "ServiceWorker.stopWorker"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.unregister

-- | Parameters of the 'ServiceWorker.unregister' command.
data PServiceWorkerUnregister = PServiceWorkerUnregister {
  pServiceWorkerUnregisterScopeURL :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUnregister  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUnregister where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PServiceWorkerUnregister where
   type CommandResponse PServiceWorkerUnregister = ()
   commandName _ = "ServiceWorker.unregister"
   fromJSON = const . A.Success . const ()


-- | ServiceWorker.updateRegistration

-- | Parameters of the 'ServiceWorker.updateRegistration' command.
data PServiceWorkerUpdateRegistration = PServiceWorkerUpdateRegistration {
  pServiceWorkerUpdateRegistrationScopeURL :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUpdateRegistration  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUpdateRegistration where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Command PServiceWorkerUpdateRegistration where
   type CommandResponse PServiceWorkerUpdateRegistration = ()
   commandName _ = "ServiceWorker.updateRegistration"
   fromJSON = const . A.Success . const ()



