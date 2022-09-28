{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.BrowserTarget as BrowserTarget


-- | Type 'ServiceWorker.RegistrationID' .
type ServiceWorkerRegistrationId = String

-- | ServiceWorker registration.
data ServiceWorkerServiceWorkerRegistration = ServiceWorkerServiceWorkerRegistration {



} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerRegistration  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerRegistration where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'ServiceWorker.ServiceWorkerVersionRunningStatus' .
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



-- | Type 'ServiceWorker.ServiceWorkerVersionStatus' .
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



-- | ServiceWorker version.
data ServiceWorkerServiceWorkerVersion = ServiceWorkerServiceWorkerVersion {





   serviceWorkerServiceWorkerVersionScriptLastModified :: ServiceWorkerServiceWorkerVersionScriptLastModified, -- ^ The Last-Modified header value of the main script.
   serviceWorkerServiceWorkerVersionScriptResponseTime :: ServiceWorkerServiceWorkerVersionScriptResponseTime, -- ^ The time at which the response headers of the main script were received from the server.
For cached script it is the last time the cache entry was validated.


} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | ServiceWorker error message.
data ServiceWorkerServiceWorkerErrorMessage = ServiceWorkerServiceWorkerErrorMessage {






} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerErrorMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerErrorMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }





-- | Type of the 'ServiceWorker.workerErrorReported' event.
data ServiceWorkerWorkerErrorReported = ServiceWorkerWorkerErrorReported {
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerErrorReported  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerErrorReported where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type of the 'ServiceWorker.workerRegistrationUpdated' event.
data ServiceWorkerWorkerRegistrationUpdated = ServiceWorkerWorkerRegistrationUpdated {
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerRegistrationUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerRegistrationUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type of the 'ServiceWorker.workerVersionUpdated' event.
data ServiceWorkerWorkerVersionUpdated = ServiceWorkerWorkerVersionUpdated {
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerVersionUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerVersionUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }





-- | Parameters of the 'serviceWorkerDeliverPushMessage' command.
data PServiceWorkerDeliverPushMessage = PServiceWorkerDeliverPushMessage {



} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDeliverPushMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDeliverPushMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'ServiceWorker.deliverPushMessage'.
-- Parameters: 'PServiceWorkerDeliverPushMessage'
serviceWorkerDeliverPushMessage :: Handle ev -> PServiceWorkerDeliverPushMessage -> IO (Maybe Error)
serviceWorkerDeliverPushMessage handle params = sendReceiveCommand handle "ServiceWorker.deliverPushMessage" (Just params)


-- | Function for the command 'ServiceWorker.disable'.
serviceWorkerDisable :: Handle ev -> IO (Maybe Error)
serviceWorkerDisable handle = sendReceiveCommand handle "ServiceWorker.disable" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerDispatchSyncEvent' command.
data PServiceWorkerDispatchSyncEvent = PServiceWorkerDispatchSyncEvent {




} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'ServiceWorker.dispatchSyncEvent'.
-- Parameters: 'PServiceWorkerDispatchSyncEvent'
serviceWorkerDispatchSyncEvent :: Handle ev -> PServiceWorkerDispatchSyncEvent -> IO (Maybe Error)
serviceWorkerDispatchSyncEvent handle params = sendReceiveCommand handle "ServiceWorker.dispatchSyncEvent" (Just params)


-- | Parameters of the 'serviceWorkerDispatchPeriodicSyncEvent' command.
data PServiceWorkerDispatchPeriodicSyncEvent = PServiceWorkerDispatchPeriodicSyncEvent {



} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchPeriodicSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchPeriodicSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


-- | Function for the command 'ServiceWorker.dispatchPeriodicSyncEvent'.
-- Parameters: 'PServiceWorkerDispatchPeriodicSyncEvent'
serviceWorkerDispatchPeriodicSyncEvent :: Handle ev -> PServiceWorkerDispatchPeriodicSyncEvent -> IO (Maybe Error)
serviceWorkerDispatchPeriodicSyncEvent handle params = sendReceiveCommand handle "ServiceWorker.dispatchPeriodicSyncEvent" (Just params)


-- | Function for the command 'ServiceWorker.enable'.
serviceWorkerEnable :: Handle ev -> IO (Maybe Error)
serviceWorkerEnable handle = sendReceiveCommand handle "ServiceWorker.enable" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerInspectWorker' command.
data PServiceWorkerInspectWorker = PServiceWorkerInspectWorker {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerInspectWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerInspectWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'ServiceWorker.inspectWorker'.
-- Parameters: 'PServiceWorkerInspectWorker'
serviceWorkerInspectWorker :: Handle ev -> PServiceWorkerInspectWorker -> IO (Maybe Error)
serviceWorkerInspectWorker handle params = sendReceiveCommand handle "ServiceWorker.inspectWorker" (Just params)


-- | Parameters of the 'serviceWorkerSetForceUpdateOnPageLoad' command.
data PServiceWorkerSetForceUpdateOnPageLoad = PServiceWorkerSetForceUpdateOnPageLoad {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSetForceUpdateOnPageLoad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSetForceUpdateOnPageLoad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the command 'ServiceWorker.setForceUpdateOnPageLoad'.
-- Parameters: 'PServiceWorkerSetForceUpdateOnPageLoad'
serviceWorkerSetForceUpdateOnPageLoad :: Handle ev -> PServiceWorkerSetForceUpdateOnPageLoad -> IO (Maybe Error)
serviceWorkerSetForceUpdateOnPageLoad handle params = sendReceiveCommand handle "ServiceWorker.setForceUpdateOnPageLoad" (Just params)


-- | Parameters of the 'serviceWorkerSkipWaiting' command.
data PServiceWorkerSkipWaiting = PServiceWorkerSkipWaiting {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSkipWaiting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSkipWaiting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'ServiceWorker.skipWaiting'.
-- Parameters: 'PServiceWorkerSkipWaiting'
serviceWorkerSkipWaiting :: Handle ev -> PServiceWorkerSkipWaiting -> IO (Maybe Error)
serviceWorkerSkipWaiting handle params = sendReceiveCommand handle "ServiceWorker.skipWaiting" (Just params)


-- | Parameters of the 'serviceWorkerStartWorker' command.
data PServiceWorkerStartWorker = PServiceWorkerStartWorker {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStartWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStartWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'ServiceWorker.startWorker'.
-- Parameters: 'PServiceWorkerStartWorker'
serviceWorkerStartWorker :: Handle ev -> PServiceWorkerStartWorker -> IO (Maybe Error)
serviceWorkerStartWorker handle params = sendReceiveCommand handle "ServiceWorker.startWorker" (Just params)


-- | Function for the command 'ServiceWorker.stopAllWorkers'.
serviceWorkerStopAllWorkers :: Handle ev -> IO (Maybe Error)
serviceWorkerStopAllWorkers handle = sendReceiveCommand handle "ServiceWorker.stopAllWorkers" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerStopWorker' command.
data PServiceWorkerStopWorker = PServiceWorkerStopWorker {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStopWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStopWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'ServiceWorker.stopWorker'.
-- Parameters: 'PServiceWorkerStopWorker'
serviceWorkerStopWorker :: Handle ev -> PServiceWorkerStopWorker -> IO (Maybe Error)
serviceWorkerStopWorker handle params = sendReceiveCommand handle "ServiceWorker.stopWorker" (Just params)


-- | Parameters of the 'serviceWorkerUnregister' command.
data PServiceWorkerUnregister = PServiceWorkerUnregister {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUnregister  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUnregister where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'ServiceWorker.unregister'.
-- Parameters: 'PServiceWorkerUnregister'
serviceWorkerUnregister :: Handle ev -> PServiceWorkerUnregister -> IO (Maybe Error)
serviceWorkerUnregister handle params = sendReceiveCommand handle "ServiceWorker.unregister" (Just params)


-- | Parameters of the 'serviceWorkerUpdateRegistration' command.
data PServiceWorkerUpdateRegistration = PServiceWorkerUpdateRegistration {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUpdateRegistration  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUpdateRegistration where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'ServiceWorker.updateRegistration'.
-- Parameters: 'PServiceWorkerUpdateRegistration'
serviceWorkerUpdateRegistration :: Handle ev -> PServiceWorkerUpdateRegistration -> IO (Maybe Error)
serviceWorkerUpdateRegistration handle params = sendReceiveCommand handle "ServiceWorker.updateRegistration" (Just params)



