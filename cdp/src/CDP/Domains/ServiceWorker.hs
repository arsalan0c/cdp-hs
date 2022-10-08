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


-- | Type 'ServiceWorker.RegistrationID'.
type ServiceWorkerRegistrationId = String

-- | Type 'ServiceWorker.ServiceWorkerRegistration'.
--   ServiceWorker registration.
data ServiceWorkerServiceWorkerRegistration = ServiceWorkerServiceWorkerRegistration {
  serviceWorkerServiceWorkerRegistrationRegistrationId :: ServiceWorkerRegistrationId,
  serviceWorkerServiceWorkerRegistrationScopeUrl :: String,
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
  serviceWorkerServiceWorkerVersionRegistrationId :: ServiceWorkerRegistrationId,
  serviceWorkerServiceWorkerVersionScriptUrl :: String,
  serviceWorkerServiceWorkerVersionRunningStatus :: ServiceWorkerServiceWorkerVersionRunningStatus,
  serviceWorkerServiceWorkerVersionStatus :: ServiceWorkerServiceWorkerVersionStatus,
  -- | The Last-Modified header value of the main script.
  serviceWorkerServiceWorkerVersionScriptLastModified :: Maybe Double,
  -- | The time at which the response headers of the main script were received from the server.
  --   For cached script it is the last time the cache entry was validated.
  serviceWorkerServiceWorkerVersionScriptResponseTime :: Maybe Double,
  serviceWorkerServiceWorkerVersionControlledClients :: Maybe [BrowserTarget.TargetTargetId],
  serviceWorkerServiceWorkerVersionTargetId :: Maybe BrowserTarget.TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerServiceWorkerVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerServiceWorkerVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'ServiceWorker.ServiceWorkerErrorMessage'.
--   ServiceWorker error message.
data ServiceWorkerServiceWorkerErrorMessage = ServiceWorkerServiceWorkerErrorMessage {
  serviceWorkerServiceWorkerErrorMessageErrorMessage :: String,
  serviceWorkerServiceWorkerErrorMessageRegistrationId :: ServiceWorkerRegistrationId,
  serviceWorkerServiceWorkerErrorMessageVersionId :: String,
  serviceWorkerServiceWorkerErrorMessageSourceUrl :: String,
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



-- | Type of the 'ServiceWorker.workerRegistrationUpdated' event.
data ServiceWorkerWorkerRegistrationUpdated = ServiceWorkerWorkerRegistrationUpdated {
  serviceWorkerWorkerRegistrationUpdatedRegistrations :: [ServiceWorkerServiceWorkerRegistration]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerRegistrationUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerRegistrationUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type of the 'ServiceWorker.workerVersionUpdated' event.
data ServiceWorkerWorkerVersionUpdated = ServiceWorkerWorkerVersionUpdated {
  serviceWorkerWorkerVersionUpdatedVersions :: [ServiceWorkerServiceWorkerVersion]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ServiceWorkerWorkerVersionUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  ServiceWorkerWorkerVersionUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }





-- | Parameters of the 'serviceWorkerDeliverPushMessage' command.
data PServiceWorkerDeliverPushMessage = PServiceWorkerDeliverPushMessage {
  pServiceWorkerDeliverPushMessageOrigin :: String,
  pServiceWorkerDeliverPushMessageRegistrationId :: ServiceWorkerRegistrationId,
  pServiceWorkerDeliverPushMessageData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDeliverPushMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDeliverPushMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'ServiceWorker.deliverPushMessage' command.
--   
--   Parameters: 'PServiceWorkerDeliverPushMessage'
serviceWorkerDeliverPushMessage :: Handle ev -> Maybe String -> PServiceWorkerDeliverPushMessage -> IO ()
serviceWorkerDeliverPushMessage handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.deliverPushMessage" (Just params )


-- | Function for the 'ServiceWorker.disable' command.
serviceWorkerDisable :: Handle ev -> Maybe String -> IO ()
serviceWorkerDisable handle sessionId = sendReceiveCommand handle sessionId "ServiceWorker.disable" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerDispatchSyncEvent' command.
data PServiceWorkerDispatchSyncEvent = PServiceWorkerDispatchSyncEvent {
  pServiceWorkerDispatchSyncEventOrigin :: String,
  pServiceWorkerDispatchSyncEventRegistrationId :: ServiceWorkerRegistrationId,
  pServiceWorkerDispatchSyncEventTag :: String,
  pServiceWorkerDispatchSyncEventLastChance :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'ServiceWorker.dispatchSyncEvent' command.
--   
--   Parameters: 'PServiceWorkerDispatchSyncEvent'
serviceWorkerDispatchSyncEvent :: Handle ev -> Maybe String -> PServiceWorkerDispatchSyncEvent -> IO ()
serviceWorkerDispatchSyncEvent handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.dispatchSyncEvent" (Just params )


-- | Parameters of the 'serviceWorkerDispatchPeriodicSyncEvent' command.
data PServiceWorkerDispatchPeriodicSyncEvent = PServiceWorkerDispatchPeriodicSyncEvent {
  pServiceWorkerDispatchPeriodicSyncEventOrigin :: String,
  pServiceWorkerDispatchPeriodicSyncEventRegistrationId :: ServiceWorkerRegistrationId,
  pServiceWorkerDispatchPeriodicSyncEventTag :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerDispatchPeriodicSyncEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerDispatchPeriodicSyncEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


-- | Function for the 'ServiceWorker.dispatchPeriodicSyncEvent' command.
--   
--   Parameters: 'PServiceWorkerDispatchPeriodicSyncEvent'
serviceWorkerDispatchPeriodicSyncEvent :: Handle ev -> Maybe String -> PServiceWorkerDispatchPeriodicSyncEvent -> IO ()
serviceWorkerDispatchPeriodicSyncEvent handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.dispatchPeriodicSyncEvent" (Just params )


-- | Function for the 'ServiceWorker.enable' command.
serviceWorkerEnable :: Handle ev -> Maybe String -> IO ()
serviceWorkerEnable handle sessionId = sendReceiveCommand handle sessionId "ServiceWorker.enable" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerInspectWorker' command.
data PServiceWorkerInspectWorker = PServiceWorkerInspectWorker {
  pServiceWorkerInspectWorkerVersionId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerInspectWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerInspectWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'ServiceWorker.inspectWorker' command.
--   
--   Parameters: 'PServiceWorkerInspectWorker'
serviceWorkerInspectWorker :: Handle ev -> Maybe String -> PServiceWorkerInspectWorker -> IO ()
serviceWorkerInspectWorker handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.inspectWorker" (Just params )


-- | Parameters of the 'serviceWorkerSetForceUpdateOnPageLoad' command.
data PServiceWorkerSetForceUpdateOnPageLoad = PServiceWorkerSetForceUpdateOnPageLoad {
  pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSetForceUpdateOnPageLoad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSetForceUpdateOnPageLoad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'ServiceWorker.setForceUpdateOnPageLoad' command.
--   
--   Parameters: 'PServiceWorkerSetForceUpdateOnPageLoad'
serviceWorkerSetForceUpdateOnPageLoad :: Handle ev -> Maybe String -> PServiceWorkerSetForceUpdateOnPageLoad -> IO ()
serviceWorkerSetForceUpdateOnPageLoad handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.setForceUpdateOnPageLoad" (Just params )


-- | Parameters of the 'serviceWorkerSkipWaiting' command.
data PServiceWorkerSkipWaiting = PServiceWorkerSkipWaiting {
  pServiceWorkerSkipWaitingScopeUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerSkipWaiting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerSkipWaiting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'ServiceWorker.skipWaiting' command.
--   
--   Parameters: 'PServiceWorkerSkipWaiting'
serviceWorkerSkipWaiting :: Handle ev -> Maybe String -> PServiceWorkerSkipWaiting -> IO ()
serviceWorkerSkipWaiting handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.skipWaiting" (Just params )


-- | Parameters of the 'serviceWorkerStartWorker' command.
data PServiceWorkerStartWorker = PServiceWorkerStartWorker {
  pServiceWorkerStartWorkerScopeUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStartWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStartWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'ServiceWorker.startWorker' command.
--   
--   Parameters: 'PServiceWorkerStartWorker'
serviceWorkerStartWorker :: Handle ev -> Maybe String -> PServiceWorkerStartWorker -> IO ()
serviceWorkerStartWorker handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.startWorker" (Just params )


-- | Function for the 'ServiceWorker.stopAllWorkers' command.
serviceWorkerStopAllWorkers :: Handle ev -> Maybe String -> IO ()
serviceWorkerStopAllWorkers handle sessionId = sendReceiveCommand handle sessionId "ServiceWorker.stopAllWorkers" (Nothing :: Maybe ())


-- | Parameters of the 'serviceWorkerStopWorker' command.
data PServiceWorkerStopWorker = PServiceWorkerStopWorker {
  pServiceWorkerStopWorkerVersionId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerStopWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerStopWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'ServiceWorker.stopWorker' command.
--   
--   Parameters: 'PServiceWorkerStopWorker'
serviceWorkerStopWorker :: Handle ev -> Maybe String -> PServiceWorkerStopWorker -> IO ()
serviceWorkerStopWorker handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.stopWorker" (Just params )


-- | Parameters of the 'serviceWorkerUnregister' command.
data PServiceWorkerUnregister = PServiceWorkerUnregister {
  pServiceWorkerUnregisterScopeUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUnregister  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUnregister where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'ServiceWorker.unregister' command.
--   
--   Parameters: 'PServiceWorkerUnregister'
serviceWorkerUnregister :: Handle ev -> Maybe String -> PServiceWorkerUnregister -> IO ()
serviceWorkerUnregister handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.unregister" (Just params )


-- | Parameters of the 'serviceWorkerUpdateRegistration' command.
data PServiceWorkerUpdateRegistration = PServiceWorkerUpdateRegistration {
  pServiceWorkerUpdateRegistrationScopeUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PServiceWorkerUpdateRegistration  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PServiceWorkerUpdateRegistration where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'ServiceWorker.updateRegistration' command.
--   
--   Parameters: 'PServiceWorkerUpdateRegistration'
serviceWorkerUpdateRegistration :: Handle ev -> Maybe String -> PServiceWorkerUpdateRegistration -> IO ()
serviceWorkerUpdateRegistration handle sessionId params = sendReceiveCommand handle sessionId "ServiceWorker.updateRegistration" (Just params )



