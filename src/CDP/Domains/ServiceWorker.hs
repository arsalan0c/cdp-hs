{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= ServiceWorker

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
type ServiceWorkerRegistrationID = T.Text

-- | Type 'ServiceWorker.ServiceWorkerRegistration'.
--   ServiceWorker registration.
data ServiceWorkerServiceWorkerRegistration = ServiceWorkerServiceWorkerRegistration
  {
    serviceWorkerServiceWorkerRegistrationRegistrationId :: ServiceWorkerRegistrationID,
    serviceWorkerServiceWorkerRegistrationScopeURL :: T.Text,
    serviceWorkerServiceWorkerRegistrationIsDeleted :: Bool
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerServiceWorkerRegistration where
  parseJSON = A.withObject "ServiceWorkerServiceWorkerRegistration" $ \o -> ServiceWorkerServiceWorkerRegistration
    <$> o A..: "registrationId"
    <*> o A..: "scopeURL"
    <*> o A..: "isDeleted"
instance ToJSON ServiceWorkerServiceWorkerRegistration where
  toJSON p = A.object $ catMaybes [
    ("registrationId" A..=) <$> Just (serviceWorkerServiceWorkerRegistrationRegistrationId p),
    ("scopeURL" A..=) <$> Just (serviceWorkerServiceWorkerRegistrationScopeURL p),
    ("isDeleted" A..=) <$> Just (serviceWorkerServiceWorkerRegistrationIsDeleted p)
    ]

-- | Type 'ServiceWorker.ServiceWorkerVersionRunningStatus'.
data ServiceWorkerServiceWorkerVersionRunningStatus = ServiceWorkerServiceWorkerVersionRunningStatusStopped | ServiceWorkerServiceWorkerVersionRunningStatusStarting | ServiceWorkerServiceWorkerVersionRunningStatusRunning | ServiceWorkerServiceWorkerVersionRunningStatusStopping
  deriving (Ord, Eq, Show, Read)
instance FromJSON ServiceWorkerServiceWorkerVersionRunningStatus where
  parseJSON = A.withText "ServiceWorkerServiceWorkerVersionRunningStatus" $ \v -> case v of
    "stopped" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStopped
    "starting" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStarting
    "running" -> pure ServiceWorkerServiceWorkerVersionRunningStatusRunning
    "stopping" -> pure ServiceWorkerServiceWorkerVersionRunningStatusStopping
    "_" -> fail "failed to parse ServiceWorkerServiceWorkerVersionRunningStatus"
instance ToJSON ServiceWorkerServiceWorkerVersionRunningStatus where
  toJSON v = A.String $ case v of
    ServiceWorkerServiceWorkerVersionRunningStatusStopped -> "stopped"
    ServiceWorkerServiceWorkerVersionRunningStatusStarting -> "starting"
    ServiceWorkerServiceWorkerVersionRunningStatusRunning -> "running"
    ServiceWorkerServiceWorkerVersionRunningStatusStopping -> "stopping"

-- | Type 'ServiceWorker.ServiceWorkerVersionStatus'.
data ServiceWorkerServiceWorkerVersionStatus = ServiceWorkerServiceWorkerVersionStatusNew | ServiceWorkerServiceWorkerVersionStatusInstalling | ServiceWorkerServiceWorkerVersionStatusInstalled | ServiceWorkerServiceWorkerVersionStatusActivating | ServiceWorkerServiceWorkerVersionStatusActivated | ServiceWorkerServiceWorkerVersionStatusRedundant
  deriving (Ord, Eq, Show, Read)
instance FromJSON ServiceWorkerServiceWorkerVersionStatus where
  parseJSON = A.withText "ServiceWorkerServiceWorkerVersionStatus" $ \v -> case v of
    "new" -> pure ServiceWorkerServiceWorkerVersionStatusNew
    "installing" -> pure ServiceWorkerServiceWorkerVersionStatusInstalling
    "installed" -> pure ServiceWorkerServiceWorkerVersionStatusInstalled
    "activating" -> pure ServiceWorkerServiceWorkerVersionStatusActivating
    "activated" -> pure ServiceWorkerServiceWorkerVersionStatusActivated
    "redundant" -> pure ServiceWorkerServiceWorkerVersionStatusRedundant
    "_" -> fail "failed to parse ServiceWorkerServiceWorkerVersionStatus"
instance ToJSON ServiceWorkerServiceWorkerVersionStatus where
  toJSON v = A.String $ case v of
    ServiceWorkerServiceWorkerVersionStatusNew -> "new"
    ServiceWorkerServiceWorkerVersionStatusInstalling -> "installing"
    ServiceWorkerServiceWorkerVersionStatusInstalled -> "installed"
    ServiceWorkerServiceWorkerVersionStatusActivating -> "activating"
    ServiceWorkerServiceWorkerVersionStatusActivated -> "activated"
    ServiceWorkerServiceWorkerVersionStatusRedundant -> "redundant"

-- | Type 'ServiceWorker.ServiceWorkerVersion'.
--   ServiceWorker version.
data ServiceWorkerServiceWorkerVersion = ServiceWorkerServiceWorkerVersion
  {
    serviceWorkerServiceWorkerVersionVersionId :: T.Text,
    serviceWorkerServiceWorkerVersionRegistrationId :: ServiceWorkerRegistrationID,
    serviceWorkerServiceWorkerVersionScriptURL :: T.Text,
    serviceWorkerServiceWorkerVersionRunningStatus :: ServiceWorkerServiceWorkerVersionRunningStatus,
    serviceWorkerServiceWorkerVersionStatus :: ServiceWorkerServiceWorkerVersionStatus,
    -- | The Last-Modified header value of the main script.
    serviceWorkerServiceWorkerVersionScriptLastModified :: Maybe Double,
    -- | The time at which the response headers of the main script were received from the server.
    --   For cached script it is the last time the cache entry was validated.
    serviceWorkerServiceWorkerVersionScriptResponseTime :: Maybe Double,
    serviceWorkerServiceWorkerVersionControlledClients :: Maybe [BrowserTarget.TargetTargetID],
    serviceWorkerServiceWorkerVersionTargetId :: Maybe BrowserTarget.TargetTargetID
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerServiceWorkerVersion where
  parseJSON = A.withObject "ServiceWorkerServiceWorkerVersion" $ \o -> ServiceWorkerServiceWorkerVersion
    <$> o A..: "versionId"
    <*> o A..: "registrationId"
    <*> o A..: "scriptURL"
    <*> o A..: "runningStatus"
    <*> o A..: "status"
    <*> o A..:? "scriptLastModified"
    <*> o A..:? "scriptResponseTime"
    <*> o A..:? "controlledClients"
    <*> o A..:? "targetId"
instance ToJSON ServiceWorkerServiceWorkerVersion where
  toJSON p = A.object $ catMaybes [
    ("versionId" A..=) <$> Just (serviceWorkerServiceWorkerVersionVersionId p),
    ("registrationId" A..=) <$> Just (serviceWorkerServiceWorkerVersionRegistrationId p),
    ("scriptURL" A..=) <$> Just (serviceWorkerServiceWorkerVersionScriptURL p),
    ("runningStatus" A..=) <$> Just (serviceWorkerServiceWorkerVersionRunningStatus p),
    ("status" A..=) <$> Just (serviceWorkerServiceWorkerVersionStatus p),
    ("scriptLastModified" A..=) <$> (serviceWorkerServiceWorkerVersionScriptLastModified p),
    ("scriptResponseTime" A..=) <$> (serviceWorkerServiceWorkerVersionScriptResponseTime p),
    ("controlledClients" A..=) <$> (serviceWorkerServiceWorkerVersionControlledClients p),
    ("targetId" A..=) <$> (serviceWorkerServiceWorkerVersionTargetId p)
    ]

-- | Type 'ServiceWorker.ServiceWorkerErrorMessage'.
--   ServiceWorker error message.
data ServiceWorkerServiceWorkerErrorMessage = ServiceWorkerServiceWorkerErrorMessage
  {
    serviceWorkerServiceWorkerErrorMessageErrorMessage :: T.Text,
    serviceWorkerServiceWorkerErrorMessageRegistrationId :: ServiceWorkerRegistrationID,
    serviceWorkerServiceWorkerErrorMessageVersionId :: T.Text,
    serviceWorkerServiceWorkerErrorMessageSourceURL :: T.Text,
    serviceWorkerServiceWorkerErrorMessageLineNumber :: Int,
    serviceWorkerServiceWorkerErrorMessageColumnNumber :: Int
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerServiceWorkerErrorMessage where
  parseJSON = A.withObject "ServiceWorkerServiceWorkerErrorMessage" $ \o -> ServiceWorkerServiceWorkerErrorMessage
    <$> o A..: "errorMessage"
    <*> o A..: "registrationId"
    <*> o A..: "versionId"
    <*> o A..: "sourceURL"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
instance ToJSON ServiceWorkerServiceWorkerErrorMessage where
  toJSON p = A.object $ catMaybes [
    ("errorMessage" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageErrorMessage p),
    ("registrationId" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageRegistrationId p),
    ("versionId" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageVersionId p),
    ("sourceURL" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageSourceURL p),
    ("lineNumber" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageLineNumber p),
    ("columnNumber" A..=) <$> Just (serviceWorkerServiceWorkerErrorMessageColumnNumber p)
    ]

-- | Type of the 'ServiceWorker.workerErrorReported' event.
data ServiceWorkerWorkerErrorReported = ServiceWorkerWorkerErrorReported
  {
    serviceWorkerWorkerErrorReportedErrorMessage :: ServiceWorkerServiceWorkerErrorMessage
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerWorkerErrorReported where
  parseJSON = A.withObject "ServiceWorkerWorkerErrorReported" $ \o -> ServiceWorkerWorkerErrorReported
    <$> o A..: "errorMessage"
instance Event ServiceWorkerWorkerErrorReported where
  eventName _ = "ServiceWorker.workerErrorReported"

-- | Type of the 'ServiceWorker.workerRegistrationUpdated' event.
data ServiceWorkerWorkerRegistrationUpdated = ServiceWorkerWorkerRegistrationUpdated
  {
    serviceWorkerWorkerRegistrationUpdatedRegistrations :: [ServiceWorkerServiceWorkerRegistration]
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerWorkerRegistrationUpdated where
  parseJSON = A.withObject "ServiceWorkerWorkerRegistrationUpdated" $ \o -> ServiceWorkerWorkerRegistrationUpdated
    <$> o A..: "registrations"
instance Event ServiceWorkerWorkerRegistrationUpdated where
  eventName _ = "ServiceWorker.workerRegistrationUpdated"

-- | Type of the 'ServiceWorker.workerVersionUpdated' event.
data ServiceWorkerWorkerVersionUpdated = ServiceWorkerWorkerVersionUpdated
  {
    serviceWorkerWorkerVersionUpdatedVersions :: [ServiceWorkerServiceWorkerVersion]
  }
  deriving (Eq, Show)
instance FromJSON ServiceWorkerWorkerVersionUpdated where
  parseJSON = A.withObject "ServiceWorkerWorkerVersionUpdated" $ \o -> ServiceWorkerWorkerVersionUpdated
    <$> o A..: "versions"
instance Event ServiceWorkerWorkerVersionUpdated where
  eventName _ = "ServiceWorker.workerVersionUpdated"


-- | Parameters of the 'ServiceWorker.deliverPushMessage' command.
data PServiceWorkerDeliverPushMessage = PServiceWorkerDeliverPushMessage
  {
    pServiceWorkerDeliverPushMessageOrigin :: T.Text,
    pServiceWorkerDeliverPushMessageRegistrationId :: ServiceWorkerRegistrationID,
    pServiceWorkerDeliverPushMessageData :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerDeliverPushMessage
  :: T.Text
  -> ServiceWorkerRegistrationID
  -> T.Text
  -> PServiceWorkerDeliverPushMessage
pServiceWorkerDeliverPushMessage
  arg_pServiceWorkerDeliverPushMessageOrigin
  arg_pServiceWorkerDeliverPushMessageRegistrationId
  arg_pServiceWorkerDeliverPushMessageData
  = PServiceWorkerDeliverPushMessage
    arg_pServiceWorkerDeliverPushMessageOrigin
    arg_pServiceWorkerDeliverPushMessageRegistrationId
    arg_pServiceWorkerDeliverPushMessageData
instance ToJSON PServiceWorkerDeliverPushMessage where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pServiceWorkerDeliverPushMessageOrigin p),
    ("registrationId" A..=) <$> Just (pServiceWorkerDeliverPushMessageRegistrationId p),
    ("data" A..=) <$> Just (pServiceWorkerDeliverPushMessageData p)
    ]
instance Command PServiceWorkerDeliverPushMessage where
  type CommandResponse PServiceWorkerDeliverPushMessage = ()
  commandName _ = "ServiceWorker.deliverPushMessage"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.disable' command.
data PServiceWorkerDisable = PServiceWorkerDisable
  deriving (Eq, Show)
pServiceWorkerDisable
  :: PServiceWorkerDisable
pServiceWorkerDisable
  = PServiceWorkerDisable
instance ToJSON PServiceWorkerDisable where
  toJSON _ = A.Null
instance Command PServiceWorkerDisable where
  type CommandResponse PServiceWorkerDisable = ()
  commandName _ = "ServiceWorker.disable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.dispatchSyncEvent' command.
data PServiceWorkerDispatchSyncEvent = PServiceWorkerDispatchSyncEvent
  {
    pServiceWorkerDispatchSyncEventOrigin :: T.Text,
    pServiceWorkerDispatchSyncEventRegistrationId :: ServiceWorkerRegistrationID,
    pServiceWorkerDispatchSyncEventTag :: T.Text,
    pServiceWorkerDispatchSyncEventLastChance :: Bool
  }
  deriving (Eq, Show)
pServiceWorkerDispatchSyncEvent
  :: T.Text
  -> ServiceWorkerRegistrationID
  -> T.Text
  -> Bool
  -> PServiceWorkerDispatchSyncEvent
pServiceWorkerDispatchSyncEvent
  arg_pServiceWorkerDispatchSyncEventOrigin
  arg_pServiceWorkerDispatchSyncEventRegistrationId
  arg_pServiceWorkerDispatchSyncEventTag
  arg_pServiceWorkerDispatchSyncEventLastChance
  = PServiceWorkerDispatchSyncEvent
    arg_pServiceWorkerDispatchSyncEventOrigin
    arg_pServiceWorkerDispatchSyncEventRegistrationId
    arg_pServiceWorkerDispatchSyncEventTag
    arg_pServiceWorkerDispatchSyncEventLastChance
instance ToJSON PServiceWorkerDispatchSyncEvent where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pServiceWorkerDispatchSyncEventOrigin p),
    ("registrationId" A..=) <$> Just (pServiceWorkerDispatchSyncEventRegistrationId p),
    ("tag" A..=) <$> Just (pServiceWorkerDispatchSyncEventTag p),
    ("lastChance" A..=) <$> Just (pServiceWorkerDispatchSyncEventLastChance p)
    ]
instance Command PServiceWorkerDispatchSyncEvent where
  type CommandResponse PServiceWorkerDispatchSyncEvent = ()
  commandName _ = "ServiceWorker.dispatchSyncEvent"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.dispatchPeriodicSyncEvent' command.
data PServiceWorkerDispatchPeriodicSyncEvent = PServiceWorkerDispatchPeriodicSyncEvent
  {
    pServiceWorkerDispatchPeriodicSyncEventOrigin :: T.Text,
    pServiceWorkerDispatchPeriodicSyncEventRegistrationId :: ServiceWorkerRegistrationID,
    pServiceWorkerDispatchPeriodicSyncEventTag :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerDispatchPeriodicSyncEvent
  :: T.Text
  -> ServiceWorkerRegistrationID
  -> T.Text
  -> PServiceWorkerDispatchPeriodicSyncEvent
pServiceWorkerDispatchPeriodicSyncEvent
  arg_pServiceWorkerDispatchPeriodicSyncEventOrigin
  arg_pServiceWorkerDispatchPeriodicSyncEventRegistrationId
  arg_pServiceWorkerDispatchPeriodicSyncEventTag
  = PServiceWorkerDispatchPeriodicSyncEvent
    arg_pServiceWorkerDispatchPeriodicSyncEventOrigin
    arg_pServiceWorkerDispatchPeriodicSyncEventRegistrationId
    arg_pServiceWorkerDispatchPeriodicSyncEventTag
instance ToJSON PServiceWorkerDispatchPeriodicSyncEvent where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pServiceWorkerDispatchPeriodicSyncEventOrigin p),
    ("registrationId" A..=) <$> Just (pServiceWorkerDispatchPeriodicSyncEventRegistrationId p),
    ("tag" A..=) <$> Just (pServiceWorkerDispatchPeriodicSyncEventTag p)
    ]
instance Command PServiceWorkerDispatchPeriodicSyncEvent where
  type CommandResponse PServiceWorkerDispatchPeriodicSyncEvent = ()
  commandName _ = "ServiceWorker.dispatchPeriodicSyncEvent"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.enable' command.
data PServiceWorkerEnable = PServiceWorkerEnable
  deriving (Eq, Show)
pServiceWorkerEnable
  :: PServiceWorkerEnable
pServiceWorkerEnable
  = PServiceWorkerEnable
instance ToJSON PServiceWorkerEnable where
  toJSON _ = A.Null
instance Command PServiceWorkerEnable where
  type CommandResponse PServiceWorkerEnable = ()
  commandName _ = "ServiceWorker.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.inspectWorker' command.
data PServiceWorkerInspectWorker = PServiceWorkerInspectWorker
  {
    pServiceWorkerInspectWorkerVersionId :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerInspectWorker
  :: T.Text
  -> PServiceWorkerInspectWorker
pServiceWorkerInspectWorker
  arg_pServiceWorkerInspectWorkerVersionId
  = PServiceWorkerInspectWorker
    arg_pServiceWorkerInspectWorkerVersionId
instance ToJSON PServiceWorkerInspectWorker where
  toJSON p = A.object $ catMaybes [
    ("versionId" A..=) <$> Just (pServiceWorkerInspectWorkerVersionId p)
    ]
instance Command PServiceWorkerInspectWorker where
  type CommandResponse PServiceWorkerInspectWorker = ()
  commandName _ = "ServiceWorker.inspectWorker"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.setForceUpdateOnPageLoad' command.
data PServiceWorkerSetForceUpdateOnPageLoad = PServiceWorkerSetForceUpdateOnPageLoad
  {
    pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad :: Bool
  }
  deriving (Eq, Show)
pServiceWorkerSetForceUpdateOnPageLoad
  :: Bool
  -> PServiceWorkerSetForceUpdateOnPageLoad
pServiceWorkerSetForceUpdateOnPageLoad
  arg_pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad
  = PServiceWorkerSetForceUpdateOnPageLoad
    arg_pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad
instance ToJSON PServiceWorkerSetForceUpdateOnPageLoad where
  toJSON p = A.object $ catMaybes [
    ("forceUpdateOnPageLoad" A..=) <$> Just (pServiceWorkerSetForceUpdateOnPageLoadForceUpdateOnPageLoad p)
    ]
instance Command PServiceWorkerSetForceUpdateOnPageLoad where
  type CommandResponse PServiceWorkerSetForceUpdateOnPageLoad = ()
  commandName _ = "ServiceWorker.setForceUpdateOnPageLoad"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.skipWaiting' command.
data PServiceWorkerSkipWaiting = PServiceWorkerSkipWaiting
  {
    pServiceWorkerSkipWaitingScopeURL :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerSkipWaiting
  :: T.Text
  -> PServiceWorkerSkipWaiting
pServiceWorkerSkipWaiting
  arg_pServiceWorkerSkipWaitingScopeURL
  = PServiceWorkerSkipWaiting
    arg_pServiceWorkerSkipWaitingScopeURL
instance ToJSON PServiceWorkerSkipWaiting where
  toJSON p = A.object $ catMaybes [
    ("scopeURL" A..=) <$> Just (pServiceWorkerSkipWaitingScopeURL p)
    ]
instance Command PServiceWorkerSkipWaiting where
  type CommandResponse PServiceWorkerSkipWaiting = ()
  commandName _ = "ServiceWorker.skipWaiting"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.startWorker' command.
data PServiceWorkerStartWorker = PServiceWorkerStartWorker
  {
    pServiceWorkerStartWorkerScopeURL :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerStartWorker
  :: T.Text
  -> PServiceWorkerStartWorker
pServiceWorkerStartWorker
  arg_pServiceWorkerStartWorkerScopeURL
  = PServiceWorkerStartWorker
    arg_pServiceWorkerStartWorkerScopeURL
instance ToJSON PServiceWorkerStartWorker where
  toJSON p = A.object $ catMaybes [
    ("scopeURL" A..=) <$> Just (pServiceWorkerStartWorkerScopeURL p)
    ]
instance Command PServiceWorkerStartWorker where
  type CommandResponse PServiceWorkerStartWorker = ()
  commandName _ = "ServiceWorker.startWorker"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.stopAllWorkers' command.
data PServiceWorkerStopAllWorkers = PServiceWorkerStopAllWorkers
  deriving (Eq, Show)
pServiceWorkerStopAllWorkers
  :: PServiceWorkerStopAllWorkers
pServiceWorkerStopAllWorkers
  = PServiceWorkerStopAllWorkers
instance ToJSON PServiceWorkerStopAllWorkers where
  toJSON _ = A.Null
instance Command PServiceWorkerStopAllWorkers where
  type CommandResponse PServiceWorkerStopAllWorkers = ()
  commandName _ = "ServiceWorker.stopAllWorkers"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.stopWorker' command.
data PServiceWorkerStopWorker = PServiceWorkerStopWorker
  {
    pServiceWorkerStopWorkerVersionId :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerStopWorker
  :: T.Text
  -> PServiceWorkerStopWorker
pServiceWorkerStopWorker
  arg_pServiceWorkerStopWorkerVersionId
  = PServiceWorkerStopWorker
    arg_pServiceWorkerStopWorkerVersionId
instance ToJSON PServiceWorkerStopWorker where
  toJSON p = A.object $ catMaybes [
    ("versionId" A..=) <$> Just (pServiceWorkerStopWorkerVersionId p)
    ]
instance Command PServiceWorkerStopWorker where
  type CommandResponse PServiceWorkerStopWorker = ()
  commandName _ = "ServiceWorker.stopWorker"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.unregister' command.
data PServiceWorkerUnregister = PServiceWorkerUnregister
  {
    pServiceWorkerUnregisterScopeURL :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerUnregister
  :: T.Text
  -> PServiceWorkerUnregister
pServiceWorkerUnregister
  arg_pServiceWorkerUnregisterScopeURL
  = PServiceWorkerUnregister
    arg_pServiceWorkerUnregisterScopeURL
instance ToJSON PServiceWorkerUnregister where
  toJSON p = A.object $ catMaybes [
    ("scopeURL" A..=) <$> Just (pServiceWorkerUnregisterScopeURL p)
    ]
instance Command PServiceWorkerUnregister where
  type CommandResponse PServiceWorkerUnregister = ()
  commandName _ = "ServiceWorker.unregister"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'ServiceWorker.updateRegistration' command.
data PServiceWorkerUpdateRegistration = PServiceWorkerUpdateRegistration
  {
    pServiceWorkerUpdateRegistrationScopeURL :: T.Text
  }
  deriving (Eq, Show)
pServiceWorkerUpdateRegistration
  :: T.Text
  -> PServiceWorkerUpdateRegistration
pServiceWorkerUpdateRegistration
  arg_pServiceWorkerUpdateRegistrationScopeURL
  = PServiceWorkerUpdateRegistration
    arg_pServiceWorkerUpdateRegistrationScopeURL
instance ToJSON PServiceWorkerUpdateRegistration where
  toJSON p = A.object $ catMaybes [
    ("scopeURL" A..=) <$> Just (pServiceWorkerUpdateRegistrationScopeURL p)
    ]
instance Command PServiceWorkerUpdateRegistration where
  type CommandResponse PServiceWorkerUpdateRegistration = ()
  commandName _ = "ServiceWorker.updateRegistration"
  fromJSON = const . A.Success . const ()

