{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  Log :
     Provides access to log entries.

-}


module CDP.Domains.Log (module CDP.Domains.Log) where

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

import CDP.Internal.Runtime
import CDP.Handle


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'Log.LogEntry'.
--   Log entry.
data LogLogEntrySource = LogLogEntrySourceXml | LogLogEntrySourceJavascript | LogLogEntrySourceNetwork | LogLogEntrySourceStorage | LogLogEntrySourceAppcache | LogLogEntrySourceRendering | LogLogEntrySourceSecurity | LogLogEntrySourceDeprecation | LogLogEntrySourceWorker | LogLogEntrySourceViolation | LogLogEntrySourceIntervention | LogLogEntrySourceRecommendation | LogLogEntrySourceOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntrySource where
   parseJSON = A.withText  "LogLogEntrySource"  $ \v -> do
      case v of
         "xml" -> pure LogLogEntrySourceXml
         "javascript" -> pure LogLogEntrySourceJavascript
         "network" -> pure LogLogEntrySourceNetwork
         "storage" -> pure LogLogEntrySourceStorage
         "appcache" -> pure LogLogEntrySourceAppcache
         "rendering" -> pure LogLogEntrySourceRendering
         "security" -> pure LogLogEntrySourceSecurity
         "deprecation" -> pure LogLogEntrySourceDeprecation
         "worker" -> pure LogLogEntrySourceWorker
         "violation" -> pure LogLogEntrySourceViolation
         "intervention" -> pure LogLogEntrySourceIntervention
         "recommendation" -> pure LogLogEntrySourceRecommendation
         "other" -> pure LogLogEntrySourceOther
         _ -> fail "failed to parse LogLogEntrySource"

instance ToJSON LogLogEntrySource where
   toJSON v = A.String $
      case v of
         LogLogEntrySourceXml -> "xml"
         LogLogEntrySourceJavascript -> "javascript"
         LogLogEntrySourceNetwork -> "network"
         LogLogEntrySourceStorage -> "storage"
         LogLogEntrySourceAppcache -> "appcache"
         LogLogEntrySourceRendering -> "rendering"
         LogLogEntrySourceSecurity -> "security"
         LogLogEntrySourceDeprecation -> "deprecation"
         LogLogEntrySourceWorker -> "worker"
         LogLogEntrySourceViolation -> "violation"
         LogLogEntrySourceIntervention -> "intervention"
         LogLogEntrySourceRecommendation -> "recommendation"
         LogLogEntrySourceOther -> "other"


data LogLogEntryLevel = LogLogEntryLevelVerbose | LogLogEntryLevelInfo | LogLogEntryLevelWarning | LogLogEntryLevelError
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntryLevel where
   parseJSON = A.withText  "LogLogEntryLevel"  $ \v -> do
      case v of
         "verbose" -> pure LogLogEntryLevelVerbose
         "info" -> pure LogLogEntryLevelInfo
         "warning" -> pure LogLogEntryLevelWarning
         "error" -> pure LogLogEntryLevelError
         _ -> fail "failed to parse LogLogEntryLevel"

instance ToJSON LogLogEntryLevel where
   toJSON v = A.String $
      case v of
         LogLogEntryLevelVerbose -> "verbose"
         LogLogEntryLevelInfo -> "info"
         LogLogEntryLevelWarning -> "warning"
         LogLogEntryLevelError -> "error"


data LogLogEntryCategory = LogLogEntryCategoryCors
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntryCategory where
   parseJSON = A.withText  "LogLogEntryCategory"  $ \v -> do
      case v of
         "cors" -> pure LogLogEntryCategoryCors
         _ -> fail "failed to parse LogLogEntryCategory"

instance ToJSON LogLogEntryCategory where
   toJSON v = A.String $
      case v of
         LogLogEntryCategoryCors -> "cors"



data LogLogEntry = LogLogEntry {
  -- | Log entry source.
  logLogEntrySource :: LogLogEntrySource,
  -- | Log entry severity.
  logLogEntryLevel :: LogLogEntryLevel,
  -- | Logged text.
  logLogEntryText :: String,
  logLogEntryCategory :: LogLogEntryCategory,
  -- | Timestamp when this entry was added.
  logLogEntryTimestamp :: Runtime.RuntimeTimestamp,
  -- | URL of the resource if known.
  logLogEntryUrl :: Maybe String,
  -- | Line number in the resource.
  logLogEntryLineNumber :: Maybe Int,
  -- | JavaScript stack trace.
  logLogEntryStackTrace :: Maybe Runtime.RuntimeStackTrace,
  -- | Identifier of the network request associated with this entry.
  logLogEntryNetworkRequestId :: Maybe DOMPageNetworkEmulationSecurity.NetworkRequestId,
  -- | Identifier of the worker associated with this entry.
  logLogEntryWorkerId :: Maybe String,
  -- | Call arguments.
  logLogEntryArgs :: Maybe [Runtime.RuntimeRemoteObject]
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogLogEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  LogLogEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'Log.ViolationSetting'.
--   Violation configuration setting.
data LogViolationSettingName = LogViolationSettingNameLongTask | LogViolationSettingNameLongLayout | LogViolationSettingNameBlockedEvent | LogViolationSettingNameBlockedParser | LogViolationSettingNameDiscouragedAPIUse | LogViolationSettingNameHandler | LogViolationSettingNameRecurringHandler
   deriving (Ord, Eq, Show, Read)
instance FromJSON LogViolationSettingName where
   parseJSON = A.withText  "LogViolationSettingName"  $ \v -> do
      case v of
         "longTask" -> pure LogViolationSettingNameLongTask
         "longLayout" -> pure LogViolationSettingNameLongLayout
         "blockedEvent" -> pure LogViolationSettingNameBlockedEvent
         "blockedParser" -> pure LogViolationSettingNameBlockedParser
         "discouragedAPIUse" -> pure LogViolationSettingNameDiscouragedAPIUse
         "handler" -> pure LogViolationSettingNameHandler
         "recurringHandler" -> pure LogViolationSettingNameRecurringHandler
         _ -> fail "failed to parse LogViolationSettingName"

instance ToJSON LogViolationSettingName where
   toJSON v = A.String $
      case v of
         LogViolationSettingNameLongTask -> "longTask"
         LogViolationSettingNameLongLayout -> "longLayout"
         LogViolationSettingNameBlockedEvent -> "blockedEvent"
         LogViolationSettingNameBlockedParser -> "blockedParser"
         LogViolationSettingNameDiscouragedAPIUse -> "discouragedAPIUse"
         LogViolationSettingNameHandler -> "handler"
         LogViolationSettingNameRecurringHandler -> "recurringHandler"



data LogViolationSetting = LogViolationSetting {
  -- | Violation type.
  logViolationSettingName :: LogViolationSettingName,
  -- | Time threshold to trigger upon.
  logViolationSettingThreshold :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogViolationSetting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  LogViolationSetting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Type of the 'Log.entryAdded' event.
data LogEntryAdded = LogEntryAdded {
  -- | The entry.
  logEntryAddedEntry :: LogLogEntry
} deriving (Generic, Eq, Show, Read)
instance ToJSON LogEntryAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  LogEntryAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


instance Event LogEntryAdded where
    eventName _ = "Log.entryAdded"



-- | Function for the 'Log.clear' command.
--   Clears the log.
logClear :: Handle ev -> IO ()
logClear handle = sendReceiveCommand handle "Log.clear" (Nothing :: Maybe ())


-- | Function for the 'Log.disable' command.
--   Disables log domain, prevents further log entries from being reported to the client.
logDisable :: Handle ev -> IO ()
logDisable handle = sendReceiveCommand handle "Log.disable" (Nothing :: Maybe ())


-- | Function for the 'Log.enable' command.
--   Enables log domain, sends the entries collected so far to the client by means of the
--   `entryAdded` notification.
logEnable :: Handle ev -> IO ()
logEnable handle = sendReceiveCommand handle "Log.enable" (Nothing :: Maybe ())


-- | Parameters of the 'logStartViolationsReport' command.
data PLogStartViolationsReport = PLogStartViolationsReport {
  -- | Configuration for violations.
  pLogStartViolationsReportConfig :: [LogViolationSetting]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLogStartViolationsReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLogStartViolationsReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Log.startViolationsReport' command.
--   start violation reporting.
--   Parameters: 'PLogStartViolationsReport'
logStartViolationsReport :: Handle ev -> PLogStartViolationsReport -> IO ()
logStartViolationsReport handle params = sendReceiveCommand handle "Log.startViolationsReport" (Just params)


-- | Function for the 'Log.stopViolationsReport' command.
--   Stop violation reporting.
logStopViolationsReport :: Handle ev -> IO ()
logStopViolationsReport handle = sendReceiveCommand handle "Log.stopViolationsReport" (Nothing :: Maybe ())



