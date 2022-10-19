{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Log

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

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'Log.LogEntry'.
--   Log entry.
data LogLogEntrySource = LogLogEntrySourceXml | LogLogEntrySourceJavascript | LogLogEntrySourceNetwork | LogLogEntrySourceStorage | LogLogEntrySourceAppcache | LogLogEntrySourceRendering | LogLogEntrySourceSecurity | LogLogEntrySourceDeprecation | LogLogEntrySourceWorker | LogLogEntrySourceViolation | LogLogEntrySourceIntervention | LogLogEntrySourceRecommendation | LogLogEntrySourceOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntrySource where
  parseJSON = A.withText "LogLogEntrySource" $ \v -> case v of
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
    "_" -> fail "failed to parse LogLogEntrySource"
instance ToJSON LogLogEntrySource where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "LogLogEntryLevel" $ \v -> case v of
    "verbose" -> pure LogLogEntryLevelVerbose
    "info" -> pure LogLogEntryLevelInfo
    "warning" -> pure LogLogEntryLevelWarning
    "error" -> pure LogLogEntryLevelError
    "_" -> fail "failed to parse LogLogEntryLevel"
instance ToJSON LogLogEntryLevel where
  toJSON v = A.String $ case v of
    LogLogEntryLevelVerbose -> "verbose"
    LogLogEntryLevelInfo -> "info"
    LogLogEntryLevelWarning -> "warning"
    LogLogEntryLevelError -> "error"
data LogLogEntryCategory = LogLogEntryCategoryCors
  deriving (Ord, Eq, Show, Read)
instance FromJSON LogLogEntryCategory where
  parseJSON = A.withText "LogLogEntryCategory" $ \v -> case v of
    "cors" -> pure LogLogEntryCategoryCors
    "_" -> fail "failed to parse LogLogEntryCategory"
instance ToJSON LogLogEntryCategory where
  toJSON v = A.String $ case v of
    LogLogEntryCategoryCors -> "cors"
data LogLogEntry = LogLogEntry
  {
    -- | Log entry source.
    logLogEntrySource :: LogLogEntrySource,
    -- | Log entry severity.
    logLogEntryLevel :: LogLogEntryLevel,
    -- | Logged text.
    logLogEntryText :: T.Text,
    logLogEntryCategory :: Maybe LogLogEntryCategory,
    -- | Timestamp when this entry was added.
    logLogEntryTimestamp :: Runtime.RuntimeTimestamp,
    -- | URL of the resource if known.
    logLogEntryUrl :: Maybe T.Text,
    -- | Line number in the resource.
    logLogEntryLineNumber :: Maybe Int,
    -- | JavaScript stack trace.
    logLogEntryStackTrace :: Maybe Runtime.RuntimeStackTrace,
    -- | Identifier of the network request associated with this entry.
    logLogEntryNetworkRequestId :: Maybe DOMPageNetworkEmulationSecurity.NetworkRequestId,
    -- | Identifier of the worker associated with this entry.
    logLogEntryWorkerId :: Maybe T.Text,
    -- | Call arguments.
    logLogEntryArgs :: Maybe [Runtime.RuntimeRemoteObject]
  }
  deriving (Eq, Show)
instance FromJSON LogLogEntry where
  parseJSON = A.withObject "LogLogEntry" $ \o -> LogLogEntry
    <$> o A..: "source"
    <*> o A..: "level"
    <*> o A..: "text"
    <*> o A..:? "category"
    <*> o A..: "timestamp"
    <*> o A..:? "url"
    <*> o A..:? "lineNumber"
    <*> o A..:? "stackTrace"
    <*> o A..:? "networkRequestId"
    <*> o A..:? "workerId"
    <*> o A..:? "args"
instance ToJSON LogLogEntry where
  toJSON p = A.object $ catMaybes [
    ("source" A..=) <$> Just (logLogEntrySource p),
    ("level" A..=) <$> Just (logLogEntryLevel p),
    ("text" A..=) <$> Just (logLogEntryText p),
    ("category" A..=) <$> (logLogEntryCategory p),
    ("timestamp" A..=) <$> Just (logLogEntryTimestamp p),
    ("url" A..=) <$> (logLogEntryUrl p),
    ("lineNumber" A..=) <$> (logLogEntryLineNumber p),
    ("stackTrace" A..=) <$> (logLogEntryStackTrace p),
    ("networkRequestId" A..=) <$> (logLogEntryNetworkRequestId p),
    ("workerId" A..=) <$> (logLogEntryWorkerId p),
    ("args" A..=) <$> (logLogEntryArgs p)
    ]

-- | Type 'Log.ViolationSetting'.
--   Violation configuration setting.
data LogViolationSettingName = LogViolationSettingNameLongTask | LogViolationSettingNameLongLayout | LogViolationSettingNameBlockedEvent | LogViolationSettingNameBlockedParser | LogViolationSettingNameDiscouragedAPIUse | LogViolationSettingNameHandler | LogViolationSettingNameRecurringHandler
  deriving (Ord, Eq, Show, Read)
instance FromJSON LogViolationSettingName where
  parseJSON = A.withText "LogViolationSettingName" $ \v -> case v of
    "longTask" -> pure LogViolationSettingNameLongTask
    "longLayout" -> pure LogViolationSettingNameLongLayout
    "blockedEvent" -> pure LogViolationSettingNameBlockedEvent
    "blockedParser" -> pure LogViolationSettingNameBlockedParser
    "discouragedAPIUse" -> pure LogViolationSettingNameDiscouragedAPIUse
    "handler" -> pure LogViolationSettingNameHandler
    "recurringHandler" -> pure LogViolationSettingNameRecurringHandler
    "_" -> fail "failed to parse LogViolationSettingName"
instance ToJSON LogViolationSettingName where
  toJSON v = A.String $ case v of
    LogViolationSettingNameLongTask -> "longTask"
    LogViolationSettingNameLongLayout -> "longLayout"
    LogViolationSettingNameBlockedEvent -> "blockedEvent"
    LogViolationSettingNameBlockedParser -> "blockedParser"
    LogViolationSettingNameDiscouragedAPIUse -> "discouragedAPIUse"
    LogViolationSettingNameHandler -> "handler"
    LogViolationSettingNameRecurringHandler -> "recurringHandler"
data LogViolationSetting = LogViolationSetting
  {
    -- | Violation type.
    logViolationSettingName :: LogViolationSettingName,
    -- | Time threshold to trigger upon.
    logViolationSettingThreshold :: Double
  }
  deriving (Eq, Show)
instance FromJSON LogViolationSetting where
  parseJSON = A.withObject "LogViolationSetting" $ \o -> LogViolationSetting
    <$> o A..: "name"
    <*> o A..: "threshold"
instance ToJSON LogViolationSetting where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (logViolationSettingName p),
    ("threshold" A..=) <$> Just (logViolationSettingThreshold p)
    ]

-- | Type of the 'Log.entryAdded' event.
data LogEntryAdded = LogEntryAdded
  {
    -- | The entry.
    logEntryAddedEntry :: LogLogEntry
  }
  deriving (Eq, Show)
instance FromJSON LogEntryAdded where
  parseJSON = A.withObject "LogEntryAdded" $ \o -> LogEntryAdded
    <$> o A..: "entry"
instance Event LogEntryAdded where
  eventName _ = "Log.entryAdded"

-- | Clears the log.

-- | Parameters of the 'Log.clear' command.
data PLogClear = PLogClear
  deriving (Eq, Show)
pLogClear
  :: PLogClear
pLogClear
  = PLogClear
instance ToJSON PLogClear where
  toJSON _ = A.Null
instance Command PLogClear where
  type CommandResponse PLogClear = ()
  commandName _ = "Log.clear"
  fromJSON = const . A.Success . const ()

-- | Disables log domain, prevents further log entries from being reported to the client.

-- | Parameters of the 'Log.disable' command.
data PLogDisable = PLogDisable
  deriving (Eq, Show)
pLogDisable
  :: PLogDisable
pLogDisable
  = PLogDisable
instance ToJSON PLogDisable where
  toJSON _ = A.Null
instance Command PLogDisable where
  type CommandResponse PLogDisable = ()
  commandName _ = "Log.disable"
  fromJSON = const . A.Success . const ()

-- | Enables log domain, sends the entries collected so far to the client by means of the
--   `entryAdded` notification.

-- | Parameters of the 'Log.enable' command.
data PLogEnable = PLogEnable
  deriving (Eq, Show)
pLogEnable
  :: PLogEnable
pLogEnable
  = PLogEnable
instance ToJSON PLogEnable where
  toJSON _ = A.Null
instance Command PLogEnable where
  type CommandResponse PLogEnable = ()
  commandName _ = "Log.enable"
  fromJSON = const . A.Success . const ()

-- | start violation reporting.

-- | Parameters of the 'Log.startViolationsReport' command.
data PLogStartViolationsReport = PLogStartViolationsReport
  {
    -- | Configuration for violations.
    pLogStartViolationsReportConfig :: [LogViolationSetting]
  }
  deriving (Eq, Show)
pLogStartViolationsReport
  -- | Configuration for violations.
  :: [LogViolationSetting]
  -> PLogStartViolationsReport
pLogStartViolationsReport
  arg_pLogStartViolationsReportConfig
  = PLogStartViolationsReport
    arg_pLogStartViolationsReportConfig
instance ToJSON PLogStartViolationsReport where
  toJSON p = A.object $ catMaybes [
    ("config" A..=) <$> Just (pLogStartViolationsReportConfig p)
    ]
instance Command PLogStartViolationsReport where
  type CommandResponse PLogStartViolationsReport = ()
  commandName _ = "Log.startViolationsReport"
  fromJSON = const . A.Success . const ()

-- | Stop violation reporting.

-- | Parameters of the 'Log.stopViolationsReport' command.
data PLogStopViolationsReport = PLogStopViolationsReport
  deriving (Eq, Show)
pLogStopViolationsReport
  :: PLogStopViolationsReport
pLogStopViolationsReport
  = PLogStopViolationsReport
instance ToJSON PLogStopViolationsReport where
  toJSON _ = A.Null
instance Command PLogStopViolationsReport where
  type CommandResponse PLogStopViolationsReport = ()
  commandName _ = "Log.stopViolationsReport"
  fromJSON = const . A.Success . const ()

