{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Log (module Domains.Log) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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

import qualified Domains.Runtime as Runtime
import qualified Domains.Network as Network


import Utils

data EntryAdded = EntryAdded {
    entryAddedEntry :: LogEntry
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  EntryAdded where
    parseJSON = A.withObject "EntryAdded" $ \v ->
         EntryAdded <$> v .:  "entry"


instance ToJSON EntryAdded  where
    toJSON v = A.object
        [ "entry" .= entryAddedEntry v
        ]



data LogEntry = LogEntry {
    logEntrySource :: String,
    logEntryLevel :: String,
    logEntryText :: String,
    logEntryTimestamp :: Runtime.Timestamp,
    logEntryCategory :: Maybe String,
    logEntryUrl :: Maybe String,
    logEntryLineNumber :: Maybe Int,
    logEntryStackTrace :: Maybe Runtime.StackTrace,
    logEntryNetworkRequestId :: Maybe Network.RequestId,
    logEntryWorkerId :: Maybe String,
    logEntryArgs :: Maybe [Runtime.RemoteObject]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LogEntry where
    parseJSON = A.withObject "LogEntry" $ \v ->
         LogEntry <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:  "timestamp"
            <*> v  .:?  "category"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "networkRequestId"
            <*> v  .:?  "workerId"
            <*> v  .:?  "args"


instance ToJSON LogEntry  where
    toJSON v = A.object
        [ "source" .= logEntrySource v
        , "level" .= logEntryLevel v
        , "text" .= logEntryText v
        , "timestamp" .= logEntryTimestamp v
        , "category" .= logEntryCategory v
        , "url" .= logEntryUrl v
        , "lineNumber" .= logEntryLineNumber v
        , "stackTrace" .= logEntryStackTrace v
        , "networkRequestId" .= logEntryNetworkRequestId v
        , "workerId" .= logEntryWorkerId v
        , "args" .= logEntryArgs v
        ]



data ViolationSetting = ViolationSetting {
    violationSettingName :: String,
    violationSettingThreshold :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ViolationSetting where
    parseJSON = A.withObject "ViolationSetting" $ \v ->
         ViolationSetting <$> v .:  "name"
            <*> v  .:  "threshold"


instance ToJSON ViolationSetting  where
    toJSON v = A.object
        [ "name" .= violationSettingName v
        , "threshold" .= violationSettingThreshold v
        ]



clear :: Session a -> IO (Maybe Error)
clear session  = sendReceiveCommand (conn session) ("Log","clear") ([] ++ (catMaybes []))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Log","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Log","enable") ([] ++ (catMaybes []))


startViolationsReport :: Session a -> [ViolationSetting] -> IO (Maybe Error)
startViolationsReport session startViolationsReportConfig = sendReceiveCommand (conn session) ("Log","startViolationsReport") ([("config", ToJSONEx startViolationsReportConfig)] ++ (catMaybes []))


stopViolationsReport :: Session a -> IO (Maybe Error)
stopViolationsReport session  = sendReceiveCommand (conn session) ("Log","stopViolationsReport") ([] ++ (catMaybes []))


