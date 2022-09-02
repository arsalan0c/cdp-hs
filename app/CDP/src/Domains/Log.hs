{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.Log (module Domains.Log) where

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

import Utils

import qualified Domains.Runtime as Runtime
import qualified Domains.Network as Network


data LogEvent = EVLogEntryAdded LogEntryAdded
    deriving (Eq, Show, Read)

data LogEntryAdded = LogEntryAdded {
    logEntryAddedEntry :: LogLogEntry
} deriving (Eq, Show, Read)
instance FromJSON  LogEntryAdded where
    parseJSON = A.withObject "LogEntryAdded" $ \v ->
         LogEntryAdded <$> v .:  "entry"


instance ToJSON LogEntryAdded  where
    toJSON v = A.object
        [ "entry" .= logEntryAddedEntry v
        ]


instance FromEvent LogEvent LogEntryAdded where
    eventName  _ _    =  "Log.entryAdded"
    fromEvent ev =  case ev of EVLogEntryAdded v -> Just v; _ -> Nothing




subscribe :: forall a. FromEvent LogEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy LogEvent
    pa       = Proxy :: Proxy a


data LogLogEntry = LogLogEntry {
    logLogEntrySource :: String,
    logLogEntryLevel :: String,
    logLogEntryText :: String,
    logLogEntryTimestamp :: RuntimeTimestamp,
    logLogEntryCategory :: Maybe String,
    logLogEntryUrl :: Maybe String,
    logLogEntryLineNumber :: Maybe Int,
    logLogEntryStackTrace :: Maybe RuntimeStackTrace,
    logLogEntryNetworkRequestId :: Maybe NetworkRequestId,
    logLogEntryWorkerId :: Maybe String,
    logLogEntryArgs :: Maybe [RuntimeRemoteObject]
} deriving (Eq, Show, Read)
instance FromJSON  LogLogEntry where
    parseJSON = A.withObject "LogLogEntry" $ \v ->
         LogLogEntry <$> v .:  "source"
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


instance ToJSON LogLogEntry  where
    toJSON v = A.object
        [ "source" .= logLogEntrySource v
        , "level" .= logLogEntryLevel v
        , "text" .= logLogEntryText v
        , "timestamp" .= logLogEntryTimestamp v
        , "category" .= logLogEntryCategory v
        , "url" .= logLogEntryUrl v
        , "lineNumber" .= logLogEntryLineNumber v
        , "stackTrace" .= logLogEntryStackTrace v
        , "networkRequestId" .= logLogEntryNetworkRequestId v
        , "workerId" .= logLogEntryWorkerId v
        , "args" .= logLogEntryArgs v
        ]



data LogViolationSetting = LogViolationSetting {
    logViolationSettingName :: String,
    logViolationSettingThreshold :: Int
} deriving (Eq, Show, Read)
instance FromJSON  LogViolationSetting where
    parseJSON = A.withObject "LogViolationSetting" $ \v ->
         LogViolationSetting <$> v .:  "name"
            <*> v  .:  "threshold"


instance ToJSON LogViolationSetting  where
    toJSON v = A.object
        [ "name" .= logViolationSettingName v
        , "threshold" .= logViolationSettingThreshold v
        ]






logClear :: Session -> IO (Maybe Error)
logClear session = sendReceiveCommand session "Log.clear" (Nothing :: Maybe ())




logDisable :: Session -> IO (Maybe Error)
logDisable session = sendReceiveCommand session "Log.disable" (Nothing :: Maybe ())




logEnable :: Session -> IO (Maybe Error)
logEnable session = sendReceiveCommand session "Log.enable" (Nothing :: Maybe ())



data PLogStartViolationsReport = PLogStartViolationsReport {
    pLogStartViolationsReportConfig :: [LogViolationSetting]
} deriving (Eq, Show, Read)
instance FromJSON  PLogStartViolationsReport where
    parseJSON = A.withObject "PLogStartViolationsReport" $ \v ->
         PLogStartViolationsReport <$> v .:  "config"


instance ToJSON PLogStartViolationsReport  where
    toJSON v = A.object
        [ "config" .= pLogStartViolationsReportConfig v
        ]


logStartViolationsReport :: Session -> PLogStartViolationsReport -> IO (Maybe Error)
logStartViolationsReport session params = sendReceiveCommand session "Log.startViolationsReport" (Just params)




logStopViolationsReport :: Session -> IO (Maybe Error)
logStopViolationsReport session = sendReceiveCommand session "Log.stopViolationsReport" (Nothing :: Maybe ())

