{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Performance

-}


module CDP.Domains.Performance (module CDP.Domains.Performance) where

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




-- | Type 'Performance.Metric'.
--   Run-time execution metric.
data PerformanceMetric = PerformanceMetric
  {
    -- | Metric name.
    performanceMetricName :: T.Text,
    -- | Metric value.
    performanceMetricValue :: Double
  }
  deriving (Eq, Show)
instance FromJSON PerformanceMetric where
  parseJSON = A.withObject "PerformanceMetric" $ \o -> PerformanceMetric
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON PerformanceMetric where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (performanceMetricName p),
    ("value" A..=) <$> Just (performanceMetricValue p)
    ]

-- | Type of the 'Performance.metrics' event.
data PerformanceMetrics = PerformanceMetrics
  {
    -- | Current values of the metrics.
    performanceMetricsMetrics :: [PerformanceMetric],
    -- | Timestamp title.
    performanceMetricsTitle :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PerformanceMetrics where
  parseJSON = A.withObject "PerformanceMetrics" $ \o -> PerformanceMetrics
    <$> o A..: "metrics"
    <*> o A..: "title"
instance Event PerformanceMetrics where
  eventName _ = "Performance.metrics"

-- | Disable collecting and reporting metrics.

-- | Parameters of the 'Performance.disable' command.
data PPerformanceDisable = PPerformanceDisable
  deriving (Eq, Show)
pPerformanceDisable
  :: PPerformanceDisable
pPerformanceDisable
  = PPerformanceDisable
instance ToJSON PPerformanceDisable where
  toJSON _ = A.Null
instance Command PPerformanceDisable where
  type CommandResponse PPerformanceDisable = ()
  commandName _ = "Performance.disable"
  fromJSON = const . A.Success . const ()

-- | Enable collecting and reporting metrics.

-- | Parameters of the 'Performance.enable' command.
data PPerformanceEnableTimeDomain = PPerformanceEnableTimeDomainTimeTicks | PPerformanceEnableTimeDomainThreadTicks
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPerformanceEnableTimeDomain where
  parseJSON = A.withText "PPerformanceEnableTimeDomain" $ \v -> case v of
    "timeTicks" -> pure PPerformanceEnableTimeDomainTimeTicks
    "threadTicks" -> pure PPerformanceEnableTimeDomainThreadTicks
    "_" -> fail "failed to parse PPerformanceEnableTimeDomain"
instance ToJSON PPerformanceEnableTimeDomain where
  toJSON v = A.String $ case v of
    PPerformanceEnableTimeDomainTimeTicks -> "timeTicks"
    PPerformanceEnableTimeDomainThreadTicks -> "threadTicks"
data PPerformanceEnable = PPerformanceEnable
  {
    -- | Time domain to use for collecting and reporting duration metrics.
    pPerformanceEnableTimeDomain :: Maybe PPerformanceEnableTimeDomain
  }
  deriving (Eq, Show)
pPerformanceEnable
  :: PPerformanceEnable
pPerformanceEnable
  = PPerformanceEnable
    Nothing
instance ToJSON PPerformanceEnable where
  toJSON p = A.object $ catMaybes [
    ("timeDomain" A..=) <$> (pPerformanceEnableTimeDomain p)
    ]
instance Command PPerformanceEnable where
  type CommandResponse PPerformanceEnable = ()
  commandName _ = "Performance.enable"
  fromJSON = const . A.Success . const ()

-- | Retrieve current values of run-time metrics.

-- | Parameters of the 'Performance.getMetrics' command.
data PPerformanceGetMetrics = PPerformanceGetMetrics
  deriving (Eq, Show)
pPerformanceGetMetrics
  :: PPerformanceGetMetrics
pPerformanceGetMetrics
  = PPerformanceGetMetrics
instance ToJSON PPerformanceGetMetrics where
  toJSON _ = A.Null
data PerformanceGetMetrics = PerformanceGetMetrics
  {
    -- | Current values for run-time metrics.
    performanceGetMetricsMetrics :: [PerformanceMetric]
  }
  deriving (Eq, Show)
instance FromJSON PerformanceGetMetrics where
  parseJSON = A.withObject "PerformanceGetMetrics" $ \o -> PerformanceGetMetrics
    <$> o A..: "metrics"
instance Command PPerformanceGetMetrics where
  type CommandResponse PPerformanceGetMetrics = PerformanceGetMetrics
  commandName _ = "Performance.getMetrics"

