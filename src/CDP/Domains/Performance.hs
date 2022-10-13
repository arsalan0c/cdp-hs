{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Performance 
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
data PerformanceMetric = PerformanceMetric {
  -- | Metric name.
  performanceMetricName :: String,
  -- | Metric value.
  performanceMetricValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetric  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetric where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }





-- | Type of the 'Performance.metrics' event.
data PerformanceMetrics = PerformanceMetrics {
  -- | Current values of the metrics.
  performanceMetricsMetrics :: [PerformanceMetric],
  -- | Timestamp title.
  performanceMetricsTitle :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetrics  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Event PerformanceMetrics where
    eventName _ = "Performance.metrics"



-- | Performance.disable
--   Disable collecting and reporting metrics.

-- | Parameters of the 'Performance.disable' command.
data PPerformanceDisable = PPerformanceDisable
instance ToJSON PPerformanceDisable where toJSON _ = A.Null

instance Command PPerformanceDisable where
   type CommandResponse PPerformanceDisable = ()
   commandName _ = "Performance.disable"
   fromJSON = const . A.Success . const ()


-- | Performance.enable
--   Enable collecting and reporting metrics.

-- | Parameters of the 'Performance.enable' command.
data PPerformanceEnableTimeDomain = PPerformanceEnableTimeDomainTimeTicks | PPerformanceEnableTimeDomainThreadTicks
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPerformanceEnableTimeDomain where
   parseJSON = A.withText  "PPerformanceEnableTimeDomain"  $ \v -> do
      case v of
         "timeTicks" -> pure PPerformanceEnableTimeDomainTimeTicks
         "threadTicks" -> pure PPerformanceEnableTimeDomainThreadTicks
         _ -> fail "failed to parse PPerformanceEnableTimeDomain"

instance ToJSON PPerformanceEnableTimeDomain where
   toJSON v = A.String $
      case v of
         PPerformanceEnableTimeDomainTimeTicks -> "timeTicks"
         PPerformanceEnableTimeDomainThreadTicks -> "threadTicks"



data PPerformanceEnable = PPerformanceEnable {
  -- | Time domain to use for collecting and reporting duration metrics.
  pPerformanceEnableTimeDomain :: PPerformanceEnableTimeDomain
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPerformanceEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PPerformanceEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Command PPerformanceEnable where
   type CommandResponse PPerformanceEnable = ()
   commandName _ = "Performance.enable"
   fromJSON = const . A.Success . const ()


-- | Performance.getMetrics
--   Retrieve current values of run-time metrics.

-- | Parameters of the 'Performance.getMetrics' command.
data PPerformanceGetMetrics = PPerformanceGetMetrics
instance ToJSON PPerformanceGetMetrics where toJSON _ = A.Null

-- | Return type of the 'Performance.getMetrics' command.
data PerformanceGetMetrics = PerformanceGetMetrics {
  -- | Current values for run-time metrics.
  performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PerformanceGetMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PPerformanceGetMetrics where
   type CommandResponse PPerformanceGetMetrics = PerformanceGetMetrics
   commandName _ = "Performance.getMetrics"




