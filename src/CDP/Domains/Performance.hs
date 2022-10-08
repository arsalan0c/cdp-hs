{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


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





-- | Function for the 'Performance.disable' command.
--   Disable collecting and reporting metrics.
performanceDisable :: Handle ev -> IO ()
performanceDisable handle = sendReceiveCommand handle "Performance.disable" (Nothing :: Maybe ())


-- | Parameters of the 'performanceEnable' command.
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


-- | Function for the 'Performance.enable' command.
--   Enable collecting and reporting metrics.
--   Parameters: 'PPerformanceEnable'
performanceEnable :: Handle ev -> PPerformanceEnable -> IO ()
performanceEnable handle params = sendReceiveCommand handle "Performance.enable" (Just params)


-- | Function for the 'Performance.getMetrics' command.
--   Retrieve current values of run-time metrics.
--   Returns: 'PerformanceGetMetrics'
performanceGetMetrics :: Handle ev -> IO PerformanceGetMetrics
performanceGetMetrics handle = sendReceiveCommandResult handle "Performance.getMetrics" (Nothing :: Maybe ())

-- | Return type of the 'performanceGetMetrics' command.
data PerformanceGetMetrics = PerformanceGetMetrics {
  -- | Current values for run-time metrics.
  performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PerformanceGetMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PerformanceGetMetrics where
   commandName _ = "Performance.getMetrics"




