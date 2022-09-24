{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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




data PerformanceMetric = PerformanceMetric {
   performanceMetricName :: String,
   performanceMetricValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetric  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetric where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }





data PerformanceMetrics = PerformanceMetrics {
   performanceMetricsMetrics :: [PerformanceMetric],
   performanceMetricsTitle :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceMetrics  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PerformanceMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }




performanceDisable :: Handle ev -> IO (Maybe Error)
performanceDisable handle = sendReceiveCommand handle "Performance.disable" (Nothing :: Maybe ())


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
   pPerformanceEnableTimeDomain :: PPerformanceEnableTimeDomain
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPerformanceEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PPerformanceEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


performanceEnable :: Handle ev -> PPerformanceEnable -> IO (Maybe Error)
performanceEnable handle params = sendReceiveCommand handle "Performance.enable" (Just params)


performanceGetMetrics :: Handle ev -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics handle = sendReceiveCommandResult handle "Performance.getMetrics" (Nothing :: Maybe ())

data PerformanceGetMetrics = PerformanceGetMetrics {
   performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PerformanceGetMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PerformanceGetMetrics where
   commandName _ = "Performance.getMetrics"




