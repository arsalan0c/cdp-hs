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

module Domains.Performance (module Domains.Performance) where

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



data PerformanceEvent = EVPerformanceMetrics PerformanceMetrics
    deriving (Eq, Show, Read)

data PerformanceMetrics = PerformanceMetrics {
    performanceMetricsMetrics :: [PerformanceMetric],
    performanceMetricsTitle :: String
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceMetrics where
    parseJSON = A.withObject "PerformanceMetrics" $ \v ->
         PerformanceMetrics <$> v .:  "metrics"
            <*> v  .:  "title"


instance ToJSON PerformanceMetrics  where
    toJSON v = A.object
        [ "metrics" .= performanceMetricsMetrics v
        , "title" .= performanceMetricsTitle v
        ]


instance FromEvent PerformanceEvent PerformanceMetrics where
    eventName  _ _    =  "Performance.metrics"
    fromEvent ev =  case ev of EVPerformanceMetrics v -> Just v; _ -> Nothing




subscribe :: forall a. FromEvent PerformanceEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy PerformanceEvent
    pa       = Proxy :: Proxy a


data PerformanceMetric = PerformanceMetric {
    performanceMetricName :: String,
    performanceMetricValue :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceMetric where
    parseJSON = A.withObject "PerformanceMetric" $ \v ->
         PerformanceMetric <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON PerformanceMetric  where
    toJSON v = A.object
        [ "name" .= performanceMetricName v
        , "value" .= performanceMetricValue v
        ]






performanceDisable :: Session -> IO (Maybe Error)
performanceDisable session = sendReceiveCommand session "Performance.disable" (Nothing :: Maybe ())



data PPerformanceEnable = PPerformanceEnable {
    pPerformanceEnableTimeDomain :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PPerformanceEnable where
    parseJSON = A.withObject "PPerformanceEnable" $ \v ->
         PPerformanceEnable <$> v .:?  "timeDomain"


instance ToJSON PPerformanceEnable  where
    toJSON v = A.object
        [ "timeDomain" .= pPerformanceEnableTimeDomain v
        ]


performanceEnable :: Session -> PPerformanceEnable -> IO (Maybe Error)
performanceEnable session params = sendReceiveCommand session "Performance.enable" (Just params)

data PerformanceGetMetrics = PerformanceGetMetrics {
    performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving (Eq, Show, Read)
instance FromJSON  PerformanceGetMetrics where
    parseJSON = A.withObject "PerformanceGetMetrics" $ \v ->
         PerformanceGetMetrics <$> v .:  "metrics"



instance Command  PerformanceGetMetrics where
    commandName _ = "Performance.getMetrics"


performanceGetMetrics :: Session -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics session = sendReceiveCommandResult session "Performance.getMetrics" (Nothing :: Maybe ())

