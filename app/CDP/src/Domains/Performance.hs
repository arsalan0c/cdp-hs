{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Performance (module Domains.Performance) where
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



import Utils

data Metrics = Metrics {
    metricsMetrics :: [Metric],
    metricsTitle :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Metrics where
    parseJSON = A.withObject "Metrics" $ \v ->
         Metrics <$> v .:  "metrics"
            <*> v  .:  "title"


instance ToJSON Metrics  where
    toJSON v = A.object
        [ "metrics" .= metricsMetrics v
        , "title" .= metricsTitle v
        ]



data Metric = Metric {
    metricName :: String,
    metricValue :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Metric where
    parseJSON = A.withObject "Metric" $ \v ->
         Metric <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON Metric  where
    toJSON v = A.object
        [ "name" .= metricName v
        , "value" .= metricValue v
        ]



disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Performance","disable") ([] ++ (catMaybes []))


enable :: Session a -> Maybe String -> IO (Maybe Error)
enable session enableTimeDomain = sendReceiveCommand (conn session) ("Performance","enable") ([] ++ (catMaybes [fmap (("timeDomain",) . ToJSONEx) enableTimeDomain]))

data GetMetrics = GetMetrics {
    getMetricsMetrics :: [Metric]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetMetrics where
    parseJSON = A.withObject "GetMetrics" $ \v ->
         GetMetrics <$> v .:  "metrics"



getMetrics :: Session a -> IO (Either Error GetMetrics)
getMetrics session  = sendReceiveCommandResult (conn session) ("Performance","getMetrics") ([] ++ (catMaybes []))


