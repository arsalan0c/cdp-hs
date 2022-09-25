{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.PerformanceTimeline (module CDP.Domains.PerformanceTimeline) where

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity



data PerformanceTimelineLargestContentfulPaint = PerformanceTimelineLargestContentfulPaint {
   performanceTimelineLargestContentfulPaintRenderTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
   performanceTimelineLargestContentfulPaintLoadTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
   performanceTimelineLargestContentfulPaintSize :: Double,
   performanceTimelineLargestContentfulPaintElementId :: Maybe String,
   performanceTimelineLargestContentfulPaintUrl :: Maybe String,
   performanceTimelineLargestContentfulPaintNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLargestContentfulPaint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLargestContentfulPaint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



data PerformanceTimelineLayoutShiftAttribution = PerformanceTimelineLayoutShiftAttribution {
   performanceTimelineLayoutShiftAttributionPreviousRect :: DOMPageNetworkEmulationSecurity.DomRect,
   performanceTimelineLayoutShiftAttributionCurrentRect :: DOMPageNetworkEmulationSecurity.DomRect,
   performanceTimelineLayoutShiftAttributionNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLayoutShiftAttribution  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLayoutShiftAttribution where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



data PerformanceTimelineLayoutShift = PerformanceTimelineLayoutShift {
   performanceTimelineLayoutShiftValue :: Double,
   performanceTimelineLayoutShiftHadRecentInput :: Bool,
   performanceTimelineLayoutShiftLastInputTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
   performanceTimelineLayoutShiftSources :: [PerformanceTimelineLayoutShiftAttribution]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLayoutShift  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLayoutShift where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data PerformanceTimelineTimelineEvent = PerformanceTimelineTimelineEvent {
   performanceTimelineTimelineEventFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   performanceTimelineTimelineEventType :: String,
   performanceTimelineTimelineEventName :: String,
   performanceTimelineTimelineEventTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
   performanceTimelineTimelineEventDuration :: Maybe Double,
   performanceTimelineTimelineEventLcpDetails :: Maybe PerformanceTimelineLargestContentfulPaint,
   performanceTimelineTimelineEventLayoutShiftDetails :: Maybe PerformanceTimelineLayoutShift
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineTimelineEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineTimelineEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }





data PerformanceTimelineTimelineEventAdded = PerformanceTimelineTimelineEventAdded {
   performanceTimelineTimelineEventAddedEvent :: PerformanceTimelineTimelineEvent
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineTimelineEventAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineTimelineEventAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }





data PPerformanceTimelineEnable = PPerformanceTimelineEnable {
   pPerformanceTimelineEnableEventTypes :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPerformanceTimelineEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PPerformanceTimelineEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


performanceTimelineEnable :: Handle ev -> PPerformanceTimelineEnable -> IO (Maybe Error)
performanceTimelineEnable handle params = sendReceiveCommand handle "PerformanceTimeline.enable" (Just params)



