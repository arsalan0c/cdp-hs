{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  PerformanceTimeline :
     Reporting of performance timeline events, as specified in
     https://w3c.github.io/performance-timeline/#dom-performanceobserver.

-}


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


-- | See https://github.com/WICG/LargestContentfulPaint and largest_contentful_paint.idl
data PerformanceTimelineLargestContentfulPaint = PerformanceTimelineLargestContentfulPaint {
  performanceTimelineLargestContentfulPaintRenderTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  performanceTimelineLargestContentfulPaintLoadTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  -- | The number of pixels being painted.
  performanceTimelineLargestContentfulPaintSize :: Double,
  -- | The id attribute of the element, if available.
  performanceTimelineLargestContentfulPaintElementId :: Maybe String,
  -- | The URL of the image (may be trimmed).
  performanceTimelineLargestContentfulPaintUrl :: Maybe String,
  performanceTimelineLargestContentfulPaintNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLargestContentfulPaint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLargestContentfulPaint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | Type 'PerformanceTimeline.LayoutShiftAttribution' .
data PerformanceTimelineLayoutShiftAttribution = PerformanceTimelineLayoutShiftAttribution {
  performanceTimelineLayoutShiftAttributionPreviousRect :: DOMPageNetworkEmulationSecurity.DomRect,
  performanceTimelineLayoutShiftAttributionCurrentRect :: DOMPageNetworkEmulationSecurity.DomRect,
  performanceTimelineLayoutShiftAttributionNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLayoutShiftAttribution  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLayoutShiftAttribution where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | See https://wicg.github.io/layout-instability/#sec-layout-shift and layout_shift.idl
data PerformanceTimelineLayoutShift = PerformanceTimelineLayoutShift {
  -- | Score increment produced by this event.
  performanceTimelineLayoutShiftValue :: Double,
  performanceTimelineLayoutShiftHadRecentInput :: Bool,
  performanceTimelineLayoutShiftLastInputTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  performanceTimelineLayoutShiftSources :: [PerformanceTimelineLayoutShiftAttribution]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineLayoutShift  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineLayoutShift where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'PerformanceTimeline.TimelineEvent' .
data PerformanceTimelineTimelineEvent = PerformanceTimelineTimelineEvent {
  -- | Identifies the frame that this event is related to. Empty for non-frame targets.
  performanceTimelineTimelineEventFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
  -- | The event type, as specified in https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype
  -- This determines which of the optional "details" fiedls is present.
  performanceTimelineTimelineEventType :: String,
  -- | Name may be empty depending on the type.
  performanceTimelineTimelineEventName :: String,
  -- | Time in seconds since Epoch, monotonically increasing within document lifetime.
  performanceTimelineTimelineEventTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  -- | Event duration, if applicable.
  performanceTimelineTimelineEventDuration :: Maybe Double,
  performanceTimelineTimelineEventLcpDetails :: Maybe PerformanceTimelineLargestContentfulPaint,
  performanceTimelineTimelineEventLayoutShiftDetails :: Maybe PerformanceTimelineLayoutShift
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineTimelineEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineTimelineEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }





-- | Type of the 'PerformanceTimeline.timelineEventAdded' event.
data PerformanceTimelineTimelineEventAdded = PerformanceTimelineTimelineEventAdded {
  performanceTimelineTimelineEventAddedEvent :: PerformanceTimelineTimelineEvent
} deriving (Generic, Eq, Show, Read)
instance ToJSON PerformanceTimelineTimelineEventAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PerformanceTimelineTimelineEventAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }





-- | Parameters of the 'performanceTimelineEnable' command.
data PPerformanceTimelineEnable = PPerformanceTimelineEnable {
  -- | The types of event to report, as specified in
  -- https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype
  -- The specified filter overrides any previous filters, passing empty
  -- filter disables recording.
  -- Note that not all types exposed to the web platform are currently supported.
  pPerformanceTimelineEnableEventTypes :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPerformanceTimelineEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PPerformanceTimelineEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'PerformanceTimeline.enable' command.
-- Previously buffered events would be reported before method returns.
-- See also: timelineEventAdded
-- Parameters: 'PPerformanceTimelineEnable'
performanceTimelineEnable :: Handle ev -> PPerformanceTimelineEnable -> IO (Maybe Error)
performanceTimelineEnable handle params = sendReceiveCommand handle "PerformanceTimeline.enable" (Just params)



