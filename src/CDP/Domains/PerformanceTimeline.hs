{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= PerformanceTimeline

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'PerformanceTimeline.LargestContentfulPaint'.
--   See https://github.com/WICG/LargestContentfulPaint and largest_contentful_paint.idl
data PerformanceTimelineLargestContentfulPaint = PerformanceTimelineLargestContentfulPaint
  {
    performanceTimelineLargestContentfulPaintRenderTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    performanceTimelineLargestContentfulPaintLoadTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    -- | The number of pixels being painted.
    performanceTimelineLargestContentfulPaintSize :: Double,
    -- | The id attribute of the element, if available.
    performanceTimelineLargestContentfulPaintElementId :: Maybe T.Text,
    -- | The URL of the image (may be trimmed).
    performanceTimelineLargestContentfulPaintUrl :: Maybe T.Text,
    performanceTimelineLargestContentfulPaintNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON PerformanceTimelineLargestContentfulPaint where
  parseJSON = A.withObject "PerformanceTimelineLargestContentfulPaint" $ \o -> PerformanceTimelineLargestContentfulPaint
    <$> o A..: "renderTime"
    <*> o A..: "loadTime"
    <*> o A..: "size"
    <*> o A..:? "elementId"
    <*> o A..:? "url"
    <*> o A..:? "nodeId"
instance ToJSON PerformanceTimelineLargestContentfulPaint where
  toJSON p = A.object $ catMaybes [
    ("renderTime" A..=) <$> Just (performanceTimelineLargestContentfulPaintRenderTime p),
    ("loadTime" A..=) <$> Just (performanceTimelineLargestContentfulPaintLoadTime p),
    ("size" A..=) <$> Just (performanceTimelineLargestContentfulPaintSize p),
    ("elementId" A..=) <$> (performanceTimelineLargestContentfulPaintElementId p),
    ("url" A..=) <$> (performanceTimelineLargestContentfulPaintUrl p),
    ("nodeId" A..=) <$> (performanceTimelineLargestContentfulPaintNodeId p)
    ]

-- | Type 'PerformanceTimeline.LayoutShiftAttribution'.
data PerformanceTimelineLayoutShiftAttribution = PerformanceTimelineLayoutShiftAttribution
  {
    performanceTimelineLayoutShiftAttributionPreviousRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    performanceTimelineLayoutShiftAttributionCurrentRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    performanceTimelineLayoutShiftAttributionNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON PerformanceTimelineLayoutShiftAttribution where
  parseJSON = A.withObject "PerformanceTimelineLayoutShiftAttribution" $ \o -> PerformanceTimelineLayoutShiftAttribution
    <$> o A..: "previousRect"
    <*> o A..: "currentRect"
    <*> o A..:? "nodeId"
instance ToJSON PerformanceTimelineLayoutShiftAttribution where
  toJSON p = A.object $ catMaybes [
    ("previousRect" A..=) <$> Just (performanceTimelineLayoutShiftAttributionPreviousRect p),
    ("currentRect" A..=) <$> Just (performanceTimelineLayoutShiftAttributionCurrentRect p),
    ("nodeId" A..=) <$> (performanceTimelineLayoutShiftAttributionNodeId p)
    ]

-- | Type 'PerformanceTimeline.LayoutShift'.
--   See https://wicg.github.io/layout-instability/#sec-layout-shift and layout_shift.idl
data PerformanceTimelineLayoutShift = PerformanceTimelineLayoutShift
  {
    -- | Score increment produced by this event.
    performanceTimelineLayoutShiftValue :: Double,
    performanceTimelineLayoutShiftHadRecentInput :: Bool,
    performanceTimelineLayoutShiftLastInputTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    performanceTimelineLayoutShiftSources :: [PerformanceTimelineLayoutShiftAttribution]
  }
  deriving (Eq, Show)
instance FromJSON PerformanceTimelineLayoutShift where
  parseJSON = A.withObject "PerformanceTimelineLayoutShift" $ \o -> PerformanceTimelineLayoutShift
    <$> o A..: "value"
    <*> o A..: "hadRecentInput"
    <*> o A..: "lastInputTime"
    <*> o A..: "sources"
instance ToJSON PerformanceTimelineLayoutShift where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (performanceTimelineLayoutShiftValue p),
    ("hadRecentInput" A..=) <$> Just (performanceTimelineLayoutShiftHadRecentInput p),
    ("lastInputTime" A..=) <$> Just (performanceTimelineLayoutShiftLastInputTime p),
    ("sources" A..=) <$> Just (performanceTimelineLayoutShiftSources p)
    ]

-- | Type 'PerformanceTimeline.TimelineEvent'.
data PerformanceTimelineTimelineEvent = PerformanceTimelineTimelineEvent
  {
    -- | Identifies the frame that this event is related to. Empty for non-frame targets.
    performanceTimelineTimelineEventFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | The event type, as specified in https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype
    --   This determines which of the optional "details" fiedls is present.
    performanceTimelineTimelineEventType :: T.Text,
    -- | Name may be empty depending on the type.
    performanceTimelineTimelineEventName :: T.Text,
    -- | Time in seconds since Epoch, monotonically increasing within document lifetime.
    performanceTimelineTimelineEventTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    -- | Event duration, if applicable.
    performanceTimelineTimelineEventDuration :: Maybe Double,
    performanceTimelineTimelineEventLcpDetails :: Maybe PerformanceTimelineLargestContentfulPaint,
    performanceTimelineTimelineEventLayoutShiftDetails :: Maybe PerformanceTimelineLayoutShift
  }
  deriving (Eq, Show)
instance FromJSON PerformanceTimelineTimelineEvent where
  parseJSON = A.withObject "PerformanceTimelineTimelineEvent" $ \o -> PerformanceTimelineTimelineEvent
    <$> o A..: "frameId"
    <*> o A..: "type"
    <*> o A..: "name"
    <*> o A..: "time"
    <*> o A..:? "duration"
    <*> o A..:? "lcpDetails"
    <*> o A..:? "layoutShiftDetails"
instance ToJSON PerformanceTimelineTimelineEvent where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (performanceTimelineTimelineEventFrameId p),
    ("type" A..=) <$> Just (performanceTimelineTimelineEventType p),
    ("name" A..=) <$> Just (performanceTimelineTimelineEventName p),
    ("time" A..=) <$> Just (performanceTimelineTimelineEventTime p),
    ("duration" A..=) <$> (performanceTimelineTimelineEventDuration p),
    ("lcpDetails" A..=) <$> (performanceTimelineTimelineEventLcpDetails p),
    ("layoutShiftDetails" A..=) <$> (performanceTimelineTimelineEventLayoutShiftDetails p)
    ]

-- | Type of the 'PerformanceTimeline.timelineEventAdded' event.
data PerformanceTimelineTimelineEventAdded = PerformanceTimelineTimelineEventAdded
  {
    performanceTimelineTimelineEventAddedEvent :: PerformanceTimelineTimelineEvent
  }
  deriving (Eq, Show)
instance FromJSON PerformanceTimelineTimelineEventAdded where
  parseJSON = A.withObject "PerformanceTimelineTimelineEventAdded" $ \o -> PerformanceTimelineTimelineEventAdded
    <$> o A..: "event"
instance Event PerformanceTimelineTimelineEventAdded where
  eventName _ = "PerformanceTimeline.timelineEventAdded"

-- | Previously buffered events would be reported before method returns.
--   See also: timelineEventAdded

-- | Parameters of the 'PerformanceTimeline.enable' command.
data PPerformanceTimelineEnable = PPerformanceTimelineEnable
  {
    -- | The types of event to report, as specified in
    --   https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype
    --   The specified filter overrides any previous filters, passing empty
    --   filter disables recording.
    --   Note that not all types exposed to the web platform are currently supported.
    pPerformanceTimelineEnableEventTypes :: [T.Text]
  }
  deriving (Eq, Show)
pPerformanceTimelineEnable
  {-
  -- | The types of event to report, as specified in
  --   https://w3c.github.io/performance-timeline/#dom-performanceentry-entrytype
  --   The specified filter overrides any previous filters, passing empty
  --   filter disables recording.
  --   Note that not all types exposed to the web platform are currently supported.
  -}
  :: [T.Text]
  -> PPerformanceTimelineEnable
pPerformanceTimelineEnable
  arg_pPerformanceTimelineEnableEventTypes
  = PPerformanceTimelineEnable
    arg_pPerformanceTimelineEnableEventTypes
instance ToJSON PPerformanceTimelineEnable where
  toJSON p = A.object $ catMaybes [
    ("eventTypes" A..=) <$> Just (pPerformanceTimelineEnableEventTypes p)
    ]
instance Command PPerformanceTimelineEnable where
  type CommandResponse PPerformanceTimelineEnable = ()
  commandName _ = "PerformanceTimeline.enable"
  fromJSON = const . A.Success . const ()

