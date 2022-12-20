{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Profiler

-}


module CDP.Domains.Profiler (module CDP.Domains.Profiler) where

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


import CDP.Domains.Debugger as Debugger
import CDP.Domains.Runtime as Runtime


-- | Type 'Profiler.ProfileNode'.
--   Profile node. Holds callsite information, execution statistics and child nodes.
data ProfilerProfileNode = ProfilerProfileNode
  {
    -- | Unique id of the node.
    profilerProfileNodeId :: Int,
    -- | Function location.
    profilerProfileNodeCallFrame :: Runtime.RuntimeCallFrame,
    -- | Number of samples where this node was on top of the call stack.
    profilerProfileNodeHitCount :: Maybe Int,
    -- | Child node ids.
    profilerProfileNodeChildren :: Maybe [Int],
    -- | The reason of being not optimized. The function may be deoptimized or marked as don't
    --   optimize.
    profilerProfileNodeDeoptReason :: Maybe T.Text,
    -- | An array of source position ticks.
    profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
  }
  deriving (Eq, Show)
instance FromJSON ProfilerProfileNode where
  parseJSON = A.withObject "ProfilerProfileNode" $ \o -> ProfilerProfileNode
    <$> o A..: "id"
    <*> o A..: "callFrame"
    <*> o A..:? "hitCount"
    <*> o A..:? "children"
    <*> o A..:? "deoptReason"
    <*> o A..:? "positionTicks"
instance ToJSON ProfilerProfileNode where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (profilerProfileNodeId p),
    ("callFrame" A..=) <$> Just (profilerProfileNodeCallFrame p),
    ("hitCount" A..=) <$> (profilerProfileNodeHitCount p),
    ("children" A..=) <$> (profilerProfileNodeChildren p),
    ("deoptReason" A..=) <$> (profilerProfileNodeDeoptReason p),
    ("positionTicks" A..=) <$> (profilerProfileNodePositionTicks p)
    ]

-- | Type 'Profiler.Profile'.
--   Profile.
data ProfilerProfile = ProfilerProfile
  {
    -- | The list of profile nodes. First item is the root node.
    profilerProfileNodes :: [ProfilerProfileNode],
    -- | Profiling start timestamp in microseconds.
    profilerProfileStartTime :: Double,
    -- | Profiling end timestamp in microseconds.
    profilerProfileEndTime :: Double,
    -- | Ids of samples top nodes.
    profilerProfileSamples :: Maybe [Int],
    -- | Time intervals between adjacent samples in microseconds. The first delta is relative to the
    --   profile startTime.
    profilerProfileTimeDeltas :: Maybe [Int]
  }
  deriving (Eq, Show)
instance FromJSON ProfilerProfile where
  parseJSON = A.withObject "ProfilerProfile" $ \o -> ProfilerProfile
    <$> o A..: "nodes"
    <*> o A..: "startTime"
    <*> o A..: "endTime"
    <*> o A..:? "samples"
    <*> o A..:? "timeDeltas"
instance ToJSON ProfilerProfile where
  toJSON p = A.object $ catMaybes [
    ("nodes" A..=) <$> Just (profilerProfileNodes p),
    ("startTime" A..=) <$> Just (profilerProfileStartTime p),
    ("endTime" A..=) <$> Just (profilerProfileEndTime p),
    ("samples" A..=) <$> (profilerProfileSamples p),
    ("timeDeltas" A..=) <$> (profilerProfileTimeDeltas p)
    ]

-- | Type 'Profiler.PositionTickInfo'.
--   Specifies a number of samples attributed to a certain source position.
data ProfilerPositionTickInfo = ProfilerPositionTickInfo
  {
    -- | Source line number (1-based).
    profilerPositionTickInfoLine :: Int,
    -- | Number of samples attributed to the source line.
    profilerPositionTickInfoTicks :: Int
  }
  deriving (Eq, Show)
instance FromJSON ProfilerPositionTickInfo where
  parseJSON = A.withObject "ProfilerPositionTickInfo" $ \o -> ProfilerPositionTickInfo
    <$> o A..: "line"
    <*> o A..: "ticks"
instance ToJSON ProfilerPositionTickInfo where
  toJSON p = A.object $ catMaybes [
    ("line" A..=) <$> Just (profilerPositionTickInfoLine p),
    ("ticks" A..=) <$> Just (profilerPositionTickInfoTicks p)
    ]

-- | Type 'Profiler.CoverageRange'.
--   Coverage data for a source range.
data ProfilerCoverageRange = ProfilerCoverageRange
  {
    -- | JavaScript script source offset for the range start.
    profilerCoverageRangeStartOffset :: Int,
    -- | JavaScript script source offset for the range end.
    profilerCoverageRangeEndOffset :: Int,
    -- | Collected execution count of the source range.
    profilerCoverageRangeCount :: Int
  }
  deriving (Eq, Show)
instance FromJSON ProfilerCoverageRange where
  parseJSON = A.withObject "ProfilerCoverageRange" $ \o -> ProfilerCoverageRange
    <$> o A..: "startOffset"
    <*> o A..: "endOffset"
    <*> o A..: "count"
instance ToJSON ProfilerCoverageRange where
  toJSON p = A.object $ catMaybes [
    ("startOffset" A..=) <$> Just (profilerCoverageRangeStartOffset p),
    ("endOffset" A..=) <$> Just (profilerCoverageRangeEndOffset p),
    ("count" A..=) <$> Just (profilerCoverageRangeCount p)
    ]

-- | Type 'Profiler.FunctionCoverage'.
--   Coverage data for a JavaScript function.
data ProfilerFunctionCoverage = ProfilerFunctionCoverage
  {
    -- | JavaScript function name.
    profilerFunctionCoverageFunctionName :: T.Text,
    -- | Source ranges inside the function with coverage data.
    profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
    -- | Whether coverage data for this function has block granularity.
    profilerFunctionCoverageIsBlockCoverage :: Bool
  }
  deriving (Eq, Show)
instance FromJSON ProfilerFunctionCoverage where
  parseJSON = A.withObject "ProfilerFunctionCoverage" $ \o -> ProfilerFunctionCoverage
    <$> o A..: "functionName"
    <*> o A..: "ranges"
    <*> o A..: "isBlockCoverage"
instance ToJSON ProfilerFunctionCoverage where
  toJSON p = A.object $ catMaybes [
    ("functionName" A..=) <$> Just (profilerFunctionCoverageFunctionName p),
    ("ranges" A..=) <$> Just (profilerFunctionCoverageRanges p),
    ("isBlockCoverage" A..=) <$> Just (profilerFunctionCoverageIsBlockCoverage p)
    ]

-- | Type 'Profiler.ScriptCoverage'.
--   Coverage data for a JavaScript script.
data ProfilerScriptCoverage = ProfilerScriptCoverage
  {
    -- | JavaScript script id.
    profilerScriptCoverageScriptId :: Runtime.RuntimeScriptId,
    -- | JavaScript script name or url.
    profilerScriptCoverageUrl :: T.Text,
    -- | Functions contained in the script that has coverage data.
    profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
  }
  deriving (Eq, Show)
instance FromJSON ProfilerScriptCoverage where
  parseJSON = A.withObject "ProfilerScriptCoverage" $ \o -> ProfilerScriptCoverage
    <$> o A..: "scriptId"
    <*> o A..: "url"
    <*> o A..: "functions"
instance ToJSON ProfilerScriptCoverage where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (profilerScriptCoverageScriptId p),
    ("url" A..=) <$> Just (profilerScriptCoverageUrl p),
    ("functions" A..=) <$> Just (profilerScriptCoverageFunctions p)
    ]

-- | Type of the 'Profiler.consoleProfileFinished' event.
data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished
  {
    profilerConsoleProfileFinishedId :: T.Text,
    -- | Location of console.profileEnd().
    profilerConsoleProfileFinishedLocation :: Debugger.DebuggerLocation,
    profilerConsoleProfileFinishedProfile :: ProfilerProfile,
    -- | Profile title passed as an argument to console.profile().
    profilerConsoleProfileFinishedTitle :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON ProfilerConsoleProfileFinished where
  parseJSON = A.withObject "ProfilerConsoleProfileFinished" $ \o -> ProfilerConsoleProfileFinished
    <$> o A..: "id"
    <*> o A..: "location"
    <*> o A..: "profile"
    <*> o A..:? "title"
instance Event ProfilerConsoleProfileFinished where
  eventName _ = "Profiler.consoleProfileFinished"

-- | Type of the 'Profiler.consoleProfileStarted' event.
data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted
  {
    profilerConsoleProfileStartedId :: T.Text,
    -- | Location of console.profile().
    profilerConsoleProfileStartedLocation :: Debugger.DebuggerLocation,
    -- | Profile title passed as an argument to console.profile().
    profilerConsoleProfileStartedTitle :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON ProfilerConsoleProfileStarted where
  parseJSON = A.withObject "ProfilerConsoleProfileStarted" $ \o -> ProfilerConsoleProfileStarted
    <$> o A..: "id"
    <*> o A..: "location"
    <*> o A..:? "title"
instance Event ProfilerConsoleProfileStarted where
  eventName _ = "Profiler.consoleProfileStarted"

-- | Type of the 'Profiler.preciseCoverageDeltaUpdate' event.
data ProfilerPreciseCoverageDeltaUpdate = ProfilerPreciseCoverageDeltaUpdate
  {
    -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
    profilerPreciseCoverageDeltaUpdateTimestamp :: Double,
    -- | Identifier for distinguishing coverage events.
    profilerPreciseCoverageDeltaUpdateOccasion :: T.Text,
    -- | Coverage data for the current isolate.
    profilerPreciseCoverageDeltaUpdateResult :: [ProfilerScriptCoverage]
  }
  deriving (Eq, Show)
instance FromJSON ProfilerPreciseCoverageDeltaUpdate where
  parseJSON = A.withObject "ProfilerPreciseCoverageDeltaUpdate" $ \o -> ProfilerPreciseCoverageDeltaUpdate
    <$> o A..: "timestamp"
    <*> o A..: "occasion"
    <*> o A..: "result"
instance Event ProfilerPreciseCoverageDeltaUpdate where
  eventName _ = "Profiler.preciseCoverageDeltaUpdate"


-- | Parameters of the 'Profiler.disable' command.
data PProfilerDisable = PProfilerDisable
  deriving (Eq, Show)
pProfilerDisable
  :: PProfilerDisable
pProfilerDisable
  = PProfilerDisable
instance ToJSON PProfilerDisable where
  toJSON _ = A.Null
instance Command PProfilerDisable where
  type CommandResponse PProfilerDisable = ()
  commandName _ = "Profiler.disable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Profiler.enable' command.
data PProfilerEnable = PProfilerEnable
  deriving (Eq, Show)
pProfilerEnable
  :: PProfilerEnable
pProfilerEnable
  = PProfilerEnable
instance ToJSON PProfilerEnable where
  toJSON _ = A.Null
instance Command PProfilerEnable where
  type CommandResponse PProfilerEnable = ()
  commandName _ = "Profiler.enable"
  fromJSON = const . A.Success . const ()

-- | Collect coverage data for the current isolate. The coverage data may be incomplete due to
--   garbage collection.

-- | Parameters of the 'Profiler.getBestEffortCoverage' command.
data PProfilerGetBestEffortCoverage = PProfilerGetBestEffortCoverage
  deriving (Eq, Show)
pProfilerGetBestEffortCoverage
  :: PProfilerGetBestEffortCoverage
pProfilerGetBestEffortCoverage
  = PProfilerGetBestEffortCoverage
instance ToJSON PProfilerGetBestEffortCoverage where
  toJSON _ = A.Null
data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage
  {
    -- | Coverage data for the current isolate.
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
  }
  deriving (Eq, Show)
instance FromJSON ProfilerGetBestEffortCoverage where
  parseJSON = A.withObject "ProfilerGetBestEffortCoverage" $ \o -> ProfilerGetBestEffortCoverage
    <$> o A..: "result"
instance Command PProfilerGetBestEffortCoverage where
  type CommandResponse PProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage
  commandName _ = "Profiler.getBestEffortCoverage"

-- | Changes CPU profiler sampling interval. Must be called before CPU profiles recording started.

-- | Parameters of the 'Profiler.setSamplingInterval' command.
data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval
  {
    -- | New sampling interval in microseconds.
    pProfilerSetSamplingIntervalInterval :: Int
  }
  deriving (Eq, Show)
pProfilerSetSamplingInterval
  {-
  -- | New sampling interval in microseconds.
  -}
  :: Int
  -> PProfilerSetSamplingInterval
pProfilerSetSamplingInterval
  arg_pProfilerSetSamplingIntervalInterval
  = PProfilerSetSamplingInterval
    arg_pProfilerSetSamplingIntervalInterval
instance ToJSON PProfilerSetSamplingInterval where
  toJSON p = A.object $ catMaybes [
    ("interval" A..=) <$> Just (pProfilerSetSamplingIntervalInterval p)
    ]
instance Command PProfilerSetSamplingInterval where
  type CommandResponse PProfilerSetSamplingInterval = ()
  commandName _ = "Profiler.setSamplingInterval"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Profiler.start' command.
data PProfilerStart = PProfilerStart
  deriving (Eq, Show)
pProfilerStart
  :: PProfilerStart
pProfilerStart
  = PProfilerStart
instance ToJSON PProfilerStart where
  toJSON _ = A.Null
instance Command PProfilerStart where
  type CommandResponse PProfilerStart = ()
  commandName _ = "Profiler.start"
  fromJSON = const . A.Success . const ()

-- | Enable precise code coverage. Coverage data for JavaScript executed before enabling precise code
--   coverage may be incomplete. Enabling prevents running optimized code and resets execution
--   counters.

-- | Parameters of the 'Profiler.startPreciseCoverage' command.
data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage
  {
    -- | Collect accurate call counts beyond simple 'covered' or 'not covered'.
    pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
    -- | Collect block-based coverage.
    pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
    -- | Allow the backend to send updates on its own initiative
    pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
  }
  deriving (Eq, Show)
pProfilerStartPreciseCoverage
  :: PProfilerStartPreciseCoverage
pProfilerStartPreciseCoverage
  = PProfilerStartPreciseCoverage
    Nothing
    Nothing
    Nothing
instance ToJSON PProfilerStartPreciseCoverage where
  toJSON p = A.object $ catMaybes [
    ("callCount" A..=) <$> (pProfilerStartPreciseCoverageCallCount p),
    ("detailed" A..=) <$> (pProfilerStartPreciseCoverageDetailed p),
    ("allowTriggeredUpdates" A..=) <$> (pProfilerStartPreciseCoverageAllowTriggeredUpdates p)
    ]
data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage
  {
    -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
    profilerStartPreciseCoverageTimestamp :: Double
  }
  deriving (Eq, Show)
instance FromJSON ProfilerStartPreciseCoverage where
  parseJSON = A.withObject "ProfilerStartPreciseCoverage" $ \o -> ProfilerStartPreciseCoverage
    <$> o A..: "timestamp"
instance Command PProfilerStartPreciseCoverage where
  type CommandResponse PProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage
  commandName _ = "Profiler.startPreciseCoverage"


-- | Parameters of the 'Profiler.stop' command.
data PProfilerStop = PProfilerStop
  deriving (Eq, Show)
pProfilerStop
  :: PProfilerStop
pProfilerStop
  = PProfilerStop
instance ToJSON PProfilerStop where
  toJSON _ = A.Null
data ProfilerStop = ProfilerStop
  {
    -- | Recorded profile.
    profilerStopProfile :: ProfilerProfile
  }
  deriving (Eq, Show)
instance FromJSON ProfilerStop where
  parseJSON = A.withObject "ProfilerStop" $ \o -> ProfilerStop
    <$> o A..: "profile"
instance Command PProfilerStop where
  type CommandResponse PProfilerStop = ProfilerStop
  commandName _ = "Profiler.stop"

-- | Disable precise code coverage. Disabling releases unnecessary execution count records and allows
--   executing optimized code.

-- | Parameters of the 'Profiler.stopPreciseCoverage' command.
data PProfilerStopPreciseCoverage = PProfilerStopPreciseCoverage
  deriving (Eq, Show)
pProfilerStopPreciseCoverage
  :: PProfilerStopPreciseCoverage
pProfilerStopPreciseCoverage
  = PProfilerStopPreciseCoverage
instance ToJSON PProfilerStopPreciseCoverage where
  toJSON _ = A.Null
instance Command PProfilerStopPreciseCoverage where
  type CommandResponse PProfilerStopPreciseCoverage = ()
  commandName _ = "Profiler.stopPreciseCoverage"
  fromJSON = const . A.Success . const ()

-- | Collect coverage data for the current isolate, and resets execution counters. Precise code
--   coverage needs to have started.

-- | Parameters of the 'Profiler.takePreciseCoverage' command.
data PProfilerTakePreciseCoverage = PProfilerTakePreciseCoverage
  deriving (Eq, Show)
pProfilerTakePreciseCoverage
  :: PProfilerTakePreciseCoverage
pProfilerTakePreciseCoverage
  = PProfilerTakePreciseCoverage
instance ToJSON PProfilerTakePreciseCoverage where
  toJSON _ = A.Null
data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage
  {
    -- | Coverage data for the current isolate.
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
    profilerTakePreciseCoverageTimestamp :: Double
  }
  deriving (Eq, Show)
instance FromJSON ProfilerTakePreciseCoverage where
  parseJSON = A.withObject "ProfilerTakePreciseCoverage" $ \o -> ProfilerTakePreciseCoverage
    <$> o A..: "result"
    <*> o A..: "timestamp"
instance Command PProfilerTakePreciseCoverage where
  type CommandResponse PProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage
  commandName _ = "Profiler.takePreciseCoverage"

