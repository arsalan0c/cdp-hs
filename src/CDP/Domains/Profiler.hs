{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Profiler 
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
data ProfilerProfileNode = ProfilerProfileNode {
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
  profilerProfileNodeDeoptReason :: Maybe String,
  -- | An array of source position ticks.
  profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Profiler.Profile'.
--   Profile.
data ProfilerProfile = ProfilerProfile {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'Profiler.PositionTickInfo'.
--   Specifies a number of samples attributed to a certain source position.
data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
  -- | Source line number (1-based).
  profilerPositionTickInfoLine :: Int,
  -- | Number of samples attributed to the source line.
  profilerPositionTickInfoTicks :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPositionTickInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerPositionTickInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Profiler.CoverageRange'.
--   Coverage data for a source range.
data ProfilerCoverageRange = ProfilerCoverageRange {
  -- | JavaScript script source offset for the range start.
  profilerCoverageRangeStartOffset :: Int,
  -- | JavaScript script source offset for the range end.
  profilerCoverageRangeEndOffset :: Int,
  -- | Collected execution count of the source range.
  profilerCoverageRangeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerCoverageRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  ProfilerCoverageRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Profiler.FunctionCoverage'.
--   Coverage data for a JavaScript function.
data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
  -- | JavaScript function name.
  profilerFunctionCoverageFunctionName :: String,
  -- | Source ranges inside the function with coverage data.
  profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
  -- | Whether coverage data for this function has block granularity.
  profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerFunctionCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerFunctionCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Profiler.ScriptCoverage'.
--   Coverage data for a JavaScript script.
data ProfilerScriptCoverage = ProfilerScriptCoverage {
  -- | JavaScript script id.
  profilerScriptCoverageScriptId :: Runtime.RuntimeScriptId,
  -- | JavaScript script name or url.
  profilerScriptCoverageUrl :: String,
  -- | Functions contained in the script that has coverage data.
  profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Profiler.TypeObject'.
--   Describes a type collected during runtime.
data ProfilerTypeObject = ProfilerTypeObject {
  -- | Name of a type collected with type profiling.
  profilerTypeObjectName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerTypeObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  ProfilerTypeObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Profiler.TypeProfileEntry'.
--   Source offset and types for a parameter or return value.
data ProfilerTypeProfileEntry = ProfilerTypeProfileEntry {
  -- | Source offset of the parameter or end of function for return values.
  profilerTypeProfileEntryOffset :: Int,
  -- | The types for this parameter or return value.
  profilerTypeProfileEntryTypes :: [ProfilerTypeObject]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerTypeProfileEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerTypeProfileEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Profiler.ScriptTypeProfile'.
--   Type profile data collected during runtime for a JavaScript script.
data ProfilerScriptTypeProfile = ProfilerScriptTypeProfile {
  -- | JavaScript script id.
  profilerScriptTypeProfileScriptId :: Runtime.RuntimeScriptId,
  -- | JavaScript script name or url.
  profilerScriptTypeProfileUrl :: String,
  -- | Type profile entries for parameters and return values of the functions in the script.
  profilerScriptTypeProfileEntries :: [ProfilerTypeProfileEntry]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptTypeProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptTypeProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }





-- | Type of the 'Profiler.consoleProfileFinished' event.
data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
  profilerConsoleProfileFinishedId :: String,
  -- | Location of console.profileEnd().
  profilerConsoleProfileFinishedLocation :: Debugger.DebuggerLocation,
  profilerConsoleProfileFinishedProfile :: ProfilerProfile,
  -- | Profile title passed as an argument to console.profile().
  profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Event ProfilerConsoleProfileFinished where
    eventName _ = "Profiler.consoleProfileFinished"

-- | Type of the 'Profiler.consoleProfileStarted' event.
data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
  profilerConsoleProfileStartedId :: String,
  -- | Location of console.profile().
  profilerConsoleProfileStartedLocation :: Debugger.DebuggerLocation,
  -- | Profile title passed as an argument to console.profile().
  profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Event ProfilerConsoleProfileStarted where
    eventName _ = "Profiler.consoleProfileStarted"

-- | Type of the 'Profiler.preciseCoverageDeltaUpdate' event.
data ProfilerPreciseCoverageDeltaUpdate = ProfilerPreciseCoverageDeltaUpdate {
  -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
  profilerPreciseCoverageDeltaUpdateTimestamp :: Double,
  -- | Identifier for distinguishing coverage events.
  profilerPreciseCoverageDeltaUpdateOccasion :: String,
  -- | Coverage data for the current isolate.
  profilerPreciseCoverageDeltaUpdateResult :: [ProfilerScriptCoverage]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPreciseCoverageDeltaUpdate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  ProfilerPreciseCoverageDeltaUpdate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Event ProfilerPreciseCoverageDeltaUpdate where
    eventName _ = "Profiler.preciseCoverageDeltaUpdate"



-- | Profiler.disable

-- | Parameters of the 'Profiler.disable' command.
data PProfilerDisable = PProfilerDisable
instance ToJSON PProfilerDisable where toJSON _ = A.Null

instance Command PProfilerDisable where
   type CommandResponse PProfilerDisable = ()
   commandName _ = "Profiler.disable"
   fromJSON = const . A.Success . const ()


-- | Profiler.enable

-- | Parameters of the 'Profiler.enable' command.
data PProfilerEnable = PProfilerEnable
instance ToJSON PProfilerEnable where toJSON _ = A.Null

instance Command PProfilerEnable where
   type CommandResponse PProfilerEnable = ()
   commandName _ = "Profiler.enable"
   fromJSON = const . A.Success . const ()


-- | Profiler.getBestEffortCoverage
--   Collect coverage data for the current isolate. The coverage data may be incomplete due to
--   garbage collection.

-- | Parameters of the 'Profiler.getBestEffortCoverage' command.
data PProfilerGetBestEffortCoverage = PProfilerGetBestEffortCoverage
instance ToJSON PProfilerGetBestEffortCoverage where toJSON _ = A.Null

-- | Return type of the 'Profiler.getBestEffortCoverage' command.
data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
  -- | Coverage data for the current isolate.
  profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerGetBestEffortCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PProfilerGetBestEffortCoverage where
   type CommandResponse PProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage
   commandName _ = "Profiler.getBestEffortCoverage"



-- | Profiler.setSamplingInterval
--   Changes CPU profiler sampling interval. Must be called before CPU profiles recording started.

-- | Parameters of the 'Profiler.setSamplingInterval' command.
data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
  -- | New sampling interval in microseconds.
  pProfilerSetSamplingIntervalInterval :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerSetSamplingInterval  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PProfilerSetSamplingInterval where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PProfilerSetSamplingInterval where
   type CommandResponse PProfilerSetSamplingInterval = ()
   commandName _ = "Profiler.setSamplingInterval"
   fromJSON = const . A.Success . const ()


-- | Profiler.start

-- | Parameters of the 'Profiler.start' command.
data PProfilerStart = PProfilerStart
instance ToJSON PProfilerStart where toJSON _ = A.Null

instance Command PProfilerStart where
   type CommandResponse PProfilerStart = ()
   commandName _ = "Profiler.start"
   fromJSON = const . A.Success . const ()


-- | Profiler.startPreciseCoverage
--   Enable precise code coverage. Coverage data for JavaScript executed before enabling precise code
--   coverage may be incomplete. Enabling prevents running optimized code and resets execution
--   counters.

-- | Parameters of the 'Profiler.startPreciseCoverage' command.
data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
  -- | Collect accurate call counts beyond simple 'covered' or 'not covered'.
  pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
  -- | Collect block-based coverage.
  pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
  -- | Allow the backend to send updates on its own initiative
  pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerStartPreciseCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Return type of the 'Profiler.startPreciseCoverage' command.
data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
  -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
  profilerStartPreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command PProfilerStartPreciseCoverage where
   type CommandResponse PProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage
   commandName _ = "Profiler.startPreciseCoverage"



-- | Profiler.startTypeProfile
--   Enable type profile.

-- | Parameters of the 'Profiler.startTypeProfile' command.
data PProfilerStartTypeProfile = PProfilerStartTypeProfile
instance ToJSON PProfilerStartTypeProfile where toJSON _ = A.Null

instance Command PProfilerStartTypeProfile where
   type CommandResponse PProfilerStartTypeProfile = ()
   commandName _ = "Profiler.startTypeProfile"
   fromJSON = const . A.Success . const ()


-- | Profiler.stop

-- | Parameters of the 'Profiler.stop' command.
data PProfilerStop = PProfilerStop
instance ToJSON PProfilerStop where toJSON _ = A.Null

-- | Return type of the 'Profiler.stop' command.
data ProfilerStop = ProfilerStop {
  -- | Recorded profile.
  profilerStopProfile :: ProfilerProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStop where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PProfilerStop where
   type CommandResponse PProfilerStop = ProfilerStop
   commandName _ = "Profiler.stop"



-- | Profiler.stopPreciseCoverage
--   Disable precise code coverage. Disabling releases unnecessary execution count records and allows
--   executing optimized code.

-- | Parameters of the 'Profiler.stopPreciseCoverage' command.
data PProfilerStopPreciseCoverage = PProfilerStopPreciseCoverage
instance ToJSON PProfilerStopPreciseCoverage where toJSON _ = A.Null

instance Command PProfilerStopPreciseCoverage where
   type CommandResponse PProfilerStopPreciseCoverage = ()
   commandName _ = "Profiler.stopPreciseCoverage"
   fromJSON = const . A.Success . const ()


-- | Profiler.stopTypeProfile
--   Disable type profile. Disabling releases type profile data collected so far.

-- | Parameters of the 'Profiler.stopTypeProfile' command.
data PProfilerStopTypeProfile = PProfilerStopTypeProfile
instance ToJSON PProfilerStopTypeProfile where toJSON _ = A.Null

instance Command PProfilerStopTypeProfile where
   type CommandResponse PProfilerStopTypeProfile = ()
   commandName _ = "Profiler.stopTypeProfile"
   fromJSON = const . A.Success . const ()


-- | Profiler.takePreciseCoverage
--   Collect coverage data for the current isolate, and resets execution counters. Precise code
--   coverage needs to have started.

-- | Parameters of the 'Profiler.takePreciseCoverage' command.
data PProfilerTakePreciseCoverage = PProfilerTakePreciseCoverage
instance ToJSON PProfilerTakePreciseCoverage where toJSON _ = A.Null

-- | Return type of the 'Profiler.takePreciseCoverage' command.
data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
  -- | Coverage data for the current isolate.
  profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
  -- | Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
  profilerTakePreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakePreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PProfilerTakePreciseCoverage where
   type CommandResponse PProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage
   commandName _ = "Profiler.takePreciseCoverage"



-- | Profiler.takeTypeProfile
--   Collect type profile.

-- | Parameters of the 'Profiler.takeTypeProfile' command.
data PProfilerTakeTypeProfile = PProfilerTakeTypeProfile
instance ToJSON PProfilerTakeTypeProfile where toJSON _ = A.Null

-- | Return type of the 'Profiler.takeTypeProfile' command.
data ProfilerTakeTypeProfile = ProfilerTakeTypeProfile {
  -- | Type profile for all scripts since startTypeProfile() was turned on.
  profilerTakeTypeProfileResult :: [ProfilerScriptTypeProfile]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakeTypeProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PProfilerTakeTypeProfile where
   type CommandResponse PProfilerTakeTypeProfile = ProfilerTakeTypeProfile
   commandName _ = "Profiler.takeTypeProfile"




