{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.Debugger as Debugger
import CDP.Domains.Runtime as Runtime


-- | Profile node. Holds callsite information, execution statistics and child nodes.
data ProfilerProfileNode = ProfilerProfileNode {
   profilerProfileNodeId :: ProfilerProfileNodeId, -- ^ Unique id of the node.
   profilerProfileNodeCallFrame :: ProfilerProfileNodeCallFrame, -- ^ Function location.
   profilerProfileNodeHitCount :: ProfilerProfileNodeHitCount, -- ^ Number of samples where this node was on top of the call stack.
   profilerProfileNodeChildren :: ProfilerProfileNodeChildren, -- ^ Child node ids.
   profilerProfileNodeDeoptReason :: ProfilerProfileNodeDeoptReason, -- ^ The reason of being not optimized. The function may be deoptimized or marked as don't
optimize.
   profilerProfileNodePositionTicks :: ProfilerProfileNodePositionTicks -- ^ An array of source position ticks.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Profile.
data ProfilerProfile = ProfilerProfile {
   profilerProfileNodes :: ProfilerProfileNodes, -- ^ The list of profile nodes. First item is the root node.
   profilerProfileStartTime :: ProfilerProfileStartTime, -- ^ Profiling start timestamp in microseconds.
   profilerProfileEndTime :: ProfilerProfileEndTime, -- ^ Profiling end timestamp in microseconds.
   profilerProfileSamples :: ProfilerProfileSamples, -- ^ Ids of samples top nodes.
   profilerProfileTimeDeltas :: ProfilerProfileTimeDeltas -- ^ Time intervals between adjacent samples in microseconds. The first delta is relative to the
profile startTime.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Specifies a number of samples attributed to a certain source position.
data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
   profilerPositionTickInfoLine :: ProfilerPositionTickInfoLine, -- ^ Source line number (1-based).
   profilerPositionTickInfoTicks :: ProfilerPositionTickInfoTicks -- ^ Number of samples attributed to the source line.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPositionTickInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerPositionTickInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Coverage data for a source range.
data ProfilerCoverageRange = ProfilerCoverageRange {
   profilerCoverageRangeStartOffset :: ProfilerCoverageRangeStartOffset, -- ^ JavaScript script source offset for the range start.
   profilerCoverageRangeEndOffset :: ProfilerCoverageRangeEndOffset, -- ^ JavaScript script source offset for the range end.
   profilerCoverageRangeCount :: ProfilerCoverageRangeCount -- ^ Collected execution count of the source range.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerCoverageRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  ProfilerCoverageRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Coverage data for a JavaScript function.
data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
   profilerFunctionCoverageFunctionName :: ProfilerFunctionCoverageFunctionName, -- ^ JavaScript function name.
   profilerFunctionCoverageRanges :: ProfilerFunctionCoverageRanges, -- ^ Source ranges inside the function with coverage data.
   profilerFunctionCoverageIsBlockCoverage :: ProfilerFunctionCoverageIsBlockCoverage -- ^ Whether coverage data for this function has block granularity.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerFunctionCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerFunctionCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Coverage data for a JavaScript script.
data ProfilerScriptCoverage = ProfilerScriptCoverage {
   profilerScriptCoverageScriptId :: ProfilerScriptCoverageScriptId, -- ^ JavaScript script id.
   profilerScriptCoverageUrl :: ProfilerScriptCoverageUrl, -- ^ JavaScript script name or url.
   profilerScriptCoverageFunctions :: ProfilerScriptCoverageFunctions -- ^ Functions contained in the script that has coverage data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Describes a type collected during runtime.
data ProfilerTypeObject = ProfilerTypeObject {
   profilerTypeObjectName :: ProfilerTypeObjectName -- ^ Name of a type collected with type profiling.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerTypeObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  ProfilerTypeObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Source offset and types for a parameter or return value.
data ProfilerTypeProfileEntry = ProfilerTypeProfileEntry {
   profilerTypeProfileEntryOffset :: ProfilerTypeProfileEntryOffset, -- ^ Source offset of the parameter or end of function for return values.
   profilerTypeProfileEntryTypes :: ProfilerTypeProfileEntryTypes -- ^ The types for this parameter or return value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerTypeProfileEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerTypeProfileEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type profile data collected during runtime for a JavaScript script.
data ProfilerScriptTypeProfile = ProfilerScriptTypeProfile {
   profilerScriptTypeProfileScriptId :: ProfilerScriptTypeProfileScriptId, -- ^ JavaScript script id.
   profilerScriptTypeProfileUrl :: ProfilerScriptTypeProfileUrl, -- ^ JavaScript script name or url.
   profilerScriptTypeProfileEntries :: ProfilerScriptTypeProfileEntries -- ^ Type profile entries for parameters and return values of the functions in the script.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptTypeProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptTypeProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }





-- | Type of the 'Profiler.consoleProfileFinished' event.
data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {

   profilerConsoleProfileFinishedLocation :: ProfilerConsoleProfileFinishedLocation, -- ^ Location of console.profileEnd().

   profilerConsoleProfileFinishedTitle :: ProfilerConsoleProfileFinishedTitle -- ^ Profile title passed as an argument to console.profile().
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Profiler.consoleProfileStarted' event.
data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {

   profilerConsoleProfileStartedLocation :: ProfilerConsoleProfileStartedLocation, -- ^ Location of console.profile().
   profilerConsoleProfileStartedTitle :: ProfilerConsoleProfileStartedTitle -- ^ Profile title passed as an argument to console.profile().
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Profiler.preciseCoverageDeltaUpdate' event.
data ProfilerPreciseCoverageDeltaUpdate = ProfilerPreciseCoverageDeltaUpdate {
   profilerPreciseCoverageDeltaUpdateTimestamp :: ProfilerPreciseCoverageDeltaUpdateTimestamp, -- ^ Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
   profilerPreciseCoverageDeltaUpdateOccasion :: ProfilerPreciseCoverageDeltaUpdateOccasion, -- ^ Identifier for distinguishing coverage events.
   profilerPreciseCoverageDeltaUpdateResult :: ProfilerPreciseCoverageDeltaUpdateResult -- ^ Coverage data for the current isolate.
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPreciseCoverageDeltaUpdate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  ProfilerPreciseCoverageDeltaUpdate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }





-- | Function for the command 'Profiler.disable'.
profilerDisable :: Handle ev -> IO (Maybe Error)
profilerDisable handle = sendReceiveCommand handle "Profiler.disable" (Nothing :: Maybe ())


-- | Function for the command 'Profiler.enable'.
profilerEnable :: Handle ev -> IO (Maybe Error)
profilerEnable handle = sendReceiveCommand handle "Profiler.enable" (Nothing :: Maybe ())


-- | Function for the command 'Profiler.getBestEffortCoverage'.
-- Collect coverage data for the current isolate. The coverage data may be incomplete due to
-- garbage collection.
-- Returns: 'ProfilerGetBestEffortCoverage'
profilerGetBestEffortCoverage :: Handle ev -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage handle = sendReceiveCommandResult handle "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())

-- | Return type of the 'profilerGetBestEffortCoverage' command.
data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
   profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage] -- ^ Coverage data for the current isolate.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerGetBestEffortCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command ProfilerGetBestEffortCoverage where
   commandName _ = "Profiler.getBestEffortCoverage"



-- | Parameters of the 'profilerSetSamplingInterval' command.
data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
   pProfilerSetSamplingIntervalInterval :: PProfilerSetSamplingIntervalInterval -- ^ New sampling interval in microseconds.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerSetSamplingInterval  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PProfilerSetSamplingInterval where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Profiler.setSamplingInterval'.
-- Changes CPU profiler sampling interval. Must be called before CPU profiles recording started.
-- Parameters: 'PProfilerSetSamplingInterval'
profilerSetSamplingInterval :: Handle ev -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval handle params = sendReceiveCommand handle "Profiler.setSamplingInterval" (Just params)


-- | Function for the command 'Profiler.start'.
profilerStart :: Handle ev -> IO (Maybe Error)
profilerStart handle = sendReceiveCommand handle "Profiler.start" (Nothing :: Maybe ())


-- | Parameters of the 'profilerStartPreciseCoverage' command.
data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
   pProfilerStartPreciseCoverageCallCount :: PProfilerStartPreciseCoverageCallCount, -- ^ Collect accurate call counts beyond simple 'covered' or 'not covered'.
   pProfilerStartPreciseCoverageDetailed :: PProfilerStartPreciseCoverageDetailed, -- ^ Collect block-based coverage.
   pProfilerStartPreciseCoverageAllowTriggeredUpdates :: PProfilerStartPreciseCoverageAllowTriggeredUpdates -- ^ Allow the backend to send updates on its own initiative
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerStartPreciseCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Profiler.startPreciseCoverage'.
-- Enable precise code coverage. Coverage data for JavaScript executed before enabling precise code
-- coverage may be incomplete. Enabling prevents running optimized code and resets execution
-- counters.
-- Parameters: 'PProfilerStartPreciseCoverage'
-- Returns: 'ProfilerStartPreciseCoverage'
profilerStartPreciseCoverage :: Handle ev -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage handle params = sendReceiveCommandResult handle "Profiler.startPreciseCoverage" (Just params)

-- | Return type of the 'profilerStartPreciseCoverage' command.
data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
   profilerStartPreciseCoverageTimestamp :: Double -- ^ Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command ProfilerStartPreciseCoverage where
   commandName _ = "Profiler.startPreciseCoverage"



-- | Function for the command 'Profiler.startTypeProfile'.
-- Enable type profile.
profilerStartTypeProfile :: Handle ev -> IO (Maybe Error)
profilerStartTypeProfile handle = sendReceiveCommand handle "Profiler.startTypeProfile" (Nothing :: Maybe ())


-- | Function for the command 'Profiler.stop'.
-- Returns: 'ProfilerStop'
profilerStop :: Handle ev -> IO (Either Error ProfilerStop)
profilerStop handle = sendReceiveCommandResult handle "Profiler.stop" (Nothing :: Maybe ())

-- | Return type of the 'profilerStop' command.
data ProfilerStop = ProfilerStop {
   profilerStopProfile :: ProfilerProfile -- ^ Recorded profile.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStop where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command ProfilerStop where
   commandName _ = "Profiler.stop"



-- | Function for the command 'Profiler.stopPreciseCoverage'.
-- Disable precise code coverage. Disabling releases unnecessary execution count records and allows
-- executing optimized code.
profilerStopPreciseCoverage :: Handle ev -> IO (Maybe Error)
profilerStopPreciseCoverage handle = sendReceiveCommand handle "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())


-- | Function for the command 'Profiler.stopTypeProfile'.
-- Disable type profile. Disabling releases type profile data collected so far.
profilerStopTypeProfile :: Handle ev -> IO (Maybe Error)
profilerStopTypeProfile handle = sendReceiveCommand handle "Profiler.stopTypeProfile" (Nothing :: Maybe ())


-- | Function for the command 'Profiler.takePreciseCoverage'.
-- Collect coverage data for the current isolate, and resets execution counters. Precise code
-- coverage needs to have started.
-- Returns: 'ProfilerTakePreciseCoverage'
profilerTakePreciseCoverage :: Handle ev -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage handle = sendReceiveCommandResult handle "Profiler.takePreciseCoverage" (Nothing :: Maybe ())

-- | Return type of the 'profilerTakePreciseCoverage' command.
data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
   profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage], -- ^ Coverage data for the current isolate.
   profilerTakePreciseCoverageTimestamp :: Double -- ^ Monotonically increasing time (in seconds) when the coverage update was taken in the backend.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakePreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command ProfilerTakePreciseCoverage where
   commandName _ = "Profiler.takePreciseCoverage"



-- | Function for the command 'Profiler.takeTypeProfile'.
-- Collect type profile.
-- Returns: 'ProfilerTakeTypeProfile'
profilerTakeTypeProfile :: Handle ev -> IO (Either Error ProfilerTakeTypeProfile)
profilerTakeTypeProfile handle = sendReceiveCommandResult handle "Profiler.takeTypeProfile" (Nothing :: Maybe ())

-- | Return type of the 'profilerTakeTypeProfile' command.
data ProfilerTakeTypeProfile = ProfilerTakeTypeProfile {
   profilerTakeTypeProfileResult :: [ProfilerScriptTypeProfile] -- ^ Type profile for all scripts since startTypeProfile() was turned on.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakeTypeProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command ProfilerTakeTypeProfile where
   commandName _ = "Profiler.takeTypeProfile"




