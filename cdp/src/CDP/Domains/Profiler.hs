{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



data ProfilerProfileNode = ProfilerProfileNode {
   profilerProfileNodeId :: Int,
   profilerProfileNodeCallFrame :: Runtime.RuntimeCallFrame,
   profilerProfileNodeHitCount :: Maybe Int,
   profilerProfileNodeChildren :: Maybe [Int],
   profilerProfileNodeDeoptReason :: Maybe String,
   profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data ProfilerProfile = ProfilerProfile {
   profilerProfileNodes :: [ProfilerProfileNode],
   profilerProfileStartTime :: Double,
   profilerProfileEndTime :: Double,
   profilerProfileSamples :: Maybe [Int],
   profilerProfileTimeDeltas :: Maybe [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  ProfilerProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
   profilerPositionTickInfoLine :: Int,
   profilerPositionTickInfoTicks :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerPositionTickInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerPositionTickInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data ProfilerCoverageRange = ProfilerCoverageRange {
   profilerCoverageRangeStartOffset :: Int,
   profilerCoverageRangeEndOffset :: Int,
   profilerCoverageRangeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerCoverageRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  ProfilerCoverageRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
   profilerFunctionCoverageFunctionName :: String,
   profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
   profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerFunctionCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  ProfilerFunctionCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data ProfilerScriptCoverage = ProfilerScriptCoverage {
   profilerScriptCoverageScriptId :: Runtime.RuntimeScriptId,
   profilerScriptCoverageUrl :: String,
   profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerScriptCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  ProfilerScriptCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
   profilerConsoleProfileFinishedId :: String,
   profilerConsoleProfileFinishedLocation :: Debugger.DebuggerLocation,
   profilerConsoleProfileFinishedProfile :: ProfilerProfile,
   profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
   profilerConsoleProfileStartedId :: String,
   profilerConsoleProfileStartedLocation :: Debugger.DebuggerLocation,
   profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON ProfilerConsoleProfileStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  ProfilerConsoleProfileStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }




profilerDisable :: Handle ev -> IO (Maybe Error)
profilerDisable handle = sendReceiveCommand handle "Profiler.disable" (Nothing :: Maybe ())


profilerEnable :: Handle ev -> IO (Maybe Error)
profilerEnable handle = sendReceiveCommand handle "Profiler.enable" (Nothing :: Maybe ())


profilerGetBestEffortCoverage :: Handle ev -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage handle = sendReceiveCommandResult handle "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
   profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerGetBestEffortCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command ProfilerGetBestEffortCoverage where
   commandName _ = "Profiler.getBestEffortCoverage"




data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
   pProfilerSetSamplingIntervalInterval :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerSetSamplingInterval  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PProfilerSetSamplingInterval where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


profilerSetSamplingInterval :: Handle ev -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval handle params = sendReceiveCommand handle "Profiler.setSamplingInterval" (Just params)


profilerStart :: Handle ev -> IO (Maybe Error)
profilerStart handle = sendReceiveCommand handle "Profiler.start" (Nothing :: Maybe ())



data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
   pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
   pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
   pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PProfilerStartPreciseCoverage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


profilerStartPreciseCoverage :: Handle ev -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage handle params = sendReceiveCommandResult handle "Profiler.startPreciseCoverage" (Just params)

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
   profilerStartPreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStartPreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command ProfilerStartPreciseCoverage where
   commandName _ = "Profiler.startPreciseCoverage"



profilerStop :: Handle ev -> IO (Either Error ProfilerStop)
profilerStop handle = sendReceiveCommandResult handle "Profiler.stop" (Nothing :: Maybe ())

data ProfilerStop = ProfilerStop {
   profilerStopProfile :: ProfilerProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerStop where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command ProfilerStop where
   commandName _ = "Profiler.stop"



profilerStopPreciseCoverage :: Handle ev -> IO (Maybe Error)
profilerStopPreciseCoverage handle = sendReceiveCommand handle "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())


profilerTakePreciseCoverage :: Handle ev -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage handle = sendReceiveCommandResult handle "Profiler.takePreciseCoverage" (Nothing :: Maybe ())

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
   profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
   profilerTakePreciseCoverageTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  ProfilerTakePreciseCoverage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command ProfilerTakePreciseCoverage where
   commandName _ = "Profiler.takePreciseCoverage"




