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

module Domains.Profiler (module Domains.Profiler) where

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

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


data ProfilerConsoleProfileFinished = ProfilerConsoleProfileFinished {
    profilerConsoleProfileFinishedId :: String,
    profilerConsoleProfileFinishedLocation :: DebuggerLocation,
    profilerConsoleProfileFinishedProfile :: ProfilerProfile,
    profilerConsoleProfileFinishedTitle :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerConsoleProfileFinished where
    parseJSON = A.withObject "ProfilerConsoleProfileFinished" $ \v ->
         ProfilerConsoleProfileFinished <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:  "profile"
            <*> v  .:?  "title"


instance ToJSON ProfilerConsoleProfileFinished  where
    toJSON v = A.object
        [ "id" .= profilerConsoleProfileFinishedId v
        , "location" .= profilerConsoleProfileFinishedLocation v
        , "profile" .= profilerConsoleProfileFinishedProfile v
        , "title" .= profilerConsoleProfileFinishedTitle v
        ]


instance FromEvent Event ProfilerConsoleProfileFinished where
    eventName  _ _    =  "Profiler.consoleProfileFinished"
    fromEvent ev =  case ev of EVProfilerConsoleProfileFinished v -> Just v; _ -> Nothing

data ProfilerConsoleProfileStarted = ProfilerConsoleProfileStarted {
    profilerConsoleProfileStartedId :: String,
    profilerConsoleProfileStartedLocation :: DebuggerLocation,
    profilerConsoleProfileStartedTitle :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerConsoleProfileStarted where
    parseJSON = A.withObject "ProfilerConsoleProfileStarted" $ \v ->
         ProfilerConsoleProfileStarted <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:?  "title"


instance ToJSON ProfilerConsoleProfileStarted  where
    toJSON v = A.object
        [ "id" .= profilerConsoleProfileStartedId v
        , "location" .= profilerConsoleProfileStartedLocation v
        , "title" .= profilerConsoleProfileStartedTitle v
        ]


instance FromEvent Event ProfilerConsoleProfileStarted where
    eventName  _ _    =  "Profiler.consoleProfileStarted"
    fromEvent ev =  case ev of EVProfilerConsoleProfileStarted v -> Just v; _ -> Nothing



data ProfilerProfileNode = ProfilerProfileNode {
    profilerProfileNodeId :: Int,
    profilerProfileNodeCallFrame :: RuntimeCallFrame,
    profilerProfileNodeHitCount :: Maybe Int,
    profilerProfileNodeChildren :: Maybe [Int],
    profilerProfileNodeDeoptReason :: Maybe String,
    profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerProfileNode where
    parseJSON = A.withObject "ProfilerProfileNode" $ \v ->
         ProfilerProfileNode <$> v .:  "id"
            <*> v  .:  "callFrame"
            <*> v  .:?  "hitCount"
            <*> v  .:?  "children"
            <*> v  .:?  "deoptReason"
            <*> v  .:?  "positionTicks"


instance ToJSON ProfilerProfileNode  where
    toJSON v = A.object
        [ "id" .= profilerProfileNodeId v
        , "callFrame" .= profilerProfileNodeCallFrame v
        , "hitCount" .= profilerProfileNodeHitCount v
        , "children" .= profilerProfileNodeChildren v
        , "deoptReason" .= profilerProfileNodeDeoptReason v
        , "positionTicks" .= profilerProfileNodePositionTicks v
        ]



data ProfilerProfile = ProfilerProfile {
    profilerProfileNodes :: [ProfilerProfileNode],
    profilerProfileStartTime :: Int,
    profilerProfileEndTime :: Int,
    profilerProfileSamples :: Maybe [Int],
    profilerProfileTimeDeltas :: Maybe [Int]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerProfile where
    parseJSON = A.withObject "ProfilerProfile" $ \v ->
         ProfilerProfile <$> v .:  "nodes"
            <*> v  .:  "startTime"
            <*> v  .:  "endTime"
            <*> v  .:?  "samples"
            <*> v  .:?  "timeDeltas"


instance ToJSON ProfilerProfile  where
    toJSON v = A.object
        [ "nodes" .= profilerProfileNodes v
        , "startTime" .= profilerProfileStartTime v
        , "endTime" .= profilerProfileEndTime v
        , "samples" .= profilerProfileSamples v
        , "timeDeltas" .= profilerProfileTimeDeltas v
        ]



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
    profilerPositionTickInfoLine :: Int,
    profilerPositionTickInfoTicks :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerPositionTickInfo where
    parseJSON = A.withObject "ProfilerPositionTickInfo" $ \v ->
         ProfilerPositionTickInfo <$> v .:  "line"
            <*> v  .:  "ticks"


instance ToJSON ProfilerPositionTickInfo  where
    toJSON v = A.object
        [ "line" .= profilerPositionTickInfoLine v
        , "ticks" .= profilerPositionTickInfoTicks v
        ]



data ProfilerCoverageRange = ProfilerCoverageRange {
    profilerCoverageRangeStartOffset :: Int,
    profilerCoverageRangeEndOffset :: Int,
    profilerCoverageRangeCount :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerCoverageRange where
    parseJSON = A.withObject "ProfilerCoverageRange" $ \v ->
         ProfilerCoverageRange <$> v .:  "startOffset"
            <*> v  .:  "endOffset"
            <*> v  .:  "count"


instance ToJSON ProfilerCoverageRange  where
    toJSON v = A.object
        [ "startOffset" .= profilerCoverageRangeStartOffset v
        , "endOffset" .= profilerCoverageRangeEndOffset v
        , "count" .= profilerCoverageRangeCount v
        ]



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
    profilerFunctionCoverageFunctionName :: String,
    profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
    profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerFunctionCoverage where
    parseJSON = A.withObject "ProfilerFunctionCoverage" $ \v ->
         ProfilerFunctionCoverage <$> v .:  "functionName"
            <*> v  .:  "ranges"
            <*> v  .:  "isBlockCoverage"


instance ToJSON ProfilerFunctionCoverage  where
    toJSON v = A.object
        [ "functionName" .= profilerFunctionCoverageFunctionName v
        , "ranges" .= profilerFunctionCoverageRanges v
        , "isBlockCoverage" .= profilerFunctionCoverageIsBlockCoverage v
        ]



data ProfilerScriptCoverage = ProfilerScriptCoverage {
    profilerScriptCoverageScriptId :: RuntimeScriptId,
    profilerScriptCoverageUrl :: String,
    profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerScriptCoverage where
    parseJSON = A.withObject "ProfilerScriptCoverage" $ \v ->
         ProfilerScriptCoverage <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "functions"


instance ToJSON ProfilerScriptCoverage  where
    toJSON v = A.object
        [ "scriptId" .= profilerScriptCoverageScriptId v
        , "url" .= profilerScriptCoverageUrl v
        , "functions" .= profilerScriptCoverageFunctions v
        ]






profilerDisable :: Session -> IO (Maybe Error)
profilerDisable session = sendReceiveCommand session "Profiler.disable" (Nothing :: Maybe ())




profilerEnable :: Session -> IO (Maybe Error)
profilerEnable session = sendReceiveCommand session "Profiler.enable" (Nothing :: Maybe ())

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerGetBestEffortCoverage where
    parseJSON = A.withObject "ProfilerGetBestEffortCoverage" $ \v ->
         ProfilerGetBestEffortCoverage <$> v .:  "result"



instance Command  ProfilerGetBestEffortCoverage where
    commandName _ = "Profiler.getBestEffortCoverage"


profilerGetBestEffortCoverage :: Session -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage session = sendReceiveCommandResult session "Profiler.getBestEffortCoverage" (Nothing :: Maybe ())



data PProfilerSetSamplingInterval = PProfilerSetSamplingInterval {
    pProfilerSetSamplingIntervalInterval :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PProfilerSetSamplingInterval where
    parseJSON = A.withObject "PProfilerSetSamplingInterval" $ \v ->
         PProfilerSetSamplingInterval <$> v .:  "interval"


instance ToJSON PProfilerSetSamplingInterval  where
    toJSON v = A.object
        [ "interval" .= pProfilerSetSamplingIntervalInterval v
        ]


profilerSetSamplingInterval :: Session -> PProfilerSetSamplingInterval -> IO (Maybe Error)
profilerSetSamplingInterval session params = sendReceiveCommand session "Profiler.setSamplingInterval" (Just params)




profilerStart :: Session -> IO (Maybe Error)
profilerStart session = sendReceiveCommand session "Profiler.start" (Nothing :: Maybe ())

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
    profilerStartPreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStartPreciseCoverage where
    parseJSON = A.withObject "ProfilerStartPreciseCoverage" $ \v ->
         ProfilerStartPreciseCoverage <$> v .:  "timestamp"



instance Command  ProfilerStartPreciseCoverage where
    commandName _ = "Profiler.startPreciseCoverage"

data PProfilerStartPreciseCoverage = PProfilerStartPreciseCoverage {
    pProfilerStartPreciseCoverageCallCount :: Maybe Bool,
    pProfilerStartPreciseCoverageDetailed :: Maybe Bool,
    pProfilerStartPreciseCoverageAllowTriggeredUpdates :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PProfilerStartPreciseCoverage where
    parseJSON = A.withObject "PProfilerStartPreciseCoverage" $ \v ->
         PProfilerStartPreciseCoverage <$> v .:?  "callCount"
            <*> v  .:?  "detailed"
            <*> v  .:?  "allowTriggeredUpdates"


instance ToJSON PProfilerStartPreciseCoverage  where
    toJSON v = A.object
        [ "callCount" .= pProfilerStartPreciseCoverageCallCount v
        , "detailed" .= pProfilerStartPreciseCoverageDetailed v
        , "allowTriggeredUpdates" .= pProfilerStartPreciseCoverageAllowTriggeredUpdates v
        ]


profilerStartPreciseCoverage :: Session -> PProfilerStartPreciseCoverage -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage session params = sendReceiveCommandResult session "Profiler.startPreciseCoverage" (Just params)

data ProfilerStop = ProfilerStop {
    profilerStopProfile :: ProfilerProfile
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerStop where
    parseJSON = A.withObject "ProfilerStop" $ \v ->
         ProfilerStop <$> v .:  "profile"



instance Command  ProfilerStop where
    commandName _ = "Profiler.stop"


profilerStop :: Session -> IO (Either Error ProfilerStop)
profilerStop session = sendReceiveCommandResult session "Profiler.stop" (Nothing :: Maybe ())




profilerStopPreciseCoverage :: Session -> IO (Maybe Error)
profilerStopPreciseCoverage session = sendReceiveCommand session "Profiler.stopPreciseCoverage" (Nothing :: Maybe ())

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    profilerTakePreciseCoverageTimestamp :: Int
} deriving (Eq, Show, Read)
instance FromJSON  ProfilerTakePreciseCoverage where
    parseJSON = A.withObject "ProfilerTakePreciseCoverage" $ \v ->
         ProfilerTakePreciseCoverage <$> v .:  "result"
            <*> v  .:  "timestamp"



instance Command  ProfilerTakePreciseCoverage where
    commandName _ = "Profiler.takePreciseCoverage"


profilerTakePreciseCoverage :: Session -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage session = sendReceiveCommandResult session "Profiler.takePreciseCoverage" (Nothing :: Maybe ())

