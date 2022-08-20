{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Profiler (module Domains.Profiler) where
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

import qualified Domains.Runtime as Runtime
import qualified Domains.Debugger as Debugger


import Utils

data ConsoleProfileFinished = ConsoleProfileFinished {
    consoleProfileFinishedId :: String,
    consoleProfileFinishedLocation :: Debugger.Location,
    consoleProfileFinishedProfile :: Profile,
    consoleProfileFinishedTitle :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ConsoleProfileFinished where
    parseJSON = A.withObject "ConsoleProfileFinished" $ \v ->
         ConsoleProfileFinished <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:  "profile"
            <*> v  .:?  "title"


instance ToJSON ConsoleProfileFinished  where
    toJSON v = A.object
        [ "id" .= consoleProfileFinishedId v
        , "location" .= consoleProfileFinishedLocation v
        , "profile" .= consoleProfileFinishedProfile v
        , "title" .= consoleProfileFinishedTitle v
        ]


data ConsoleProfileStarted = ConsoleProfileStarted {
    consoleProfileStartedId :: String,
    consoleProfileStartedLocation :: Debugger.Location,
    consoleProfileStartedTitle :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ConsoleProfileStarted where
    parseJSON = A.withObject "ConsoleProfileStarted" $ \v ->
         ConsoleProfileStarted <$> v .:  "id"
            <*> v  .:  "location"
            <*> v  .:?  "title"


instance ToJSON ConsoleProfileStarted  where
    toJSON v = A.object
        [ "id" .= consoleProfileStartedId v
        , "location" .= consoleProfileStartedLocation v
        , "title" .= consoleProfileStartedTitle v
        ]



data ProfileNode = ProfileNode {
    profileNodeId :: Int,
    profileNodeCallFrame :: Runtime.CallFrame,
    profileNodeHitCount :: Maybe Int,
    profileNodeChildren :: Maybe [Int],
    profileNodeDeoptReason :: Maybe String,
    profileNodePositionTicks :: Maybe [PositionTickInfo]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ProfileNode where
    parseJSON = A.withObject "ProfileNode" $ \v ->
         ProfileNode <$> v .:  "id"
            <*> v  .:  "callFrame"
            <*> v  .:?  "hitCount"
            <*> v  .:?  "children"
            <*> v  .:?  "deoptReason"
            <*> v  .:?  "positionTicks"


instance ToJSON ProfileNode  where
    toJSON v = A.object
        [ "id" .= profileNodeId v
        , "callFrame" .= profileNodeCallFrame v
        , "hitCount" .= profileNodeHitCount v
        , "children" .= profileNodeChildren v
        , "deoptReason" .= profileNodeDeoptReason v
        , "positionTicks" .= profileNodePositionTicks v
        ]



data Profile = Profile {
    profileNodes :: [ProfileNode],
    profileStartTime :: Int,
    profileEndTime :: Int,
    profileSamples :: Maybe [Int],
    profileTimeDeltas :: Maybe [Int]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Profile where
    parseJSON = A.withObject "Profile" $ \v ->
         Profile <$> v .:  "nodes"
            <*> v  .:  "startTime"
            <*> v  .:  "endTime"
            <*> v  .:?  "samples"
            <*> v  .:?  "timeDeltas"


instance ToJSON Profile  where
    toJSON v = A.object
        [ "nodes" .= profileNodes v
        , "startTime" .= profileStartTime v
        , "endTime" .= profileEndTime v
        , "samples" .= profileSamples v
        , "timeDeltas" .= profileTimeDeltas v
        ]



data PositionTickInfo = PositionTickInfo {
    positionTickInfoLine :: Int,
    positionTickInfoTicks :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  PositionTickInfo where
    parseJSON = A.withObject "PositionTickInfo" $ \v ->
         PositionTickInfo <$> v .:  "line"
            <*> v  .:  "ticks"


instance ToJSON PositionTickInfo  where
    toJSON v = A.object
        [ "line" .= positionTickInfoLine v
        , "ticks" .= positionTickInfoTicks v
        ]



data CoverageRange = CoverageRange {
    coverageRangeStartOffset :: Int,
    coverageRangeEndOffset :: Int,
    coverageRangeCount :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CoverageRange where
    parseJSON = A.withObject "CoverageRange" $ \v ->
         CoverageRange <$> v .:  "startOffset"
            <*> v  .:  "endOffset"
            <*> v  .:  "count"


instance ToJSON CoverageRange  where
    toJSON v = A.object
        [ "startOffset" .= coverageRangeStartOffset v
        , "endOffset" .= coverageRangeEndOffset v
        , "count" .= coverageRangeCount v
        ]



data FunctionCoverage = FunctionCoverage {
    functionCoverageFunctionName :: String,
    functionCoverageRanges :: [CoverageRange],
    functionCoverageIsBlockCoverage :: Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  FunctionCoverage where
    parseJSON = A.withObject "FunctionCoverage" $ \v ->
         FunctionCoverage <$> v .:  "functionName"
            <*> v  .:  "ranges"
            <*> v  .:  "isBlockCoverage"


instance ToJSON FunctionCoverage  where
    toJSON v = A.object
        [ "functionName" .= functionCoverageFunctionName v
        , "ranges" .= functionCoverageRanges v
        , "isBlockCoverage" .= functionCoverageIsBlockCoverage v
        ]



data ScriptCoverage = ScriptCoverage {
    scriptCoverageScriptId :: Runtime.ScriptId,
    scriptCoverageUrl :: String,
    scriptCoverageFunctions :: [FunctionCoverage]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ScriptCoverage where
    parseJSON = A.withObject "ScriptCoverage" $ \v ->
         ScriptCoverage <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "functions"


instance ToJSON ScriptCoverage  where
    toJSON v = A.object
        [ "scriptId" .= scriptCoverageScriptId v
        , "url" .= scriptCoverageUrl v
        , "functions" .= scriptCoverageFunctions v
        ]



disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Profiler","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Profiler","enable") ([] ++ (catMaybes []))

data GetBestEffortCoverage = GetBestEffortCoverage {
    getBestEffortCoverageResult :: [ScriptCoverage]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetBestEffortCoverage where
    parseJSON = A.withObject "GetBestEffortCoverage" $ \v ->
         GetBestEffortCoverage <$> v .:  "result"



getBestEffortCoverage :: Session a -> IO (Either Error GetBestEffortCoverage)
getBestEffortCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","getBestEffortCoverage") ([] ++ (catMaybes []))


setSamplingInterval :: Session a -> Int -> IO (Maybe Error)
setSamplingInterval session setSamplingIntervalInterval = sendReceiveCommand (conn session) ("Profiler","setSamplingInterval") ([("interval", ToJSONEx setSamplingIntervalInterval)] ++ (catMaybes []))


start :: Session a -> IO (Maybe Error)
start session  = sendReceiveCommand (conn session) ("Profiler","start") ([] ++ (catMaybes []))

data StartPreciseCoverage = StartPreciseCoverage {
    startPreciseCoverageTimestamp :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  StartPreciseCoverage where
    parseJSON = A.withObject "StartPreciseCoverage" $ \v ->
         StartPreciseCoverage <$> v .:  "timestamp"



startPreciseCoverage :: Session a -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error StartPreciseCoverage)
startPreciseCoverage session startPreciseCoverageCallCount startPreciseCoverageDetailed startPreciseCoverageAllowTriggeredUpdates = sendReceiveCommandResult (conn session) ("Profiler","startPreciseCoverage") ([] ++ (catMaybes [fmap (("callCount",) . ToJSONEx) startPreciseCoverageCallCount, fmap (("detailed",) . ToJSONEx) startPreciseCoverageDetailed, fmap (("allowTriggeredUpdates",) . ToJSONEx) startPreciseCoverageAllowTriggeredUpdates]))

data Stop = Stop {
    stopProfile :: Profile
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Stop where
    parseJSON = A.withObject "Stop" $ \v ->
         Stop <$> v .:  "profile"



stop :: Session a -> IO (Either Error Stop)
stop session  = sendReceiveCommandResult (conn session) ("Profiler","stop") ([] ++ (catMaybes []))


stopPreciseCoverage :: Session a -> IO (Maybe Error)
stopPreciseCoverage session  = sendReceiveCommand (conn session) ("Profiler","stopPreciseCoverage") ([] ++ (catMaybes []))

data TakePreciseCoverage = TakePreciseCoverage {
    takePreciseCoverageResult :: [ScriptCoverage],
    takePreciseCoverageTimestamp :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TakePreciseCoverage where
    parseJSON = A.withObject "TakePreciseCoverage" $ \v ->
         TakePreciseCoverage <$> v .:  "result"
            <*> v  .:  "timestamp"



takePreciseCoverage :: Session a -> IO (Either Error TakePreciseCoverage)
takePreciseCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","takePreciseCoverage") ([] ++ (catMaybes []))


