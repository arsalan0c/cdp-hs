{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  HeapProfiler 
-}


module CDP.Domains.HeapProfiler (module CDP.Domains.HeapProfiler) where

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

import CDP.Internal.Runtime


import CDP.Domains.Runtime as Runtime


-- | Type 'HeapProfiler.HeapSnapshotObjectId'.
--   Heap snapshot object id.
type HeapProfilerHeapSnapshotObjectId = String

-- | Type 'HeapProfiler.SamplingHeapProfileNode'.
--   Sampling Heap Profile node. Holds callsite information, allocation statistics and child nodes.
data HeapProfilerSamplingHeapProfileNode = HeapProfilerSamplingHeapProfileNode {
  -- | Function location.
  heapProfilerSamplingHeapProfileNodeCallFrame :: Runtime.RuntimeCallFrame,
  -- | Allocations size in bytes for the node excluding children.
  heapProfilerSamplingHeapProfileNodeSelfSize :: Double,
  -- | Node id. Ids are unique across all profiles collected between startSampling and stopSampling.
  heapProfilerSamplingHeapProfileNodeId :: Int,
  -- | Child nodes.
  heapProfilerSamplingHeapProfileNodeChildren :: [HeapProfilerSamplingHeapProfileNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'HeapProfiler.SamplingHeapProfileSample'.
--   A single sample from a sampling profile.
data HeapProfilerSamplingHeapProfileSample = HeapProfilerSamplingHeapProfileSample {
  -- | Allocation size in bytes attributed to the sample.
  heapProfilerSamplingHeapProfileSampleSize :: Double,
  -- | Id of the corresponding profile tree node.
  heapProfilerSamplingHeapProfileSampleNodeId :: Int,
  -- | Time-ordered sample ordinal number. It is unique across all profiles retrieved
  --   between startSampling and stopSampling.
  heapProfilerSamplingHeapProfileSampleOrdinal :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfileSample  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfileSample where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



-- | Type 'HeapProfiler.SamplingHeapProfile'.
--   Sampling profile.
data HeapProfilerSamplingHeapProfile = HeapProfilerSamplingHeapProfile {
  heapProfilerSamplingHeapProfileHead :: HeapProfilerSamplingHeapProfileNode,
  heapProfilerSamplingHeapProfileSamples :: [HeapProfilerSamplingHeapProfileSample]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }





-- | Type of the 'HeapProfiler.addHeapSnapshotChunk' event.
data HeapProfilerAddHeapSnapshotChunk = HeapProfilerAddHeapSnapshotChunk {
  heapProfilerAddHeapSnapshotChunkChunk :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerAddHeapSnapshotChunk  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerAddHeapSnapshotChunk where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event HeapProfilerAddHeapSnapshotChunk where
    eventName _ = "HeapProfiler.addHeapSnapshotChunk"

-- | Type of the 'HeapProfiler.heapStatsUpdate' event.
data HeapProfilerHeapStatsUpdate = HeapProfilerHeapStatsUpdate {
  -- | An array of triplets. Each triplet describes a fragment. The first integer is the fragment
  --   index, the second integer is a total count of objects for the fragment, the third integer is
  --   a total size of the objects for the fragment.
  heapProfilerHeapStatsUpdateStatsUpdate :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerHeapStatsUpdate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerHeapStatsUpdate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Event HeapProfilerHeapStatsUpdate where
    eventName _ = "HeapProfiler.heapStatsUpdate"

-- | Type of the 'HeapProfiler.lastSeenObjectId' event.
data HeapProfilerLastSeenObjectId = HeapProfilerLastSeenObjectId {
  heapProfilerLastSeenObjectIdLastSeenObjectId :: Int,
  heapProfilerLastSeenObjectIdTimestamp :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerLastSeenObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerLastSeenObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Event HeapProfilerLastSeenObjectId where
    eventName _ = "HeapProfiler.lastSeenObjectId"

-- | Type of the 'HeapProfiler.reportHeapSnapshotProgress' event.
data HeapProfilerReportHeapSnapshotProgress = HeapProfilerReportHeapSnapshotProgress {
  heapProfilerReportHeapSnapshotProgressDone :: Int,
  heapProfilerReportHeapSnapshotProgressTotal :: Int,
  heapProfilerReportHeapSnapshotProgressFinished :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerReportHeapSnapshotProgress  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerReportHeapSnapshotProgress where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


instance Event HeapProfilerReportHeapSnapshotProgress where
    eventName _ = "HeapProfiler.reportHeapSnapshotProgress"

-- | Type of the 'HeapProfiler.resetProfiles' event.
data HeapProfilerResetProfiles = HeapProfilerResetProfiles
   deriving (Eq, Show, Read)
instance FromJSON HeapProfilerResetProfiles where
   parseJSON = A.withText  "HeapProfilerResetProfiles"  $ \v -> do
      case v of
         "HeapProfilerResetProfiles" -> pure HeapProfilerResetProfiles
         _ -> fail "failed to parse HeapProfilerResetProfiles"


instance Event HeapProfilerResetProfiles where
    eventName _ = "HeapProfiler.resetProfiles"



-- | Parameters of the 'heapProfilerAddInspectedHeapObject' command.
data PHeapProfilerAddInspectedHeapObject = PHeapProfilerAddInspectedHeapObject {
  -- | Heap snapshot object id to be accessible by means of $x command line API.
  pHeapProfilerAddInspectedHeapObjectHeapObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerAddInspectedHeapObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerAddInspectedHeapObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'HeapProfiler.addInspectedHeapObject' command.
--   Enables console to refer to the node with given id via $x (see Command Line API for more details
--   $x functions).
--   Returns: 'PHeapProfilerAddInspectedHeapObject'
heapProfilerAddInspectedHeapObject :: Handle -> PHeapProfilerAddInspectedHeapObject -> IO ()
heapProfilerAddInspectedHeapObject handle params = sendReceiveCommand handle params

instance Command PHeapProfilerAddInspectedHeapObject where
    type CommandResponse PHeapProfilerAddInspectedHeapObject = NoResponse
    commandName _ = "HeapProfiler.addInspectedHeapObject"


-- | Parameters of the 'heapProfilerCollectGarbage' command.
data PHeapProfilerCollectGarbage = PHeapProfilerCollectGarbage
instance ToJSON PHeapProfilerCollectGarbage where toJSON _ = A.Null

-- | Function for the 'HeapProfiler.collectGarbage' command.
heapProfilerCollectGarbage :: Handle -> IO ()
heapProfilerCollectGarbage handle = sendReceiveCommand handle PHeapProfilerCollectGarbage

instance Command PHeapProfilerCollectGarbage where
    type CommandResponse PHeapProfilerCollectGarbage = NoResponse
    commandName _ = "HeapProfiler.collectGarbage"


-- | Parameters of the 'heapProfilerDisable' command.
data PHeapProfilerDisable = PHeapProfilerDisable
instance ToJSON PHeapProfilerDisable where toJSON _ = A.Null

-- | Function for the 'HeapProfiler.disable' command.
heapProfilerDisable :: Handle -> IO ()
heapProfilerDisable handle = sendReceiveCommand handle PHeapProfilerDisable

instance Command PHeapProfilerDisable where
    type CommandResponse PHeapProfilerDisable = NoResponse
    commandName _ = "HeapProfiler.disable"


-- | Parameters of the 'heapProfilerEnable' command.
data PHeapProfilerEnable = PHeapProfilerEnable
instance ToJSON PHeapProfilerEnable where toJSON _ = A.Null

-- | Function for the 'HeapProfiler.enable' command.
heapProfilerEnable :: Handle -> IO ()
heapProfilerEnable handle = sendReceiveCommand handle PHeapProfilerEnable

instance Command PHeapProfilerEnable where
    type CommandResponse PHeapProfilerEnable = NoResponse
    commandName _ = "HeapProfiler.enable"


-- | Parameters of the 'heapProfilerGetHeapObjectId' command.
data PHeapProfilerGetHeapObjectId = PHeapProfilerGetHeapObjectId {
  -- | Identifier of the object to get heap object id for.
  pHeapProfilerGetHeapObjectIdObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'HeapProfiler.getHeapObjectId' command.
--   
--   Returns: 'PHeapProfilerGetHeapObjectId'
--   Returns: 'HeapProfilerGetHeapObjectId'
heapProfilerGetHeapObjectId :: Handle -> PHeapProfilerGetHeapObjectId -> IO HeapProfilerGetHeapObjectId
heapProfilerGetHeapObjectId handle params = sendReceiveCommandResult handle params

-- | Return type of the 'heapProfilerGetHeapObjectId' command.
data HeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId {
  -- | Id of the heap snapshot object corresponding to the passed remote object id.
  heapProfilerGetHeapObjectIdHeapSnapshotObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PHeapProfilerGetHeapObjectId where
    type CommandResponse PHeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId
    commandName _ = "HeapProfiler.getHeapObjectId"


-- | Parameters of the 'heapProfilerGetObjectByHeapObjectId' command.
data PHeapProfilerGetObjectByHeapObjectId = PHeapProfilerGetObjectByHeapObjectId {
  pHeapProfilerGetObjectByHeapObjectIdObjectId :: HeapProfilerHeapSnapshotObjectId,
  -- | Symbolic group name that can be used to release multiple objects.
  pHeapProfilerGetObjectByHeapObjectIdObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetObjectByHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'HeapProfiler.getObjectByHeapObjectId' command.
--   
--   Returns: 'PHeapProfilerGetObjectByHeapObjectId'
--   Returns: 'HeapProfilerGetObjectByHeapObjectId'
heapProfilerGetObjectByHeapObjectId :: Handle -> PHeapProfilerGetObjectByHeapObjectId -> IO HeapProfilerGetObjectByHeapObjectId
heapProfilerGetObjectByHeapObjectId handle params = sendReceiveCommandResult handle params

-- | Return type of the 'heapProfilerGetObjectByHeapObjectId' command.
data HeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId {
  -- | Evaluation result.
  heapProfilerGetObjectByHeapObjectIdResult :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command PHeapProfilerGetObjectByHeapObjectId where
    type CommandResponse PHeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId
    commandName _ = "HeapProfiler.getObjectByHeapObjectId"


-- | Parameters of the 'heapProfilerGetSamplingProfile' command.
data PHeapProfilerGetSamplingProfile = PHeapProfilerGetSamplingProfile
instance ToJSON PHeapProfilerGetSamplingProfile where toJSON _ = A.Null

-- | Function for the 'HeapProfiler.getSamplingProfile' command.
--   
--   Returns: 'HeapProfilerGetSamplingProfile'
heapProfilerGetSamplingProfile :: Handle -> IO HeapProfilerGetSamplingProfile
heapProfilerGetSamplingProfile handle = sendReceiveCommandResult handle PHeapProfilerGetSamplingProfile

-- | Return type of the 'heapProfilerGetSamplingProfile' command.
data HeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile {
  -- | Return the sampling profile being collected.
  heapProfilerGetSamplingProfileProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command PHeapProfilerGetSamplingProfile where
    type CommandResponse PHeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile
    commandName _ = "HeapProfiler.getSamplingProfile"


-- | Parameters of the 'heapProfilerStartSampling' command.
data PHeapProfilerStartSampling = PHeapProfilerStartSampling {
  -- | Average sample interval in bytes. Poisson distribution is used for the intervals. The
  --   default value is 32768 bytes.
  pHeapProfilerStartSamplingSamplingInterval :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'HeapProfiler.startSampling' command.
--   
--   Returns: 'PHeapProfilerStartSampling'
heapProfilerStartSampling :: Handle -> PHeapProfilerStartSampling -> IO ()
heapProfilerStartSampling handle params = sendReceiveCommand handle params

instance Command PHeapProfilerStartSampling where
    type CommandResponse PHeapProfilerStartSampling = NoResponse
    commandName _ = "HeapProfiler.startSampling"


-- | Parameters of the 'heapProfilerStartTrackingHeapObjects' command.
data PHeapProfilerStartTrackingHeapObjects = PHeapProfilerStartTrackingHeapObjects {
  pHeapProfilerStartTrackingHeapObjectsTrackAllocations :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the 'HeapProfiler.startTrackingHeapObjects' command.
--   
--   Returns: 'PHeapProfilerStartTrackingHeapObjects'
heapProfilerStartTrackingHeapObjects :: Handle -> PHeapProfilerStartTrackingHeapObjects -> IO ()
heapProfilerStartTrackingHeapObjects handle params = sendReceiveCommand handle params

instance Command PHeapProfilerStartTrackingHeapObjects where
    type CommandResponse PHeapProfilerStartTrackingHeapObjects = NoResponse
    commandName _ = "HeapProfiler.startTrackingHeapObjects"


-- | Parameters of the 'heapProfilerStopSampling' command.
data PHeapProfilerStopSampling = PHeapProfilerStopSampling
instance ToJSON PHeapProfilerStopSampling where toJSON _ = A.Null

-- | Function for the 'HeapProfiler.stopSampling' command.
--   
--   Returns: 'HeapProfilerStopSampling'
heapProfilerStopSampling :: Handle -> IO HeapProfilerStopSampling
heapProfilerStopSampling handle = sendReceiveCommandResult handle PHeapProfilerStopSampling

-- | Return type of the 'heapProfilerStopSampling' command.
data HeapProfilerStopSampling = HeapProfilerStopSampling {
  -- | Recorded sampling heap profile.
  heapProfilerStopSamplingProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerStopSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PHeapProfilerStopSampling where
    type CommandResponse PHeapProfilerStopSampling = HeapProfilerStopSampling
    commandName _ = "HeapProfiler.stopSampling"


-- | Parameters of the 'heapProfilerStopTrackingHeapObjects' command.
data PHeapProfilerStopTrackingHeapObjects = PHeapProfilerStopTrackingHeapObjects {
  -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken
  --   when the tracking is stopped.
  pHeapProfilerStopTrackingHeapObjectsReportProgress :: Maybe Bool,
  pHeapProfilerStopTrackingHeapObjectsTreatGlobalObjectsAsRoots :: Maybe Bool,
  -- | If true, numerical values are included in the snapshot
  pHeapProfilerStopTrackingHeapObjectsCaptureNumericValue :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStopTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStopTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'HeapProfiler.stopTrackingHeapObjects' command.
--   
--   Returns: 'PHeapProfilerStopTrackingHeapObjects'
heapProfilerStopTrackingHeapObjects :: Handle -> PHeapProfilerStopTrackingHeapObjects -> IO ()
heapProfilerStopTrackingHeapObjects handle params = sendReceiveCommand handle params

instance Command PHeapProfilerStopTrackingHeapObjects where
    type CommandResponse PHeapProfilerStopTrackingHeapObjects = NoResponse
    commandName _ = "HeapProfiler.stopTrackingHeapObjects"


-- | Parameters of the 'heapProfilerTakeHeapSnapshot' command.
data PHeapProfilerTakeHeapSnapshot = PHeapProfilerTakeHeapSnapshot {
  -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken.
  pHeapProfilerTakeHeapSnapshotReportProgress :: Maybe Bool,
  -- | If true, a raw snapshot without artificial roots will be generated
  pHeapProfilerTakeHeapSnapshotTreatGlobalObjectsAsRoots :: Maybe Bool,
  -- | If true, numerical values are included in the snapshot
  pHeapProfilerTakeHeapSnapshotCaptureNumericValue :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerTakeHeapSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerTakeHeapSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'HeapProfiler.takeHeapSnapshot' command.
--   
--   Returns: 'PHeapProfilerTakeHeapSnapshot'
heapProfilerTakeHeapSnapshot :: Handle -> PHeapProfilerTakeHeapSnapshot -> IO ()
heapProfilerTakeHeapSnapshot handle params = sendReceiveCommand handle params

instance Command PHeapProfilerTakeHeapSnapshot where
    type CommandResponse PHeapProfilerTakeHeapSnapshot = NoResponse
    commandName _ = "HeapProfiler.takeHeapSnapshot"



