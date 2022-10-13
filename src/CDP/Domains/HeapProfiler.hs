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

import CDP.Internal.Utils


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



-- | HeapProfiler.addInspectedHeapObject
--   Enables console to refer to the node with given id via $x (see Command Line API for more details
--   $x functions).

-- | Parameters of the 'HeapProfiler.addInspectedHeapObject' command.
data PHeapProfilerAddInspectedHeapObject = PHeapProfilerAddInspectedHeapObject {
  -- | Heap snapshot object id to be accessible by means of $x command line API.
  pHeapProfilerAddInspectedHeapObjectHeapObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerAddInspectedHeapObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerAddInspectedHeapObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PHeapProfilerAddInspectedHeapObject where
   type CommandResponse PHeapProfilerAddInspectedHeapObject = ()
   commandName _ = "HeapProfiler.addInspectedHeapObject"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.collectGarbage

-- | Parameters of the 'HeapProfiler.collectGarbage' command.
data PHeapProfilerCollectGarbage = PHeapProfilerCollectGarbage
instance ToJSON PHeapProfilerCollectGarbage where toJSON _ = A.Null

instance Command PHeapProfilerCollectGarbage where
   type CommandResponse PHeapProfilerCollectGarbage = ()
   commandName _ = "HeapProfiler.collectGarbage"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.disable

-- | Parameters of the 'HeapProfiler.disable' command.
data PHeapProfilerDisable = PHeapProfilerDisable
instance ToJSON PHeapProfilerDisable where toJSON _ = A.Null

instance Command PHeapProfilerDisable where
   type CommandResponse PHeapProfilerDisable = ()
   commandName _ = "HeapProfiler.disable"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.enable

-- | Parameters of the 'HeapProfiler.enable' command.
data PHeapProfilerEnable = PHeapProfilerEnable
instance ToJSON PHeapProfilerEnable where toJSON _ = A.Null

instance Command PHeapProfilerEnable where
   type CommandResponse PHeapProfilerEnable = ()
   commandName _ = "HeapProfiler.enable"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.getHeapObjectId

-- | Parameters of the 'HeapProfiler.getHeapObjectId' command.
data PHeapProfilerGetHeapObjectId = PHeapProfilerGetHeapObjectId {
  -- | Identifier of the object to get heap object id for.
  pHeapProfilerGetHeapObjectIdObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Return type of the 'HeapProfiler.getHeapObjectId' command.
data HeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId {
  -- | Id of the heap snapshot object corresponding to the passed remote object id.
  heapProfilerGetHeapObjectIdHeapSnapshotObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PHeapProfilerGetHeapObjectId where
   type CommandResponse PHeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId
   commandName _ = "HeapProfiler.getHeapObjectId"



-- | HeapProfiler.getObjectByHeapObjectId

-- | Parameters of the 'HeapProfiler.getObjectByHeapObjectId' command.
data PHeapProfilerGetObjectByHeapObjectId = PHeapProfilerGetObjectByHeapObjectId {
  pHeapProfilerGetObjectByHeapObjectIdObjectId :: HeapProfilerHeapSnapshotObjectId,
  -- | Symbolic group name that can be used to release multiple objects.
  pHeapProfilerGetObjectByHeapObjectIdObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetObjectByHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Return type of the 'HeapProfiler.getObjectByHeapObjectId' command.
data HeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId {
  -- | Evaluation result.
  heapProfilerGetObjectByHeapObjectIdResult :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command PHeapProfilerGetObjectByHeapObjectId where
   type CommandResponse PHeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId
   commandName _ = "HeapProfiler.getObjectByHeapObjectId"



-- | HeapProfiler.getSamplingProfile

-- | Parameters of the 'HeapProfiler.getSamplingProfile' command.
data PHeapProfilerGetSamplingProfile = PHeapProfilerGetSamplingProfile
instance ToJSON PHeapProfilerGetSamplingProfile where toJSON _ = A.Null

-- | Return type of the 'HeapProfiler.getSamplingProfile' command.
data HeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile {
  -- | Return the sampling profile being collected.
  heapProfilerGetSamplingProfileProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command PHeapProfilerGetSamplingProfile where
   type CommandResponse PHeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile
   commandName _ = "HeapProfiler.getSamplingProfile"



-- | HeapProfiler.startSampling

-- | Parameters of the 'HeapProfiler.startSampling' command.
data PHeapProfilerStartSampling = PHeapProfilerStartSampling {
  -- | Average sample interval in bytes. Poisson distribution is used for the intervals. The
  --   default value is 32768 bytes.
  pHeapProfilerStartSamplingSamplingInterval :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PHeapProfilerStartSampling where
   type CommandResponse PHeapProfilerStartSampling = ()
   commandName _ = "HeapProfiler.startSampling"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.startTrackingHeapObjects

-- | Parameters of the 'HeapProfiler.startTrackingHeapObjects' command.
data PHeapProfilerStartTrackingHeapObjects = PHeapProfilerStartTrackingHeapObjects {
  pHeapProfilerStartTrackingHeapObjectsTrackAllocations :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


instance Command PHeapProfilerStartTrackingHeapObjects where
   type CommandResponse PHeapProfilerStartTrackingHeapObjects = ()
   commandName _ = "HeapProfiler.startTrackingHeapObjects"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.stopSampling

-- | Parameters of the 'HeapProfiler.stopSampling' command.
data PHeapProfilerStopSampling = PHeapProfilerStopSampling
instance ToJSON PHeapProfilerStopSampling where toJSON _ = A.Null

-- | Return type of the 'HeapProfiler.stopSampling' command.
data HeapProfilerStopSampling = HeapProfilerStopSampling {
  -- | Recorded sampling heap profile.
  heapProfilerStopSamplingProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerStopSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PHeapProfilerStopSampling where
   type CommandResponse PHeapProfilerStopSampling = HeapProfilerStopSampling
   commandName _ = "HeapProfiler.stopSampling"



-- | HeapProfiler.stopTrackingHeapObjects

-- | Parameters of the 'HeapProfiler.stopTrackingHeapObjects' command.
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


instance Command PHeapProfilerStopTrackingHeapObjects where
   type CommandResponse PHeapProfilerStopTrackingHeapObjects = ()
   commandName _ = "HeapProfiler.stopTrackingHeapObjects"
   fromJSON = const . A.Success . const ()


-- | HeapProfiler.takeHeapSnapshot

-- | Parameters of the 'HeapProfiler.takeHeapSnapshot' command.
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


instance Command PHeapProfilerTakeHeapSnapshot where
   type CommandResponse PHeapProfilerTakeHeapSnapshot = ()
   commandName _ = "HeapProfiler.takeHeapSnapshot"
   fromJSON = const . A.Success . const ()



