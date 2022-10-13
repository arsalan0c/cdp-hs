{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= HeapProfiler

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
data HeapProfilerSamplingHeapProfileNode = HeapProfilerSamplingHeapProfileNode
  {
    -- | Function location.
    heapProfilerSamplingHeapProfileNodeCallFrame :: Runtime.RuntimeCallFrame,
    -- | Allocations size in bytes for the node excluding children.
    heapProfilerSamplingHeapProfileNodeSelfSize :: Double,
    -- | Node id. Ids are unique across all profiles collected between startSampling and stopSampling.
    heapProfilerSamplingHeapProfileNodeId :: Int,
    -- | Child nodes.
    heapProfilerSamplingHeapProfileNodeChildren :: [HeapProfilerSamplingHeapProfileNode]
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerSamplingHeapProfileNode where
  parseJSON = A.withObject "HeapProfilerSamplingHeapProfileNode" $ \o -> HeapProfilerSamplingHeapProfileNode
    <$> o A..: "callFrame"
    <*> o A..: "selfSize"
    <*> o A..: "id"
    <*> o A..: "children"
instance ToJSON HeapProfilerSamplingHeapProfileNode where
  toJSON p = A.object $ catMaybes [
    ("callFrame" A..=) <$> Just (heapProfilerSamplingHeapProfileNodeCallFrame p),
    ("selfSize" A..=) <$> Just (heapProfilerSamplingHeapProfileNodeSelfSize p),
    ("id" A..=) <$> Just (heapProfilerSamplingHeapProfileNodeId p),
    ("children" A..=) <$> Just (heapProfilerSamplingHeapProfileNodeChildren p)
    ]

-- | Type 'HeapProfiler.SamplingHeapProfileSample'.
--   A single sample from a sampling profile.
data HeapProfilerSamplingHeapProfileSample = HeapProfilerSamplingHeapProfileSample
  {
    -- | Allocation size in bytes attributed to the sample.
    heapProfilerSamplingHeapProfileSampleSize :: Double,
    -- | Id of the corresponding profile tree node.
    heapProfilerSamplingHeapProfileSampleNodeId :: Int,
    -- | Time-ordered sample ordinal number. It is unique across all profiles retrieved
    --   between startSampling and stopSampling.
    heapProfilerSamplingHeapProfileSampleOrdinal :: Double
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerSamplingHeapProfileSample where
  parseJSON = A.withObject "HeapProfilerSamplingHeapProfileSample" $ \o -> HeapProfilerSamplingHeapProfileSample
    <$> o A..: "size"
    <*> o A..: "nodeId"
    <*> o A..: "ordinal"
instance ToJSON HeapProfilerSamplingHeapProfileSample where
  toJSON p = A.object $ catMaybes [
    ("size" A..=) <$> Just (heapProfilerSamplingHeapProfileSampleSize p),
    ("nodeId" A..=) <$> Just (heapProfilerSamplingHeapProfileSampleNodeId p),
    ("ordinal" A..=) <$> Just (heapProfilerSamplingHeapProfileSampleOrdinal p)
    ]

-- | Type 'HeapProfiler.SamplingHeapProfile'.
--   Sampling profile.
data HeapProfilerSamplingHeapProfile = HeapProfilerSamplingHeapProfile
  {
    heapProfilerSamplingHeapProfileHead :: HeapProfilerSamplingHeapProfileNode,
    heapProfilerSamplingHeapProfileSamples :: [HeapProfilerSamplingHeapProfileSample]
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerSamplingHeapProfile where
  parseJSON = A.withObject "HeapProfilerSamplingHeapProfile" $ \o -> HeapProfilerSamplingHeapProfile
    <$> o A..: "head"
    <*> o A..: "samples"
instance ToJSON HeapProfilerSamplingHeapProfile where
  toJSON p = A.object $ catMaybes [
    ("head" A..=) <$> Just (heapProfilerSamplingHeapProfileHead p),
    ("samples" A..=) <$> Just (heapProfilerSamplingHeapProfileSamples p)
    ]

-- | Type of the 'HeapProfiler.addHeapSnapshotChunk' event.
data HeapProfilerAddHeapSnapshotChunk = HeapProfilerAddHeapSnapshotChunk
  {
    heapProfilerAddHeapSnapshotChunkChunk :: String
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerAddHeapSnapshotChunk where
  parseJSON = A.withObject "HeapProfilerAddHeapSnapshotChunk" $ \o -> HeapProfilerAddHeapSnapshotChunk
    <$> o A..: "chunk"
instance Event HeapProfilerAddHeapSnapshotChunk where
  eventName _ = "HeapProfiler.addHeapSnapshotChunk"

-- | Type of the 'HeapProfiler.heapStatsUpdate' event.
data HeapProfilerHeapStatsUpdate = HeapProfilerHeapStatsUpdate
  {
    -- | An array of triplets. Each triplet describes a fragment. The first integer is the fragment
    --   index, the second integer is a total count of objects for the fragment, the third integer is
    --   a total size of the objects for the fragment.
    heapProfilerHeapStatsUpdateStatsUpdate :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerHeapStatsUpdate where
  parseJSON = A.withObject "HeapProfilerHeapStatsUpdate" $ \o -> HeapProfilerHeapStatsUpdate
    <$> o A..: "statsUpdate"
instance Event HeapProfilerHeapStatsUpdate where
  eventName _ = "HeapProfiler.heapStatsUpdate"

-- | Type of the 'HeapProfiler.lastSeenObjectId' event.
data HeapProfilerLastSeenObjectId = HeapProfilerLastSeenObjectId
  {
    heapProfilerLastSeenObjectIdLastSeenObjectId :: Int,
    heapProfilerLastSeenObjectIdTimestamp :: Double
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerLastSeenObjectId where
  parseJSON = A.withObject "HeapProfilerLastSeenObjectId" $ \o -> HeapProfilerLastSeenObjectId
    <$> o A..: "lastSeenObjectId"
    <*> o A..: "timestamp"
instance Event HeapProfilerLastSeenObjectId where
  eventName _ = "HeapProfiler.lastSeenObjectId"

-- | Type of the 'HeapProfiler.reportHeapSnapshotProgress' event.
data HeapProfilerReportHeapSnapshotProgress = HeapProfilerReportHeapSnapshotProgress
  {
    heapProfilerReportHeapSnapshotProgressDone :: Int,
    heapProfilerReportHeapSnapshotProgressTotal :: Int,
    heapProfilerReportHeapSnapshotProgressFinished :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerReportHeapSnapshotProgress where
  parseJSON = A.withObject "HeapProfilerReportHeapSnapshotProgress" $ \o -> HeapProfilerReportHeapSnapshotProgress
    <$> o A..: "done"
    <*> o A..: "total"
    <*> o A..:? "finished"
instance Event HeapProfilerReportHeapSnapshotProgress where
  eventName _ = "HeapProfiler.reportHeapSnapshotProgress"

-- | Type of the 'HeapProfiler.resetProfiles' event.
data HeapProfilerResetProfiles = HeapProfilerResetProfiles
  deriving (Eq, Show, Read)
instance FromJSON HeapProfilerResetProfiles where
  parseJSON _ = pure HeapProfilerResetProfiles
instance Event HeapProfilerResetProfiles where
  eventName _ = "HeapProfiler.resetProfiles"

-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
--   $x functions).

-- | Parameters of the 'HeapProfiler.addInspectedHeapObject' command.
data PHeapProfilerAddInspectedHeapObject = PHeapProfilerAddInspectedHeapObject
  {
    -- | Heap snapshot object id to be accessible by means of $x command line API.
    pHeapProfilerAddInspectedHeapObjectHeapObjectId :: HeapProfilerHeapSnapshotObjectId
  }
  deriving (Eq, Show)
pHeapProfilerAddInspectedHeapObject
  -- | Heap snapshot object id to be accessible by means of $x command line API.
  :: HeapProfilerHeapSnapshotObjectId
  -> PHeapProfilerAddInspectedHeapObject
pHeapProfilerAddInspectedHeapObject
  arg_pHeapProfilerAddInspectedHeapObjectHeapObjectId
  = PHeapProfilerAddInspectedHeapObject
    arg_pHeapProfilerAddInspectedHeapObjectHeapObjectId
instance ToJSON PHeapProfilerAddInspectedHeapObject where
  toJSON p = A.object $ catMaybes [
    ("heapObjectId" A..=) <$> Just (pHeapProfilerAddInspectedHeapObjectHeapObjectId p)
    ]
instance Command PHeapProfilerAddInspectedHeapObject where
  type CommandResponse PHeapProfilerAddInspectedHeapObject = ()
  commandName _ = "HeapProfiler.addInspectedHeapObject"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.collectGarbage' command.
data PHeapProfilerCollectGarbage = PHeapProfilerCollectGarbage
  deriving (Eq, Show)
pHeapProfilerCollectGarbage
  :: PHeapProfilerCollectGarbage
pHeapProfilerCollectGarbage
  = PHeapProfilerCollectGarbage
instance ToJSON PHeapProfilerCollectGarbage where
  toJSON _ = A.Null
instance Command PHeapProfilerCollectGarbage where
  type CommandResponse PHeapProfilerCollectGarbage = ()
  commandName _ = "HeapProfiler.collectGarbage"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.disable' command.
data PHeapProfilerDisable = PHeapProfilerDisable
  deriving (Eq, Show)
pHeapProfilerDisable
  :: PHeapProfilerDisable
pHeapProfilerDisable
  = PHeapProfilerDisable
instance ToJSON PHeapProfilerDisable where
  toJSON _ = A.Null
instance Command PHeapProfilerDisable where
  type CommandResponse PHeapProfilerDisable = ()
  commandName _ = "HeapProfiler.disable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.enable' command.
data PHeapProfilerEnable = PHeapProfilerEnable
  deriving (Eq, Show)
pHeapProfilerEnable
  :: PHeapProfilerEnable
pHeapProfilerEnable
  = PHeapProfilerEnable
instance ToJSON PHeapProfilerEnable where
  toJSON _ = A.Null
instance Command PHeapProfilerEnable where
  type CommandResponse PHeapProfilerEnable = ()
  commandName _ = "HeapProfiler.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.getHeapObjectId' command.
data PHeapProfilerGetHeapObjectId = PHeapProfilerGetHeapObjectId
  {
    -- | Identifier of the object to get heap object id for.
    pHeapProfilerGetHeapObjectIdObjectId :: Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pHeapProfilerGetHeapObjectId
  -- | Identifier of the object to get heap object id for.
  :: Runtime.RuntimeRemoteObjectId
  -> PHeapProfilerGetHeapObjectId
pHeapProfilerGetHeapObjectId
  arg_pHeapProfilerGetHeapObjectIdObjectId
  = PHeapProfilerGetHeapObjectId
    arg_pHeapProfilerGetHeapObjectIdObjectId
instance ToJSON PHeapProfilerGetHeapObjectId where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pHeapProfilerGetHeapObjectIdObjectId p)
    ]
data HeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId
  {
    -- | Id of the heap snapshot object corresponding to the passed remote object id.
    heapProfilerGetHeapObjectIdHeapSnapshotObjectId :: HeapProfilerHeapSnapshotObjectId
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerGetHeapObjectId where
  parseJSON = A.withObject "HeapProfilerGetHeapObjectId" $ \o -> HeapProfilerGetHeapObjectId
    <$> o A..: "heapSnapshotObjectId"
instance Command PHeapProfilerGetHeapObjectId where
  type CommandResponse PHeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId
  commandName _ = "HeapProfiler.getHeapObjectId"


-- | Parameters of the 'HeapProfiler.getObjectByHeapObjectId' command.
data PHeapProfilerGetObjectByHeapObjectId = PHeapProfilerGetObjectByHeapObjectId
  {
    pHeapProfilerGetObjectByHeapObjectIdObjectId :: HeapProfilerHeapSnapshotObjectId,
    -- | Symbolic group name that can be used to release multiple objects.
    pHeapProfilerGetObjectByHeapObjectIdObjectGroup :: Maybe String
  }
  deriving (Eq, Show)
pHeapProfilerGetObjectByHeapObjectId
  :: HeapProfilerHeapSnapshotObjectId
  -> PHeapProfilerGetObjectByHeapObjectId
pHeapProfilerGetObjectByHeapObjectId
  arg_pHeapProfilerGetObjectByHeapObjectIdObjectId
  = PHeapProfilerGetObjectByHeapObjectId
    arg_pHeapProfilerGetObjectByHeapObjectIdObjectId
    Nothing
instance ToJSON PHeapProfilerGetObjectByHeapObjectId where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pHeapProfilerGetObjectByHeapObjectIdObjectId p),
    ("objectGroup" A..=) <$> (pHeapProfilerGetObjectByHeapObjectIdObjectGroup p)
    ]
data HeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId
  {
    -- | Evaluation result.
    heapProfilerGetObjectByHeapObjectIdResult :: Runtime.RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerGetObjectByHeapObjectId where
  parseJSON = A.withObject "HeapProfilerGetObjectByHeapObjectId" $ \o -> HeapProfilerGetObjectByHeapObjectId
    <$> o A..: "result"
instance Command PHeapProfilerGetObjectByHeapObjectId where
  type CommandResponse PHeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId
  commandName _ = "HeapProfiler.getObjectByHeapObjectId"


-- | Parameters of the 'HeapProfiler.getSamplingProfile' command.
data PHeapProfilerGetSamplingProfile = PHeapProfilerGetSamplingProfile
  deriving (Eq, Show)
pHeapProfilerGetSamplingProfile
  :: PHeapProfilerGetSamplingProfile
pHeapProfilerGetSamplingProfile
  = PHeapProfilerGetSamplingProfile
instance ToJSON PHeapProfilerGetSamplingProfile where
  toJSON _ = A.Null
data HeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile
  {
    -- | Return the sampling profile being collected.
    heapProfilerGetSamplingProfileProfile :: HeapProfilerSamplingHeapProfile
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerGetSamplingProfile where
  parseJSON = A.withObject "HeapProfilerGetSamplingProfile" $ \o -> HeapProfilerGetSamplingProfile
    <$> o A..: "profile"
instance Command PHeapProfilerGetSamplingProfile where
  type CommandResponse PHeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile
  commandName _ = "HeapProfiler.getSamplingProfile"


-- | Parameters of the 'HeapProfiler.startSampling' command.
data PHeapProfilerStartSampling = PHeapProfilerStartSampling
  {
    -- | Average sample interval in bytes. Poisson distribution is used for the intervals. The
    --   default value is 32768 bytes.
    pHeapProfilerStartSamplingSamplingInterval :: Maybe Double,
    -- | By default, the sampling heap profiler reports only objects which are
    --   still alive when the profile is returned via getSamplingProfile or
    --   stopSampling, which is useful for determining what functions contribute
    --   the most to steady-state memory usage. This flag instructs the sampling
    --   heap profiler to also include information about objects discarded by
    --   major GC, which will show which functions cause large temporary memory
    --   usage or long GC pauses.
    pHeapProfilerStartSamplingIncludeObjectsCollectedByMajorGC :: Maybe Bool,
    -- | By default, the sampling heap profiler reports only objects which are
    --   still alive when the profile is returned via getSamplingProfile or
    --   stopSampling, which is useful for determining what functions contribute
    --   the most to steady-state memory usage. This flag instructs the sampling
    --   heap profiler to also include information about objects discarded by
    --   minor GC, which is useful when tuning a latency-sensitive application
    --   for minimal GC activity.
    pHeapProfilerStartSamplingIncludeObjectsCollectedByMinorGC :: Maybe Bool
  }
  deriving (Eq, Show)
pHeapProfilerStartSampling
  :: PHeapProfilerStartSampling
pHeapProfilerStartSampling
  = PHeapProfilerStartSampling
    Nothing
    Nothing
    Nothing
instance ToJSON PHeapProfilerStartSampling where
  toJSON p = A.object $ catMaybes [
    ("samplingInterval" A..=) <$> (pHeapProfilerStartSamplingSamplingInterval p),
    ("includeObjectsCollectedByMajorGC" A..=) <$> (pHeapProfilerStartSamplingIncludeObjectsCollectedByMajorGC p),
    ("includeObjectsCollectedByMinorGC" A..=) <$> (pHeapProfilerStartSamplingIncludeObjectsCollectedByMinorGC p)
    ]
instance Command PHeapProfilerStartSampling where
  type CommandResponse PHeapProfilerStartSampling = ()
  commandName _ = "HeapProfiler.startSampling"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.startTrackingHeapObjects' command.
data PHeapProfilerStartTrackingHeapObjects = PHeapProfilerStartTrackingHeapObjects
  {
    pHeapProfilerStartTrackingHeapObjectsTrackAllocations :: Maybe Bool
  }
  deriving (Eq, Show)
pHeapProfilerStartTrackingHeapObjects
  :: PHeapProfilerStartTrackingHeapObjects
pHeapProfilerStartTrackingHeapObjects
  = PHeapProfilerStartTrackingHeapObjects
    Nothing
instance ToJSON PHeapProfilerStartTrackingHeapObjects where
  toJSON p = A.object $ catMaybes [
    ("trackAllocations" A..=) <$> (pHeapProfilerStartTrackingHeapObjectsTrackAllocations p)
    ]
instance Command PHeapProfilerStartTrackingHeapObjects where
  type CommandResponse PHeapProfilerStartTrackingHeapObjects = ()
  commandName _ = "HeapProfiler.startTrackingHeapObjects"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.stopSampling' command.
data PHeapProfilerStopSampling = PHeapProfilerStopSampling
  deriving (Eq, Show)
pHeapProfilerStopSampling
  :: PHeapProfilerStopSampling
pHeapProfilerStopSampling
  = PHeapProfilerStopSampling
instance ToJSON PHeapProfilerStopSampling where
  toJSON _ = A.Null
data HeapProfilerStopSampling = HeapProfilerStopSampling
  {
    -- | Recorded sampling heap profile.
    heapProfilerStopSamplingProfile :: HeapProfilerSamplingHeapProfile
  }
  deriving (Eq, Show)
instance FromJSON HeapProfilerStopSampling where
  parseJSON = A.withObject "HeapProfilerStopSampling" $ \o -> HeapProfilerStopSampling
    <$> o A..: "profile"
instance Command PHeapProfilerStopSampling where
  type CommandResponse PHeapProfilerStopSampling = HeapProfilerStopSampling
  commandName _ = "HeapProfiler.stopSampling"


-- | Parameters of the 'HeapProfiler.stopTrackingHeapObjects' command.
data PHeapProfilerStopTrackingHeapObjects = PHeapProfilerStopTrackingHeapObjects
  {
    -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken
    --   when the tracking is stopped.
    pHeapProfilerStopTrackingHeapObjectsReportProgress :: Maybe Bool,
    -- | If true, numerical values are included in the snapshot
    pHeapProfilerStopTrackingHeapObjectsCaptureNumericValue :: Maybe Bool,
    -- | If true, exposes internals of the snapshot.
    pHeapProfilerStopTrackingHeapObjectsExposeInternals :: Maybe Bool
  }
  deriving (Eq, Show)
pHeapProfilerStopTrackingHeapObjects
  :: PHeapProfilerStopTrackingHeapObjects
pHeapProfilerStopTrackingHeapObjects
  = PHeapProfilerStopTrackingHeapObjects
    Nothing
    Nothing
    Nothing
instance ToJSON PHeapProfilerStopTrackingHeapObjects where
  toJSON p = A.object $ catMaybes [
    ("reportProgress" A..=) <$> (pHeapProfilerStopTrackingHeapObjectsReportProgress p),
    ("captureNumericValue" A..=) <$> (pHeapProfilerStopTrackingHeapObjectsCaptureNumericValue p),
    ("exposeInternals" A..=) <$> (pHeapProfilerStopTrackingHeapObjectsExposeInternals p)
    ]
instance Command PHeapProfilerStopTrackingHeapObjects where
  type CommandResponse PHeapProfilerStopTrackingHeapObjects = ()
  commandName _ = "HeapProfiler.stopTrackingHeapObjects"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'HeapProfiler.takeHeapSnapshot' command.
data PHeapProfilerTakeHeapSnapshot = PHeapProfilerTakeHeapSnapshot
  {
    -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken.
    pHeapProfilerTakeHeapSnapshotReportProgress :: Maybe Bool,
    -- | If true, numerical values are included in the snapshot
    pHeapProfilerTakeHeapSnapshotCaptureNumericValue :: Maybe Bool,
    -- | If true, exposes internals of the snapshot.
    pHeapProfilerTakeHeapSnapshotExposeInternals :: Maybe Bool
  }
  deriving (Eq, Show)
pHeapProfilerTakeHeapSnapshot
  :: PHeapProfilerTakeHeapSnapshot
pHeapProfilerTakeHeapSnapshot
  = PHeapProfilerTakeHeapSnapshot
    Nothing
    Nothing
    Nothing
instance ToJSON PHeapProfilerTakeHeapSnapshot where
  toJSON p = A.object $ catMaybes [
    ("reportProgress" A..=) <$> (pHeapProfilerTakeHeapSnapshotReportProgress p),
    ("captureNumericValue" A..=) <$> (pHeapProfilerTakeHeapSnapshotCaptureNumericValue p),
    ("exposeInternals" A..=) <$> (pHeapProfilerTakeHeapSnapshotExposeInternals p)
    ]
instance Command PHeapProfilerTakeHeapSnapshot where
  type CommandResponse PHeapProfilerTakeHeapSnapshot = ()
  commandName _ = "HeapProfiler.takeHeapSnapshot"
  fromJSON = const . A.Success . const ()

