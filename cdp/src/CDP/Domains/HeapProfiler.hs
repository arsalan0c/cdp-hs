{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.Runtime as Runtime


-- | Heap snapshot object id.
type HeapProfilerHeapSnapshotObjectId = String

-- | Sampling Heap Profile node. Holds callsite information, allocation statistics and child nodes.
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



-- | A single sample from a sampling profile.
data HeapProfilerSamplingHeapProfileSample = HeapProfilerSamplingHeapProfileSample {
  -- | Allocation size in bytes attributed to the sample.
  heapProfilerSamplingHeapProfileSampleSize :: Double,
  -- | Id of the corresponding profile tree node.
  heapProfilerSamplingHeapProfileSampleNodeId :: Int,
  -- | Time-ordered sample ordinal number. It is unique across all profiles retrieved
  -- between startSampling and stopSampling.
  heapProfilerSamplingHeapProfileSampleOrdinal :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfileSample  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfileSample where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



-- | Sampling profile.
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



-- | Type of the 'HeapProfiler.heapStatsUpdate' event.
data HeapProfilerHeapStatsUpdate = HeapProfilerHeapStatsUpdate {
  -- | An array of triplets. Each triplet describes a fragment. The first integer is the fragment
  -- index, the second integer is a total count of objects for the fragment, the third integer is
  -- a total size of the objects for the fragment.
  heapProfilerHeapStatsUpdateStatsUpdate :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerHeapStatsUpdate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerHeapStatsUpdate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'HeapProfiler.lastSeenObjectId' event.
data HeapProfilerLastSeenObjectId = HeapProfilerLastSeenObjectId {
  heapProfilerLastSeenObjectIdLastSeenObjectId :: Int,
  heapProfilerLastSeenObjectIdTimestamp :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerLastSeenObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerLastSeenObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



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



-- | Type of the 'HeapProfiler.resetProfiles' event.
data HeapProfilerResetProfiles = HeapProfilerResetProfiles
   deriving (Eq, Show, Read)
instance FromJSON HeapProfilerResetProfiles where
   parseJSON = A.withText  "HeapProfilerResetProfiles"  $ \v -> do
      case v of
         "HeapProfilerResetProfiles" -> pure HeapProfilerResetProfiles
         _ -> fail "failed to parse HeapProfilerResetProfiles"





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
-- Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
-- Parameters: 'PHeapProfilerAddInspectedHeapObject'
heapProfilerAddInspectedHeapObject :: Handle ev -> PHeapProfilerAddInspectedHeapObject -> IO (Maybe Error)
heapProfilerAddInspectedHeapObject handle params = sendReceiveCommand handle "HeapProfiler.addInspectedHeapObject" (Just params)


-- | Function for the 'HeapProfiler.collectGarbage' command.
heapProfilerCollectGarbage :: Handle ev -> IO (Maybe Error)
heapProfilerCollectGarbage handle = sendReceiveCommand handle "HeapProfiler.collectGarbage" (Nothing :: Maybe ())


-- | Function for the 'HeapProfiler.disable' command.
heapProfilerDisable :: Handle ev -> IO (Maybe Error)
heapProfilerDisable handle = sendReceiveCommand handle "HeapProfiler.disable" (Nothing :: Maybe ())


-- | Function for the 'HeapProfiler.enable' command.
heapProfilerEnable :: Handle ev -> IO (Maybe Error)
heapProfilerEnable handle = sendReceiveCommand handle "HeapProfiler.enable" (Nothing :: Maybe ())


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
-- Parameters: 'PHeapProfilerGetHeapObjectId'
-- Returns: 'HeapProfilerGetHeapObjectId'
heapProfilerGetHeapObjectId :: Handle ev -> PHeapProfilerGetHeapObjectId -> IO (Either Error HeapProfilerGetHeapObjectId)
heapProfilerGetHeapObjectId handle params = sendReceiveCommandResult handle "HeapProfiler.getHeapObjectId" (Just params)

-- | Return type of the 'heapProfilerGetHeapObjectId' command.
data HeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId {
  -- | Id of the heap snapshot object corresponding to the passed remote object id.
  heapProfilerGetHeapObjectIdHeapSnapshotObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command HeapProfilerGetHeapObjectId where
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
-- Parameters: 'PHeapProfilerGetObjectByHeapObjectId'
-- Returns: 'HeapProfilerGetObjectByHeapObjectId'
heapProfilerGetObjectByHeapObjectId :: Handle ev -> PHeapProfilerGetObjectByHeapObjectId -> IO (Either Error HeapProfilerGetObjectByHeapObjectId)
heapProfilerGetObjectByHeapObjectId handle params = sendReceiveCommandResult handle "HeapProfiler.getObjectByHeapObjectId" (Just params)

-- | Return type of the 'heapProfilerGetObjectByHeapObjectId' command.
data HeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId {
  -- | Evaluation result.
  heapProfilerGetObjectByHeapObjectIdResult :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command HeapProfilerGetObjectByHeapObjectId where
   commandName _ = "HeapProfiler.getObjectByHeapObjectId"



-- | Function for the 'HeapProfiler.getSamplingProfile' command.
-- Returns: 'HeapProfilerGetSamplingProfile'
heapProfilerGetSamplingProfile :: Handle ev -> IO (Either Error HeapProfilerGetSamplingProfile)
heapProfilerGetSamplingProfile handle = sendReceiveCommandResult handle "HeapProfiler.getSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'heapProfilerGetSamplingProfile' command.
data HeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile {
  -- | Return the sampling profile being collected.
  heapProfilerGetSamplingProfileProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command HeapProfilerGetSamplingProfile where
   commandName _ = "HeapProfiler.getSamplingProfile"



-- | Parameters of the 'heapProfilerStartSampling' command.
data PHeapProfilerStartSampling = PHeapProfilerStartSampling {
  -- | Average sample interval in bytes. Poisson distribution is used for the intervals. The
  -- default value is 32768 bytes.
  pHeapProfilerStartSamplingSamplingInterval :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'HeapProfiler.startSampling' command.
-- Parameters: 'PHeapProfilerStartSampling'
heapProfilerStartSampling :: Handle ev -> PHeapProfilerStartSampling -> IO (Maybe Error)
heapProfilerStartSampling handle params = sendReceiveCommand handle "HeapProfiler.startSampling" (Just params)


-- | Parameters of the 'heapProfilerStartTrackingHeapObjects' command.
data PHeapProfilerStartTrackingHeapObjects = PHeapProfilerStartTrackingHeapObjects {
  pHeapProfilerStartTrackingHeapObjectsTrackAllocations :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the 'HeapProfiler.startTrackingHeapObjects' command.
-- Parameters: 'PHeapProfilerStartTrackingHeapObjects'
heapProfilerStartTrackingHeapObjects :: Handle ev -> PHeapProfilerStartTrackingHeapObjects -> IO (Maybe Error)
heapProfilerStartTrackingHeapObjects handle params = sendReceiveCommand handle "HeapProfiler.startTrackingHeapObjects" (Just params)


-- | Function for the 'HeapProfiler.stopSampling' command.
-- Returns: 'HeapProfilerStopSampling'
heapProfilerStopSampling :: Handle ev -> IO (Either Error HeapProfilerStopSampling)
heapProfilerStopSampling handle = sendReceiveCommandResult handle "HeapProfiler.stopSampling" (Nothing :: Maybe ())

-- | Return type of the 'heapProfilerStopSampling' command.
data HeapProfilerStopSampling = HeapProfilerStopSampling {
  -- | Recorded sampling heap profile.
  heapProfilerStopSamplingProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerStopSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command HeapProfilerStopSampling where
   commandName _ = "HeapProfiler.stopSampling"



-- | Parameters of the 'heapProfilerStopTrackingHeapObjects' command.
data PHeapProfilerStopTrackingHeapObjects = PHeapProfilerStopTrackingHeapObjects {
  -- | If true 'reportHeapSnapshotProgress' events will be generated while snapshot is being taken
  -- when the tracking is stopped.
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
-- Parameters: 'PHeapProfilerStopTrackingHeapObjects'
heapProfilerStopTrackingHeapObjects :: Handle ev -> PHeapProfilerStopTrackingHeapObjects -> IO (Maybe Error)
heapProfilerStopTrackingHeapObjects handle params = sendReceiveCommand handle "HeapProfiler.stopTrackingHeapObjects" (Just params)


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
-- Parameters: 'PHeapProfilerTakeHeapSnapshot'
heapProfilerTakeHeapSnapshot :: Handle ev -> PHeapProfilerTakeHeapSnapshot -> IO (Maybe Error)
heapProfilerTakeHeapSnapshot handle params = sendReceiveCommand handle "HeapProfiler.takeHeapSnapshot" (Just params)



