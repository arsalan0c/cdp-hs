{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type HeapProfilerHeapSnapshotObjectId = String

data HeapProfilerSamplingHeapProfileNode = HeapProfilerSamplingHeapProfileNode {
   heapProfilerSamplingHeapProfileNodeCallFrame :: Runtime.RuntimeCallFrame,
   heapProfilerSamplingHeapProfileNodeSelfSize :: Double,
   heapProfilerSamplingHeapProfileNodeId :: Int,
   heapProfilerSamplingHeapProfileNodeChildren :: [HeapProfilerSamplingHeapProfileNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



data HeapProfilerSamplingHeapProfileSample = HeapProfilerSamplingHeapProfileSample {
   heapProfilerSamplingHeapProfileSampleSize :: Double,
   heapProfilerSamplingHeapProfileSampleNodeId :: Int,
   heapProfilerSamplingHeapProfileSampleOrdinal :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfileSample  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfileSample where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



data HeapProfilerSamplingHeapProfile = HeapProfilerSamplingHeapProfile {
   heapProfilerSamplingHeapProfileHead :: HeapProfilerSamplingHeapProfileNode,
   heapProfilerSamplingHeapProfileSamples :: [HeapProfilerSamplingHeapProfileSample]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerSamplingHeapProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerSamplingHeapProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }





data HeapProfilerAddHeapSnapshotChunk = HeapProfilerAddHeapSnapshotChunk {
   heapProfilerAddHeapSnapshotChunkChunk :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerAddHeapSnapshotChunk  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerAddHeapSnapshotChunk where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



data HeapProfilerHeapStatsUpdate = HeapProfilerHeapStatsUpdate {
   heapProfilerHeapStatsUpdateStatsUpdate :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerHeapStatsUpdate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerHeapStatsUpdate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data HeapProfilerLastSeenObjectId = HeapProfilerLastSeenObjectId {
   heapProfilerLastSeenObjectIdLastSeenObjectId :: Int,
   heapProfilerLastSeenObjectIdTimestamp :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerLastSeenObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerLastSeenObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data HeapProfilerReportHeapSnapshotProgress = HeapProfilerReportHeapSnapshotProgress {
   heapProfilerReportHeapSnapshotProgressDone :: Int,
   heapProfilerReportHeapSnapshotProgressTotal :: Int,
   heapProfilerReportHeapSnapshotProgressFinished :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeapProfilerReportHeapSnapshotProgress  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  HeapProfilerReportHeapSnapshotProgress where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


data HeapProfilerResetProfiles = HeapProfilerResetProfiles
   deriving (Eq, Show, Read)
instance FromJSON HeapProfilerResetProfiles where
   parseJSON = A.withText  "HeapProfilerResetProfiles"  $ \v -> do
      case v of
         "HeapProfilerResetProfiles" -> pure HeapProfilerResetProfiles
         _ -> fail "failed to parse HeapProfilerResetProfiles"





data PHeapProfilerAddInspectedHeapObject = PHeapProfilerAddInspectedHeapObject {
   pHeapProfilerAddInspectedHeapObjectHeapObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerAddInspectedHeapObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerAddInspectedHeapObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


heapProfilerAddInspectedHeapObject :: Handle ev -> PHeapProfilerAddInspectedHeapObject -> IO (Maybe Error)
heapProfilerAddInspectedHeapObject handle params = sendReceiveCommand handle "HeapProfiler.addInspectedHeapObject" (Just params)


heapProfilerCollectGarbage :: Handle ev -> IO (Maybe Error)
heapProfilerCollectGarbage handle = sendReceiveCommand handle "HeapProfiler.collectGarbage" (Nothing :: Maybe ())


heapProfilerDisable :: Handle ev -> IO (Maybe Error)
heapProfilerDisable handle = sendReceiveCommand handle "HeapProfiler.disable" (Nothing :: Maybe ())


heapProfilerEnable :: Handle ev -> IO (Maybe Error)
heapProfilerEnable handle = sendReceiveCommand handle "HeapProfiler.enable" (Nothing :: Maybe ())



data PHeapProfilerGetHeapObjectId = PHeapProfilerGetHeapObjectId {
   pHeapProfilerGetHeapObjectIdObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


heapProfilerGetHeapObjectId :: Handle ev -> PHeapProfilerGetHeapObjectId -> IO (Either Error HeapProfilerGetHeapObjectId)
heapProfilerGetHeapObjectId handle params = sendReceiveCommandResult handle "HeapProfiler.getHeapObjectId" (Just params)

data HeapProfilerGetHeapObjectId = HeapProfilerGetHeapObjectId {
   heapProfilerGetHeapObjectIdHeapSnapshotObjectId :: HeapProfilerHeapSnapshotObjectId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command HeapProfilerGetHeapObjectId where
   commandName _ = "HeapProfiler.getHeapObjectId"




data PHeapProfilerGetObjectByHeapObjectId = PHeapProfilerGetObjectByHeapObjectId {
   pHeapProfilerGetObjectByHeapObjectIdObjectId :: HeapProfilerHeapSnapshotObjectId,
   pHeapProfilerGetObjectByHeapObjectIdObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerGetObjectByHeapObjectId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


heapProfilerGetObjectByHeapObjectId :: Handle ev -> PHeapProfilerGetObjectByHeapObjectId -> IO (Either Error HeapProfilerGetObjectByHeapObjectId)
heapProfilerGetObjectByHeapObjectId handle params = sendReceiveCommandResult handle "HeapProfiler.getObjectByHeapObjectId" (Just params)

data HeapProfilerGetObjectByHeapObjectId = HeapProfilerGetObjectByHeapObjectId {
   heapProfilerGetObjectByHeapObjectIdResult :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetObjectByHeapObjectId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command HeapProfilerGetObjectByHeapObjectId where
   commandName _ = "HeapProfiler.getObjectByHeapObjectId"



heapProfilerGetSamplingProfile :: Handle ev -> IO (Either Error HeapProfilerGetSamplingProfile)
heapProfilerGetSamplingProfile handle = sendReceiveCommandResult handle "HeapProfiler.getSamplingProfile" (Nothing :: Maybe ())

data HeapProfilerGetSamplingProfile = HeapProfilerGetSamplingProfile {
   heapProfilerGetSamplingProfileProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command HeapProfilerGetSamplingProfile where
   commandName _ = "HeapProfiler.getSamplingProfile"




data PHeapProfilerStartSampling = PHeapProfilerStartSampling {
   pHeapProfilerStartSamplingSamplingInterval :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


heapProfilerStartSampling :: Handle ev -> PHeapProfilerStartSampling -> IO (Maybe Error)
heapProfilerStartSampling handle params = sendReceiveCommand handle "HeapProfiler.startSampling" (Just params)



data PHeapProfilerStartTrackingHeapObjects = PHeapProfilerStartTrackingHeapObjects {
   pHeapProfilerStartTrackingHeapObjectsTrackAllocations :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStartTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStartTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


heapProfilerStartTrackingHeapObjects :: Handle ev -> PHeapProfilerStartTrackingHeapObjects -> IO (Maybe Error)
heapProfilerStartTrackingHeapObjects handle params = sendReceiveCommand handle "HeapProfiler.startTrackingHeapObjects" (Just params)


heapProfilerStopSampling :: Handle ev -> IO (Either Error HeapProfilerStopSampling)
heapProfilerStopSampling handle = sendReceiveCommandResult handle "HeapProfiler.stopSampling" (Nothing :: Maybe ())

data HeapProfilerStopSampling = HeapProfilerStopSampling {
   heapProfilerStopSamplingProfile :: HeapProfilerSamplingHeapProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeapProfilerStopSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command HeapProfilerStopSampling where
   commandName _ = "HeapProfiler.stopSampling"




data PHeapProfilerStopTrackingHeapObjects = PHeapProfilerStopTrackingHeapObjects {
   pHeapProfilerStopTrackingHeapObjectsReportProgress :: Maybe Bool,
   pHeapProfilerStopTrackingHeapObjectsTreatGlobalObjectsAsRoots :: Maybe Bool,
   pHeapProfilerStopTrackingHeapObjectsCaptureNumericValue :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerStopTrackingHeapObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerStopTrackingHeapObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


heapProfilerStopTrackingHeapObjects :: Handle ev -> PHeapProfilerStopTrackingHeapObjects -> IO (Maybe Error)
heapProfilerStopTrackingHeapObjects handle params = sendReceiveCommand handle "HeapProfiler.stopTrackingHeapObjects" (Just params)



data PHeapProfilerTakeHeapSnapshot = PHeapProfilerTakeHeapSnapshot {
   pHeapProfilerTakeHeapSnapshotReportProgress :: Maybe Bool,
   pHeapProfilerTakeHeapSnapshotTreatGlobalObjectsAsRoots :: Maybe Bool,
   pHeapProfilerTakeHeapSnapshotCaptureNumericValue :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeapProfilerTakeHeapSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PHeapProfilerTakeHeapSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


heapProfilerTakeHeapSnapshot :: Handle ev -> PHeapProfilerTakeHeapSnapshot -> IO (Maybe Error)
heapProfilerTakeHeapSnapshot handle params = sendReceiveCommand handle "HeapProfiler.takeHeapSnapshot" (Just params)



