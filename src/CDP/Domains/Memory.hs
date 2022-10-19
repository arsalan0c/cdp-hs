{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Memory

-}


module CDP.Domains.Memory (module CDP.Domains.Memory) where

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




-- | Type 'Memory.PressureLevel'.
--   Memory pressure level.
data MemoryPressureLevel = MemoryPressureLevelModerate | MemoryPressureLevelCritical
  deriving (Ord, Eq, Show, Read)
instance FromJSON MemoryPressureLevel where
  parseJSON = A.withText "MemoryPressureLevel" $ \v -> case v of
    "moderate" -> pure MemoryPressureLevelModerate
    "critical" -> pure MemoryPressureLevelCritical
    "_" -> fail "failed to parse MemoryPressureLevel"
instance ToJSON MemoryPressureLevel where
  toJSON v = A.String $ case v of
    MemoryPressureLevelModerate -> "moderate"
    MemoryPressureLevelCritical -> "critical"

-- | Type 'Memory.SamplingProfileNode'.
--   Heap profile sample.
data MemorySamplingProfileNode = MemorySamplingProfileNode
  {
    -- | Size of the sampled allocation.
    memorySamplingProfileNodeSize :: Double,
    -- | Total bytes attributed to this sample.
    memorySamplingProfileNodeTotal :: Double,
    -- | Execution stack at the point of allocation.
    memorySamplingProfileNodeStack :: [String]
  }
  deriving (Eq, Show)
instance FromJSON MemorySamplingProfileNode where
  parseJSON = A.withObject "MemorySamplingProfileNode" $ \o -> MemorySamplingProfileNode
    <$> o A..: "size"
    <*> o A..: "total"
    <*> o A..: "stack"
instance ToJSON MemorySamplingProfileNode where
  toJSON p = A.object $ catMaybes [
    ("size" A..=) <$> Just (memorySamplingProfileNodeSize p),
    ("total" A..=) <$> Just (memorySamplingProfileNodeTotal p),
    ("stack" A..=) <$> Just (memorySamplingProfileNodeStack p)
    ]

-- | Type 'Memory.SamplingProfile'.
--   Array of heap profile samples.
data MemorySamplingProfile = MemorySamplingProfile
  {
    memorySamplingProfileSamples :: [MemorySamplingProfileNode],
    memorySamplingProfileModules :: [MemoryModule]
  }
  deriving (Eq, Show)
instance FromJSON MemorySamplingProfile where
  parseJSON = A.withObject "MemorySamplingProfile" $ \o -> MemorySamplingProfile
    <$> o A..: "samples"
    <*> o A..: "modules"
instance ToJSON MemorySamplingProfile where
  toJSON p = A.object $ catMaybes [
    ("samples" A..=) <$> Just (memorySamplingProfileSamples p),
    ("modules" A..=) <$> Just (memorySamplingProfileModules p)
    ]

-- | Type 'Memory.Module'.
--   Executable module information
data MemoryModule = MemoryModule
  {
    -- | Name of the module.
    memoryModuleName :: String,
    -- | UUID of the module.
    memoryModuleUuid :: String,
    -- | Base address where the module is loaded into memory. Encoded as a decimal
    --   or hexadecimal (0x prefixed) string.
    memoryModuleBaseAddress :: String,
    -- | Size of the module in bytes.
    memoryModuleSize :: Double
  }
  deriving (Eq, Show)
instance FromJSON MemoryModule where
  parseJSON = A.withObject "MemoryModule" $ \o -> MemoryModule
    <$> o A..: "name"
    <*> o A..: "uuid"
    <*> o A..: "baseAddress"
    <*> o A..: "size"
instance ToJSON MemoryModule where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (memoryModuleName p),
    ("uuid" A..=) <$> Just (memoryModuleUuid p),
    ("baseAddress" A..=) <$> Just (memoryModuleBaseAddress p),
    ("size" A..=) <$> Just (memoryModuleSize p)
    ]


-- | Parameters of the 'Memory.getDOMCounters' command.
data PMemoryGetDOMCounters = PMemoryGetDOMCounters
  deriving (Eq, Show)
pMemoryGetDOMCounters
  :: PMemoryGetDOMCounters
pMemoryGetDOMCounters
  = PMemoryGetDOMCounters
instance ToJSON PMemoryGetDOMCounters where
  toJSON _ = A.Null
data MemoryGetDOMCounters = MemoryGetDOMCounters
  {
    memoryGetDOMCountersDocuments :: Int,
    memoryGetDOMCountersNodes :: Int,
    memoryGetDOMCountersJsEventListeners :: Int
  }
  deriving (Eq, Show)
instance FromJSON MemoryGetDOMCounters where
  parseJSON = A.withObject "MemoryGetDOMCounters" $ \o -> MemoryGetDOMCounters
    <$> o A..: "documents"
    <*> o A..: "nodes"
    <*> o A..: "jsEventListeners"
instance Command PMemoryGetDOMCounters where
  type CommandResponse PMemoryGetDOMCounters = MemoryGetDOMCounters
  commandName _ = "Memory.getDOMCounters"


-- | Parameters of the 'Memory.prepareForLeakDetection' command.
data PMemoryPrepareForLeakDetection = PMemoryPrepareForLeakDetection
  deriving (Eq, Show)
pMemoryPrepareForLeakDetection
  :: PMemoryPrepareForLeakDetection
pMemoryPrepareForLeakDetection
  = PMemoryPrepareForLeakDetection
instance ToJSON PMemoryPrepareForLeakDetection where
  toJSON _ = A.Null
instance Command PMemoryPrepareForLeakDetection where
  type CommandResponse PMemoryPrepareForLeakDetection = ()
  commandName _ = "Memory.prepareForLeakDetection"
  fromJSON = const . A.Success . const ()

-- | Simulate OomIntervention by purging V8 memory.

-- | Parameters of the 'Memory.forciblyPurgeJavaScriptMemory' command.
data PMemoryForciblyPurgeJavaScriptMemory = PMemoryForciblyPurgeJavaScriptMemory
  deriving (Eq, Show)
pMemoryForciblyPurgeJavaScriptMemory
  :: PMemoryForciblyPurgeJavaScriptMemory
pMemoryForciblyPurgeJavaScriptMemory
  = PMemoryForciblyPurgeJavaScriptMemory
instance ToJSON PMemoryForciblyPurgeJavaScriptMemory where
  toJSON _ = A.Null
instance Command PMemoryForciblyPurgeJavaScriptMemory where
  type CommandResponse PMemoryForciblyPurgeJavaScriptMemory = ()
  commandName _ = "Memory.forciblyPurgeJavaScriptMemory"
  fromJSON = const . A.Success . const ()

-- | Enable/disable suppressing memory pressure notifications in all processes.

-- | Parameters of the 'Memory.setPressureNotificationsSuppressed' command.
data PMemorySetPressureNotificationsSuppressed = PMemorySetPressureNotificationsSuppressed
  {
    -- | If true, memory pressure notifications will be suppressed.
    pMemorySetPressureNotificationsSuppressedSuppressed :: Bool
  }
  deriving (Eq, Show)
pMemorySetPressureNotificationsSuppressed
  -- | If true, memory pressure notifications will be suppressed.
  :: Bool
  -> PMemorySetPressureNotificationsSuppressed
pMemorySetPressureNotificationsSuppressed
  arg_pMemorySetPressureNotificationsSuppressedSuppressed
  = PMemorySetPressureNotificationsSuppressed
    arg_pMemorySetPressureNotificationsSuppressedSuppressed
instance ToJSON PMemorySetPressureNotificationsSuppressed where
  toJSON p = A.object $ catMaybes [
    ("suppressed" A..=) <$> Just (pMemorySetPressureNotificationsSuppressedSuppressed p)
    ]
instance Command PMemorySetPressureNotificationsSuppressed where
  type CommandResponse PMemorySetPressureNotificationsSuppressed = ()
  commandName _ = "Memory.setPressureNotificationsSuppressed"
  fromJSON = const . A.Success . const ()

-- | Simulate a memory pressure notification in all processes.

-- | Parameters of the 'Memory.simulatePressureNotification' command.
data PMemorySimulatePressureNotification = PMemorySimulatePressureNotification
  {
    -- | Memory pressure level of the notification.
    pMemorySimulatePressureNotificationLevel :: MemoryPressureLevel
  }
  deriving (Eq, Show)
pMemorySimulatePressureNotification
  -- | Memory pressure level of the notification.
  :: MemoryPressureLevel
  -> PMemorySimulatePressureNotification
pMemorySimulatePressureNotification
  arg_pMemorySimulatePressureNotificationLevel
  = PMemorySimulatePressureNotification
    arg_pMemorySimulatePressureNotificationLevel
instance ToJSON PMemorySimulatePressureNotification where
  toJSON p = A.object $ catMaybes [
    ("level" A..=) <$> Just (pMemorySimulatePressureNotificationLevel p)
    ]
instance Command PMemorySimulatePressureNotification where
  type CommandResponse PMemorySimulatePressureNotification = ()
  commandName _ = "Memory.simulatePressureNotification"
  fromJSON = const . A.Success . const ()

-- | Start collecting native memory profile.

-- | Parameters of the 'Memory.startSampling' command.
data PMemoryStartSampling = PMemoryStartSampling
  {
    -- | Average number of bytes between samples.
    pMemoryStartSamplingSamplingInterval :: Maybe Int,
    -- | Do not randomize intervals between samples.
    pMemoryStartSamplingSuppressRandomness :: Maybe Bool
  }
  deriving (Eq, Show)
pMemoryStartSampling
  :: PMemoryStartSampling
pMemoryStartSampling
  = PMemoryStartSampling
    Nothing
    Nothing
instance ToJSON PMemoryStartSampling where
  toJSON p = A.object $ catMaybes [
    ("samplingInterval" A..=) <$> (pMemoryStartSamplingSamplingInterval p),
    ("suppressRandomness" A..=) <$> (pMemoryStartSamplingSuppressRandomness p)
    ]
instance Command PMemoryStartSampling where
  type CommandResponse PMemoryStartSampling = ()
  commandName _ = "Memory.startSampling"
  fromJSON = const . A.Success . const ()

-- | Stop collecting native memory profile.

-- | Parameters of the 'Memory.stopSampling' command.
data PMemoryStopSampling = PMemoryStopSampling
  deriving (Eq, Show)
pMemoryStopSampling
  :: PMemoryStopSampling
pMemoryStopSampling
  = PMemoryStopSampling
instance ToJSON PMemoryStopSampling where
  toJSON _ = A.Null
instance Command PMemoryStopSampling where
  type CommandResponse PMemoryStopSampling = ()
  commandName _ = "Memory.stopSampling"
  fromJSON = const . A.Success . const ()

-- | Retrieve native memory allocations profile
--   collected since renderer process startup.

-- | Parameters of the 'Memory.getAllTimeSamplingProfile' command.
data PMemoryGetAllTimeSamplingProfile = PMemoryGetAllTimeSamplingProfile
  deriving (Eq, Show)
pMemoryGetAllTimeSamplingProfile
  :: PMemoryGetAllTimeSamplingProfile
pMemoryGetAllTimeSamplingProfile
  = PMemoryGetAllTimeSamplingProfile
instance ToJSON PMemoryGetAllTimeSamplingProfile where
  toJSON _ = A.Null
data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile
  {
    memoryGetAllTimeSamplingProfileProfile :: MemorySamplingProfile
  }
  deriving (Eq, Show)
instance FromJSON MemoryGetAllTimeSamplingProfile where
  parseJSON = A.withObject "MemoryGetAllTimeSamplingProfile" $ \o -> MemoryGetAllTimeSamplingProfile
    <$> o A..: "profile"
instance Command PMemoryGetAllTimeSamplingProfile where
  type CommandResponse PMemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile
  commandName _ = "Memory.getAllTimeSamplingProfile"

-- | Retrieve native memory allocations profile
--   collected since browser process startup.

-- | Parameters of the 'Memory.getBrowserSamplingProfile' command.
data PMemoryGetBrowserSamplingProfile = PMemoryGetBrowserSamplingProfile
  deriving (Eq, Show)
pMemoryGetBrowserSamplingProfile
  :: PMemoryGetBrowserSamplingProfile
pMemoryGetBrowserSamplingProfile
  = PMemoryGetBrowserSamplingProfile
instance ToJSON PMemoryGetBrowserSamplingProfile where
  toJSON _ = A.Null
data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile
  {
    memoryGetBrowserSamplingProfileProfile :: MemorySamplingProfile
  }
  deriving (Eq, Show)
instance FromJSON MemoryGetBrowserSamplingProfile where
  parseJSON = A.withObject "MemoryGetBrowserSamplingProfile" $ \o -> MemoryGetBrowserSamplingProfile
    <$> o A..: "profile"
instance Command PMemoryGetBrowserSamplingProfile where
  type CommandResponse PMemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile
  commandName _ = "Memory.getBrowserSamplingProfile"

-- | Retrieve native memory allocations profile collected since last
--   `startSampling` call.

-- | Parameters of the 'Memory.getSamplingProfile' command.
data PMemoryGetSamplingProfile = PMemoryGetSamplingProfile
  deriving (Eq, Show)
pMemoryGetSamplingProfile
  :: PMemoryGetSamplingProfile
pMemoryGetSamplingProfile
  = PMemoryGetSamplingProfile
instance ToJSON PMemoryGetSamplingProfile where
  toJSON _ = A.Null
data MemoryGetSamplingProfile = MemoryGetSamplingProfile
  {
    memoryGetSamplingProfileProfile :: MemorySamplingProfile
  }
  deriving (Eq, Show)
instance FromJSON MemoryGetSamplingProfile where
  parseJSON = A.withObject "MemoryGetSamplingProfile" $ \o -> MemoryGetSamplingProfile
    <$> o A..: "profile"
instance Command PMemoryGetSamplingProfile where
  type CommandResponse PMemoryGetSamplingProfile = MemoryGetSamplingProfile
  commandName _ = "Memory.getSamplingProfile"

