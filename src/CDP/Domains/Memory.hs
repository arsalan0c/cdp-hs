{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Memory 
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
   parseJSON = A.withText  "MemoryPressureLevel"  $ \v -> do
      case v of
         "moderate" -> pure MemoryPressureLevelModerate
         "critical" -> pure MemoryPressureLevelCritical
         _ -> fail "failed to parse MemoryPressureLevel"

instance ToJSON MemoryPressureLevel where
   toJSON v = A.String $
      case v of
         MemoryPressureLevelModerate -> "moderate"
         MemoryPressureLevelCritical -> "critical"



-- | Type 'Memory.SamplingProfileNode'.
--   Heap profile sample.
data MemorySamplingProfileNode = MemorySamplingProfileNode {
  -- | Size of the sampled allocation.
  memorySamplingProfileNodeSize :: Double,
  -- | Total bytes attributed to this sample.
  memorySamplingProfileNodeTotal :: Double,
  -- | Execution stack at the point of allocation.
  memorySamplingProfileNodeStack :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'Memory.SamplingProfile'.
--   Array of heap profile samples.
data MemorySamplingProfile = MemorySamplingProfile {
  memorySamplingProfileSamples :: [MemorySamplingProfileNode],
  memorySamplingProfileModules :: [MemoryModule]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Memory.Module'.
--   Executable module information
data MemoryModule = MemoryModule {
  -- | Name of the module.
  memoryModuleName :: String,
  -- | UUID of the module.
  memoryModuleUuid :: String,
  -- | Base address where the module is loaded into memory. Encoded as a decimal
  --   or hexadecimal (0x prefixed) string.
  memoryModuleBaseAddress :: String,
  -- | Size of the module in bytes.
  memoryModuleSize :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemoryModule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  MemoryModule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }







-- | Memory.getDOMCounters

-- | Parameters of the 'Memory.getDOMCounters' command.
data PMemoryGetDOMCounters = PMemoryGetDOMCounters
instance ToJSON PMemoryGetDOMCounters where toJSON _ = A.Null

-- | Return type of the 'Memory.getDOMCounters' command.
data MemoryGetDOMCounters = MemoryGetDOMCounters {
  memoryGetDOMCountersDocuments :: Int,
  memoryGetDOMCountersNodes :: Int,
  memoryGetDOMCountersJsEventListeners :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetDOMCounters where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PMemoryGetDOMCounters where
   type CommandResponse PMemoryGetDOMCounters = MemoryGetDOMCounters
   commandName _ = "Memory.getDOMCounters"



-- | Memory.prepareForLeakDetection

-- | Parameters of the 'Memory.prepareForLeakDetection' command.
data PMemoryPrepareForLeakDetection = PMemoryPrepareForLeakDetection
instance ToJSON PMemoryPrepareForLeakDetection where toJSON _ = A.Null

instance Command PMemoryPrepareForLeakDetection where
   type CommandResponse PMemoryPrepareForLeakDetection = ()
   commandName _ = "Memory.prepareForLeakDetection"
   fromJSON = const . A.Success . const ()


-- | Memory.forciblyPurgeJavaScriptMemory
--   Simulate OomIntervention by purging V8 memory.

-- | Parameters of the 'Memory.forciblyPurgeJavaScriptMemory' command.
data PMemoryForciblyPurgeJavaScriptMemory = PMemoryForciblyPurgeJavaScriptMemory
instance ToJSON PMemoryForciblyPurgeJavaScriptMemory where toJSON _ = A.Null

instance Command PMemoryForciblyPurgeJavaScriptMemory where
   type CommandResponse PMemoryForciblyPurgeJavaScriptMemory = ()
   commandName _ = "Memory.forciblyPurgeJavaScriptMemory"
   fromJSON = const . A.Success . const ()


-- | Memory.setPressureNotificationsSuppressed
--   Enable/disable suppressing memory pressure notifications in all processes.

-- | Parameters of the 'Memory.setPressureNotificationsSuppressed' command.
data PMemorySetPressureNotificationsSuppressed = PMemorySetPressureNotificationsSuppressed {
  -- | If true, memory pressure notifications will be suppressed.
  pMemorySetPressureNotificationsSuppressedSuppressed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySetPressureNotificationsSuppressed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PMemorySetPressureNotificationsSuppressed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


instance Command PMemorySetPressureNotificationsSuppressed where
   type CommandResponse PMemorySetPressureNotificationsSuppressed = ()
   commandName _ = "Memory.setPressureNotificationsSuppressed"
   fromJSON = const . A.Success . const ()


-- | Memory.simulatePressureNotification
--   Simulate a memory pressure notification in all processes.

-- | Parameters of the 'Memory.simulatePressureNotification' command.
data PMemorySimulatePressureNotification = PMemorySimulatePressureNotification {
  -- | Memory pressure level of the notification.
  pMemorySimulatePressureNotificationLevel :: MemoryPressureLevel
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySimulatePressureNotification  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PMemorySimulatePressureNotification where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PMemorySimulatePressureNotification where
   type CommandResponse PMemorySimulatePressureNotification = ()
   commandName _ = "Memory.simulatePressureNotification"
   fromJSON = const . A.Success . const ()


-- | Memory.startSampling
--   Start collecting native memory profile.

-- | Parameters of the 'Memory.startSampling' command.
data PMemoryStartSampling = PMemoryStartSampling {
  -- | Average number of bytes between samples.
  pMemoryStartSamplingSamplingInterval :: Maybe Int,
  -- | Do not randomize intervals between samples.
  pMemoryStartSamplingSuppressRandomness :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemoryStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PMemoryStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command PMemoryStartSampling where
   type CommandResponse PMemoryStartSampling = ()
   commandName _ = "Memory.startSampling"
   fromJSON = const . A.Success . const ()


-- | Memory.stopSampling
--   Stop collecting native memory profile.

-- | Parameters of the 'Memory.stopSampling' command.
data PMemoryStopSampling = PMemoryStopSampling
instance ToJSON PMemoryStopSampling where toJSON _ = A.Null

instance Command PMemoryStopSampling where
   type CommandResponse PMemoryStopSampling = ()
   commandName _ = "Memory.stopSampling"
   fromJSON = const . A.Success . const ()


-- | Memory.getAllTimeSamplingProfile
--   Retrieve native memory allocations profile
--   collected since renderer process startup.

-- | Parameters of the 'Memory.getAllTimeSamplingProfile' command.
data PMemoryGetAllTimeSamplingProfile = PMemoryGetAllTimeSamplingProfile
instance ToJSON PMemoryGetAllTimeSamplingProfile where toJSON _ = A.Null

-- | Return type of the 'Memory.getAllTimeSamplingProfile' command.
data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile {
  memoryGetAllTimeSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetAllTimeSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PMemoryGetAllTimeSamplingProfile where
   type CommandResponse PMemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile
   commandName _ = "Memory.getAllTimeSamplingProfile"



-- | Memory.getBrowserSamplingProfile
--   Retrieve native memory allocations profile
--   collected since browser process startup.

-- | Parameters of the 'Memory.getBrowserSamplingProfile' command.
data PMemoryGetBrowserSamplingProfile = PMemoryGetBrowserSamplingProfile
instance ToJSON PMemoryGetBrowserSamplingProfile where toJSON _ = A.Null

-- | Return type of the 'Memory.getBrowserSamplingProfile' command.
data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile {
  memoryGetBrowserSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetBrowserSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PMemoryGetBrowserSamplingProfile where
   type CommandResponse PMemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile
   commandName _ = "Memory.getBrowserSamplingProfile"



-- | Memory.getSamplingProfile
--   Retrieve native memory allocations profile collected since last
--   `startSampling` call.

-- | Parameters of the 'Memory.getSamplingProfile' command.
data PMemoryGetSamplingProfile = PMemoryGetSamplingProfile
instance ToJSON PMemoryGetSamplingProfile where toJSON _ = A.Null

-- | Return type of the 'Memory.getSamplingProfile' command.
data MemoryGetSamplingProfile = MemoryGetSamplingProfile {
  memoryGetSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PMemoryGetSamplingProfile where
   type CommandResponse PMemoryGetSamplingProfile = MemoryGetSamplingProfile
   commandName _ = "Memory.getSamplingProfile"




