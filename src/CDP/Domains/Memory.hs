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

import CDP.Internal.Runtime




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







-- | Parameters of the 'memoryGetDOMCounters' command.
data PMemoryGetDOMCounters = PMemoryGetDOMCounters
instance ToJSON PMemoryGetDOMCounters where toJSON _ = A.Null

-- | Function for the 'Memory.getDOMCounters' command.
--   
--   Returns: 'MemoryGetDOMCounters'
memoryGetDOMCounters :: Handle -> IO MemoryGetDOMCounters
memoryGetDOMCounters handle = sendReceiveCommandResult handle PMemoryGetDOMCounters

-- | Return type of the 'memoryGetDOMCounters' command.
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


-- | Parameters of the 'memoryPrepareForLeakDetection' command.
data PMemoryPrepareForLeakDetection = PMemoryPrepareForLeakDetection
instance ToJSON PMemoryPrepareForLeakDetection where toJSON _ = A.Null

-- | Function for the 'Memory.prepareForLeakDetection' command.
memoryPrepareForLeakDetection :: Handle -> IO ()
memoryPrepareForLeakDetection handle = sendReceiveCommand handle PMemoryPrepareForLeakDetection

instance Command PMemoryPrepareForLeakDetection where
    type CommandResponse PMemoryPrepareForLeakDetection = NoResponse
    commandName _ = "Memory.prepareForLeakDetection"


-- | Parameters of the 'memoryForciblyPurgeJavaScriptMemory' command.
data PMemoryForciblyPurgeJavaScriptMemory = PMemoryForciblyPurgeJavaScriptMemory
instance ToJSON PMemoryForciblyPurgeJavaScriptMemory where toJSON _ = A.Null

-- | Function for the 'Memory.forciblyPurgeJavaScriptMemory' command.
--   Simulate OomIntervention by purging V8 memory.
memoryForciblyPurgeJavaScriptMemory :: Handle -> IO ()
memoryForciblyPurgeJavaScriptMemory handle = sendReceiveCommand handle PMemoryForciblyPurgeJavaScriptMemory

instance Command PMemoryForciblyPurgeJavaScriptMemory where
    type CommandResponse PMemoryForciblyPurgeJavaScriptMemory = NoResponse
    commandName _ = "Memory.forciblyPurgeJavaScriptMemory"


-- | Parameters of the 'memorySetPressureNotificationsSuppressed' command.
data PMemorySetPressureNotificationsSuppressed = PMemorySetPressureNotificationsSuppressed {
  -- | If true, memory pressure notifications will be suppressed.
  pMemorySetPressureNotificationsSuppressedSuppressed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySetPressureNotificationsSuppressed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PMemorySetPressureNotificationsSuppressed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


-- | Function for the 'Memory.setPressureNotificationsSuppressed' command.
--   Enable/disable suppressing memory pressure notifications in all processes.
--   Returns: 'PMemorySetPressureNotificationsSuppressed'
memorySetPressureNotificationsSuppressed :: Handle -> PMemorySetPressureNotificationsSuppressed -> IO ()
memorySetPressureNotificationsSuppressed handle params = sendReceiveCommand handle params

instance Command PMemorySetPressureNotificationsSuppressed where
    type CommandResponse PMemorySetPressureNotificationsSuppressed = NoResponse
    commandName _ = "Memory.setPressureNotificationsSuppressed"


-- | Parameters of the 'memorySimulatePressureNotification' command.
data PMemorySimulatePressureNotification = PMemorySimulatePressureNotification {
  -- | Memory pressure level of the notification.
  pMemorySimulatePressureNotificationLevel :: MemoryPressureLevel
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySimulatePressureNotification  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PMemorySimulatePressureNotification where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'Memory.simulatePressureNotification' command.
--   Simulate a memory pressure notification in all processes.
--   Returns: 'PMemorySimulatePressureNotification'
memorySimulatePressureNotification :: Handle -> PMemorySimulatePressureNotification -> IO ()
memorySimulatePressureNotification handle params = sendReceiveCommand handle params

instance Command PMemorySimulatePressureNotification where
    type CommandResponse PMemorySimulatePressureNotification = NoResponse
    commandName _ = "Memory.simulatePressureNotification"


-- | Parameters of the 'memoryStartSampling' command.
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


-- | Function for the 'Memory.startSampling' command.
--   Start collecting native memory profile.
--   Returns: 'PMemoryStartSampling'
memoryStartSampling :: Handle -> PMemoryStartSampling -> IO ()
memoryStartSampling handle params = sendReceiveCommand handle params

instance Command PMemoryStartSampling where
    type CommandResponse PMemoryStartSampling = NoResponse
    commandName _ = "Memory.startSampling"


-- | Parameters of the 'memoryStopSampling' command.
data PMemoryStopSampling = PMemoryStopSampling
instance ToJSON PMemoryStopSampling where toJSON _ = A.Null

-- | Function for the 'Memory.stopSampling' command.
--   Stop collecting native memory profile.
memoryStopSampling :: Handle -> IO ()
memoryStopSampling handle = sendReceiveCommand handle PMemoryStopSampling

instance Command PMemoryStopSampling where
    type CommandResponse PMemoryStopSampling = NoResponse
    commandName _ = "Memory.stopSampling"


-- | Parameters of the 'memoryGetAllTimeSamplingProfile' command.
data PMemoryGetAllTimeSamplingProfile = PMemoryGetAllTimeSamplingProfile
instance ToJSON PMemoryGetAllTimeSamplingProfile where toJSON _ = A.Null

-- | Function for the 'Memory.getAllTimeSamplingProfile' command.
--   Retrieve native memory allocations profile
--   collected since renderer process startup.
--   Returns: 'MemoryGetAllTimeSamplingProfile'
memoryGetAllTimeSamplingProfile :: Handle -> IO MemoryGetAllTimeSamplingProfile
memoryGetAllTimeSamplingProfile handle = sendReceiveCommandResult handle PMemoryGetAllTimeSamplingProfile

-- | Return type of the 'memoryGetAllTimeSamplingProfile' command.
data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile {
  memoryGetAllTimeSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetAllTimeSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PMemoryGetAllTimeSamplingProfile where
    type CommandResponse PMemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile
    commandName _ = "Memory.getAllTimeSamplingProfile"


-- | Parameters of the 'memoryGetBrowserSamplingProfile' command.
data PMemoryGetBrowserSamplingProfile = PMemoryGetBrowserSamplingProfile
instance ToJSON PMemoryGetBrowserSamplingProfile where toJSON _ = A.Null

-- | Function for the 'Memory.getBrowserSamplingProfile' command.
--   Retrieve native memory allocations profile
--   collected since browser process startup.
--   Returns: 'MemoryGetBrowserSamplingProfile'
memoryGetBrowserSamplingProfile :: Handle -> IO MemoryGetBrowserSamplingProfile
memoryGetBrowserSamplingProfile handle = sendReceiveCommandResult handle PMemoryGetBrowserSamplingProfile

-- | Return type of the 'memoryGetBrowserSamplingProfile' command.
data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile {
  memoryGetBrowserSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetBrowserSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PMemoryGetBrowserSamplingProfile where
    type CommandResponse PMemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile
    commandName _ = "Memory.getBrowserSamplingProfile"


-- | Parameters of the 'memoryGetSamplingProfile' command.
data PMemoryGetSamplingProfile = PMemoryGetSamplingProfile
instance ToJSON PMemoryGetSamplingProfile where toJSON _ = A.Null

-- | Function for the 'Memory.getSamplingProfile' command.
--   Retrieve native memory allocations profile collected since last
--   `startSampling` call.
--   Returns: 'MemoryGetSamplingProfile'
memoryGetSamplingProfile :: Handle -> IO MemoryGetSamplingProfile
memoryGetSamplingProfile handle = sendReceiveCommandResult handle PMemoryGetSamplingProfile

-- | Return type of the 'memoryGetSamplingProfile' command.
data MemoryGetSamplingProfile = MemoryGetSamplingProfile {
  memoryGetSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PMemoryGetSamplingProfile where
    type CommandResponse PMemoryGetSamplingProfile = MemoryGetSamplingProfile
    commandName _ = "Memory.getSamplingProfile"



