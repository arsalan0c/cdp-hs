{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


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
import CDP.Handle




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







-- | Function for the 'Memory.getDOMCounters' command.
--   
--   Returns: 'MemoryGetDOMCounters'
memoryGetDOMCounters :: Handle -> IO MemoryGetDOMCounters
memoryGetDOMCounters handle = sendReceiveCommandResult handle "Memory.getDOMCounters" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetDOMCounters' command.
data MemoryGetDOMCounters = MemoryGetDOMCounters {
  memoryGetDOMCountersDocuments :: Int,
  memoryGetDOMCountersNodes :: Int,
  memoryGetDOMCountersJsEventListeners :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetDOMCounters where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command MemoryGetDOMCounters where
   commandName _ = "Memory.getDOMCounters"



-- | Function for the 'Memory.prepareForLeakDetection' command.
memoryPrepareForLeakDetection :: Handle -> IO ()
memoryPrepareForLeakDetection handle = sendReceiveCommand handle "Memory.prepareForLeakDetection" (Nothing :: Maybe ())


-- | Function for the 'Memory.forciblyPurgeJavaScriptMemory' command.
--   Simulate OomIntervention by purging V8 memory.
memoryForciblyPurgeJavaScriptMemory :: Handle -> IO ()
memoryForciblyPurgeJavaScriptMemory handle = sendReceiveCommand handle "Memory.forciblyPurgeJavaScriptMemory" (Nothing :: Maybe ())


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
--   Parameters: 'PMemorySetPressureNotificationsSuppressed'
memorySetPressureNotificationsSuppressed :: Handle -> PMemorySetPressureNotificationsSuppressed -> IO ()
memorySetPressureNotificationsSuppressed handle params = sendReceiveCommand handle "Memory.setPressureNotificationsSuppressed" (Just params)


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
--   Parameters: 'PMemorySimulatePressureNotification'
memorySimulatePressureNotification :: Handle -> PMemorySimulatePressureNotification -> IO ()
memorySimulatePressureNotification handle params = sendReceiveCommand handle "Memory.simulatePressureNotification" (Just params)


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
--   Parameters: 'PMemoryStartSampling'
memoryStartSampling :: Handle -> PMemoryStartSampling -> IO ()
memoryStartSampling handle params = sendReceiveCommand handle "Memory.startSampling" (Just params)


-- | Function for the 'Memory.stopSampling' command.
--   Stop collecting native memory profile.
memoryStopSampling :: Handle -> IO ()
memoryStopSampling handle = sendReceiveCommand handle "Memory.stopSampling" (Nothing :: Maybe ())


-- | Function for the 'Memory.getAllTimeSamplingProfile' command.
--   Retrieve native memory allocations profile
--   collected since renderer process startup.
--   Returns: 'MemoryGetAllTimeSamplingProfile'
memoryGetAllTimeSamplingProfile :: Handle -> IO MemoryGetAllTimeSamplingProfile
memoryGetAllTimeSamplingProfile handle = sendReceiveCommandResult handle "Memory.getAllTimeSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetAllTimeSamplingProfile' command.
data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile {
  memoryGetAllTimeSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetAllTimeSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetAllTimeSamplingProfile where
   commandName _ = "Memory.getAllTimeSamplingProfile"



-- | Function for the 'Memory.getBrowserSamplingProfile' command.
--   Retrieve native memory allocations profile
--   collected since browser process startup.
--   Returns: 'MemoryGetBrowserSamplingProfile'
memoryGetBrowserSamplingProfile :: Handle -> IO MemoryGetBrowserSamplingProfile
memoryGetBrowserSamplingProfile handle = sendReceiveCommandResult handle "Memory.getBrowserSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetBrowserSamplingProfile' command.
data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile {
  memoryGetBrowserSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetBrowserSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetBrowserSamplingProfile where
   commandName _ = "Memory.getBrowserSamplingProfile"



-- | Function for the 'Memory.getSamplingProfile' command.
--   Retrieve native memory allocations profile collected since last
--   `startSampling` call.
--   Returns: 'MemoryGetSamplingProfile'
memoryGetSamplingProfile :: Handle -> IO MemoryGetSamplingProfile
memoryGetSamplingProfile handle = sendReceiveCommandResult handle "Memory.getSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetSamplingProfile' command.
data MemoryGetSamplingProfile = MemoryGetSamplingProfile {
  memoryGetSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command MemoryGetSamplingProfile where
   commandName _ = "Memory.getSamplingProfile"




