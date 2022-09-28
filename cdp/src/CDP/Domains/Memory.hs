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



-- | Memory pressure level.
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



-- | Heap profile sample.
data MemorySamplingProfileNode = MemorySamplingProfileNode {
   memorySamplingProfileNodeSize :: MemorySamplingProfileNodeSize, -- ^ Size of the sampled allocation.
   memorySamplingProfileNodeTotal :: MemorySamplingProfileNodeTotal, -- ^ Total bytes attributed to this sample.
   memorySamplingProfileNodeStack :: MemorySamplingProfileNodeStack -- ^ Execution stack at the point of allocation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Array of heap profile samples.
data MemorySamplingProfile = MemorySamplingProfile {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Executable module information
data MemoryModule = MemoryModule {
   memoryModuleName :: MemoryModuleName, -- ^ Name of the module.
   memoryModuleUuid :: MemoryModuleUuid, -- ^ UUID of the module.
   memoryModuleBaseAddress :: MemoryModuleBaseAddress, -- ^ Base address where the module is loaded into memory. Encoded as a decimal
or hexadecimal (0x prefixed) string.
   memoryModuleSize :: MemoryModuleSize -- ^ Size of the module in bytes.
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemoryModule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  MemoryModule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }







-- | Function for the command 'Memory.getDOMCounters'.
-- Returns: 'MemoryGetDomCounters'
memoryGetDomCounters :: Handle ev -> IO (Either Error MemoryGetDomCounters)
memoryGetDomCounters handle = sendReceiveCommandResult handle "Memory.getDOMCounters" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetDomCounters' command.
data MemoryGetDomCounters = MemoryGetDomCounters {



} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetDomCounters where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command MemoryGetDomCounters where
   commandName _ = "Memory.getDOMCounters"



-- | Function for the command 'Memory.prepareForLeakDetection'.
memoryPrepareForLeakDetection :: Handle ev -> IO (Maybe Error)
memoryPrepareForLeakDetection handle = sendReceiveCommand handle "Memory.prepareForLeakDetection" (Nothing :: Maybe ())


-- | Function for the command 'Memory.forciblyPurgeJavaScriptMemory'.
-- Simulate OomIntervention by purging V8 memory.
memoryForciblyPurgeJavaScriptMemory :: Handle ev -> IO (Maybe Error)
memoryForciblyPurgeJavaScriptMemory handle = sendReceiveCommand handle "Memory.forciblyPurgeJavaScriptMemory" (Nothing :: Maybe ())


-- | Parameters of the 'memorySetPressureNotificationsSuppressed' command.
data PMemorySetPressureNotificationsSuppressed = PMemorySetPressureNotificationsSuppressed {
   pMemorySetPressureNotificationsSuppressedSuppressed :: PMemorySetPressureNotificationsSuppressedSuppressed -- ^ If true, memory pressure notifications will be suppressed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySetPressureNotificationsSuppressed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PMemorySetPressureNotificationsSuppressed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


-- | Function for the command 'Memory.setPressureNotificationsSuppressed'.
-- Enable/disable suppressing memory pressure notifications in all processes.
-- Parameters: 'PMemorySetPressureNotificationsSuppressed'
memorySetPressureNotificationsSuppressed :: Handle ev -> PMemorySetPressureNotificationsSuppressed -> IO (Maybe Error)
memorySetPressureNotificationsSuppressed handle params = sendReceiveCommand handle "Memory.setPressureNotificationsSuppressed" (Just params)


-- | Parameters of the 'memorySimulatePressureNotification' command.
data PMemorySimulatePressureNotification = PMemorySimulatePressureNotification {
   pMemorySimulatePressureNotificationLevel :: PMemorySimulatePressureNotificationLevel -- ^ Memory pressure level of the notification.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySimulatePressureNotification  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PMemorySimulatePressureNotification where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'Memory.simulatePressureNotification'.
-- Simulate a memory pressure notification in all processes.
-- Parameters: 'PMemorySimulatePressureNotification'
memorySimulatePressureNotification :: Handle ev -> PMemorySimulatePressureNotification -> IO (Maybe Error)
memorySimulatePressureNotification handle params = sendReceiveCommand handle "Memory.simulatePressureNotification" (Just params)


-- | Parameters of the 'memoryStartSampling' command.
data PMemoryStartSampling = PMemoryStartSampling {
   pMemoryStartSamplingSamplingInterval :: PMemoryStartSamplingSamplingInterval, -- ^ Average number of bytes between samples.
   pMemoryStartSamplingSuppressRandomness :: PMemoryStartSamplingSuppressRandomness -- ^ Do not randomize intervals between samples.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemoryStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PMemoryStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Memory.startSampling'.
-- Start collecting native memory profile.
-- Parameters: 'PMemoryStartSampling'
memoryStartSampling :: Handle ev -> PMemoryStartSampling -> IO (Maybe Error)
memoryStartSampling handle params = sendReceiveCommand handle "Memory.startSampling" (Just params)


-- | Function for the command 'Memory.stopSampling'.
-- Stop collecting native memory profile.
memoryStopSampling :: Handle ev -> IO (Maybe Error)
memoryStopSampling handle = sendReceiveCommand handle "Memory.stopSampling" (Nothing :: Maybe ())


-- | Function for the command 'Memory.getAllTimeSamplingProfile'.
-- Retrieve native memory allocations profile
-- collected since renderer process startup.
-- Returns: 'MemoryGetAllTimeSamplingProfile'
memoryGetAllTimeSamplingProfile :: Handle ev -> IO (Either Error MemoryGetAllTimeSamplingProfile)
memoryGetAllTimeSamplingProfile handle = sendReceiveCommandResult handle "Memory.getAllTimeSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetAllTimeSamplingProfile' command.
data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetAllTimeSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetAllTimeSamplingProfile where
   commandName _ = "Memory.getAllTimeSamplingProfile"



-- | Function for the command 'Memory.getBrowserSamplingProfile'.
-- Retrieve native memory allocations profile
-- collected since browser process startup.
-- Returns: 'MemoryGetBrowserSamplingProfile'
memoryGetBrowserSamplingProfile :: Handle ev -> IO (Either Error MemoryGetBrowserSamplingProfile)
memoryGetBrowserSamplingProfile handle = sendReceiveCommandResult handle "Memory.getBrowserSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetBrowserSamplingProfile' command.
data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetBrowserSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetBrowserSamplingProfile where
   commandName _ = "Memory.getBrowserSamplingProfile"



-- | Function for the command 'Memory.getSamplingProfile'.
-- Retrieve native memory allocations profile collected since last
-- `startSampling` call.
-- Returns: 'MemoryGetSamplingProfile'
memoryGetSamplingProfile :: Handle ev -> IO (Either Error MemoryGetSamplingProfile)
memoryGetSamplingProfile handle = sendReceiveCommandResult handle "Memory.getSamplingProfile" (Nothing :: Maybe ())

-- | Return type of the 'memoryGetSamplingProfile' command.
data MemoryGetSamplingProfile = MemoryGetSamplingProfile {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command MemoryGetSamplingProfile where
   commandName _ = "Memory.getSamplingProfile"




