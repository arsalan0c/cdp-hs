{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



data MemorySamplingProfileNode = MemorySamplingProfileNode {
   memorySamplingProfileNodeSize :: Double,
   memorySamplingProfileNodeTotal :: Double,
   memorySamplingProfileNodeStack :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfileNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfileNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data MemorySamplingProfile = MemorySamplingProfile {
   memorySamplingProfileSamples :: [MemorySamplingProfileNode],
   memorySamplingProfileModules :: [MemoryModule]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemorySamplingProfile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  MemorySamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data MemoryModule = MemoryModule {
   memoryModuleName :: String,
   memoryModuleUuid :: String,
   memoryModuleBaseAddress :: String,
   memoryModuleSize :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON MemoryModule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  MemoryModule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }






memoryGetDomCounters :: Handle ev -> IO (Either Error MemoryGetDomCounters)
memoryGetDomCounters handle = sendReceiveCommandResult handle "Memory.getDOMCounters" (Nothing :: Maybe ())

data MemoryGetDomCounters = MemoryGetDomCounters {
   memoryGetDomCountersDocuments :: Int,
   memoryGetDomCountersNodes :: Int,
   memoryGetDomCountersJsEventListeners :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetDomCounters where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command MemoryGetDomCounters where
   commandName _ = "Memory.getDOMCounters"



memoryPrepareForLeakDetection :: Handle ev -> IO (Maybe Error)
memoryPrepareForLeakDetection handle = sendReceiveCommand handle "Memory.prepareForLeakDetection" (Nothing :: Maybe ())


memoryForciblyPurgeJavaScriptMemory :: Handle ev -> IO (Maybe Error)
memoryForciblyPurgeJavaScriptMemory handle = sendReceiveCommand handle "Memory.forciblyPurgeJavaScriptMemory" (Nothing :: Maybe ())



data PMemorySetPressureNotificationsSuppressed = PMemorySetPressureNotificationsSuppressed {
   pMemorySetPressureNotificationsSuppressedSuppressed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySetPressureNotificationsSuppressed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PMemorySetPressureNotificationsSuppressed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


memorySetPressureNotificationsSuppressed :: Handle ev -> PMemorySetPressureNotificationsSuppressed -> IO (Maybe Error)
memorySetPressureNotificationsSuppressed handle params = sendReceiveCommand handle "Memory.setPressureNotificationsSuppressed" (Just params)



data PMemorySimulatePressureNotification = PMemorySimulatePressureNotification {
   pMemorySimulatePressureNotificationLevel :: MemoryPressureLevel
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemorySimulatePressureNotification  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PMemorySimulatePressureNotification where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


memorySimulatePressureNotification :: Handle ev -> PMemorySimulatePressureNotification -> IO (Maybe Error)
memorySimulatePressureNotification handle params = sendReceiveCommand handle "Memory.simulatePressureNotification" (Just params)



data PMemoryStartSampling = PMemoryStartSampling {
   pMemoryStartSamplingSamplingInterval :: Maybe Int,
   pMemoryStartSamplingSuppressRandomness :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PMemoryStartSampling  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PMemoryStartSampling where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


memoryStartSampling :: Handle ev -> PMemoryStartSampling -> IO (Maybe Error)
memoryStartSampling handle params = sendReceiveCommand handle "Memory.startSampling" (Just params)


memoryStopSampling :: Handle ev -> IO (Maybe Error)
memoryStopSampling handle = sendReceiveCommand handle "Memory.stopSampling" (Nothing :: Maybe ())


memoryGetAllTimeSamplingProfile :: Handle ev -> IO (Either Error MemoryGetAllTimeSamplingProfile)
memoryGetAllTimeSamplingProfile handle = sendReceiveCommandResult handle "Memory.getAllTimeSamplingProfile" (Nothing :: Maybe ())

data MemoryGetAllTimeSamplingProfile = MemoryGetAllTimeSamplingProfile {
   memoryGetAllTimeSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetAllTimeSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetAllTimeSamplingProfile where
   commandName _ = "Memory.getAllTimeSamplingProfile"



memoryGetBrowserSamplingProfile :: Handle ev -> IO (Either Error MemoryGetBrowserSamplingProfile)
memoryGetBrowserSamplingProfile handle = sendReceiveCommandResult handle "Memory.getBrowserSamplingProfile" (Nothing :: Maybe ())

data MemoryGetBrowserSamplingProfile = MemoryGetBrowserSamplingProfile {
   memoryGetBrowserSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetBrowserSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command MemoryGetBrowserSamplingProfile where
   commandName _ = "Memory.getBrowserSamplingProfile"



memoryGetSamplingProfile :: Handle ev -> IO (Either Error MemoryGetSamplingProfile)
memoryGetSamplingProfile handle = sendReceiveCommandResult handle "Memory.getSamplingProfile" (Nothing :: Maybe ())

data MemoryGetSamplingProfile = MemoryGetSamplingProfile {
   memoryGetSamplingProfileProfile :: MemorySamplingProfile
} deriving (Generic, Eq, Show, Read)

instance FromJSON  MemoryGetSamplingProfile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command MemoryGetSamplingProfile where
   commandName _ = "Memory.getSamplingProfile"




