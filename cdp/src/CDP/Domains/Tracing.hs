{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Tracing (module CDP.Domains.Tracing) where

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

import CDP.Domains.IO as IO


type TracingMemoryDumpConfig = [(String, String)]
data TracingTraceConfigRecordMode = TracingTraceConfigRecordModeRecordUntilFull | TracingTraceConfigRecordModeRecordContinuously | TracingTraceConfigRecordModeRecordAsMuchAsPossible | TracingTraceConfigRecordModeEchoToConsole
   deriving (Ord, Eq, Show, Read)
instance FromJSON TracingTraceConfigRecordMode where
   parseJSON = A.withText  "TracingTraceConfigRecordMode"  $ \v -> do
      case v of
         "recordUntilFull" -> pure TracingTraceConfigRecordModeRecordUntilFull
         "recordContinuously" -> pure TracingTraceConfigRecordModeRecordContinuously
         "recordAsMuchAsPossible" -> pure TracingTraceConfigRecordModeRecordAsMuchAsPossible
         "echoToConsole" -> pure TracingTraceConfigRecordModeEchoToConsole
         _ -> fail "failed to parse TracingTraceConfigRecordMode"

instance ToJSON TracingTraceConfigRecordMode where
   toJSON v = A.String $
      case v of
         TracingTraceConfigRecordModeRecordUntilFull -> "recordUntilFull"
         TracingTraceConfigRecordModeRecordContinuously -> "recordContinuously"
         TracingTraceConfigRecordModeRecordAsMuchAsPossible -> "recordAsMuchAsPossible"
         TracingTraceConfigRecordModeEchoToConsole -> "echoToConsole"



data TracingTraceConfig = TracingTraceConfig {
   tracingTraceConfigRecordMode :: TracingTraceConfigRecordMode,
   tracingTraceConfigEnableSampling :: Maybe Bool,
   tracingTraceConfigEnableSystrace :: Maybe Bool,
   tracingTraceConfigEnableArgumentFilter :: Maybe Bool,
   tracingTraceConfigIncludedCategories :: Maybe [String],
   tracingTraceConfigExcludedCategories :: Maybe [String],
   tracingTraceConfigSyntheticDelays :: Maybe [String],
   tracingTraceConfigMemoryDumpConfig :: Maybe TracingMemoryDumpConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingTraceConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  TracingTraceConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


data TracingStreamFormat = TracingStreamFormatJson | TracingStreamFormatProto
   deriving (Ord, Eq, Show, Read)
instance FromJSON TracingStreamFormat where
   parseJSON = A.withText  "TracingStreamFormat"  $ \v -> do
      case v of
         "json" -> pure TracingStreamFormatJson
         "proto" -> pure TracingStreamFormatProto
         _ -> fail "failed to parse TracingStreamFormat"

instance ToJSON TracingStreamFormat where
   toJSON v = A.String $
      case v of
         TracingStreamFormatJson -> "json"
         TracingStreamFormatProto -> "proto"


data TracingStreamCompression = TracingStreamCompressionNone | TracingStreamCompressionGzip
   deriving (Ord, Eq, Show, Read)
instance FromJSON TracingStreamCompression where
   parseJSON = A.withText  "TracingStreamCompression"  $ \v -> do
      case v of
         "none" -> pure TracingStreamCompressionNone
         "gzip" -> pure TracingStreamCompressionGzip
         _ -> fail "failed to parse TracingStreamCompression"

instance ToJSON TracingStreamCompression where
   toJSON v = A.String $
      case v of
         TracingStreamCompressionNone -> "none"
         TracingStreamCompressionGzip -> "gzip"


data TracingMemoryDumpLevelOfDetail = TracingMemoryDumpLevelOfDetailBackground | TracingMemoryDumpLevelOfDetailLight | TracingMemoryDumpLevelOfDetailDetailed
   deriving (Ord, Eq, Show, Read)
instance FromJSON TracingMemoryDumpLevelOfDetail where
   parseJSON = A.withText  "TracingMemoryDumpLevelOfDetail"  $ \v -> do
      case v of
         "background" -> pure TracingMemoryDumpLevelOfDetailBackground
         "light" -> pure TracingMemoryDumpLevelOfDetailLight
         "detailed" -> pure TracingMemoryDumpLevelOfDetailDetailed
         _ -> fail "failed to parse TracingMemoryDumpLevelOfDetail"

instance ToJSON TracingMemoryDumpLevelOfDetail where
   toJSON v = A.String $
      case v of
         TracingMemoryDumpLevelOfDetailBackground -> "background"
         TracingMemoryDumpLevelOfDetailLight -> "light"
         TracingMemoryDumpLevelOfDetailDetailed -> "detailed"


data TracingTracingBackend = TracingTracingBackendAuto | TracingTracingBackendChrome | TracingTracingBackendSystem
   deriving (Ord, Eq, Show, Read)
instance FromJSON TracingTracingBackend where
   parseJSON = A.withText  "TracingTracingBackend"  $ \v -> do
      case v of
         "auto" -> pure TracingTracingBackendAuto
         "chrome" -> pure TracingTracingBackendChrome
         "system" -> pure TracingTracingBackendSystem
         _ -> fail "failed to parse TracingTracingBackend"

instance ToJSON TracingTracingBackend where
   toJSON v = A.String $
      case v of
         TracingTracingBackendAuto -> "auto"
         TracingTracingBackendChrome -> "chrome"
         TracingTracingBackendSystem -> "system"





data TracingBufferUsage = TracingBufferUsage {
   tracingBufferUsagePercentFull :: Maybe Double,
   tracingBufferUsageEventCount :: Maybe Double,
   tracingBufferUsageValue :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingBufferUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  TracingBufferUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data TracingDataCollected = TracingDataCollected {
   tracingDataCollectedValue :: [[(String, String)]]
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingDataCollected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  TracingDataCollected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data TracingTracingComplete = TracingTracingComplete {
   tracingTracingCompleteDataLossOccurred :: Bool,
   tracingTracingCompleteStream :: Maybe IO.IoStreamHandle,
   tracingTracingCompleteTraceFormat :: Maybe TracingStreamFormat,
   tracingTracingCompleteStreamCompression :: Maybe TracingStreamCompression
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingTracingComplete  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  TracingTracingComplete where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }




tracingEnd :: Handle ev -> IO (Maybe Error)
tracingEnd handle = sendReceiveCommand handle "Tracing.end" (Nothing :: Maybe ())


tracingGetCategories :: Handle ev -> IO (Either Error TracingGetCategories)
tracingGetCategories handle = sendReceiveCommandResult handle "Tracing.getCategories" (Nothing :: Maybe ())

data TracingGetCategories = TracingGetCategories {
   tracingGetCategoriesCategories :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TracingGetCategories where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TracingGetCategories where
   commandName _ = "Tracing.getCategories"




data PTracingRecordClockSyncMarker = PTracingRecordClockSyncMarker {
   pTracingRecordClockSyncMarkerSyncId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingRecordClockSyncMarker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PTracingRecordClockSyncMarker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


tracingRecordClockSyncMarker :: Handle ev -> PTracingRecordClockSyncMarker -> IO (Maybe Error)
tracingRecordClockSyncMarker handle params = sendReceiveCommand handle "Tracing.recordClockSyncMarker" (Just params)



data PTracingRequestMemoryDump = PTracingRequestMemoryDump {
   pTracingRequestMemoryDumpDeterministic :: Maybe Bool,
   pTracingRequestMemoryDumpLevelOfDetail :: Maybe TracingMemoryDumpLevelOfDetail
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingRequestMemoryDump  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTracingRequestMemoryDump where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


tracingRequestMemoryDump :: Handle ev -> PTracingRequestMemoryDump -> IO (Either Error TracingRequestMemoryDump)
tracingRequestMemoryDump handle params = sendReceiveCommandResult handle "Tracing.requestMemoryDump" (Just params)

data TracingRequestMemoryDump = TracingRequestMemoryDump {
   tracingRequestMemoryDumpDumpGuid :: String,
   tracingRequestMemoryDumpSuccess :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TracingRequestMemoryDump where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command TracingRequestMemoryDump where
   commandName _ = "Tracing.requestMemoryDump"



data PTracingStartTransferMode = PTracingStartTransferModeReportEvents | PTracingStartTransferModeReturnAsStream
   deriving (Ord, Eq, Show, Read)
instance FromJSON PTracingStartTransferMode where
   parseJSON = A.withText  "PTracingStartTransferMode"  $ \v -> do
      case v of
         "ReportEvents" -> pure PTracingStartTransferModeReportEvents
         "ReturnAsStream" -> pure PTracingStartTransferModeReturnAsStream
         _ -> fail "failed to parse PTracingStartTransferMode"

instance ToJSON PTracingStartTransferMode where
   toJSON v = A.String $
      case v of
         PTracingStartTransferModeReportEvents -> "ReportEvents"
         PTracingStartTransferModeReturnAsStream -> "ReturnAsStream"



data PTracingStart = PTracingStart {
   pTracingStartBufferUsageReportingInterval :: Maybe Double,
   pTracingStartTransferMode :: PTracingStartTransferMode,
   pTracingStartStreamFormat :: Maybe TracingStreamFormat,
   pTracingStartStreamCompression :: Maybe TracingStreamCompression,
   pTracingStartTraceConfig :: Maybe TracingTraceConfig,
   pTracingStartPerfettoConfig :: Maybe String,
   pTracingStartTracingBackend :: Maybe TracingTracingBackend
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingStart  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PTracingStart where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


tracingStart :: Handle ev -> PTracingStart -> IO (Maybe Error)
tracingStart handle params = sendReceiveCommand handle "Tracing.start" (Just params)



