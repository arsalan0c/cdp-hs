{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Tracing 
-}


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


-- | Configuration for memory dump. Used only when "memory-infra" category is enabled.
type TracingMemoryDumpConfig = [(String, String)]

-- | Type 'Tracing.TraceConfig' .
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
   tracingTraceConfigRecordMode :: TracingTraceConfigRecordMode, -- ^ Controls how the trace buffer stores data.
   tracingTraceConfigEnableSampling :: TracingTraceConfigEnableSampling, -- ^ Turns on JavaScript stack sampling.
   tracingTraceConfigEnableSystrace :: TracingTraceConfigEnableSystrace, -- ^ Turns on system tracing.
   tracingTraceConfigEnableArgumentFilter :: TracingTraceConfigEnableArgumentFilter, -- ^ Turns on argument filter.
   tracingTraceConfigIncludedCategories :: TracingTraceConfigIncludedCategories, -- ^ Included category filters.
   tracingTraceConfigExcludedCategories :: TracingTraceConfigExcludedCategories, -- ^ Excluded category filters.
   tracingTraceConfigSyntheticDelays :: TracingTraceConfigSyntheticDelays, -- ^ Configuration to synthesize the delays in tracing.
   tracingTraceConfigMemoryDumpConfig :: TracingTraceConfigMemoryDumpConfig -- ^ Configuration for memory dump triggers. Used only when "memory-infra" category is enabled.
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingTraceConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  TracingTraceConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Data format of a trace. Can be either the legacy JSON format or the
-- protocol buffer format. Note that the JSON format will be deprecated soon.
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



-- | Compression type to use for traces returned via streams.
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



-- | Details exposed when memory request explicitly declared.
-- Keep consistent with memory_dump_request_args.h and
-- memory_instrumentation.mojom
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



-- | Backend type to use for tracing. `chrome` uses the Chrome-integrated
-- tracing service and is supported on all platforms. `system` is only
-- supported on Chrome OS and uses the Perfetto system tracing service.
-- `auto` chooses `system` when the perfettoConfig provided to Tracing.start
-- specifies at least one non-Chrome data source; otherwise uses `chrome`.
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





-- | Type of the 'Tracing.bufferUsage' event.
data TracingBufferUsage = TracingBufferUsage {
   tracingBufferUsagePercentFull :: TracingBufferUsagePercentFull, -- ^ A number in range [0..1] that indicates the used size of event buffer as a fraction of its
total size.
   tracingBufferUsageEventCount :: TracingBufferUsageEventCount, -- ^ An approximate number of events in the trace log.
   tracingBufferUsageValue :: TracingBufferUsageValue -- ^ A number in range [0..1] that indicates the used size of event buffer as a fraction of its
total size.
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingBufferUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  TracingBufferUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Tracing.dataCollected' event.
data TracingDataCollected = TracingDataCollected {
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingDataCollected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  TracingDataCollected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'Tracing.tracingComplete' event.
data TracingTracingComplete = TracingTracingComplete {
   tracingTracingCompleteDataLossOccurred :: TracingTracingCompleteDataLossOccurred, -- ^ Indicates whether some trace data is known to have been lost, e.g. because the trace ring
buffer wrapped around.
   tracingTracingCompleteStream :: TracingTracingCompleteStream, -- ^ A handle of the stream that holds resulting trace data.
   tracingTracingCompleteTraceFormat :: TracingTracingCompleteTraceFormat, -- ^ Trace data format of returned stream.
   tracingTracingCompleteStreamCompression :: TracingTracingCompleteStreamCompression -- ^ Compression format of returned stream.
} deriving (Generic, Eq, Show, Read)
instance ToJSON TracingTracingComplete  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  TracingTracingComplete where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





-- | Function for the command 'Tracing.end'.
-- Stop trace events collection.
tracingEnd :: Handle ev -> IO (Maybe Error)
tracingEnd handle = sendReceiveCommand handle "Tracing.end" (Nothing :: Maybe ())


-- | Function for the command 'Tracing.getCategories'.
-- Gets supported tracing categories.
-- Returns: 'TracingGetCategories'
tracingGetCategories :: Handle ev -> IO (Either Error TracingGetCategories)
tracingGetCategories handle = sendReceiveCommandResult handle "Tracing.getCategories" (Nothing :: Maybe ())

-- | Return type of the 'tracingGetCategories' command.
data TracingGetCategories = TracingGetCategories {
   tracingGetCategoriesCategories :: [String] -- ^ A list of supported tracing categories.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TracingGetCategories where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TracingGetCategories where
   commandName _ = "Tracing.getCategories"



-- | Parameters of the 'tracingRecordClockSyncMarker' command.
data PTracingRecordClockSyncMarker = PTracingRecordClockSyncMarker {
   pTracingRecordClockSyncMarkerSyncId :: PTracingRecordClockSyncMarkerSyncId -- ^ The ID of this clock sync marker
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingRecordClockSyncMarker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PTracingRecordClockSyncMarker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Tracing.recordClockSyncMarker'.
-- Record a clock sync marker in the trace.
-- Parameters: 'PTracingRecordClockSyncMarker'
tracingRecordClockSyncMarker :: Handle ev -> PTracingRecordClockSyncMarker -> IO (Maybe Error)
tracingRecordClockSyncMarker handle params = sendReceiveCommand handle "Tracing.recordClockSyncMarker" (Just params)


-- | Parameters of the 'tracingRequestMemoryDump' command.
data PTracingRequestMemoryDump = PTracingRequestMemoryDump {
   pTracingRequestMemoryDumpDeterministic :: PTracingRequestMemoryDumpDeterministic, -- ^ Enables more deterministic results by forcing garbage collection
   pTracingRequestMemoryDumpLevelOfDetail :: PTracingRequestMemoryDumpLevelOfDetail -- ^ Specifies level of details in memory dump. Defaults to "detailed".
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingRequestMemoryDump  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTracingRequestMemoryDump where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Tracing.requestMemoryDump'.
-- Request a global memory dump.
-- Parameters: 'PTracingRequestMemoryDump'
-- Returns: 'TracingRequestMemoryDump'
tracingRequestMemoryDump :: Handle ev -> PTracingRequestMemoryDump -> IO (Either Error TracingRequestMemoryDump)
tracingRequestMemoryDump handle params = sendReceiveCommandResult handle "Tracing.requestMemoryDump" (Just params)

-- | Return type of the 'tracingRequestMemoryDump' command.
data TracingRequestMemoryDump = TracingRequestMemoryDump {
   tracingRequestMemoryDumpDumpGuid :: String, -- ^ GUID of the resulting global memory dump.
   tracingRequestMemoryDumpSuccess :: Bool -- ^ True iff the global memory dump succeeded.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TracingRequestMemoryDump where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command TracingRequestMemoryDump where
   commandName _ = "Tracing.requestMemoryDump"



-- | Parameters of the 'tracingStart' command.
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
   pTracingStartBufferUsageReportingInterval :: PTracingStartBufferUsageReportingInterval, -- ^ If set, the agent will issue bufferUsage events at this interval, specified in milliseconds
   pTracingStartTransferMode :: PTracingStartTransferMode, -- ^ Whether to report trace events as series of dataCollected events or to save trace to a
stream (defaults to `ReportEvents`).
   pTracingStartStreamFormat :: PTracingStartStreamFormat, -- ^ Trace data format to use. This only applies when using `ReturnAsStream`
transfer mode (defaults to `json`).
   pTracingStartStreamCompression :: PTracingStartStreamCompression, -- ^ Compression format to use. This only applies when using `ReturnAsStream`
transfer mode (defaults to `none`)

   pTracingStartPerfettoConfig :: PTracingStartPerfettoConfig, -- ^ Base64-encoded serialized perfetto.protos.TraceConfig protobuf message
When specified, the parameters `categories`, `options`, `traceConfig`
are ignored. (Encoded as a base64 string when passed over JSON)
   pTracingStartTracingBackend :: PTracingStartTracingBackend -- ^ Backend type (defaults to `auto`)
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTracingStart  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PTracingStart where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


-- | Function for the command 'Tracing.start'.
-- Start trace events collection.
-- Parameters: 'PTracingStart'
tracingStart :: Handle ev -> PTracingStart -> IO (Maybe Error)
tracingStart handle params = sendReceiveCommand handle "Tracing.start" (Just params)


