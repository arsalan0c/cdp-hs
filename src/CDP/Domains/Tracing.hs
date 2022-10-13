{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Tracing

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.IO as IO


-- | Type 'Tracing.MemoryDumpConfig'.
--   Configuration for memory dump. Used only when "memory-infra" category is enabled.
type TracingMemoryDumpConfig = [(String, String)]

-- | Type 'Tracing.TraceConfig'.
data TracingTraceConfigRecordMode = TracingTraceConfigRecordModeRecordUntilFull | TracingTraceConfigRecordModeRecordContinuously | TracingTraceConfigRecordModeRecordAsMuchAsPossible | TracingTraceConfigRecordModeEchoToConsole
  deriving (Ord, Eq, Show, Read)
instance FromJSON TracingTraceConfigRecordMode where
  parseJSON = A.withText "TracingTraceConfigRecordMode" $ \v -> case v of
    "recordUntilFull" -> pure TracingTraceConfigRecordModeRecordUntilFull
    "recordContinuously" -> pure TracingTraceConfigRecordModeRecordContinuously
    "recordAsMuchAsPossible" -> pure TracingTraceConfigRecordModeRecordAsMuchAsPossible
    "echoToConsole" -> pure TracingTraceConfigRecordModeEchoToConsole
    "_" -> fail "failed to parse TracingTraceConfigRecordMode"
instance ToJSON TracingTraceConfigRecordMode where
  toJSON v = A.String $ case v of
    TracingTraceConfigRecordModeRecordUntilFull -> "recordUntilFull"
    TracingTraceConfigRecordModeRecordContinuously -> "recordContinuously"
    TracingTraceConfigRecordModeRecordAsMuchAsPossible -> "recordAsMuchAsPossible"
    TracingTraceConfigRecordModeEchoToConsole -> "echoToConsole"
data TracingTraceConfig = TracingTraceConfig
  {
    -- | Controls how the trace buffer stores data.
    tracingTraceConfigRecordMode :: Maybe TracingTraceConfigRecordMode,
    -- | Size of the trace buffer in kilobytes. If not specified or zero is passed, a default value
    --   of 200 MB would be used.
    tracingTraceConfigTraceBufferSizeInKb :: Maybe Double,
    -- | Turns on JavaScript stack sampling.
    tracingTraceConfigEnableSampling :: Maybe Bool,
    -- | Turns on system tracing.
    tracingTraceConfigEnableSystrace :: Maybe Bool,
    -- | Turns on argument filter.
    tracingTraceConfigEnableArgumentFilter :: Maybe Bool,
    -- | Included category filters.
    tracingTraceConfigIncludedCategories :: Maybe [String],
    -- | Excluded category filters.
    tracingTraceConfigExcludedCategories :: Maybe [String],
    -- | Configuration to synthesize the delays in tracing.
    tracingTraceConfigSyntheticDelays :: Maybe [String],
    -- | Configuration for memory dump triggers. Used only when "memory-infra" category is enabled.
    tracingTraceConfigMemoryDumpConfig :: Maybe TracingMemoryDumpConfig
  }
  deriving (Eq, Show)
instance FromJSON TracingTraceConfig where
  parseJSON = A.withObject "TracingTraceConfig" $ \o -> TracingTraceConfig
    <$> o A..:? "recordMode"
    <*> o A..:? "traceBufferSizeInKb"
    <*> o A..:? "enableSampling"
    <*> o A..:? "enableSystrace"
    <*> o A..:? "enableArgumentFilter"
    <*> o A..:? "includedCategories"
    <*> o A..:? "excludedCategories"
    <*> o A..:? "syntheticDelays"
    <*> o A..:? "memoryDumpConfig"
instance ToJSON TracingTraceConfig where
  toJSON p = A.object $ catMaybes [
    ("recordMode" A..=) <$> (tracingTraceConfigRecordMode p),
    ("traceBufferSizeInKb" A..=) <$> (tracingTraceConfigTraceBufferSizeInKb p),
    ("enableSampling" A..=) <$> (tracingTraceConfigEnableSampling p),
    ("enableSystrace" A..=) <$> (tracingTraceConfigEnableSystrace p),
    ("enableArgumentFilter" A..=) <$> (tracingTraceConfigEnableArgumentFilter p),
    ("includedCategories" A..=) <$> (tracingTraceConfigIncludedCategories p),
    ("excludedCategories" A..=) <$> (tracingTraceConfigExcludedCategories p),
    ("syntheticDelays" A..=) <$> (tracingTraceConfigSyntheticDelays p),
    ("memoryDumpConfig" A..=) <$> (tracingTraceConfigMemoryDumpConfig p)
    ]

-- | Type 'Tracing.StreamFormat'.
--   Data format of a trace. Can be either the legacy JSON format or the
--   protocol buffer format. Note that the JSON format will be deprecated soon.
data TracingStreamFormat = TracingStreamFormatJson | TracingStreamFormatProto
  deriving (Ord, Eq, Show, Read)
instance FromJSON TracingStreamFormat where
  parseJSON = A.withText "TracingStreamFormat" $ \v -> case v of
    "json" -> pure TracingStreamFormatJson
    "proto" -> pure TracingStreamFormatProto
    "_" -> fail "failed to parse TracingStreamFormat"
instance ToJSON TracingStreamFormat where
  toJSON v = A.String $ case v of
    TracingStreamFormatJson -> "json"
    TracingStreamFormatProto -> "proto"

-- | Type 'Tracing.StreamCompression'.
--   Compression type to use for traces returned via streams.
data TracingStreamCompression = TracingStreamCompressionNone | TracingStreamCompressionGzip
  deriving (Ord, Eq, Show, Read)
instance FromJSON TracingStreamCompression where
  parseJSON = A.withText "TracingStreamCompression" $ \v -> case v of
    "none" -> pure TracingStreamCompressionNone
    "gzip" -> pure TracingStreamCompressionGzip
    "_" -> fail "failed to parse TracingStreamCompression"
instance ToJSON TracingStreamCompression where
  toJSON v = A.String $ case v of
    TracingStreamCompressionNone -> "none"
    TracingStreamCompressionGzip -> "gzip"

-- | Type 'Tracing.MemoryDumpLevelOfDetail'.
--   Details exposed when memory request explicitly declared.
--   Keep consistent with memory_dump_request_args.h and
--   memory_instrumentation.mojom
data TracingMemoryDumpLevelOfDetail = TracingMemoryDumpLevelOfDetailBackground | TracingMemoryDumpLevelOfDetailLight | TracingMemoryDumpLevelOfDetailDetailed
  deriving (Ord, Eq, Show, Read)
instance FromJSON TracingMemoryDumpLevelOfDetail where
  parseJSON = A.withText "TracingMemoryDumpLevelOfDetail" $ \v -> case v of
    "background" -> pure TracingMemoryDumpLevelOfDetailBackground
    "light" -> pure TracingMemoryDumpLevelOfDetailLight
    "detailed" -> pure TracingMemoryDumpLevelOfDetailDetailed
    "_" -> fail "failed to parse TracingMemoryDumpLevelOfDetail"
instance ToJSON TracingMemoryDumpLevelOfDetail where
  toJSON v = A.String $ case v of
    TracingMemoryDumpLevelOfDetailBackground -> "background"
    TracingMemoryDumpLevelOfDetailLight -> "light"
    TracingMemoryDumpLevelOfDetailDetailed -> "detailed"

-- | Type 'Tracing.TracingBackend'.
--   Backend type to use for tracing. `chrome` uses the Chrome-integrated
--   tracing service and is supported on all platforms. `system` is only
--   supported on Chrome OS and uses the Perfetto system tracing service.
--   `auto` chooses `system` when the perfettoConfig provided to Tracing.start
--   specifies at least one non-Chrome data source; otherwise uses `chrome`.
data TracingTracingBackend = TracingTracingBackendAuto | TracingTracingBackendChrome | TracingTracingBackendSystem
  deriving (Ord, Eq, Show, Read)
instance FromJSON TracingTracingBackend where
  parseJSON = A.withText "TracingTracingBackend" $ \v -> case v of
    "auto" -> pure TracingTracingBackendAuto
    "chrome" -> pure TracingTracingBackendChrome
    "system" -> pure TracingTracingBackendSystem
    "_" -> fail "failed to parse TracingTracingBackend"
instance ToJSON TracingTracingBackend where
  toJSON v = A.String $ case v of
    TracingTracingBackendAuto -> "auto"
    TracingTracingBackendChrome -> "chrome"
    TracingTracingBackendSystem -> "system"

-- | Type of the 'Tracing.bufferUsage' event.
data TracingBufferUsage = TracingBufferUsage
  {
    -- | A number in range [0..1] that indicates the used size of event buffer as a fraction of its
    --   total size.
    tracingBufferUsagePercentFull :: Maybe Double,
    -- | An approximate number of events in the trace log.
    tracingBufferUsageEventCount :: Maybe Double,
    -- | A number in range [0..1] that indicates the used size of event buffer as a fraction of its
    --   total size.
    tracingBufferUsageValue :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON TracingBufferUsage where
  parseJSON = A.withObject "TracingBufferUsage" $ \o -> TracingBufferUsage
    <$> o A..:? "percentFull"
    <*> o A..:? "eventCount"
    <*> o A..:? "value"
instance Event TracingBufferUsage where
  eventName _ = "Tracing.bufferUsage"

-- | Type of the 'Tracing.dataCollected' event.
data TracingDataCollected = TracingDataCollected
  {
    tracingDataCollectedValue :: [[(String, String)]]
  }
  deriving (Eq, Show)
instance FromJSON TracingDataCollected where
  parseJSON = A.withObject "TracingDataCollected" $ \o -> TracingDataCollected
    <$> o A..: "value"
instance Event TracingDataCollected where
  eventName _ = "Tracing.dataCollected"

-- | Type of the 'Tracing.tracingComplete' event.
data TracingTracingComplete = TracingTracingComplete
  {
    -- | Indicates whether some trace data is known to have been lost, e.g. because the trace ring
    --   buffer wrapped around.
    tracingTracingCompleteDataLossOccurred :: Bool,
    -- | A handle of the stream that holds resulting trace data.
    tracingTracingCompleteStream :: Maybe IO.IOStreamHandle,
    -- | Trace data format of returned stream.
    tracingTracingCompleteTraceFormat :: Maybe TracingStreamFormat,
    -- | Compression format of returned stream.
    tracingTracingCompleteStreamCompression :: Maybe TracingStreamCompression
  }
  deriving (Eq, Show)
instance FromJSON TracingTracingComplete where
  parseJSON = A.withObject "TracingTracingComplete" $ \o -> TracingTracingComplete
    <$> o A..: "dataLossOccurred"
    <*> o A..:? "stream"
    <*> o A..:? "traceFormat"
    <*> o A..:? "streamCompression"
instance Event TracingTracingComplete where
  eventName _ = "Tracing.tracingComplete"

-- | Stop trace events collection.

-- | Parameters of the 'Tracing.end' command.
data PTracingEnd = PTracingEnd
  deriving (Eq, Show)
pTracingEnd
  :: PTracingEnd
pTracingEnd
  = PTracingEnd
instance ToJSON PTracingEnd where
  toJSON _ = A.Null
instance Command PTracingEnd where
  type CommandResponse PTracingEnd = ()
  commandName _ = "Tracing.end"
  fromJSON = const . A.Success . const ()

-- | Gets supported tracing categories.

-- | Parameters of the 'Tracing.getCategories' command.
data PTracingGetCategories = PTracingGetCategories
  deriving (Eq, Show)
pTracingGetCategories
  :: PTracingGetCategories
pTracingGetCategories
  = PTracingGetCategories
instance ToJSON PTracingGetCategories where
  toJSON _ = A.Null
data TracingGetCategories = TracingGetCategories
  {
    -- | A list of supported tracing categories.
    tracingGetCategoriesCategories :: [String]
  }
  deriving (Eq, Show)
instance FromJSON TracingGetCategories where
  parseJSON = A.withObject "TracingGetCategories" $ \o -> TracingGetCategories
    <$> o A..: "categories"
instance Command PTracingGetCategories where
  type CommandResponse PTracingGetCategories = TracingGetCategories
  commandName _ = "Tracing.getCategories"

-- | Record a clock sync marker in the trace.

-- | Parameters of the 'Tracing.recordClockSyncMarker' command.
data PTracingRecordClockSyncMarker = PTracingRecordClockSyncMarker
  {
    -- | The ID of this clock sync marker
    pTracingRecordClockSyncMarkerSyncId :: String
  }
  deriving (Eq, Show)
pTracingRecordClockSyncMarker
  -- | The ID of this clock sync marker
  :: String
  -> PTracingRecordClockSyncMarker
pTracingRecordClockSyncMarker
  arg_pTracingRecordClockSyncMarkerSyncId
  = PTracingRecordClockSyncMarker
    arg_pTracingRecordClockSyncMarkerSyncId
instance ToJSON PTracingRecordClockSyncMarker where
  toJSON p = A.object $ catMaybes [
    ("syncId" A..=) <$> Just (pTracingRecordClockSyncMarkerSyncId p)
    ]
instance Command PTracingRecordClockSyncMarker where
  type CommandResponse PTracingRecordClockSyncMarker = ()
  commandName _ = "Tracing.recordClockSyncMarker"
  fromJSON = const . A.Success . const ()

-- | Request a global memory dump.

-- | Parameters of the 'Tracing.requestMemoryDump' command.
data PTracingRequestMemoryDump = PTracingRequestMemoryDump
  {
    -- | Enables more deterministic results by forcing garbage collection
    pTracingRequestMemoryDumpDeterministic :: Maybe Bool,
    -- | Specifies level of details in memory dump. Defaults to "detailed".
    pTracingRequestMemoryDumpLevelOfDetail :: Maybe TracingMemoryDumpLevelOfDetail
  }
  deriving (Eq, Show)
pTracingRequestMemoryDump
  :: PTracingRequestMemoryDump
pTracingRequestMemoryDump
  = PTracingRequestMemoryDump
    Nothing
    Nothing
instance ToJSON PTracingRequestMemoryDump where
  toJSON p = A.object $ catMaybes [
    ("deterministic" A..=) <$> (pTracingRequestMemoryDumpDeterministic p),
    ("levelOfDetail" A..=) <$> (pTracingRequestMemoryDumpLevelOfDetail p)
    ]
data TracingRequestMemoryDump = TracingRequestMemoryDump
  {
    -- | GUID of the resulting global memory dump.
    tracingRequestMemoryDumpDumpGuid :: String,
    -- | True iff the global memory dump succeeded.
    tracingRequestMemoryDumpSuccess :: Bool
  }
  deriving (Eq, Show)
instance FromJSON TracingRequestMemoryDump where
  parseJSON = A.withObject "TracingRequestMemoryDump" $ \o -> TracingRequestMemoryDump
    <$> o A..: "dumpGuid"
    <*> o A..: "success"
instance Command PTracingRequestMemoryDump where
  type CommandResponse PTracingRequestMemoryDump = TracingRequestMemoryDump
  commandName _ = "Tracing.requestMemoryDump"

-- | Start trace events collection.

-- | Parameters of the 'Tracing.start' command.
data PTracingStartTransferMode = PTracingStartTransferModeReportEvents | PTracingStartTransferModeReturnAsStream
  deriving (Ord, Eq, Show, Read)
instance FromJSON PTracingStartTransferMode where
  parseJSON = A.withText "PTracingStartTransferMode" $ \v -> case v of
    "ReportEvents" -> pure PTracingStartTransferModeReportEvents
    "ReturnAsStream" -> pure PTracingStartTransferModeReturnAsStream
    "_" -> fail "failed to parse PTracingStartTransferMode"
instance ToJSON PTracingStartTransferMode where
  toJSON v = A.String $ case v of
    PTracingStartTransferModeReportEvents -> "ReportEvents"
    PTracingStartTransferModeReturnAsStream -> "ReturnAsStream"
data PTracingStart = PTracingStart
  {
    -- | If set, the agent will issue bufferUsage events at this interval, specified in milliseconds
    pTracingStartBufferUsageReportingInterval :: Maybe Double,
    -- | Whether to report trace events as series of dataCollected events or to save trace to a
    --   stream (defaults to `ReportEvents`).
    pTracingStartTransferMode :: Maybe PTracingStartTransferMode,
    -- | Trace data format to use. This only applies when using `ReturnAsStream`
    --   transfer mode (defaults to `json`).
    pTracingStartStreamFormat :: Maybe TracingStreamFormat,
    -- | Compression format to use. This only applies when using `ReturnAsStream`
    --   transfer mode (defaults to `none`)
    pTracingStartStreamCompression :: Maybe TracingStreamCompression,
    pTracingStartTraceConfig :: Maybe TracingTraceConfig,
    -- | Base64-encoded serialized perfetto.protos.TraceConfig protobuf message
    --   When specified, the parameters `categories`, `options`, `traceConfig`
    --   are ignored. (Encoded as a base64 string when passed over JSON)
    pTracingStartPerfettoConfig :: Maybe String,
    -- | Backend type (defaults to `auto`)
    pTracingStartTracingBackend :: Maybe TracingTracingBackend
  }
  deriving (Eq, Show)
pTracingStart
  :: PTracingStart
pTracingStart
  = PTracingStart
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PTracingStart where
  toJSON p = A.object $ catMaybes [
    ("bufferUsageReportingInterval" A..=) <$> (pTracingStartBufferUsageReportingInterval p),
    ("transferMode" A..=) <$> (pTracingStartTransferMode p),
    ("streamFormat" A..=) <$> (pTracingStartStreamFormat p),
    ("streamCompression" A..=) <$> (pTracingStartStreamCompression p),
    ("traceConfig" A..=) <$> (pTracingStartTraceConfig p),
    ("perfettoConfig" A..=) <$> (pTracingStartPerfettoConfig p),
    ("tracingBackend" A..=) <$> (pTracingStartTracingBackend p)
    ]
instance Command PTracingStart where
  type CommandResponse PTracingStart = ()
  commandName _ = "Tracing.start"
  fromJSON = const . A.Success . const ()

