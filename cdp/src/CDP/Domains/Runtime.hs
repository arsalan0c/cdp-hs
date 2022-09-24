{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Runtime (module CDP.Domains.Runtime) where

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



type RuntimeScriptId = String
data RuntimeWebDriverValueType = RuntimeWebDriverValueTypeUndefined | RuntimeWebDriverValueTypeNull | RuntimeWebDriverValueTypeString | RuntimeWebDriverValueTypeNumber | RuntimeWebDriverValueTypeBoolean | RuntimeWebDriverValueTypeBigint | RuntimeWebDriverValueTypeRegexp | RuntimeWebDriverValueTypeDate | RuntimeWebDriverValueTypeSymbol | RuntimeWebDriverValueTypeArray | RuntimeWebDriverValueTypeObject | RuntimeWebDriverValueTypeFunction | RuntimeWebDriverValueTypeMap | RuntimeWebDriverValueTypeSet | RuntimeWebDriverValueTypeWeakmap | RuntimeWebDriverValueTypeWeakset | RuntimeWebDriverValueTypeError | RuntimeWebDriverValueTypeProxy | RuntimeWebDriverValueTypePromise | RuntimeWebDriverValueTypeTypedarray | RuntimeWebDriverValueTypeArraybuffer | RuntimeWebDriverValueTypeNode | RuntimeWebDriverValueTypeWindow
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeWebDriverValueType where
   parseJSON = A.withText  "RuntimeWebDriverValueType"  $ \v -> do
      case v of
         "undefined" -> pure RuntimeWebDriverValueTypeUndefined
         "null" -> pure RuntimeWebDriverValueTypeNull
         "string" -> pure RuntimeWebDriverValueTypeString
         "number" -> pure RuntimeWebDriverValueTypeNumber
         "boolean" -> pure RuntimeWebDriverValueTypeBoolean
         "bigint" -> pure RuntimeWebDriverValueTypeBigint
         "regexp" -> pure RuntimeWebDriverValueTypeRegexp
         "date" -> pure RuntimeWebDriverValueTypeDate
         "symbol" -> pure RuntimeWebDriverValueTypeSymbol
         "array" -> pure RuntimeWebDriverValueTypeArray
         "object" -> pure RuntimeWebDriverValueTypeObject
         "function" -> pure RuntimeWebDriverValueTypeFunction
         "map" -> pure RuntimeWebDriverValueTypeMap
         "set" -> pure RuntimeWebDriverValueTypeSet
         "weakmap" -> pure RuntimeWebDriverValueTypeWeakmap
         "weakset" -> pure RuntimeWebDriverValueTypeWeakset
         "error" -> pure RuntimeWebDriverValueTypeError
         "proxy" -> pure RuntimeWebDriverValueTypeProxy
         "promise" -> pure RuntimeWebDriverValueTypePromise
         "typedarray" -> pure RuntimeWebDriverValueTypeTypedarray
         "arraybuffer" -> pure RuntimeWebDriverValueTypeArraybuffer
         "node" -> pure RuntimeWebDriverValueTypeNode
         "window" -> pure RuntimeWebDriverValueTypeWindow
         _ -> fail "failed to parse RuntimeWebDriverValueType"

instance ToJSON RuntimeWebDriverValueType where
   toJSON v = A.String $
      case v of
         RuntimeWebDriverValueTypeUndefined -> "undefined"
         RuntimeWebDriverValueTypeNull -> "null"
         RuntimeWebDriverValueTypeString -> "string"
         RuntimeWebDriverValueTypeNumber -> "number"
         RuntimeWebDriverValueTypeBoolean -> "boolean"
         RuntimeWebDriverValueTypeBigint -> "bigint"
         RuntimeWebDriverValueTypeRegexp -> "regexp"
         RuntimeWebDriverValueTypeDate -> "date"
         RuntimeWebDriverValueTypeSymbol -> "symbol"
         RuntimeWebDriverValueTypeArray -> "array"
         RuntimeWebDriverValueTypeObject -> "object"
         RuntimeWebDriverValueTypeFunction -> "function"
         RuntimeWebDriverValueTypeMap -> "map"
         RuntimeWebDriverValueTypeSet -> "set"
         RuntimeWebDriverValueTypeWeakmap -> "weakmap"
         RuntimeWebDriverValueTypeWeakset -> "weakset"
         RuntimeWebDriverValueTypeError -> "error"
         RuntimeWebDriverValueTypeProxy -> "proxy"
         RuntimeWebDriverValueTypePromise -> "promise"
         RuntimeWebDriverValueTypeTypedarray -> "typedarray"
         RuntimeWebDriverValueTypeArraybuffer -> "arraybuffer"
         RuntimeWebDriverValueTypeNode -> "node"
         RuntimeWebDriverValueTypeWindow -> "window"



data RuntimeWebDriverValue = RuntimeWebDriverValue {
   runtimeWebDriverValueType :: RuntimeWebDriverValueType,
   runtimeWebDriverValueValue :: Maybe Int,
   runtimeWebDriverValueObjectId :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeWebDriverValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  RuntimeWebDriverValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


type RuntimeRemoteObjectId = String
type RuntimeUnserializableValue = String
data RuntimeRemoteObjectType = RuntimeRemoteObjectTypeObject | RuntimeRemoteObjectTypeFunction | RuntimeRemoteObjectTypeUndefined | RuntimeRemoteObjectTypeString | RuntimeRemoteObjectTypeNumber | RuntimeRemoteObjectTypeBoolean | RuntimeRemoteObjectTypeSymbol | RuntimeRemoteObjectTypeBigint
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectType where
   parseJSON = A.withText  "RuntimeRemoteObjectType"  $ \v -> do
      case v of
         "object" -> pure RuntimeRemoteObjectTypeObject
         "function" -> pure RuntimeRemoteObjectTypeFunction
         "undefined" -> pure RuntimeRemoteObjectTypeUndefined
         "string" -> pure RuntimeRemoteObjectTypeString
         "number" -> pure RuntimeRemoteObjectTypeNumber
         "boolean" -> pure RuntimeRemoteObjectTypeBoolean
         "symbol" -> pure RuntimeRemoteObjectTypeSymbol
         "bigint" -> pure RuntimeRemoteObjectTypeBigint
         _ -> fail "failed to parse RuntimeRemoteObjectType"

instance ToJSON RuntimeRemoteObjectType where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectTypeObject -> "object"
         RuntimeRemoteObjectTypeFunction -> "function"
         RuntimeRemoteObjectTypeUndefined -> "undefined"
         RuntimeRemoteObjectTypeString -> "string"
         RuntimeRemoteObjectTypeNumber -> "number"
         RuntimeRemoteObjectTypeBoolean -> "boolean"
         RuntimeRemoteObjectTypeSymbol -> "symbol"
         RuntimeRemoteObjectTypeBigint -> "bigint"


data RuntimeRemoteObjectSubtype = RuntimeRemoteObjectSubtypeArray | RuntimeRemoteObjectSubtypeNull | RuntimeRemoteObjectSubtypeNode | RuntimeRemoteObjectSubtypeRegexp | RuntimeRemoteObjectSubtypeDate | RuntimeRemoteObjectSubtypeMap | RuntimeRemoteObjectSubtypeSet | RuntimeRemoteObjectSubtypeWeakmap | RuntimeRemoteObjectSubtypeWeakset | RuntimeRemoteObjectSubtypeIterator | RuntimeRemoteObjectSubtypeGenerator | RuntimeRemoteObjectSubtypeError | RuntimeRemoteObjectSubtypeProxy | RuntimeRemoteObjectSubtypePromise | RuntimeRemoteObjectSubtypeTypedarray | RuntimeRemoteObjectSubtypeArraybuffer | RuntimeRemoteObjectSubtypeDataview | RuntimeRemoteObjectSubtypeWebassemblymemory | RuntimeRemoteObjectSubtypeWasmvalue
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectSubtype where
   parseJSON = A.withText  "RuntimeRemoteObjectSubtype"  $ \v -> do
      case v of
         "array" -> pure RuntimeRemoteObjectSubtypeArray
         "null" -> pure RuntimeRemoteObjectSubtypeNull
         "node" -> pure RuntimeRemoteObjectSubtypeNode
         "regexp" -> pure RuntimeRemoteObjectSubtypeRegexp
         "date" -> pure RuntimeRemoteObjectSubtypeDate
         "map" -> pure RuntimeRemoteObjectSubtypeMap
         "set" -> pure RuntimeRemoteObjectSubtypeSet
         "weakmap" -> pure RuntimeRemoteObjectSubtypeWeakmap
         "weakset" -> pure RuntimeRemoteObjectSubtypeWeakset
         "iterator" -> pure RuntimeRemoteObjectSubtypeIterator
         "generator" -> pure RuntimeRemoteObjectSubtypeGenerator
         "error" -> pure RuntimeRemoteObjectSubtypeError
         "proxy" -> pure RuntimeRemoteObjectSubtypeProxy
         "promise" -> pure RuntimeRemoteObjectSubtypePromise
         "typedarray" -> pure RuntimeRemoteObjectSubtypeTypedarray
         "arraybuffer" -> pure RuntimeRemoteObjectSubtypeArraybuffer
         "dataview" -> pure RuntimeRemoteObjectSubtypeDataview
         "webassemblymemory" -> pure RuntimeRemoteObjectSubtypeWebassemblymemory
         "wasmvalue" -> pure RuntimeRemoteObjectSubtypeWasmvalue
         _ -> fail "failed to parse RuntimeRemoteObjectSubtype"

instance ToJSON RuntimeRemoteObjectSubtype where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectSubtypeArray -> "array"
         RuntimeRemoteObjectSubtypeNull -> "null"
         RuntimeRemoteObjectSubtypeNode -> "node"
         RuntimeRemoteObjectSubtypeRegexp -> "regexp"
         RuntimeRemoteObjectSubtypeDate -> "date"
         RuntimeRemoteObjectSubtypeMap -> "map"
         RuntimeRemoteObjectSubtypeSet -> "set"
         RuntimeRemoteObjectSubtypeWeakmap -> "weakmap"
         RuntimeRemoteObjectSubtypeWeakset -> "weakset"
         RuntimeRemoteObjectSubtypeIterator -> "iterator"
         RuntimeRemoteObjectSubtypeGenerator -> "generator"
         RuntimeRemoteObjectSubtypeError -> "error"
         RuntimeRemoteObjectSubtypeProxy -> "proxy"
         RuntimeRemoteObjectSubtypePromise -> "promise"
         RuntimeRemoteObjectSubtypeTypedarray -> "typedarray"
         RuntimeRemoteObjectSubtypeArraybuffer -> "arraybuffer"
         RuntimeRemoteObjectSubtypeDataview -> "dataview"
         RuntimeRemoteObjectSubtypeWebassemblymemory -> "webassemblymemory"
         RuntimeRemoteObjectSubtypeWasmvalue -> "wasmvalue"



data RuntimeRemoteObject = RuntimeRemoteObject {
   runtimeRemoteObjectType :: RuntimeRemoteObjectType,
   runtimeRemoteObjectSubtype :: RuntimeRemoteObjectSubtype,
   runtimeRemoteObjectClassName :: Maybe String,
   runtimeRemoteObjectValue :: Maybe Int,
   runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
   runtimeRemoteObjectDescription :: Maybe String,
   runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeRemoteObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeRemoteObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
   runtimePropertyDescriptorName :: String,
   runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorWritable :: Maybe Bool,
   runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
   runtimePropertyDescriptorConfigurable :: Bool,
   runtimePropertyDescriptorEnumerable :: Bool,
   runtimePropertyDescriptorWasThrown :: Maybe Bool,
   runtimePropertyDescriptorIsOwn :: Maybe Bool,
   runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimePropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  RuntimePropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
   runtimeInternalPropertyDescriptorName :: String,
   runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInternalPropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  RuntimeInternalPropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data RuntimeCallArgument = RuntimeCallArgument {
   runtimeCallArgumentValue :: Maybe Int,
   runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
   runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
   runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
   runtimeExecutionContextDescriptionOrigin :: String,
   runtimeExecutionContextDescriptionName :: String,
   runtimeExecutionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDescription  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDescription where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }



data RuntimeExceptionDetails = RuntimeExceptionDetails {
   runtimeExceptionDetailsExceptionId :: Int,
   runtimeExceptionDetailsText :: String,
   runtimeExceptionDetailsLineNumber :: Int,
   runtimeExceptionDetailsColumnNumber :: Int,
   runtimeExceptionDetailsScriptId :: Maybe RuntimeScriptId,
   runtimeExceptionDetailsUrl :: Maybe String,
   runtimeExceptionDetailsStackTrace :: Maybe RuntimeStackTrace,
   runtimeExceptionDetailsException :: Maybe RuntimeRemoteObject,
   runtimeExceptionDetailsExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


type RuntimeTimestamp = Double
type RuntimeTimeDelta = Double

data RuntimeCallFrame = RuntimeCallFrame {
   runtimeCallFrameFunctionName :: String,
   runtimeCallFrameScriptId :: RuntimeScriptId,
   runtimeCallFrameUrl :: String,
   runtimeCallFrameLineNumber :: Int,
   runtimeCallFrameColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data RuntimeStackTrace = RuntimeStackTrace {
   runtimeStackTraceDescription :: Maybe String,
   runtimeStackTraceCallFrames :: [RuntimeCallFrame],
   runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeStackTrace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  RuntimeStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }




data RuntimeConsoleApiCalledType = RuntimeConsoleApiCalledTypeLog | RuntimeConsoleApiCalledTypeDebug | RuntimeConsoleApiCalledTypeInfo | RuntimeConsoleApiCalledTypeError | RuntimeConsoleApiCalledTypeWarning | RuntimeConsoleApiCalledTypeDir | RuntimeConsoleApiCalledTypeDirxml | RuntimeConsoleApiCalledTypeTable | RuntimeConsoleApiCalledTypeTrace | RuntimeConsoleApiCalledTypeClear | RuntimeConsoleApiCalledTypeStartGroup | RuntimeConsoleApiCalledTypeStartGroupCollapsed | RuntimeConsoleApiCalledTypeEndGroup | RuntimeConsoleApiCalledTypeAssert | RuntimeConsoleApiCalledTypeProfile | RuntimeConsoleApiCalledTypeProfileEnd | RuntimeConsoleApiCalledTypeCount | RuntimeConsoleApiCalledTypeTimeEnd
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeConsoleApiCalledType where
   parseJSON = A.withText  "RuntimeConsoleApiCalledType"  $ \v -> do
      case v of
         "log" -> pure RuntimeConsoleApiCalledTypeLog
         "debug" -> pure RuntimeConsoleApiCalledTypeDebug
         "info" -> pure RuntimeConsoleApiCalledTypeInfo
         "error" -> pure RuntimeConsoleApiCalledTypeError
         "warning" -> pure RuntimeConsoleApiCalledTypeWarning
         "dir" -> pure RuntimeConsoleApiCalledTypeDir
         "dirxml" -> pure RuntimeConsoleApiCalledTypeDirxml
         "table" -> pure RuntimeConsoleApiCalledTypeTable
         "trace" -> pure RuntimeConsoleApiCalledTypeTrace
         "clear" -> pure RuntimeConsoleApiCalledTypeClear
         "startGroup" -> pure RuntimeConsoleApiCalledTypeStartGroup
         "startGroupCollapsed" -> pure RuntimeConsoleApiCalledTypeStartGroupCollapsed
         "endGroup" -> pure RuntimeConsoleApiCalledTypeEndGroup
         "assert" -> pure RuntimeConsoleApiCalledTypeAssert
         "profile" -> pure RuntimeConsoleApiCalledTypeProfile
         "profileEnd" -> pure RuntimeConsoleApiCalledTypeProfileEnd
         "count" -> pure RuntimeConsoleApiCalledTypeCount
         "timeEnd" -> pure RuntimeConsoleApiCalledTypeTimeEnd
         _ -> fail "failed to parse RuntimeConsoleApiCalledType"

instance ToJSON RuntimeConsoleApiCalledType where
   toJSON v = A.String $
      case v of
         RuntimeConsoleApiCalledTypeLog -> "log"
         RuntimeConsoleApiCalledTypeDebug -> "debug"
         RuntimeConsoleApiCalledTypeInfo -> "info"
         RuntimeConsoleApiCalledTypeError -> "error"
         RuntimeConsoleApiCalledTypeWarning -> "warning"
         RuntimeConsoleApiCalledTypeDir -> "dir"
         RuntimeConsoleApiCalledTypeDirxml -> "dirxml"
         RuntimeConsoleApiCalledTypeTable -> "table"
         RuntimeConsoleApiCalledTypeTrace -> "trace"
         RuntimeConsoleApiCalledTypeClear -> "clear"
         RuntimeConsoleApiCalledTypeStartGroup -> "startGroup"
         RuntimeConsoleApiCalledTypeStartGroupCollapsed -> "startGroupCollapsed"
         RuntimeConsoleApiCalledTypeEndGroup -> "endGroup"
         RuntimeConsoleApiCalledTypeAssert -> "assert"
         RuntimeConsoleApiCalledTypeProfile -> "profile"
         RuntimeConsoleApiCalledTypeProfileEnd -> "profileEnd"
         RuntimeConsoleApiCalledTypeCount -> "count"
         RuntimeConsoleApiCalledTypeTimeEnd -> "timeEnd"



data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
   runtimeConsoleApiCalledType :: RuntimeConsoleApiCalledType,
   runtimeConsoleApiCalledArgs :: [RuntimeRemoteObject],
   runtimeConsoleApiCalledExecutionContextId :: RuntimeExecutionContextId,
   runtimeConsoleApiCalledTimestamp :: RuntimeTimestamp,
   runtimeConsoleApiCalledStackTrace :: Maybe RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeConsoleApiCalled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeConsoleApiCalled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
   runtimeExceptionRevokedReason :: String,
   runtimeExceptionRevokedExceptionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionRevoked  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionRevoked where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data RuntimeExceptionThrown = RuntimeExceptionThrown {
   runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
   runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionThrown  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionThrown where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
   runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
   runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
   deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
   parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
      case v of
         "RuntimeExecutionContextsCleared" -> pure RuntimeExecutionContextsCleared
         _ -> fail "failed to parse RuntimeExecutionContextsCleared"



data RuntimeInspectRequested = RuntimeInspectRequested {
   runtimeInspectRequestedObject :: RuntimeRemoteObject,
   runtimeInspectRequestedHints :: [(String, String)]
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInspectRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeInspectRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
   pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
   pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
   pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeAwaitPromise  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


runtimeAwaitPromise :: Handle ev -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise handle params = sendReceiveCommandResult handle "Runtime.awaitPromise" (Just params)

data RuntimeAwaitPromise = RuntimeAwaitPromise {
   runtimeAwaitPromiseResult :: RuntimeRemoteObject,
   runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeAwaitPromise where
   commandName _ = "Runtime.awaitPromise"




data PRuntimeCallFunctionOn = PRuntimeCallFunctionOn {
   pRuntimeCallFunctionOnFunctionDeclaration :: String,
   pRuntimeCallFunctionOnObjectId :: Maybe RuntimeRemoteObjectId,
   pRuntimeCallFunctionOnArguments :: Maybe [RuntimeCallArgument],
   pRuntimeCallFunctionOnSilent :: Maybe Bool,
   pRuntimeCallFunctionOnReturnByValue :: Maybe Bool,
   pRuntimeCallFunctionOnUserGesture :: Maybe Bool,
   pRuntimeCallFunctionOnAwaitPromise :: Maybe Bool,
   pRuntimeCallFunctionOnExecutionContextId :: Maybe RuntimeExecutionContextId,
   pRuntimeCallFunctionOnObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCallFunctionOn  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


runtimeCallFunctionOn :: Handle ev -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn handle params = sendReceiveCommandResult handle "Runtime.callFunctionOn" (Just params)

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
   runtimeCallFunctionOnResult :: RuntimeRemoteObject,
   runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command RuntimeCallFunctionOn where
   commandName _ = "Runtime.callFunctionOn"




data PRuntimeCompileScript = PRuntimeCompileScript {
   pRuntimeCompileScriptExpression :: String,
   pRuntimeCompileScriptSourceUrl :: String,
   pRuntimeCompileScriptPersistScript :: Bool,
   pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCompileScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeCompileScript :: Handle ev -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript handle params = sendReceiveCommandResult handle "Runtime.compileScript" (Just params)

data RuntimeCompileScript = RuntimeCompileScript {
   runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
   runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeCompileScript where
   commandName _ = "Runtime.compileScript"



runtimeDisable :: Handle ev -> IO (Maybe Error)
runtimeDisable handle = sendReceiveCommand handle "Runtime.disable" (Nothing :: Maybe ())


runtimeDiscardConsoleEntries :: Handle ev -> IO (Maybe Error)
runtimeDiscardConsoleEntries handle = sendReceiveCommand handle "Runtime.discardConsoleEntries" (Nothing :: Maybe ())


runtimeEnable :: Handle ev -> IO (Maybe Error)
runtimeEnable handle = sendReceiveCommand handle "Runtime.enable" (Nothing :: Maybe ())



data PRuntimeEvaluate = PRuntimeEvaluate {
   pRuntimeEvaluateExpression :: String,
   pRuntimeEvaluateObjectGroup :: Maybe String,
   pRuntimeEvaluateIncludeCommandLineApi :: Maybe Bool,
   pRuntimeEvaluateSilent :: Maybe Bool,
   pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
   pRuntimeEvaluateReturnByValue :: Maybe Bool,
   pRuntimeEvaluateUserGesture :: Maybe Bool,
   pRuntimeEvaluateAwaitPromise :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeEvaluate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PRuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


runtimeEvaluate :: Handle ev -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate handle params = sendReceiveCommandResult handle "Runtime.evaluate" (Just params)

data RuntimeEvaluate = RuntimeEvaluate {
   runtimeEvaluateResult :: RuntimeRemoteObject,
   runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command RuntimeEvaluate where
   commandName _ = "Runtime.evaluate"




data PRuntimeGetProperties = PRuntimeGetProperties {
   pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
   pRuntimeGetPropertiesOwnProperties :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGetProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeGetProperties :: Handle ev -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties handle params = sendReceiveCommandResult handle "Runtime.getProperties" (Just params)

data RuntimeGetProperties = RuntimeGetProperties {
   runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
   runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
   runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeGetProperties where
   commandName _ = "Runtime.getProperties"




data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
   pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGlobalLexicalScopeNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


runtimeGlobalLexicalScopeNames :: Handle ev -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames handle params = sendReceiveCommandResult handle "Runtime.globalLexicalScopeNames" (Just params)

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
   runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command RuntimeGlobalLexicalScopeNames where
   commandName _ = "Runtime.globalLexicalScopeNames"




data PRuntimeQueryObjects = PRuntimeQueryObjects {
   pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
   pRuntimeQueryObjectsObjectGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeQueryObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


runtimeQueryObjects :: Handle ev -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects handle params = sendReceiveCommandResult handle "Runtime.queryObjects" (Just params)

data RuntimeQueryObjects = RuntimeQueryObjects {
   runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeQueryObjects where
   commandName _ = "Runtime.queryObjects"




data PRuntimeReleaseObject = PRuntimeReleaseObject {
   pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


runtimeReleaseObject :: Handle ev -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject handle params = sendReceiveCommand handle "Runtime.releaseObject" (Just params)



data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
   pRuntimeReleaseObjectGroupObjectGroup :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObjectGroup  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObjectGroup where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


runtimeReleaseObjectGroup :: Handle ev -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup handle params = sendReceiveCommand handle "Runtime.releaseObjectGroup" (Just params)


runtimeRunIfWaitingForDebugger :: Handle ev -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger handle = sendReceiveCommand handle "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())



data PRuntimeRunScript = PRuntimeRunScript {
   pRuntimeRunScriptScriptId :: RuntimeScriptId,
   pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
   pRuntimeRunScriptObjectGroup :: Maybe String,
   pRuntimeRunScriptSilent :: Maybe Bool,
   pRuntimeRunScriptIncludeCommandLineApi :: Maybe Bool,
   pRuntimeRunScriptReturnByValue :: Maybe Bool,
   pRuntimeRunScriptGeneratePreview :: Maybe Bool,
   pRuntimeRunScriptAwaitPromise :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeRunScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PRuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


runtimeRunScript :: Handle ev -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript handle params = sendReceiveCommandResult handle "Runtime.runScript" (Just params)

data RuntimeRunScript = RuntimeRunScript {
   runtimeRunScriptResult :: RuntimeRemoteObject,
   runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command RuntimeRunScript where
   commandName _ = "Runtime.runScript"




data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
   pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PRuntimeSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


runtimeSetAsyncCallStackDepth :: Handle ev -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Runtime.setAsyncCallStackDepth" (Just params)



