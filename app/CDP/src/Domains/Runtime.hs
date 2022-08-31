{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.Runtime (module Domains.Runtime) where

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

import Utils

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Schema as Schema


data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
    runtimeConsoleApiCalledType :: String,
    runtimeConsoleApiCalledArgs :: [RuntimeRemoteObject],
    runtimeConsoleApiCalledExecutionContextId :: RuntimeExecutionContextId,
    runtimeConsoleApiCalledTimestamp :: RuntimeTimestamp,
    runtimeConsoleApiCalledStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeConsoleApiCalled where
    parseJSON = A.withObject "RuntimeConsoleApiCalled" $ \v ->
         RuntimeConsoleApiCalled <$> v .:  "type"
            <*> v  .:  "args"
            <*> v  .:  "executionContextId"
            <*> v  .:  "timestamp"
            <*> v  .:?  "stackTrace"


instance ToJSON RuntimeConsoleApiCalled  where
    toJSON v = A.object
        [ "type" .= runtimeConsoleApiCalledType v
        , "args" .= runtimeConsoleApiCalledArgs v
        , "executionContextId" .= runtimeConsoleApiCalledExecutionContextId v
        , "timestamp" .= runtimeConsoleApiCalledTimestamp v
        , "stackTrace" .= runtimeConsoleApiCalledStackTrace v
        ]


instance FromEvent Event RuntimeConsoleApiCalled where
    eventName  _ _    =  "Runtime.consoleAPICalled"
    fromEvent ev =  case ev of EVRuntimeConsoleApiCalled v -> Just v; _ -> Nothing

data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
    runtimeExceptionRevokedReason :: String,
    runtimeExceptionRevokedExceptionId :: Int
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionRevoked where
    parseJSON = A.withObject "RuntimeExceptionRevoked" $ \v ->
         RuntimeExceptionRevoked <$> v .:  "reason"
            <*> v  .:  "exceptionId"


instance ToJSON RuntimeExceptionRevoked  where
    toJSON v = A.object
        [ "reason" .= runtimeExceptionRevokedReason v
        , "exceptionId" .= runtimeExceptionRevokedExceptionId v
        ]


instance FromEvent Event RuntimeExceptionRevoked where
    eventName  _ _    =  "Runtime.exceptionRevoked"
    fromEvent ev =  case ev of EVRuntimeExceptionRevoked v -> Just v; _ -> Nothing

data RuntimeExceptionThrown = RuntimeExceptionThrown {
    runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
    runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionThrown where
    parseJSON = A.withObject "RuntimeExceptionThrown" $ \v ->
         RuntimeExceptionThrown <$> v .:  "timestamp"
            <*> v  .:  "exceptionDetails"


instance ToJSON RuntimeExceptionThrown  where
    toJSON v = A.object
        [ "timestamp" .= runtimeExceptionThrownTimestamp v
        , "exceptionDetails" .= runtimeExceptionThrownExceptionDetails v
        ]


instance FromEvent Event RuntimeExceptionThrown where
    eventName  _ _    =  "Runtime.exceptionThrown"
    fromEvent ev =  case ev of EVRuntimeExceptionThrown v -> Just v; _ -> Nothing

data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
    runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextCreated where
    parseJSON = A.withObject "RuntimeExecutionContextCreated" $ \v ->
         RuntimeExecutionContextCreated <$> v .:  "context"


instance ToJSON RuntimeExecutionContextCreated  where
    toJSON v = A.object
        [ "context" .= runtimeExecutionContextCreatedContext v
        ]


instance FromEvent Event RuntimeExecutionContextCreated where
    eventName  _ _    =  "Runtime.executionContextCreated"
    fromEvent ev =  case ev of EVRuntimeExecutionContextCreated v -> Just v; _ -> Nothing

data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
    runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextDestroyed where
    parseJSON = A.withObject "RuntimeExecutionContextDestroyed" $ \v ->
         RuntimeExecutionContextDestroyed <$> v .:  "executionContextId"


instance ToJSON RuntimeExecutionContextDestroyed  where
    toJSON v = A.object
        [ "executionContextId" .= runtimeExecutionContextDestroyedExecutionContextId v
        ]


instance FromEvent Event RuntimeExecutionContextDestroyed where
    eventName  _ _    =  "Runtime.executionContextDestroyed"
    fromEvent ev =  case ev of EVRuntimeExecutionContextDestroyed v -> Just v; _ -> Nothing

data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
    deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
    parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
        case v of
                "RuntimeExecutionContextsCleared" -> pure $ RuntimeExecutionContextsCleared
                _ -> fail "failed to parse RuntimeExecutionContextsCleared"

instance FromEvent Event RuntimeExecutionContextsCleared where
    eventName  _ _    =  "Runtime.executionContextsCleared"
    fromEvent ev =  case ev of EVRuntimeExecutionContextsCleared v -> Just v; _ -> Nothing

data RuntimeInspectRequested = RuntimeInspectRequested {
    runtimeInspectRequestedObject :: RuntimeRemoteObject,
    runtimeInspectRequestedHints :: [(String, String)]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeInspectRequested where
    parseJSON = A.withObject "RuntimeInspectRequested" $ \v ->
         RuntimeInspectRequested <$> v .:  "object"
            <*> v  .:  "hints"


instance ToJSON RuntimeInspectRequested  where
    toJSON v = A.object
        [ "object" .= runtimeInspectRequestedObject v
        , "hints" .= runtimeInspectRequestedHints v
        ]


instance FromEvent Event RuntimeInspectRequested where
    eventName  _ _    =  "Runtime.inspectRequested"
    fromEvent ev =  case ev of EVRuntimeInspectRequested v -> Just v; _ -> Nothing



type RuntimeScriptId = String

data RuntimeWebDriverValue = RuntimeWebDriverValue {
    runtimeWebDriverValueType :: String,
    runtimeWebDriverValueValue :: Maybe Int,
    runtimeWebDriverValueObjectId :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeWebDriverValue where
    parseJSON = A.withObject "RuntimeWebDriverValue" $ \v ->
         RuntimeWebDriverValue <$> v .:  "type"
            <*> v  .:?  "value"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeWebDriverValue  where
    toJSON v = A.object
        [ "type" .= runtimeWebDriverValueType v
        , "value" .= runtimeWebDriverValueValue v
        , "objectId" .= runtimeWebDriverValueObjectId v
        ]



type RuntimeRemoteObjectId = String

type RuntimeUnserializableValue = String

data RuntimeRemoteObject = RuntimeRemoteObject {
    runtimeRemoteObjectType :: String,
    runtimeRemoteObjectSubtype :: Maybe String,
    runtimeRemoteObjectClassName :: Maybe String,
    runtimeRemoteObjectValue :: Maybe Int,
    runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeRemoteObjectDescription :: Maybe String,
    runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeRemoteObject where
    parseJSON = A.withObject "RuntimeRemoteObject" $ \v ->
         RuntimeRemoteObject <$> v .:  "type"
            <*> v  .:?  "subtype"
            <*> v  .:?  "className"
            <*> v  .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "description"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeRemoteObject  where
    toJSON v = A.object
        [ "type" .= runtimeRemoteObjectType v
        , "subtype" .= runtimeRemoteObjectSubtype v
        , "className" .= runtimeRemoteObjectClassName v
        , "value" .= runtimeRemoteObjectValue v
        , "unserializableValue" .= runtimeRemoteObjectUnserializableValue v
        , "description" .= runtimeRemoteObjectDescription v
        , "objectId" .= runtimeRemoteObjectObjectId v
        ]



data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
    runtimePropertyDescriptorName :: String,
    runtimePropertyDescriptorConfigurable :: Bool,
    runtimePropertyDescriptorEnumerable :: Bool,
    runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWritable :: Maybe Bool,
    runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWasThrown :: Maybe Bool,
    runtimePropertyDescriptorIsOwn :: Maybe Bool,
    runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimePropertyDescriptor where
    parseJSON = A.withObject "RuntimePropertyDescriptor" $ \v ->
         RuntimePropertyDescriptor <$> v .:  "name"
            <*> v  .:  "configurable"
            <*> v  .:  "enumerable"
            <*> v  .:?  "value"
            <*> v  .:?  "writable"
            <*> v  .:?  "get"
            <*> v  .:?  "set"
            <*> v  .:?  "wasThrown"
            <*> v  .:?  "isOwn"
            <*> v  .:?  "symbol"


instance ToJSON RuntimePropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimePropertyDescriptorName v
        , "configurable" .= runtimePropertyDescriptorConfigurable v
        , "enumerable" .= runtimePropertyDescriptorEnumerable v
        , "value" .= runtimePropertyDescriptorValue v
        , "writable" .= runtimePropertyDescriptorWritable v
        , "get" .= runtimePropertyDescriptorGet v
        , "set" .= runtimePropertyDescriptorSet v
        , "wasThrown" .= runtimePropertyDescriptorWasThrown v
        , "isOwn" .= runtimePropertyDescriptorIsOwn v
        , "symbol" .= runtimePropertyDescriptorSymbol v
        ]



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
    runtimeInternalPropertyDescriptorName :: String,
    runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeInternalPropertyDescriptor where
    parseJSON = A.withObject "RuntimeInternalPropertyDescriptor" $ \v ->
         RuntimeInternalPropertyDescriptor <$> v .:  "name"
            <*> v  .:?  "value"


instance ToJSON RuntimeInternalPropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimeInternalPropertyDescriptorName v
        , "value" .= runtimeInternalPropertyDescriptorValue v
        ]



data RuntimeCallArgument = RuntimeCallArgument {
    runtimeCallArgumentValue :: Maybe Int,
    runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallArgument where
    parseJSON = A.withObject "RuntimeCallArgument" $ \v ->
         RuntimeCallArgument <$> v .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeCallArgument  where
    toJSON v = A.object
        [ "value" .= runtimeCallArgumentValue v
        , "unserializableValue" .= runtimeCallArgumentUnserializableValue v
        , "objectId" .= runtimeCallArgumentObjectId v
        ]



type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
    runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
    runtimeExecutionContextDescriptionOrigin :: String,
    runtimeExecutionContextDescriptionName :: String,
    runtimeExecutionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExecutionContextDescription where
    parseJSON = A.withObject "RuntimeExecutionContextDescription" $ \v ->
         RuntimeExecutionContextDescription <$> v .:  "id"
            <*> v  .:  "origin"
            <*> v  .:  "name"
            <*> v  .:?  "auxData"


instance ToJSON RuntimeExecutionContextDescription  where
    toJSON v = A.object
        [ "id" .= runtimeExecutionContextDescriptionId v
        , "origin" .= runtimeExecutionContextDescriptionOrigin v
        , "name" .= runtimeExecutionContextDescriptionName v
        , "auxData" .= runtimeExecutionContextDescriptionAuxData v
        ]



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
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeExceptionDetails where
    parseJSON = A.withObject "RuntimeExceptionDetails" $ \v ->
         RuntimeExceptionDetails <$> v .:  "exceptionId"
            <*> v  .:  "text"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "scriptId"
            <*> v  .:?  "url"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "exception"
            <*> v  .:?  "executionContextId"


instance ToJSON RuntimeExceptionDetails  where
    toJSON v = A.object
        [ "exceptionId" .= runtimeExceptionDetailsExceptionId v
        , "text" .= runtimeExceptionDetailsText v
        , "lineNumber" .= runtimeExceptionDetailsLineNumber v
        , "columnNumber" .= runtimeExceptionDetailsColumnNumber v
        , "scriptId" .= runtimeExceptionDetailsScriptId v
        , "url" .= runtimeExceptionDetailsUrl v
        , "stackTrace" .= runtimeExceptionDetailsStackTrace v
        , "exception" .= runtimeExceptionDetailsException v
        , "executionContextId" .= runtimeExceptionDetailsExecutionContextId v
        ]



type RuntimeTimestamp = Int

type RuntimeTimeDelta = Int

data RuntimeCallFrame = RuntimeCallFrame {
    runtimeCallFrameFunctionName :: String,
    runtimeCallFrameScriptId :: RuntimeScriptId,
    runtimeCallFrameUrl :: String,
    runtimeCallFrameLineNumber :: Int,
    runtimeCallFrameColumnNumber :: Int
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallFrame where
    parseJSON = A.withObject "RuntimeCallFrame" $ \v ->
         RuntimeCallFrame <$> v .:  "functionName"
            <*> v  .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"


instance ToJSON RuntimeCallFrame  where
    toJSON v = A.object
        [ "functionName" .= runtimeCallFrameFunctionName v
        , "scriptId" .= runtimeCallFrameScriptId v
        , "url" .= runtimeCallFrameUrl v
        , "lineNumber" .= runtimeCallFrameLineNumber v
        , "columnNumber" .= runtimeCallFrameColumnNumber v
        ]



data RuntimeStackTrace = RuntimeStackTrace {
    runtimeStackTraceCallFrames :: [RuntimeCallFrame],
    runtimeStackTraceDescription :: Maybe String,
    runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeStackTrace where
    parseJSON = A.withObject "RuntimeStackTrace" $ \v ->
         RuntimeStackTrace <$> v .:  "callFrames"
            <*> v  .:?  "description"
            <*> v  .:?  "parent"


instance ToJSON RuntimeStackTrace  where
    toJSON v = A.object
        [ "callFrames" .= runtimeStackTraceCallFrames v
        , "description" .= runtimeStackTraceDescription v
        , "parent" .= runtimeStackTraceParent v
        ]



data RuntimeAwaitPromise = RuntimeAwaitPromise {
    runtimeAwaitPromiseResult :: RuntimeRemoteObject,
    runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeAwaitPromise where
    parseJSON = A.withObject "RuntimeAwaitPromise" $ \v ->
         RuntimeAwaitPromise <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeAwaitPromise where
    commandName _ = "Runtime.awaitPromise"

data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
    pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
    pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
    pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeAwaitPromise where
    parseJSON = A.withObject "PRuntimeAwaitPromise" $ \v ->
         PRuntimeAwaitPromise <$> v .:  "promiseObjectId"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "generatePreview"


instance ToJSON PRuntimeAwaitPromise  where
    toJSON v = A.object
        [ "promiseObjectId" .= pRuntimeAwaitPromisePromiseObjectId v
        , "returnByValue" .= pRuntimeAwaitPromiseReturnByValue v
        , "generatePreview" .= pRuntimeAwaitPromiseGeneratePreview v
        ]


runtimeAwaitPromise :: Session -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise session params = sendReceiveCommandResult session "Runtime.awaitPromise" (Just params)

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCallFunctionOn where
    parseJSON = A.withObject "RuntimeCallFunctionOn" $ \v ->
         RuntimeCallFunctionOn <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeCallFunctionOn where
    commandName _ = "Runtime.callFunctionOn"

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
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeCallFunctionOn where
    parseJSON = A.withObject "PRuntimeCallFunctionOn" $ \v ->
         PRuntimeCallFunctionOn <$> v .:  "functionDeclaration"
            <*> v  .:?  "objectId"
            <*> v  .:?  "arguments"
            <*> v  .:?  "silent"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "userGesture"
            <*> v  .:?  "awaitPromise"
            <*> v  .:?  "executionContextId"
            <*> v  .:?  "objectGroup"


instance ToJSON PRuntimeCallFunctionOn  where
    toJSON v = A.object
        [ "functionDeclaration" .= pRuntimeCallFunctionOnFunctionDeclaration v
        , "objectId" .= pRuntimeCallFunctionOnObjectId v
        , "arguments" .= pRuntimeCallFunctionOnArguments v
        , "silent" .= pRuntimeCallFunctionOnSilent v
        , "returnByValue" .= pRuntimeCallFunctionOnReturnByValue v
        , "userGesture" .= pRuntimeCallFunctionOnUserGesture v
        , "awaitPromise" .= pRuntimeCallFunctionOnAwaitPromise v
        , "executionContextId" .= pRuntimeCallFunctionOnExecutionContextId v
        , "objectGroup" .= pRuntimeCallFunctionOnObjectGroup v
        ]


runtimeCallFunctionOn :: Session -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn session params = sendReceiveCommandResult session "Runtime.callFunctionOn" (Just params)

data RuntimeCompileScript = RuntimeCompileScript {
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeCompileScript where
    parseJSON = A.withObject "RuntimeCompileScript" $ \v ->
         RuntimeCompileScript <$> v .:?  "scriptId"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeCompileScript where
    commandName _ = "Runtime.compileScript"

data PRuntimeCompileScript = PRuntimeCompileScript {
    pRuntimeCompileScriptExpression :: String,
    pRuntimeCompileScriptSourceUrl :: String,
    pRuntimeCompileScriptPersistScript :: Bool,
    pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeCompileScript where
    parseJSON = A.withObject "PRuntimeCompileScript" $ \v ->
         PRuntimeCompileScript <$> v .:  "expression"
            <*> v  .:  "sourceURL"
            <*> v  .:  "persistScript"
            <*> v  .:?  "executionContextId"


instance ToJSON PRuntimeCompileScript  where
    toJSON v = A.object
        [ "expression" .= pRuntimeCompileScriptExpression v
        , "sourceURL" .= pRuntimeCompileScriptSourceUrl v
        , "persistScript" .= pRuntimeCompileScriptPersistScript v
        , "executionContextId" .= pRuntimeCompileScriptExecutionContextId v
        ]


runtimeCompileScript :: Session -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript session params = sendReceiveCommandResult session "Runtime.compileScript" (Just params)




runtimeDisable :: Session -> IO (Maybe Error)
runtimeDisable session = sendReceiveCommand session "Runtime.disable" (Nothing :: Maybe ())




runtimeDiscardConsoleEntries :: Session -> IO (Maybe Error)
runtimeDiscardConsoleEntries session = sendReceiveCommand session "Runtime.discardConsoleEntries" (Nothing :: Maybe ())




runtimeEnable :: Session -> IO (Maybe Error)
runtimeEnable session = sendReceiveCommand session "Runtime.enable" (Nothing :: Maybe ())

data RuntimeEvaluate = RuntimeEvaluate {
    runtimeEvaluateResult :: RuntimeRemoteObject,
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeEvaluate where
    parseJSON = A.withObject "RuntimeEvaluate" $ \v ->
         RuntimeEvaluate <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeEvaluate where
    commandName _ = "Runtime.evaluate"

data PRuntimeEvaluate = PRuntimeEvaluate {
    pRuntimeEvaluateExpression :: String,
    pRuntimeEvaluateObjectGroup :: Maybe String,
    pRuntimeEvaluateIncludeCommandLineApi :: Maybe Bool,
    pRuntimeEvaluateSilent :: Maybe Bool,
    pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeEvaluateReturnByValue :: Maybe Bool,
    pRuntimeEvaluateUserGesture :: Maybe Bool,
    pRuntimeEvaluateAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeEvaluate where
    parseJSON = A.withObject "PRuntimeEvaluate" $ \v ->
         PRuntimeEvaluate <$> v .:  "expression"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "silent"
            <*> v  .:?  "contextId"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "userGesture"
            <*> v  .:?  "awaitPromise"


instance ToJSON PRuntimeEvaluate  where
    toJSON v = A.object
        [ "expression" .= pRuntimeEvaluateExpression v
        , "objectGroup" .= pRuntimeEvaluateObjectGroup v
        , "includeCommandLineAPI" .= pRuntimeEvaluateIncludeCommandLineApi v
        , "silent" .= pRuntimeEvaluateSilent v
        , "contextId" .= pRuntimeEvaluateContextId v
        , "returnByValue" .= pRuntimeEvaluateReturnByValue v
        , "userGesture" .= pRuntimeEvaluateUserGesture v
        , "awaitPromise" .= pRuntimeEvaluateAwaitPromise v
        ]


runtimeEvaluate :: Session -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate session params = sendReceiveCommandResult session "Runtime.evaluate" (Just params)

data RuntimeGetProperties = RuntimeGetProperties {
    runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
    runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
    runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeGetProperties where
    parseJSON = A.withObject "RuntimeGetProperties" $ \v ->
         RuntimeGetProperties <$> v .:  "result"
            <*> v  .:?  "internalProperties"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeGetProperties where
    commandName _ = "Runtime.getProperties"

data PRuntimeGetProperties = PRuntimeGetProperties {
    pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
    pRuntimeGetPropertiesOwnProperties :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeGetProperties where
    parseJSON = A.withObject "PRuntimeGetProperties" $ \v ->
         PRuntimeGetProperties <$> v .:  "objectId"
            <*> v  .:?  "ownProperties"


instance ToJSON PRuntimeGetProperties  where
    toJSON v = A.object
        [ "objectId" .= pRuntimeGetPropertiesObjectId v
        , "ownProperties" .= pRuntimeGetPropertiesOwnProperties v
        ]


runtimeGetProperties :: Session -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties session params = sendReceiveCommandResult session "Runtime.getProperties" (Just params)

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
    runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "RuntimeGlobalLexicalScopeNames" $ \v ->
         RuntimeGlobalLexicalScopeNames <$> v .:  "names"



instance Command  RuntimeGlobalLexicalScopeNames where
    commandName _ = "Runtime.globalLexicalScopeNames"

data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
    pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "PRuntimeGlobalLexicalScopeNames" $ \v ->
         PRuntimeGlobalLexicalScopeNames <$> v .:?  "executionContextId"


instance ToJSON PRuntimeGlobalLexicalScopeNames  where
    toJSON v = A.object
        [ "executionContextId" .= pRuntimeGlobalLexicalScopeNamesExecutionContextId v
        ]


runtimeGlobalLexicalScopeNames :: Session -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames session params = sendReceiveCommandResult session "Runtime.globalLexicalScopeNames" (Just params)

data RuntimeQueryObjects = RuntimeQueryObjects {
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeQueryObjects where
    parseJSON = A.withObject "RuntimeQueryObjects" $ \v ->
         RuntimeQueryObjects <$> v .:  "objects"



instance Command  RuntimeQueryObjects where
    commandName _ = "Runtime.queryObjects"

data PRuntimeQueryObjects = PRuntimeQueryObjects {
    pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
    pRuntimeQueryObjectsObjectGroup :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeQueryObjects where
    parseJSON = A.withObject "PRuntimeQueryObjects" $ \v ->
         PRuntimeQueryObjects <$> v .:  "prototypeObjectId"
            <*> v  .:?  "objectGroup"


instance ToJSON PRuntimeQueryObjects  where
    toJSON v = A.object
        [ "prototypeObjectId" .= pRuntimeQueryObjectsPrototypeObjectId v
        , "objectGroup" .= pRuntimeQueryObjectsObjectGroup v
        ]


runtimeQueryObjects :: Session -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects session params = sendReceiveCommandResult session "Runtime.queryObjects" (Just params)



data PRuntimeReleaseObject = PRuntimeReleaseObject {
    pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeReleaseObject where
    parseJSON = A.withObject "PRuntimeReleaseObject" $ \v ->
         PRuntimeReleaseObject <$> v .:  "objectId"


instance ToJSON PRuntimeReleaseObject  where
    toJSON v = A.object
        [ "objectId" .= pRuntimeReleaseObjectObjectId v
        ]


runtimeReleaseObject :: Session -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject session params = sendReceiveCommand session "Runtime.releaseObject" (Just params)



data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
    pRuntimeReleaseObjectGroupObjectGroup :: String
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeReleaseObjectGroup where
    parseJSON = A.withObject "PRuntimeReleaseObjectGroup" $ \v ->
         PRuntimeReleaseObjectGroup <$> v .:  "objectGroup"


instance ToJSON PRuntimeReleaseObjectGroup  where
    toJSON v = A.object
        [ "objectGroup" .= pRuntimeReleaseObjectGroupObjectGroup v
        ]


runtimeReleaseObjectGroup :: Session -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup session params = sendReceiveCommand session "Runtime.releaseObjectGroup" (Just params)




runtimeRunIfWaitingForDebugger :: Session -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger session = sendReceiveCommand session "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())

data RuntimeRunScript = RuntimeRunScript {
    runtimeRunScriptResult :: RuntimeRemoteObject,
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  RuntimeRunScript where
    parseJSON = A.withObject "RuntimeRunScript" $ \v ->
         RuntimeRunScript <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  RuntimeRunScript where
    commandName _ = "Runtime.runScript"

data PRuntimeRunScript = PRuntimeRunScript {
    pRuntimeRunScriptScriptId :: RuntimeScriptId,
    pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
    pRuntimeRunScriptObjectGroup :: Maybe String,
    pRuntimeRunScriptSilent :: Maybe Bool,
    pRuntimeRunScriptIncludeCommandLineApi :: Maybe Bool,
    pRuntimeRunScriptReturnByValue :: Maybe Bool,
    pRuntimeRunScriptGeneratePreview :: Maybe Bool,
    pRuntimeRunScriptAwaitPromise :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeRunScript where
    parseJSON = A.withObject "PRuntimeRunScript" $ \v ->
         PRuntimeRunScript <$> v .:  "scriptId"
            <*> v  .:?  "executionContextId"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "silent"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "generatePreview"
            <*> v  .:?  "awaitPromise"


instance ToJSON PRuntimeRunScript  where
    toJSON v = A.object
        [ "scriptId" .= pRuntimeRunScriptScriptId v
        , "executionContextId" .= pRuntimeRunScriptExecutionContextId v
        , "objectGroup" .= pRuntimeRunScriptObjectGroup v
        , "silent" .= pRuntimeRunScriptSilent v
        , "includeCommandLineAPI" .= pRuntimeRunScriptIncludeCommandLineApi v
        , "returnByValue" .= pRuntimeRunScriptReturnByValue v
        , "generatePreview" .= pRuntimeRunScriptGeneratePreview v
        , "awaitPromise" .= pRuntimeRunScriptAwaitPromise v
        ]


runtimeRunScript :: Session -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript session params = sendReceiveCommandResult session "Runtime.runScript" (Just params)



data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
    pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PRuntimeSetAsyncCallStackDepth where
    parseJSON = A.withObject "PRuntimeSetAsyncCallStackDepth" $ \v ->
         PRuntimeSetAsyncCallStackDepth <$> v .:  "maxDepth"


instance ToJSON PRuntimeSetAsyncCallStackDepth  where
    toJSON v = A.object
        [ "maxDepth" .= pRuntimeSetAsyncCallStackDepthMaxDepth v
        ]


runtimeSetAsyncCallStackDepth :: Session -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth session params = sendReceiveCommand session "Runtime.setAsyncCallStackDepth" (Just params)

