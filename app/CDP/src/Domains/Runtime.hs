{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Runtime (module Domains.Runtime) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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



import Utils

data ConsoleApiCalled = ConsoleApiCalled {
    consoleApiCalledType :: String,
    consoleApiCalledArgs :: [RemoteObject],
    consoleApiCalledExecutionContextId :: ExecutionContextId,
    consoleApiCalledTimestamp :: Timestamp,
    consoleApiCalledStackTrace :: Maybe StackTrace
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ConsoleApiCalled where
    parseJSON = A.withObject "ConsoleApiCalled" $ \v ->
         ConsoleApiCalled <$> v .:  "type"
            <*> v  .:  "args"
            <*> v  .:  "executionContextId"
            <*> v  .:  "timestamp"
            <*> v  .:?  "stackTrace"


instance ToJSON ConsoleApiCalled  where
    toJSON v = A.object
        [ "type" .= consoleApiCalledType v
        , "args" .= consoleApiCalledArgs v
        , "executionContextId" .= consoleApiCalledExecutionContextId v
        , "timestamp" .= consoleApiCalledTimestamp v
        , "stackTrace" .= consoleApiCalledStackTrace v
        ]


data ExceptionRevoked = ExceptionRevoked {
    exceptionRevokedReason :: String,
    exceptionRevokedExceptionId :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExceptionRevoked where
    parseJSON = A.withObject "ExceptionRevoked" $ \v ->
         ExceptionRevoked <$> v .:  "reason"
            <*> v  .:  "exceptionId"


instance ToJSON ExceptionRevoked  where
    toJSON v = A.object
        [ "reason" .= exceptionRevokedReason v
        , "exceptionId" .= exceptionRevokedExceptionId v
        ]


data ExceptionThrown = ExceptionThrown {
    exceptionThrownTimestamp :: Timestamp,
    exceptionThrownExceptionDetails :: ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExceptionThrown where
    parseJSON = A.withObject "ExceptionThrown" $ \v ->
         ExceptionThrown <$> v .:  "timestamp"
            <*> v  .:  "exceptionDetails"


instance ToJSON ExceptionThrown  where
    toJSON v = A.object
        [ "timestamp" .= exceptionThrownTimestamp v
        , "exceptionDetails" .= exceptionThrownExceptionDetails v
        ]


data ExecutionContextCreated = ExecutionContextCreated {
    executionContextCreatedContext :: ExecutionContextDescription
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExecutionContextCreated where
    parseJSON = A.withObject "ExecutionContextCreated" $ \v ->
         ExecutionContextCreated <$> v .:  "context"


instance ToJSON ExecutionContextCreated  where
    toJSON v = A.object
        [ "context" .= executionContextCreatedContext v
        ]


data ExecutionContextDestroyed = ExecutionContextDestroyed {
    executionContextDestroyedExecutionContextId :: ExecutionContextId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExecutionContextDestroyed where
    parseJSON = A.withObject "ExecutionContextDestroyed" $ \v ->
         ExecutionContextDestroyed <$> v .:  "executionContextId"


instance ToJSON ExecutionContextDestroyed  where
    toJSON v = A.object
        [ "executionContextId" .= executionContextDestroyedExecutionContextId v
        ]


data ExecutionContextsCleared = ExecutionContextsCleared
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ExecutionContextsCleared where
    parseJSON = A.withText  "ExecutionContextsCleared"  $ \v -> do
        pure $ case v of
                "ExecutionContextsCleared" -> ExecutionContextsCleared
                _ -> error "failed to parse ExecutionContextsCleared"

data InspectRequested = InspectRequested {
    inspectRequestedObject :: RemoteObject,
    inspectRequestedHints :: [(String, String)]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  InspectRequested where
    parseJSON = A.withObject "InspectRequested" $ \v ->
         InspectRequested <$> v .:  "object"
            <*> v  .:  "hints"


instance ToJSON InspectRequested  where
    toJSON v = A.object
        [ "object" .= inspectRequestedObject v
        , "hints" .= inspectRequestedHints v
        ]



type ScriptId = String

data WebDriverValue = WebDriverValue {
    webDriverValueType :: String,
    webDriverValueValue :: Maybe Int,
    webDriverValueObjectId :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebDriverValue where
    parseJSON = A.withObject "WebDriverValue" $ \v ->
         WebDriverValue <$> v .:  "type"
            <*> v  .:?  "value"
            <*> v  .:?  "objectId"


instance ToJSON WebDriverValue  where
    toJSON v = A.object
        [ "type" .= webDriverValueType v
        , "value" .= webDriverValueValue v
        , "objectId" .= webDriverValueObjectId v
        ]



type RemoteObjectId = String

type UnserializableValue = String

data RemoteObject = RemoteObject {
    remoteObjectType :: String,
    remoteObjectSubtype :: Maybe String,
    remoteObjectClassName :: Maybe String,
    remoteObjectValue :: Maybe Int,
    remoteObjectUnserializableValue :: Maybe UnserializableValue,
    remoteObjectDescription :: Maybe String,
    remoteObjectObjectId :: Maybe RemoteObjectId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RemoteObject where
    parseJSON = A.withObject "RemoteObject" $ \v ->
         RemoteObject <$> v .:  "type"
            <*> v  .:?  "subtype"
            <*> v  .:?  "className"
            <*> v  .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "description"
            <*> v  .:?  "objectId"


instance ToJSON RemoteObject  where
    toJSON v = A.object
        [ "type" .= remoteObjectType v
        , "subtype" .= remoteObjectSubtype v
        , "className" .= remoteObjectClassName v
        , "value" .= remoteObjectValue v
        , "unserializableValue" .= remoteObjectUnserializableValue v
        , "description" .= remoteObjectDescription v
        , "objectId" .= remoteObjectObjectId v
        ]



data PropertyDescriptor = PropertyDescriptor {
    propertyDescriptorName :: String,
    propertyDescriptorConfigurable :: Bool,
    propertyDescriptorEnumerable :: Bool,
    propertyDescriptorValue :: Maybe RemoteObject,
    propertyDescriptorWritable :: Maybe Bool,
    propertyDescriptorGet :: Maybe RemoteObject,
    propertyDescriptorSet :: Maybe RemoteObject,
    propertyDescriptorWasThrown :: Maybe Bool,
    propertyDescriptorIsOwn :: Maybe Bool,
    propertyDescriptorSymbol :: Maybe RemoteObject
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  PropertyDescriptor where
    parseJSON = A.withObject "PropertyDescriptor" $ \v ->
         PropertyDescriptor <$> v .:  "name"
            <*> v  .:  "configurable"
            <*> v  .:  "enumerable"
            <*> v  .:?  "value"
            <*> v  .:?  "writable"
            <*> v  .:?  "get"
            <*> v  .:?  "set"
            <*> v  .:?  "wasThrown"
            <*> v  .:?  "isOwn"
            <*> v  .:?  "symbol"


instance ToJSON PropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= propertyDescriptorName v
        , "configurable" .= propertyDescriptorConfigurable v
        , "enumerable" .= propertyDescriptorEnumerable v
        , "value" .= propertyDescriptorValue v
        , "writable" .= propertyDescriptorWritable v
        , "get" .= propertyDescriptorGet v
        , "set" .= propertyDescriptorSet v
        , "wasThrown" .= propertyDescriptorWasThrown v
        , "isOwn" .= propertyDescriptorIsOwn v
        , "symbol" .= propertyDescriptorSymbol v
        ]



data InternalPropertyDescriptor = InternalPropertyDescriptor {
    internalPropertyDescriptorName :: String,
    internalPropertyDescriptorValue :: Maybe RemoteObject
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  InternalPropertyDescriptor where
    parseJSON = A.withObject "InternalPropertyDescriptor" $ \v ->
         InternalPropertyDescriptor <$> v .:  "name"
            <*> v  .:?  "value"


instance ToJSON InternalPropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= internalPropertyDescriptorName v
        , "value" .= internalPropertyDescriptorValue v
        ]



data CallArgument = CallArgument {
    callArgumentValue :: Maybe Int,
    callArgumentUnserializableValue :: Maybe UnserializableValue,
    callArgumentObjectId :: Maybe RemoteObjectId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CallArgument where
    parseJSON = A.withObject "CallArgument" $ \v ->
         CallArgument <$> v .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "objectId"


instance ToJSON CallArgument  where
    toJSON v = A.object
        [ "value" .= callArgumentValue v
        , "unserializableValue" .= callArgumentUnserializableValue v
        , "objectId" .= callArgumentObjectId v
        ]



type ExecutionContextId = Int

data ExecutionContextDescription = ExecutionContextDescription {
    executionContextDescriptionId :: ExecutionContextId,
    executionContextDescriptionOrigin :: String,
    executionContextDescriptionName :: String,
    executionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExecutionContextDescription where
    parseJSON = A.withObject "ExecutionContextDescription" $ \v ->
         ExecutionContextDescription <$> v .:  "id"
            <*> v  .:  "origin"
            <*> v  .:  "name"
            <*> v  .:?  "auxData"


instance ToJSON ExecutionContextDescription  where
    toJSON v = A.object
        [ "id" .= executionContextDescriptionId v
        , "origin" .= executionContextDescriptionOrigin v
        , "name" .= executionContextDescriptionName v
        , "auxData" .= executionContextDescriptionAuxData v
        ]



data ExceptionDetails = ExceptionDetails {
    exceptionDetailsExceptionId :: Int,
    exceptionDetailsText :: String,
    exceptionDetailsLineNumber :: Int,
    exceptionDetailsColumnNumber :: Int,
    exceptionDetailsScriptId :: Maybe ScriptId,
    exceptionDetailsUrl :: Maybe String,
    exceptionDetailsStackTrace :: Maybe StackTrace,
    exceptionDetailsException :: Maybe RemoteObject,
    exceptionDetailsExecutionContextId :: Maybe ExecutionContextId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ExceptionDetails where
    parseJSON = A.withObject "ExceptionDetails" $ \v ->
         ExceptionDetails <$> v .:  "exceptionId"
            <*> v  .:  "text"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "scriptId"
            <*> v  .:?  "url"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "exception"
            <*> v  .:?  "executionContextId"


instance ToJSON ExceptionDetails  where
    toJSON v = A.object
        [ "exceptionId" .= exceptionDetailsExceptionId v
        , "text" .= exceptionDetailsText v
        , "lineNumber" .= exceptionDetailsLineNumber v
        , "columnNumber" .= exceptionDetailsColumnNumber v
        , "scriptId" .= exceptionDetailsScriptId v
        , "url" .= exceptionDetailsUrl v
        , "stackTrace" .= exceptionDetailsStackTrace v
        , "exception" .= exceptionDetailsException v
        , "executionContextId" .= exceptionDetailsExecutionContextId v
        ]



type Timestamp = Int

type TimeDelta = Int

data CallFrame = CallFrame {
    callFrameFunctionName :: String,
    callFrameScriptId :: ScriptId,
    callFrameUrl :: String,
    callFrameLineNumber :: Int,
    callFrameColumnNumber :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CallFrame where
    parseJSON = A.withObject "CallFrame" $ \v ->
         CallFrame <$> v .:  "functionName"
            <*> v  .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"


instance ToJSON CallFrame  where
    toJSON v = A.object
        [ "functionName" .= callFrameFunctionName v
        , "scriptId" .= callFrameScriptId v
        , "url" .= callFrameUrl v
        , "lineNumber" .= callFrameLineNumber v
        , "columnNumber" .= callFrameColumnNumber v
        ]



data StackTrace = StackTrace {
    stackTraceCallFrames :: [CallFrame],
    stackTraceDescription :: Maybe String,
    stackTraceParent :: Maybe StackTrace
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  StackTrace where
    parseJSON = A.withObject "StackTrace" $ \v ->
         StackTrace <$> v .:  "callFrames"
            <*> v  .:?  "description"
            <*> v  .:?  "parent"


instance ToJSON StackTrace  where
    toJSON v = A.object
        [ "callFrames" .= stackTraceCallFrames v
        , "description" .= stackTraceDescription v
        , "parent" .= stackTraceParent v
        ]


data AwaitPromise = AwaitPromise {
    awaitPromiseResult :: RemoteObject,
    awaitPromiseExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AwaitPromise where
    parseJSON = A.withObject "AwaitPromise" $ \v ->
         AwaitPromise <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



awaitPromise :: Session a -> RemoteObjectId -> Maybe Bool -> Maybe Bool -> IO (Either Error AwaitPromise)
awaitPromise session awaitPromisePromiseObjectId awaitPromiseReturnByValue awaitPromiseGeneratePreview = sendReceiveCommandResult (conn session) ("Runtime","awaitPromise") ([("promiseObjectId", ToJSONEx awaitPromisePromiseObjectId)] ++ (catMaybes [fmap (("returnByValue",) . ToJSONEx) awaitPromiseReturnByValue, fmap (("generatePreview",) . ToJSONEx) awaitPromiseGeneratePreview]))

data CallFunctionOn = CallFunctionOn {
    callFunctionOnResult :: RemoteObject,
    callFunctionOnExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CallFunctionOn where
    parseJSON = A.withObject "CallFunctionOn" $ \v ->
         CallFunctionOn <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



callFunctionOn :: Session a -> String -> Maybe RemoteObjectId -> Maybe [CallArgument] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe ExecutionContextId -> Maybe String -> IO (Either Error CallFunctionOn)
callFunctionOn session callFunctionOnFunctionDeclaration callFunctionOnObjectId callFunctionOnArguments callFunctionOnSilent callFunctionOnReturnByValue callFunctionOnUserGesture callFunctionOnAwaitPromise callFunctionOnExecutionContextId callFunctionOnObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","callFunctionOn") ([("functionDeclaration", ToJSONEx callFunctionOnFunctionDeclaration)] ++ (catMaybes [fmap (("objectId",) . ToJSONEx) callFunctionOnObjectId, fmap (("arguments",) . ToJSONEx) callFunctionOnArguments, fmap (("silent",) . ToJSONEx) callFunctionOnSilent, fmap (("returnByValue",) . ToJSONEx) callFunctionOnReturnByValue, fmap (("userGesture",) . ToJSONEx) callFunctionOnUserGesture, fmap (("awaitPromise",) . ToJSONEx) callFunctionOnAwaitPromise, fmap (("executionContextId",) . ToJSONEx) callFunctionOnExecutionContextId, fmap (("objectGroup",) . ToJSONEx) callFunctionOnObjectGroup]))

data CompileScript = CompileScript {
    compileScriptScriptId :: Maybe ScriptId,
    compileScriptExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CompileScript where
    parseJSON = A.withObject "CompileScript" $ \v ->
         CompileScript <$> v .:?  "scriptId"
            <*> v  .:?  "exceptionDetails"



compileScript :: Session a -> String -> String -> Bool -> Maybe ExecutionContextId -> IO (Either Error CompileScript)
compileScript session compileScriptExpression compileScriptSourceUrl compileScriptPersistScript compileScriptExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","compileScript") ([("expression", ToJSONEx compileScriptExpression), ("sourceURL", ToJSONEx compileScriptSourceUrl), ("persistScript", ToJSONEx compileScriptPersistScript)] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) compileScriptExecutionContextId]))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Runtime","disable") ([] ++ (catMaybes []))


discardConsoleEntries :: Session a -> IO (Maybe Error)
discardConsoleEntries session  = sendReceiveCommand (conn session) ("Runtime","discardConsoleEntries") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Runtime","enable") ([] ++ (catMaybes []))

data Evaluate = Evaluate {
    evaluateResult :: RemoteObject,
    evaluateExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Evaluate where
    parseJSON = A.withObject "Evaluate" $ \v ->
         Evaluate <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



evaluate :: Session a -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe ExecutionContextId -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error Evaluate)
evaluate session evaluateExpression evaluateObjectGroup evaluateIncludeCommandLineApi evaluateSilent evaluateContextId evaluateReturnByValue evaluateUserGesture evaluateAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","evaluate") ([("expression", ToJSONEx evaluateExpression)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) evaluateObjectGroup, fmap (("includeCommandLineAPI",) . ToJSONEx) evaluateIncludeCommandLineApi, fmap (("silent",) . ToJSONEx) evaluateSilent, fmap (("contextId",) . ToJSONEx) evaluateContextId, fmap (("returnByValue",) . ToJSONEx) evaluateReturnByValue, fmap (("userGesture",) . ToJSONEx) evaluateUserGesture, fmap (("awaitPromise",) . ToJSONEx) evaluateAwaitPromise]))

data GetProperties = GetProperties {
    getPropertiesResult :: [PropertyDescriptor],
    getPropertiesInternalProperties :: Maybe [InternalPropertyDescriptor],
    getPropertiesExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetProperties where
    parseJSON = A.withObject "GetProperties" $ \v ->
         GetProperties <$> v .:  "result"
            <*> v  .:?  "internalProperties"
            <*> v  .:?  "exceptionDetails"



getProperties :: Session a -> RemoteObjectId -> Maybe Bool -> IO (Either Error GetProperties)
getProperties session getPropertiesObjectId getPropertiesOwnProperties = sendReceiveCommandResult (conn session) ("Runtime","getProperties") ([("objectId", ToJSONEx getPropertiesObjectId)] ++ (catMaybes [fmap (("ownProperties",) . ToJSONEx) getPropertiesOwnProperties]))

data GlobalLexicalScopeNames = GlobalLexicalScopeNames {
    globalLexicalScopeNamesNames :: [String]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GlobalLexicalScopeNames where
    parseJSON = A.withObject "GlobalLexicalScopeNames" $ \v ->
         GlobalLexicalScopeNames <$> v .:  "names"



globalLexicalScopeNames :: Session a -> Maybe ExecutionContextId -> IO (Either Error GlobalLexicalScopeNames)
globalLexicalScopeNames session globalLexicalScopeNamesExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","globalLexicalScopeNames") ([] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) globalLexicalScopeNamesExecutionContextId]))

data QueryObjects = QueryObjects {
    queryObjectsObjects :: RemoteObject
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  QueryObjects where
    parseJSON = A.withObject "QueryObjects" $ \v ->
         QueryObjects <$> v .:  "objects"



queryObjects :: Session a -> RemoteObjectId -> Maybe String -> IO (Either Error QueryObjects)
queryObjects session queryObjectsPrototypeObjectId queryObjectsObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","queryObjects") ([("prototypeObjectId", ToJSONEx queryObjectsPrototypeObjectId)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) queryObjectsObjectGroup]))


releaseObject :: Session a -> RemoteObjectId -> IO (Maybe Error)
releaseObject session releaseObjectObjectId = sendReceiveCommand (conn session) ("Runtime","releaseObject") ([("objectId", ToJSONEx releaseObjectObjectId)] ++ (catMaybes []))


releaseObjectGroup :: Session a -> String -> IO (Maybe Error)
releaseObjectGroup session releaseObjectGroupObjectGroup = sendReceiveCommand (conn session) ("Runtime","releaseObjectGroup") ([("objectGroup", ToJSONEx releaseObjectGroupObjectGroup)] ++ (catMaybes []))


runIfWaitingForDebugger :: Session a -> IO (Maybe Error)
runIfWaitingForDebugger session  = sendReceiveCommand (conn session) ("Runtime","runIfWaitingForDebugger") ([] ++ (catMaybes []))

data RunScript = RunScript {
    runScriptResult :: RemoteObject,
    runScriptExceptionDetails :: Maybe ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RunScript where
    parseJSON = A.withObject "RunScript" $ \v ->
         RunScript <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runScript :: Session a -> ScriptId -> Maybe ExecutionContextId -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error RunScript)
runScript session runScriptScriptId runScriptExecutionContextId runScriptObjectGroup runScriptSilent runScriptIncludeCommandLineApi runScriptReturnByValue runScriptGeneratePreview runScriptAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","runScript") ([("scriptId", ToJSONEx runScriptScriptId)] ++ (catMaybes [fmap (("executionContextId",) . ToJSONEx) runScriptExecutionContextId, fmap (("objectGroup",) . ToJSONEx) runScriptObjectGroup, fmap (("silent",) . ToJSONEx) runScriptSilent, fmap (("includeCommandLineAPI",) . ToJSONEx) runScriptIncludeCommandLineApi, fmap (("returnByValue",) . ToJSONEx) runScriptReturnByValue, fmap (("generatePreview",) . ToJSONEx) runScriptGeneratePreview, fmap (("awaitPromise",) . ToJSONEx) runScriptAwaitPromise]))


setAsyncCallStackDepth :: Session a -> Int -> IO (Maybe Error)
setAsyncCallStackDepth session setAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Runtime","setAsyncCallStackDepth") ([("maxDepth", ToJSONEx setAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))


