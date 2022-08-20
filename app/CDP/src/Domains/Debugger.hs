{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Debugger (module Domains.Debugger) where
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

import qualified Domains.Runtime as Runtime


import Utils

data BreakpointResolved = BreakpointResolved {
    breakpointResolvedBreakpointId :: BreakpointId,
    breakpointResolvedLocation :: Location
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  BreakpointResolved where
    parseJSON = A.withObject "BreakpointResolved" $ \v ->
         BreakpointResolved <$> v .:  "breakpointId"
            <*> v  .:  "location"


instance ToJSON BreakpointResolved  where
    toJSON v = A.object
        [ "breakpointId" .= breakpointResolvedBreakpointId v
        , "location" .= breakpointResolvedLocation v
        ]


data Paused = Paused {
    pausedCallFrames :: [CallFrame],
    pausedReason :: String,
    pausedData :: Maybe [(String, String)],
    pausedHitBreakpoints :: Maybe [String],
    pausedAsyncStackTrace :: Maybe Runtime.StackTrace
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Paused where
    parseJSON = A.withObject "Paused" $ \v ->
         Paused <$> v .:  "callFrames"
            <*> v  .:  "reason"
            <*> v  .:?  "data"
            <*> v  .:?  "hitBreakpoints"
            <*> v  .:?  "asyncStackTrace"


instance ToJSON Paused  where
    toJSON v = A.object
        [ "callFrames" .= pausedCallFrames v
        , "reason" .= pausedReason v
        , "data" .= pausedData v
        , "hitBreakpoints" .= pausedHitBreakpoints v
        , "asyncStackTrace" .= pausedAsyncStackTrace v
        ]


data Resumed = Resumed
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON Resumed where
    parseJSON = A.withText  "Resumed"  $ \v -> do
        pure $ case v of
                "Resumed" -> Resumed
                _ -> error "failed to parse Resumed"

data ScriptFailedToParse = ScriptFailedToParse {
    scriptFailedToParseScriptId :: Runtime.ScriptId,
    scriptFailedToParseUrl :: String,
    scriptFailedToParseStartLine :: Int,
    scriptFailedToParseStartColumn :: Int,
    scriptFailedToParseEndLine :: Int,
    scriptFailedToParseEndColumn :: Int,
    scriptFailedToParseExecutionContextId :: Runtime.ExecutionContextId,
    scriptFailedToParseHash :: String,
    scriptFailedToParseExecutionContextAuxData :: Maybe [(String, String)],
    scriptFailedToParseSourceMapUrl :: Maybe String,
    scriptFailedToParseHasSourceUrl :: Maybe Bool,
    scriptFailedToParseIsModule :: Maybe Bool,
    scriptFailedToParseLength :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ScriptFailedToParse where
    parseJSON = A.withObject "ScriptFailedToParse" $ \v ->
         ScriptFailedToParse <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "startLine"
            <*> v  .:  "startColumn"
            <*> v  .:  "endLine"
            <*> v  .:  "endColumn"
            <*> v  .:  "executionContextId"
            <*> v  .:  "hash"
            <*> v  .:?  "executionContextAuxData"
            <*> v  .:?  "sourceMapURL"
            <*> v  .:?  "hasSourceURL"
            <*> v  .:?  "isModule"
            <*> v  .:?  "length"


instance ToJSON ScriptFailedToParse  where
    toJSON v = A.object
        [ "scriptId" .= scriptFailedToParseScriptId v
        , "url" .= scriptFailedToParseUrl v
        , "startLine" .= scriptFailedToParseStartLine v
        , "startColumn" .= scriptFailedToParseStartColumn v
        , "endLine" .= scriptFailedToParseEndLine v
        , "endColumn" .= scriptFailedToParseEndColumn v
        , "executionContextId" .= scriptFailedToParseExecutionContextId v
        , "hash" .= scriptFailedToParseHash v
        , "executionContextAuxData" .= scriptFailedToParseExecutionContextAuxData v
        , "sourceMapURL" .= scriptFailedToParseSourceMapUrl v
        , "hasSourceURL" .= scriptFailedToParseHasSourceUrl v
        , "isModule" .= scriptFailedToParseIsModule v
        , "length" .= scriptFailedToParseLength v
        ]


data ScriptParsed = ScriptParsed {
    scriptParsedScriptId :: Runtime.ScriptId,
    scriptParsedUrl :: String,
    scriptParsedStartLine :: Int,
    scriptParsedStartColumn :: Int,
    scriptParsedEndLine :: Int,
    scriptParsedEndColumn :: Int,
    scriptParsedExecutionContextId :: Runtime.ExecutionContextId,
    scriptParsedHash :: String,
    scriptParsedExecutionContextAuxData :: Maybe [(String, String)],
    scriptParsedSourceMapUrl :: Maybe String,
    scriptParsedHasSourceUrl :: Maybe Bool,
    scriptParsedIsModule :: Maybe Bool,
    scriptParsedLength :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ScriptParsed where
    parseJSON = A.withObject "ScriptParsed" $ \v ->
         ScriptParsed <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "startLine"
            <*> v  .:  "startColumn"
            <*> v  .:  "endLine"
            <*> v  .:  "endColumn"
            <*> v  .:  "executionContextId"
            <*> v  .:  "hash"
            <*> v  .:?  "executionContextAuxData"
            <*> v  .:?  "sourceMapURL"
            <*> v  .:?  "hasSourceURL"
            <*> v  .:?  "isModule"
            <*> v  .:?  "length"


instance ToJSON ScriptParsed  where
    toJSON v = A.object
        [ "scriptId" .= scriptParsedScriptId v
        , "url" .= scriptParsedUrl v
        , "startLine" .= scriptParsedStartLine v
        , "startColumn" .= scriptParsedStartColumn v
        , "endLine" .= scriptParsedEndLine v
        , "endColumn" .= scriptParsedEndColumn v
        , "executionContextId" .= scriptParsedExecutionContextId v
        , "hash" .= scriptParsedHash v
        , "executionContextAuxData" .= scriptParsedExecutionContextAuxData v
        , "sourceMapURL" .= scriptParsedSourceMapUrl v
        , "hasSourceURL" .= scriptParsedHasSourceUrl v
        , "isModule" .= scriptParsedIsModule v
        , "length" .= scriptParsedLength v
        ]



type BreakpointId = String

type CallFrameId = String

data Location = Location {
    locationScriptId :: Runtime.ScriptId,
    locationLineNumber :: Int,
    locationColumnNumber :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Location where
    parseJSON = A.withObject "Location" $ \v ->
         Location <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"


instance ToJSON Location  where
    toJSON v = A.object
        [ "scriptId" .= locationScriptId v
        , "lineNumber" .= locationLineNumber v
        , "columnNumber" .= locationColumnNumber v
        ]



data CallFrame = CallFrame {
    callFrameCallFrameId :: CallFrameId,
    callFrameFunctionName :: String,
    callFrameLocation :: Location,
    callFrameScopeChain :: [Scope],
    callFrameThis :: Runtime.RemoteObject,
    callFrameFunctionLocation :: Maybe Location,
    callFrameReturnValue :: Maybe Runtime.RemoteObject
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CallFrame where
    parseJSON = A.withObject "CallFrame" $ \v ->
         CallFrame <$> v .:  "callFrameId"
            <*> v  .:  "functionName"
            <*> v  .:  "location"
            <*> v  .:  "scopeChain"
            <*> v  .:  "this"
            <*> v  .:?  "functionLocation"
            <*> v  .:?  "returnValue"


instance ToJSON CallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= callFrameCallFrameId v
        , "functionName" .= callFrameFunctionName v
        , "location" .= callFrameLocation v
        , "scopeChain" .= callFrameScopeChain v
        , "this" .= callFrameThis v
        , "functionLocation" .= callFrameFunctionLocation v
        , "returnValue" .= callFrameReturnValue v
        ]



data Scope = Scope {
    scopeType :: String,
    scopeObject :: Runtime.RemoteObject,
    scopeName :: Maybe String,
    scopeStartLocation :: Maybe Location,
    scopeEndLocation :: Maybe Location
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Scope where
    parseJSON = A.withObject "Scope" $ \v ->
         Scope <$> v .:  "type"
            <*> v  .:  "object"
            <*> v  .:?  "name"
            <*> v  .:?  "startLocation"
            <*> v  .:?  "endLocation"


instance ToJSON Scope  where
    toJSON v = A.object
        [ "type" .= scopeType v
        , "object" .= scopeObject v
        , "name" .= scopeName v
        , "startLocation" .= scopeStartLocation v
        , "endLocation" .= scopeEndLocation v
        ]



data SearchMatch = SearchMatch {
    searchMatchLineNumber :: Int,
    searchMatchLineContent :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SearchMatch where
    parseJSON = A.withObject "SearchMatch" $ \v ->
         SearchMatch <$> v .:  "lineNumber"
            <*> v  .:  "lineContent"


instance ToJSON SearchMatch  where
    toJSON v = A.object
        [ "lineNumber" .= searchMatchLineNumber v
        , "lineContent" .= searchMatchLineContent v
        ]



data BreakLocation = BreakLocation {
    breakLocationScriptId :: Runtime.ScriptId,
    breakLocationLineNumber :: Int,
    breakLocationColumnNumber :: Maybe Int,
    breakLocationType :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  BreakLocation where
    parseJSON = A.withObject "BreakLocation" $ \v ->
         BreakLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "type"


instance ToJSON BreakLocation  where
    toJSON v = A.object
        [ "scriptId" .= breakLocationScriptId v
        , "lineNumber" .= breakLocationLineNumber v
        , "columnNumber" .= breakLocationColumnNumber v
        , "type" .= breakLocationType v
        ]



data ScriptLanguage = ScriptLanguageJavaScript | ScriptLanguageWebAssembly
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ScriptLanguage where
    parseJSON = A.withText  "ScriptLanguage"  $ \v -> do
        pure $ case v of
                "JavaScript" -> ScriptLanguageJavaScript
                "WebAssembly" -> ScriptLanguageWebAssembly
                _ -> error "failed to parse ScriptLanguage"

instance ToJSON ScriptLanguage where
    toJSON v = A.String $
        case v of
                ScriptLanguageJavaScript -> "JavaScript"
                ScriptLanguageWebAssembly -> "WebAssembly"



data DebugSymbols = DebugSymbols {
    debugSymbolsType :: String,
    debugSymbolsExternalUrl :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  DebugSymbols where
    parseJSON = A.withObject "DebugSymbols" $ \v ->
         DebugSymbols <$> v .:  "type"
            <*> v  .:?  "externalURL"


instance ToJSON DebugSymbols  where
    toJSON v = A.object
        [ "type" .= debugSymbolsType v
        , "externalURL" .= debugSymbolsExternalUrl v
        ]



continueToLocation :: Session a -> Location -> Maybe String -> IO (Maybe Error)
continueToLocation session continueToLocationLocation continueToLocationTargetCallFrames = sendReceiveCommand (conn session) ("Debugger","continueToLocation") ([("location", ToJSONEx continueToLocationLocation)] ++ (catMaybes [fmap (("targetCallFrames",) . ToJSONEx) continueToLocationTargetCallFrames]))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Debugger","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Debugger","enable") ([] ++ (catMaybes []))

data EvaluateOnCallFrame = EvaluateOnCallFrame {
    evaluateOnCallFrameResult :: Runtime.RemoteObject,
    evaluateOnCallFrameExceptionDetails :: Maybe Runtime.ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  EvaluateOnCallFrame where
    parseJSON = A.withObject "EvaluateOnCallFrame" $ \v ->
         EvaluateOnCallFrame <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



evaluateOnCallFrame :: Session a -> CallFrameId -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error EvaluateOnCallFrame)
evaluateOnCallFrame session evaluateOnCallFrameCallFrameId evaluateOnCallFrameExpression evaluateOnCallFrameObjectGroup evaluateOnCallFrameIncludeCommandLineApi evaluateOnCallFrameSilent evaluateOnCallFrameReturnByValue evaluateOnCallFrameThrowOnSideEffect = sendReceiveCommandResult (conn session) ("Debugger","evaluateOnCallFrame") ([("callFrameId", ToJSONEx evaluateOnCallFrameCallFrameId), ("expression", ToJSONEx evaluateOnCallFrameExpression)] ++ (catMaybes [fmap (("objectGroup",) . ToJSONEx) evaluateOnCallFrameObjectGroup, fmap (("includeCommandLineAPI",) . ToJSONEx) evaluateOnCallFrameIncludeCommandLineApi, fmap (("silent",) . ToJSONEx) evaluateOnCallFrameSilent, fmap (("returnByValue",) . ToJSONEx) evaluateOnCallFrameReturnByValue, fmap (("throwOnSideEffect",) . ToJSONEx) evaluateOnCallFrameThrowOnSideEffect]))

data GetPossibleBreakpoints = GetPossibleBreakpoints {
    getPossibleBreakpointsLocations :: [BreakLocation]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetPossibleBreakpoints where
    parseJSON = A.withObject "GetPossibleBreakpoints" $ \v ->
         GetPossibleBreakpoints <$> v .:  "locations"



getPossibleBreakpoints :: Session a -> Location -> Maybe Location -> Maybe Bool -> IO (Either Error GetPossibleBreakpoints)
getPossibleBreakpoints session getPossibleBreakpointsStart getPossibleBreakpointsEnd getPossibleBreakpointsRestrictToFunction = sendReceiveCommandResult (conn session) ("Debugger","getPossibleBreakpoints") ([("start", ToJSONEx getPossibleBreakpointsStart)] ++ (catMaybes [fmap (("end",) . ToJSONEx) getPossibleBreakpointsEnd, fmap (("restrictToFunction",) . ToJSONEx) getPossibleBreakpointsRestrictToFunction]))

data GetScriptSource = GetScriptSource {
    getScriptSourceScriptSource :: String,
    getScriptSourceBytecode :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetScriptSource where
    parseJSON = A.withObject "GetScriptSource" $ \v ->
         GetScriptSource <$> v .:  "scriptSource"
            <*> v  .:?  "bytecode"



getScriptSource :: Session a -> Runtime.ScriptId -> IO (Either Error GetScriptSource)
getScriptSource session getScriptSourceScriptId = sendReceiveCommandResult (conn session) ("Debugger","getScriptSource") ([("scriptId", ToJSONEx getScriptSourceScriptId)] ++ (catMaybes []))


pause :: Session a -> IO (Maybe Error)
pause session  = sendReceiveCommand (conn session) ("Debugger","pause") ([] ++ (catMaybes []))


removeBreakpoint :: Session a -> BreakpointId -> IO (Maybe Error)
removeBreakpoint session removeBreakpointBreakpointId = sendReceiveCommand (conn session) ("Debugger","removeBreakpoint") ([("breakpointId", ToJSONEx removeBreakpointBreakpointId)] ++ (catMaybes []))


resume :: Session a -> Maybe Bool -> IO (Maybe Error)
resume session resumeTerminateOnResume = sendReceiveCommand (conn session) ("Debugger","resume") ([] ++ (catMaybes [fmap (("terminateOnResume",) . ToJSONEx) resumeTerminateOnResume]))

data SearchInContent = SearchInContent {
    searchInContentResult :: [SearchMatch]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SearchInContent where
    parseJSON = A.withObject "SearchInContent" $ \v ->
         SearchInContent <$> v .:  "result"



searchInContent :: Session a -> Runtime.ScriptId -> String -> Maybe Bool -> Maybe Bool -> IO (Either Error SearchInContent)
searchInContent session searchInContentScriptId searchInContentQuery searchInContentCaseSensitive searchInContentIsRegex = sendReceiveCommandResult (conn session) ("Debugger","searchInContent") ([("scriptId", ToJSONEx searchInContentScriptId), ("query", ToJSONEx searchInContentQuery)] ++ (catMaybes [fmap (("caseSensitive",) . ToJSONEx) searchInContentCaseSensitive, fmap (("isRegex",) . ToJSONEx) searchInContentIsRegex]))


setAsyncCallStackDepth :: Session a -> Int -> IO (Maybe Error)
setAsyncCallStackDepth session setAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Debugger","setAsyncCallStackDepth") ([("maxDepth", ToJSONEx setAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))

data SetBreakpoint = SetBreakpoint {
    setBreakpointBreakpointId :: BreakpointId,
    setBreakpointActualLocation :: Location
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetBreakpoint where
    parseJSON = A.withObject "SetBreakpoint" $ \v ->
         SetBreakpoint <$> v .:  "breakpointId"
            <*> v  .:  "actualLocation"



setBreakpoint :: Session a -> Location -> Maybe String -> IO (Either Error SetBreakpoint)
setBreakpoint session setBreakpointLocation setBreakpointCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpoint") ([("location", ToJSONEx setBreakpointLocation)] ++ (catMaybes [fmap (("condition",) . ToJSONEx) setBreakpointCondition]))

data SetInstrumentationBreakpoint = SetInstrumentationBreakpoint {
    setInstrumentationBreakpointBreakpointId :: BreakpointId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetInstrumentationBreakpoint where
    parseJSON = A.withObject "SetInstrumentationBreakpoint" $ \v ->
         SetInstrumentationBreakpoint <$> v .:  "breakpointId"



setInstrumentationBreakpoint :: Session a -> String -> IO (Either Error SetInstrumentationBreakpoint)
setInstrumentationBreakpoint session setInstrumentationBreakpointInstrumentation = sendReceiveCommandResult (conn session) ("Debugger","setInstrumentationBreakpoint") ([("instrumentation", ToJSONEx setInstrumentationBreakpointInstrumentation)] ++ (catMaybes []))

data SetBreakpointByUrl = SetBreakpointByUrl {
    setBreakpointByUrlBreakpointId :: BreakpointId,
    setBreakpointByUrlLocations :: [Location]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetBreakpointByUrl where
    parseJSON = A.withObject "SetBreakpointByUrl" $ \v ->
         SetBreakpointByUrl <$> v .:  "breakpointId"
            <*> v  .:  "locations"



setBreakpointByUrl :: Session a -> Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe String -> IO (Either Error SetBreakpointByUrl)
setBreakpointByUrl session setBreakpointByUrlLineNumber setBreakpointByUrlUrl setBreakpointByUrlUrlRegex setBreakpointByUrlScriptHash setBreakpointByUrlColumnNumber setBreakpointByUrlCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpointByUrl") ([("lineNumber", ToJSONEx setBreakpointByUrlLineNumber)] ++ (catMaybes [fmap (("url",) . ToJSONEx) setBreakpointByUrlUrl, fmap (("urlRegex",) . ToJSONEx) setBreakpointByUrlUrlRegex, fmap (("scriptHash",) . ToJSONEx) setBreakpointByUrlScriptHash, fmap (("columnNumber",) . ToJSONEx) setBreakpointByUrlColumnNumber, fmap (("condition",) . ToJSONEx) setBreakpointByUrlCondition]))


setBreakpointsActive :: Session a -> Bool -> IO (Maybe Error)
setBreakpointsActive session setBreakpointsActiveActive = sendReceiveCommand (conn session) ("Debugger","setBreakpointsActive") ([("active", ToJSONEx setBreakpointsActiveActive)] ++ (catMaybes []))


setPauseOnExceptions :: Session a -> String -> IO (Maybe Error)
setPauseOnExceptions session setPauseOnExceptionsState = sendReceiveCommand (conn session) ("Debugger","setPauseOnExceptions") ([("state", ToJSONEx setPauseOnExceptionsState)] ++ (catMaybes []))

data SetScriptSource = SetScriptSource {
    setScriptSourceCallFrames :: Maybe [CallFrame],
    setScriptSourceStackChanged :: Maybe Bool,
    setScriptSourceAsyncStackTrace :: Maybe Runtime.StackTrace,
    setScriptSourceExceptionDetails :: Maybe Runtime.ExceptionDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetScriptSource where
    parseJSON = A.withObject "SetScriptSource" $ \v ->
         SetScriptSource <$> v .:?  "callFrames"
            <*> v  .:?  "stackChanged"
            <*> v  .:?  "asyncStackTrace"
            <*> v  .:?  "exceptionDetails"



setScriptSource :: Session a -> Runtime.ScriptId -> String -> Maybe Bool -> IO (Either Error SetScriptSource)
setScriptSource session setScriptSourceScriptId setScriptSourceScriptSource setScriptSourceDryRun = sendReceiveCommandResult (conn session) ("Debugger","setScriptSource") ([("scriptId", ToJSONEx setScriptSourceScriptId), ("scriptSource", ToJSONEx setScriptSourceScriptSource)] ++ (catMaybes [fmap (("dryRun",) . ToJSONEx) setScriptSourceDryRun]))


setSkipAllPauses :: Session a -> Bool -> IO (Maybe Error)
setSkipAllPauses session setSkipAllPausesSkip = sendReceiveCommand (conn session) ("Debugger","setSkipAllPauses") ([("skip", ToJSONEx setSkipAllPausesSkip)] ++ (catMaybes []))


setVariableValue :: Session a -> Int -> String -> Runtime.CallArgument -> CallFrameId -> IO (Maybe Error)
setVariableValue session setVariableValueScopeNumber setVariableValueVariableName setVariableValueNewValue setVariableValueCallFrameId = sendReceiveCommand (conn session) ("Debugger","setVariableValue") ([("scopeNumber", ToJSONEx setVariableValueScopeNumber), ("variableName", ToJSONEx setVariableValueVariableName), ("newValue", ToJSONEx setVariableValueNewValue), ("callFrameId", ToJSONEx setVariableValueCallFrameId)] ++ (catMaybes []))


stepInto :: Session a -> IO (Maybe Error)
stepInto session  = sendReceiveCommand (conn session) ("Debugger","stepInto") ([] ++ (catMaybes []))


stepOut :: Session a -> IO (Maybe Error)
stepOut session  = sendReceiveCommand (conn session) ("Debugger","stepOut") ([] ++ (catMaybes []))


stepOver :: Session a -> IO (Maybe Error)
stepOver session  = sendReceiveCommand (conn session) ("Debugger","stepOver") ([] ++ (catMaybes []))


