{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Debugger

Debugger domain exposes JavaScript debugging capabilities. It allows setting and removing
breakpoints, stepping through execution, exploring stack traces, etc.
-}


module CDP.Domains.Debugger (module CDP.Domains.Debugger) where

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


import CDP.Domains.Runtime as Runtime


-- | Type 'Debugger.BreakpointId'.
--   Breakpoint identifier.
type DebuggerBreakpointId = T.Text

-- | Type 'Debugger.CallFrameId'.
--   Call frame identifier.
type DebuggerCallFrameId = T.Text

-- | Type 'Debugger.Location'.
--   Location in the source code.
data DebuggerLocation = DebuggerLocation
  {
    -- | Script identifier as reported in the `Debugger.scriptParsed`.
    debuggerLocationScriptId :: Runtime.RuntimeScriptId,
    -- | Line number in the script (0-based).
    debuggerLocationLineNumber :: Int,
    -- | Column number in the script (0-based).
    debuggerLocationColumnNumber :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON DebuggerLocation where
  parseJSON = A.withObject "DebuggerLocation" $ \o -> DebuggerLocation
    <$> o A..: "scriptId"
    <*> o A..: "lineNumber"
    <*> o A..:? "columnNumber"
instance ToJSON DebuggerLocation where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (debuggerLocationScriptId p),
    ("lineNumber" A..=) <$> Just (debuggerLocationLineNumber p),
    ("columnNumber" A..=) <$> (debuggerLocationColumnNumber p)
    ]

-- | Type 'Debugger.ScriptPosition'.
--   Location in the source code.
data DebuggerScriptPosition = DebuggerScriptPosition
  {
    debuggerScriptPositionLineNumber :: Int,
    debuggerScriptPositionColumnNumber :: Int
  }
  deriving (Eq, Show)
instance FromJSON DebuggerScriptPosition where
  parseJSON = A.withObject "DebuggerScriptPosition" $ \o -> DebuggerScriptPosition
    <$> o A..: "lineNumber"
    <*> o A..: "columnNumber"
instance ToJSON DebuggerScriptPosition where
  toJSON p = A.object $ catMaybes [
    ("lineNumber" A..=) <$> Just (debuggerScriptPositionLineNumber p),
    ("columnNumber" A..=) <$> Just (debuggerScriptPositionColumnNumber p)
    ]

-- | Type 'Debugger.LocationRange'.
--   Location range within one script.
data DebuggerLocationRange = DebuggerLocationRange
  {
    debuggerLocationRangeScriptId :: Runtime.RuntimeScriptId,
    debuggerLocationRangeStart :: DebuggerScriptPosition,
    debuggerLocationRangeEnd :: DebuggerScriptPosition
  }
  deriving (Eq, Show)
instance FromJSON DebuggerLocationRange where
  parseJSON = A.withObject "DebuggerLocationRange" $ \o -> DebuggerLocationRange
    <$> o A..: "scriptId"
    <*> o A..: "start"
    <*> o A..: "end"
instance ToJSON DebuggerLocationRange where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (debuggerLocationRangeScriptId p),
    ("start" A..=) <$> Just (debuggerLocationRangeStart p),
    ("end" A..=) <$> Just (debuggerLocationRangeEnd p)
    ]

-- | Type 'Debugger.CallFrame'.
--   JavaScript call frame. Array of call frames form the call stack.
data DebuggerCallFrame = DebuggerCallFrame
  {
    -- | Call frame identifier. This identifier is only valid while the virtual machine is paused.
    debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
    -- | Name of the JavaScript function called on this call frame.
    debuggerCallFrameFunctionName :: T.Text,
    -- | Location in the source code.
    debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
    -- | Location in the source code.
    debuggerCallFrameLocation :: DebuggerLocation,
    -- | Scope chain for this call frame.
    debuggerCallFrameScopeChain :: [DebuggerScope],
    -- | `this` object for this call frame.
    debuggerCallFrameThis :: Runtime.RuntimeRemoteObject,
    -- | The value being returned, if the function is at return point.
    debuggerCallFrameReturnValue :: Maybe Runtime.RuntimeRemoteObject,
    -- | Valid only while the VM is paused and indicates whether this frame
    --   can be restarted or not. Note that a `true` value here does not
    --   guarantee that Debugger#restartFrame with this CallFrameId will be
    --   successful, but it is very likely.
    debuggerCallFrameCanBeRestarted :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON DebuggerCallFrame where
  parseJSON = A.withObject "DebuggerCallFrame" $ \o -> DebuggerCallFrame
    <$> o A..: "callFrameId"
    <*> o A..: "functionName"
    <*> o A..:? "functionLocation"
    <*> o A..: "location"
    <*> o A..: "scopeChain"
    <*> o A..: "this"
    <*> o A..:? "returnValue"
    <*> o A..:? "canBeRestarted"
instance ToJSON DebuggerCallFrame where
  toJSON p = A.object $ catMaybes [
    ("callFrameId" A..=) <$> Just (debuggerCallFrameCallFrameId p),
    ("functionName" A..=) <$> Just (debuggerCallFrameFunctionName p),
    ("functionLocation" A..=) <$> (debuggerCallFrameFunctionLocation p),
    ("location" A..=) <$> Just (debuggerCallFrameLocation p),
    ("scopeChain" A..=) <$> Just (debuggerCallFrameScopeChain p),
    ("this" A..=) <$> Just (debuggerCallFrameThis p),
    ("returnValue" A..=) <$> (debuggerCallFrameReturnValue p),
    ("canBeRestarted" A..=) <$> (debuggerCallFrameCanBeRestarted p)
    ]

-- | Type 'Debugger.Scope'.
--   Scope description.
data DebuggerScopeType = DebuggerScopeTypeGlobal | DebuggerScopeTypeLocal | DebuggerScopeTypeWith | DebuggerScopeTypeClosure | DebuggerScopeTypeCatch | DebuggerScopeTypeBlock | DebuggerScopeTypeScript | DebuggerScopeTypeEval | DebuggerScopeTypeModule | DebuggerScopeTypeWasmExpressionStack
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerScopeType where
  parseJSON = A.withText "DebuggerScopeType" $ \v -> case v of
    "global" -> pure DebuggerScopeTypeGlobal
    "local" -> pure DebuggerScopeTypeLocal
    "with" -> pure DebuggerScopeTypeWith
    "closure" -> pure DebuggerScopeTypeClosure
    "catch" -> pure DebuggerScopeTypeCatch
    "block" -> pure DebuggerScopeTypeBlock
    "script" -> pure DebuggerScopeTypeScript
    "eval" -> pure DebuggerScopeTypeEval
    "module" -> pure DebuggerScopeTypeModule
    "wasm-expression-stack" -> pure DebuggerScopeTypeWasmExpressionStack
    "_" -> fail "failed to parse DebuggerScopeType"
instance ToJSON DebuggerScopeType where
  toJSON v = A.String $ case v of
    DebuggerScopeTypeGlobal -> "global"
    DebuggerScopeTypeLocal -> "local"
    DebuggerScopeTypeWith -> "with"
    DebuggerScopeTypeClosure -> "closure"
    DebuggerScopeTypeCatch -> "catch"
    DebuggerScopeTypeBlock -> "block"
    DebuggerScopeTypeScript -> "script"
    DebuggerScopeTypeEval -> "eval"
    DebuggerScopeTypeModule -> "module"
    DebuggerScopeTypeWasmExpressionStack -> "wasm-expression-stack"
data DebuggerScope = DebuggerScope
  {
    -- | Scope type.
    debuggerScopeType :: DebuggerScopeType,
    -- | Object representing the scope. For `global` and `with` scopes it represents the actual
    --   object; for the rest of the scopes, it is artificial transient object enumerating scope
    --   variables as its properties.
    debuggerScopeObject :: Runtime.RuntimeRemoteObject,
    debuggerScopeName :: Maybe T.Text,
    -- | Location in the source code where scope starts
    debuggerScopeStartLocation :: Maybe DebuggerLocation,
    -- | Location in the source code where scope ends
    debuggerScopeEndLocation :: Maybe DebuggerLocation
  }
  deriving (Eq, Show)
instance FromJSON DebuggerScope where
  parseJSON = A.withObject "DebuggerScope" $ \o -> DebuggerScope
    <$> o A..: "type"
    <*> o A..: "object"
    <*> o A..:? "name"
    <*> o A..:? "startLocation"
    <*> o A..:? "endLocation"
instance ToJSON DebuggerScope where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (debuggerScopeType p),
    ("object" A..=) <$> Just (debuggerScopeObject p),
    ("name" A..=) <$> (debuggerScopeName p),
    ("startLocation" A..=) <$> (debuggerScopeStartLocation p),
    ("endLocation" A..=) <$> (debuggerScopeEndLocation p)
    ]

-- | Type 'Debugger.SearchMatch'.
--   Search match for resource.
data DebuggerSearchMatch = DebuggerSearchMatch
  {
    -- | Line number in resource content.
    debuggerSearchMatchLineNumber :: Double,
    -- | Line with match content.
    debuggerSearchMatchLineContent :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSearchMatch where
  parseJSON = A.withObject "DebuggerSearchMatch" $ \o -> DebuggerSearchMatch
    <$> o A..: "lineNumber"
    <*> o A..: "lineContent"
instance ToJSON DebuggerSearchMatch where
  toJSON p = A.object $ catMaybes [
    ("lineNumber" A..=) <$> Just (debuggerSearchMatchLineNumber p),
    ("lineContent" A..=) <$> Just (debuggerSearchMatchLineContent p)
    ]

-- | Type 'Debugger.BreakLocation'.
data DebuggerBreakLocationType = DebuggerBreakLocationTypeDebuggerStatement | DebuggerBreakLocationTypeCall | DebuggerBreakLocationTypeReturn
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerBreakLocationType where
  parseJSON = A.withText "DebuggerBreakLocationType" $ \v -> case v of
    "debuggerStatement" -> pure DebuggerBreakLocationTypeDebuggerStatement
    "call" -> pure DebuggerBreakLocationTypeCall
    "return" -> pure DebuggerBreakLocationTypeReturn
    "_" -> fail "failed to parse DebuggerBreakLocationType"
instance ToJSON DebuggerBreakLocationType where
  toJSON v = A.String $ case v of
    DebuggerBreakLocationTypeDebuggerStatement -> "debuggerStatement"
    DebuggerBreakLocationTypeCall -> "call"
    DebuggerBreakLocationTypeReturn -> "return"
data DebuggerBreakLocation = DebuggerBreakLocation
  {
    -- | Script identifier as reported in the `Debugger.scriptParsed`.
    debuggerBreakLocationScriptId :: Runtime.RuntimeScriptId,
    -- | Line number in the script (0-based).
    debuggerBreakLocationLineNumber :: Int,
    -- | Column number in the script (0-based).
    debuggerBreakLocationColumnNumber :: Maybe Int,
    debuggerBreakLocationType :: Maybe DebuggerBreakLocationType
  }
  deriving (Eq, Show)
instance FromJSON DebuggerBreakLocation where
  parseJSON = A.withObject "DebuggerBreakLocation" $ \o -> DebuggerBreakLocation
    <$> o A..: "scriptId"
    <*> o A..: "lineNumber"
    <*> o A..:? "columnNumber"
    <*> o A..:? "type"
instance ToJSON DebuggerBreakLocation where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (debuggerBreakLocationScriptId p),
    ("lineNumber" A..=) <$> Just (debuggerBreakLocationLineNumber p),
    ("columnNumber" A..=) <$> (debuggerBreakLocationColumnNumber p),
    ("type" A..=) <$> (debuggerBreakLocationType p)
    ]

-- | Type 'Debugger.WasmDisassemblyChunk'.
data DebuggerWasmDisassemblyChunk = DebuggerWasmDisassemblyChunk
  {
    -- | The next chunk of disassembled lines.
    debuggerWasmDisassemblyChunkLines :: [T.Text],
    -- | The bytecode offsets describing the start of each line.
    debuggerWasmDisassemblyChunkBytecodeOffsets :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON DebuggerWasmDisassemblyChunk where
  parseJSON = A.withObject "DebuggerWasmDisassemblyChunk" $ \o -> DebuggerWasmDisassemblyChunk
    <$> o A..: "lines"
    <*> o A..: "bytecodeOffsets"
instance ToJSON DebuggerWasmDisassemblyChunk where
  toJSON p = A.object $ catMaybes [
    ("lines" A..=) <$> Just (debuggerWasmDisassemblyChunkLines p),
    ("bytecodeOffsets" A..=) <$> Just (debuggerWasmDisassemblyChunkBytecodeOffsets p)
    ]

-- | Type 'Debugger.ScriptLanguage'.
--   Enum of possible script languages.
data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerScriptLanguage where
  parseJSON = A.withText "DebuggerScriptLanguage" $ \v -> case v of
    "JavaScript" -> pure DebuggerScriptLanguageJavaScript
    "WebAssembly" -> pure DebuggerScriptLanguageWebAssembly
    "_" -> fail "failed to parse DebuggerScriptLanguage"
instance ToJSON DebuggerScriptLanguage where
  toJSON v = A.String $ case v of
    DebuggerScriptLanguageJavaScript -> "JavaScript"
    DebuggerScriptLanguageWebAssembly -> "WebAssembly"

-- | Type 'Debugger.DebugSymbols'.
--   Debug symbols available for a wasm script.
data DebuggerDebugSymbolsType = DebuggerDebugSymbolsTypeNone | DebuggerDebugSymbolsTypeSourceMap | DebuggerDebugSymbolsTypeEmbeddedDWARF | DebuggerDebugSymbolsTypeExternalDWARF
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerDebugSymbolsType where
  parseJSON = A.withText "DebuggerDebugSymbolsType" $ \v -> case v of
    "None" -> pure DebuggerDebugSymbolsTypeNone
    "SourceMap" -> pure DebuggerDebugSymbolsTypeSourceMap
    "EmbeddedDWARF" -> pure DebuggerDebugSymbolsTypeEmbeddedDWARF
    "ExternalDWARF" -> pure DebuggerDebugSymbolsTypeExternalDWARF
    "_" -> fail "failed to parse DebuggerDebugSymbolsType"
instance ToJSON DebuggerDebugSymbolsType where
  toJSON v = A.String $ case v of
    DebuggerDebugSymbolsTypeNone -> "None"
    DebuggerDebugSymbolsTypeSourceMap -> "SourceMap"
    DebuggerDebugSymbolsTypeEmbeddedDWARF -> "EmbeddedDWARF"
    DebuggerDebugSymbolsTypeExternalDWARF -> "ExternalDWARF"
data DebuggerDebugSymbols = DebuggerDebugSymbols
  {
    -- | Type of the debug symbols.
    debuggerDebugSymbolsType :: DebuggerDebugSymbolsType,
    -- | URL of the external symbol source.
    debuggerDebugSymbolsExternalURL :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON DebuggerDebugSymbols where
  parseJSON = A.withObject "DebuggerDebugSymbols" $ \o -> DebuggerDebugSymbols
    <$> o A..: "type"
    <*> o A..:? "externalURL"
instance ToJSON DebuggerDebugSymbols where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (debuggerDebugSymbolsType p),
    ("externalURL" A..=) <$> (debuggerDebugSymbolsExternalURL p)
    ]

-- | Type of the 'Debugger.breakpointResolved' event.
data DebuggerBreakpointResolved = DebuggerBreakpointResolved
  {
    -- | Breakpoint unique identifier.
    debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
    -- | Actual breakpoint location.
    debuggerBreakpointResolvedLocation :: DebuggerLocation
  }
  deriving (Eq, Show)
instance FromJSON DebuggerBreakpointResolved where
  parseJSON = A.withObject "DebuggerBreakpointResolved" $ \o -> DebuggerBreakpointResolved
    <$> o A..: "breakpointId"
    <*> o A..: "location"
instance Event DebuggerBreakpointResolved where
  eventName _ = "Debugger.breakpointResolved"

-- | Type of the 'Debugger.paused' event.
data DebuggerPausedReason = DebuggerPausedReasonAmbiguous | DebuggerPausedReasonAssert | DebuggerPausedReasonCSPViolation | DebuggerPausedReasonDebugCommand | DebuggerPausedReasonDOM | DebuggerPausedReasonEventListener | DebuggerPausedReasonException | DebuggerPausedReasonInstrumentation | DebuggerPausedReasonOOM | DebuggerPausedReasonOther | DebuggerPausedReasonPromiseRejection | DebuggerPausedReasonXHR
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerPausedReason where
  parseJSON = A.withText "DebuggerPausedReason" $ \v -> case v of
    "ambiguous" -> pure DebuggerPausedReasonAmbiguous
    "assert" -> pure DebuggerPausedReasonAssert
    "CSPViolation" -> pure DebuggerPausedReasonCSPViolation
    "debugCommand" -> pure DebuggerPausedReasonDebugCommand
    "DOM" -> pure DebuggerPausedReasonDOM
    "EventListener" -> pure DebuggerPausedReasonEventListener
    "exception" -> pure DebuggerPausedReasonException
    "instrumentation" -> pure DebuggerPausedReasonInstrumentation
    "OOM" -> pure DebuggerPausedReasonOOM
    "other" -> pure DebuggerPausedReasonOther
    "promiseRejection" -> pure DebuggerPausedReasonPromiseRejection
    "XHR" -> pure DebuggerPausedReasonXHR
    "_" -> fail "failed to parse DebuggerPausedReason"
instance ToJSON DebuggerPausedReason where
  toJSON v = A.String $ case v of
    DebuggerPausedReasonAmbiguous -> "ambiguous"
    DebuggerPausedReasonAssert -> "assert"
    DebuggerPausedReasonCSPViolation -> "CSPViolation"
    DebuggerPausedReasonDebugCommand -> "debugCommand"
    DebuggerPausedReasonDOM -> "DOM"
    DebuggerPausedReasonEventListener -> "EventListener"
    DebuggerPausedReasonException -> "exception"
    DebuggerPausedReasonInstrumentation -> "instrumentation"
    DebuggerPausedReasonOOM -> "OOM"
    DebuggerPausedReasonOther -> "other"
    DebuggerPausedReasonPromiseRejection -> "promiseRejection"
    DebuggerPausedReasonXHR -> "XHR"
data DebuggerPaused = DebuggerPaused
  {
    -- | Call stack the virtual machine stopped on.
    debuggerPausedCallFrames :: [DebuggerCallFrame],
    -- | Pause reason.
    debuggerPausedReason :: DebuggerPausedReason,
    -- | Object containing break-specific auxiliary properties.
    debuggerPausedData :: Maybe [(T.Text, T.Text)],
    -- | Hit breakpoints IDs
    debuggerPausedHitBreakpoints :: Maybe [T.Text],
    -- | Async stack trace, if any.
    debuggerPausedAsyncStackTrace :: Maybe Runtime.RuntimeStackTrace,
    -- | Async stack trace, if any.
    debuggerPausedAsyncStackTraceId :: Maybe Runtime.RuntimeStackTraceId
  }
  deriving (Eq, Show)
instance FromJSON DebuggerPaused where
  parseJSON = A.withObject "DebuggerPaused" $ \o -> DebuggerPaused
    <$> o A..: "callFrames"
    <*> o A..: "reason"
    <*> o A..:? "data"
    <*> o A..:? "hitBreakpoints"
    <*> o A..:? "asyncStackTrace"
    <*> o A..:? "asyncStackTraceId"
instance Event DebuggerPaused where
  eventName _ = "Debugger.paused"

-- | Type of the 'Debugger.resumed' event.
data DebuggerResumed = DebuggerResumed
  deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
  parseJSON _ = pure DebuggerResumed
instance Event DebuggerResumed where
  eventName _ = "Debugger.resumed"

-- | Type of the 'Debugger.scriptFailedToParse' event.
data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse
  {
    -- | Identifier of the script parsed.
    debuggerScriptFailedToParseScriptId :: Runtime.RuntimeScriptId,
    -- | URL or name of the script parsed (if any).
    debuggerScriptFailedToParseUrl :: T.Text,
    -- | Line offset of the script within the resource with given URL (for script tags).
    debuggerScriptFailedToParseStartLine :: Int,
    -- | Column offset of the script within the resource with given URL.
    debuggerScriptFailedToParseStartColumn :: Int,
    -- | Last line of the script.
    debuggerScriptFailedToParseEndLine :: Int,
    -- | Length of the last line of the script.
    debuggerScriptFailedToParseEndColumn :: Int,
    -- | Specifies script creation context.
    debuggerScriptFailedToParseExecutionContextId :: Runtime.RuntimeExecutionContextId,
    -- | Content hash of the script, SHA-256.
    debuggerScriptFailedToParseHash :: T.Text,
    -- | Embedder-specific auxiliary data.
    debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(T.Text, T.Text)],
    -- | URL of source map associated with script (if any).
    debuggerScriptFailedToParseSourceMapURL :: Maybe T.Text,
    -- | True, if this script has sourceURL.
    debuggerScriptFailedToParseHasSourceURL :: Maybe Bool,
    -- | True, if this script is ES6 module.
    debuggerScriptFailedToParseIsModule :: Maybe Bool,
    -- | This script length.
    debuggerScriptFailedToParseLength :: Maybe Int,
    -- | JavaScript top stack frame of where the script parsed event was triggered if available.
    debuggerScriptFailedToParseStackTrace :: Maybe Runtime.RuntimeStackTrace,
    -- | If the scriptLanguage is WebAssembly, the code section offset in the module.
    debuggerScriptFailedToParseCodeOffset :: Maybe Int,
    -- | The language of the script.
    debuggerScriptFailedToParseScriptLanguage :: Maybe DebuggerScriptLanguage,
    -- | The name the embedder supplied for this script.
    debuggerScriptFailedToParseEmbedderName :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON DebuggerScriptFailedToParse where
  parseJSON = A.withObject "DebuggerScriptFailedToParse" $ \o -> DebuggerScriptFailedToParse
    <$> o A..: "scriptId"
    <*> o A..: "url"
    <*> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
    <*> o A..: "executionContextId"
    <*> o A..: "hash"
    <*> o A..:? "executionContextAuxData"
    <*> o A..:? "sourceMapURL"
    <*> o A..:? "hasSourceURL"
    <*> o A..:? "isModule"
    <*> o A..:? "length"
    <*> o A..:? "stackTrace"
    <*> o A..:? "codeOffset"
    <*> o A..:? "scriptLanguage"
    <*> o A..:? "embedderName"
instance Event DebuggerScriptFailedToParse where
  eventName _ = "Debugger.scriptFailedToParse"

-- | Type of the 'Debugger.scriptParsed' event.
data DebuggerScriptParsed = DebuggerScriptParsed
  {
    -- | Identifier of the script parsed.
    debuggerScriptParsedScriptId :: Runtime.RuntimeScriptId,
    -- | URL or name of the script parsed (if any).
    debuggerScriptParsedUrl :: T.Text,
    -- | Line offset of the script within the resource with given URL (for script tags).
    debuggerScriptParsedStartLine :: Int,
    -- | Column offset of the script within the resource with given URL.
    debuggerScriptParsedStartColumn :: Int,
    -- | Last line of the script.
    debuggerScriptParsedEndLine :: Int,
    -- | Length of the last line of the script.
    debuggerScriptParsedEndColumn :: Int,
    -- | Specifies script creation context.
    debuggerScriptParsedExecutionContextId :: Runtime.RuntimeExecutionContextId,
    -- | Content hash of the script, SHA-256.
    debuggerScriptParsedHash :: T.Text,
    -- | Embedder-specific auxiliary data.
    debuggerScriptParsedExecutionContextAuxData :: Maybe [(T.Text, T.Text)],
    -- | True, if this script is generated as a result of the live edit operation.
    debuggerScriptParsedIsLiveEdit :: Maybe Bool,
    -- | URL of source map associated with script (if any).
    debuggerScriptParsedSourceMapURL :: Maybe T.Text,
    -- | True, if this script has sourceURL.
    debuggerScriptParsedHasSourceURL :: Maybe Bool,
    -- | True, if this script is ES6 module.
    debuggerScriptParsedIsModule :: Maybe Bool,
    -- | This script length.
    debuggerScriptParsedLength :: Maybe Int,
    -- | JavaScript top stack frame of where the script parsed event was triggered if available.
    debuggerScriptParsedStackTrace :: Maybe Runtime.RuntimeStackTrace,
    -- | If the scriptLanguage is WebAssembly, the code section offset in the module.
    debuggerScriptParsedCodeOffset :: Maybe Int,
    -- | The language of the script.
    debuggerScriptParsedScriptLanguage :: Maybe DebuggerScriptLanguage,
    -- | If the scriptLanguage is WebASsembly, the source of debug symbols for the module.
    debuggerScriptParsedDebugSymbols :: Maybe DebuggerDebugSymbols,
    -- | The name the embedder supplied for this script.
    debuggerScriptParsedEmbedderName :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON DebuggerScriptParsed where
  parseJSON = A.withObject "DebuggerScriptParsed" $ \o -> DebuggerScriptParsed
    <$> o A..: "scriptId"
    <*> o A..: "url"
    <*> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
    <*> o A..: "executionContextId"
    <*> o A..: "hash"
    <*> o A..:? "executionContextAuxData"
    <*> o A..:? "isLiveEdit"
    <*> o A..:? "sourceMapURL"
    <*> o A..:? "hasSourceURL"
    <*> o A..:? "isModule"
    <*> o A..:? "length"
    <*> o A..:? "stackTrace"
    <*> o A..:? "codeOffset"
    <*> o A..:? "scriptLanguage"
    <*> o A..:? "debugSymbols"
    <*> o A..:? "embedderName"
instance Event DebuggerScriptParsed where
  eventName _ = "Debugger.scriptParsed"

-- | Continues execution until specific location is reached.

-- | Parameters of the 'Debugger.continueToLocation' command.
data PDebuggerContinueToLocationTargetCallFrames = PDebuggerContinueToLocationTargetCallFramesAny | PDebuggerContinueToLocationTargetCallFramesCurrent
  deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerContinueToLocationTargetCallFrames where
  parseJSON = A.withText "PDebuggerContinueToLocationTargetCallFrames" $ \v -> case v of
    "any" -> pure PDebuggerContinueToLocationTargetCallFramesAny
    "current" -> pure PDebuggerContinueToLocationTargetCallFramesCurrent
    "_" -> fail "failed to parse PDebuggerContinueToLocationTargetCallFrames"
instance ToJSON PDebuggerContinueToLocationTargetCallFrames where
  toJSON v = A.String $ case v of
    PDebuggerContinueToLocationTargetCallFramesAny -> "any"
    PDebuggerContinueToLocationTargetCallFramesCurrent -> "current"
data PDebuggerContinueToLocation = PDebuggerContinueToLocation
  {
    -- | Location to continue to.
    pDebuggerContinueToLocationLocation :: DebuggerLocation,
    pDebuggerContinueToLocationTargetCallFrames :: Maybe PDebuggerContinueToLocationTargetCallFrames
  }
  deriving (Eq, Show)
pDebuggerContinueToLocation
  {-
  -- | Location to continue to.
  -}
  :: DebuggerLocation
  -> PDebuggerContinueToLocation
pDebuggerContinueToLocation
  arg_pDebuggerContinueToLocationLocation
  = PDebuggerContinueToLocation
    arg_pDebuggerContinueToLocationLocation
    Nothing
instance ToJSON PDebuggerContinueToLocation where
  toJSON p = A.object $ catMaybes [
    ("location" A..=) <$> Just (pDebuggerContinueToLocationLocation p),
    ("targetCallFrames" A..=) <$> (pDebuggerContinueToLocationTargetCallFrames p)
    ]
instance Command PDebuggerContinueToLocation where
  type CommandResponse PDebuggerContinueToLocation = ()
  commandName _ = "Debugger.continueToLocation"
  fromJSON = const . A.Success . const ()

-- | Disables debugger for given page.

-- | Parameters of the 'Debugger.disable' command.
data PDebuggerDisable = PDebuggerDisable
  deriving (Eq, Show)
pDebuggerDisable
  :: PDebuggerDisable
pDebuggerDisable
  = PDebuggerDisable
instance ToJSON PDebuggerDisable where
  toJSON _ = A.Null
instance Command PDebuggerDisable where
  type CommandResponse PDebuggerDisable = ()
  commandName _ = "Debugger.disable"
  fromJSON = const . A.Success . const ()

-- | Enables debugger for the given page. Clients should not assume that the debugging has been
--   enabled until the result for this command is received.

-- | Parameters of the 'Debugger.enable' command.
data PDebuggerEnable = PDebuggerEnable
  {
    -- | The maximum size in bytes of collected scripts (not referenced by other heap objects)
    --   the debugger can hold. Puts no limit if parameter is omitted.
    pDebuggerEnableMaxScriptsCacheSize :: Maybe Double
  }
  deriving (Eq, Show)
pDebuggerEnable
  :: PDebuggerEnable
pDebuggerEnable
  = PDebuggerEnable
    Nothing
instance ToJSON PDebuggerEnable where
  toJSON p = A.object $ catMaybes [
    ("maxScriptsCacheSize" A..=) <$> (pDebuggerEnableMaxScriptsCacheSize p)
    ]
data DebuggerEnable = DebuggerEnable
  {
    -- | Unique identifier of the debugger.
    debuggerEnableDebuggerId :: Runtime.RuntimeUniqueDebuggerId
  }
  deriving (Eq, Show)
instance FromJSON DebuggerEnable where
  parseJSON = A.withObject "DebuggerEnable" $ \o -> DebuggerEnable
    <$> o A..: "debuggerId"
instance Command PDebuggerEnable where
  type CommandResponse PDebuggerEnable = DebuggerEnable
  commandName _ = "Debugger.enable"

-- | Evaluates expression on a given call frame.

-- | Parameters of the 'Debugger.evaluateOnCallFrame' command.
data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame
  {
    -- | Call frame identifier to evaluate on.
    pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
    -- | Expression to evaluate.
    pDebuggerEvaluateOnCallFrameExpression :: T.Text,
    -- | String object group name to put result into (allows rapid releasing resulting object handles
    --   using `releaseObjectGroup`).
    pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe T.Text,
    -- | Specifies whether command line API should be available to the evaluated expression, defaults
    --   to false.
    pDebuggerEvaluateOnCallFrameIncludeCommandLineAPI :: Maybe Bool,
    -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
    --   execution. Overrides `setPauseOnException` state.
    pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
    -- | Whether the result is expected to be a JSON object that should be sent by value.
    pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
    -- | Whether preview should be generated for the result.
    pDebuggerEvaluateOnCallFrameGeneratePreview :: Maybe Bool,
    -- | Whether to throw an exception if side effect cannot be ruled out during evaluation.
    pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool,
    -- | Terminate execution after timing out (number of milliseconds).
    pDebuggerEvaluateOnCallFrameTimeout :: Maybe Runtime.RuntimeTimeDelta
  }
  deriving (Eq, Show)
pDebuggerEvaluateOnCallFrame
  {-
  -- | Call frame identifier to evaluate on.
  -}
  :: DebuggerCallFrameId
  {-
  -- | Expression to evaluate.
  -}
  -> T.Text
  -> PDebuggerEvaluateOnCallFrame
pDebuggerEvaluateOnCallFrame
  arg_pDebuggerEvaluateOnCallFrameCallFrameId
  arg_pDebuggerEvaluateOnCallFrameExpression
  = PDebuggerEvaluateOnCallFrame
    arg_pDebuggerEvaluateOnCallFrameCallFrameId
    arg_pDebuggerEvaluateOnCallFrameExpression
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDebuggerEvaluateOnCallFrame where
  toJSON p = A.object $ catMaybes [
    ("callFrameId" A..=) <$> Just (pDebuggerEvaluateOnCallFrameCallFrameId p),
    ("expression" A..=) <$> Just (pDebuggerEvaluateOnCallFrameExpression p),
    ("objectGroup" A..=) <$> (pDebuggerEvaluateOnCallFrameObjectGroup p),
    ("includeCommandLineAPI" A..=) <$> (pDebuggerEvaluateOnCallFrameIncludeCommandLineAPI p),
    ("silent" A..=) <$> (pDebuggerEvaluateOnCallFrameSilent p),
    ("returnByValue" A..=) <$> (pDebuggerEvaluateOnCallFrameReturnByValue p),
    ("generatePreview" A..=) <$> (pDebuggerEvaluateOnCallFrameGeneratePreview p),
    ("throwOnSideEffect" A..=) <$> (pDebuggerEvaluateOnCallFrameThrowOnSideEffect p),
    ("timeout" A..=) <$> (pDebuggerEvaluateOnCallFrameTimeout p)
    ]
data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame
  {
    -- | Object wrapper for the evaluation result.
    debuggerEvaluateOnCallFrameResult :: Runtime.RuntimeRemoteObject,
    -- | Exception details.
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON DebuggerEvaluateOnCallFrame where
  parseJSON = A.withObject "DebuggerEvaluateOnCallFrame" $ \o -> DebuggerEvaluateOnCallFrame
    <$> o A..: "result"
    <*> o A..:? "exceptionDetails"
instance Command PDebuggerEvaluateOnCallFrame where
  type CommandResponse PDebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame
  commandName _ = "Debugger.evaluateOnCallFrame"

-- | Returns possible locations for breakpoint. scriptId in start and end range locations should be
--   the same.

-- | Parameters of the 'Debugger.getPossibleBreakpoints' command.
data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints
  {
    -- | Start of range to search possible breakpoint locations in.
    pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
    -- | End of range to search possible breakpoint locations in (excluding). When not specified, end
    --   of scripts is used as end of range.
    pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
    -- | Only consider locations which are in the same (non-nested) function as start.
    pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
  }
  deriving (Eq, Show)
pDebuggerGetPossibleBreakpoints
  {-
  -- | Start of range to search possible breakpoint locations in.
  -}
  :: DebuggerLocation
  -> PDebuggerGetPossibleBreakpoints
pDebuggerGetPossibleBreakpoints
  arg_pDebuggerGetPossibleBreakpointsStart
  = PDebuggerGetPossibleBreakpoints
    arg_pDebuggerGetPossibleBreakpointsStart
    Nothing
    Nothing
instance ToJSON PDebuggerGetPossibleBreakpoints where
  toJSON p = A.object $ catMaybes [
    ("start" A..=) <$> Just (pDebuggerGetPossibleBreakpointsStart p),
    ("end" A..=) <$> (pDebuggerGetPossibleBreakpointsEnd p),
    ("restrictToFunction" A..=) <$> (pDebuggerGetPossibleBreakpointsRestrictToFunction p)
    ]
data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints
  {
    -- | List of the possible breakpoint locations.
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
  }
  deriving (Eq, Show)
instance FromJSON DebuggerGetPossibleBreakpoints where
  parseJSON = A.withObject "DebuggerGetPossibleBreakpoints" $ \o -> DebuggerGetPossibleBreakpoints
    <$> o A..: "locations"
instance Command PDebuggerGetPossibleBreakpoints where
  type CommandResponse PDebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints
  commandName _ = "Debugger.getPossibleBreakpoints"

-- | Returns source for the script with given id.

-- | Parameters of the 'Debugger.getScriptSource' command.
data PDebuggerGetScriptSource = PDebuggerGetScriptSource
  {
    -- | Id of the script to get source for.
    pDebuggerGetScriptSourceScriptId :: Runtime.RuntimeScriptId
  }
  deriving (Eq, Show)
pDebuggerGetScriptSource
  {-
  -- | Id of the script to get source for.
  -}
  :: Runtime.RuntimeScriptId
  -> PDebuggerGetScriptSource
pDebuggerGetScriptSource
  arg_pDebuggerGetScriptSourceScriptId
  = PDebuggerGetScriptSource
    arg_pDebuggerGetScriptSourceScriptId
instance ToJSON PDebuggerGetScriptSource where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pDebuggerGetScriptSourceScriptId p)
    ]
data DebuggerGetScriptSource = DebuggerGetScriptSource
  {
    -- | Script source (empty in case of Wasm bytecode).
    debuggerGetScriptSourceScriptSource :: T.Text,
    -- | Wasm bytecode. (Encoded as a base64 string when passed over JSON)
    debuggerGetScriptSourceBytecode :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON DebuggerGetScriptSource where
  parseJSON = A.withObject "DebuggerGetScriptSource" $ \o -> DebuggerGetScriptSource
    <$> o A..: "scriptSource"
    <*> o A..:? "bytecode"
instance Command PDebuggerGetScriptSource where
  type CommandResponse PDebuggerGetScriptSource = DebuggerGetScriptSource
  commandName _ = "Debugger.getScriptSource"


-- | Parameters of the 'Debugger.disassembleWasmModule' command.
data PDebuggerDisassembleWasmModule = PDebuggerDisassembleWasmModule
  {
    -- | Id of the script to disassemble
    pDebuggerDisassembleWasmModuleScriptId :: Runtime.RuntimeScriptId
  }
  deriving (Eq, Show)
pDebuggerDisassembleWasmModule
  {-
  -- | Id of the script to disassemble
  -}
  :: Runtime.RuntimeScriptId
  -> PDebuggerDisassembleWasmModule
pDebuggerDisassembleWasmModule
  arg_pDebuggerDisassembleWasmModuleScriptId
  = PDebuggerDisassembleWasmModule
    arg_pDebuggerDisassembleWasmModuleScriptId
instance ToJSON PDebuggerDisassembleWasmModule where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pDebuggerDisassembleWasmModuleScriptId p)
    ]
data DebuggerDisassembleWasmModule = DebuggerDisassembleWasmModule
  {
    -- | For large modules, return a stream from which additional chunks of
    --   disassembly can be read successively.
    debuggerDisassembleWasmModuleStreamId :: Maybe T.Text,
    -- | The total number of lines in the disassembly text.
    debuggerDisassembleWasmModuleTotalNumberOfLines :: Int,
    -- | The offsets of all function bodies, in the format [start1, end1,
    --   start2, end2, ...] where all ends are exclusive.
    debuggerDisassembleWasmModuleFunctionBodyOffsets :: [Int],
    -- | The first chunk of disassembly.
    debuggerDisassembleWasmModuleChunk :: DebuggerWasmDisassemblyChunk
  }
  deriving (Eq, Show)
instance FromJSON DebuggerDisassembleWasmModule where
  parseJSON = A.withObject "DebuggerDisassembleWasmModule" $ \o -> DebuggerDisassembleWasmModule
    <$> o A..:? "streamId"
    <*> o A..: "totalNumberOfLines"
    <*> o A..: "functionBodyOffsets"
    <*> o A..: "chunk"
instance Command PDebuggerDisassembleWasmModule where
  type CommandResponse PDebuggerDisassembleWasmModule = DebuggerDisassembleWasmModule
  commandName _ = "Debugger.disassembleWasmModule"

-- | Disassemble the next chunk of lines for the module corresponding to the
--   stream. If disassembly is complete, this API will invalidate the streamId
--   and return an empty chunk. Any subsequent calls for the now invalid stream
--   will return errors.

-- | Parameters of the 'Debugger.nextWasmDisassemblyChunk' command.
data PDebuggerNextWasmDisassemblyChunk = PDebuggerNextWasmDisassemblyChunk
  {
    pDebuggerNextWasmDisassemblyChunkStreamId :: T.Text
  }
  deriving (Eq, Show)
pDebuggerNextWasmDisassemblyChunk
  :: T.Text
  -> PDebuggerNextWasmDisassemblyChunk
pDebuggerNextWasmDisassemblyChunk
  arg_pDebuggerNextWasmDisassemblyChunkStreamId
  = PDebuggerNextWasmDisassemblyChunk
    arg_pDebuggerNextWasmDisassemblyChunkStreamId
instance ToJSON PDebuggerNextWasmDisassemblyChunk where
  toJSON p = A.object $ catMaybes [
    ("streamId" A..=) <$> Just (pDebuggerNextWasmDisassemblyChunkStreamId p)
    ]
data DebuggerNextWasmDisassemblyChunk = DebuggerNextWasmDisassemblyChunk
  {
    -- | The next chunk of disassembly.
    debuggerNextWasmDisassemblyChunkChunk :: DebuggerWasmDisassemblyChunk
  }
  deriving (Eq, Show)
instance FromJSON DebuggerNextWasmDisassemblyChunk where
  parseJSON = A.withObject "DebuggerNextWasmDisassemblyChunk" $ \o -> DebuggerNextWasmDisassemblyChunk
    <$> o A..: "chunk"
instance Command PDebuggerNextWasmDisassemblyChunk where
  type CommandResponse PDebuggerNextWasmDisassemblyChunk = DebuggerNextWasmDisassemblyChunk
  commandName _ = "Debugger.nextWasmDisassemblyChunk"

-- | Returns stack trace with given `stackTraceId`.

-- | Parameters of the 'Debugger.getStackTrace' command.
data PDebuggerGetStackTrace = PDebuggerGetStackTrace
  {
    pDebuggerGetStackTraceStackTraceId :: Runtime.RuntimeStackTraceId
  }
  deriving (Eq, Show)
pDebuggerGetStackTrace
  :: Runtime.RuntimeStackTraceId
  -> PDebuggerGetStackTrace
pDebuggerGetStackTrace
  arg_pDebuggerGetStackTraceStackTraceId
  = PDebuggerGetStackTrace
    arg_pDebuggerGetStackTraceStackTraceId
instance ToJSON PDebuggerGetStackTrace where
  toJSON p = A.object $ catMaybes [
    ("stackTraceId" A..=) <$> Just (pDebuggerGetStackTraceStackTraceId p)
    ]
data DebuggerGetStackTrace = DebuggerGetStackTrace
  {
    debuggerGetStackTraceStackTrace :: Runtime.RuntimeStackTrace
  }
  deriving (Eq, Show)
instance FromJSON DebuggerGetStackTrace where
  parseJSON = A.withObject "DebuggerGetStackTrace" $ \o -> DebuggerGetStackTrace
    <$> o A..: "stackTrace"
instance Command PDebuggerGetStackTrace where
  type CommandResponse PDebuggerGetStackTrace = DebuggerGetStackTrace
  commandName _ = "Debugger.getStackTrace"

-- | Stops on the next JavaScript statement.

-- | Parameters of the 'Debugger.pause' command.
data PDebuggerPause = PDebuggerPause
  deriving (Eq, Show)
pDebuggerPause
  :: PDebuggerPause
pDebuggerPause
  = PDebuggerPause
instance ToJSON PDebuggerPause where
  toJSON _ = A.Null
instance Command PDebuggerPause where
  type CommandResponse PDebuggerPause = ()
  commandName _ = "Debugger.pause"
  fromJSON = const . A.Success . const ()

-- | Removes JavaScript breakpoint.

-- | Parameters of the 'Debugger.removeBreakpoint' command.
data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint
  {
    pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
  }
  deriving (Eq, Show)
pDebuggerRemoveBreakpoint
  :: DebuggerBreakpointId
  -> PDebuggerRemoveBreakpoint
pDebuggerRemoveBreakpoint
  arg_pDebuggerRemoveBreakpointBreakpointId
  = PDebuggerRemoveBreakpoint
    arg_pDebuggerRemoveBreakpointBreakpointId
instance ToJSON PDebuggerRemoveBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("breakpointId" A..=) <$> Just (pDebuggerRemoveBreakpointBreakpointId p)
    ]
instance Command PDebuggerRemoveBreakpoint where
  type CommandResponse PDebuggerRemoveBreakpoint = ()
  commandName _ = "Debugger.removeBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Restarts particular call frame from the beginning. The old, deprecated
--   behavior of `restartFrame` is to stay paused and allow further CDP commands
--   after a restart was scheduled. This can cause problems with restarting, so
--   we now continue execution immediatly after it has been scheduled until we
--   reach the beginning of the restarted frame.
--   
--   To stay back-wards compatible, `restartFrame` now expects a `mode`
--   parameter to be present. If the `mode` parameter is missing, `restartFrame`
--   errors out.
--   
--   The various return values are deprecated and `callFrames` is always empty.
--   Use the call frames from the `Debugger#paused` events instead, that fires
--   once V8 pauses at the beginning of the restarted function.

-- | Parameters of the 'Debugger.restartFrame' command.
data PDebuggerRestartFrameMode = PDebuggerRestartFrameModeStepInto
  deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerRestartFrameMode where
  parseJSON = A.withText "PDebuggerRestartFrameMode" $ \v -> case v of
    "StepInto" -> pure PDebuggerRestartFrameModeStepInto
    "_" -> fail "failed to parse PDebuggerRestartFrameMode"
instance ToJSON PDebuggerRestartFrameMode where
  toJSON v = A.String $ case v of
    PDebuggerRestartFrameModeStepInto -> "StepInto"
data PDebuggerRestartFrame = PDebuggerRestartFrame
  {
    -- | Call frame identifier to evaluate on.
    pDebuggerRestartFrameCallFrameId :: DebuggerCallFrameId,
    -- | The `mode` parameter must be present and set to 'StepInto', otherwise
    --   `restartFrame` will error out.
    pDebuggerRestartFrameMode :: Maybe PDebuggerRestartFrameMode
  }
  deriving (Eq, Show)
pDebuggerRestartFrame
  {-
  -- | Call frame identifier to evaluate on.
  -}
  :: DebuggerCallFrameId
  -> PDebuggerRestartFrame
pDebuggerRestartFrame
  arg_pDebuggerRestartFrameCallFrameId
  = PDebuggerRestartFrame
    arg_pDebuggerRestartFrameCallFrameId
    Nothing
instance ToJSON PDebuggerRestartFrame where
  toJSON p = A.object $ catMaybes [
    ("callFrameId" A..=) <$> Just (pDebuggerRestartFrameCallFrameId p),
    ("mode" A..=) <$> (pDebuggerRestartFrameMode p)
    ]
instance Command PDebuggerRestartFrame where
  type CommandResponse PDebuggerRestartFrame = ()
  commandName _ = "Debugger.restartFrame"
  fromJSON = const . A.Success . const ()

-- | Resumes JavaScript execution.

-- | Parameters of the 'Debugger.resume' command.
data PDebuggerResume = PDebuggerResume
  {
    -- | Set to true to terminate execution upon resuming execution. In contrast
    --   to Runtime.terminateExecution, this will allows to execute further
    --   JavaScript (i.e. via evaluation) until execution of the paused code
    --   is actually resumed, at which point termination is triggered.
    --   If execution is currently not paused, this parameter has no effect.
    pDebuggerResumeTerminateOnResume :: Maybe Bool
  }
  deriving (Eq, Show)
pDebuggerResume
  :: PDebuggerResume
pDebuggerResume
  = PDebuggerResume
    Nothing
instance ToJSON PDebuggerResume where
  toJSON p = A.object $ catMaybes [
    ("terminateOnResume" A..=) <$> (pDebuggerResumeTerminateOnResume p)
    ]
instance Command PDebuggerResume where
  type CommandResponse PDebuggerResume = ()
  commandName _ = "Debugger.resume"
  fromJSON = const . A.Success . const ()

-- | Searches for given string in script content.

-- | Parameters of the 'Debugger.searchInContent' command.
data PDebuggerSearchInContent = PDebuggerSearchInContent
  {
    -- | Id of the script to search in.
    pDebuggerSearchInContentScriptId :: Runtime.RuntimeScriptId,
    -- | String to search for.
    pDebuggerSearchInContentQuery :: T.Text,
    -- | If true, search is case sensitive.
    pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
    -- | If true, treats string parameter as regex.
    pDebuggerSearchInContentIsRegex :: Maybe Bool
  }
  deriving (Eq, Show)
pDebuggerSearchInContent
  {-
  -- | Id of the script to search in.
  -}
  :: Runtime.RuntimeScriptId
  {-
  -- | String to search for.
  -}
  -> T.Text
  -> PDebuggerSearchInContent
pDebuggerSearchInContent
  arg_pDebuggerSearchInContentScriptId
  arg_pDebuggerSearchInContentQuery
  = PDebuggerSearchInContent
    arg_pDebuggerSearchInContentScriptId
    arg_pDebuggerSearchInContentQuery
    Nothing
    Nothing
instance ToJSON PDebuggerSearchInContent where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pDebuggerSearchInContentScriptId p),
    ("query" A..=) <$> Just (pDebuggerSearchInContentQuery p),
    ("caseSensitive" A..=) <$> (pDebuggerSearchInContentCaseSensitive p),
    ("isRegex" A..=) <$> (pDebuggerSearchInContentIsRegex p)
    ]
data DebuggerSearchInContent = DebuggerSearchInContent
  {
    -- | List of search matches.
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSearchInContent where
  parseJSON = A.withObject "DebuggerSearchInContent" $ \o -> DebuggerSearchInContent
    <$> o A..: "result"
instance Command PDebuggerSearchInContent where
  type CommandResponse PDebuggerSearchInContent = DebuggerSearchInContent
  commandName _ = "Debugger.searchInContent"

-- | Enables or disables async call stacks tracking.

-- | Parameters of the 'Debugger.setAsyncCallStackDepth' command.
data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth
  {
    -- | Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
    --   call stacks (default).
    pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
  }
  deriving (Eq, Show)
pDebuggerSetAsyncCallStackDepth
  {-
  -- | Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
  --   call stacks (default).
  -}
  :: Int
  -> PDebuggerSetAsyncCallStackDepth
pDebuggerSetAsyncCallStackDepth
  arg_pDebuggerSetAsyncCallStackDepthMaxDepth
  = PDebuggerSetAsyncCallStackDepth
    arg_pDebuggerSetAsyncCallStackDepthMaxDepth
instance ToJSON PDebuggerSetAsyncCallStackDepth where
  toJSON p = A.object $ catMaybes [
    ("maxDepth" A..=) <$> Just (pDebuggerSetAsyncCallStackDepthMaxDepth p)
    ]
instance Command PDebuggerSetAsyncCallStackDepth where
  type CommandResponse PDebuggerSetAsyncCallStackDepth = ()
  commandName _ = "Debugger.setAsyncCallStackDepth"
  fromJSON = const . A.Success . const ()

-- | Replace previous blackbox patterns with passed ones. Forces backend to skip stepping/pausing in
--   scripts with url matching one of the patterns. VM will try to leave blackboxed script by
--   performing 'step in' several times, finally resorting to 'step out' if unsuccessful.

-- | Parameters of the 'Debugger.setBlackboxPatterns' command.
data PDebuggerSetBlackboxPatterns = PDebuggerSetBlackboxPatterns
  {
    -- | Array of regexps that will be used to check script url for blackbox state.
    pDebuggerSetBlackboxPatternsPatterns :: [T.Text]
  }
  deriving (Eq, Show)
pDebuggerSetBlackboxPatterns
  {-
  -- | Array of regexps that will be used to check script url for blackbox state.
  -}
  :: [T.Text]
  -> PDebuggerSetBlackboxPatterns
pDebuggerSetBlackboxPatterns
  arg_pDebuggerSetBlackboxPatternsPatterns
  = PDebuggerSetBlackboxPatterns
    arg_pDebuggerSetBlackboxPatternsPatterns
instance ToJSON PDebuggerSetBlackboxPatterns where
  toJSON p = A.object $ catMaybes [
    ("patterns" A..=) <$> Just (pDebuggerSetBlackboxPatternsPatterns p)
    ]
instance Command PDebuggerSetBlackboxPatterns where
  type CommandResponse PDebuggerSetBlackboxPatterns = ()
  commandName _ = "Debugger.setBlackboxPatterns"
  fromJSON = const . A.Success . const ()

-- | Makes backend skip steps in the script in blackboxed ranges. VM will try leave blacklisted
--   scripts by performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
--   Positions array contains positions where blackbox state is changed. First interval isn't
--   blackboxed. Array should be sorted.

-- | Parameters of the 'Debugger.setBlackboxedRanges' command.
data PDebuggerSetBlackboxedRanges = PDebuggerSetBlackboxedRanges
  {
    -- | Id of the script.
    pDebuggerSetBlackboxedRangesScriptId :: Runtime.RuntimeScriptId,
    pDebuggerSetBlackboxedRangesPositions :: [DebuggerScriptPosition]
  }
  deriving (Eq, Show)
pDebuggerSetBlackboxedRanges
  {-
  -- | Id of the script.
  -}
  :: Runtime.RuntimeScriptId
  -> [DebuggerScriptPosition]
  -> PDebuggerSetBlackboxedRanges
pDebuggerSetBlackboxedRanges
  arg_pDebuggerSetBlackboxedRangesScriptId
  arg_pDebuggerSetBlackboxedRangesPositions
  = PDebuggerSetBlackboxedRanges
    arg_pDebuggerSetBlackboxedRangesScriptId
    arg_pDebuggerSetBlackboxedRangesPositions
instance ToJSON PDebuggerSetBlackboxedRanges where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pDebuggerSetBlackboxedRangesScriptId p),
    ("positions" A..=) <$> Just (pDebuggerSetBlackboxedRangesPositions p)
    ]
instance Command PDebuggerSetBlackboxedRanges where
  type CommandResponse PDebuggerSetBlackboxedRanges = ()
  commandName _ = "Debugger.setBlackboxedRanges"
  fromJSON = const . A.Success . const ()

-- | Sets JavaScript breakpoint at a given location.

-- | Parameters of the 'Debugger.setBreakpoint' command.
data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint
  {
    -- | Location to set breakpoint in.
    pDebuggerSetBreakpointLocation :: DebuggerLocation,
    -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
    --   breakpoint if this expression evaluates to true.
    pDebuggerSetBreakpointCondition :: Maybe T.Text
  }
  deriving (Eq, Show)
pDebuggerSetBreakpoint
  {-
  -- | Location to set breakpoint in.
  -}
  :: DebuggerLocation
  -> PDebuggerSetBreakpoint
pDebuggerSetBreakpoint
  arg_pDebuggerSetBreakpointLocation
  = PDebuggerSetBreakpoint
    arg_pDebuggerSetBreakpointLocation
    Nothing
instance ToJSON PDebuggerSetBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("location" A..=) <$> Just (pDebuggerSetBreakpointLocation p),
    ("condition" A..=) <$> (pDebuggerSetBreakpointCondition p)
    ]
data DebuggerSetBreakpoint = DebuggerSetBreakpoint
  {
    -- | Id of the created breakpoint for further reference.
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    -- | Location this breakpoint resolved into.
    debuggerSetBreakpointActualLocation :: DebuggerLocation
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSetBreakpoint where
  parseJSON = A.withObject "DebuggerSetBreakpoint" $ \o -> DebuggerSetBreakpoint
    <$> o A..: "breakpointId"
    <*> o A..: "actualLocation"
instance Command PDebuggerSetBreakpoint where
  type CommandResponse PDebuggerSetBreakpoint = DebuggerSetBreakpoint
  commandName _ = "Debugger.setBreakpoint"

-- | Sets instrumentation breakpoint.

-- | Parameters of the 'Debugger.setInstrumentationBreakpoint' command.
data PDebuggerSetInstrumentationBreakpointInstrumentation = PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution | PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
  deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
  parseJSON = A.withText "PDebuggerSetInstrumentationBreakpointInstrumentation" $ \v -> case v of
    "beforeScriptExecution" -> pure PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution
    "beforeScriptWithSourceMapExecution" -> pure PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
    "_" -> fail "failed to parse PDebuggerSetInstrumentationBreakpointInstrumentation"
instance ToJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
  toJSON v = A.String $ case v of
    PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution -> "beforeScriptExecution"
    PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution -> "beforeScriptWithSourceMapExecution"
data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint
  {
    -- | Instrumentation name.
    pDebuggerSetInstrumentationBreakpointInstrumentation :: PDebuggerSetInstrumentationBreakpointInstrumentation
  }
  deriving (Eq, Show)
pDebuggerSetInstrumentationBreakpoint
  {-
  -- | Instrumentation name.
  -}
  :: PDebuggerSetInstrumentationBreakpointInstrumentation
  -> PDebuggerSetInstrumentationBreakpoint
pDebuggerSetInstrumentationBreakpoint
  arg_pDebuggerSetInstrumentationBreakpointInstrumentation
  = PDebuggerSetInstrumentationBreakpoint
    arg_pDebuggerSetInstrumentationBreakpointInstrumentation
instance ToJSON PDebuggerSetInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("instrumentation" A..=) <$> Just (pDebuggerSetInstrumentationBreakpointInstrumentation p)
    ]
data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint
  {
    -- | Id of the created breakpoint for further reference.
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSetInstrumentationBreakpoint where
  parseJSON = A.withObject "DebuggerSetInstrumentationBreakpoint" $ \o -> DebuggerSetInstrumentationBreakpoint
    <$> o A..: "breakpointId"
instance Command PDebuggerSetInstrumentationBreakpoint where
  type CommandResponse PDebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint
  commandName _ = "Debugger.setInstrumentationBreakpoint"

-- | Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
--   command is issued, all existing parsed scripts will have breakpoints resolved and returned in
--   `locations` property. Further matching script parsing will result in subsequent
--   `breakpointResolved` events issued. This logical breakpoint will survive page reloads.

-- | Parameters of the 'Debugger.setBreakpointByUrl' command.
data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl
  {
    -- | Line number to set breakpoint at.
    pDebuggerSetBreakpointByUrlLineNumber :: Int,
    -- | URL of the resources to set breakpoint on.
    pDebuggerSetBreakpointByUrlUrl :: Maybe T.Text,
    -- | Regex pattern for the URLs of the resources to set breakpoints on. Either `url` or
    --   `urlRegex` must be specified.
    pDebuggerSetBreakpointByUrlUrlRegex :: Maybe T.Text,
    -- | Script hash of the resources to set breakpoint on.
    pDebuggerSetBreakpointByUrlScriptHash :: Maybe T.Text,
    -- | Offset in the line to set breakpoint at.
    pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
    -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
    --   breakpoint if this expression evaluates to true.
    pDebuggerSetBreakpointByUrlCondition :: Maybe T.Text
  }
  deriving (Eq, Show)
pDebuggerSetBreakpointByUrl
  {-
  -- | Line number to set breakpoint at.
  -}
  :: Int
  -> PDebuggerSetBreakpointByUrl
pDebuggerSetBreakpointByUrl
  arg_pDebuggerSetBreakpointByUrlLineNumber
  = PDebuggerSetBreakpointByUrl
    arg_pDebuggerSetBreakpointByUrlLineNumber
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDebuggerSetBreakpointByUrl where
  toJSON p = A.object $ catMaybes [
    ("lineNumber" A..=) <$> Just (pDebuggerSetBreakpointByUrlLineNumber p),
    ("url" A..=) <$> (pDebuggerSetBreakpointByUrlUrl p),
    ("urlRegex" A..=) <$> (pDebuggerSetBreakpointByUrlUrlRegex p),
    ("scriptHash" A..=) <$> (pDebuggerSetBreakpointByUrlScriptHash p),
    ("columnNumber" A..=) <$> (pDebuggerSetBreakpointByUrlColumnNumber p),
    ("condition" A..=) <$> (pDebuggerSetBreakpointByUrlCondition p)
    ]
data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl
  {
    -- | Id of the created breakpoint for further reference.
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    -- | List of the locations this breakpoint resolved into upon addition.
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSetBreakpointByUrl where
  parseJSON = A.withObject "DebuggerSetBreakpointByUrl" $ \o -> DebuggerSetBreakpointByUrl
    <$> o A..: "breakpointId"
    <*> o A..: "locations"
instance Command PDebuggerSetBreakpointByUrl where
  type CommandResponse PDebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl
  commandName _ = "Debugger.setBreakpointByUrl"

-- | Sets JavaScript breakpoint before each call to the given function.
--   If another function was created from the same source as a given one,
--   calling it will also trigger the breakpoint.

-- | Parameters of the 'Debugger.setBreakpointOnFunctionCall' command.
data PDebuggerSetBreakpointOnFunctionCall = PDebuggerSetBreakpointOnFunctionCall
  {
    -- | Function object id.
    pDebuggerSetBreakpointOnFunctionCallObjectId :: Runtime.RuntimeRemoteObjectId,
    -- | Expression to use as a breakpoint condition. When specified, debugger will
    --   stop on the breakpoint if this expression evaluates to true.
    pDebuggerSetBreakpointOnFunctionCallCondition :: Maybe T.Text
  }
  deriving (Eq, Show)
pDebuggerSetBreakpointOnFunctionCall
  {-
  -- | Function object id.
  -}
  :: Runtime.RuntimeRemoteObjectId
  -> PDebuggerSetBreakpointOnFunctionCall
pDebuggerSetBreakpointOnFunctionCall
  arg_pDebuggerSetBreakpointOnFunctionCallObjectId
  = PDebuggerSetBreakpointOnFunctionCall
    arg_pDebuggerSetBreakpointOnFunctionCallObjectId
    Nothing
instance ToJSON PDebuggerSetBreakpointOnFunctionCall where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pDebuggerSetBreakpointOnFunctionCallObjectId p),
    ("condition" A..=) <$> (pDebuggerSetBreakpointOnFunctionCallCondition p)
    ]
data DebuggerSetBreakpointOnFunctionCall = DebuggerSetBreakpointOnFunctionCall
  {
    -- | Id of the created breakpoint for further reference.
    debuggerSetBreakpointOnFunctionCallBreakpointId :: DebuggerBreakpointId
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSetBreakpointOnFunctionCall where
  parseJSON = A.withObject "DebuggerSetBreakpointOnFunctionCall" $ \o -> DebuggerSetBreakpointOnFunctionCall
    <$> o A..: "breakpointId"
instance Command PDebuggerSetBreakpointOnFunctionCall where
  type CommandResponse PDebuggerSetBreakpointOnFunctionCall = DebuggerSetBreakpointOnFunctionCall
  commandName _ = "Debugger.setBreakpointOnFunctionCall"

-- | Activates / deactivates all breakpoints on the page.

-- | Parameters of the 'Debugger.setBreakpointsActive' command.
data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive
  {
    -- | New value for breakpoints active state.
    pDebuggerSetBreakpointsActiveActive :: Bool
  }
  deriving (Eq, Show)
pDebuggerSetBreakpointsActive
  {-
  -- | New value for breakpoints active state.
  -}
  :: Bool
  -> PDebuggerSetBreakpointsActive
pDebuggerSetBreakpointsActive
  arg_pDebuggerSetBreakpointsActiveActive
  = PDebuggerSetBreakpointsActive
    arg_pDebuggerSetBreakpointsActiveActive
instance ToJSON PDebuggerSetBreakpointsActive where
  toJSON p = A.object $ catMaybes [
    ("active" A..=) <$> Just (pDebuggerSetBreakpointsActiveActive p)
    ]
instance Command PDebuggerSetBreakpointsActive where
  type CommandResponse PDebuggerSetBreakpointsActive = ()
  commandName _ = "Debugger.setBreakpointsActive"
  fromJSON = const . A.Success . const ()

-- | Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or
--   no exceptions. Initial pause on exceptions state is `none`.

-- | Parameters of the 'Debugger.setPauseOnExceptions' command.
data PDebuggerSetPauseOnExceptionsState = PDebuggerSetPauseOnExceptionsStateNone | PDebuggerSetPauseOnExceptionsStateUncaught | PDebuggerSetPauseOnExceptionsStateAll
  deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetPauseOnExceptionsState where
  parseJSON = A.withText "PDebuggerSetPauseOnExceptionsState" $ \v -> case v of
    "none" -> pure PDebuggerSetPauseOnExceptionsStateNone
    "uncaught" -> pure PDebuggerSetPauseOnExceptionsStateUncaught
    "all" -> pure PDebuggerSetPauseOnExceptionsStateAll
    "_" -> fail "failed to parse PDebuggerSetPauseOnExceptionsState"
instance ToJSON PDebuggerSetPauseOnExceptionsState where
  toJSON v = A.String $ case v of
    PDebuggerSetPauseOnExceptionsStateNone -> "none"
    PDebuggerSetPauseOnExceptionsStateUncaught -> "uncaught"
    PDebuggerSetPauseOnExceptionsStateAll -> "all"
data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions
  {
    -- | Pause on exceptions mode.
    pDebuggerSetPauseOnExceptionsState :: PDebuggerSetPauseOnExceptionsState
  }
  deriving (Eq, Show)
pDebuggerSetPauseOnExceptions
  {-
  -- | Pause on exceptions mode.
  -}
  :: PDebuggerSetPauseOnExceptionsState
  -> PDebuggerSetPauseOnExceptions
pDebuggerSetPauseOnExceptions
  arg_pDebuggerSetPauseOnExceptionsState
  = PDebuggerSetPauseOnExceptions
    arg_pDebuggerSetPauseOnExceptionsState
instance ToJSON PDebuggerSetPauseOnExceptions where
  toJSON p = A.object $ catMaybes [
    ("state" A..=) <$> Just (pDebuggerSetPauseOnExceptionsState p)
    ]
instance Command PDebuggerSetPauseOnExceptions where
  type CommandResponse PDebuggerSetPauseOnExceptions = ()
  commandName _ = "Debugger.setPauseOnExceptions"
  fromJSON = const . A.Success . const ()

-- | Changes return value in top frame. Available only at return break position.

-- | Parameters of the 'Debugger.setReturnValue' command.
data PDebuggerSetReturnValue = PDebuggerSetReturnValue
  {
    -- | New return value.
    pDebuggerSetReturnValueNewValue :: Runtime.RuntimeCallArgument
  }
  deriving (Eq, Show)
pDebuggerSetReturnValue
  {-
  -- | New return value.
  -}
  :: Runtime.RuntimeCallArgument
  -> PDebuggerSetReturnValue
pDebuggerSetReturnValue
  arg_pDebuggerSetReturnValueNewValue
  = PDebuggerSetReturnValue
    arg_pDebuggerSetReturnValueNewValue
instance ToJSON PDebuggerSetReturnValue where
  toJSON p = A.object $ catMaybes [
    ("newValue" A..=) <$> Just (pDebuggerSetReturnValueNewValue p)
    ]
instance Command PDebuggerSetReturnValue where
  type CommandResponse PDebuggerSetReturnValue = ()
  commandName _ = "Debugger.setReturnValue"
  fromJSON = const . A.Success . const ()

-- | Edits JavaScript source live.
--   
--   In general, functions that are currently on the stack can not be edited with
--   a single exception: If the edited function is the top-most stack frame and
--   that is the only activation of that function on the stack. In this case
--   the live edit will be successful and a `Debugger.restartFrame` for the
--   top-most function is automatically triggered.

-- | Parameters of the 'Debugger.setScriptSource' command.
data PDebuggerSetScriptSource = PDebuggerSetScriptSource
  {
    -- | Id of the script to edit.
    pDebuggerSetScriptSourceScriptId :: Runtime.RuntimeScriptId,
    -- | New content of the script.
    pDebuggerSetScriptSourceScriptSource :: T.Text,
    -- | If true the change will not actually be applied. Dry run may be used to get result
    --   description without actually modifying the code.
    pDebuggerSetScriptSourceDryRun :: Maybe Bool,
    -- | If true, then `scriptSource` is allowed to change the function on top of the stack
    --   as long as the top-most stack frame is the only activation of that function.
    pDebuggerSetScriptSourceAllowTopFrameEditing :: Maybe Bool
  }
  deriving (Eq, Show)
pDebuggerSetScriptSource
  {-
  -- | Id of the script to edit.
  -}
  :: Runtime.RuntimeScriptId
  {-
  -- | New content of the script.
  -}
  -> T.Text
  -> PDebuggerSetScriptSource
pDebuggerSetScriptSource
  arg_pDebuggerSetScriptSourceScriptId
  arg_pDebuggerSetScriptSourceScriptSource
  = PDebuggerSetScriptSource
    arg_pDebuggerSetScriptSourceScriptId
    arg_pDebuggerSetScriptSourceScriptSource
    Nothing
    Nothing
instance ToJSON PDebuggerSetScriptSource where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pDebuggerSetScriptSourceScriptId p),
    ("scriptSource" A..=) <$> Just (pDebuggerSetScriptSourceScriptSource p),
    ("dryRun" A..=) <$> (pDebuggerSetScriptSourceDryRun p),
    ("allowTopFrameEditing" A..=) <$> (pDebuggerSetScriptSourceAllowTopFrameEditing p)
    ]
data DebuggerSetScriptSourceStatus = DebuggerSetScriptSourceStatusOk | DebuggerSetScriptSourceStatusCompileError | DebuggerSetScriptSourceStatusBlockedByActiveGenerator | DebuggerSetScriptSourceStatusBlockedByActiveFunction
  deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerSetScriptSourceStatus where
  parseJSON = A.withText "DebuggerSetScriptSourceStatus" $ \v -> case v of
    "Ok" -> pure DebuggerSetScriptSourceStatusOk
    "CompileError" -> pure DebuggerSetScriptSourceStatusCompileError
    "BlockedByActiveGenerator" -> pure DebuggerSetScriptSourceStatusBlockedByActiveGenerator
    "BlockedByActiveFunction" -> pure DebuggerSetScriptSourceStatusBlockedByActiveFunction
    "_" -> fail "failed to parse DebuggerSetScriptSourceStatus"
instance ToJSON DebuggerSetScriptSourceStatus where
  toJSON v = A.String $ case v of
    DebuggerSetScriptSourceStatusOk -> "Ok"
    DebuggerSetScriptSourceStatusCompileError -> "CompileError"
    DebuggerSetScriptSourceStatusBlockedByActiveGenerator -> "BlockedByActiveGenerator"
    DebuggerSetScriptSourceStatusBlockedByActiveFunction -> "BlockedByActiveFunction"
data DebuggerSetScriptSource = DebuggerSetScriptSource
  {
    -- | Whether the operation was successful or not. Only `Ok` denotes a
    --   successful live edit while the other enum variants denote why
    --   the live edit failed.
    debuggerSetScriptSourceStatus :: DebuggerSetScriptSourceStatus,
    -- | Exception details if any. Only present when `status` is `CompileError`.
    debuggerSetScriptSourceExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON DebuggerSetScriptSource where
  parseJSON = A.withObject "DebuggerSetScriptSource" $ \o -> DebuggerSetScriptSource
    <$> o A..: "status"
    <*> o A..:? "exceptionDetails"
instance Command PDebuggerSetScriptSource where
  type CommandResponse PDebuggerSetScriptSource = DebuggerSetScriptSource
  commandName _ = "Debugger.setScriptSource"

-- | Makes page not interrupt on any pauses (breakpoint, exception, dom exception etc).

-- | Parameters of the 'Debugger.setSkipAllPauses' command.
data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses
  {
    -- | New value for skip pauses state.
    pDebuggerSetSkipAllPausesSkip :: Bool
  }
  deriving (Eq, Show)
pDebuggerSetSkipAllPauses
  {-
  -- | New value for skip pauses state.
  -}
  :: Bool
  -> PDebuggerSetSkipAllPauses
pDebuggerSetSkipAllPauses
  arg_pDebuggerSetSkipAllPausesSkip
  = PDebuggerSetSkipAllPauses
    arg_pDebuggerSetSkipAllPausesSkip
instance ToJSON PDebuggerSetSkipAllPauses where
  toJSON p = A.object $ catMaybes [
    ("skip" A..=) <$> Just (pDebuggerSetSkipAllPausesSkip p)
    ]
instance Command PDebuggerSetSkipAllPauses where
  type CommandResponse PDebuggerSetSkipAllPauses = ()
  commandName _ = "Debugger.setSkipAllPauses"
  fromJSON = const . A.Success . const ()

-- | Changes value of variable in a callframe. Object-based scopes are not supported and must be
--   mutated manually.

-- | Parameters of the 'Debugger.setVariableValue' command.
data PDebuggerSetVariableValue = PDebuggerSetVariableValue
  {
    -- | 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'
    --   scope types are allowed. Other scopes could be manipulated manually.
    pDebuggerSetVariableValueScopeNumber :: Int,
    -- | Variable name.
    pDebuggerSetVariableValueVariableName :: T.Text,
    -- | New variable value.
    pDebuggerSetVariableValueNewValue :: Runtime.RuntimeCallArgument,
    -- | Id of callframe that holds variable.
    pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
  }
  deriving (Eq, Show)
pDebuggerSetVariableValue
  {-
  -- | 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'
  --   scope types are allowed. Other scopes could be manipulated manually.
  -}
  :: Int
  {-
  -- | Variable name.
  -}
  -> T.Text
  {-
  -- | New variable value.
  -}
  -> Runtime.RuntimeCallArgument
  {-
  -- | Id of callframe that holds variable.
  -}
  -> DebuggerCallFrameId
  -> PDebuggerSetVariableValue
pDebuggerSetVariableValue
  arg_pDebuggerSetVariableValueScopeNumber
  arg_pDebuggerSetVariableValueVariableName
  arg_pDebuggerSetVariableValueNewValue
  arg_pDebuggerSetVariableValueCallFrameId
  = PDebuggerSetVariableValue
    arg_pDebuggerSetVariableValueScopeNumber
    arg_pDebuggerSetVariableValueVariableName
    arg_pDebuggerSetVariableValueNewValue
    arg_pDebuggerSetVariableValueCallFrameId
instance ToJSON PDebuggerSetVariableValue where
  toJSON p = A.object $ catMaybes [
    ("scopeNumber" A..=) <$> Just (pDebuggerSetVariableValueScopeNumber p),
    ("variableName" A..=) <$> Just (pDebuggerSetVariableValueVariableName p),
    ("newValue" A..=) <$> Just (pDebuggerSetVariableValueNewValue p),
    ("callFrameId" A..=) <$> Just (pDebuggerSetVariableValueCallFrameId p)
    ]
instance Command PDebuggerSetVariableValue where
  type CommandResponse PDebuggerSetVariableValue = ()
  commandName _ = "Debugger.setVariableValue"
  fromJSON = const . A.Success . const ()

-- | Steps into the function call.

-- | Parameters of the 'Debugger.stepInto' command.
data PDebuggerStepInto = PDebuggerStepInto
  {
    -- | Debugger will pause on the execution of the first async task which was scheduled
    --   before next pause.
    pDebuggerStepIntoBreakOnAsyncCall :: Maybe Bool,
    -- | The skipList specifies location ranges that should be skipped on step into.
    pDebuggerStepIntoSkipList :: Maybe [DebuggerLocationRange]
  }
  deriving (Eq, Show)
pDebuggerStepInto
  :: PDebuggerStepInto
pDebuggerStepInto
  = PDebuggerStepInto
    Nothing
    Nothing
instance ToJSON PDebuggerStepInto where
  toJSON p = A.object $ catMaybes [
    ("breakOnAsyncCall" A..=) <$> (pDebuggerStepIntoBreakOnAsyncCall p),
    ("skipList" A..=) <$> (pDebuggerStepIntoSkipList p)
    ]
instance Command PDebuggerStepInto where
  type CommandResponse PDebuggerStepInto = ()
  commandName _ = "Debugger.stepInto"
  fromJSON = const . A.Success . const ()

-- | Steps out of the function call.

-- | Parameters of the 'Debugger.stepOut' command.
data PDebuggerStepOut = PDebuggerStepOut
  deriving (Eq, Show)
pDebuggerStepOut
  :: PDebuggerStepOut
pDebuggerStepOut
  = PDebuggerStepOut
instance ToJSON PDebuggerStepOut where
  toJSON _ = A.Null
instance Command PDebuggerStepOut where
  type CommandResponse PDebuggerStepOut = ()
  commandName _ = "Debugger.stepOut"
  fromJSON = const . A.Success . const ()

-- | Steps over the statement.

-- | Parameters of the 'Debugger.stepOver' command.
data PDebuggerStepOver = PDebuggerStepOver
  {
    -- | The skipList specifies location ranges that should be skipped on step over.
    pDebuggerStepOverSkipList :: Maybe [DebuggerLocationRange]
  }
  deriving (Eq, Show)
pDebuggerStepOver
  :: PDebuggerStepOver
pDebuggerStepOver
  = PDebuggerStepOver
    Nothing
instance ToJSON PDebuggerStepOver where
  toJSON p = A.object $ catMaybes [
    ("skipList" A..=) <$> (pDebuggerStepOverSkipList p)
    ]
instance Command PDebuggerStepOver where
  type CommandResponse PDebuggerStepOver = ()
  commandName _ = "Debugger.stepOver"
  fromJSON = const . A.Success . const ()

