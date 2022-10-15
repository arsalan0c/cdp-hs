{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Debugger :
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
type DebuggerBreakpointId = String

-- | Type 'Debugger.CallFrameId'.
--   Call frame identifier.
type DebuggerCallFrameId = String

-- | Type 'Debugger.Location'.
--   Location in the source code.
data DebuggerLocation = DebuggerLocation {
  -- | Script identifier as reported in the `Debugger.scriptParsed`.
  debuggerLocationScriptId :: Runtime.RuntimeScriptId,
  -- | Line number in the script (0-based).
  debuggerLocationLineNumber :: Int,
  -- | Column number in the script (0-based).
  debuggerLocationColumnNumber :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Debugger.ScriptPosition'.
--   Location in the source code.
data DebuggerScriptPosition = DebuggerScriptPosition {
  debuggerScriptPositionLineNumber :: Int,
  debuggerScriptPositionColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptPosition  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptPosition where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Debugger.LocationRange'.
--   Location range within one script.
data DebuggerLocationRange = DebuggerLocationRange {
  debuggerLocationRangeScriptId :: Runtime.RuntimeScriptId,
  debuggerLocationRangeStart :: DebuggerScriptPosition,
  debuggerLocationRangeEnd :: DebuggerScriptPosition
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocationRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocationRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Debugger.CallFrame'.
--   JavaScript call frame. Array of call frames form the call stack.
data DebuggerCallFrame = DebuggerCallFrame {
  -- | Call frame identifier. This identifier is only valid while the virtual machine is paused.
  debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
  -- | Name of the JavaScript function called on this call frame.
  debuggerCallFrameFunctionName :: String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  DebuggerCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'Debugger.Scope'.
--   Scope description.
data DebuggerScopeType = DebuggerScopeTypeGlobal | DebuggerScopeTypeLocal | DebuggerScopeTypeWith | DebuggerScopeTypeClosure | DebuggerScopeTypeCatch | DebuggerScopeTypeBlock | DebuggerScopeTypeScript | DebuggerScopeTypeEval | DebuggerScopeTypeModule | DebuggerScopeTypeWasmExpressionStack
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerScopeType where
   parseJSON = A.withText  "DebuggerScopeType"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse DebuggerScopeType"

instance ToJSON DebuggerScopeType where
   toJSON v = A.String $
      case v of
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



data DebuggerScope = DebuggerScope {
  -- | Scope type.
  debuggerScopeType :: DebuggerScopeType,
  -- | Object representing the scope. For `global` and `with` scopes it represents the actual
  --   object; for the rest of the scopes, it is artificial transient object enumerating scope
  --   variables as its properties.
  debuggerScopeObject :: Runtime.RuntimeRemoteObject,
  debuggerScopeName :: Maybe String,
  -- | Location in the source code where scope starts
  debuggerScopeStartLocation :: Maybe DebuggerLocation,
  -- | Location in the source code where scope ends
  debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScope  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DebuggerScope where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Debugger.SearchMatch'.
--   Search match for resource.
data DebuggerSearchMatch = DebuggerSearchMatch {
  -- | Line number in resource content.
  debuggerSearchMatchLineNumber :: Double,
  -- | Line with match content.
  debuggerSearchMatchLineContent :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerSearchMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DebuggerSearchMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Debugger.BreakLocation'.
data DebuggerBreakLocationType = DebuggerBreakLocationTypeDebuggerStatement | DebuggerBreakLocationTypeCall | DebuggerBreakLocationTypeReturn
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerBreakLocationType where
   parseJSON = A.withText  "DebuggerBreakLocationType"  $ \v -> do
      case v of
         "debuggerStatement" -> pure DebuggerBreakLocationTypeDebuggerStatement
         "call" -> pure DebuggerBreakLocationTypeCall
         "return" -> pure DebuggerBreakLocationTypeReturn
         _ -> fail "failed to parse DebuggerBreakLocationType"

instance ToJSON DebuggerBreakLocationType where
   toJSON v = A.String $
      case v of
         DebuggerBreakLocationTypeDebuggerStatement -> "debuggerStatement"
         DebuggerBreakLocationTypeCall -> "call"
         DebuggerBreakLocationTypeReturn -> "return"



data DebuggerBreakLocation = DebuggerBreakLocation {
  -- | Script identifier as reported in the `Debugger.scriptParsed`.
  debuggerBreakLocationScriptId :: Runtime.RuntimeScriptId,
  -- | Line number in the script (0-based).
  debuggerBreakLocationLineNumber :: Int,
  -- | Column number in the script (0-based).
  debuggerBreakLocationColumnNumber :: Maybe Int,
  debuggerBreakLocationType :: DebuggerBreakLocationType
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Debugger.WasmDisassemblyChunk'.
data DebuggerWasmDisassemblyChunk = DebuggerWasmDisassemblyChunk {
  -- | The next chunk of disassembled lines.
  debuggerWasmDisassemblyChunkLines :: [String],
  -- | The bytecode offsets describing the start of each line.
  debuggerWasmDisassemblyChunkBytecodeOffsets :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerWasmDisassemblyChunk  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  DebuggerWasmDisassemblyChunk where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type 'Debugger.ScriptLanguage'.
--   Enum of possible script languages.
data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerScriptLanguage where
   parseJSON = A.withText  "DebuggerScriptLanguage"  $ \v -> do
      case v of
         "JavaScript" -> pure DebuggerScriptLanguageJavaScript
         "WebAssembly" -> pure DebuggerScriptLanguageWebAssembly
         _ -> fail "failed to parse DebuggerScriptLanguage"

instance ToJSON DebuggerScriptLanguage where
   toJSON v = A.String $
      case v of
         DebuggerScriptLanguageJavaScript -> "JavaScript"
         DebuggerScriptLanguageWebAssembly -> "WebAssembly"



-- | Type 'Debugger.DebugSymbols'.
--   Debug symbols available for a wasm script.
data DebuggerDebugSymbolsType = DebuggerDebugSymbolsTypeNone | DebuggerDebugSymbolsTypeSourceMap | DebuggerDebugSymbolsTypeEmbeddedDWARF | DebuggerDebugSymbolsTypeExternalDWARF
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerDebugSymbolsType where
   parseJSON = A.withText  "DebuggerDebugSymbolsType"  $ \v -> do
      case v of
         "None" -> pure DebuggerDebugSymbolsTypeNone
         "SourceMap" -> pure DebuggerDebugSymbolsTypeSourceMap
         "EmbeddedDWARF" -> pure DebuggerDebugSymbolsTypeEmbeddedDWARF
         "ExternalDWARF" -> pure DebuggerDebugSymbolsTypeExternalDWARF
         _ -> fail "failed to parse DebuggerDebugSymbolsType"

instance ToJSON DebuggerDebugSymbolsType where
   toJSON v = A.String $
      case v of
         DebuggerDebugSymbolsTypeNone -> "None"
         DebuggerDebugSymbolsTypeSourceMap -> "SourceMap"
         DebuggerDebugSymbolsTypeEmbeddedDWARF -> "EmbeddedDWARF"
         DebuggerDebugSymbolsTypeExternalDWARF -> "ExternalDWARF"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
  -- | Type of the debug symbols.
  debuggerDebugSymbolsType :: DebuggerDebugSymbolsType,
  -- | URL of the external symbol source.
  debuggerDebugSymbolsExternalURL :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerDebugSymbols  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerDebugSymbols where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Type of the 'Debugger.breakpointResolved' event.
data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
  -- | Breakpoint unique identifier.
  debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
  -- | Actual breakpoint location.
  debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakpointResolved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakpointResolved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Event DebuggerBreakpointResolved where
    eventName _ = "Debugger.breakpointResolved"

-- | Type of the 'Debugger.paused' event.
data DebuggerPausedReason = DebuggerPausedReasonAmbiguous | DebuggerPausedReasonAssert | DebuggerPausedReasonCSPViolation | DebuggerPausedReasonDebugCommand | DebuggerPausedReasonDOM | DebuggerPausedReasonEventListener | DebuggerPausedReasonException | DebuggerPausedReasonInstrumentation | DebuggerPausedReasonOOM | DebuggerPausedReasonOther | DebuggerPausedReasonPromiseRejection | DebuggerPausedReasonXHR
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerPausedReason where
   parseJSON = A.withText  "DebuggerPausedReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse DebuggerPausedReason"

instance ToJSON DebuggerPausedReason where
   toJSON v = A.String $
      case v of
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



data DebuggerPaused = DebuggerPaused {
  -- | Call stack the virtual machine stopped on.
  debuggerPausedCallFrames :: [DebuggerCallFrame],
  -- | Pause reason.
  debuggerPausedReason :: DebuggerPausedReason,
  -- | Object containing break-specific auxiliary properties.
  debuggerPausedData :: Maybe [(String, String)],
  -- | Hit breakpoints IDs
  debuggerPausedHitBreakpoints :: Maybe [String],
  -- | Async stack trace, if any.
  debuggerPausedAsyncStackTrace :: Maybe Runtime.RuntimeStackTrace,
  -- | Async stack trace, if any.
  debuggerPausedAsyncStackTraceId :: Maybe Runtime.RuntimeStackTraceId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DebuggerPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Event DebuggerPaused where
    eventName _ = "Debugger.paused"

-- | Type of the 'Debugger.resumed' event.
data DebuggerResumed = DebuggerResumed
   deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
   parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
      case v of
         "DebuggerResumed" -> pure DebuggerResumed
         _ -> fail "failed to parse DebuggerResumed"


instance Event DebuggerResumed where
    eventName _ = "Debugger.resumed"

-- | Type of the 'Debugger.scriptFailedToParse' event.
data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
  -- | Identifier of the script parsed.
  debuggerScriptFailedToParseScriptId :: Runtime.RuntimeScriptId,
  -- | URL or name of the script parsed (if any).
  debuggerScriptFailedToParseUrl :: String,
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
  debuggerScriptFailedToParseHash :: String,
  -- | Embedder-specific auxiliary data.
  debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(String, String)],
  -- | URL of source map associated with script (if any).
  debuggerScriptFailedToParseSourceMapURL :: Maybe String,
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
  debuggerScriptFailedToParseEmbedderName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptFailedToParse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptFailedToParse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Event DebuggerScriptFailedToParse where
    eventName _ = "Debugger.scriptFailedToParse"

-- | Type of the 'Debugger.scriptParsed' event.
data DebuggerScriptParsed = DebuggerScriptParsed {
  -- | Identifier of the script parsed.
  debuggerScriptParsedScriptId :: Runtime.RuntimeScriptId,
  -- | URL or name of the script parsed (if any).
  debuggerScriptParsedUrl :: String,
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
  debuggerScriptParsedHash :: String,
  -- | Embedder-specific auxiliary data.
  debuggerScriptParsedExecutionContextAuxData :: Maybe [(String, String)],
  -- | True, if this script is generated as a result of the live edit operation.
  debuggerScriptParsedIsLiveEdit :: Maybe Bool,
  -- | URL of source map associated with script (if any).
  debuggerScriptParsedSourceMapURL :: Maybe String,
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
  debuggerScriptParsedEmbedderName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Event DebuggerScriptParsed where
    eventName _ = "Debugger.scriptParsed"



-- | Debugger.continueToLocation
--   Continues execution until specific location is reached.

-- | Parameters of the 'Debugger.continueToLocation' command.
data PDebuggerContinueToLocationTargetCallFrames = PDebuggerContinueToLocationTargetCallFramesAny | PDebuggerContinueToLocationTargetCallFramesCurrent
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerContinueToLocationTargetCallFrames where
   parseJSON = A.withText  "PDebuggerContinueToLocationTargetCallFrames"  $ \v -> do
      case v of
         "any" -> pure PDebuggerContinueToLocationTargetCallFramesAny
         "current" -> pure PDebuggerContinueToLocationTargetCallFramesCurrent
         _ -> fail "failed to parse PDebuggerContinueToLocationTargetCallFrames"

instance ToJSON PDebuggerContinueToLocationTargetCallFrames where
   toJSON v = A.String $
      case v of
         PDebuggerContinueToLocationTargetCallFramesAny -> "any"
         PDebuggerContinueToLocationTargetCallFramesCurrent -> "current"



data PDebuggerContinueToLocation = PDebuggerContinueToLocation {
  -- | Location to continue to.
  pDebuggerContinueToLocationLocation :: DebuggerLocation,
  pDebuggerContinueToLocationTargetCallFrames :: PDebuggerContinueToLocationTargetCallFrames
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerContinueToLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerContinueToLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PDebuggerContinueToLocation where
   type CommandResponse PDebuggerContinueToLocation = ()
   commandName _ = "Debugger.continueToLocation"
   fromJSON = const . A.Success . const ()


-- | Debugger.disable
--   Disables debugger for given page.

-- | Parameters of the 'Debugger.disable' command.
data PDebuggerDisable = PDebuggerDisable
instance ToJSON PDebuggerDisable where toJSON _ = A.Null

instance Command PDebuggerDisable where
   type CommandResponse PDebuggerDisable = ()
   commandName _ = "Debugger.disable"
   fromJSON = const . A.Success . const ()


-- | Debugger.enable
--   Enables debugger for the given page. Clients should not assume that the debugging has been
--   enabled until the result for this command is received.

-- | Parameters of the 'Debugger.enable' command.
data PDebuggerEnable = PDebuggerEnable {
  -- | The maximum size in bytes of collected scripts (not referenced by other heap objects)
  --   the debugger can hold. Puts no limit if parameter is omitted.
  pDebuggerEnableMaxScriptsCacheSize :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'Debugger.enable' command.
data DebuggerEnable = DebuggerEnable {
  -- | Unique identifier of the debugger.
  debuggerEnableDebuggerId :: Runtime.RuntimeUniqueDebuggerId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDebuggerEnable where
   type CommandResponse PDebuggerEnable = DebuggerEnable
   commandName _ = "Debugger.enable"



-- | Debugger.evaluateOnCallFrame
--   Evaluates expression on a given call frame.

-- | Parameters of the 'Debugger.evaluateOnCallFrame' command.
data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
  -- | Call frame identifier to evaluate on.
  pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
  -- | Expression to evaluate.
  pDebuggerEvaluateOnCallFrameExpression :: String,
  -- | String object group name to put result into (allows rapid releasing resulting object handles
  --   using `releaseObjectGroup`).
  pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEvaluateOnCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Return type of the 'Debugger.evaluateOnCallFrame' command.
data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
  -- | Object wrapper for the evaluation result.
  debuggerEvaluateOnCallFrameResult :: Runtime.RuntimeRemoteObject,
  -- | Exception details.
  debuggerEvaluateOnCallFrameExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PDebuggerEvaluateOnCallFrame where
   type CommandResponse PDebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame
   commandName _ = "Debugger.evaluateOnCallFrame"



-- | Debugger.getPossibleBreakpoints
--   Returns possible locations for breakpoint. scriptId in start and end range locations should be
--   the same.

-- | Parameters of the 'Debugger.getPossibleBreakpoints' command.
data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
  -- | Start of range to search possible breakpoint locations in.
  pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
  -- | End of range to search possible breakpoint locations in (excluding). When not specified, end
  --   of scripts is used as end of range.
  pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
  -- | Only consider locations which are in the same (non-nested) function as start.
  pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetPossibleBreakpoints  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Return type of the 'Debugger.getPossibleBreakpoints' command.
data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
  -- | List of the possible breakpoint locations.
  debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command PDebuggerGetPossibleBreakpoints where
   type CommandResponse PDebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints
   commandName _ = "Debugger.getPossibleBreakpoints"



-- | Debugger.getScriptSource
--   Returns source for the script with given id.

-- | Parameters of the 'Debugger.getScriptSource' command.
data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
  -- | Id of the script to get source for.
  pDebuggerGetScriptSourceScriptId :: Runtime.RuntimeScriptId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Debugger.getScriptSource' command.
data DebuggerGetScriptSource = DebuggerGetScriptSource {
  -- | Script source (empty in case of Wasm bytecode).
  debuggerGetScriptSourceScriptSource :: String,
  -- | Wasm bytecode. (Encoded as a base64 string when passed over JSON)
  debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PDebuggerGetScriptSource where
   type CommandResponse PDebuggerGetScriptSource = DebuggerGetScriptSource
   commandName _ = "Debugger.getScriptSource"



-- | Debugger.disassembleWasmModule

-- | Parameters of the 'Debugger.disassembleWasmModule' command.
data PDebuggerDisassembleWasmModule = PDebuggerDisassembleWasmModule {
  -- | Id of the script to disassemble
  pDebuggerDisassembleWasmModuleScriptId :: Runtime.RuntimeScriptId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerDisassembleWasmModule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PDebuggerDisassembleWasmModule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Return type of the 'Debugger.disassembleWasmModule' command.
data DebuggerDisassembleWasmModule = DebuggerDisassembleWasmModule {
  -- | For large modules, return a stream from which additional chunks of
  --   disassembly can be read successively.
  debuggerDisassembleWasmModuleStreamId :: Maybe String,
  -- | The total number of lines in the disassembly text.
  debuggerDisassembleWasmModuleTotalNumberOfLines :: Int,
  -- | The offsets of all function bodies, in the format [start1, end1,
  --   start2, end2, ...] where all ends are exclusive.
  debuggerDisassembleWasmModuleFunctionBodyOffsets :: [Int],
  -- | The first chunk of disassembly.
  debuggerDisassembleWasmModuleChunk :: DebuggerWasmDisassemblyChunk
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerDisassembleWasmModule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PDebuggerDisassembleWasmModule where
   type CommandResponse PDebuggerDisassembleWasmModule = DebuggerDisassembleWasmModule
   commandName _ = "Debugger.disassembleWasmModule"



-- | Debugger.nextWasmDisassemblyChunk
--   Disassemble the next chunk of lines for the module corresponding to the
--   stream. If disassembly is complete, this API will invalidate the streamId
--   and return an empty chunk. Any subsequent calls for the now invalid stream
--   will return errors.

-- | Parameters of the 'Debugger.nextWasmDisassemblyChunk' command.
data PDebuggerNextWasmDisassemblyChunk = PDebuggerNextWasmDisassemblyChunk {
  pDebuggerNextWasmDisassemblyChunkStreamId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerNextWasmDisassemblyChunk  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PDebuggerNextWasmDisassemblyChunk where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Return type of the 'Debugger.nextWasmDisassemblyChunk' command.
data DebuggerNextWasmDisassemblyChunk = DebuggerNextWasmDisassemblyChunk {
  -- | The next chunk of disassembly.
  debuggerNextWasmDisassemblyChunkChunk :: DebuggerWasmDisassemblyChunk
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerNextWasmDisassemblyChunk where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance Command PDebuggerNextWasmDisassemblyChunk where
   type CommandResponse PDebuggerNextWasmDisassemblyChunk = DebuggerNextWasmDisassemblyChunk
   commandName _ = "Debugger.nextWasmDisassemblyChunk"



-- | Debugger.getStackTrace
--   Returns stack trace with given `stackTraceId`.

-- | Parameters of the 'Debugger.getStackTrace' command.
data PDebuggerGetStackTrace = PDebuggerGetStackTrace {
  pDebuggerGetStackTraceStackTraceId :: Runtime.RuntimeStackTraceId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetStackTrace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'Debugger.getStackTrace' command.
data DebuggerGetStackTrace = DebuggerGetStackTrace {
  debuggerGetStackTraceStackTrace :: Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PDebuggerGetStackTrace where
   type CommandResponse PDebuggerGetStackTrace = DebuggerGetStackTrace
   commandName _ = "Debugger.getStackTrace"



-- | Debugger.pause
--   Stops on the next JavaScript statement.

-- | Parameters of the 'Debugger.pause' command.
data PDebuggerPause = PDebuggerPause
instance ToJSON PDebuggerPause where toJSON _ = A.Null

instance Command PDebuggerPause where
   type CommandResponse PDebuggerPause = ()
   commandName _ = "Debugger.pause"
   fromJSON = const . A.Success . const ()


-- | Debugger.removeBreakpoint
--   Removes JavaScript breakpoint.

-- | Parameters of the 'Debugger.removeBreakpoint' command.
data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
  pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerRemoveBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerRemoveBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PDebuggerRemoveBreakpoint where
   type CommandResponse PDebuggerRemoveBreakpoint = ()
   commandName _ = "Debugger.removeBreakpoint"
   fromJSON = const . A.Success . const ()


-- | Debugger.restartFrame
--   Restarts particular call frame from the beginning. The old, deprecated
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
   parseJSON = A.withText  "PDebuggerRestartFrameMode"  $ \v -> do
      case v of
         "StepInto" -> pure PDebuggerRestartFrameModeStepInto
         _ -> fail "failed to parse PDebuggerRestartFrameMode"

instance ToJSON PDebuggerRestartFrameMode where
   toJSON v = A.String $
      case v of
         PDebuggerRestartFrameModeStepInto -> "StepInto"



data PDebuggerRestartFrame = PDebuggerRestartFrame {
  -- | Call frame identifier to evaluate on.
  pDebuggerRestartFrameCallFrameId :: DebuggerCallFrameId,
  -- | The `mode` parameter must be present and set to 'StepInto', otherwise
  --   `restartFrame` will error out.
  pDebuggerRestartFrameMode :: PDebuggerRestartFrameMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerRestartFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDebuggerRestartFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command PDebuggerRestartFrame where
   type CommandResponse PDebuggerRestartFrame = ()
   commandName _ = "Debugger.restartFrame"
   fromJSON = const . A.Success . const ()


-- | Debugger.resume
--   Resumes JavaScript execution.

-- | Parameters of the 'Debugger.resume' command.
data PDebuggerResume = PDebuggerResume {
  -- | Set to true to terminate execution upon resuming execution. In contrast
  --   to Runtime.terminateExecution, this will allows to execute further
  --   JavaScript (i.e. via evaluation) until execution of the paused code
  --   is actually resumed, at which point termination is triggered.
  --   If execution is currently not paused, this parameter has no effect.
  pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerResume  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerResume where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Command PDebuggerResume where
   type CommandResponse PDebuggerResume = ()
   commandName _ = "Debugger.resume"
   fromJSON = const . A.Success . const ()


-- | Debugger.searchInContent
--   Searches for given string in script content.

-- | Parameters of the 'Debugger.searchInContent' command.
data PDebuggerSearchInContent = PDebuggerSearchInContent {
  -- | Id of the script to search in.
  pDebuggerSearchInContentScriptId :: Runtime.RuntimeScriptId,
  -- | String to search for.
  pDebuggerSearchInContentQuery :: String,
  -- | If true, search is case sensitive.
  pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
  -- | If true, treats string parameter as regex.
  pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSearchInContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Debugger.searchInContent' command.
data DebuggerSearchInContent = DebuggerSearchInContent {
  -- | List of search matches.
  debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PDebuggerSearchInContent where
   type CommandResponse PDebuggerSearchInContent = DebuggerSearchInContent
   commandName _ = "Debugger.searchInContent"



-- | Debugger.setAsyncCallStackDepth
--   Enables or disables async call stacks tracking.

-- | Parameters of the 'Debugger.setAsyncCallStackDepth' command.
data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
  -- | Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
  --   call stacks (default).
  pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PDebuggerSetAsyncCallStackDepth where
   type CommandResponse PDebuggerSetAsyncCallStackDepth = ()
   commandName _ = "Debugger.setAsyncCallStackDepth"
   fromJSON = const . A.Success . const ()


-- | Debugger.setBlackboxPatterns
--   Replace previous blackbox patterns with passed ones. Forces backend to skip stepping/pausing in
--   scripts with url matching one of the patterns. VM will try to leave blackboxed script by
--   performing 'step in' several times, finally resorting to 'step out' if unsuccessful.

-- | Parameters of the 'Debugger.setBlackboxPatterns' command.
data PDebuggerSetBlackboxPatterns = PDebuggerSetBlackboxPatterns {
  -- | Array of regexps that will be used to check script url for blackbox state.
  pDebuggerSetBlackboxPatternsPatterns :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBlackboxPatterns  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBlackboxPatterns where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PDebuggerSetBlackboxPatterns where
   type CommandResponse PDebuggerSetBlackboxPatterns = ()
   commandName _ = "Debugger.setBlackboxPatterns"
   fromJSON = const . A.Success . const ()


-- | Debugger.setBlackboxedRanges
--   Makes backend skip steps in the script in blackboxed ranges. VM will try leave blacklisted
--   scripts by performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
--   Positions array contains positions where blackbox state is changed. First interval isn't
--   blackboxed. Array should be sorted.

-- | Parameters of the 'Debugger.setBlackboxedRanges' command.
data PDebuggerSetBlackboxedRanges = PDebuggerSetBlackboxedRanges {
  -- | Id of the script.
  pDebuggerSetBlackboxedRangesScriptId :: Runtime.RuntimeScriptId,
  pDebuggerSetBlackboxedRangesPositions :: [DebuggerScriptPosition]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBlackboxedRanges  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBlackboxedRanges where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PDebuggerSetBlackboxedRanges where
   type CommandResponse PDebuggerSetBlackboxedRanges = ()
   commandName _ = "Debugger.setBlackboxedRanges"
   fromJSON = const . A.Success . const ()


-- | Debugger.setBreakpoint
--   Sets JavaScript breakpoint at a given location.

-- | Parameters of the 'Debugger.setBreakpoint' command.
data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
  -- | Location to set breakpoint in.
  pDebuggerSetBreakpointLocation :: DebuggerLocation,
  -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
  --   breakpoint if this expression evaluates to true.
  pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'Debugger.setBreakpoint' command.
data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
  -- | Id of the created breakpoint for further reference.
  debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
  -- | Location this breakpoint resolved into.
  debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PDebuggerSetBreakpoint where
   type CommandResponse PDebuggerSetBreakpoint = DebuggerSetBreakpoint
   commandName _ = "Debugger.setBreakpoint"



-- | Debugger.setInstrumentationBreakpoint
--   Sets instrumentation breakpoint.

-- | Parameters of the 'Debugger.setInstrumentationBreakpoint' command.
data PDebuggerSetInstrumentationBreakpointInstrumentation = PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution | PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
   parseJSON = A.withText  "PDebuggerSetInstrumentationBreakpointInstrumentation"  $ \v -> do
      case v of
         "beforeScriptExecution" -> pure PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution
         "beforeScriptWithSourceMapExecution" -> pure PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution
         _ -> fail "failed to parse PDebuggerSetInstrumentationBreakpointInstrumentation"

instance ToJSON PDebuggerSetInstrumentationBreakpointInstrumentation where
   toJSON v = A.String $
      case v of
         PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptExecution -> "beforeScriptExecution"
         PDebuggerSetInstrumentationBreakpointInstrumentationBeforeScriptWithSourceMapExecution -> "beforeScriptWithSourceMapExecution"



data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint {
  -- | Instrumentation name.
  pDebuggerSetInstrumentationBreakpointInstrumentation :: PDebuggerSetInstrumentationBreakpointInstrumentation
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Return type of the 'Debugger.setInstrumentationBreakpoint' command.
data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
  -- | Id of the created breakpoint for further reference.
  debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PDebuggerSetInstrumentationBreakpoint where
   type CommandResponse PDebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint
   commandName _ = "Debugger.setInstrumentationBreakpoint"



-- | Debugger.setBreakpointByUrl
--   Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
--   command is issued, all existing parsed scripts will have breakpoints resolved and returned in
--   `locations` property. Further matching script parsing will result in subsequent
--   `breakpointResolved` events issued. This logical breakpoint will survive page reloads.

-- | Parameters of the 'Debugger.setBreakpointByUrl' command.
data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
  -- | Line number to set breakpoint at.
  pDebuggerSetBreakpointByUrlLineNumber :: Int,
  -- | URL of the resources to set breakpoint on.
  pDebuggerSetBreakpointByUrlUrl :: Maybe String,
  -- | Regex pattern for the URLs of the resources to set breakpoints on. Either `url` or
  --   `urlRegex` must be specified.
  pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
  -- | Script hash of the resources to set breakpoint on.
  pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
  -- | Offset in the line to set breakpoint at.
  pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
  -- | Expression to use as a breakpoint condition. When specified, debugger will only stop on the
  --   breakpoint if this expression evaluates to true.
  pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointByUrl  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Return type of the 'Debugger.setBreakpointByUrl' command.
data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
  -- | Id of the created breakpoint for further reference.
  debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
  -- | List of the locations this breakpoint resolved into upon addition.
  debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command PDebuggerSetBreakpointByUrl where
   type CommandResponse PDebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl
   commandName _ = "Debugger.setBreakpointByUrl"



-- | Debugger.setBreakpointOnFunctionCall
--   Sets JavaScript breakpoint before each call to the given function.
--   If another function was created from the same source as a given one,
--   calling it will also trigger the breakpoint.

-- | Parameters of the 'Debugger.setBreakpointOnFunctionCall' command.
data PDebuggerSetBreakpointOnFunctionCall = PDebuggerSetBreakpointOnFunctionCall {
  -- | Function object id.
  pDebuggerSetBreakpointOnFunctionCallObjectId :: Runtime.RuntimeRemoteObjectId,
  -- | Expression to use as a breakpoint condition. When specified, debugger will
  --   stop on the breakpoint if this expression evaluates to true.
  pDebuggerSetBreakpointOnFunctionCallCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointOnFunctionCall  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointOnFunctionCall where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Return type of the 'Debugger.setBreakpointOnFunctionCall' command.
data DebuggerSetBreakpointOnFunctionCall = DebuggerSetBreakpointOnFunctionCall {
  -- | Id of the created breakpoint for further reference.
  debuggerSetBreakpointOnFunctionCallBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointOnFunctionCall where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command PDebuggerSetBreakpointOnFunctionCall where
   type CommandResponse PDebuggerSetBreakpointOnFunctionCall = DebuggerSetBreakpointOnFunctionCall
   commandName _ = "Debugger.setBreakpointOnFunctionCall"



-- | Debugger.setBreakpointsActive
--   Activates / deactivates all breakpoints on the page.

-- | Parameters of the 'Debugger.setBreakpointsActive' command.
data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
  -- | New value for breakpoints active state.
  pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointsActive  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointsActive where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PDebuggerSetBreakpointsActive where
   type CommandResponse PDebuggerSetBreakpointsActive = ()
   commandName _ = "Debugger.setBreakpointsActive"
   fromJSON = const . A.Success . const ()


-- | Debugger.setPauseOnExceptions
--   Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or
--   no exceptions. Initial pause on exceptions state is `none`.

-- | Parameters of the 'Debugger.setPauseOnExceptions' command.
data PDebuggerSetPauseOnExceptionsState = PDebuggerSetPauseOnExceptionsStateNone | PDebuggerSetPauseOnExceptionsStateUncaught | PDebuggerSetPauseOnExceptionsStateAll
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDebuggerSetPauseOnExceptionsState where
   parseJSON = A.withText  "PDebuggerSetPauseOnExceptionsState"  $ \v -> do
      case v of
         "none" -> pure PDebuggerSetPauseOnExceptionsStateNone
         "uncaught" -> pure PDebuggerSetPauseOnExceptionsStateUncaught
         "all" -> pure PDebuggerSetPauseOnExceptionsStateAll
         _ -> fail "failed to parse PDebuggerSetPauseOnExceptionsState"

instance ToJSON PDebuggerSetPauseOnExceptionsState where
   toJSON v = A.String $
      case v of
         PDebuggerSetPauseOnExceptionsStateNone -> "none"
         PDebuggerSetPauseOnExceptionsStateUncaught -> "uncaught"
         PDebuggerSetPauseOnExceptionsStateAll -> "all"



data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions {
  -- | Pause on exceptions mode.
  pDebuggerSetPauseOnExceptionsState :: PDebuggerSetPauseOnExceptionsState
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetPauseOnExceptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetPauseOnExceptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PDebuggerSetPauseOnExceptions where
   type CommandResponse PDebuggerSetPauseOnExceptions = ()
   commandName _ = "Debugger.setPauseOnExceptions"
   fromJSON = const . A.Success . const ()


-- | Debugger.setReturnValue
--   Changes return value in top frame. Available only at return break position.

-- | Parameters of the 'Debugger.setReturnValue' command.
data PDebuggerSetReturnValue = PDebuggerSetReturnValue {
  -- | New return value.
  pDebuggerSetReturnValueNewValue :: Runtime.RuntimeCallArgument
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetReturnValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetReturnValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command PDebuggerSetReturnValue where
   type CommandResponse PDebuggerSetReturnValue = ()
   commandName _ = "Debugger.setReturnValue"
   fromJSON = const . A.Success . const ()


-- | Debugger.setScriptSource
--   Edits JavaScript source live.
--   
--   In general, functions that are currently on the stack can not be edited with
--   a single exception: If the edited function is the top-most stack frame and
--   that is the only activation of that function on the stack. In this case
--   the live edit will be successful and a `Debugger.restartFrame` for the
--   top-most function is automatically triggered.

-- | Parameters of the 'Debugger.setScriptSource' command.
data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
  -- | Id of the script to edit.
  pDebuggerSetScriptSourceScriptId :: Runtime.RuntimeScriptId,
  -- | New content of the script.
  pDebuggerSetScriptSourceScriptSource :: String,
  -- | If true the change will not actually be applied. Dry run may be used to get result
  --   description without actually modifying the code.
  pDebuggerSetScriptSourceDryRun :: Maybe Bool,
  -- | If true, then `scriptSource` is allowed to change the function on top of the stack
  --   as long as the top-most stack frame is the only activation of that function.
  pDebuggerSetScriptSourceAllowTopFrameEditing :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Debugger.setScriptSource' command.
data DebuggerSetScriptSource = DebuggerSetScriptSource {
  -- | Whether the operation was successful or not. Only `Ok` denotes a
  --   successful live edit while the other enum variants denote why
  --   the live edit failed.
  debuggerSetScriptSourceStatus :: String,
  -- | Exception details if any. Only present when `status` is `CompileError`.
  debuggerSetScriptSourceExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PDebuggerSetScriptSource where
   type CommandResponse PDebuggerSetScriptSource = DebuggerSetScriptSource
   commandName _ = "Debugger.setScriptSource"



-- | Debugger.setSkipAllPauses
--   Makes page not interrupt on any pauses (breakpoint, exception, dom exception etc).

-- | Parameters of the 'Debugger.setSkipAllPauses' command.
data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
  -- | New value for skip pauses state.
  pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetSkipAllPauses  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetSkipAllPauses where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PDebuggerSetSkipAllPauses where
   type CommandResponse PDebuggerSetSkipAllPauses = ()
   commandName _ = "Debugger.setSkipAllPauses"
   fromJSON = const . A.Success . const ()


-- | Debugger.setVariableValue
--   Changes value of variable in a callframe. Object-based scopes are not supported and must be
--   mutated manually.

-- | Parameters of the 'Debugger.setVariableValue' command.
data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
  -- | 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'
  --   scope types are allowed. Other scopes could be manipulated manually.
  pDebuggerSetVariableValueScopeNumber :: Int,
  -- | Variable name.
  pDebuggerSetVariableValueVariableName :: String,
  -- | New variable value.
  pDebuggerSetVariableValueNewValue :: Runtime.RuntimeCallArgument,
  -- | Id of callframe that holds variable.
  pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetVariableValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetVariableValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PDebuggerSetVariableValue where
   type CommandResponse PDebuggerSetVariableValue = ()
   commandName _ = "Debugger.setVariableValue"
   fromJSON = const . A.Success . const ()


-- | Debugger.stepInto
--   Steps into the function call.

-- | Parameters of the 'Debugger.stepInto' command.
data PDebuggerStepInto = PDebuggerStepInto {
  -- | Debugger will pause on the execution of the first async task which was scheduled
  --   before next pause.
  pDebuggerStepIntoBreakOnAsyncCall :: Maybe Bool,
  -- | The skipList specifies location ranges that should be skipped on step into.
  pDebuggerStepIntoSkipList :: Maybe [DebuggerLocationRange]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerStepInto  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDebuggerStepInto where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PDebuggerStepInto where
   type CommandResponse PDebuggerStepInto = ()
   commandName _ = "Debugger.stepInto"
   fromJSON = const . A.Success . const ()


-- | Debugger.stepOut
--   Steps out of the function call.

-- | Parameters of the 'Debugger.stepOut' command.
data PDebuggerStepOut = PDebuggerStepOut
instance ToJSON PDebuggerStepOut where toJSON _ = A.Null

instance Command PDebuggerStepOut where
   type CommandResponse PDebuggerStepOut = ()
   commandName _ = "Debugger.stepOut"
   fromJSON = const . A.Success . const ()


-- | Debugger.stepOver
--   Steps over the statement.

-- | Parameters of the 'Debugger.stepOver' command.
data PDebuggerStepOver = PDebuggerStepOver {
  -- | The skipList specifies location ranges that should be skipped on step over.
  pDebuggerStepOverSkipList :: Maybe [DebuggerLocationRange]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerStepOver  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDebuggerStepOver where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PDebuggerStepOver where
   type CommandResponse PDebuggerStepOver = ()
   commandName _ = "Debugger.stepOver"
   fromJSON = const . A.Success . const ()



