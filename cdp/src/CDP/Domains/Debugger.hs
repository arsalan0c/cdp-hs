{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.Runtime as Runtime


-- | Breakpoint identifier.
type DebuggerBreakpointId = String

-- | Call frame identifier.
type DebuggerCallFrameId = String

-- | Location in the source code.
data DebuggerLocation = DebuggerLocation {
   debuggerLocationScriptId :: DebuggerLocationScriptId, -- ^ Script identifier as reported in the `Debugger.scriptParsed`.
   debuggerLocationLineNumber :: DebuggerLocationLineNumber, -- ^ Line number in the script (0-based).
   debuggerLocationColumnNumber :: DebuggerLocationColumnNumber -- ^ Column number in the script (0-based).
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Location in the source code.
data DebuggerScriptPosition = DebuggerScriptPosition {


} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptPosition  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptPosition where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Location range within one script.
data DebuggerLocationRange = DebuggerLocationRange {



} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocationRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocationRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | JavaScript call frame. Array of call frames form the call stack.
data DebuggerCallFrame = DebuggerCallFrame {
   debuggerCallFrameCallFrameId :: DebuggerCallFrameCallFrameId, -- ^ Call frame identifier. This identifier is only valid while the virtual machine is paused.
   debuggerCallFrameFunctionName :: DebuggerCallFrameFunctionName, -- ^ Name of the JavaScript function called on this call frame.
   debuggerCallFrameFunctionLocation :: DebuggerCallFrameFunctionLocation, -- ^ Location in the source code.
   debuggerCallFrameLocation :: DebuggerCallFrameLocation, -- ^ Location in the source code.
   debuggerCallFrameScopeChain :: DebuggerCallFrameScopeChain, -- ^ Scope chain for this call frame.
   debuggerCallFrameThis :: DebuggerCallFrameThis, -- ^ `this` object for this call frame.
   debuggerCallFrameReturnValue :: DebuggerCallFrameReturnValue, -- ^ The value being returned, if the function is at return point.
   debuggerCallFrameCanBeRestarted :: DebuggerCallFrameCanBeRestarted -- ^ Valid only while the VM is paused and indicates whether this frame
can be restarted or not. Note that a `true` value here does not
guarantee that Debugger#restartFrame with this CallFrameId will be
successful, but it is very likely.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  DebuggerCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Scope description.
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
   debuggerScopeType :: DebuggerScopeType, -- ^ Scope type.
   debuggerScopeObject :: DebuggerScopeObject, -- ^ Object representing the scope. For `global` and `with` scopes it represents the actual
object; for the rest of the scopes, it is artificial transient object enumerating scope
variables as its properties.

   debuggerScopeStartLocation :: DebuggerScopeStartLocation, -- ^ Location in the source code where scope starts
   debuggerScopeEndLocation :: DebuggerScopeEndLocation -- ^ Location in the source code where scope ends
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScope  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DebuggerScope where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Search match for resource.
data DebuggerSearchMatch = DebuggerSearchMatch {
   debuggerSearchMatchLineNumber :: DebuggerSearchMatchLineNumber, -- ^ Line number in resource content.
   debuggerSearchMatchLineContent :: DebuggerSearchMatchLineContent -- ^ Line with match content.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerSearchMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DebuggerSearchMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Debugger.BreakLocation' .
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
   debuggerBreakLocationScriptId :: DebuggerBreakLocationScriptId, -- ^ Script identifier as reported in the `Debugger.scriptParsed`.
   debuggerBreakLocationLineNumber :: DebuggerBreakLocationLineNumber, -- ^ Line number in the script (0-based).
   debuggerBreakLocationColumnNumber :: DebuggerBreakLocationColumnNumber, -- ^ Column number in the script (0-based).

} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Enum of possible script languages.
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



-- | Debug symbols available for a wasm script.
data DebuggerDebugSymbolsType = DebuggerDebugSymbolsTypeNone | DebuggerDebugSymbolsTypeSourceMap | DebuggerDebugSymbolsTypeEmbeddedDwarf | DebuggerDebugSymbolsTypeExternalDwarf
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerDebugSymbolsType where
   parseJSON = A.withText  "DebuggerDebugSymbolsType"  $ \v -> do
      case v of
         "None" -> pure DebuggerDebugSymbolsTypeNone
         "SourceMap" -> pure DebuggerDebugSymbolsTypeSourceMap
         "EmbeddedDWARF" -> pure DebuggerDebugSymbolsTypeEmbeddedDwarf
         "ExternalDWARF" -> pure DebuggerDebugSymbolsTypeExternalDwarf
         _ -> fail "failed to parse DebuggerDebugSymbolsType"

instance ToJSON DebuggerDebugSymbolsType where
   toJSON v = A.String $
      case v of
         DebuggerDebugSymbolsTypeNone -> "None"
         DebuggerDebugSymbolsTypeSourceMap -> "SourceMap"
         DebuggerDebugSymbolsTypeEmbeddedDwarf -> "EmbeddedDWARF"
         DebuggerDebugSymbolsTypeExternalDwarf -> "ExternalDWARF"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
   debuggerDebugSymbolsType :: DebuggerDebugSymbolsType, -- ^ Type of the debug symbols.
   debuggerDebugSymbolsExternalUrl :: DebuggerDebugSymbolsExternalUrl -- ^ URL of the external symbol source.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerDebugSymbols  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerDebugSymbols where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Type of the 'Debugger.breakpointResolved' event.
data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
   debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointResolvedBreakpointId, -- ^ Breakpoint unique identifier.
   debuggerBreakpointResolvedLocation :: DebuggerBreakpointResolvedLocation -- ^ Actual breakpoint location.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakpointResolved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakpointResolved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Debugger.paused' event.
data DebuggerPausedReason = DebuggerPausedReasonAmbiguous | DebuggerPausedReasonAssert | DebuggerPausedReasonCspViolation | DebuggerPausedReasonDebugCommand | DebuggerPausedReasonDom | DebuggerPausedReasonEventListener | DebuggerPausedReasonException | DebuggerPausedReasonInstrumentation | DebuggerPausedReasonOom | DebuggerPausedReasonOther | DebuggerPausedReasonPromiseRejection | DebuggerPausedReasonXhr
   deriving (Ord, Eq, Show, Read)
instance FromJSON DebuggerPausedReason where
   parseJSON = A.withText  "DebuggerPausedReason"  $ \v -> do
      case v of
         "ambiguous" -> pure DebuggerPausedReasonAmbiguous
         "assert" -> pure DebuggerPausedReasonAssert
         "CSPViolation" -> pure DebuggerPausedReasonCspViolation
         "debugCommand" -> pure DebuggerPausedReasonDebugCommand
         "DOM" -> pure DebuggerPausedReasonDom
         "EventListener" -> pure DebuggerPausedReasonEventListener
         "exception" -> pure DebuggerPausedReasonException
         "instrumentation" -> pure DebuggerPausedReasonInstrumentation
         "OOM" -> pure DebuggerPausedReasonOom
         "other" -> pure DebuggerPausedReasonOther
         "promiseRejection" -> pure DebuggerPausedReasonPromiseRejection
         "XHR" -> pure DebuggerPausedReasonXhr
         _ -> fail "failed to parse DebuggerPausedReason"

instance ToJSON DebuggerPausedReason where
   toJSON v = A.String $
      case v of
         DebuggerPausedReasonAmbiguous -> "ambiguous"
         DebuggerPausedReasonAssert -> "assert"
         DebuggerPausedReasonCspViolation -> "CSPViolation"
         DebuggerPausedReasonDebugCommand -> "debugCommand"
         DebuggerPausedReasonDom -> "DOM"
         DebuggerPausedReasonEventListener -> "EventListener"
         DebuggerPausedReasonException -> "exception"
         DebuggerPausedReasonInstrumentation -> "instrumentation"
         DebuggerPausedReasonOom -> "OOM"
         DebuggerPausedReasonOther -> "other"
         DebuggerPausedReasonPromiseRejection -> "promiseRejection"
         DebuggerPausedReasonXhr -> "XHR"



data DebuggerPaused = DebuggerPaused {
   debuggerPausedCallFrames :: DebuggerPausedCallFrames, -- ^ Call stack the virtual machine stopped on.
   debuggerPausedReason :: DebuggerPausedReason, -- ^ Pause reason.
   debuggerPausedData :: DebuggerPausedData, -- ^ Object containing break-specific auxiliary properties.
   debuggerPausedHitBreakpoints :: DebuggerPausedHitBreakpoints, -- ^ Hit breakpoints IDs
   debuggerPausedAsyncStackTrace :: DebuggerPausedAsyncStackTrace, -- ^ Async stack trace, if any.
   debuggerPausedAsyncStackTraceId :: DebuggerPausedAsyncStackTraceId -- ^ Async stack trace, if any.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DebuggerPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type of the 'Debugger.resumed' event.
data DebuggerResumed = DebuggerResumed
   deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
   parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
      case v of
         "DebuggerResumed" -> pure DebuggerResumed
         _ -> fail "failed to parse DebuggerResumed"



-- | Type of the 'Debugger.scriptFailedToParse' event.
data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
   debuggerScriptFailedToParseScriptId :: DebuggerScriptFailedToParseScriptId, -- ^ Identifier of the script parsed.
   debuggerScriptFailedToParseUrl :: DebuggerScriptFailedToParseUrl, -- ^ URL or name of the script parsed (if any).
   debuggerScriptFailedToParseStartLine :: DebuggerScriptFailedToParseStartLine, -- ^ Line offset of the script within the resource with given URL (for script tags).
   debuggerScriptFailedToParseStartColumn :: DebuggerScriptFailedToParseStartColumn, -- ^ Column offset of the script within the resource with given URL.
   debuggerScriptFailedToParseEndLine :: DebuggerScriptFailedToParseEndLine, -- ^ Last line of the script.
   debuggerScriptFailedToParseEndColumn :: DebuggerScriptFailedToParseEndColumn, -- ^ Length of the last line of the script.
   debuggerScriptFailedToParseExecutionContextId :: DebuggerScriptFailedToParseExecutionContextId, -- ^ Specifies script creation context.
   debuggerScriptFailedToParseHash :: DebuggerScriptFailedToParseHash, -- ^ Content hash of the script, SHA-256.
   debuggerScriptFailedToParseExecutionContextAuxData :: DebuggerScriptFailedToParseExecutionContextAuxData, -- ^ Embedder-specific auxiliary data.
   debuggerScriptFailedToParseSourceMapUrl :: DebuggerScriptFailedToParseSourceMapUrl, -- ^ URL of source map associated with script (if any).
   debuggerScriptFailedToParseHasSourceUrl :: DebuggerScriptFailedToParseHasSourceUrl, -- ^ True, if this script has sourceURL.
   debuggerScriptFailedToParseIsModule :: DebuggerScriptFailedToParseIsModule, -- ^ True, if this script is ES6 module.
   debuggerScriptFailedToParseLength :: DebuggerScriptFailedToParseLength, -- ^ This script length.
   debuggerScriptFailedToParseStackTrace :: DebuggerScriptFailedToParseStackTrace, -- ^ JavaScript top stack frame of where the script parsed event was triggered if available.
   debuggerScriptFailedToParseCodeOffset :: DebuggerScriptFailedToParseCodeOffset, -- ^ If the scriptLanguage is WebAssembly, the code section offset in the module.
   debuggerScriptFailedToParseScriptLanguage :: DebuggerScriptFailedToParseScriptLanguage, -- ^ The language of the script.
   debuggerScriptFailedToParseEmbedderName :: DebuggerScriptFailedToParseEmbedderName -- ^ The name the embedder supplied for this script.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptFailedToParse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptFailedToParse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Debugger.scriptParsed' event.
data DebuggerScriptParsed = DebuggerScriptParsed {
   debuggerScriptParsedScriptId :: DebuggerScriptParsedScriptId, -- ^ Identifier of the script parsed.
   debuggerScriptParsedUrl :: DebuggerScriptParsedUrl, -- ^ URL or name of the script parsed (if any).
   debuggerScriptParsedStartLine :: DebuggerScriptParsedStartLine, -- ^ Line offset of the script within the resource with given URL (for script tags).
   debuggerScriptParsedStartColumn :: DebuggerScriptParsedStartColumn, -- ^ Column offset of the script within the resource with given URL.
   debuggerScriptParsedEndLine :: DebuggerScriptParsedEndLine, -- ^ Last line of the script.
   debuggerScriptParsedEndColumn :: DebuggerScriptParsedEndColumn, -- ^ Length of the last line of the script.
   debuggerScriptParsedExecutionContextId :: DebuggerScriptParsedExecutionContextId, -- ^ Specifies script creation context.
   debuggerScriptParsedHash :: DebuggerScriptParsedHash, -- ^ Content hash of the script, SHA-256.
   debuggerScriptParsedExecutionContextAuxData :: DebuggerScriptParsedExecutionContextAuxData, -- ^ Embedder-specific auxiliary data.
   debuggerScriptParsedIsLiveEdit :: DebuggerScriptParsedIsLiveEdit, -- ^ True, if this script is generated as a result of the live edit operation.
   debuggerScriptParsedSourceMapUrl :: DebuggerScriptParsedSourceMapUrl, -- ^ URL of source map associated with script (if any).
   debuggerScriptParsedHasSourceUrl :: DebuggerScriptParsedHasSourceUrl, -- ^ True, if this script has sourceURL.
   debuggerScriptParsedIsModule :: DebuggerScriptParsedIsModule, -- ^ True, if this script is ES6 module.
   debuggerScriptParsedLength :: DebuggerScriptParsedLength, -- ^ This script length.
   debuggerScriptParsedStackTrace :: DebuggerScriptParsedStackTrace, -- ^ JavaScript top stack frame of where the script parsed event was triggered if available.
   debuggerScriptParsedCodeOffset :: DebuggerScriptParsedCodeOffset, -- ^ If the scriptLanguage is WebAssembly, the code section offset in the module.
   debuggerScriptParsedScriptLanguage :: DebuggerScriptParsedScriptLanguage, -- ^ The language of the script.
   debuggerScriptParsedDebugSymbols :: DebuggerScriptParsedDebugSymbols, -- ^ If the scriptLanguage is WebASsembly, the source of debug symbols for the module.
   debuggerScriptParsedEmbedderName :: DebuggerScriptParsedEmbedderName -- ^ The name the embedder supplied for this script.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Parameters of the 'debuggerContinueToLocation' command.
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
   pDebuggerContinueToLocationLocation :: PDebuggerContinueToLocationLocation, -- ^ Location to continue to.

} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerContinueToLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerContinueToLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Debugger.continueToLocation'.
-- Continues execution until specific location is reached.
-- Parameters: 'PDebuggerContinueToLocation'
debuggerContinueToLocation :: Handle ev -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation handle params = sendReceiveCommand handle "Debugger.continueToLocation" (Just params)


-- | Function for the command 'Debugger.disable'.
-- Disables debugger for given page.
debuggerDisable :: Handle ev -> IO (Maybe Error)
debuggerDisable handle = sendReceiveCommand handle "Debugger.disable" (Nothing :: Maybe ())


-- | Parameters of the 'debuggerEnable' command.
data PDebuggerEnable = PDebuggerEnable {
   pDebuggerEnableMaxScriptsCacheSize :: PDebuggerEnableMaxScriptsCacheSize -- ^ The maximum size in bytes of collected scripts (not referenced by other heap objects)
the debugger can hold. Puts no limit if parameter is omitted.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'Debugger.enable'.
-- Enables debugger for the given page. Clients should not assume that the debugging has been
-- enabled until the result for this command is received.
-- Parameters: 'PDebuggerEnable'
-- Returns: 'DebuggerEnable'
debuggerEnable :: Handle ev -> PDebuggerEnable -> IO (Either Error DebuggerEnable)
debuggerEnable handle params = sendReceiveCommandResult handle "Debugger.enable" (Just params)

-- | Return type of the 'debuggerEnable' command.
data DebuggerEnable = DebuggerEnable {
   debuggerEnableDebuggerId :: Runtime.RuntimeUniqueDebuggerId -- ^ Unique identifier of the debugger.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DebuggerEnable where
   commandName _ = "Debugger.enable"



-- | Parameters of the 'debuggerEvaluateOnCallFrame' command.
data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
   pDebuggerEvaluateOnCallFrameCallFrameId :: PDebuggerEvaluateOnCallFrameCallFrameId, -- ^ Call frame identifier to evaluate on.
   pDebuggerEvaluateOnCallFrameExpression :: PDebuggerEvaluateOnCallFrameExpression, -- ^ Expression to evaluate.
   pDebuggerEvaluateOnCallFrameObjectGroup :: PDebuggerEvaluateOnCallFrameObjectGroup, -- ^ String object group name to put result into (allows rapid releasing resulting object handles
using `releaseObjectGroup`).
   pDebuggerEvaluateOnCallFrameIncludeCommandLineApi :: PDebuggerEvaluateOnCallFrameIncludeCommandLineApi, -- ^ Specifies whether command line API should be available to the evaluated expression, defaults
to false.
   pDebuggerEvaluateOnCallFrameSilent :: PDebuggerEvaluateOnCallFrameSilent, -- ^ In silent mode exceptions thrown during evaluation are not reported and do not pause
execution. Overrides `setPauseOnException` state.
   pDebuggerEvaluateOnCallFrameReturnByValue :: PDebuggerEvaluateOnCallFrameReturnByValue, -- ^ Whether the result is expected to be a JSON object that should be sent by value.
   pDebuggerEvaluateOnCallFrameGeneratePreview :: PDebuggerEvaluateOnCallFrameGeneratePreview, -- ^ Whether preview should be generated for the result.
   pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: PDebuggerEvaluateOnCallFrameThrowOnSideEffect, -- ^ Whether to throw an exception if side effect cannot be ruled out during evaluation.
   pDebuggerEvaluateOnCallFrameTimeout :: PDebuggerEvaluateOnCallFrameTimeout -- ^ Terminate execution after timing out (number of milliseconds).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEvaluateOnCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Debugger.evaluateOnCallFrame'.
-- Evaluates expression on a given call frame.
-- Parameters: 'PDebuggerEvaluateOnCallFrame'
-- Returns: 'DebuggerEvaluateOnCallFrame'
debuggerEvaluateOnCallFrame :: Handle ev -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame handle params = sendReceiveCommandResult handle "Debugger.evaluateOnCallFrame" (Just params)

-- | Return type of the 'debuggerEvaluateOnCallFrame' command.
data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
   debuggerEvaluateOnCallFrameResult :: Runtime.RuntimeRemoteObject, -- ^ Object wrapper for the evaluation result.
   debuggerEvaluateOnCallFrameExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DebuggerEvaluateOnCallFrame where
   commandName _ = "Debugger.evaluateOnCallFrame"



-- | Parameters of the 'debuggerGetPossibleBreakpoints' command.
data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
   pDebuggerGetPossibleBreakpointsStart :: PDebuggerGetPossibleBreakpointsStart, -- ^ Start of range to search possible breakpoint locations in.
   pDebuggerGetPossibleBreakpointsEnd :: PDebuggerGetPossibleBreakpointsEnd, -- ^ End of range to search possible breakpoint locations in (excluding). When not specified, end
of scripts is used as end of range.
   pDebuggerGetPossibleBreakpointsRestrictToFunction :: PDebuggerGetPossibleBreakpointsRestrictToFunction -- ^ Only consider locations which are in the same (non-nested) function as start.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetPossibleBreakpoints  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Debugger.getPossibleBreakpoints'.
-- Returns possible locations for breakpoint. scriptId in start and end range locations should be
-- the same.
-- Parameters: 'PDebuggerGetPossibleBreakpoints'
-- Returns: 'DebuggerGetPossibleBreakpoints'
debuggerGetPossibleBreakpoints :: Handle ev -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints handle params = sendReceiveCommandResult handle "Debugger.getPossibleBreakpoints" (Just params)

-- | Return type of the 'debuggerGetPossibleBreakpoints' command.
data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
   debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation] -- ^ List of the possible breakpoint locations.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command DebuggerGetPossibleBreakpoints where
   commandName _ = "Debugger.getPossibleBreakpoints"



-- | Parameters of the 'debuggerGetScriptSource' command.
data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
   pDebuggerGetScriptSourceScriptId :: PDebuggerGetScriptSourceScriptId -- ^ Id of the script to get source for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Debugger.getScriptSource'.
-- Returns source for the script with given id.
-- Parameters: 'PDebuggerGetScriptSource'
-- Returns: 'DebuggerGetScriptSource'
debuggerGetScriptSource :: Handle ev -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource handle params = sendReceiveCommandResult handle "Debugger.getScriptSource" (Just params)

-- | Return type of the 'debuggerGetScriptSource' command.
data DebuggerGetScriptSource = DebuggerGetScriptSource {
   debuggerGetScriptSourceScriptSource :: String, -- ^ Script source (empty in case of Wasm bytecode).
   debuggerGetScriptSourceBytecode :: Maybe String -- ^ Wasm bytecode. (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerGetScriptSource where
   commandName _ = "Debugger.getScriptSource"



-- | Parameters of the 'debuggerGetStackTrace' command.
data PDebuggerGetStackTrace = PDebuggerGetStackTrace {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetStackTrace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Debugger.getStackTrace'.
-- Returns stack trace with given `stackTraceId`.
-- Parameters: 'PDebuggerGetStackTrace'
-- Returns: 'DebuggerGetStackTrace'
debuggerGetStackTrace :: Handle ev -> PDebuggerGetStackTrace -> IO (Either Error DebuggerGetStackTrace)
debuggerGetStackTrace handle params = sendReceiveCommandResult handle "Debugger.getStackTrace" (Just params)

-- | Return type of the 'debuggerGetStackTrace' command.
data DebuggerGetStackTrace = DebuggerGetStackTrace {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DebuggerGetStackTrace where
   commandName _ = "Debugger.getStackTrace"



-- | Function for the command 'Debugger.pause'.
-- Stops on the next JavaScript statement.
debuggerPause :: Handle ev -> IO (Maybe Error)
debuggerPause handle = sendReceiveCommand handle "Debugger.pause" (Nothing :: Maybe ())


-- | Parameters of the 'debuggerRemoveBreakpoint' command.
data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerRemoveBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerRemoveBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Debugger.removeBreakpoint'.
-- Removes JavaScript breakpoint.
-- Parameters: 'PDebuggerRemoveBreakpoint'
debuggerRemoveBreakpoint :: Handle ev -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint handle params = sendReceiveCommand handle "Debugger.removeBreakpoint" (Just params)


-- | Parameters of the 'debuggerResume' command.
data PDebuggerResume = PDebuggerResume {
   pDebuggerResumeTerminateOnResume :: PDebuggerResumeTerminateOnResume -- ^ Set to true to terminate execution upon resuming execution. In contrast
to Runtime.terminateExecution, this will allows to execute further
JavaScript (i.e. via evaluation) until execution of the paused code
is actually resumed, at which point termination is triggered.
If execution is currently not paused, this parameter has no effect.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerResume  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerResume where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'Debugger.resume'.
-- Resumes JavaScript execution.
-- Parameters: 'PDebuggerResume'
debuggerResume :: Handle ev -> PDebuggerResume -> IO (Maybe Error)
debuggerResume handle params = sendReceiveCommand handle "Debugger.resume" (Just params)


-- | Parameters of the 'debuggerSearchInContent' command.
data PDebuggerSearchInContent = PDebuggerSearchInContent {
   pDebuggerSearchInContentScriptId :: PDebuggerSearchInContentScriptId, -- ^ Id of the script to search in.
   pDebuggerSearchInContentQuery :: PDebuggerSearchInContentQuery, -- ^ String to search for.
   pDebuggerSearchInContentCaseSensitive :: PDebuggerSearchInContentCaseSensitive, -- ^ If true, search is case sensitive.
   pDebuggerSearchInContentIsRegex :: PDebuggerSearchInContentIsRegex -- ^ If true, treats string parameter as regex.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSearchInContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Debugger.searchInContent'.
-- Searches for given string in script content.
-- Parameters: 'PDebuggerSearchInContent'
-- Returns: 'DebuggerSearchInContent'
debuggerSearchInContent :: Handle ev -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent handle params = sendReceiveCommandResult handle "Debugger.searchInContent" (Just params)

-- | Return type of the 'debuggerSearchInContent' command.
data DebuggerSearchInContent = DebuggerSearchInContent {
   debuggerSearchInContentResult :: [DebuggerSearchMatch] -- ^ List of search matches.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSearchInContent where
   commandName _ = "Debugger.searchInContent"



-- | Parameters of the 'debuggerSetAsyncCallStackDepth' command.
data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
   pDebuggerSetAsyncCallStackDepthMaxDepth :: PDebuggerSetAsyncCallStackDepthMaxDepth -- ^ Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
call stacks (default).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Debugger.setAsyncCallStackDepth'.
-- Enables or disables async call stacks tracking.
-- Parameters: 'PDebuggerSetAsyncCallStackDepth'
debuggerSetAsyncCallStackDepth :: Handle ev -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Debugger.setAsyncCallStackDepth" (Just params)


-- | Parameters of the 'debuggerSetBlackboxPatterns' command.
data PDebuggerSetBlackboxPatterns = PDebuggerSetBlackboxPatterns {
   pDebuggerSetBlackboxPatternsPatterns :: PDebuggerSetBlackboxPatternsPatterns -- ^ Array of regexps that will be used to check script url for blackbox state.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBlackboxPatterns  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBlackboxPatterns where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Debugger.setBlackboxPatterns'.
-- Replace previous blackbox patterns with passed ones. Forces backend to skip stepping/pausing in
-- scripts with url matching one of the patterns. VM will try to leave blackboxed script by
-- performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
-- Parameters: 'PDebuggerSetBlackboxPatterns'
debuggerSetBlackboxPatterns :: Handle ev -> PDebuggerSetBlackboxPatterns -> IO (Maybe Error)
debuggerSetBlackboxPatterns handle params = sendReceiveCommand handle "Debugger.setBlackboxPatterns" (Just params)


-- | Parameters of the 'debuggerSetBlackboxedRanges' command.
data PDebuggerSetBlackboxedRanges = PDebuggerSetBlackboxedRanges {
   pDebuggerSetBlackboxedRangesScriptId :: PDebuggerSetBlackboxedRangesScriptId, -- ^ Id of the script.

} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBlackboxedRanges  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBlackboxedRanges where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Debugger.setBlackboxedRanges'.
-- Makes backend skip steps in the script in blackboxed ranges. VM will try leave blacklisted
-- scripts by performing 'step in' several times, finally resorting to 'step out' if unsuccessful.
-- Positions array contains positions where blackbox state is changed. First interval isn't
-- blackboxed. Array should be sorted.
-- Parameters: 'PDebuggerSetBlackboxedRanges'
debuggerSetBlackboxedRanges :: Handle ev -> PDebuggerSetBlackboxedRanges -> IO (Maybe Error)
debuggerSetBlackboxedRanges handle params = sendReceiveCommand handle "Debugger.setBlackboxedRanges" (Just params)


-- | Parameters of the 'debuggerSetBreakpoint' command.
data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
   pDebuggerSetBreakpointLocation :: PDebuggerSetBreakpointLocation, -- ^ Location to set breakpoint in.
   pDebuggerSetBreakpointCondition :: PDebuggerSetBreakpointCondition -- ^ Expression to use as a breakpoint condition. When specified, debugger will only stop on the
breakpoint if this expression evaluates to true.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Debugger.setBreakpoint'.
-- Sets JavaScript breakpoint at a given location.
-- Parameters: 'PDebuggerSetBreakpoint'
-- Returns: 'DebuggerSetBreakpoint'
debuggerSetBreakpoint :: Handle ev -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setBreakpoint" (Just params)

-- | Return type of the 'debuggerSetBreakpoint' command.
data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
   debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId, -- ^ Id of the created breakpoint for further reference.
   debuggerSetBreakpointActualLocation :: DebuggerLocation -- ^ Location this breakpoint resolved into.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DebuggerSetBreakpoint where
   commandName _ = "Debugger.setBreakpoint"



-- | Parameters of the 'debuggerSetInstrumentationBreakpoint' command.
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
   pDebuggerSetInstrumentationBreakpointInstrumentation :: PDebuggerSetInstrumentationBreakpointInstrumentation -- ^ Instrumentation name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the command 'Debugger.setInstrumentationBreakpoint'.
-- Sets instrumentation breakpoint.
-- Parameters: 'PDebuggerSetInstrumentationBreakpoint'
-- Returns: 'DebuggerSetInstrumentationBreakpoint'
debuggerSetInstrumentationBreakpoint :: Handle ev -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setInstrumentationBreakpoint" (Just params)

-- | Return type of the 'debuggerSetInstrumentationBreakpoint' command.
data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
   debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId -- ^ Id of the created breakpoint for further reference.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command DebuggerSetInstrumentationBreakpoint where
   commandName _ = "Debugger.setInstrumentationBreakpoint"



-- | Parameters of the 'debuggerSetBreakpointByUrl' command.
data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
   pDebuggerSetBreakpointByUrlLineNumber :: PDebuggerSetBreakpointByUrlLineNumber, -- ^ Line number to set breakpoint at.
   pDebuggerSetBreakpointByUrlUrl :: PDebuggerSetBreakpointByUrlUrl, -- ^ URL of the resources to set breakpoint on.
   pDebuggerSetBreakpointByUrlUrlRegex :: PDebuggerSetBreakpointByUrlUrlRegex, -- ^ Regex pattern for the URLs of the resources to set breakpoints on. Either `url` or
`urlRegex` must be specified.
   pDebuggerSetBreakpointByUrlScriptHash :: PDebuggerSetBreakpointByUrlScriptHash, -- ^ Script hash of the resources to set breakpoint on.
   pDebuggerSetBreakpointByUrlColumnNumber :: PDebuggerSetBreakpointByUrlColumnNumber, -- ^ Offset in the line to set breakpoint at.
   pDebuggerSetBreakpointByUrlCondition :: PDebuggerSetBreakpointByUrlCondition -- ^ Expression to use as a breakpoint condition. When specified, debugger will only stop on the
breakpoint if this expression evaluates to true.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointByUrl  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Debugger.setBreakpointByUrl'.
-- Sets JavaScript breakpoint at given location specified either by URL or URL regex. Once this
-- command is issued, all existing parsed scripts will have breakpoints resolved and returned in
-- `locations` property. Further matching script parsing will result in subsequent
-- `breakpointResolved` events issued. This logical breakpoint will survive page reloads.
-- Parameters: 'PDebuggerSetBreakpointByUrl'
-- Returns: 'DebuggerSetBreakpointByUrl'
debuggerSetBreakpointByUrl :: Handle ev -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl handle params = sendReceiveCommandResult handle "Debugger.setBreakpointByUrl" (Just params)

-- | Return type of the 'debuggerSetBreakpointByUrl' command.
data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
   debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId, -- ^ Id of the created breakpoint for further reference.
   debuggerSetBreakpointByUrlLocations :: [DebuggerLocation] -- ^ List of the locations this breakpoint resolved into upon addition.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DebuggerSetBreakpointByUrl where
   commandName _ = "Debugger.setBreakpointByUrl"



-- | Parameters of the 'debuggerSetBreakpointOnFunctionCall' command.
data PDebuggerSetBreakpointOnFunctionCall = PDebuggerSetBreakpointOnFunctionCall {
   pDebuggerSetBreakpointOnFunctionCallObjectId :: PDebuggerSetBreakpointOnFunctionCallObjectId, -- ^ Function object id.
   pDebuggerSetBreakpointOnFunctionCallCondition :: PDebuggerSetBreakpointOnFunctionCallCondition -- ^ Expression to use as a breakpoint condition. When specified, debugger will
stop on the breakpoint if this expression evaluates to true.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointOnFunctionCall  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointOnFunctionCall where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the command 'Debugger.setBreakpointOnFunctionCall'.
-- Sets JavaScript breakpoint before each call to the given function.
-- If another function was created from the same source as a given one,
-- calling it will also trigger the breakpoint.
-- Parameters: 'PDebuggerSetBreakpointOnFunctionCall'
-- Returns: 'DebuggerSetBreakpointOnFunctionCall'
debuggerSetBreakpointOnFunctionCall :: Handle ev -> PDebuggerSetBreakpointOnFunctionCall -> IO (Either Error DebuggerSetBreakpointOnFunctionCall)
debuggerSetBreakpointOnFunctionCall handle params = sendReceiveCommandResult handle "Debugger.setBreakpointOnFunctionCall" (Just params)

-- | Return type of the 'debuggerSetBreakpointOnFunctionCall' command.
data DebuggerSetBreakpointOnFunctionCall = DebuggerSetBreakpointOnFunctionCall {
   debuggerSetBreakpointOnFunctionCallBreakpointId :: DebuggerBreakpointId -- ^ Id of the created breakpoint for further reference.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointOnFunctionCall where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }

instance Command DebuggerSetBreakpointOnFunctionCall where
   commandName _ = "Debugger.setBreakpointOnFunctionCall"



-- | Parameters of the 'debuggerSetBreakpointsActive' command.
data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
   pDebuggerSetBreakpointsActiveActive :: PDebuggerSetBreakpointsActiveActive -- ^ New value for breakpoints active state.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointsActive  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointsActive where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Debugger.setBreakpointsActive'.
-- Activates / deactivates all breakpoints on the page.
-- Parameters: 'PDebuggerSetBreakpointsActive'
debuggerSetBreakpointsActive :: Handle ev -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive handle params = sendReceiveCommand handle "Debugger.setBreakpointsActive" (Just params)


-- | Parameters of the 'debuggerSetPauseOnExceptions' command.
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
   pDebuggerSetPauseOnExceptionsState :: PDebuggerSetPauseOnExceptionsState -- ^ Pause on exceptions mode.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetPauseOnExceptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetPauseOnExceptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Debugger.setPauseOnExceptions'.
-- Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or
-- no exceptions. Initial pause on exceptions state is `none`.
-- Parameters: 'PDebuggerSetPauseOnExceptions'
debuggerSetPauseOnExceptions :: Handle ev -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions handle params = sendReceiveCommand handle "Debugger.setPauseOnExceptions" (Just params)


-- | Parameters of the 'debuggerSetReturnValue' command.
data PDebuggerSetReturnValue = PDebuggerSetReturnValue {
   pDebuggerSetReturnValueNewValue :: PDebuggerSetReturnValueNewValue -- ^ New return value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetReturnValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetReturnValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Debugger.setReturnValue'.
-- Changes return value in top frame. Available only at return break position.
-- Parameters: 'PDebuggerSetReturnValue'
debuggerSetReturnValue :: Handle ev -> PDebuggerSetReturnValue -> IO (Maybe Error)
debuggerSetReturnValue handle params = sendReceiveCommand handle "Debugger.setReturnValue" (Just params)


-- | Parameters of the 'debuggerSetScriptSource' command.
data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
   pDebuggerSetScriptSourceScriptId :: PDebuggerSetScriptSourceScriptId, -- ^ Id of the script to edit.
   pDebuggerSetScriptSourceScriptSource :: PDebuggerSetScriptSourceScriptSource, -- ^ New content of the script.
   pDebuggerSetScriptSourceDryRun :: PDebuggerSetScriptSourceDryRun -- ^ If true the change will not actually be applied. Dry run may be used to get result
description without actually modifying the code.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Debugger.setScriptSource'.
-- Edits JavaScript source live.
-- Parameters: 'PDebuggerSetScriptSource'
-- Returns: 'DebuggerSetScriptSource'
debuggerSetScriptSource :: Handle ev -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource handle params = sendReceiveCommandResult handle "Debugger.setScriptSource" (Just params)

-- | Return type of the 'debuggerSetScriptSource' command.
data DebuggerSetScriptSource = DebuggerSetScriptSource {
   debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame], -- ^ New stack trace in case editing has happened while VM was stopped.
   debuggerSetScriptSourceStackChanged :: Maybe Bool, -- ^ Whether current call stack  was modified after applying the changes.
   debuggerSetScriptSourceAsyncStackTrace :: Maybe Runtime.RuntimeStackTrace, -- ^ Async stack trace, if any.
   debuggerSetScriptSourceAsyncStackTraceId :: Maybe Runtime.RuntimeStackTraceId, -- ^ Async stack trace, if any.
   debuggerSetScriptSourceExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails -- ^ Exception details if any.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSetScriptSource where
   commandName _ = "Debugger.setScriptSource"



-- | Parameters of the 'debuggerSetSkipAllPauses' command.
data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
   pDebuggerSetSkipAllPausesSkip :: PDebuggerSetSkipAllPausesSkip -- ^ New value for skip pauses state.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetSkipAllPauses  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetSkipAllPauses where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Debugger.setSkipAllPauses'.
-- Makes page not interrupt on any pauses (breakpoint, exception, dom exception etc).
-- Parameters: 'PDebuggerSetSkipAllPauses'
debuggerSetSkipAllPauses :: Handle ev -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses handle params = sendReceiveCommand handle "Debugger.setSkipAllPauses" (Just params)


-- | Parameters of the 'debuggerSetVariableValue' command.
data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
   pDebuggerSetVariableValueScopeNumber :: PDebuggerSetVariableValueScopeNumber, -- ^ 0-based number of scope as was listed in scope chain. Only 'local', 'closure' and 'catch'
scope types are allowed. Other scopes could be manipulated manually.
   pDebuggerSetVariableValueVariableName :: PDebuggerSetVariableValueVariableName, -- ^ Variable name.
   pDebuggerSetVariableValueNewValue :: PDebuggerSetVariableValueNewValue, -- ^ New variable value.
   pDebuggerSetVariableValueCallFrameId :: PDebuggerSetVariableValueCallFrameId -- ^ Id of callframe that holds variable.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetVariableValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetVariableValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Debugger.setVariableValue'.
-- Changes value of variable in a callframe. Object-based scopes are not supported and must be
-- mutated manually.
-- Parameters: 'PDebuggerSetVariableValue'
debuggerSetVariableValue :: Handle ev -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue handle params = sendReceiveCommand handle "Debugger.setVariableValue" (Just params)


-- | Parameters of the 'debuggerStepInto' command.
data PDebuggerStepInto = PDebuggerStepInto {
   pDebuggerStepIntoBreakOnAsyncCall :: PDebuggerStepIntoBreakOnAsyncCall, -- ^ Debugger will pause on the execution of the first async task which was scheduled
before next pause.
   pDebuggerStepIntoSkipList :: PDebuggerStepIntoSkipList -- ^ The skipList specifies location ranges that should be skipped on step into.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerStepInto  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDebuggerStepInto where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Debugger.stepInto'.
-- Steps into the function call.
-- Parameters: 'PDebuggerStepInto'
debuggerStepInto :: Handle ev -> PDebuggerStepInto -> IO (Maybe Error)
debuggerStepInto handle params = sendReceiveCommand handle "Debugger.stepInto" (Just params)


-- | Function for the command 'Debugger.stepOut'.
-- Steps out of the function call.
debuggerStepOut :: Handle ev -> IO (Maybe Error)
debuggerStepOut handle = sendReceiveCommand handle "Debugger.stepOut" (Nothing :: Maybe ())


-- | Parameters of the 'debuggerStepOver' command.
data PDebuggerStepOver = PDebuggerStepOver {
   pDebuggerStepOverSkipList :: PDebuggerStepOverSkipList -- ^ The skipList specifies location ranges that should be skipped on step over.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerStepOver  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDebuggerStepOver where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Debugger.stepOver'.
-- Steps over the statement.
-- Parameters: 'PDebuggerStepOver'
debuggerStepOver :: Handle ev -> PDebuggerStepOver -> IO (Maybe Error)
debuggerStepOver handle params = sendReceiveCommand handle "Debugger.stepOver" (Just params)



