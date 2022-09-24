{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type DebuggerBreakpointId = String
type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
   debuggerLocationScriptId :: Runtime.RuntimeScriptId,
   debuggerLocationLineNumber :: Int,
   debuggerLocationColumnNumber :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DebuggerLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data DebuggerCallFrame = DebuggerCallFrame {
   debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
   debuggerCallFrameFunctionName :: String,
   debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
   debuggerCallFrameLocation :: DebuggerLocation,
   debuggerCallFrameScopeChain :: [DebuggerScope],
   debuggerCallFrameThis :: Runtime.RuntimeRemoteObject,
   debuggerCallFrameReturnValue :: Maybe Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  DebuggerCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


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
   debuggerScopeType :: DebuggerScopeType,
   debuggerScopeObject :: Runtime.RuntimeRemoteObject,
   debuggerScopeName :: Maybe String,
   debuggerScopeStartLocation :: Maybe DebuggerLocation,
   debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScope  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DebuggerScope where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



data DebuggerSearchMatch = DebuggerSearchMatch {
   debuggerSearchMatchLineNumber :: Double,
   debuggerSearchMatchLineContent :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerSearchMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DebuggerSearchMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


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
   debuggerBreakLocationScriptId :: Runtime.RuntimeScriptId,
   debuggerBreakLocationLineNumber :: Int,
   debuggerBreakLocationColumnNumber :: Maybe Int,
   debuggerBreakLocationType :: DebuggerBreakLocationType
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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
   debuggerDebugSymbolsType :: DebuggerDebugSymbolsType,
   debuggerDebugSymbolsExternalUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerDebugSymbols  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerDebugSymbols where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
   debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
   debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerBreakpointResolved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DebuggerBreakpointResolved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


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
   debuggerPausedCallFrames :: [DebuggerCallFrame],
   debuggerPausedReason :: DebuggerPausedReason,
   debuggerPausedData :: Maybe [(String, String)],
   debuggerPausedHitBreakpoints :: Maybe [String],
   debuggerPausedAsyncStackTrace :: Maybe Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DebuggerPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


data DebuggerResumed = DebuggerResumed
   deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
   parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
      case v of
         "DebuggerResumed" -> pure DebuggerResumed
         _ -> fail "failed to parse DebuggerResumed"



data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
   debuggerScriptFailedToParseScriptId :: Runtime.RuntimeScriptId,
   debuggerScriptFailedToParseUrl :: String,
   debuggerScriptFailedToParseStartLine :: Int,
   debuggerScriptFailedToParseStartColumn :: Int,
   debuggerScriptFailedToParseEndLine :: Int,
   debuggerScriptFailedToParseEndColumn :: Int,
   debuggerScriptFailedToParseExecutionContextId :: Runtime.RuntimeExecutionContextId,
   debuggerScriptFailedToParseHash :: String,
   debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(String, String)],
   debuggerScriptFailedToParseSourceMapUrl :: Maybe String,
   debuggerScriptFailedToParseHasSourceUrl :: Maybe Bool,
   debuggerScriptFailedToParseIsModule :: Maybe Bool,
   debuggerScriptFailedToParseLength :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptFailedToParse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptFailedToParse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data DebuggerScriptParsed = DebuggerScriptParsed {
   debuggerScriptParsedScriptId :: Runtime.RuntimeScriptId,
   debuggerScriptParsedUrl :: String,
   debuggerScriptParsedStartLine :: Int,
   debuggerScriptParsedStartColumn :: Int,
   debuggerScriptParsedEndLine :: Int,
   debuggerScriptParsedEndColumn :: Int,
   debuggerScriptParsedExecutionContextId :: Runtime.RuntimeExecutionContextId,
   debuggerScriptParsedHash :: String,
   debuggerScriptParsedExecutionContextAuxData :: Maybe [(String, String)],
   debuggerScriptParsedSourceMapUrl :: Maybe String,
   debuggerScriptParsedHasSourceUrl :: Maybe Bool,
   debuggerScriptParsedIsModule :: Maybe Bool,
   debuggerScriptParsedLength :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DebuggerScriptParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DebuggerScriptParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }




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
   pDebuggerContinueToLocationLocation :: DebuggerLocation,
   pDebuggerContinueToLocationTargetCallFrames :: PDebuggerContinueToLocationTargetCallFrames
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerContinueToLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerContinueToLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


debuggerContinueToLocation :: Handle ev -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation handle params = sendReceiveCommand handle "Debugger.continueToLocation" (Just params)


debuggerDisable :: Handle ev -> IO (Maybe Error)
debuggerDisable handle = sendReceiveCommand handle "Debugger.disable" (Nothing :: Maybe ())


debuggerEnable :: Handle ev -> IO (Maybe Error)
debuggerEnable handle = sendReceiveCommand handle "Debugger.enable" (Nothing :: Maybe ())



data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
   pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
   pDebuggerEvaluateOnCallFrameExpression :: String,
   pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
   pDebuggerEvaluateOnCallFrameIncludeCommandLineApi :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
   pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerEvaluateOnCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


debuggerEvaluateOnCallFrame :: Handle ev -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame handle params = sendReceiveCommandResult handle "Debugger.evaluateOnCallFrame" (Just params)

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
   debuggerEvaluateOnCallFrameResult :: Runtime.RuntimeRemoteObject,
   debuggerEvaluateOnCallFrameExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerEvaluateOnCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DebuggerEvaluateOnCallFrame where
   commandName _ = "Debugger.evaluateOnCallFrame"




data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
   pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
   pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
   pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetPossibleBreakpoints  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


debuggerGetPossibleBreakpoints :: Handle ev -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints handle params = sendReceiveCommandResult handle "Debugger.getPossibleBreakpoints" (Just params)

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
   debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetPossibleBreakpoints where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command DebuggerGetPossibleBreakpoints where
   commandName _ = "Debugger.getPossibleBreakpoints"




data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
   pDebuggerGetScriptSourceScriptId :: Runtime.RuntimeScriptId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerGetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerGetScriptSource :: Handle ev -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource handle params = sendReceiveCommandResult handle "Debugger.getScriptSource" (Just params)

data DebuggerGetScriptSource = DebuggerGetScriptSource {
   debuggerGetScriptSourceScriptSource :: String,
   debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerGetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerGetScriptSource where
   commandName _ = "Debugger.getScriptSource"



debuggerPause :: Handle ev -> IO (Maybe Error)
debuggerPause handle = sendReceiveCommand handle "Debugger.pause" (Nothing :: Maybe ())



data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
   pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerRemoveBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerRemoveBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerRemoveBreakpoint :: Handle ev -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint handle params = sendReceiveCommand handle "Debugger.removeBreakpoint" (Just params)



data PDebuggerResume = PDebuggerResume {
   pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerResume  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDebuggerResume where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


debuggerResume :: Handle ev -> PDebuggerResume -> IO (Maybe Error)
debuggerResume handle params = sendReceiveCommand handle "Debugger.resume" (Just params)



data PDebuggerSearchInContent = PDebuggerSearchInContent {
   pDebuggerSearchInContentScriptId :: Runtime.RuntimeScriptId,
   pDebuggerSearchInContentQuery :: String,
   pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
   pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSearchInContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerSearchInContent :: Handle ev -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent handle params = sendReceiveCommandResult handle "Debugger.searchInContent" (Just params)

data DebuggerSearchInContent = DebuggerSearchInContent {
   debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSearchInContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSearchInContent where
   commandName _ = "Debugger.searchInContent"




data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
   pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


debuggerSetAsyncCallStackDepth :: Handle ev -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Debugger.setAsyncCallStackDepth" (Just params)



data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
   pDebuggerSetBreakpointLocation :: DebuggerLocation,
   pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


debuggerSetBreakpoint :: Handle ev -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setBreakpoint" (Just params)

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
   debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
   debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DebuggerSetBreakpoint where
   commandName _ = "Debugger.setBreakpoint"



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
   pDebuggerSetInstrumentationBreakpointInstrumentation :: PDebuggerSetInstrumentationBreakpointInstrumentation
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


debuggerSetInstrumentationBreakpoint :: Handle ev -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint handle params = sendReceiveCommandResult handle "Debugger.setInstrumentationBreakpoint" (Just params)

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
   debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command DebuggerSetInstrumentationBreakpoint where
   commandName _ = "Debugger.setInstrumentationBreakpoint"




data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
   pDebuggerSetBreakpointByUrlLineNumber :: Int,
   pDebuggerSetBreakpointByUrlUrl :: Maybe String,
   pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
   pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
   pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
   pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointByUrl  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


debuggerSetBreakpointByUrl :: Handle ev -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl handle params = sendReceiveCommandResult handle "Debugger.setBreakpointByUrl" (Just params)

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
   debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
   debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetBreakpointByUrl where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DebuggerSetBreakpointByUrl where
   commandName _ = "Debugger.setBreakpointByUrl"




data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
   pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetBreakpointsActive  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetBreakpointsActive where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


debuggerSetBreakpointsActive :: Handle ev -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive handle params = sendReceiveCommand handle "Debugger.setBreakpointsActive" (Just params)


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
   pDebuggerSetPauseOnExceptionsState :: PDebuggerSetPauseOnExceptionsState
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetPauseOnExceptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetPauseOnExceptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


debuggerSetPauseOnExceptions :: Handle ev -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions handle params = sendReceiveCommand handle "Debugger.setPauseOnExceptions" (Just params)



data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
   pDebuggerSetScriptSourceScriptId :: Runtime.RuntimeScriptId,
   pDebuggerSetScriptSourceScriptSource :: String,
   pDebuggerSetScriptSourceDryRun :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetScriptSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


debuggerSetScriptSource :: Handle ev -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource handle params = sendReceiveCommandResult handle "Debugger.setScriptSource" (Just params)

data DebuggerSetScriptSource = DebuggerSetScriptSource {
   debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
   debuggerSetScriptSourceStackChanged :: Maybe Bool,
   debuggerSetScriptSourceAsyncStackTrace :: Maybe Runtime.RuntimeStackTrace,
   debuggerSetScriptSourceExceptionDetails :: Maybe Runtime.RuntimeExceptionDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DebuggerSetScriptSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command DebuggerSetScriptSource where
   commandName _ = "Debugger.setScriptSource"




data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
   pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetSkipAllPauses  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetSkipAllPauses where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerSetSkipAllPauses :: Handle ev -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses handle params = sendReceiveCommand handle "Debugger.setSkipAllPauses" (Just params)



data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
   pDebuggerSetVariableValueScopeNumber :: Int,
   pDebuggerSetVariableValueVariableName :: String,
   pDebuggerSetVariableValueNewValue :: Runtime.RuntimeCallArgument,
   pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDebuggerSetVariableValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PDebuggerSetVariableValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


debuggerSetVariableValue :: Handle ev -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue handle params = sendReceiveCommand handle "Debugger.setVariableValue" (Just params)


debuggerStepInto :: Handle ev -> IO (Maybe Error)
debuggerStepInto handle = sendReceiveCommand handle "Debugger.stepInto" (Nothing :: Maybe ())


debuggerStepOut :: Handle ev -> IO (Maybe Error)
debuggerStepOut handle = sendReceiveCommand handle "Debugger.stepOut" (Nothing :: Maybe ())


debuggerStepOver :: Handle ev -> IO (Maybe Error)
debuggerStepOver handle = sendReceiveCommand handle "Debugger.stepOver" (Nothing :: Maybe ())



