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

module Domains.Debugger (module Domains.Debugger) where

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
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


data DebuggerBreakpointResolved = DebuggerBreakpointResolved {
    debuggerBreakpointResolvedBreakpointId :: DebuggerBreakpointId,
    debuggerBreakpointResolvedLocation :: DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerBreakpointResolved where
    parseJSON = A.withObject "DebuggerBreakpointResolved" $ \v ->
         DebuggerBreakpointResolved <$> v .:  "breakpointId"
            <*> v  .:  "location"


instance ToJSON DebuggerBreakpointResolved  where
    toJSON v = A.object
        [ "breakpointId" .= debuggerBreakpointResolvedBreakpointId v
        , "location" .= debuggerBreakpointResolvedLocation v
        ]


instance FromEvent Event DebuggerBreakpointResolved where
    eventName  _ _    =  "Debugger.breakpointResolved"
    fromEvent ev =  case ev of EVDebuggerBreakpointResolved v -> Just v; _ -> Nothing

data DebuggerPaused = DebuggerPaused {
    debuggerPausedCallFrames :: [DebuggerCallFrame],
    debuggerPausedReason :: String,
    debuggerPausedData :: Maybe [(String, String)],
    debuggerPausedHitBreakpoints :: Maybe [String],
    debuggerPausedAsyncStackTrace :: Maybe RuntimeStackTrace
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerPaused where
    parseJSON = A.withObject "DebuggerPaused" $ \v ->
         DebuggerPaused <$> v .:  "callFrames"
            <*> v  .:  "reason"
            <*> v  .:?  "data"
            <*> v  .:?  "hitBreakpoints"
            <*> v  .:?  "asyncStackTrace"


instance ToJSON DebuggerPaused  where
    toJSON v = A.object
        [ "callFrames" .= debuggerPausedCallFrames v
        , "reason" .= debuggerPausedReason v
        , "data" .= debuggerPausedData v
        , "hitBreakpoints" .= debuggerPausedHitBreakpoints v
        , "asyncStackTrace" .= debuggerPausedAsyncStackTrace v
        ]


instance FromEvent Event DebuggerPaused where
    eventName  _ _    =  "Debugger.paused"
    fromEvent ev =  case ev of EVDebuggerPaused v -> Just v; _ -> Nothing

data DebuggerResumed = DebuggerResumed
    deriving (Eq, Show, Read)
instance FromJSON DebuggerResumed where
    parseJSON = A.withText  "DebuggerResumed"  $ \v -> do
        case v of
                "DebuggerResumed" -> pure $ DebuggerResumed
                _ -> fail "failed to parse DebuggerResumed"

instance FromEvent Event DebuggerResumed where
    eventName  _ _    =  "Debugger.resumed"
    fromEvent ev =  case ev of EVDebuggerResumed v -> Just v; _ -> Nothing

data DebuggerScriptFailedToParse = DebuggerScriptFailedToParse {
    debuggerScriptFailedToParseScriptId :: RuntimeScriptId,
    debuggerScriptFailedToParseUrl :: String,
    debuggerScriptFailedToParseStartLine :: Int,
    debuggerScriptFailedToParseStartColumn :: Int,
    debuggerScriptFailedToParseEndLine :: Int,
    debuggerScriptFailedToParseEndColumn :: Int,
    debuggerScriptFailedToParseExecutionContextId :: RuntimeExecutionContextId,
    debuggerScriptFailedToParseHash :: String,
    debuggerScriptFailedToParseExecutionContextAuxData :: Maybe [(String, String)],
    debuggerScriptFailedToParseSourceMapUrl :: Maybe String,
    debuggerScriptFailedToParseHasSourceUrl :: Maybe Bool,
    debuggerScriptFailedToParseIsModule :: Maybe Bool,
    debuggerScriptFailedToParseLength :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScriptFailedToParse where
    parseJSON = A.withObject "DebuggerScriptFailedToParse" $ \v ->
         DebuggerScriptFailedToParse <$> v .:  "scriptId"
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


instance ToJSON DebuggerScriptFailedToParse  where
    toJSON v = A.object
        [ "scriptId" .= debuggerScriptFailedToParseScriptId v
        , "url" .= debuggerScriptFailedToParseUrl v
        , "startLine" .= debuggerScriptFailedToParseStartLine v
        , "startColumn" .= debuggerScriptFailedToParseStartColumn v
        , "endLine" .= debuggerScriptFailedToParseEndLine v
        , "endColumn" .= debuggerScriptFailedToParseEndColumn v
        , "executionContextId" .= debuggerScriptFailedToParseExecutionContextId v
        , "hash" .= debuggerScriptFailedToParseHash v
        , "executionContextAuxData" .= debuggerScriptFailedToParseExecutionContextAuxData v
        , "sourceMapURL" .= debuggerScriptFailedToParseSourceMapUrl v
        , "hasSourceURL" .= debuggerScriptFailedToParseHasSourceUrl v
        , "isModule" .= debuggerScriptFailedToParseIsModule v
        , "length" .= debuggerScriptFailedToParseLength v
        ]


instance FromEvent Event DebuggerScriptFailedToParse where
    eventName  _ _    =  "Debugger.scriptFailedToParse"
    fromEvent ev =  case ev of EVDebuggerScriptFailedToParse v -> Just v; _ -> Nothing

data DebuggerScriptParsed = DebuggerScriptParsed {
    debuggerScriptParsedScriptId :: RuntimeScriptId,
    debuggerScriptParsedUrl :: String,
    debuggerScriptParsedStartLine :: Int,
    debuggerScriptParsedStartColumn :: Int,
    debuggerScriptParsedEndLine :: Int,
    debuggerScriptParsedEndColumn :: Int,
    debuggerScriptParsedExecutionContextId :: RuntimeExecutionContextId,
    debuggerScriptParsedHash :: String,
    debuggerScriptParsedExecutionContextAuxData :: Maybe [(String, String)],
    debuggerScriptParsedSourceMapUrl :: Maybe String,
    debuggerScriptParsedHasSourceUrl :: Maybe Bool,
    debuggerScriptParsedIsModule :: Maybe Bool,
    debuggerScriptParsedLength :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScriptParsed where
    parseJSON = A.withObject "DebuggerScriptParsed" $ \v ->
         DebuggerScriptParsed <$> v .:  "scriptId"
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


instance ToJSON DebuggerScriptParsed  where
    toJSON v = A.object
        [ "scriptId" .= debuggerScriptParsedScriptId v
        , "url" .= debuggerScriptParsedUrl v
        , "startLine" .= debuggerScriptParsedStartLine v
        , "startColumn" .= debuggerScriptParsedStartColumn v
        , "endLine" .= debuggerScriptParsedEndLine v
        , "endColumn" .= debuggerScriptParsedEndColumn v
        , "executionContextId" .= debuggerScriptParsedExecutionContextId v
        , "hash" .= debuggerScriptParsedHash v
        , "executionContextAuxData" .= debuggerScriptParsedExecutionContextAuxData v
        , "sourceMapURL" .= debuggerScriptParsedSourceMapUrl v
        , "hasSourceURL" .= debuggerScriptParsedHasSourceUrl v
        , "isModule" .= debuggerScriptParsedIsModule v
        , "length" .= debuggerScriptParsedLength v
        ]


instance FromEvent Event DebuggerScriptParsed where
    eventName  _ _    =  "Debugger.scriptParsed"
    fromEvent ev =  case ev of EVDebuggerScriptParsed v -> Just v; _ -> Nothing



type DebuggerBreakpointId = String

type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
    debuggerLocationScriptId :: RuntimeScriptId,
    debuggerLocationLineNumber :: Int,
    debuggerLocationColumnNumber :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerLocation where
    parseJSON = A.withObject "DebuggerLocation" $ \v ->
         DebuggerLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"


instance ToJSON DebuggerLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerLocationScriptId v
        , "lineNumber" .= debuggerLocationLineNumber v
        , "columnNumber" .= debuggerLocationColumnNumber v
        ]



data DebuggerCallFrame = DebuggerCallFrame {
    debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
    debuggerCallFrameFunctionName :: String,
    debuggerCallFrameLocation :: DebuggerLocation,
    debuggerCallFrameScopeChain :: [DebuggerScope],
    debuggerCallFrameThis :: RuntimeRemoteObject,
    debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
    debuggerCallFrameReturnValue :: Maybe RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerCallFrame where
    parseJSON = A.withObject "DebuggerCallFrame" $ \v ->
         DebuggerCallFrame <$> v .:  "callFrameId"
            <*> v  .:  "functionName"
            <*> v  .:  "location"
            <*> v  .:  "scopeChain"
            <*> v  .:  "this"
            <*> v  .:?  "functionLocation"
            <*> v  .:?  "returnValue"


instance ToJSON DebuggerCallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= debuggerCallFrameCallFrameId v
        , "functionName" .= debuggerCallFrameFunctionName v
        , "location" .= debuggerCallFrameLocation v
        , "scopeChain" .= debuggerCallFrameScopeChain v
        , "this" .= debuggerCallFrameThis v
        , "functionLocation" .= debuggerCallFrameFunctionLocation v
        , "returnValue" .= debuggerCallFrameReturnValue v
        ]



data DebuggerScope = DebuggerScope {
    debuggerScopeType :: String,
    debuggerScopeObject :: RuntimeRemoteObject,
    debuggerScopeName :: Maybe String,
    debuggerScopeStartLocation :: Maybe DebuggerLocation,
    debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerScope where
    parseJSON = A.withObject "DebuggerScope" $ \v ->
         DebuggerScope <$> v .:  "type"
            <*> v  .:  "object"
            <*> v  .:?  "name"
            <*> v  .:?  "startLocation"
            <*> v  .:?  "endLocation"


instance ToJSON DebuggerScope  where
    toJSON v = A.object
        [ "type" .= debuggerScopeType v
        , "object" .= debuggerScopeObject v
        , "name" .= debuggerScopeName v
        , "startLocation" .= debuggerScopeStartLocation v
        , "endLocation" .= debuggerScopeEndLocation v
        ]



data DebuggerSearchMatch = DebuggerSearchMatch {
    debuggerSearchMatchLineNumber :: Int,
    debuggerSearchMatchLineContent :: String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSearchMatch where
    parseJSON = A.withObject "DebuggerSearchMatch" $ \v ->
         DebuggerSearchMatch <$> v .:  "lineNumber"
            <*> v  .:  "lineContent"


instance ToJSON DebuggerSearchMatch  where
    toJSON v = A.object
        [ "lineNumber" .= debuggerSearchMatchLineNumber v
        , "lineContent" .= debuggerSearchMatchLineContent v
        ]



data DebuggerBreakLocation = DebuggerBreakLocation {
    debuggerBreakLocationScriptId :: RuntimeScriptId,
    debuggerBreakLocationLineNumber :: Int,
    debuggerBreakLocationColumnNumber :: Maybe Int,
    debuggerBreakLocationType :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerBreakLocation where
    parseJSON = A.withObject "DebuggerBreakLocation" $ \v ->
         DebuggerBreakLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "type"


instance ToJSON DebuggerBreakLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerBreakLocationScriptId v
        , "lineNumber" .= debuggerBreakLocationLineNumber v
        , "columnNumber" .= debuggerBreakLocationColumnNumber v
        , "type" .= debuggerBreakLocationType v
        ]



data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
    deriving (Eq, Show, Read)
instance FromJSON DebuggerScriptLanguage where
    parseJSON = A.withText  "DebuggerScriptLanguage"  $ \v -> do
        case v of
                "JavaScript" -> pure $ DebuggerScriptLanguageJavaScript
                "WebAssembly" -> pure $ DebuggerScriptLanguageWebAssembly
                _ -> fail "failed to parse DebuggerScriptLanguage"

instance ToJSON DebuggerScriptLanguage where
    toJSON v = A.String $
        case v of
                DebuggerScriptLanguageJavaScript -> "JavaScript"
                DebuggerScriptLanguageWebAssembly -> "WebAssembly"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
    debuggerDebugSymbolsType :: String,
    debuggerDebugSymbolsExternalUrl :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerDebugSymbols where
    parseJSON = A.withObject "DebuggerDebugSymbols" $ \v ->
         DebuggerDebugSymbols <$> v .:  "type"
            <*> v  .:?  "externalURL"


instance ToJSON DebuggerDebugSymbols  where
    toJSON v = A.object
        [ "type" .= debuggerDebugSymbolsType v
        , "externalURL" .= debuggerDebugSymbolsExternalUrl v
        ]





data PDebuggerContinueToLocation = PDebuggerContinueToLocation {
    pDebuggerContinueToLocationLocation :: DebuggerLocation,
    pDebuggerContinueToLocationTargetCallFrames :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerContinueToLocation where
    parseJSON = A.withObject "PDebuggerContinueToLocation" $ \v ->
         PDebuggerContinueToLocation <$> v .:  "location"
            <*> v  .:?  "targetCallFrames"


instance ToJSON PDebuggerContinueToLocation  where
    toJSON v = A.object
        [ "location" .= pDebuggerContinueToLocationLocation v
        , "targetCallFrames" .= pDebuggerContinueToLocationTargetCallFrames v
        ]


debuggerContinueToLocation :: Session -> PDebuggerContinueToLocation -> IO (Maybe Error)
debuggerContinueToLocation session params = sendReceiveCommand session "Debugger.continueToLocation" (Just params)




debuggerDisable :: Session -> IO (Maybe Error)
debuggerDisable session = sendReceiveCommand session "Debugger.disable" (Nothing :: Maybe ())




debuggerEnable :: Session -> IO (Maybe Error)
debuggerEnable session = sendReceiveCommand session "Debugger.enable" (Nothing :: Maybe ())

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
    debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "DebuggerEvaluateOnCallFrame" $ \v ->
         DebuggerEvaluateOnCallFrame <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



instance Command  DebuggerEvaluateOnCallFrame where
    commandName _ = "Debugger.evaluateOnCallFrame"

data PDebuggerEvaluateOnCallFrame = PDebuggerEvaluateOnCallFrame {
    pDebuggerEvaluateOnCallFrameCallFrameId :: DebuggerCallFrameId,
    pDebuggerEvaluateOnCallFrameExpression :: String,
    pDebuggerEvaluateOnCallFrameObjectGroup :: Maybe String,
    pDebuggerEvaluateOnCallFrameIncludeCommandLineApi :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameSilent :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameReturnByValue :: Maybe Bool,
    pDebuggerEvaluateOnCallFrameThrowOnSideEffect :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "PDebuggerEvaluateOnCallFrame" $ \v ->
         PDebuggerEvaluateOnCallFrame <$> v .:  "callFrameId"
            <*> v  .:  "expression"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "includeCommandLineAPI"
            <*> v  .:?  "silent"
            <*> v  .:?  "returnByValue"
            <*> v  .:?  "throwOnSideEffect"


instance ToJSON PDebuggerEvaluateOnCallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= pDebuggerEvaluateOnCallFrameCallFrameId v
        , "expression" .= pDebuggerEvaluateOnCallFrameExpression v
        , "objectGroup" .= pDebuggerEvaluateOnCallFrameObjectGroup v
        , "includeCommandLineAPI" .= pDebuggerEvaluateOnCallFrameIncludeCommandLineApi v
        , "silent" .= pDebuggerEvaluateOnCallFrameSilent v
        , "returnByValue" .= pDebuggerEvaluateOnCallFrameReturnByValue v
        , "throwOnSideEffect" .= pDebuggerEvaluateOnCallFrameThrowOnSideEffect v
        ]


debuggerEvaluateOnCallFrame :: Session -> PDebuggerEvaluateOnCallFrame -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame session params = sendReceiveCommandResult session "Debugger.evaluateOnCallFrame" (Just params)

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "DebuggerGetPossibleBreakpoints" $ \v ->
         DebuggerGetPossibleBreakpoints <$> v .:  "locations"



instance Command  DebuggerGetPossibleBreakpoints where
    commandName _ = "Debugger.getPossibleBreakpoints"

data PDebuggerGetPossibleBreakpoints = PDebuggerGetPossibleBreakpoints {
    pDebuggerGetPossibleBreakpointsStart :: DebuggerLocation,
    pDebuggerGetPossibleBreakpointsEnd :: Maybe DebuggerLocation,
    pDebuggerGetPossibleBreakpointsRestrictToFunction :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "PDebuggerGetPossibleBreakpoints" $ \v ->
         PDebuggerGetPossibleBreakpoints <$> v .:  "start"
            <*> v  .:?  "end"
            <*> v  .:?  "restrictToFunction"


instance ToJSON PDebuggerGetPossibleBreakpoints  where
    toJSON v = A.object
        [ "start" .= pDebuggerGetPossibleBreakpointsStart v
        , "end" .= pDebuggerGetPossibleBreakpointsEnd v
        , "restrictToFunction" .= pDebuggerGetPossibleBreakpointsRestrictToFunction v
        ]


debuggerGetPossibleBreakpoints :: Session -> PDebuggerGetPossibleBreakpoints -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints session params = sendReceiveCommandResult session "Debugger.getPossibleBreakpoints" (Just params)

data DebuggerGetScriptSource = DebuggerGetScriptSource {
    debuggerGetScriptSourceScriptSource :: String,
    debuggerGetScriptSourceBytecode :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerGetScriptSource where
    parseJSON = A.withObject "DebuggerGetScriptSource" $ \v ->
         DebuggerGetScriptSource <$> v .:  "scriptSource"
            <*> v  .:?  "bytecode"



instance Command  DebuggerGetScriptSource where
    commandName _ = "Debugger.getScriptSource"

data PDebuggerGetScriptSource = PDebuggerGetScriptSource {
    pDebuggerGetScriptSourceScriptId :: RuntimeScriptId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerGetScriptSource where
    parseJSON = A.withObject "PDebuggerGetScriptSource" $ \v ->
         PDebuggerGetScriptSource <$> v .:  "scriptId"


instance ToJSON PDebuggerGetScriptSource  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerGetScriptSourceScriptId v
        ]


debuggerGetScriptSource :: Session -> PDebuggerGetScriptSource -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource session params = sendReceiveCommandResult session "Debugger.getScriptSource" (Just params)




debuggerPause :: Session -> IO (Maybe Error)
debuggerPause session = sendReceiveCommand session "Debugger.pause" (Nothing :: Maybe ())



data PDebuggerRemoveBreakpoint = PDebuggerRemoveBreakpoint {
    pDebuggerRemoveBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerRemoveBreakpoint where
    parseJSON = A.withObject "PDebuggerRemoveBreakpoint" $ \v ->
         PDebuggerRemoveBreakpoint <$> v .:  "breakpointId"


instance ToJSON PDebuggerRemoveBreakpoint  where
    toJSON v = A.object
        [ "breakpointId" .= pDebuggerRemoveBreakpointBreakpointId v
        ]


debuggerRemoveBreakpoint :: Session -> PDebuggerRemoveBreakpoint -> IO (Maybe Error)
debuggerRemoveBreakpoint session params = sendReceiveCommand session "Debugger.removeBreakpoint" (Just params)



data PDebuggerResume = PDebuggerResume {
    pDebuggerResumeTerminateOnResume :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerResume where
    parseJSON = A.withObject "PDebuggerResume" $ \v ->
         PDebuggerResume <$> v .:?  "terminateOnResume"


instance ToJSON PDebuggerResume  where
    toJSON v = A.object
        [ "terminateOnResume" .= pDebuggerResumeTerminateOnResume v
        ]


debuggerResume :: Session -> PDebuggerResume -> IO (Maybe Error)
debuggerResume session params = sendReceiveCommand session "Debugger.resume" (Just params)

data DebuggerSearchInContent = DebuggerSearchInContent {
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSearchInContent where
    parseJSON = A.withObject "DebuggerSearchInContent" $ \v ->
         DebuggerSearchInContent <$> v .:  "result"



instance Command  DebuggerSearchInContent where
    commandName _ = "Debugger.searchInContent"

data PDebuggerSearchInContent = PDebuggerSearchInContent {
    pDebuggerSearchInContentScriptId :: RuntimeScriptId,
    pDebuggerSearchInContentQuery :: String,
    pDebuggerSearchInContentCaseSensitive :: Maybe Bool,
    pDebuggerSearchInContentIsRegex :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSearchInContent where
    parseJSON = A.withObject "PDebuggerSearchInContent" $ \v ->
         PDebuggerSearchInContent <$> v .:  "scriptId"
            <*> v  .:  "query"
            <*> v  .:?  "caseSensitive"
            <*> v  .:?  "isRegex"


instance ToJSON PDebuggerSearchInContent  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerSearchInContentScriptId v
        , "query" .= pDebuggerSearchInContentQuery v
        , "caseSensitive" .= pDebuggerSearchInContentCaseSensitive v
        , "isRegex" .= pDebuggerSearchInContentIsRegex v
        ]


debuggerSearchInContent :: Session -> PDebuggerSearchInContent -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent session params = sendReceiveCommandResult session "Debugger.searchInContent" (Just params)



data PDebuggerSetAsyncCallStackDepth = PDebuggerSetAsyncCallStackDepth {
    pDebuggerSetAsyncCallStackDepthMaxDepth :: Int
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetAsyncCallStackDepth where
    parseJSON = A.withObject "PDebuggerSetAsyncCallStackDepth" $ \v ->
         PDebuggerSetAsyncCallStackDepth <$> v .:  "maxDepth"


instance ToJSON PDebuggerSetAsyncCallStackDepth  where
    toJSON v = A.object
        [ "maxDepth" .= pDebuggerSetAsyncCallStackDepthMaxDepth v
        ]


debuggerSetAsyncCallStackDepth :: Session -> PDebuggerSetAsyncCallStackDepth -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth session params = sendReceiveCommand session "Debugger.setAsyncCallStackDepth" (Just params)

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpoint where
    parseJSON = A.withObject "DebuggerSetBreakpoint" $ \v ->
         DebuggerSetBreakpoint <$> v .:  "breakpointId"
            <*> v  .:  "actualLocation"



instance Command  DebuggerSetBreakpoint where
    commandName _ = "Debugger.setBreakpoint"

data PDebuggerSetBreakpoint = PDebuggerSetBreakpoint {
    pDebuggerSetBreakpointLocation :: DebuggerLocation,
    pDebuggerSetBreakpointCondition :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpoint where
    parseJSON = A.withObject "PDebuggerSetBreakpoint" $ \v ->
         PDebuggerSetBreakpoint <$> v .:  "location"
            <*> v  .:?  "condition"


instance ToJSON PDebuggerSetBreakpoint  where
    toJSON v = A.object
        [ "location" .= pDebuggerSetBreakpointLocation v
        , "condition" .= pDebuggerSetBreakpointCondition v
        ]


debuggerSetBreakpoint :: Session -> PDebuggerSetBreakpoint -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint session params = sendReceiveCommandResult session "Debugger.setBreakpoint" (Just params)

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "DebuggerSetInstrumentationBreakpoint" $ \v ->
         DebuggerSetInstrumentationBreakpoint <$> v .:  "breakpointId"



instance Command  DebuggerSetInstrumentationBreakpoint where
    commandName _ = "Debugger.setInstrumentationBreakpoint"

data PDebuggerSetInstrumentationBreakpoint = PDebuggerSetInstrumentationBreakpoint {
    pDebuggerSetInstrumentationBreakpointInstrumentation :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "PDebuggerSetInstrumentationBreakpoint" $ \v ->
         PDebuggerSetInstrumentationBreakpoint <$> v .:  "instrumentation"


instance ToJSON PDebuggerSetInstrumentationBreakpoint  where
    toJSON v = A.object
        [ "instrumentation" .= pDebuggerSetInstrumentationBreakpointInstrumentation v
        ]


debuggerSetInstrumentationBreakpoint :: Session -> PDebuggerSetInstrumentationBreakpoint -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint session params = sendReceiveCommandResult session "Debugger.setInstrumentationBreakpoint" (Just params)

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "DebuggerSetBreakpointByUrl" $ \v ->
         DebuggerSetBreakpointByUrl <$> v .:  "breakpointId"
            <*> v  .:  "locations"



instance Command  DebuggerSetBreakpointByUrl where
    commandName _ = "Debugger.setBreakpointByUrl"

data PDebuggerSetBreakpointByUrl = PDebuggerSetBreakpointByUrl {
    pDebuggerSetBreakpointByUrlLineNumber :: Int,
    pDebuggerSetBreakpointByUrlUrl :: Maybe String,
    pDebuggerSetBreakpointByUrlUrlRegex :: Maybe String,
    pDebuggerSetBreakpointByUrlScriptHash :: Maybe String,
    pDebuggerSetBreakpointByUrlColumnNumber :: Maybe Int,
    pDebuggerSetBreakpointByUrlCondition :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "PDebuggerSetBreakpointByUrl" $ \v ->
         PDebuggerSetBreakpointByUrl <$> v .:  "lineNumber"
            <*> v  .:?  "url"
            <*> v  .:?  "urlRegex"
            <*> v  .:?  "scriptHash"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "condition"


instance ToJSON PDebuggerSetBreakpointByUrl  where
    toJSON v = A.object
        [ "lineNumber" .= pDebuggerSetBreakpointByUrlLineNumber v
        , "url" .= pDebuggerSetBreakpointByUrlUrl v
        , "urlRegex" .= pDebuggerSetBreakpointByUrlUrlRegex v
        , "scriptHash" .= pDebuggerSetBreakpointByUrlScriptHash v
        , "columnNumber" .= pDebuggerSetBreakpointByUrlColumnNumber v
        , "condition" .= pDebuggerSetBreakpointByUrlCondition v
        ]


debuggerSetBreakpointByUrl :: Session -> PDebuggerSetBreakpointByUrl -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl session params = sendReceiveCommandResult session "Debugger.setBreakpointByUrl" (Just params)



data PDebuggerSetBreakpointsActive = PDebuggerSetBreakpointsActive {
    pDebuggerSetBreakpointsActiveActive :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetBreakpointsActive where
    parseJSON = A.withObject "PDebuggerSetBreakpointsActive" $ \v ->
         PDebuggerSetBreakpointsActive <$> v .:  "active"


instance ToJSON PDebuggerSetBreakpointsActive  where
    toJSON v = A.object
        [ "active" .= pDebuggerSetBreakpointsActiveActive v
        ]


debuggerSetBreakpointsActive :: Session -> PDebuggerSetBreakpointsActive -> IO (Maybe Error)
debuggerSetBreakpointsActive session params = sendReceiveCommand session "Debugger.setBreakpointsActive" (Just params)



data PDebuggerSetPauseOnExceptions = PDebuggerSetPauseOnExceptions {
    pDebuggerSetPauseOnExceptionsState :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetPauseOnExceptions where
    parseJSON = A.withObject "PDebuggerSetPauseOnExceptions" $ \v ->
         PDebuggerSetPauseOnExceptions <$> v .:  "state"


instance ToJSON PDebuggerSetPauseOnExceptions  where
    toJSON v = A.object
        [ "state" .= pDebuggerSetPauseOnExceptionsState v
        ]


debuggerSetPauseOnExceptions :: Session -> PDebuggerSetPauseOnExceptions -> IO (Maybe Error)
debuggerSetPauseOnExceptions session params = sendReceiveCommand session "Debugger.setPauseOnExceptions" (Just params)

data DebuggerSetScriptSource = DebuggerSetScriptSource {
    debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
    debuggerSetScriptSourceStackChanged :: Maybe Bool,
    debuggerSetScriptSourceAsyncStackTrace :: Maybe RuntimeStackTrace,
    debuggerSetScriptSourceExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving (Eq, Show, Read)
instance FromJSON  DebuggerSetScriptSource where
    parseJSON = A.withObject "DebuggerSetScriptSource" $ \v ->
         DebuggerSetScriptSource <$> v .:?  "callFrames"
            <*> v  .:?  "stackChanged"
            <*> v  .:?  "asyncStackTrace"
            <*> v  .:?  "exceptionDetails"



instance Command  DebuggerSetScriptSource where
    commandName _ = "Debugger.setScriptSource"

data PDebuggerSetScriptSource = PDebuggerSetScriptSource {
    pDebuggerSetScriptSourceScriptId :: RuntimeScriptId,
    pDebuggerSetScriptSourceScriptSource :: String,
    pDebuggerSetScriptSourceDryRun :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetScriptSource where
    parseJSON = A.withObject "PDebuggerSetScriptSource" $ \v ->
         PDebuggerSetScriptSource <$> v .:  "scriptId"
            <*> v  .:  "scriptSource"
            <*> v  .:?  "dryRun"


instance ToJSON PDebuggerSetScriptSource  where
    toJSON v = A.object
        [ "scriptId" .= pDebuggerSetScriptSourceScriptId v
        , "scriptSource" .= pDebuggerSetScriptSourceScriptSource v
        , "dryRun" .= pDebuggerSetScriptSourceDryRun v
        ]


debuggerSetScriptSource :: Session -> PDebuggerSetScriptSource -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource session params = sendReceiveCommandResult session "Debugger.setScriptSource" (Just params)



data PDebuggerSetSkipAllPauses = PDebuggerSetSkipAllPauses {
    pDebuggerSetSkipAllPausesSkip :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetSkipAllPauses where
    parseJSON = A.withObject "PDebuggerSetSkipAllPauses" $ \v ->
         PDebuggerSetSkipAllPauses <$> v .:  "skip"


instance ToJSON PDebuggerSetSkipAllPauses  where
    toJSON v = A.object
        [ "skip" .= pDebuggerSetSkipAllPausesSkip v
        ]


debuggerSetSkipAllPauses :: Session -> PDebuggerSetSkipAllPauses -> IO (Maybe Error)
debuggerSetSkipAllPauses session params = sendReceiveCommand session "Debugger.setSkipAllPauses" (Just params)



data PDebuggerSetVariableValue = PDebuggerSetVariableValue {
    pDebuggerSetVariableValueScopeNumber :: Int,
    pDebuggerSetVariableValueVariableName :: String,
    pDebuggerSetVariableValueNewValue :: RuntimeCallArgument,
    pDebuggerSetVariableValueCallFrameId :: DebuggerCallFrameId
} deriving (Eq, Show, Read)
instance FromJSON  PDebuggerSetVariableValue where
    parseJSON = A.withObject "PDebuggerSetVariableValue" $ \v ->
         PDebuggerSetVariableValue <$> v .:  "scopeNumber"
            <*> v  .:  "variableName"
            <*> v  .:  "newValue"
            <*> v  .:  "callFrameId"


instance ToJSON PDebuggerSetVariableValue  where
    toJSON v = A.object
        [ "scopeNumber" .= pDebuggerSetVariableValueScopeNumber v
        , "variableName" .= pDebuggerSetVariableValueVariableName v
        , "newValue" .= pDebuggerSetVariableValueNewValue v
        , "callFrameId" .= pDebuggerSetVariableValueCallFrameId v
        ]


debuggerSetVariableValue :: Session -> PDebuggerSetVariableValue -> IO (Maybe Error)
debuggerSetVariableValue session params = sendReceiveCommand session "Debugger.setVariableValue" (Just params)




debuggerStepInto :: Session -> IO (Maybe Error)
debuggerStepInto session = sendReceiveCommand session "Debugger.stepInto" (Nothing :: Maybe ())




debuggerStepOut :: Session -> IO (Maybe Error)
debuggerStepOut session = sendReceiveCommand session "Debugger.stepOut" (Nothing :: Maybe ())




debuggerStepOver :: Session -> IO (Maybe Error)
debuggerStepOver session = sendReceiveCommand session "Debugger.stepOver" (Nothing :: Maybe ())

