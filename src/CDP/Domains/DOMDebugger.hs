{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DOMDebugger

DOM debugging allows setting breakpoints on particular DOM operations and events. JavaScript
execution will stop on these operations as if there was a regular breakpoint set.
-}


module CDP.Domains.DOMDebugger (module CDP.Domains.DOMDebugger) where

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


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'DOMDebugger.DOMBreakpointType'.
--   DOM breakpoint type.
data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
  deriving (Ord, Eq, Show, Read)
instance FromJSON DOMDebuggerDOMBreakpointType where
  parseJSON = A.withText "DOMDebuggerDOMBreakpointType" $ \v -> case v of
    "subtree-modified" -> pure DOMDebuggerDOMBreakpointTypeSubtreeModified
    "attribute-modified" -> pure DOMDebuggerDOMBreakpointTypeAttributeModified
    "node-removed" -> pure DOMDebuggerDOMBreakpointTypeNodeRemoved
    "_" -> fail "failed to parse DOMDebuggerDOMBreakpointType"
instance ToJSON DOMDebuggerDOMBreakpointType where
  toJSON v = A.String $ case v of
    DOMDebuggerDOMBreakpointTypeSubtreeModified -> "subtree-modified"
    DOMDebuggerDOMBreakpointTypeAttributeModified -> "attribute-modified"
    DOMDebuggerDOMBreakpointTypeNodeRemoved -> "node-removed"

-- | Type 'DOMDebugger.CSPViolationType'.
--   CSP Violation type.
data DOMDebuggerCSPViolationType = DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation | DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation
  deriving (Ord, Eq, Show, Read)
instance FromJSON DOMDebuggerCSPViolationType where
  parseJSON = A.withText "DOMDebuggerCSPViolationType" $ \v -> case v of
    "trustedtype-sink-violation" -> pure DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation
    "trustedtype-policy-violation" -> pure DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation
    "_" -> fail "failed to parse DOMDebuggerCSPViolationType"
instance ToJSON DOMDebuggerCSPViolationType where
  toJSON v = A.String $ case v of
    DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation -> "trustedtype-sink-violation"
    DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation -> "trustedtype-policy-violation"

-- | Type 'DOMDebugger.EventListener'.
--   Object event listener.
data DOMDebuggerEventListener = DOMDebuggerEventListener
  {
    -- | `EventListener`'s type.
    dOMDebuggerEventListenerType :: T.Text,
    -- | `EventListener`'s useCapture.
    dOMDebuggerEventListenerUseCapture :: Bool,
    -- | `EventListener`'s passive flag.
    dOMDebuggerEventListenerPassive :: Bool,
    -- | `EventListener`'s once flag.
    dOMDebuggerEventListenerOnce :: Bool,
    -- | Script id of the handler code.
    dOMDebuggerEventListenerScriptId :: Runtime.RuntimeScriptId,
    -- | Line number in the script (0-based).
    dOMDebuggerEventListenerLineNumber :: Int,
    -- | Column number in the script (0-based).
    dOMDebuggerEventListenerColumnNumber :: Int,
    -- | Event handler function value.
    dOMDebuggerEventListenerHandler :: Maybe Runtime.RuntimeRemoteObject,
    -- | Event original handler function value.
    dOMDebuggerEventListenerOriginalHandler :: Maybe Runtime.RuntimeRemoteObject,
    -- | Node the listener is added to (if any).
    dOMDebuggerEventListenerBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMDebuggerEventListener where
  parseJSON = A.withObject "DOMDebuggerEventListener" $ \o -> DOMDebuggerEventListener
    <$> o A..: "type"
    <*> o A..: "useCapture"
    <*> o A..: "passive"
    <*> o A..: "once"
    <*> o A..: "scriptId"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
    <*> o A..:? "handler"
    <*> o A..:? "originalHandler"
    <*> o A..:? "backendNodeId"
instance ToJSON DOMDebuggerEventListener where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (dOMDebuggerEventListenerType p),
    ("useCapture" A..=) <$> Just (dOMDebuggerEventListenerUseCapture p),
    ("passive" A..=) <$> Just (dOMDebuggerEventListenerPassive p),
    ("once" A..=) <$> Just (dOMDebuggerEventListenerOnce p),
    ("scriptId" A..=) <$> Just (dOMDebuggerEventListenerScriptId p),
    ("lineNumber" A..=) <$> Just (dOMDebuggerEventListenerLineNumber p),
    ("columnNumber" A..=) <$> Just (dOMDebuggerEventListenerColumnNumber p),
    ("handler" A..=) <$> (dOMDebuggerEventListenerHandler p),
    ("originalHandler" A..=) <$> (dOMDebuggerEventListenerOriginalHandler p),
    ("backendNodeId" A..=) <$> (dOMDebuggerEventListenerBackendNodeId p)
    ]

-- | Returns event listeners of the given object.

-- | Parameters of the 'DOMDebugger.getEventListeners' command.
data PDOMDebuggerGetEventListeners = PDOMDebuggerGetEventListeners
  {
    -- | Identifier of the object to return listeners for.
    pDOMDebuggerGetEventListenersObjectId :: Runtime.RuntimeRemoteObjectId,
    -- | The maximum depth at which Node children should be retrieved, defaults to 1. Use -1 for the
    --   entire subtree or provide an integer larger than 0.
    pDOMDebuggerGetEventListenersDepth :: Maybe Int,
    -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
    --   (default is false). Reports listeners for all contexts if pierce is enabled.
    pDOMDebuggerGetEventListenersPierce :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMDebuggerGetEventListeners
  -- | Identifier of the object to return listeners for.
  :: Runtime.RuntimeRemoteObjectId
  -> PDOMDebuggerGetEventListeners
pDOMDebuggerGetEventListeners
  arg_pDOMDebuggerGetEventListenersObjectId
  = PDOMDebuggerGetEventListeners
    arg_pDOMDebuggerGetEventListenersObjectId
    Nothing
    Nothing
instance ToJSON PDOMDebuggerGetEventListeners where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pDOMDebuggerGetEventListenersObjectId p),
    ("depth" A..=) <$> (pDOMDebuggerGetEventListenersDepth p),
    ("pierce" A..=) <$> (pDOMDebuggerGetEventListenersPierce p)
    ]
data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners
  {
    -- | Array of relevant listeners.
    dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
  }
  deriving (Eq, Show)
instance FromJSON DOMDebuggerGetEventListeners where
  parseJSON = A.withObject "DOMDebuggerGetEventListeners" $ \o -> DOMDebuggerGetEventListeners
    <$> o A..: "listeners"
instance Command PDOMDebuggerGetEventListeners where
  type CommandResponse PDOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners
  commandName _ = "DOMDebugger.getEventListeners"

-- | Removes DOM breakpoint that was set using `setDOMBreakpoint`.

-- | Parameters of the 'DOMDebugger.removeDOMBreakpoint' command.
data PDOMDebuggerRemoveDOMBreakpoint = PDOMDebuggerRemoveDOMBreakpoint
  {
    -- | Identifier of the node to remove breakpoint from.
    pDOMDebuggerRemoveDOMBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Type of the breakpoint to remove.
    pDOMDebuggerRemoveDOMBreakpointType :: DOMDebuggerDOMBreakpointType
  }
  deriving (Eq, Show)
pDOMDebuggerRemoveDOMBreakpoint
  -- | Identifier of the node to remove breakpoint from.
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -- | Type of the breakpoint to remove.
  -> DOMDebuggerDOMBreakpointType
  -> PDOMDebuggerRemoveDOMBreakpoint
pDOMDebuggerRemoveDOMBreakpoint
  arg_pDOMDebuggerRemoveDOMBreakpointNodeId
  arg_pDOMDebuggerRemoveDOMBreakpointType
  = PDOMDebuggerRemoveDOMBreakpoint
    arg_pDOMDebuggerRemoveDOMBreakpointNodeId
    arg_pDOMDebuggerRemoveDOMBreakpointType
instance ToJSON PDOMDebuggerRemoveDOMBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMDebuggerRemoveDOMBreakpointNodeId p),
    ("type" A..=) <$> Just (pDOMDebuggerRemoveDOMBreakpointType p)
    ]
instance Command PDOMDebuggerRemoveDOMBreakpoint where
  type CommandResponse PDOMDebuggerRemoveDOMBreakpoint = ()
  commandName _ = "DOMDebugger.removeDOMBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes breakpoint on particular DOM event.

-- | Parameters of the 'DOMDebugger.removeEventListenerBreakpoint' command.
data PDOMDebuggerRemoveEventListenerBreakpoint = PDOMDebuggerRemoveEventListenerBreakpoint
  {
    -- | Event name.
    pDOMDebuggerRemoveEventListenerBreakpointEventName :: T.Text,
    -- | EventTarget interface name.
    pDOMDebuggerRemoveEventListenerBreakpointTargetName :: Maybe T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerRemoveEventListenerBreakpoint
  -- | Event name.
  :: T.Text
  -> PDOMDebuggerRemoveEventListenerBreakpoint
pDOMDebuggerRemoveEventListenerBreakpoint
  arg_pDOMDebuggerRemoveEventListenerBreakpointEventName
  = PDOMDebuggerRemoveEventListenerBreakpoint
    arg_pDOMDebuggerRemoveEventListenerBreakpointEventName
    Nothing
instance ToJSON PDOMDebuggerRemoveEventListenerBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pDOMDebuggerRemoveEventListenerBreakpointEventName p),
    ("targetName" A..=) <$> (pDOMDebuggerRemoveEventListenerBreakpointTargetName p)
    ]
instance Command PDOMDebuggerRemoveEventListenerBreakpoint where
  type CommandResponse PDOMDebuggerRemoveEventListenerBreakpoint = ()
  commandName _ = "DOMDebugger.removeEventListenerBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes breakpoint on particular native event.

-- | Parameters of the 'DOMDebugger.removeInstrumentationBreakpoint' command.
data PDOMDebuggerRemoveInstrumentationBreakpoint = PDOMDebuggerRemoveInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pDOMDebuggerRemoveInstrumentationBreakpointEventName :: T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerRemoveInstrumentationBreakpoint
  -- | Instrumentation name to stop on.
  :: T.Text
  -> PDOMDebuggerRemoveInstrumentationBreakpoint
pDOMDebuggerRemoveInstrumentationBreakpoint
  arg_pDOMDebuggerRemoveInstrumentationBreakpointEventName
  = PDOMDebuggerRemoveInstrumentationBreakpoint
    arg_pDOMDebuggerRemoveInstrumentationBreakpointEventName
instance ToJSON PDOMDebuggerRemoveInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pDOMDebuggerRemoveInstrumentationBreakpointEventName p)
    ]
instance Command PDOMDebuggerRemoveInstrumentationBreakpoint where
  type CommandResponse PDOMDebuggerRemoveInstrumentationBreakpoint = ()
  commandName _ = "DOMDebugger.removeInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes breakpoint from XMLHttpRequest.

-- | Parameters of the 'DOMDebugger.removeXHRBreakpoint' command.
data PDOMDebuggerRemoveXHRBreakpoint = PDOMDebuggerRemoveXHRBreakpoint
  {
    -- | Resource URL substring.
    pDOMDebuggerRemoveXHRBreakpointUrl :: T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerRemoveXHRBreakpoint
  -- | Resource URL substring.
  :: T.Text
  -> PDOMDebuggerRemoveXHRBreakpoint
pDOMDebuggerRemoveXHRBreakpoint
  arg_pDOMDebuggerRemoveXHRBreakpointUrl
  = PDOMDebuggerRemoveXHRBreakpoint
    arg_pDOMDebuggerRemoveXHRBreakpointUrl
instance ToJSON PDOMDebuggerRemoveXHRBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pDOMDebuggerRemoveXHRBreakpointUrl p)
    ]
instance Command PDOMDebuggerRemoveXHRBreakpoint where
  type CommandResponse PDOMDebuggerRemoveXHRBreakpoint = ()
  commandName _ = "DOMDebugger.removeXHRBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Sets breakpoint on particular CSP violations.

-- | Parameters of the 'DOMDebugger.setBreakOnCSPViolation' command.
data PDOMDebuggerSetBreakOnCSPViolation = PDOMDebuggerSetBreakOnCSPViolation
  {
    -- | CSP Violations to stop upon.
    pDOMDebuggerSetBreakOnCSPViolationViolationTypes :: [DOMDebuggerCSPViolationType]
  }
  deriving (Eq, Show)
pDOMDebuggerSetBreakOnCSPViolation
  -- | CSP Violations to stop upon.
  :: [DOMDebuggerCSPViolationType]
  -> PDOMDebuggerSetBreakOnCSPViolation
pDOMDebuggerSetBreakOnCSPViolation
  arg_pDOMDebuggerSetBreakOnCSPViolationViolationTypes
  = PDOMDebuggerSetBreakOnCSPViolation
    arg_pDOMDebuggerSetBreakOnCSPViolationViolationTypes
instance ToJSON PDOMDebuggerSetBreakOnCSPViolation where
  toJSON p = A.object $ catMaybes [
    ("violationTypes" A..=) <$> Just (pDOMDebuggerSetBreakOnCSPViolationViolationTypes p)
    ]
instance Command PDOMDebuggerSetBreakOnCSPViolation where
  type CommandResponse PDOMDebuggerSetBreakOnCSPViolation = ()
  commandName _ = "DOMDebugger.setBreakOnCSPViolation"
  fromJSON = const . A.Success . const ()

-- | Sets breakpoint on particular operation with DOM.

-- | Parameters of the 'DOMDebugger.setDOMBreakpoint' command.
data PDOMDebuggerSetDOMBreakpoint = PDOMDebuggerSetDOMBreakpoint
  {
    -- | Identifier of the node to set breakpoint on.
    pDOMDebuggerSetDOMBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Type of the operation to stop upon.
    pDOMDebuggerSetDOMBreakpointType :: DOMDebuggerDOMBreakpointType
  }
  deriving (Eq, Show)
pDOMDebuggerSetDOMBreakpoint
  -- | Identifier of the node to set breakpoint on.
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -- | Type of the operation to stop upon.
  -> DOMDebuggerDOMBreakpointType
  -> PDOMDebuggerSetDOMBreakpoint
pDOMDebuggerSetDOMBreakpoint
  arg_pDOMDebuggerSetDOMBreakpointNodeId
  arg_pDOMDebuggerSetDOMBreakpointType
  = PDOMDebuggerSetDOMBreakpoint
    arg_pDOMDebuggerSetDOMBreakpointNodeId
    arg_pDOMDebuggerSetDOMBreakpointType
instance ToJSON PDOMDebuggerSetDOMBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMDebuggerSetDOMBreakpointNodeId p),
    ("type" A..=) <$> Just (pDOMDebuggerSetDOMBreakpointType p)
    ]
instance Command PDOMDebuggerSetDOMBreakpoint where
  type CommandResponse PDOMDebuggerSetDOMBreakpoint = ()
  commandName _ = "DOMDebugger.setDOMBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Sets breakpoint on particular DOM event.

-- | Parameters of the 'DOMDebugger.setEventListenerBreakpoint' command.
data PDOMDebuggerSetEventListenerBreakpoint = PDOMDebuggerSetEventListenerBreakpoint
  {
    -- | DOM Event name to stop on (any DOM event will do).
    pDOMDebuggerSetEventListenerBreakpointEventName :: T.Text,
    -- | EventTarget interface name to stop on. If equal to `"*"` or not provided, will stop on any
    --   EventTarget.
    pDOMDebuggerSetEventListenerBreakpointTargetName :: Maybe T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerSetEventListenerBreakpoint
  -- | DOM Event name to stop on (any DOM event will do).
  :: T.Text
  -> PDOMDebuggerSetEventListenerBreakpoint
pDOMDebuggerSetEventListenerBreakpoint
  arg_pDOMDebuggerSetEventListenerBreakpointEventName
  = PDOMDebuggerSetEventListenerBreakpoint
    arg_pDOMDebuggerSetEventListenerBreakpointEventName
    Nothing
instance ToJSON PDOMDebuggerSetEventListenerBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pDOMDebuggerSetEventListenerBreakpointEventName p),
    ("targetName" A..=) <$> (pDOMDebuggerSetEventListenerBreakpointTargetName p)
    ]
instance Command PDOMDebuggerSetEventListenerBreakpoint where
  type CommandResponse PDOMDebuggerSetEventListenerBreakpoint = ()
  commandName _ = "DOMDebugger.setEventListenerBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Sets breakpoint on particular native event.

-- | Parameters of the 'DOMDebugger.setInstrumentationBreakpoint' command.
data PDOMDebuggerSetInstrumentationBreakpoint = PDOMDebuggerSetInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pDOMDebuggerSetInstrumentationBreakpointEventName :: T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerSetInstrumentationBreakpoint
  -- | Instrumentation name to stop on.
  :: T.Text
  -> PDOMDebuggerSetInstrumentationBreakpoint
pDOMDebuggerSetInstrumentationBreakpoint
  arg_pDOMDebuggerSetInstrumentationBreakpointEventName
  = PDOMDebuggerSetInstrumentationBreakpoint
    arg_pDOMDebuggerSetInstrumentationBreakpointEventName
instance ToJSON PDOMDebuggerSetInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pDOMDebuggerSetInstrumentationBreakpointEventName p)
    ]
instance Command PDOMDebuggerSetInstrumentationBreakpoint where
  type CommandResponse PDOMDebuggerSetInstrumentationBreakpoint = ()
  commandName _ = "DOMDebugger.setInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Sets breakpoint on XMLHttpRequest.

-- | Parameters of the 'DOMDebugger.setXHRBreakpoint' command.
data PDOMDebuggerSetXHRBreakpoint = PDOMDebuggerSetXHRBreakpoint
  {
    -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
    pDOMDebuggerSetXHRBreakpointUrl :: T.Text
  }
  deriving (Eq, Show)
pDOMDebuggerSetXHRBreakpoint
  -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
  :: T.Text
  -> PDOMDebuggerSetXHRBreakpoint
pDOMDebuggerSetXHRBreakpoint
  arg_pDOMDebuggerSetXHRBreakpointUrl
  = PDOMDebuggerSetXHRBreakpoint
    arg_pDOMDebuggerSetXHRBreakpointUrl
instance ToJSON PDOMDebuggerSetXHRBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pDOMDebuggerSetXHRBreakpointUrl p)
    ]
instance Command PDOMDebuggerSetXHRBreakpoint where
  type CommandResponse PDOMDebuggerSetXHRBreakpoint = ()
  commandName _ = "DOMDebugger.setXHRBreakpoint"
  fromJSON = const . A.Success . const ()

