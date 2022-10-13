{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  DOMDebugger :
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
   parseJSON = A.withText  "DOMDebuggerDOMBreakpointType"  $ \v -> do
      case v of
         "subtree-modified" -> pure DOMDebuggerDOMBreakpointTypeSubtreeModified
         "attribute-modified" -> pure DOMDebuggerDOMBreakpointTypeAttributeModified
         "node-removed" -> pure DOMDebuggerDOMBreakpointTypeNodeRemoved
         _ -> fail "failed to parse DOMDebuggerDOMBreakpointType"

instance ToJSON DOMDebuggerDOMBreakpointType where
   toJSON v = A.String $
      case v of
         DOMDebuggerDOMBreakpointTypeSubtreeModified -> "subtree-modified"
         DOMDebuggerDOMBreakpointTypeAttributeModified -> "attribute-modified"
         DOMDebuggerDOMBreakpointTypeNodeRemoved -> "node-removed"



-- | Type 'DOMDebugger.CSPViolationType'.
--   CSP Violation type.
data DOMDebuggerCSPViolationType = DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation | DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation
   deriving (Ord, Eq, Show, Read)
instance FromJSON DOMDebuggerCSPViolationType where
   parseJSON = A.withText  "DOMDebuggerCSPViolationType"  $ \v -> do
      case v of
         "trustedtype-sink-violation" -> pure DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation
         "trustedtype-policy-violation" -> pure DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation
         _ -> fail "failed to parse DOMDebuggerCSPViolationType"

instance ToJSON DOMDebuggerCSPViolationType where
   toJSON v = A.String $
      case v of
         DOMDebuggerCSPViolationTypeTrustedtypeSinkViolation -> "trustedtype-sink-violation"
         DOMDebuggerCSPViolationTypeTrustedtypePolicyViolation -> "trustedtype-policy-violation"



-- | Type 'DOMDebugger.EventListener'.
--   Object event listener.
data DOMDebuggerEventListener = DOMDebuggerEventListener {
  -- | `EventListener`'s type.
  dOMDebuggerEventListenerType :: String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMDebuggerEventListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DOMDebuggerEventListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }







-- | DOMDebugger.getEventListeners
--   Returns event listeners of the given object.

-- | Parameters of the 'DOMDebugger.getEventListeners' command.
data PDOMDebuggerGetEventListeners = PDOMDebuggerGetEventListeners {
  -- | Identifier of the object to return listeners for.
  pDOMDebuggerGetEventListenersObjectId :: Runtime.RuntimeRemoteObjectId,
  -- | The maximum depth at which Node children should be retrieved, defaults to 1. Use -1 for the
  --   entire subtree or provide an integer larger than 0.
  pDOMDebuggerGetEventListenersDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
  --   (default is false). Reports listeners for all contexts if pierce is enabled.
  pDOMDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerGetEventListeners  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Return type of the 'DOMDebugger.getEventListeners' command.
data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners {
  -- | Array of relevant listeners.
  dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command PDOMDebuggerGetEventListeners where
   type CommandResponse PDOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners
   commandName _ = "DOMDebugger.getEventListeners"



-- | DOMDebugger.removeDOMBreakpoint
--   Removes DOM breakpoint that was set using `setDOMBreakpoint`.

-- | Parameters of the 'DOMDebugger.removeDOMBreakpoint' command.
data PDOMDebuggerRemoveDOMBreakpoint = PDOMDebuggerRemoveDOMBreakpoint {
  -- | Identifier of the node to remove breakpoint from.
  pDOMDebuggerRemoveDOMBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Type of the breakpoint to remove.
  pDOMDebuggerRemoveDOMBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveDOMBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveDOMBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PDOMDebuggerRemoveDOMBreakpoint where
   type CommandResponse PDOMDebuggerRemoveDOMBreakpoint = ()
   commandName _ = "DOMDebugger.removeDOMBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.removeEventListenerBreakpoint
--   Removes breakpoint on particular DOM event.

-- | Parameters of the 'DOMDebugger.removeEventListenerBreakpoint' command.
data PDOMDebuggerRemoveEventListenerBreakpoint = PDOMDebuggerRemoveEventListenerBreakpoint {
  -- | Event name.
  pDOMDebuggerRemoveEventListenerBreakpointEventName :: String,
  -- | EventTarget interface name.
  pDOMDebuggerRemoveEventListenerBreakpointTargetName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


instance Command PDOMDebuggerRemoveEventListenerBreakpoint where
   type CommandResponse PDOMDebuggerRemoveEventListenerBreakpoint = ()
   commandName _ = "DOMDebugger.removeEventListenerBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.removeInstrumentationBreakpoint
--   Removes breakpoint on particular native event.

-- | Parameters of the 'DOMDebugger.removeInstrumentationBreakpoint' command.
data PDOMDebuggerRemoveInstrumentationBreakpoint = PDOMDebuggerRemoveInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDOMDebuggerRemoveInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


instance Command PDOMDebuggerRemoveInstrumentationBreakpoint where
   type CommandResponse PDOMDebuggerRemoveInstrumentationBreakpoint = ()
   commandName _ = "DOMDebugger.removeInstrumentationBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.removeXHRBreakpoint
--   Removes breakpoint from XMLHttpRequest.

-- | Parameters of the 'DOMDebugger.removeXHRBreakpoint' command.
data PDOMDebuggerRemoveXHRBreakpoint = PDOMDebuggerRemoveXHRBreakpoint {
  -- | Resource URL substring.
  pDOMDebuggerRemoveXHRBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveXHRBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveXHRBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PDOMDebuggerRemoveXHRBreakpoint where
   type CommandResponse PDOMDebuggerRemoveXHRBreakpoint = ()
   commandName _ = "DOMDebugger.removeXHRBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.setBreakOnCSPViolation
--   Sets breakpoint on particular CSP violations.

-- | Parameters of the 'DOMDebugger.setBreakOnCSPViolation' command.
data PDOMDebuggerSetBreakOnCSPViolation = PDOMDebuggerSetBreakOnCSPViolation {
  -- | CSP Violations to stop upon.
  pDOMDebuggerSetBreakOnCSPViolationViolationTypes :: [DOMDebuggerCSPViolationType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetBreakOnCSPViolation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetBreakOnCSPViolation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Command PDOMDebuggerSetBreakOnCSPViolation where
   type CommandResponse PDOMDebuggerSetBreakOnCSPViolation = ()
   commandName _ = "DOMDebugger.setBreakOnCSPViolation"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.setDOMBreakpoint
--   Sets breakpoint on particular operation with DOM.

-- | Parameters of the 'DOMDebugger.setDOMBreakpoint' command.
data PDOMDebuggerSetDOMBreakpoint = PDOMDebuggerSetDOMBreakpoint {
  -- | Identifier of the node to set breakpoint on.
  pDOMDebuggerSetDOMBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Type of the operation to stop upon.
  pDOMDebuggerSetDOMBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetDOMBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetDOMBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PDOMDebuggerSetDOMBreakpoint where
   type CommandResponse PDOMDebuggerSetDOMBreakpoint = ()
   commandName _ = "DOMDebugger.setDOMBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.setEventListenerBreakpoint
--   Sets breakpoint on particular DOM event.

-- | Parameters of the 'DOMDebugger.setEventListenerBreakpoint' command.
data PDOMDebuggerSetEventListenerBreakpoint = PDOMDebuggerSetEventListenerBreakpoint {
  -- | DOM Event name to stop on (any DOM event will do).
  pDOMDebuggerSetEventListenerBreakpointEventName :: String,
  -- | EventTarget interface name to stop on. If equal to `"*"` or not provided, will stop on any
  --   EventTarget.
  pDOMDebuggerSetEventListenerBreakpointTargetName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


instance Command PDOMDebuggerSetEventListenerBreakpoint where
   type CommandResponse PDOMDebuggerSetEventListenerBreakpoint = ()
   commandName _ = "DOMDebugger.setEventListenerBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.setInstrumentationBreakpoint
--   Sets breakpoint on particular native event.

-- | Parameters of the 'DOMDebugger.setInstrumentationBreakpoint' command.
data PDOMDebuggerSetInstrumentationBreakpoint = PDOMDebuggerSetInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDOMDebuggerSetInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


instance Command PDOMDebuggerSetInstrumentationBreakpoint where
   type CommandResponse PDOMDebuggerSetInstrumentationBreakpoint = ()
   commandName _ = "DOMDebugger.setInstrumentationBreakpoint"
   fromJSON = const . A.Success . const ()


-- | DOMDebugger.setXHRBreakpoint
--   Sets breakpoint on XMLHttpRequest.

-- | Parameters of the 'DOMDebugger.setXHRBreakpoint' command.
data PDOMDebuggerSetXHRBreakpoint = PDOMDebuggerSetXHRBreakpoint {
  -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
  pDOMDebuggerSetXHRBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetXHRBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetXHRBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PDOMDebuggerSetXHRBreakpoint where
   type CommandResponse PDOMDebuggerSetXHRBreakpoint = ()
   commandName _ = "DOMDebugger.setXHRBreakpoint"
   fromJSON = const . A.Success . const ()



