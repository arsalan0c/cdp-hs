{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'DOMDebugger.DOMBreakpointType'.
--   DOM breakpoint type.
data DomDebuggerDomBreakpointType = DomDebuggerDomBreakpointTypeSubtreeModified | DomDebuggerDomBreakpointTypeAttributeModified | DomDebuggerDomBreakpointTypeNodeRemoved
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomDebuggerDomBreakpointType where
   parseJSON = A.withText  "DomDebuggerDomBreakpointType"  $ \v -> do
      case v of
         "subtree-modified" -> pure DomDebuggerDomBreakpointTypeSubtreeModified
         "attribute-modified" -> pure DomDebuggerDomBreakpointTypeAttributeModified
         "node-removed" -> pure DomDebuggerDomBreakpointTypeNodeRemoved
         _ -> fail "failed to parse DomDebuggerDomBreakpointType"

instance ToJSON DomDebuggerDomBreakpointType where
   toJSON v = A.String $
      case v of
         DomDebuggerDomBreakpointTypeSubtreeModified -> "subtree-modified"
         DomDebuggerDomBreakpointTypeAttributeModified -> "attribute-modified"
         DomDebuggerDomBreakpointTypeNodeRemoved -> "node-removed"



-- | Type 'DOMDebugger.CSPViolationType'.
--   CSP Violation type.
data DomDebuggerCspViolationType = DomDebuggerCspViolationTypeTrustedtypeSinkViolation | DomDebuggerCspViolationTypeTrustedtypePolicyViolation
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomDebuggerCspViolationType where
   parseJSON = A.withText  "DomDebuggerCspViolationType"  $ \v -> do
      case v of
         "trustedtype-sink-violation" -> pure DomDebuggerCspViolationTypeTrustedtypeSinkViolation
         "trustedtype-policy-violation" -> pure DomDebuggerCspViolationTypeTrustedtypePolicyViolation
         _ -> fail "failed to parse DomDebuggerCspViolationType"

instance ToJSON DomDebuggerCspViolationType where
   toJSON v = A.String $
      case v of
         DomDebuggerCspViolationTypeTrustedtypeSinkViolation -> "trustedtype-sink-violation"
         DomDebuggerCspViolationTypeTrustedtypePolicyViolation -> "trustedtype-policy-violation"



-- | Type 'DOMDebugger.EventListener'.
--   Object event listener.
data DomDebuggerEventListener = DomDebuggerEventListener {
  -- | `EventListener`'s type.
  domDebuggerEventListenerType :: String,
  -- | `EventListener`'s useCapture.
  domDebuggerEventListenerUseCapture :: Bool,
  -- | `EventListener`'s passive flag.
  domDebuggerEventListenerPassive :: Bool,
  -- | `EventListener`'s once flag.
  domDebuggerEventListenerOnce :: Bool,
  -- | Script id of the handler code.
  domDebuggerEventListenerScriptId :: Runtime.RuntimeScriptId,
  -- | Line number in the script (0-based).
  domDebuggerEventListenerLineNumber :: Int,
  -- | Column number in the script (0-based).
  domDebuggerEventListenerColumnNumber :: Int,
  -- | Event handler function value.
  domDebuggerEventListenerHandler :: Maybe Runtime.RuntimeRemoteObject,
  -- | Event original handler function value.
  domDebuggerEventListenerOriginalHandler :: Maybe Runtime.RuntimeRemoteObject,
  -- | Node the listener is added to (if any).
  domDebuggerEventListenerBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomDebuggerEventListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomDebuggerEventListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }







-- | Parameters of the 'domDebuggerGetEventListeners' command.
data PDomDebuggerGetEventListeners = PDomDebuggerGetEventListeners {
  -- | Identifier of the object to return listeners for.
  pDomDebuggerGetEventListenersObjectId :: Runtime.RuntimeRemoteObjectId,
  -- | The maximum depth at which Node children should be retrieved, defaults to 1. Use -1 for the
  --   entire subtree or provide an integer larger than 0.
  pDomDebuggerGetEventListenersDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
  --   (default is false). Reports listeners for all contexts if pierce is enabled.
  pDomDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerGetEventListeners  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'DOMDebugger.getEventListeners' command.
--   Returns event listeners of the given object.
--   Parameters: 'PDomDebuggerGetEventListeners'
--   Returns: 'DomDebuggerGetEventListeners'
domDebuggerGetEventListeners :: Handle ev -> PDomDebuggerGetEventListeners -> IO DomDebuggerGetEventListeners
domDebuggerGetEventListeners handle params = sendReceiveCommandResult handle "DOMDebugger.getEventListeners" (Just params)

-- | Return type of the 'domDebuggerGetEventListeners' command.
data DomDebuggerGetEventListeners = DomDebuggerGetEventListeners {
  -- | Array of relevant listeners.
  domDebuggerGetEventListenersListeners :: [DomDebuggerEventListener]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomDebuggerGetEventListeners where
   commandName _ = "DOMDebugger.getEventListeners"



-- | Parameters of the 'domDebuggerRemoveDomBreakpoint' command.
data PDomDebuggerRemoveDomBreakpoint = PDomDebuggerRemoveDomBreakpoint {
  -- | Identifier of the node to remove breakpoint from.
  pDomDebuggerRemoveDomBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Type of the breakpoint to remove.
  pDomDebuggerRemoveDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'DOMDebugger.removeDOMBreakpoint' command.
--   Removes DOM breakpoint that was set using `setDOMBreakpoint`.
--   Parameters: 'PDomDebuggerRemoveDomBreakpoint'
domDebuggerRemoveDomBreakpoint :: Handle ev -> PDomDebuggerRemoveDomBreakpoint -> IO ()
domDebuggerRemoveDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeDOMBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerRemoveEventListenerBreakpoint' command.
data PDomDebuggerRemoveEventListenerBreakpoint = PDomDebuggerRemoveEventListenerBreakpoint {
  -- | Event name.
  pDomDebuggerRemoveEventListenerBreakpointEventName :: String,
  -- | EventTarget interface name.
  pDomDebuggerRemoveEventListenerBreakpointTargetName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


-- | Function for the 'DOMDebugger.removeEventListenerBreakpoint' command.
--   Removes breakpoint on particular DOM event.
--   Parameters: 'PDomDebuggerRemoveEventListenerBreakpoint'
domDebuggerRemoveEventListenerBreakpoint :: Handle ev -> PDomDebuggerRemoveEventListenerBreakpoint -> IO ()
domDebuggerRemoveEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeEventListenerBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerRemoveInstrumentationBreakpoint' command.
data PDomDebuggerRemoveInstrumentationBreakpoint = PDomDebuggerRemoveInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDomDebuggerRemoveInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


-- | Function for the 'DOMDebugger.removeInstrumentationBreakpoint' command.
--   Removes breakpoint on particular native event.
--   Parameters: 'PDomDebuggerRemoveInstrumentationBreakpoint'
domDebuggerRemoveInstrumentationBreakpoint :: Handle ev -> PDomDebuggerRemoveInstrumentationBreakpoint -> IO ()
domDebuggerRemoveInstrumentationBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeInstrumentationBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerRemoveXhrBreakpoint' command.
data PDomDebuggerRemoveXhrBreakpoint = PDomDebuggerRemoveXhrBreakpoint {
  -- | Resource URL substring.
  pDomDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'DOMDebugger.removeXHRBreakpoint' command.
--   Removes breakpoint from XMLHttpRequest.
--   Parameters: 'PDomDebuggerRemoveXhrBreakpoint'
domDebuggerRemoveXhrBreakpoint :: Handle ev -> PDomDebuggerRemoveXhrBreakpoint -> IO ()
domDebuggerRemoveXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeXHRBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerSetBreakOnCspViolation' command.
data PDomDebuggerSetBreakOnCspViolation = PDomDebuggerSetBreakOnCspViolation {
  -- | CSP Violations to stop upon.
  pDomDebuggerSetBreakOnCspViolationViolationTypes :: [DomDebuggerCspViolationType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetBreakOnCspViolation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetBreakOnCspViolation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'DOMDebugger.setBreakOnCSPViolation' command.
--   Sets breakpoint on particular CSP violations.
--   Parameters: 'PDomDebuggerSetBreakOnCspViolation'
domDebuggerSetBreakOnCspViolation :: Handle ev -> PDomDebuggerSetBreakOnCspViolation -> IO ()
domDebuggerSetBreakOnCspViolation handle params = sendReceiveCommand handle "DOMDebugger.setBreakOnCSPViolation" (Just params)


-- | Parameters of the 'domDebuggerSetDomBreakpoint' command.
data PDomDebuggerSetDomBreakpoint = PDomDebuggerSetDomBreakpoint {
  -- | Identifier of the node to set breakpoint on.
  pDomDebuggerSetDomBreakpointNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Type of the operation to stop upon.
  pDomDebuggerSetDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOMDebugger.setDOMBreakpoint' command.
--   Sets breakpoint on particular operation with DOM.
--   Parameters: 'PDomDebuggerSetDomBreakpoint'
domDebuggerSetDomBreakpoint :: Handle ev -> PDomDebuggerSetDomBreakpoint -> IO ()
domDebuggerSetDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setDOMBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerSetEventListenerBreakpoint' command.
data PDomDebuggerSetEventListenerBreakpoint = PDomDebuggerSetEventListenerBreakpoint {
  -- | DOM Event name to stop on (any DOM event will do).
  pDomDebuggerSetEventListenerBreakpointEventName :: String,
  -- | EventTarget interface name to stop on. If equal to `"*"` or not provided, will stop on any
  --   EventTarget.
  pDomDebuggerSetEventListenerBreakpointTargetName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'DOMDebugger.setEventListenerBreakpoint' command.
--   Sets breakpoint on particular DOM event.
--   Parameters: 'PDomDebuggerSetEventListenerBreakpoint'
domDebuggerSetEventListenerBreakpoint :: Handle ev -> PDomDebuggerSetEventListenerBreakpoint -> IO ()
domDebuggerSetEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setEventListenerBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerSetInstrumentationBreakpoint' command.
data PDomDebuggerSetInstrumentationBreakpoint = PDomDebuggerSetInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDomDebuggerSetInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the 'DOMDebugger.setInstrumentationBreakpoint' command.
--   Sets breakpoint on particular native event.
--   Parameters: 'PDomDebuggerSetInstrumentationBreakpoint'
domDebuggerSetInstrumentationBreakpoint :: Handle ev -> PDomDebuggerSetInstrumentationBreakpoint -> IO ()
domDebuggerSetInstrumentationBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setInstrumentationBreakpoint" (Just params)


-- | Parameters of the 'domDebuggerSetXhrBreakpoint' command.
data PDomDebuggerSetXhrBreakpoint = PDomDebuggerSetXhrBreakpoint {
  -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
  pDomDebuggerSetXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOMDebugger.setXHRBreakpoint' command.
--   Sets breakpoint on XMLHttpRequest.
--   Parameters: 'PDomDebuggerSetXhrBreakpoint'
domDebuggerSetXhrBreakpoint :: Handle ev -> PDomDebuggerSetXhrBreakpoint -> IO ()
domDebuggerSetXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setXHRBreakpoint" (Just params)



