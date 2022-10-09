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







-- | Parameters of the 'dOMDebuggerGetEventListeners' command.
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


-- | Function for the 'DOMDebugger.getEventListeners' command.
--   Returns event listeners of the given object.
--   Parameters: 'PDOMDebuggerGetEventListeners'
--   Returns: 'DOMDebuggerGetEventListeners'
dOMDebuggerGetEventListeners :: Handle ev -> PDOMDebuggerGetEventListeners -> IO DOMDebuggerGetEventListeners
dOMDebuggerGetEventListeners handle params = sendReceiveCommandResult handle "DOMDebugger.getEventListeners" (Just params)

-- | Return type of the 'dOMDebuggerGetEventListeners' command.
data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners {
  -- | Array of relevant listeners.
  dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DOMDebuggerGetEventListeners where
   commandName _ = "DOMDebugger.getEventListeners"



-- | Parameters of the 'dOMDebuggerRemoveDOMBreakpoint' command.
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


-- | Function for the 'DOMDebugger.removeDOMBreakpoint' command.
--   Removes DOM breakpoint that was set using `setDOMBreakpoint`.
--   Parameters: 'PDOMDebuggerRemoveDOMBreakpoint'
dOMDebuggerRemoveDOMBreakpoint :: Handle ev -> PDOMDebuggerRemoveDOMBreakpoint -> IO ()
dOMDebuggerRemoveDOMBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeDOMBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerRemoveEventListenerBreakpoint' command.
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


-- | Function for the 'DOMDebugger.removeEventListenerBreakpoint' command.
--   Removes breakpoint on particular DOM event.
--   Parameters: 'PDOMDebuggerRemoveEventListenerBreakpoint'
dOMDebuggerRemoveEventListenerBreakpoint :: Handle ev -> PDOMDebuggerRemoveEventListenerBreakpoint -> IO ()
dOMDebuggerRemoveEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeEventListenerBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerRemoveInstrumentationBreakpoint' command.
data PDOMDebuggerRemoveInstrumentationBreakpoint = PDOMDebuggerRemoveInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDOMDebuggerRemoveInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


-- | Function for the 'DOMDebugger.removeInstrumentationBreakpoint' command.
--   Removes breakpoint on particular native event.
--   Parameters: 'PDOMDebuggerRemoveInstrumentationBreakpoint'
dOMDebuggerRemoveInstrumentationBreakpoint :: Handle ev -> PDOMDebuggerRemoveInstrumentationBreakpoint -> IO ()
dOMDebuggerRemoveInstrumentationBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeInstrumentationBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerRemoveXHRBreakpoint' command.
data PDOMDebuggerRemoveXHRBreakpoint = PDOMDebuggerRemoveXHRBreakpoint {
  -- | Resource URL substring.
  pDOMDebuggerRemoveXHRBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerRemoveXHRBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerRemoveXHRBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'DOMDebugger.removeXHRBreakpoint' command.
--   Removes breakpoint from XMLHttpRequest.
--   Parameters: 'PDOMDebuggerRemoveXHRBreakpoint'
dOMDebuggerRemoveXHRBreakpoint :: Handle ev -> PDOMDebuggerRemoveXHRBreakpoint -> IO ()
dOMDebuggerRemoveXHRBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeXHRBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerSetBreakOnCSPViolation' command.
data PDOMDebuggerSetBreakOnCSPViolation = PDOMDebuggerSetBreakOnCSPViolation {
  -- | CSP Violations to stop upon.
  pDOMDebuggerSetBreakOnCSPViolationViolationTypes :: [DOMDebuggerCSPViolationType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetBreakOnCSPViolation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetBreakOnCSPViolation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'DOMDebugger.setBreakOnCSPViolation' command.
--   Sets breakpoint on particular CSP violations.
--   Parameters: 'PDOMDebuggerSetBreakOnCSPViolation'
dOMDebuggerSetBreakOnCSPViolation :: Handle ev -> PDOMDebuggerSetBreakOnCSPViolation -> IO ()
dOMDebuggerSetBreakOnCSPViolation handle params = sendReceiveCommand handle "DOMDebugger.setBreakOnCSPViolation" (Just params)


-- | Parameters of the 'dOMDebuggerSetDOMBreakpoint' command.
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


-- | Function for the 'DOMDebugger.setDOMBreakpoint' command.
--   Sets breakpoint on particular operation with DOM.
--   Parameters: 'PDOMDebuggerSetDOMBreakpoint'
dOMDebuggerSetDOMBreakpoint :: Handle ev -> PDOMDebuggerSetDOMBreakpoint -> IO ()
dOMDebuggerSetDOMBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setDOMBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerSetEventListenerBreakpoint' command.
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


-- | Function for the 'DOMDebugger.setEventListenerBreakpoint' command.
--   Sets breakpoint on particular DOM event.
--   Parameters: 'PDOMDebuggerSetEventListenerBreakpoint'
dOMDebuggerSetEventListenerBreakpoint :: Handle ev -> PDOMDebuggerSetEventListenerBreakpoint -> IO ()
dOMDebuggerSetEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setEventListenerBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerSetInstrumentationBreakpoint' command.
data PDOMDebuggerSetInstrumentationBreakpoint = PDOMDebuggerSetInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pDOMDebuggerSetInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the 'DOMDebugger.setInstrumentationBreakpoint' command.
--   Sets breakpoint on particular native event.
--   Parameters: 'PDOMDebuggerSetInstrumentationBreakpoint'
dOMDebuggerSetInstrumentationBreakpoint :: Handle ev -> PDOMDebuggerSetInstrumentationBreakpoint -> IO ()
dOMDebuggerSetInstrumentationBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setInstrumentationBreakpoint" (Just params)


-- | Parameters of the 'dOMDebuggerSetXHRBreakpoint' command.
data PDOMDebuggerSetXHRBreakpoint = PDOMDebuggerSetXHRBreakpoint {
  -- | Resource URL substring. All XHRs having this substring in the URL will get stopped upon.
  pDOMDebuggerSetXHRBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDebuggerSetXHRBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMDebuggerSetXHRBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOMDebugger.setXHRBreakpoint' command.
--   Sets breakpoint on XMLHttpRequest.
--   Parameters: 'PDOMDebuggerSetXHRBreakpoint'
dOMDebuggerSetXHRBreakpoint :: Handle ev -> PDOMDebuggerSetXHRBreakpoint -> IO ()
dOMDebuggerSetXHRBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setXHRBreakpoint" (Just params)



