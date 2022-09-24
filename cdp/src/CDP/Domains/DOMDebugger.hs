{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.DOMPageNetwork as DOMPageNetwork
import CDP.Domains.Runtime as Runtime


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



data DomDebuggerEventListener = DomDebuggerEventListener {
   domDebuggerEventListenerType :: String,
   domDebuggerEventListenerUseCapture :: Bool,
   domDebuggerEventListenerPassive :: Bool,
   domDebuggerEventListenerOnce :: Bool,
   domDebuggerEventListenerScriptId :: Runtime.RuntimeScriptId,
   domDebuggerEventListenerLineNumber :: Int,
   domDebuggerEventListenerColumnNumber :: Int,
   domDebuggerEventListenerHandler :: Maybe Runtime.RuntimeRemoteObject,
   domDebuggerEventListenerOriginalHandler :: Maybe Runtime.RuntimeRemoteObject,
   domDebuggerEventListenerBackendNodeId :: Maybe DOMPageNetwork.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomDebuggerEventListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomDebuggerEventListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }







data PDomDebuggerGetEventListeners = PDomDebuggerGetEventListeners {
   pDomDebuggerGetEventListenersObjectId :: Runtime.RuntimeRemoteObjectId,
   pDomDebuggerGetEventListenersDepth :: Maybe Int,
   pDomDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerGetEventListeners  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


domDebuggerGetEventListeners :: Handle ev -> PDomDebuggerGetEventListeners -> IO (Either Error DomDebuggerGetEventListeners)
domDebuggerGetEventListeners handle params = sendReceiveCommandResult handle "DOMDebugger.getEventListeners" (Just params)

data DomDebuggerGetEventListeners = DomDebuggerGetEventListeners {
   domDebuggerGetEventListenersListeners :: [DomDebuggerEventListener]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDebuggerGetEventListeners where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomDebuggerGetEventListeners where
   commandName _ = "DOMDebugger.getEventListeners"




data PDomDebuggerRemoveDomBreakpoint = PDomDebuggerRemoveDomBreakpoint {
   pDomDebuggerRemoveDomBreakpointNodeId :: DOMPageNetwork.DomNodeId,
   pDomDebuggerRemoveDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


domDebuggerRemoveDomBreakpoint :: Handle ev -> PDomDebuggerRemoveDomBreakpoint -> IO (Maybe Error)
domDebuggerRemoveDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeDOMBreakpoint" (Just params)



data PDomDebuggerRemoveEventListenerBreakpoint = PDomDebuggerRemoveEventListenerBreakpoint {
   pDomDebuggerRemoveEventListenerBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }


domDebuggerRemoveEventListenerBreakpoint :: Handle ev -> PDomDebuggerRemoveEventListenerBreakpoint -> IO (Maybe Error)
domDebuggerRemoveEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeEventListenerBreakpoint" (Just params)



data PDomDebuggerRemoveXhrBreakpoint = PDomDebuggerRemoveXhrBreakpoint {
   pDomDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerRemoveXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerRemoveXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


domDebuggerRemoveXhrBreakpoint :: Handle ev -> PDomDebuggerRemoveXhrBreakpoint -> IO (Maybe Error)
domDebuggerRemoveXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.removeXHRBreakpoint" (Just params)



data PDomDebuggerSetDomBreakpoint = PDomDebuggerSetDomBreakpoint {
   pDomDebuggerSetDomBreakpointNodeId :: DOMPageNetwork.DomNodeId,
   pDomDebuggerSetDomBreakpointType :: DomDebuggerDomBreakpointType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetDomBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetDomBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domDebuggerSetDomBreakpoint :: Handle ev -> PDomDebuggerSetDomBreakpoint -> IO (Maybe Error)
domDebuggerSetDomBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setDOMBreakpoint" (Just params)



data PDomDebuggerSetEventListenerBreakpoint = PDomDebuggerSetEventListenerBreakpoint {
   pDomDebuggerSetEventListenerBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetEventListenerBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetEventListenerBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


domDebuggerSetEventListenerBreakpoint :: Handle ev -> PDomDebuggerSetEventListenerBreakpoint -> IO (Maybe Error)
domDebuggerSetEventListenerBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setEventListenerBreakpoint" (Just params)



data PDomDebuggerSetXhrBreakpoint = PDomDebuggerSetXhrBreakpoint {
   pDomDebuggerSetXhrBreakpointUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDebuggerSetXhrBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomDebuggerSetXhrBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domDebuggerSetXhrBreakpoint :: Handle ev -> PDomDebuggerSetXhrBreakpoint -> IO (Maybe Error)
domDebuggerSetXhrBreakpoint handle params = sendReceiveCommand handle "DOMDebugger.setXHRBreakpoint" (Just params)



