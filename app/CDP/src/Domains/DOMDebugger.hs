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

module Domains.DOMDebugger (module Domains.DOMDebugger) where

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

import qualified Domains.DOM as DOM
import qualified Domains.Debugger as Debugger
import qualified Domains.Runtime as Runtime


data DOMDebuggerEvent = 
    deriving (Eq, Show, Read)



subscribe :: forall a. FromEvent DOMDebuggerEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy DOMDebuggerEvent
    pa       = Proxy :: Proxy a


data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
    deriving (Eq, Show, Read)
instance FromJSON DOMDebuggerDOMBreakpointType where
    parseJSON = A.withText  "DOMDebuggerDOMBreakpointType"  $ \v -> do
        case v of
                "subtree-modified" -> pure $ DOMDebuggerDOMBreakpointTypeSubtreeModified
                "attribute-modified" -> pure $ DOMDebuggerDOMBreakpointTypeAttributeModified
                "node-removed" -> pure $ DOMDebuggerDOMBreakpointTypeNodeRemoved
                _ -> fail "failed to parse DOMDebuggerDOMBreakpointType"

instance ToJSON DOMDebuggerDOMBreakpointType where
    toJSON v = A.String $
        case v of
                DOMDebuggerDOMBreakpointTypeSubtreeModified -> "subtree-modified"
                DOMDebuggerDOMBreakpointTypeAttributeModified -> "attribute-modified"
                DOMDebuggerDOMBreakpointTypeNodeRemoved -> "node-removed"



data DOMDebuggerEventListener = DOMDebuggerEventListener {
    domDebuggerEventListenerType :: String,
    domDebuggerEventListenerUseCapture :: Bool,
    domDebuggerEventListenerPassive :: Bool,
    domDebuggerEventListenerOnce :: Bool,
    domDebuggerEventListenerScriptId :: RuntimeScriptId,
    domDebuggerEventListenerLineNumber :: Int,
    domDebuggerEventListenerColumnNumber :: Int,
    domDebuggerEventListenerHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerOriginalHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerBackendNodeId :: Maybe DOMBackendNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMDebuggerEventListener where
    parseJSON = A.withObject "DOMDebuggerEventListener" $ \v ->
         DOMDebuggerEventListener <$> v .:  "type"
            <*> v  .:  "useCapture"
            <*> v  .:  "passive"
            <*> v  .:  "once"
            <*> v  .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "handler"
            <*> v  .:?  "originalHandler"
            <*> v  .:?  "backendNodeId"


instance ToJSON DOMDebuggerEventListener  where
    toJSON v = A.object
        [ "type" .= domDebuggerEventListenerType v
        , "useCapture" .= domDebuggerEventListenerUseCapture v
        , "passive" .= domDebuggerEventListenerPassive v
        , "once" .= domDebuggerEventListenerOnce v
        , "scriptId" .= domDebuggerEventListenerScriptId v
        , "lineNumber" .= domDebuggerEventListenerLineNumber v
        , "columnNumber" .= domDebuggerEventListenerColumnNumber v
        , "handler" .= domDebuggerEventListenerHandler v
        , "originalHandler" .= domDebuggerEventListenerOriginalHandler v
        , "backendNodeId" .= domDebuggerEventListenerBackendNodeId v
        ]



data DOMDebuggerGetEventListeners = DOMDebuggerGetEventListeners {
    dOMDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving (Eq, Show, Read)
instance FromJSON  DOMDebuggerGetEventListeners where
    parseJSON = A.withObject "DOMDebuggerGetEventListeners" $ \v ->
         DOMDebuggerGetEventListeners <$> v .:  "listeners"



instance Command  DOMDebuggerGetEventListeners where
    commandName _ = "DOMDebugger.getEventListeners"

data PDOMDebuggerGetEventListeners = PDOMDebuggerGetEventListeners {
    pdomDebuggerGetEventListenersObjectId :: RuntimeRemoteObjectId,
    pdomDebuggerGetEventListenersDepth :: Maybe Int,
    pdomDebuggerGetEventListenersPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerGetEventListeners where
    parseJSON = A.withObject "PDOMDebuggerGetEventListeners" $ \v ->
         PDOMDebuggerGetEventListeners <$> v .:  "objectId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMDebuggerGetEventListeners  where
    toJSON v = A.object
        [ "objectId" .= pdomDebuggerGetEventListenersObjectId v
        , "depth" .= pdomDebuggerGetEventListenersDepth v
        , "pierce" .= pdomDebuggerGetEventListenersPierce v
        ]


dOMDebuggerGetEventListeners :: Session -> PDOMDebuggerGetEventListeners -> IO (Either Error DOMDebuggerGetEventListeners)
dOMDebuggerGetEventListeners session params = sendReceiveCommandResult session "DOMDebugger.getEventListeners" (Just params)



data PDOMDebuggerRemoveDomBreakpoint = PDOMDebuggerRemoveDomBreakpoint {
    pdomDebuggerRemoveDomBreakpointNodeId :: DOMNodeId,
    pdomDebuggerRemoveDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveDomBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveDomBreakpoint" $ \v ->
         PDOMDebuggerRemoveDomBreakpoint <$> v .:  "nodeId"
            <*> v  .:  "type"


instance ToJSON PDOMDebuggerRemoveDomBreakpoint  where
    toJSON v = A.object
        [ "nodeId" .= pdomDebuggerRemoveDomBreakpointNodeId v
        , "type" .= pdomDebuggerRemoveDomBreakpointType v
        ]


dOMDebuggerRemoveDomBreakpoint :: Session -> PDOMDebuggerRemoveDomBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveDomBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeDOMBreakpoint" (Just params)



data PDOMDebuggerRemoveEventListenerBreakpoint = PDOMDebuggerRemoveEventListenerBreakpoint {
    pdomDebuggerRemoveEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveEventListenerBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveEventListenerBreakpoint" $ \v ->
         PDOMDebuggerRemoveEventListenerBreakpoint <$> v .:  "eventName"


instance ToJSON PDOMDebuggerRemoveEventListenerBreakpoint  where
    toJSON v = A.object
        [ "eventName" .= pdomDebuggerRemoveEventListenerBreakpointEventName v
        ]


dOMDebuggerRemoveEventListenerBreakpoint :: Session -> PDOMDebuggerRemoveEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveEventListenerBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeEventListenerBreakpoint" (Just params)



data PDOMDebuggerRemoveXhrBreakpoint = PDOMDebuggerRemoveXhrBreakpoint {
    pdomDebuggerRemoveXhrBreakpointUrl :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerRemoveXhrBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerRemoveXhrBreakpoint" $ \v ->
         PDOMDebuggerRemoveXhrBreakpoint <$> v .:  "url"


instance ToJSON PDOMDebuggerRemoveXhrBreakpoint  where
    toJSON v = A.object
        [ "url" .= pdomDebuggerRemoveXhrBreakpointUrl v
        ]


dOMDebuggerRemoveXhrBreakpoint :: Session -> PDOMDebuggerRemoveXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerRemoveXhrBreakpoint session params = sendReceiveCommand session "DOMDebugger.removeXHRBreakpoint" (Just params)



data PDOMDebuggerSetDomBreakpoint = PDOMDebuggerSetDomBreakpoint {
    pdomDebuggerSetDomBreakpointNodeId :: DOMNodeId,
    pdomDebuggerSetDomBreakpointType :: DOMDebuggerDOMBreakpointType
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetDomBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetDomBreakpoint" $ \v ->
         PDOMDebuggerSetDomBreakpoint <$> v .:  "nodeId"
            <*> v  .:  "type"


instance ToJSON PDOMDebuggerSetDomBreakpoint  where
    toJSON v = A.object
        [ "nodeId" .= pdomDebuggerSetDomBreakpointNodeId v
        , "type" .= pdomDebuggerSetDomBreakpointType v
        ]


dOMDebuggerSetDomBreakpoint :: Session -> PDOMDebuggerSetDomBreakpoint -> IO (Maybe Error)
dOMDebuggerSetDomBreakpoint session params = sendReceiveCommand session "DOMDebugger.setDOMBreakpoint" (Just params)



data PDOMDebuggerSetEventListenerBreakpoint = PDOMDebuggerSetEventListenerBreakpoint {
    pdomDebuggerSetEventListenerBreakpointEventName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetEventListenerBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetEventListenerBreakpoint" $ \v ->
         PDOMDebuggerSetEventListenerBreakpoint <$> v .:  "eventName"


instance ToJSON PDOMDebuggerSetEventListenerBreakpoint  where
    toJSON v = A.object
        [ "eventName" .= pdomDebuggerSetEventListenerBreakpointEventName v
        ]


dOMDebuggerSetEventListenerBreakpoint :: Session -> PDOMDebuggerSetEventListenerBreakpoint -> IO (Maybe Error)
dOMDebuggerSetEventListenerBreakpoint session params = sendReceiveCommand session "DOMDebugger.setEventListenerBreakpoint" (Just params)



data PDOMDebuggerSetXhrBreakpoint = PDOMDebuggerSetXhrBreakpoint {
    pdomDebuggerSetXhrBreakpointUrl :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDebuggerSetXhrBreakpoint where
    parseJSON = A.withObject "PDOMDebuggerSetXhrBreakpoint" $ \v ->
         PDOMDebuggerSetXhrBreakpoint <$> v .:  "url"


instance ToJSON PDOMDebuggerSetXhrBreakpoint  where
    toJSON v = A.object
        [ "url" .= pdomDebuggerSetXhrBreakpointUrl v
        ]


dOMDebuggerSetXhrBreakpoint :: Session -> PDOMDebuggerSetXhrBreakpoint -> IO (Maybe Error)
dOMDebuggerSetXhrBreakpoint session params = sendReceiveCommand session "DOMDebugger.setXHRBreakpoint" (Just params)

