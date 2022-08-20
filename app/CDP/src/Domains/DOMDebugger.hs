{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.DOMDebugger (module Domains.DOMDebugger) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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

import qualified Domains.DOM as DOM
import qualified Domains.Debugger as Debugger
import qualified Domains.Runtime as Runtime


import Utils


data DOMBreakpointType = DOMBreakpointTypeSubtreeModified | DOMBreakpointTypeAttributeModified | DOMBreakpointTypeNodeRemoved
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON DOMBreakpointType where
    parseJSON = A.withText  "DOMBreakpointType"  $ \v -> do
        pure $ case v of
                "subtree-modified" -> DOMBreakpointTypeSubtreeModified
                "attribute-modified" -> DOMBreakpointTypeAttributeModified
                "node-removed" -> DOMBreakpointTypeNodeRemoved
                _ -> error "failed to parse DOMBreakpointType"

instance ToJSON DOMBreakpointType where
    toJSON v = A.String $
        case v of
                DOMBreakpointTypeSubtreeModified -> "subtree-modified"
                DOMBreakpointTypeAttributeModified -> "attribute-modified"
                DOMBreakpointTypeNodeRemoved -> "node-removed"



data EventListener = EventListener {
    eventListenerType :: String,
    eventListenerUseCapture :: Bool,
    eventListenerPassive :: Bool,
    eventListenerOnce :: Bool,
    eventListenerScriptId :: Runtime.ScriptId,
    eventListenerLineNumber :: Int,
    eventListenerColumnNumber :: Int,
    eventListenerHandler :: Maybe Runtime.RemoteObject,
    eventListenerOriginalHandler :: Maybe Runtime.RemoteObject,
    eventListenerBackendNodeId :: Maybe DOM.BackendNodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  EventListener where
    parseJSON = A.withObject "EventListener" $ \v ->
         EventListener <$> v .:  "type"
            <*> v  .:  "useCapture"
            <*> v  .:  "passive"
            <*> v  .:  "once"
            <*> v  .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "handler"
            <*> v  .:?  "originalHandler"
            <*> v  .:?  "backendNodeId"


instance ToJSON EventListener  where
    toJSON v = A.object
        [ "type" .= eventListenerType v
        , "useCapture" .= eventListenerUseCapture v
        , "passive" .= eventListenerPassive v
        , "once" .= eventListenerOnce v
        , "scriptId" .= eventListenerScriptId v
        , "lineNumber" .= eventListenerLineNumber v
        , "columnNumber" .= eventListenerColumnNumber v
        , "handler" .= eventListenerHandler v
        , "originalHandler" .= eventListenerOriginalHandler v
        , "backendNodeId" .= eventListenerBackendNodeId v
        ]


data GetEventListeners = GetEventListeners {
    getEventListenersListeners :: [EventListener]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetEventListeners where
    parseJSON = A.withObject "GetEventListeners" $ \v ->
         GetEventListeners <$> v .:  "listeners"



getEventListeners :: Session a -> Runtime.RemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error GetEventListeners)
getEventListeners session getEventListenersObjectId getEventListenersDepth getEventListenersPierce = sendReceiveCommandResult (conn session) ("DOMDebugger","getEventListeners") ([("objectId", ToJSONEx getEventListenersObjectId)] ++ (catMaybes [fmap (("depth",) . ToJSONEx) getEventListenersDepth, fmap (("pierce",) . ToJSONEx) getEventListenersPierce]))


removeDOMBreakpoint :: Session a -> DOM.NodeId -> DOMBreakpointType -> IO (Maybe Error)
removeDOMBreakpoint session removeDomBreakpointNodeId removeDomBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","removeDOMBreakpoint") ([("nodeId", ToJSONEx removeDomBreakpointNodeId), ("type", ToJSONEx removeDomBreakpointType)] ++ (catMaybes []))


removeEventListenerBreakpoint :: Session a -> String -> IO (Maybe Error)
removeEventListenerBreakpoint session removeEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","removeEventListenerBreakpoint") ([("eventName", ToJSONEx removeEventListenerBreakpointEventName)] ++ (catMaybes []))


removeXHRBreakpoint :: Session a -> String -> IO (Maybe Error)
removeXHRBreakpoint session removeXhrBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","removeXHRBreakpoint") ([("url", ToJSONEx removeXhrBreakpointUrl)] ++ (catMaybes []))


setDOMBreakpoint :: Session a -> DOM.NodeId -> DOMBreakpointType -> IO (Maybe Error)
setDOMBreakpoint session setDomBreakpointNodeId setDomBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","setDOMBreakpoint") ([("nodeId", ToJSONEx setDomBreakpointNodeId), ("type", ToJSONEx setDomBreakpointType)] ++ (catMaybes []))


setEventListenerBreakpoint :: Session a -> String -> IO (Maybe Error)
setEventListenerBreakpoint session setEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","setEventListenerBreakpoint") ([("eventName", ToJSONEx setEventListenerBreakpointEventName)] ++ (catMaybes []))


setXHRBreakpoint :: Session a -> String -> IO (Maybe Error)
setXHRBreakpoint session setXhrBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","setXHRBreakpoint") ([("url", ToJSONEx setXhrBreakpointUrl)] ++ (catMaybes []))


