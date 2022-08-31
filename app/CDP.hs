{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
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

module CDP (module CDP) where

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


defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222) 

-- :CONFIG
hostPortToEndpoint :: (String, Int) -> Http.Request
hostPortToEndpoint (host, port) = Http.parseRequest_ . 
    ("GET " <>) . 
    mconcat $ [host, ":", show port, "/json"]

data PageInfo = PageInfo
    { debuggerUrl :: String
    } deriving Show
instance FromJSON PageInfo where
    parseJSON = A.withObject "PageInfo" $ \v ->
        PageInfo <$> v .: "webSocketDebuggerUrl"

parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of
            (':':str)   -> read str
            _           -> 80
    pure (Uri.uriRegName auth, port, Uri.uriPath u)

getPageInfo :: Http.Request -> IO PageInfo
getPageInfo request = do
    response <- Http.httpLBS request
    let body = Http.getResponseBody response
    case A.decode body of
        Just mpis -> pure $ head . catMaybes $ mpis
        Nothing   -> error "getPageInfo: Parse error"

data ToJSONEx where
   ToJSONEx :: (ToJSON a, Show a) => a -> ToJSONEx
instance ToJSON ToJSONEx where
    toJSON (ToJSONEx v) = toJSON v
instance Show ToJSONEx where
    show (ToJSONEx v) = show v

newtype CommandId = CommandId { unCommandId :: Int }
    deriving (Eq, Ord, Show, ToJSON)

instance FromJSON CommandId where
    parseJSON = A.withObject "CommandId" $ \obj -> do
        CommandId <$> obj .: "id"

data CommandObj a = CommandObj {
      coId :: CommandId
    , coMethod :: String
    , coParams :: Maybe a
    } deriving Show

instance (ToJSON a) => ToJSON (CommandObj a) where
   toJSON cmd = A.object . concat $
        [ [ "id"     .= coId cmd ]
        , [ "method" .= coMethod cmd ]
        , maybe [] (\p -> [ "params" .= p ]) $ coParams cmd
        ]

randomCommandId :: MVar StdGen -> IO CommandId
randomCommandId mg = modifyMVar mg $ \g -> do
    let (id, g2) = uniformR (0 :: Int, 1000 :: Int) g
    pure (g2, CommandId id)

class (FromJSON b) => Command b where
    commandName               :: Proxy b -> String

data CommandResponse b where
    CommandResponse :: Command b => CommandId -> Maybe b -> CommandResponse b
instance (Show b) => Show (CommandResponse b) where
    show (CommandResponse id result) = "\nid: " <> show id <> "\nresult: " <> show result

newtype Error = Error String
    deriving Show

newtype InternalError = InternalError String
    deriving Show

indent :: Int -> String -> String
indent = (<>) . flip replicate ' '

sendCommand :: forall a. (ToJSON a) => WS.Connection -> CommandId -> String -> Maybe a -> IO ()
sendCommand conn id name params = do
    let co = CommandObj id name params
    WS.sendTextData conn . A.encode $ co
    where
    paramsProxy = Proxy :: Proxy a
        
receiveResponse :: forall b. Command b => MVar CommandBuffer -> CommandId -> IO (Either InternalError (CommandResponse b))
receiveResponse buffer id = do
    bs <- untilJust $ do
            bsM <- Map.lookup id <$> readMVar buffer
            if isNothing bsM
                then threadDelay 1000 -- :CONFIG
                else pure ()

            pure bsM

    pure $ maybe (Left . InternalError $ "error in parsing response") Right . A.decode $ bs
    where
    p = Proxy :: Proxy b

sendReceiveCommandResult :: forall a b s. (ToJSON a, Command b) => Session s -> String -> Maybe a -> IO (Either Error b)
sendReceiveCommandResult session name params = do
    id <- randomCommandId $ randomGen session
    sendCommand (conn session) id name params
    response <- receiveResponse (commandBuffer session) id
    pure $ case response of
        Left _ -> Left . Error $ "internal error"
        Right (CommandResponse _ resultM) -> maybe (Left . Error $ "error in parsing result") Right $ resultM    
    where
    resultProxy = Proxy :: Proxy b
    
sendReceiveCommand :: (ToJSON a) => Session s -> String -> Maybe a -> IO (Maybe Error)
sendReceiveCommand session name params = do
    id <- randomCommandId $ randomGen session
    sendCommand (conn session) id name params
    response <- receiveResponse (commandBuffer session) id :: IO (Either InternalError (CommandResponse NoResponse))
    pure $ case response of
        Left _                            -> Just . Error $ "internal error"
        Right (CommandResponse _ resultM) -> maybe Nothing (const . Just . Error $ "got an unexpected result") $ resultM  

instance (Command b) => FromJSON (CommandResponse b) where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId <- CommandId <$> obj .: "id"
        CommandResponse (crId :: CommandId) <$> obj .:? "result"

data NoResponse = NoResponse
instance Command NoResponse where
    commandName _ = "noresponse"
instance FromJSON NoResponse where
    parseJSON _ = fail "noresponse"    

type ClientApp a b = Session a -> IO b
data Session s = MkSession 
    { randomGen      :: MVar StdGen
    , events         :: MVar (Map.Map String (s -> IO ()))
    , commandBuffer  :: MVar CommandBuffer
    , conn           :: WS.Connection
    , listenThread   :: ThreadId
    }
    
runClient :: Maybe (String, Int) -> ClientApp EventReturn a -> IO a
runClient hostPort app = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi             <- getPageInfo endpoint
    randomGen      <- newMVar . mkStdGen $ 42
    eventsM        <- newMVar Map.empty
    commandBuffer  <- newMVar Map.empty
    let (host, port, path) = parseUri . debuggerUrl $ pi
    
    WS.runClient host port path $ \conn -> do
        let act = forever $ do
                res <- WS.fromDataMessage <$> WS.receiveDataMessage conn
                if isCommand res
                    then dispatchCommandResponse commandBuffer res
                    else dispatchEventResponse eventsM res 
            
        listenThread <- forkIO act
        let session = MkSession randomGen eventsM commandBuffer conn listenThread
        result <- app session
        killThread listenThread
        pure result

type CommandBuffer = Map.Map CommandId BS.ByteString

isCommand :: BS.ByteString -> Bool
isCommand res = do
    case A.decode res of
        Nothing            -> False
        Just (CommandId _) -> True
        
dispatchCommandResponse :: MVar CommandBuffer -> BS.ByteString -> IO ()
dispatchCommandResponse commandBuffer res = do
    case A.decode res of
        Nothing  -> error "not a command"
        Just cid@(CommandId id)  -> do
            updateCommandBuffer commandBuffer (Map.insert cid res)
            
updateCommandBuffer :: MVar CommandBuffer -> (CommandBuffer -> CommandBuffer) -> IO ()
updateCommandBuffer commandBuffer f = ($ pure . f) . modifyMVar_ $ commandBuffer

dispatchEventResponse :: forall s. (FromJSON (EventResponse s)) => MVar (Map.Map String (s -> IO ())) -> BS.ByteString -> IO ()
dispatchEventResponse handlers res = do
    void $ go handlers . A.decode $ res
  where
    go :: forall s. (FromJSON (EventResponse s)) => MVar (Map.Map String (s -> IO ())) -> Maybe (EventResponse s) -> IO ()
    go handlers evr = maybe (pure ()) f evr
      where
        ps = Proxy :: Proxy s
        -- g = evrToA ps p
        f (EventResponse ps p v) = do
            evs <- readMVar handlers
            let handler = maybe print id $ Map.lookup (eventName ps p) evs -- >>= evrToA ps p
            maybe (pure ()) handler v

updateEvents :: Session a -> (Map.Map String (a -> IO ()) -> Map.Map String (a -> IO ())) -> IO ()
updateEvents session f = ($ pure . f) . modifyMVar_ . events $ session

subscribe :: forall a s. Event s a => Session s -> (a -> IO ()) -> IO ()
subscribe session h = updateEvents session $ Map.insert (eventName ps p) handler
  where
    handler = maybe (pure ()) h . (evrToA ps p)
    p  = (Proxy :: Proxy a)
    ps = (Proxy :: Proxy s)

unsubscribe :: forall s a. Event s a => Session s -> Proxy a -> IO ()
unsubscribe session p = updateEvents session (Map.delete (eventName ps p))
  where
    ps = Proxy :: Proxy s

class (FromJSON a, Show a, Eq a) => Event s a | a -> s where
    eventName     :: Proxy s -> Proxy a  -> String
    evrToA        :: Proxy s -> Proxy a -> s -> Maybe a

data EventResponse s where
    EventResponse :: (Show s, Show a, Event s a) => Proxy s -> Proxy a -> Maybe s -> EventResponse s

data EventReturn = HDOMAttributeModified DOMAttributeModified
    deriving Show

instance FromJSON (EventResponse EventReturn) where
    parseJSON = A.withObject  "EventResponse"  $ \obj -> do
        name <- obj .: "method"
        -- error ""
        case (name :: String) of
                "DOM.attributeModified" -> EventResponse (Proxy :: Proxy EventReturn) (Proxy :: Proxy DOMAttributeModified) . fmap HDOMAttributeModified <$> obj .:? "params"
                _ -> fail "failed to parse EventResponse"

data DOMAttributeModified = DOMAttributeModified {
    domAttributeModifiedNodeId :: String,
    domAttributeModifiedName   :: String,
    domAttributeModifiedValue  :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMAttributeModified where
    parseJSON = A.withObject "DOMAttributeModified" $ \v ->
         DOMAttributeModified <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"

instance ToJSON DOMAttributeModified  where
    toJSON v = A.object
        [ "nodeId" .= domAttributeModifiedNodeId v
        , "name" .= domAttributeModifiedName v
        , "value" .= domAttributeModifiedValue v
        ]

instance Event EventReturn  DOMAttributeModified where
    eventName  _ _   =  "DOM.attributeModified"
    evrToA _ _ evr =  case evr of HDOMAttributeModified v -> Just v; _ -> Nothing

