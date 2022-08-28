{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections  #-}
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


defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222)

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

newtype CommandId = CommandId String
    deriving (Eq, Ord, Show)
instance FromJSON CommandId where
    parseJSON = A.withObject "CommandId" $ \obj -> do
        CommandId <$> obj .: "id"
instance ToJSON CommandId where
    toJSON (CommandId id) = toJSON id

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

type ClientApp a = Session -> IO a
data Session = MkSession 
    { events         :: MVar (Map.Map String MissedHandler)
    , commandBuffer  :: MVar CommandBuffer
    , conn           :: WS.Connection
    , listenThread   :: ThreadId
    }
    
runClient :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort app = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi   <- getPageInfo endpoint
    eventsM <- newMVar Map.empty
    commandBuffer  <- newMVar Map.empty
    commandHistory <- newMVar Map.empty
    let (host, port, path) = parseUri . debuggerUrl $ pi
    
    WS.runClient host port path $ \conn -> do
        listenThread <- forkIO $ forever $ do
            res <- WS.fromDataMessage <$> WS.receiveDataMessage conn
            isCommand <- dispatchCommandResponse commandBuffer res
            if isCommand
                then pure ()
                else (error "no command") -- (dispatchEventResponse eventsM res) 
            pure ()
              
        let session = MkSession eventsM commandBuffer conn listenThread
        result <- app session
        killThread listenThread
        pure result

type CommandBuffer = Map.Map CommandId BS.ByteString
type MissedHandler = String

dispatchCommandResponse :: MVar CommandBuffer -> BS.ByteString -> IO Bool
dispatchCommandResponse commandBuffer res = do
    case A.decode res of
        Nothing  -> pure False
        Just cid@(CommandId id)  -> do
            updateCommandBuffer commandBuffer (Map.insert cid res)
            pure True

updateCommandBuffer :: MVar CommandBuffer -> (CommandBuffer -> CommandBuffer) -> IO ()
updateCommandBuffer commandBuffer f = ($ pure . f) . modifyMVar_ $ commandBuffer

-- decodeEvent :: BS.ByteString -> Maybe (EventResponse)
-- decodeEvent = A.decode

-- dispatchEventResponse :: MVar (Map.Map String MissedHandler) -> BS.ByteString -> IO ()
-- dispatchEventResponse handlers res = do
--     void $ go handlers . decodeEvent $ res
--   where
--     go :: MVar (Map.Map String MissedHandler) -> Maybe EventResponse -> IO ()
--     go handlers evr = maybe (pure ()) f evr
--       where
--         f (EventResponse p v) = do
--             evs <- readMVar handlers
--             let handler = maybe print id $ Map.lookup (eventName p) evs >>= handlerToF p
--             maybe (pure ()) handler v

updateEvents :: Session -> (Map.Map String MissedHandler -> Map.Map String MissedHandler) -> IO ()
updateEvents session f = ($ pure . f) . modifyMVar_ . events $ session

subscribe :: forall a. Event a => Session -> (a -> IO ()) -> IO ()
subscribe session h = updateEvents session $ Map.insert (eventName p) $ fToMissedHandler p h
  where
    p = (Proxy :: Proxy a)

unsubscribe :: Event a => Session -> Proxy a -> IO ()
unsubscribe session p = updateEvents session (Map.delete (eventName p))

class (FromJSON a, Show a, Eq a) => Event a where
    eventName         :: Proxy a  -> String
    fToMissedHandler  :: Proxy a  -> ((a -> IO ()) -> MissedHandler)
    handlerToF  :: Proxy a  -> MissedHandler -> Maybe (a -> IO ())

data EventResponse where
    EventResponse :: Event a => Proxy a -> Maybe a -> EventResponse

class (FromJSON b) => Command b where
    commandName               :: Proxy b -> String

data CommandResponse b where
    CommandResponse :: Command b => CommandId -> Maybe b -> CommandResponse b
instance (Show b) => Show (CommandResponse b) where
    show (CommandResponse id result) = "\nid: " <> show id <> "\nresult: " <> show result

newtype Error = Error String
    deriving Show

indent :: Int -> String -> String
indent = (<>) . flip replicate ' '

sendCommand :: forall a. (ToJSON a) => WS.Connection -> CommandId -> String -> Maybe a -> IO ()
sendCommand conn id name params = do
    WS.sendTextData conn . A.encode $ CommandObj id name params
  where
    paramsProxy = Proxy :: Proxy a
       
receiveResponse :: forall b. Command b => MVar CommandBuffer -> CommandId -> IO (CommandResponse b)
receiveResponse buffer id = untilJust $ do
    res <- Map.lookup id <$> readMVar buffer
    if isNothing res
        then threadDelay 1000
        else pure ()

    pure $ maybe (error "parse error") A.decode res          
  where
    p = Proxy :: Proxy b

sendReceiveCommandResult :: forall a b. (ToJSON a, Command b) => Session -> Maybe a -> IO (Either Error b)
sendReceiveCommandResult session params = do
    id <- CommandId <$> pure "1"  -- TODO: randomly generate
    sendCommand (conn session) id (commandName resultProxy) params 
    res <- (\((CommandResponse _ v)) -> v) <$> receiveResponse (commandBuffer session) id
    pure $ maybe (error "could not get response") Right res
  where
    resultProxy = Proxy :: Proxy b
    
sendReceiveCommand :: (ToJSON a) => Session -> Maybe a -> IO (Maybe Error)
sendReceiveCommand session params = do
    id <- CommandId <$> pure "1"  -- TODO: randomly generate
    sendCommand (conn session) id "a" params
    res <- (\((CommandResponse _ Nothing)) -> Nothing) <$> (receiveResponse (commandBuffer session) id :: IO (CommandResponse ()))
    pure $ maybe (Just . responseParseError $ params) 
        (const Nothing) res

responseParseError :: ToJSON a => a -> Error
responseParseError a = Error "" -- Error . unlines $ ["unable to parse response", commandToStr c]

instance (Command b) => FromJSON (CommandResponse b) where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId <- obj .: "id"
        CommandResponse (crId :: CommandId) <$> obj .:? "result"

browserClose :: Session -> IO (Maybe Error)
browserClose session  = sendReceiveCommand session (Nothing :: Maybe ())

instance Command () where
    commandName         _ = "unit"

data BrowserGetVersion = BrowserGetVersion {
    browserGetVersionProtocolVersion :: String,
    browserGetVersionProduct :: String,
    browserGetVersionRevision :: String,
    browserGetVersionUserAgent :: String,
    browserGetVersionJsVersion :: String
} deriving (Eq, Show, Read)
instance FromJSON  BrowserGetVersion where
    parseJSON = A.withObject "BrowserGetVersion" $ \v ->
            BrowserGetVersion <$> v .:  "protocolVersion"
            <*> v  .:  "product"
            <*> v  .:  "revision"
            <*> v  .:  "userAgent"
            <*> v  .:  "jsVersion"

browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session  = sendReceiveCommandResult session (Nothing :: Maybe ())
instance Command BrowserGetVersion where
    commandName _ = "Browser.getVersion"