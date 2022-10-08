{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module CDP.Internal.Runtime (module CDP.Internal.Runtime) where

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
import qualified Network.WebSockets as WS
import Control.Concurrent
import qualified Text.Casing as C
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import Control.Applicative
import Data.Default
import Control.Exception
import System.Timeout
import Data.Char

import CDP.Internal.Endpoints

type EventHandlerId   = (String, Maybe SessionId) 
type EventHandlers ev = Map.Map EventHandlerId (ev -> IO ())
type CommandBuffer    = Map.Map CommandId BS.ByteString

data Handle' ev = MkHandle
    { config            :: Config
    , randomGen        :: MVar StdGen
    , eventHandlers    :: MVar (EventHandlers ev)
    , commandBuffer    :: MVar CommandBuffer
    , conn             :: WS.Connection
    , listenThread     :: ThreadId
    , responseBuffer   :: MVar [(String, BS.ByteString)]
    }

data Config = Config
    { hostPort              :: (String, Int)
    , doLogResponses        :: Bool
    , commandTimeout        :: Maybe Int
        -- ^ number of microseconds to wait for a command response.
        --   waits forever if Nothing 
    } deriving Show
instance Default Config where
    def = Config{..}
      where
        hostPort       = ("http://127.0.0.1", 9222) 
        doLogResponses = False
        commandTimeout = def

type FromJSONEvent ev = FromJSON (EventResponse ev)
type ClientApp' ev b  = Handle' ev -> IO b
runClient' :: forall ev b. FromJSONEvent ev => Config -> ClientApp' ev b -> IO b
runClient' config app = do
    randomGen      <- newMVar . mkStdGen $ 42
    eventHandlers  <- newMVar Map.empty
    commandBuffer  <- newMVar Map.empty
    responseBuffer <- newMVar []

    let endpoint = hostPortToEndpoint $ hostPort config
    pi <- getPageInfo endpoint
    let (host, port, path) = parseUri . debuggerUrl $ pi

    WS.runClient host port path $ \conn -> do
        let listen = forkIO $ do
                listenThread <- myThreadId
                forever $ do
                    bs <- WS.fromDataMessage <$> WS.receiveDataMessage conn
                    if isCommand bs
                        then dispatchCommandResponse commandBuffer bs
                        else dispatchEventResponse MkHandle{..} bs
        
        bracket listen killThread (\listenThread -> app MkHandle{..})
      
isCommand :: BS.ByteString -> Bool
isCommand bs = do
    case A.decode bs of
        Nothing            -> False
        Just (CommandId _) -> True
        
dispatchCommandResponse :: MVar CommandBuffer -> BS.ByteString -> IO ()
dispatchCommandResponse commandBuffer bs = do
    case A.decode bs of
        Nothing  -> error "not a command"
        Just cid@(CommandId id)  -> do
            updateCommandBuffer commandBuffer (Map.insert cid bs)
            
updateCommandBuffer :: MVar CommandBuffer -> (CommandBuffer -> CommandBuffer) -> IO ()
updateCommandBuffer commandBuffer f = ($ pure . f) . modifyMVar_ $ commandBuffer

updateCommandBufferM :: MVar CommandBuffer -> (CommandBuffer -> IO (CommandBuffer, a)) -> IO a
updateCommandBufferM = modifyMVar

logResponse :: forall ev. Handle' ev -> String -> BS.ByteString -> IO ()
logResponse handle s bs = if (doLogResponses (config handle)) 
        then modifyMVar_ (responseBuffer handle) $ pure . (++ [(s, bs)])
        else pure ()

dispatchEventResponse :: forall ev. FromJSONEvent ev => Handle' ev -> BS.ByteString -> IO ()
dispatchEventResponse handle bs = void $ go handle . A.decode $ bs
  where
    go :: forall ev. FromJSONEvent ev => Handle' ev -> Maybe (EventResponse ev) -> IO ()
    go handle evr = maybe (pure ()) f evr -- TODO: throw error
      where
        f (EventResponse ps p sessionIdM v) = do
            evs <- readMVar (eventHandlers handle)
            let handlerM = Map.lookup (eventName ps p, sessionIdM) evs
            if isJust handlerM
                then logResponse handle (eventName ps p) bs
                else pure ()
            
            let handler = fromMaybe print handlerM -- :CONFIG
            maybe (pure ()) handler v

updateEventHandlers :: forall ev. FromJSONEvent ev => Handle' ev 
    -> (EventHandlers ev -> EventHandlers ev) 
    -> IO ()
updateEventHandlers handle f = ($ pure . f) . modifyMVar_ . eventHandlers $ handle

subscribe' :: forall ev a . (FromJSONEvent ev, FromEvent ev a) => Handle' ev -> Maybe SessionId -> (a -> IO ()) -> IO ()
subscribe' handle sessionId handler1 = updateEventHandlers handle $ Map.insert (eventName ps p, sessionId) handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    p  = (Proxy :: Proxy a)
    ps = (Proxy :: Proxy s)

unsubscribe' :: forall ev a. (FromJSONEvent ev, FromEvent ev a) => Handle' ev -> Maybe SessionId -> Proxy a -> IO ()
unsubscribe' handle sessionId p = updateEventHandlers handle (Map.delete (eventName ps p, sessionId))
  where
    ps = Proxy :: Proxy ev

class (FromJSON a) => FromEvent ev a | a -> ev where
    eventName     :: Proxy ev -> Proxy a -> String
    fromEvent     :: ev       -> Maybe a

data EventResponse ev where
    EventResponse :: (Show ev, Show a, FromEvent ev a) => Proxy ev -> Proxy a -> Maybe SessionId -> Maybe ev -> EventResponse ev


newtype CommandId = CommandId { unCommandId :: Int }
    deriving (Eq, Ord, Show, ToJSON)

instance FromJSON CommandId where
    parseJSON = A.withObject "CommandId" $ \obj -> do
        CommandId <$> obj .: "id"

type SessionId = String
data CommandObj a = CommandObj {
      coId        :: CommandId
    , coSessionId :: Maybe SessionId
    , coMethod    :: String
    , coParams    :: Maybe a
    } deriving Show

instance (ToJSON a) => ToJSON (CommandObj a) where
   toJSON cmd = A.object . concat $
        [ [ "id"        .= coId cmd ]
        , [ "method"    .= coMethod cmd ]
        , maybe [] (\sid -> [ "sessionId" .= sid ]) $ coSessionId cmd
        , maybe [] (\p   -> [ "params" .= p ]) $ coParams cmd
        ]

randomCommandId :: Handle' ev -> IO CommandId
randomCommandId handle = modifyMVar (randomGen handle) $ \g -> do
    let (id, g2) = uniformR (0 :: Int, 1000000 :: Int) g -- TODO:
    pure (g2, CommandId id)

class (FromJSON b) => Command b where
    commandName :: Proxy b -> String

data CommandResponse b = CommandResponse 
    { crId        :: CommandId
    , crResult    :: Either ProtocolError b
    , crSessionId :: Maybe SessionId
    }
    deriving (Eq, Show)

instance (Command b) => FromJSON (CommandResponse b) where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId        <- CommandId <$> obj .: "id"
        crSessionId <- obj .:? "sessionId"
        crResult    <- (Left <$> (obj .: "error")) <|> (Right <$> (obj .: "result"))
        pure CommandResponse{..}
        
data NoResponse = NoResponse
    deriving (Eq, Show)
instance Command NoResponse where
    commandName _ = "noresponse"
instance FromJSON NoResponse where
    parseJSON _ = pure NoResponse

data ProtocolError = 
    PEParse          String      -- ^ Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text
  | PEInvalidRequest String      -- ^ The JSON sent is not a valid Request object
  | PEMethodNotFound String      -- ^ The method does not exist / is not available
  | PEInvalidParams  String      -- ^ Invalid method parameter (s)
  | PEInternalError  String      -- ^ Internal JSON-RPC error
  | PEServerError    String      -- ^ Server error
  | PEOther          String      -- ^ An uncategorized error
  deriving Eq

instance Exception ProtocolError

instance Show ProtocolError where
    show (PEParse msg)            = unlines ["Server parsing protocol error:", msg] 
    show (PEInvalidRequest msg)   = unlines ["Invalid request protocol error:", msg]
    show (PEMethodNotFound msg)   = unlines ["Method not found protocol error:", msg]
    show (PEInvalidParams msg)    = unlines ["Invalid params protocol error:", msg]
    show (PEInternalError msg)    = unlines ["Internal protocol error:", msg]
    show (PEServerError msg)      = unlines ["Server protocol error:", msg]
    show (PEOther msg)            = unlines ["Other protocol error:", msg]

instance FromJSON ProtocolError where
    parseJSON = A.withObject "ProtocolError" $ \obj -> do
        code <- obj .: "code"
        msg  <- obj .: "message"
        pure $ case (code :: Double) of
            -32700 -> PEParse          msg
            -32600 -> PEInvalidRequest msg
            -32601 -> PEMethodNotFound msg
            -32602 -> PEInvalidParams  msg
            -32603 -> PEInternalError  msg
            _      -> if code > -32099 && code < -32000 then PEServerError msg else PEOther msg

data Error = 
      ERRNoResponse
    | ERRParse String
    | ERRProtocol ProtocolError
    deriving Eq

instance Exception Error

instance Show Error where
    show ERRNoResponse      = "no response received from the protocol"
    show (ERRParse msg)     = unlines ["error in parsing a message received from the protocol:", msg]
    show (ERRProtocol pe)   = show pe 

sendCommand :: forall a. (Show a, ToJSON a) => WS.Connection -> CommandId -> Maybe SessionId -> String -> Maybe a -> IO ()
sendCommand conn cid sessionId name params = do
    let co = CommandObj { 
              coId        = cid
            , coSessionId = sessionId
            , coMethod    = name
            , coParams    = params
            }
    WS.sendTextData conn . A.encode $ co
  where
    paramsProxy = Proxy :: Proxy a

receiveCommandResponse :: forall ev b. Command b => Handle' ev -> CommandId -> IO (Either Error (CommandResponse b))
receiveCommandResponse handle commandId = do
    bsM <- maybe (fmap Just) timeout (commandTimeout . config $ handle) $ 
        untilJust $ do
            updateCommandBufferM (commandBuffer handle) $ \buffer -> do
                let bsM = Map.lookup commandId buffer
                if isNothing bsM
                    then do threadDelay 1000; pure (buffer, bsM)
                    else pure $ (Map.delete commandId buffer, bsM)

    maybe (pure ()) (logResponse handle (commandName p)) bsM
    pure $ maybe (Left ERRNoResponse) (either (Left . ERRParse) Right . A.eitherDecode) $ bsM
  where
    p = Proxy :: Proxy b

sendReceiveCommandResult' :: forall a b ev. (Show a, ToJSON a, Command b) => Handle' ev -> Maybe SessionId -> String -> Maybe a -> IO b
sendReceiveCommandResult' handle sessionId name params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid sessionId name params
    response <- receiveCommandResponse handle cid
    either throwIO pure $ case response of
        Left err -> Left err
        Right CommandResponse{..} -> either (Left . ERRProtocol) Right crResult
  where
    resultProxy = Proxy :: Proxy b
    
sendReceiveCommand' :: (Show a, ToJSON a) => Handle' ev -> Maybe SessionId -> String -> Maybe a -> IO ()
sendReceiveCommand' handle sessionId name params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid sessionId name params
    response <- receiveCommandResponse handle cid :: IO (Either Error (CommandResponse NoResponse))
    maybe (pure ()) throwIO $ case response of
        Left err                  -> Just err
        Right CommandResponse{..} -> either (Just . ERRProtocol) (const Nothing) crResult


uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl

data ToJSONEx where
    ToJSONEx :: (ToJSON a, Show a) => a -> ToJSONEx
instance ToJSON ToJSONEx where
    toJSON (ToJSONEx v) = toJSON v
instance Show ToJSONEx where
    show (ToJSONEx v) = show v
