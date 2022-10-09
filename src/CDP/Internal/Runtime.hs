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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import Control.Applicative
import Data.Default
import Control.Exception
import System.Timeout
import Data.Char
import qualified System.IO as IO

import CDP.Internal.Endpoints

class FromJSON a => Event a where
    eventName :: Proxy a -> String

type CommandBuffer = Map.Map CommandId BS.ByteString
data Handle' ev = MkHandle
    { config            :: Config
    , randomGen        :: MVar StdGen
    , eventHandlers    :: MVar (Map.Map String (A.Value -> IO ()))
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

type ClientApp' ev b  = Handle' ev -> IO b
runClient' :: forall ev b. Config -> ClientApp' ev b -> IO b
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
                    case A.decode bs of
                        Nothing -> IO.hPutStrLn IO.stderr $
                            "Could not parse message: " ++ show bs
                        Just im | Just method <- imMethod im -> do
                            IO.hPutStrLn IO.stderr $ "dispatching with method " ++ show method
                            dispatchEvent MkHandle{..} method (imParams im)
                        _ -> dispatchCommandResponse commandBuffer bs
                            {-
                    if isCommand bs
                        then dispatchCommandResponse commandBuffer bs
                        else dispatchEventResponse MkHandle{..} bs
                        -}
        
        bracket listen killThread (\listenThread -> app MkHandle{..})

-- | A message from the browser.  We don't know yet if this is a command
-- response or an event
data IncomingMessage = IncomingMessage
    { imMethod :: Maybe String
    , imParams :: Maybe A.Value
    , imId     :: Maybe CommandId
    , imError  :: Maybe ProtocolError
    , imResult :: Maybe A.Value
    }

instance FromJSON IncomingMessage where
    parseJSON = A.withObject "IncomingMessage" $ \obj -> IncomingMessage
        <$> obj A..:? "method"
        <*> obj A..:? "params"
        <*> obj A..:? "id"
        <*> obj A..:? "error"
        <*> obj A..:? "result"
        
dispatchCommandResponse :: MVar CommandBuffer -> BS.ByteString -> IO ()
dispatchCommandResponse commandBuffer bs = do
    case A.decode bs of
        Nothing  -> IO.hPutStrLn IO.stderr $ "not a command: " ++ show bs
        Just cid@(CommandId id)  -> do
            updateCommandBuffer commandBuffer (Map.insert cid bs)
            
updateCommandBuffer :: MVar CommandBuffer -> (CommandBuffer -> CommandBuffer) -> IO ()
updateCommandBuffer commandBuffer f = ($ pure . f) . modifyMVar_ $ commandBuffer

updateCommandBufferM :: MVar CommandBuffer -> (CommandBuffer -> IO (CommandBuffer, a)) -> IO a
updateCommandBufferM = modifyMVar

dispatchEvent :: Handle' ev -> String -> Maybe A.Value -> IO ()
dispatchEvent handle method mbParams = do
    evs <- readMVar (eventHandlers handle)
    case Map.lookup method evs of
        Nothing -> IO.hPutStrLn IO.stderr $ "No handler for " ++ show method
        Just f -> case mbParams of
            Nothing -> IO.hPutStrLn IO.stderr $ "No params for " ++ show method
            Just p -> do
                IO.hPutStrLn IO.stderr $ "Calling handler for " ++ show method
                f p

updateEventHandlers :: forall ev. Handle' ev -> (Map.Map String (A.Value -> IO ()) -> Map.Map String (A.Value -> IO ())) -> IO ()
updateEventHandlers handle f = ($ pure . f) . modifyMVar_ . eventHandlers $ handle

subscribe' :: forall ev a. Event a => Handle' ev -> (a -> IO ()) -> IO ()
subscribe' handle h = updateEventHandlers handle $ Map.insert (eventName p) handler
  where
    p = Proxy :: Proxy a

    handler :: A.Value -> IO ()
    handler val = case A.fromJSON val :: A.Result a of
        A.Error   err -> do
            IO.hPutStrLn IO.stderr $ "Error parsing JSON: " ++ err
            IO.hPutStrLn IO.stderr $ "Value: " ++ show val
        A.Success x   -> h x

unsubscribe' :: forall ev a. Event a => Handle' ev -> Proxy a -> IO ()
unsubscribe' handle proxy = updateEventHandlers handle (Map.delete (eventName proxy))


newtype CommandId = CommandId { unCommandId :: Int }
    deriving (Eq, Ord, Show, FromJSON, ToJSON)

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

randomCommandId :: Handle' ev -> IO CommandId
randomCommandId handle = modifyMVar (randomGen handle) $ \g -> do
    let (id, g2) = uniformR (0 :: Int, 1000 :: Int) g
    pure (g2, CommandId id)

class (FromJSON b) => Command b where
    commandName :: Proxy b -> String

data CommandResponse b = CommandResponse 
    { crId      :: CommandId
    , crResult  :: Either ProtocolError b
    }
    deriving (Eq, Show)

instance (Command b) => FromJSON (CommandResponse b) where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId     <- CommandId <$> obj .: "id"
        crResult <- (Left <$> (obj .: "error")) <|> (Right <$> (obj .: "result"))
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
    show (PEParse msg)            = msg 
    show (PEInvalidRequest msg)   = msg
    show (PEMethodNotFound msg)   = msg
    show (PEInvalidParams msg)    = msg
    show (PEInternalError msg)    = msg
    show (PEServerError msg)      = msg
    show (PEOther msg)            = msg

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

sendCommand :: forall a. (Show a, ToJSON a) => WS.Connection -> CommandId -> String -> Maybe a -> IO ()
sendCommand conn id name params = do
    let co = CommandObj id name params
    WS.sendTextData conn . A.encode $ co
  where
    paramsProxy = Proxy :: Proxy a

receiveCommandResponse :: forall ev b. Command b => Handle' ev -> CommandId -> IO (Either Error (CommandResponse b))
receiveCommandResponse handle id = do
    bsM <- maybe (fmap Just) timeout (commandTimeout . config $ handle) $ 
        untilJust $ do
            updateCommandBufferM (commandBuffer handle) $ \buffer -> do
                let bsM = Map.lookup id buffer
                if isNothing bsM
                    then do threadDelay 1000; pure (buffer, bsM)
                    else pure $ (Map.delete id buffer, bsM)

    pure $ maybe (Left ERRNoResponse) (either (Left . ERRParse) Right . A.eitherDecode) $ bsM
  where
    p = Proxy :: Proxy b

sendReceiveCommandResult' :: forall a b ev. (Show a, ToJSON a, Command b) => Handle' ev -> String -> Maybe a -> IO b
sendReceiveCommandResult' handle name params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid name params
    response <- receiveCommandResponse handle cid
    either throwIO pure $ case response of
        Left err -> Left err
        Right CommandResponse{..} -> either (Left . ERRProtocol) Right crResult
  where
    resultProxy = Proxy :: Proxy b
    
sendReceiveCommand' :: (Show a, ToJSON a) => Handle' ev -> String -> Maybe a -> IO ()
sendReceiveCommand' handle name params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid name params
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
