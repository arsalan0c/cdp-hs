{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

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

type CommandBuffer = Map.Map CommandId (Either ProtocolError A.Value)

data Handle = Handle
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

type ClientApp b  = Handle -> IO b

runClient :: forall b. Config -> ClientApp b -> IO b
runClient config app = do
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
                            dispatchEvent Handle{..} method (imParams im)
                        Just im | Just id' <- imId im ->
                            dispatchCommandResponse Handle {..} id'
                            (imError im) (imResult im)
                            {-
                    if isCommand bs
                        then dispatchCommandResponse commandBuffer bs
                        else dispatchEventResponse MkHandle{..} bs
                        -}

        bracket listen killThread (\listenThread -> app Handle{..})

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

dispatchCommandResponse
    :: Handle -> CommandId -> Maybe ProtocolError -> Maybe A.Value -> IO ()
dispatchCommandResponse h cid mbErr mbVal =
    updateCommandBuffer (commandBuffer h) (Map.insert cid entry)
  where
    entry = case mbErr of
        Just err -> Left err
        Nothing -> case mbVal of
            Just val -> Right val
            Nothing  -> Right A.Null
            
updateCommandBuffer :: MVar CommandBuffer -> (CommandBuffer -> CommandBuffer) -> IO ()
updateCommandBuffer commandBuffer f = ($ pure . f) . modifyMVar_ $ commandBuffer

updateCommandBufferM :: MVar CommandBuffer -> (CommandBuffer -> IO (CommandBuffer, a)) -> IO a
updateCommandBufferM = modifyMVar

dispatchEvent :: Handle -> String -> Maybe A.Value -> IO ()
dispatchEvent handle method mbParams = do
    evs <- readMVar (eventHandlers handle)
    case Map.lookup method evs of
        Nothing -> IO.hPutStrLn IO.stderr $ "No handler for " ++ show method
        Just f -> case mbParams of
            Nothing -> IO.hPutStrLn IO.stderr $ "No params for " ++ show method
            Just p -> do
                IO.hPutStrLn IO.stderr $ "Calling handler for " ++ show method
                f p

updateEventHandlers :: forall ev. Handle -> (Map.Map String (A.Value -> IO ()) -> Map.Map String (A.Value -> IO ())) -> IO ()
updateEventHandlers handle f = ($ pure . f) . modifyMVar_ . eventHandlers $ handle

subscribe :: forall ev a. Event a => Handle -> (a -> IO ()) -> IO ()
subscribe handle h = updateEventHandlers handle $ Map.insert (eventName p) handler
  where
    p = Proxy :: Proxy a

    handler :: A.Value -> IO ()
    handler val = case A.fromJSON val :: A.Result a of
        A.Error   err -> do
            IO.hPutStrLn IO.stderr $ "Error parsing JSON: " ++ err
            IO.hPutStrLn IO.stderr $ "Value: " ++ show val
        A.Success x   -> h x

unsubscribe :: forall ev a. Event a => Handle -> Proxy a -> IO ()
unsubscribe handle proxy = updateEventHandlers handle (Map.delete (eventName proxy))


newtype CommandId = CommandId { unCommandId :: Int }
    deriving (Eq, Ord, Show, FromJSON, ToJSON)

data CommandObj a = CommandObj
    { coId     :: CommandId
    , coMethod :: String
    , coParams :: a
    } deriving Show

instance (ToJSON a) => ToJSON (CommandObj a) where
   toJSON cmd = A.object . concat $
        [ [ "id"     .= coId cmd ]
        , [ "method" .= coMethod cmd ]
        , case toJSON (coParams cmd) of
            A.Null -> []
            params -> [ "params" .= params ]
        ]

randomCommandId :: Handle -> IO CommandId
randomCommandId handle = modifyMVar (randomGen handle) $ \g -> do
    let (id, g2) = uniformR (0 :: Int, 1000 :: Int) g
    pure (g2, CommandId id)

class (ToJSON cmd, FromJSON (CommandResponse cmd)) => Command cmd where
    type CommandResponse cmd :: *
    commandName :: Proxy cmd -> String

data NoResponse = NoResponse

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

sendCommand
    :: forall cmd. Command cmd
    => WS.Connection -> CommandId -> String -> cmd -> IO ()
sendCommand conn id name params = do
    let co = CommandObj id name params
    WS.sendTextData conn . A.encode $ co
  where
    paramsProxy = Proxy :: Proxy cmd

receiveCommandResponse
    :: forall cmd. Command cmd
    => Handle -> Proxy cmd -> CommandId -> IO (Either Error (CommandResponse cmd))
receiveCommandResponse handle proxy id = do
    entry <- maybe (fmap Just) timeout (commandTimeout . config $ handle) $
        untilJust $ do
            updateCommandBufferM (commandBuffer handle) $ \buffer -> do
                let entry = Map.lookup id buffer
                if isNothing entry
                    then do threadDelay 1000; pure (buffer, entry)
                    else pure $ (Map.delete id buffer, entry)

    pure $ case entry of
        Nothing -> Left ERRNoResponse
        Just (Left err) -> Left $ ERRProtocol err
        Just (Right v) -> case A.fromJSON v :: A.Result (CommandResponse cmd) of
            A.Error   err -> Left $ ERRParse err
            A.Success x   -> Right x

sendReceiveCommandResult
    :: forall cmd. Command cmd
    => Handle -> cmd -> IO (CommandResponse cmd)
sendReceiveCommandResult handle params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid (commandName proxy) params
    response <- receiveCommandResponse handle proxy cid
    either throwIO pure response
  where
    proxy = Proxy :: Proxy cmd

sendReceiveCommand
    :: forall cmd. (Command cmd, CommandResponse cmd ~ NoResponse)
    => Handle -> cmd -> IO ()
sendReceiveCommand handle params = do
    cid <- randomCommandId handle
    sendCommand (conn handle) cid (commandName proxy) params
    response <- receiveCommandResponse handle proxy cid :: IO (Either Error NoResponse)
    case response of
        Left err -> throwIO err
        Right NoResponse -> pure ()
  where
    proxy = Proxy :: Proxy cmd

uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl
