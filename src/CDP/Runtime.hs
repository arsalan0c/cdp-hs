{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE RankNTypes    #-}

module CDP.Runtime 
    ( module CDP.Runtime
    , module CDP.Internal.Utils
    ) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe
import Data.Foldable (for_)
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
import qualified Data.IORef as IORef

import CDP.Internal.Endpoints
import CDP.Internal.Utils

type ClientApp b  = Handle -> IO b

runClient :: forall b. Config -> ClientApp b -> IO b
runClient config app = do
    randomGen      <- newMVar . mkStdGen $ 42
    subscriptions  <- IORef.newIORef $ Subscriptions Map.empty 0
    commandBuffer  <- IORef.newIORef Map.empty
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
dispatchCommandResponse h cid mbErr mbVal = do
    mbMVar <- IORef.atomicModifyIORef' (commandBuffer h) $ \buffer ->
        case M.lookup cid buffer of
            Nothing -> (buffer, Nothing)
            Just mv -> (Map.delete cid buffer, Just mv)
    case mbMVar of
        Nothing -> pure ()
        Just mv -> putMVar mv $ case mbErr of
            Just err -> Left err
            Nothing -> case mbVal of
                Just val -> Right val
                Nothing  -> Right A.Null

dispatchEvent :: Handle -> String -> Maybe A.Value -> IO ()
dispatchEvent handle method mbParams = do
    byMethod <- subscriptionsHandlers <$> IORef.readIORef (subscriptions handle)
    case Map.lookup method byMethod of
        Nothing -> IO.hPutStrLn IO.stderr $ "No handler for " ++ show method
        Just byId -> case mbParams of
            Nothing -> IO.hPutStrLn IO.stderr $ "No params for " ++ show method
            Just params -> do
                IO.hPutStrLn IO.stderr $ "Calling handler for " ++ show method
                for_ byId ($ params)

data Subscription = Subscription
    { subscriptionEventName :: String
    , subscriptionId        :: Int
    }

-- | Subscribes to an event
subscribe :: forall a. Event a => Handle -> (a -> IO ()) -> IO Subscription
subscribe handle f = do
    id' <- IORef.atomicModifyIORef' (subscriptions handle) $ \s ->
        let id' = subscriptionsNextId s in
        ( s { subscriptionsNextId   = id' + 1
            , subscriptionsHandlers = Map.insertWith
                Map.union
                ename
                (Map.singleton id' handler)
                (subscriptionsHandlers s)
            }
        , id'
        )

    pure $ Subscription ename id'
  where
    ename = eventName (Proxy :: Proxy a)

    handler :: A.Value -> IO ()
    handler val = case A.fromJSON val :: A.Result a of
        A.Error   err -> do
            IO.hPutStrLn IO.stderr $ "Error parsing JSON: " ++ err
            IO.hPutStrLn IO.stderr $ "Value: " ++ show val
        A.Success x   -> f x

-- | Unsubscribes to an event
unsubscribe :: Handle -> Subscription -> IO ()
unsubscribe handle (Subscription ename id') =
    IORef.atomicModifyIORef' (subscriptions handle) $ \s ->
        ( s { subscriptionsHandlers =
                Map.adjust (Map.delete id') ename (subscriptionsHandlers s)
            }
        , ()
        )

data Promise a where
    Promise :: MVar tmp -> (tmp -> Either Error a) -> Promise a

-- | Resolves a promise to its value 
readPromise :: Promise a -> IO a
readPromise (Promise mv f) = do
    x <- readMVar mv
    either throwIO pure $ f x

randomCommandId :: Handle -> IO CommandId
randomCommandId handle = modifyMVar (randomGen handle) $ \g -> do
    let (id, g2) = uniformR (0 :: Int, 1000 :: Int) g
    pure (g2, CommandId id)

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

-- | Sends a command to the browser and waits until a response is received,
--   for timeout duration configured
sendCommandWait
    :: Command cmd
    => Handle -> cmd -> IO (CommandResponse cmd)
sendCommandWait handle params = do
    promise <- sendCommand handle params
    let r = readPromise promise
    mbRes <- maybe (fmap Just r) (flip timeout r) (commandTimeout . config $ handle)
    maybe (throwIO ERRNoResponse) pure mbRes
  where
    proxy = Proxy :: Proxy cmd

-- | Sends a command to the browser
sendCommand
    :: forall cmd. Command cmd
    => Handle -> cmd -> IO (Promise (CommandResponse cmd))
sendCommand handle params = do
    id <- randomCommandId handle
    let co = CommandObj id (commandName proxy) params
    mv <- newEmptyMVar
    IORef.atomicModifyIORef' (commandBuffer handle) $ \buffer ->
        (M.insert id mv buffer, ())
    WS.sendTextData (conn handle) . A.encode $ co
    pure $ Promise mv $ \entry -> case entry of
        Left err -> Left $ ERRProtocol err
        Right v  -> case (fromJSON (Proxy :: Proxy cmd) v) of
            A.Error   err -> Left $ ERRParse err
            A.Success x   -> Right x
  where
    proxy = Proxy :: Proxy cmd

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

data SomeCommand where
    SomeCommand :: Command cmd => cmd -> SomeCommand

fromSomeCommand :: (forall cmd. Command cmd => cmd -> r) -> SomeCommand -> r
fromSomeCommand f (SomeCommand c) = f c