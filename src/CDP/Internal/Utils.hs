{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module CDP.Internal.Utils where

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

newtype CommandId = CommandId { unCommandId :: Int }
    deriving (Eq, Ord, Show, FromJSON, ToJSON)

type CommandResponseBuffer =
    Map.Map CommandId (MVar (Either ProtocolError A.Value))

data Subscriptions = Subscriptions
    { subscriptionsHandlers :: Map.Map String (Map.Map Int (A.Value -> IO ()))
    , subscriptionsNextId   :: Int
    }

data Handle = Handle
    { config            :: Config
    , randomGen        :: MVar StdGen
    , subscriptions    :: IORef.IORef Subscriptions
    , commandBuffer    :: IORef.IORef CommandResponseBuffer
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

class FromJSON a => Event a where
    eventName :: Proxy a -> String

class (ToJSON cmd, FromJSON (CommandResponse cmd)) => Command cmd where
    type CommandResponse cmd :: *
    commandName :: Proxy cmd -> String
    fromJSON :: Proxy cmd -> A.Value -> A.Result (CommandResponse cmd)
    fromJSON = const A.fromJSON

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

uncapitalizeFirst :: String -> String
uncapitalizeFirst []     = []
uncapitalizeFirst (x:xs) = toLower x : xs