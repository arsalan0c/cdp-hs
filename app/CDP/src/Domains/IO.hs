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

module Domains.IO (module Domains.IO) where

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

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema




type IOStreamHandle = String



data PIOClose = PIOClose {
    pioCloseHandle :: IOStreamHandle
} deriving (Eq, Show, Read)
instance FromJSON  PIOClose where
    parseJSON = A.withObject "PIOClose" $ \v ->
         PIOClose <$> v .:  "handle"


instance ToJSON PIOClose  where
    toJSON v = A.object
        [ "handle" .= pioCloseHandle v
        ]


iOClose :: Session -> PIOClose -> IO (Maybe Error)
iOClose session params = sendReceiveCommand session "IO.close" (Just params)

data IORead = IORead {
    iOReadData :: String,
    iOReadEof :: Bool,
    iOReadBase64Encoded :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  IORead where
    parseJSON = A.withObject "IORead" $ \v ->
         IORead <$> v .:  "data"
            <*> v  .:  "eof"
            <*> v  .:?  "base64Encoded"



instance Command  IORead where
    commandName _ = "IO.read"

data PIORead = PIORead {
    pioReadHandle :: IOStreamHandle,
    pioReadOffset :: Maybe Int,
    pioReadSize :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PIORead where
    parseJSON = A.withObject "PIORead" $ \v ->
         PIORead <$> v .:  "handle"
            <*> v  .:?  "offset"
            <*> v  .:?  "size"


instance ToJSON PIORead  where
    toJSON v = A.object
        [ "handle" .= pioReadHandle v
        , "offset" .= pioReadOffset v
        , "size" .= pioReadSize v
        ]


iORead :: Session -> PIORead -> IO (Either Error IORead)
iORead session params = sendReceiveCommandResult session "IO.read" (Just params)

data IOResolveBlob = IOResolveBlob {
    iOResolveBlobUuid :: String
} deriving (Eq, Show, Read)
instance FromJSON  IOResolveBlob where
    parseJSON = A.withObject "IOResolveBlob" $ \v ->
         IOResolveBlob <$> v .:  "uuid"



instance Command  IOResolveBlob where
    commandName _ = "IO.resolveBlob"

data PIOResolveBlob = PIOResolveBlob {
    pioResolveBlobObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PIOResolveBlob where
    parseJSON = A.withObject "PIOResolveBlob" $ \v ->
         PIOResolveBlob <$> v .:  "objectId"


instance ToJSON PIOResolveBlob  where
    toJSON v = A.object
        [ "objectId" .= pioResolveBlobObjectId v
        ]


iOResolveBlob :: Session -> PIOResolveBlob -> IO (Either Error IOResolveBlob)
iOResolveBlob session params = sendReceiveCommandResult session "IO.resolveBlob" (Just params)

