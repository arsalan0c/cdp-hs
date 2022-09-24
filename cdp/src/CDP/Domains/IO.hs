{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.IO (module CDP.Domains.IO) where

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
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Runtime
import CDP.Handle

import CDP.Domains.Runtime as Runtime


type IoStreamHandle = String





data PIoClose = PIoClose {
   pIoCloseHandle :: IoStreamHandle
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoClose  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  PIoClose where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }


ioClose :: Handle ev -> PIoClose -> IO (Maybe Error)
ioClose handle params = sendReceiveCommand handle "IO.close" (Just params)



data PIoRead = PIoRead {
   pIoReadHandle :: IoStreamHandle,
   pIoReadOffset :: Maybe Int,
   pIoReadSize :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoRead  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  PIoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }


ioRead :: Handle ev -> PIoRead -> IO (Either Error IoRead)
ioRead handle params = sendReceiveCommandResult handle "IO.read" (Just params)

data IoRead = IoRead {
   ioReadBase64Encoded :: Maybe Bool,
   ioReadData :: String,
   ioReadEof :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 6 }

instance Command IoRead where
   commandName _ = "IO.read"




data PIoResolveBlob = PIoResolveBlob {
   pIoResolveBlobObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoResolveBlob  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PIoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


ioResolveBlob :: Handle ev -> PIoResolveBlob -> IO (Either Error IoResolveBlob)
ioResolveBlob handle params = sendReceiveCommandResult handle "IO.resolveBlob" (Just params)

data IoResolveBlob = IoResolveBlob {
   ioResolveBlobUuid :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance Command IoResolveBlob where
   commandName _ = "IO.resolveBlob"




