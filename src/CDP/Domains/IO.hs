{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= IO

Input/Output operations for streams produced by DevTools.
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.Runtime as Runtime


-- | Type 'IO.StreamHandle'.
--   This is either obtained from another method or specified as `blob:&lt;uuid&gt;` where
--   `&lt;uuid&gt` is an UUID of a Blob.
type IOStreamHandle = T.Text

-- | Close the stream, discard any temporary backing storage.

-- | Parameters of the 'IO.close' command.
data PIOClose = PIOClose
  {
    -- | Handle of the stream to close.
    pIOCloseHandle :: IOStreamHandle
  }
  deriving (Eq, Show)
pIOClose
  {-
  -- | Handle of the stream to close.
  -}
  :: IOStreamHandle
  -> PIOClose
pIOClose
  arg_pIOCloseHandle
  = PIOClose
    arg_pIOCloseHandle
instance ToJSON PIOClose where
  toJSON p = A.object $ catMaybes [
    ("handle" A..=) <$> Just (pIOCloseHandle p)
    ]
instance Command PIOClose where
  type CommandResponse PIOClose = ()
  commandName _ = "IO.close"
  fromJSON = const . A.Success . const ()

-- | Read a chunk of the stream

-- | Parameters of the 'IO.read' command.
data PIORead = PIORead
  {
    -- | Handle of the stream to read.
    pIOReadHandle :: IOStreamHandle,
    -- | Seek to the specified offset before reading (if not specificed, proceed with offset
    --   following the last read). Some types of streams may only support sequential reads.
    pIOReadOffset :: Maybe Int,
    -- | Maximum number of bytes to read (left upon the agent discretion if not specified).
    pIOReadSize :: Maybe Int
  }
  deriving (Eq, Show)
pIORead
  {-
  -- | Handle of the stream to read.
  -}
  :: IOStreamHandle
  -> PIORead
pIORead
  arg_pIOReadHandle
  = PIORead
    arg_pIOReadHandle
    Nothing
    Nothing
instance ToJSON PIORead where
  toJSON p = A.object $ catMaybes [
    ("handle" A..=) <$> Just (pIOReadHandle p),
    ("offset" A..=) <$> (pIOReadOffset p),
    ("size" A..=) <$> (pIOReadSize p)
    ]
data IORead = IORead
  {
    -- | Set if the data is base64-encoded
    iOReadBase64Encoded :: Maybe Bool,
    -- | Data that were read.
    iOReadData :: T.Text,
    -- | Set if the end-of-file condition occurred while reading.
    iOReadEof :: Bool
  }
  deriving (Eq, Show)
instance FromJSON IORead where
  parseJSON = A.withObject "IORead" $ \o -> IORead
    <$> o A..:? "base64Encoded"
    <*> o A..: "data"
    <*> o A..: "eof"
instance Command PIORead where
  type CommandResponse PIORead = IORead
  commandName _ = "IO.read"

-- | Return UUID of Blob object specified by a remote object id.

-- | Parameters of the 'IO.resolveBlob' command.
data PIOResolveBlob = PIOResolveBlob
  {
    -- | Object id of a Blob object wrapper.
    pIOResolveBlobObjectId :: Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pIOResolveBlob
  {-
  -- | Object id of a Blob object wrapper.
  -}
  :: Runtime.RuntimeRemoteObjectId
  -> PIOResolveBlob
pIOResolveBlob
  arg_pIOResolveBlobObjectId
  = PIOResolveBlob
    arg_pIOResolveBlobObjectId
instance ToJSON PIOResolveBlob where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pIOResolveBlobObjectId p)
    ]
data IOResolveBlob = IOResolveBlob
  {
    -- | UUID of the specified Blob.
    iOResolveBlobUuid :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON IOResolveBlob where
  parseJSON = A.withObject "IOResolveBlob" $ \o -> IOResolveBlob
    <$> o A..: "uuid"
instance Command PIOResolveBlob where
  type CommandResponse PIOResolveBlob = IOResolveBlob
  commandName _ = "IO.resolveBlob"

