{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  IO :
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


-- | This is either obtained from another method or specified as `blob:&lt;uuid&gt;` where
-- `&lt;uuid&gt` is an UUID of a Blob.
type IoStreamHandle = String





-- | Parameters of the 'ioClose' command.
data PIoClose = PIoClose {
   pIoCloseHandle :: PIoCloseHandle -- ^ Handle of the stream to close.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoClose  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  PIoClose where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }


-- | Function for the command 'IO.close'.
-- Close the stream, discard any temporary backing storage.
-- Parameters: 'PIoClose'
ioClose :: Handle ev -> PIoClose -> IO (Maybe Error)
ioClose handle params = sendReceiveCommand handle "IO.close" (Just params)


-- | Parameters of the 'ioRead' command.
data PIoRead = PIoRead {
   pIoReadHandle :: PIoReadHandle, -- ^ Handle of the stream to read.
   pIoReadOffset :: PIoReadOffset, -- ^ Seek to the specified offset before reading (if not specificed, proceed with offset
following the last read). Some types of streams may only support sequential reads.
   pIoReadSize :: PIoReadSize -- ^ Maximum number of bytes to read (left upon the agent discretion if not specified).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoRead  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  PIoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }


-- | Function for the command 'IO.read'.
-- Read a chunk of the stream
-- Parameters: 'PIoRead'
-- Returns: 'IoRead'
ioRead :: Handle ev -> PIoRead -> IO (Either Error IoRead)
ioRead handle params = sendReceiveCommandResult handle "IO.read" (Just params)

-- | Return type of the 'ioRead' command.
data IoRead = IoRead {
   ioReadBase64Encoded :: Maybe Bool, -- ^ Set if the data is base64-encoded
   ioReadData :: String, -- ^ Data that were read.
   ioReadEof :: Bool -- ^ Set if the end-of-file condition occurred while reading.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoRead where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 6 }

instance Command IoRead where
   commandName _ = "IO.read"



-- | Parameters of the 'ioResolveBlob' command.
data PIoResolveBlob = PIoResolveBlob {
   pIoResolveBlobObjectId :: PIoResolveBlobObjectId -- ^ Object id of a Blob object wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIoResolveBlob  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PIoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the command 'IO.resolveBlob'.
-- Return UUID of Blob object specified by a remote object id.
-- Parameters: 'PIoResolveBlob'
-- Returns: 'IoResolveBlob'
ioResolveBlob :: Handle ev -> PIoResolveBlob -> IO (Either Error IoResolveBlob)
ioResolveBlob handle params = sendReceiveCommandResult handle "IO.resolveBlob" (Just params)

-- | Return type of the 'ioResolveBlob' command.
data IoResolveBlob = IoResolveBlob {
   ioResolveBlobUuid :: String -- ^ UUID of the specified Blob.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IoResolveBlob where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }

instance Command IoResolveBlob where
   commandName _ = "IO.resolveBlob"




