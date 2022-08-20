{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.IO (module Domains.IO) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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

import qualified Domains.Runtime as Runtime
import Prelude


import Utils


type StreamHandle = String

close :: Session a -> StreamHandle -> IO (Maybe Error)
close session closeHandle = sendReceiveCommand (conn session) ("IO","close") ([("handle", ToJSONEx closeHandle)] ++ (catMaybes []))

data Reads = Reads {
    readData :: String,
    readEof :: Bool,
    readBase64Encoded :: Maybe Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Reads where
    parseJSON = A.withObject "Read" $ \v ->
         Reads <$> v .:  "data"
            <*> v  .:  "eof"
            <*> v  .:?  "base64Encoded"



read :: Session a -> StreamHandle -> Maybe Int -> Maybe Int -> IO (Either Error Reads)
read session readHandle readOffset readSize = sendReceiveCommandResult (conn session) ("IO","read") ([("handle", ToJSONEx readHandle)] ++ (catMaybes [fmap (("offset",) . ToJSONEx) readOffset, fmap (("size",) . ToJSONEx) readSize]))

data ResolveBlob = ResolveBlob {
    resolveBlobUuid :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ResolveBlob where
    parseJSON = A.withObject "ResolveBlob" $ \v ->
         ResolveBlob <$> v .:  "uuid"



resolveBlob :: Session a -> Runtime.RemoteObjectId -> IO (Either Error ResolveBlob)
resolveBlob session resolveBlobObjectId = sendReceiveCommandResult (conn session) ("IO","resolveBlob") ([("objectId", ToJSONEx resolveBlobObjectId)] ++ (catMaybes []))


