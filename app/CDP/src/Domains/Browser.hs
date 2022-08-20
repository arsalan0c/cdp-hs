{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Browser (module Domains.Browser) where
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



import Utils




close :: Session a -> IO (Maybe Error)
close session  = sendReceiveCommand (conn session) ("Browser","close") ([] ++ (catMaybes []))

data GetVersion = GetVersion {
    getVersionProtocolVersion :: String,
    getVersionProduct :: String,
    getVersionRevision :: String,
    getVersionUserAgent :: String,
    getVersionJsVersion :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetVersion where
    parseJSON = A.withObject "GetVersion" $ \v ->
         GetVersion <$> v .:  "protocolVersion"
            <*> v  .:  "product"
            <*> v  .:  "revision"
            <*> v  .:  "userAgent"
            <*> v  .:  "jsVersion"



getVersion :: Session a -> IO (Either Error GetVersion)
getVersion session  = sendReceiveCommandResult (conn session) ("Browser","getVersion") ([] ++ (catMaybes []))


