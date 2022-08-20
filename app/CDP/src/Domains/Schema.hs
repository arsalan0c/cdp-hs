{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Schema (module Domains.Schema) where
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


data Domain = Domain {
    domainName :: String,
    domainVersion :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Domain where
    parseJSON = A.withObject "Domain" $ \v ->
         Domain <$> v .:  "name"
            <*> v  .:  "version"


instance ToJSON Domain  where
    toJSON v = A.object
        [ "name" .= domainName v
        , "version" .= domainVersion v
        ]


data GetDomains = GetDomains {
    getDomainsDomains :: [Domain]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetDomains where
    parseJSON = A.withObject "GetDomains" $ \v ->
         GetDomains <$> v .:  "domains"



getDomains :: Session a -> IO (Either Error GetDomains)
getDomains session  = sendReceiveCommandResult (conn session) ("Schema","getDomains") ([] ++ (catMaybes []))


