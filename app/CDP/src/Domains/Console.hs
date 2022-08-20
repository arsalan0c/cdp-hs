{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Console (module Domains.Console) where
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


import Utils

data MessageAdded = MessageAdded {
    messageAddedMessage :: ConsoleMessage
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  MessageAdded where
    parseJSON = A.withObject "MessageAdded" $ \v ->
         MessageAdded <$> v .:  "message"


instance ToJSON MessageAdded  where
    toJSON v = A.object
        [ "message" .= messageAddedMessage v
        ]



data ConsoleMessage = ConsoleMessage {
    consoleMessageSource :: String,
    consoleMessageLevel :: String,
    consoleMessageText :: String,
    consoleMessageUrl :: Maybe String,
    consoleMessageLine :: Maybe Int,
    consoleMessageColumn :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ConsoleMessage where
    parseJSON = A.withObject "ConsoleMessage" $ \v ->
         ConsoleMessage <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:?  "url"
            <*> v  .:?  "line"
            <*> v  .:?  "column"


instance ToJSON ConsoleMessage  where
    toJSON v = A.object
        [ "source" .= consoleMessageSource v
        , "level" .= consoleMessageLevel v
        , "text" .= consoleMessageText v
        , "url" .= consoleMessageUrl v
        , "line" .= consoleMessageLine v
        , "column" .= consoleMessageColumn v
        ]



clearMessages :: Session a -> IO (Maybe Error)
clearMessages session  = sendReceiveCommand (conn session) ("Console","clearMessages") ([] ++ (catMaybes []))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Console","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Console","enable") ([] ++ (catMaybes []))


