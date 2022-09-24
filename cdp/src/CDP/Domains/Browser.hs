{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Browser (module CDP.Domains.Browser) where

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







browserClose :: Handle ev -> IO (Maybe Error)
browserClose handle = sendReceiveCommand handle "Browser.close" (Nothing :: Maybe ())


browserGetVersion :: Handle ev -> IO (Either Error BrowserGetVersion)
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())

data BrowserGetVersion = BrowserGetVersion {
   browserGetVersionProtocolVersion :: String,
   browserGetVersionProduct :: String,
   browserGetVersionRevision :: String,
   browserGetVersionUserAgent :: String,
   browserGetVersionJsVersion :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command BrowserGetVersion where
   commandName _ = "Browser.getVersion"




