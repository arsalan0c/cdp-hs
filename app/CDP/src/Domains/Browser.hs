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

module Domains.Browser (module Domains.Browser) where

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



data BrowserEvent = 
    deriving (Eq, Show, Read)



subscribe :: forall a. FromEvent BrowserEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy BrowserEvent
    pa       = Proxy :: Proxy a







browserClose :: Session -> IO (Maybe Error)
browserClose session = sendReceiveCommand session "Browser.close" (Nothing :: Maybe ())

data BrowserGetVersion = BrowserGetVersion {
    browserGetVersionProtocolVersion :: String,
    browserGetVersionProduct :: String,
    browserGetVersionRevision :: String,
    browserGetVersionUserAgent :: String,
    browserGetVersionJsVersion :: String
} deriving (Eq, Show, Read)
instance FromJSON  BrowserGetVersion where
    parseJSON = A.withObject "BrowserGetVersion" $ \v ->
         BrowserGetVersion <$> v .:  "protocolVersion"
            <*> v  .:  "product"
            <*> v  .:  "revision"
            <*> v  .:  "userAgent"
            <*> v  .:  "jsVersion"



instance Command  BrowserGetVersion where
    commandName _ = "Browser.getVersion"


browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session = sendReceiveCommandResult session "Browser.getVersion" (Nothing :: Maybe ())

