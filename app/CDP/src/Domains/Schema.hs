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

module Domains.Schema (module Domains.Schema) where

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



data SchemaEvent = 
    deriving (Eq, Show, Read)



subscribe :: forall a. FromEvent SchemaEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy SchemaEvent
    pa       = Proxy :: Proxy a


data SchemaDomain = SchemaDomain {
    schemaDomainName :: String,
    schemaDomainVersion :: String
} deriving (Eq, Show, Read)
instance FromJSON  SchemaDomain where
    parseJSON = A.withObject "SchemaDomain" $ \v ->
         SchemaDomain <$> v .:  "name"
            <*> v  .:  "version"


instance ToJSON SchemaDomain  where
    toJSON v = A.object
        [ "name" .= schemaDomainName v
        , "version" .= schemaDomainVersion v
        ]



data SchemaGetDomains = SchemaGetDomains {
    schemaGetDomainsDomains :: [SchemaDomain]
} deriving (Eq, Show, Read)
instance FromJSON  SchemaGetDomains where
    parseJSON = A.withObject "SchemaGetDomains" $ \v ->
         SchemaGetDomains <$> v .:  "domains"



instance Command  SchemaGetDomains where
    commandName _ = "Schema.getDomains"


schemaGetDomains :: Session -> IO (Either Error SchemaGetDomains)
schemaGetDomains session = sendReceiveCommandResult session "Schema.getDomains" (Nothing :: Maybe ())

