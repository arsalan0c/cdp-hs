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

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime




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

