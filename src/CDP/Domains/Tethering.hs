{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Tethering

The Tethering domain defines methods and events for browser port binding.
-}


module CDP.Domains.Tethering (module CDP.Domains.Tethering) where

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




-- | Type of the 'Tethering.accepted' event.
data TetheringAccepted = TetheringAccepted
  {
    -- | Port number that was successfully bound.
    tetheringAcceptedPort :: Int,
    -- | Connection id to be used.
    tetheringAcceptedConnectionId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON TetheringAccepted where
  parseJSON = A.withObject "TetheringAccepted" $ \o -> TetheringAccepted
    <$> o A..: "port"
    <*> o A..: "connectionId"
instance Event TetheringAccepted where
  eventName _ = "Tethering.accepted"

-- | Request browser port binding.

-- | Parameters of the 'Tethering.bind' command.
data PTetheringBind = PTetheringBind
  {
    -- | Port number to bind.
    pTetheringBindPort :: Int
  }
  deriving (Eq, Show)
pTetheringBind
  {-
  -- | Port number to bind.
  -}
  :: Int
  -> PTetheringBind
pTetheringBind
  arg_pTetheringBindPort
  = PTetheringBind
    arg_pTetheringBindPort
instance ToJSON PTetheringBind where
  toJSON p = A.object $ catMaybes [
    ("port" A..=) <$> Just (pTetheringBindPort p)
    ]
instance Command PTetheringBind where
  type CommandResponse PTetheringBind = ()
  commandName _ = "Tethering.bind"
  fromJSON = const . A.Success . const ()

-- | Request browser port unbinding.

-- | Parameters of the 'Tethering.unbind' command.
data PTetheringUnbind = PTetheringUnbind
  {
    -- | Port number to unbind.
    pTetheringUnbindPort :: Int
  }
  deriving (Eq, Show)
pTetheringUnbind
  {-
  -- | Port number to unbind.
  -}
  :: Int
  -> PTetheringUnbind
pTetheringUnbind
  arg_pTetheringUnbindPort
  = PTetheringUnbind
    arg_pTetheringUnbindPort
instance ToJSON PTetheringUnbind where
  toJSON p = A.object $ catMaybes [
    ("port" A..=) <$> Just (pTetheringUnbindPort p)
    ]
instance Command PTetheringUnbind where
  type CommandResponse PTetheringUnbind = ()
  commandName _ = "Tethering.unbind"
  fromJSON = const . A.Success . const ()

