{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  Tethering :
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






-- | Type of the 'Tethering.accepted' event.
data TetheringAccepted = TetheringAccepted {
  -- | Port number that was successfully bound.
  tetheringAcceptedPort :: Int,
  -- | Connection id to be used.
  tetheringAcceptedConnectionId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON TetheringAccepted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  TetheringAccepted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }





-- | Parameters of the 'tetheringBind' command.
data PTetheringBind = PTetheringBind {
  -- | Port number to bind.
  pTetheringBindPort :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTetheringBind  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PTetheringBind where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the 'Tethering.bind' command.
--   Request browser port binding.
--   Parameters: 'PTetheringBind'
tetheringBind :: Handle ev -> PTetheringBind -> IO ()
tetheringBind handle params = sendReceiveCommand handle "Tethering.bind" (Just params)


-- | Parameters of the 'tetheringUnbind' command.
data PTetheringUnbind = PTetheringUnbind {
  -- | Port number to unbind.
  pTetheringUnbindPort :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTetheringUnbind  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PTetheringUnbind where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'Tethering.unbind' command.
--   Request browser port unbinding.
--   Parameters: 'PTetheringUnbind'
tetheringUnbind :: Handle ev -> PTetheringUnbind -> IO ()
tetheringUnbind handle params = sendReceiveCommand handle "Tethering.unbind" (Just params)



