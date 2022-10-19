{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Inspector

-}


module CDP.Domains.Inspector (module CDP.Domains.Inspector) where

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




-- | Type of the 'Inspector.detached' event.
data InspectorDetached = InspectorDetached
  {
    -- | The reason why connection has been terminated.
    inspectorDetachedReason :: String
  }
  deriving (Eq, Show)
instance FromJSON InspectorDetached where
  parseJSON = A.withObject "InspectorDetached" $ \o -> InspectorDetached
    <$> o A..: "reason"
instance Event InspectorDetached where
  eventName _ = "Inspector.detached"

-- | Type of the 'Inspector.targetCrashed' event.
data InspectorTargetCrashed = InspectorTargetCrashed
  deriving (Eq, Show, Read)
instance FromJSON InspectorTargetCrashed where
  parseJSON _ = pure InspectorTargetCrashed
instance Event InspectorTargetCrashed where
  eventName _ = "Inspector.targetCrashed"

-- | Type of the 'Inspector.targetReloadedAfterCrash' event.
data InspectorTargetReloadedAfterCrash = InspectorTargetReloadedAfterCrash
  deriving (Eq, Show, Read)
instance FromJSON InspectorTargetReloadedAfterCrash where
  parseJSON _ = pure InspectorTargetReloadedAfterCrash
instance Event InspectorTargetReloadedAfterCrash where
  eventName _ = "Inspector.targetReloadedAfterCrash"

-- | Disables inspector domain notifications.

-- | Parameters of the 'Inspector.disable' command.
data PInspectorDisable = PInspectorDisable
  deriving (Eq, Show)
pInspectorDisable
  :: PInspectorDisable
pInspectorDisable
  = PInspectorDisable
instance ToJSON PInspectorDisable where
  toJSON _ = A.Null
instance Command PInspectorDisable where
  type CommandResponse PInspectorDisable = ()
  commandName _ = "Inspector.disable"
  fromJSON = const . A.Success . const ()

-- | Enables inspector domain notifications.

-- | Parameters of the 'Inspector.enable' command.
data PInspectorEnable = PInspectorEnable
  deriving (Eq, Show)
pInspectorEnable
  :: PInspectorEnable
pInspectorEnable
  = PInspectorEnable
instance ToJSON PInspectorEnable where
  toJSON _ = A.Null
instance Command PInspectorEnable where
  type CommandResponse PInspectorEnable = ()
  commandName _ = "Inspector.enable"
  fromJSON = const . A.Success . const ()

