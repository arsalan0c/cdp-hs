{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DeviceOrientation

-}


module CDP.Domains.DeviceOrientation (module CDP.Domains.DeviceOrientation) where

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




-- | Clears the overridden Device Orientation.

-- | Parameters of the 'DeviceOrientation.clearDeviceOrientationOverride' command.
data PDeviceOrientationClearDeviceOrientationOverride = PDeviceOrientationClearDeviceOrientationOverride
  deriving (Eq, Show)
pDeviceOrientationClearDeviceOrientationOverride
  :: PDeviceOrientationClearDeviceOrientationOverride
pDeviceOrientationClearDeviceOrientationOverride
  = PDeviceOrientationClearDeviceOrientationOverride
instance ToJSON PDeviceOrientationClearDeviceOrientationOverride where
  toJSON _ = A.Null
instance Command PDeviceOrientationClearDeviceOrientationOverride where
  type CommandResponse PDeviceOrientationClearDeviceOrientationOverride = ()
  commandName _ = "DeviceOrientation.clearDeviceOrientationOverride"
  fromJSON = const . A.Success . const ()

-- | Overrides the Device Orientation.

-- | Parameters of the 'DeviceOrientation.setDeviceOrientationOverride' command.
data PDeviceOrientationSetDeviceOrientationOverride = PDeviceOrientationSetDeviceOrientationOverride
  {
    -- | Mock alpha
    pDeviceOrientationSetDeviceOrientationOverrideAlpha :: Double,
    -- | Mock beta
    pDeviceOrientationSetDeviceOrientationOverrideBeta :: Double,
    -- | Mock gamma
    pDeviceOrientationSetDeviceOrientationOverrideGamma :: Double
  }
  deriving (Eq, Show)
pDeviceOrientationSetDeviceOrientationOverride
  {-
  -- | Mock alpha
  -}
  :: Double
  {-
  -- | Mock beta
  -}
  -> Double
  {-
  -- | Mock gamma
  -}
  -> Double
  -> PDeviceOrientationSetDeviceOrientationOverride
pDeviceOrientationSetDeviceOrientationOverride
  arg_pDeviceOrientationSetDeviceOrientationOverrideAlpha
  arg_pDeviceOrientationSetDeviceOrientationOverrideBeta
  arg_pDeviceOrientationSetDeviceOrientationOverrideGamma
  = PDeviceOrientationSetDeviceOrientationOverride
    arg_pDeviceOrientationSetDeviceOrientationOverrideAlpha
    arg_pDeviceOrientationSetDeviceOrientationOverrideBeta
    arg_pDeviceOrientationSetDeviceOrientationOverrideGamma
instance ToJSON PDeviceOrientationSetDeviceOrientationOverride where
  toJSON p = A.object $ catMaybes [
    ("alpha" A..=) <$> Just (pDeviceOrientationSetDeviceOrientationOverrideAlpha p),
    ("beta" A..=) <$> Just (pDeviceOrientationSetDeviceOrientationOverrideBeta p),
    ("gamma" A..=) <$> Just (pDeviceOrientationSetDeviceOrientationOverrideGamma p)
    ]
instance Command PDeviceOrientationSetDeviceOrientationOverride where
  type CommandResponse PDeviceOrientationSetDeviceOrientationOverride = ()
  commandName _ = "DeviceOrientation.setDeviceOrientationOverride"
  fromJSON = const . A.Success . const ()

