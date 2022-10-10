{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  DeviceOrientation 
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

import CDP.Internal.Runtime








-- | Parameters of the 'deviceOrientationClearDeviceOrientationOverride' command.
data PDeviceOrientationClearDeviceOrientationOverride = PDeviceOrientationClearDeviceOrientationOverride
instance ToJSON PDeviceOrientationClearDeviceOrientationOverride where toJSON _ = A.Null

-- | Function for the 'DeviceOrientation.clearDeviceOrientationOverride' command.
--   Clears the overridden Device Orientation.
deviceOrientationClearDeviceOrientationOverride :: Handle -> IO ()
deviceOrientationClearDeviceOrientationOverride handle = sendReceiveCommand handle PDeviceOrientationClearDeviceOrientationOverride

instance Command PDeviceOrientationClearDeviceOrientationOverride where
    type CommandResponse PDeviceOrientationClearDeviceOrientationOverride = NoResponse
    commandName _ = "DeviceOrientation.clearDeviceOrientationOverride"


-- | Parameters of the 'deviceOrientationSetDeviceOrientationOverride' command.
data PDeviceOrientationSetDeviceOrientationOverride = PDeviceOrientationSetDeviceOrientationOverride {
  -- | Mock alpha
  pDeviceOrientationSetDeviceOrientationOverrideAlpha :: Double,
  -- | Mock beta
  pDeviceOrientationSetDeviceOrientationOverrideBeta :: Double,
  -- | Mock gamma
  pDeviceOrientationSetDeviceOrientationOverrideGamma :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDeviceOrientationSetDeviceOrientationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  PDeviceOrientationSetDeviceOrientationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }


-- | Function for the 'DeviceOrientation.setDeviceOrientationOverride' command.
--   Overrides the Device Orientation.
--   Returns: 'PDeviceOrientationSetDeviceOrientationOverride'
deviceOrientationSetDeviceOrientationOverride :: Handle -> PDeviceOrientationSetDeviceOrientationOverride -> IO ()
deviceOrientationSetDeviceOrientationOverride handle params = sendReceiveCommand handle params

instance Command PDeviceOrientationSetDeviceOrientationOverride where
    type CommandResponse PDeviceOrientationSetDeviceOrientationOverride = NoResponse
    commandName _ = "DeviceOrientation.setDeviceOrientationOverride"



