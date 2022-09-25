{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.HeadlessExperimental (module CDP.Domains.HeadlessExperimental) where

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



data HeadlessExperimentalScreenshotParamsFormat = HeadlessExperimentalScreenshotParamsFormatJpeg | HeadlessExperimentalScreenshotParamsFormatPng
   deriving (Ord, Eq, Show, Read)
instance FromJSON HeadlessExperimentalScreenshotParamsFormat where
   parseJSON = A.withText  "HeadlessExperimentalScreenshotParamsFormat"  $ \v -> do
      case v of
         "jpeg" -> pure HeadlessExperimentalScreenshotParamsFormatJpeg
         "png" -> pure HeadlessExperimentalScreenshotParamsFormatPng
         _ -> fail "failed to parse HeadlessExperimentalScreenshotParamsFormat"

instance ToJSON HeadlessExperimentalScreenshotParamsFormat where
   toJSON v = A.String $
      case v of
         HeadlessExperimentalScreenshotParamsFormatJpeg -> "jpeg"
         HeadlessExperimentalScreenshotParamsFormatPng -> "png"



data HeadlessExperimentalScreenshotParams = HeadlessExperimentalScreenshotParams {
   headlessExperimentalScreenshotParamsFormat :: HeadlessExperimentalScreenshotParamsFormat,
   headlessExperimentalScreenshotParamsQuality :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeadlessExperimentalScreenshotParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  HeadlessExperimentalScreenshotParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }







data PHeadlessExperimentalBeginFrame = PHeadlessExperimentalBeginFrame {
   pHeadlessExperimentalBeginFrameFrameTimeTicks :: Maybe Double,
   pHeadlessExperimentalBeginFrameInterval :: Maybe Double,
   pHeadlessExperimentalBeginFrameNoDisplayUpdates :: Maybe Bool,
   pHeadlessExperimentalBeginFrameScreenshot :: Maybe HeadlessExperimentalScreenshotParams
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeadlessExperimentalBeginFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PHeadlessExperimentalBeginFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


headlessExperimentalBeginFrame :: Handle ev -> PHeadlessExperimentalBeginFrame -> IO (Either Error HeadlessExperimentalBeginFrame)
headlessExperimentalBeginFrame handle params = sendReceiveCommandResult handle "HeadlessExperimental.beginFrame" (Just params)

data HeadlessExperimentalBeginFrame = HeadlessExperimentalBeginFrame {
   headlessExperimentalBeginFrameHasDamage :: Bool,
   headlessExperimentalBeginFrameScreenshotData :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeadlessExperimentalBeginFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command HeadlessExperimentalBeginFrame where
   commandName _ = "HeadlessExperimental.beginFrame"



headlessExperimentalDisable :: Handle ev -> IO (Maybe Error)
headlessExperimentalDisable handle = sendReceiveCommand handle "HeadlessExperimental.disable" (Nothing :: Maybe ())


headlessExperimentalEnable :: Handle ev -> IO (Maybe Error)
headlessExperimentalEnable handle = sendReceiveCommand handle "HeadlessExperimental.enable" (Nothing :: Maybe ())



