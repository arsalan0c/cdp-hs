{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= HeadlessExperimental

This domain provides experimental commands only supported in headless mode.
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'HeadlessExperimental.ScreenshotParams'.
--   Encoding options for a screenshot.
data HeadlessExperimentalScreenshotParamsFormat = HeadlessExperimentalScreenshotParamsFormatJpeg | HeadlessExperimentalScreenshotParamsFormatPng
  deriving (Ord, Eq, Show, Read)
instance FromJSON HeadlessExperimentalScreenshotParamsFormat where
  parseJSON = A.withText "HeadlessExperimentalScreenshotParamsFormat" $ \v -> case v of
    "jpeg" -> pure HeadlessExperimentalScreenshotParamsFormatJpeg
    "png" -> pure HeadlessExperimentalScreenshotParamsFormatPng
    "_" -> fail "failed to parse HeadlessExperimentalScreenshotParamsFormat"
instance ToJSON HeadlessExperimentalScreenshotParamsFormat where
  toJSON v = A.String $ case v of
    HeadlessExperimentalScreenshotParamsFormatJpeg -> "jpeg"
    HeadlessExperimentalScreenshotParamsFormatPng -> "png"
data HeadlessExperimentalScreenshotParams = HeadlessExperimentalScreenshotParams
  {
    -- | Image compression format (defaults to png).
    headlessExperimentalScreenshotParamsFormat :: Maybe HeadlessExperimentalScreenshotParamsFormat,
    -- | Compression quality from range [0..100] (jpeg only).
    headlessExperimentalScreenshotParamsQuality :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON HeadlessExperimentalScreenshotParams where
  parseJSON = A.withObject "HeadlessExperimentalScreenshotParams" $ \o -> HeadlessExperimentalScreenshotParams
    <$> o A..:? "format"
    <*> o A..:? "quality"
instance ToJSON HeadlessExperimentalScreenshotParams where
  toJSON p = A.object $ catMaybes [
    ("format" A..=) <$> (headlessExperimentalScreenshotParamsFormat p),
    ("quality" A..=) <$> (headlessExperimentalScreenshotParamsQuality p)
    ]

-- | Sends a BeginFrame to the target and returns when the frame was completed. Optionally captures a
--   screenshot from the resulting frame. Requires that the target was created with enabled
--   BeginFrameControl. Designed for use with --run-all-compositor-stages-before-draw, see also
--   https://goo.gle/chrome-headless-rendering for more background.

-- | Parameters of the 'HeadlessExperimental.beginFrame' command.
data PHeadlessExperimentalBeginFrame = PHeadlessExperimentalBeginFrame
  {
    -- | Timestamp of this BeginFrame in Renderer TimeTicks (milliseconds of uptime). If not set,
    --   the current time will be used.
    pHeadlessExperimentalBeginFrameFrameTimeTicks :: Maybe Double,
    -- | The interval between BeginFrames that is reported to the compositor, in milliseconds.
    --   Defaults to a 60 frames/second interval, i.e. about 16.666 milliseconds.
    pHeadlessExperimentalBeginFrameInterval :: Maybe Double,
    -- | Whether updates should not be committed and drawn onto the display. False by default. If
    --   true, only side effects of the BeginFrame will be run, such as layout and animations, but
    --   any visual updates may not be visible on the display or in screenshots.
    pHeadlessExperimentalBeginFrameNoDisplayUpdates :: Maybe Bool,
    -- | If set, a screenshot of the frame will be captured and returned in the response. Otherwise,
    --   no screenshot will be captured. Note that capturing a screenshot can fail, for example,
    --   during renderer initialization. In such a case, no screenshot data will be returned.
    pHeadlessExperimentalBeginFrameScreenshot :: Maybe HeadlessExperimentalScreenshotParams
  }
  deriving (Eq, Show)
pHeadlessExperimentalBeginFrame
  :: PHeadlessExperimentalBeginFrame
pHeadlessExperimentalBeginFrame
  = PHeadlessExperimentalBeginFrame
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PHeadlessExperimentalBeginFrame where
  toJSON p = A.object $ catMaybes [
    ("frameTimeTicks" A..=) <$> (pHeadlessExperimentalBeginFrameFrameTimeTicks p),
    ("interval" A..=) <$> (pHeadlessExperimentalBeginFrameInterval p),
    ("noDisplayUpdates" A..=) <$> (pHeadlessExperimentalBeginFrameNoDisplayUpdates p),
    ("screenshot" A..=) <$> (pHeadlessExperimentalBeginFrameScreenshot p)
    ]
data HeadlessExperimentalBeginFrame = HeadlessExperimentalBeginFrame
  {
    -- | Whether the BeginFrame resulted in damage and, thus, a new frame was committed to the
    --   display. Reported for diagnostic uses, may be removed in the future.
    headlessExperimentalBeginFrameHasDamage :: Bool,
    -- | Base64-encoded image data of the screenshot, if one was requested and successfully taken. (Encoded as a base64 string when passed over JSON)
    headlessExperimentalBeginFrameScreenshotData :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON HeadlessExperimentalBeginFrame where
  parseJSON = A.withObject "HeadlessExperimentalBeginFrame" $ \o -> HeadlessExperimentalBeginFrame
    <$> o A..: "hasDamage"
    <*> o A..:? "screenshotData"
instance Command PHeadlessExperimentalBeginFrame where
  type CommandResponse PHeadlessExperimentalBeginFrame = HeadlessExperimentalBeginFrame
  commandName _ = "HeadlessExperimental.beginFrame"

-- | Disables headless events for the target.

-- | Parameters of the 'HeadlessExperimental.disable' command.
data PHeadlessExperimentalDisable = PHeadlessExperimentalDisable
  deriving (Eq, Show)
pHeadlessExperimentalDisable
  :: PHeadlessExperimentalDisable
pHeadlessExperimentalDisable
  = PHeadlessExperimentalDisable
instance ToJSON PHeadlessExperimentalDisable where
  toJSON _ = A.Null
instance Command PHeadlessExperimentalDisable where
  type CommandResponse PHeadlessExperimentalDisable = ()
  commandName _ = "HeadlessExperimental.disable"
  fromJSON = const . A.Success . const ()

-- | Enables headless events for the target.

-- | Parameters of the 'HeadlessExperimental.enable' command.
data PHeadlessExperimentalEnable = PHeadlessExperimentalEnable
  deriving (Eq, Show)
pHeadlessExperimentalEnable
  :: PHeadlessExperimentalEnable
pHeadlessExperimentalEnable
  = PHeadlessExperimentalEnable
instance ToJSON PHeadlessExperimentalEnable where
  toJSON _ = A.Null
instance Command PHeadlessExperimentalEnable where
  type CommandResponse PHeadlessExperimentalEnable = ()
  commandName _ = "HeadlessExperimental.enable"
  fromJSON = const . A.Success . const ()

