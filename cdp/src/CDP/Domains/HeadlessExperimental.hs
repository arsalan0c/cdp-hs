{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  HeadlessExperimental :
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



-- | Encoding options for a screenshot.
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
  -- | Image compression format (defaults to png).
  headlessExperimentalScreenshotParamsFormat :: HeadlessExperimentalScreenshotParamsFormat,
  -- | Compression quality from range [0..100] (jpeg only).
  headlessExperimentalScreenshotParamsQuality :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON HeadlessExperimentalScreenshotParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  HeadlessExperimentalScreenshotParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }







-- | Parameters of the 'headlessExperimentalBeginFrame' command.
data PHeadlessExperimentalBeginFrame = PHeadlessExperimentalBeginFrame {
  -- | Timestamp of this BeginFrame in Renderer TimeTicks (milliseconds of uptime). If not set,
    -- the current time will be used.
  pHeadlessExperimentalBeginFrameFrameTimeTicks :: Maybe Double,
  -- | The interval between BeginFrames that is reported to the compositor, in milliseconds.
    -- Defaults to a 60 frames/second interval, i.e. about 16.666 milliseconds.
  pHeadlessExperimentalBeginFrameInterval :: Maybe Double,
  -- | Whether updates should not be committed and drawn onto the display. False by default. If
    -- true, only side effects of the BeginFrame will be run, such as layout and animations, but
    -- any visual updates may not be visible on the display or in screenshots.
  pHeadlessExperimentalBeginFrameNoDisplayUpdates :: Maybe Bool,
  -- | If set, a screenshot of the frame will be captured and returned in the response. Otherwise,
    -- no screenshot will be captured. Note that capturing a screenshot can fail, for example,
    -- during renderer initialization. In such a case, no screenshot data will be returned.
  pHeadlessExperimentalBeginFrameScreenshot :: Maybe HeadlessExperimentalScreenshotParams
} deriving (Generic, Eq, Show, Read)
instance ToJSON PHeadlessExperimentalBeginFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PHeadlessExperimentalBeginFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'HeadlessExperimental.beginFrame' command.
 -- Sends a BeginFrame to the target and returns when the frame was completed. Optionally captures a
 -- screenshot from the resulting frame. Requires that the target was created with enabled
 -- BeginFrameControl. Designed for use with --run-all-compositor-stages-before-draw, see also
 -- https://goo.gle/chrome-headless-rendering for more background.
-- Parameters: 'PHeadlessExperimentalBeginFrame'
-- Returns: 'HeadlessExperimentalBeginFrame'
headlessExperimentalBeginFrame :: Handle ev -> PHeadlessExperimentalBeginFrame -> IO HeadlessExperimentalBeginFrame
headlessExperimentalBeginFrame handle params = sendReceiveCommandResult handle "HeadlessExperimental.beginFrame" (Just params)

-- | Return type of the 'headlessExperimentalBeginFrame' command.
data HeadlessExperimentalBeginFrame = HeadlessExperimentalBeginFrame {
  -- | Whether the BeginFrame resulted in damage and, thus, a new frame was committed to the
    -- display. Reported for diagnostic uses, may be removed in the future.
  headlessExperimentalBeginFrameHasDamage :: Bool,
  -- | Base64-encoded image data of the screenshot, if one was requested and successfully taken. (Encoded as a base64 string when passed over JSON)
  headlessExperimentalBeginFrameScreenshotData :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  HeadlessExperimentalBeginFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command HeadlessExperimentalBeginFrame where
   commandName _ = "HeadlessExperimental.beginFrame"



-- | Function for the 'HeadlessExperimental.disable' command.
 -- Disables headless events for the target.
headlessExperimentalDisable :: Handle ev -> IO ()
headlessExperimentalDisable handle = sendReceiveCommand handle "HeadlessExperimental.disable" (Nothing :: Maybe ())


-- | Function for the 'HeadlessExperimental.enable' command.
 -- Enables headless events for the target.
headlessExperimentalEnable :: Handle ev -> IO ()
headlessExperimentalEnable handle = sendReceiveCommand handle "HeadlessExperimental.enable" (Nothing :: Maybe ())



