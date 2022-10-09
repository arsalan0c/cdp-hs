{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  SystemInfo :
     The SystemInfo domain defines methods and events for querying low-level system information.

-}


module CDP.Domains.SystemInfo (module CDP.Domains.SystemInfo) where

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
import CDP.Handle




-- | Type 'SystemInfo.GPUDevice'.
--   Describes a single graphics processor (GPU).
data SystemInfoGPUDevice = SystemInfoGPUDevice {
  -- | PCI ID of the GPU vendor, if available; 0 otherwise.
  systemInfoGPUDeviceVendorId :: Double,
  -- | PCI ID of the GPU device, if available; 0 otherwise.
  systemInfoGPUDeviceDeviceId :: Double,
  -- | Sub sys ID of the GPU, only available on Windows.
  systemInfoGPUDeviceSubSysId :: Maybe Double,
  -- | Revision of the GPU, only available on Windows.
  systemInfoGPUDeviceRevision :: Maybe Double,
  -- | String description of the GPU vendor, if the PCI ID is not available.
  systemInfoGPUDeviceVendorString :: String,
  -- | String description of the GPU device, if the PCI ID is not available.
  systemInfoGPUDeviceDeviceString :: String,
  -- | String description of the GPU driver vendor.
  systemInfoGPUDeviceDriverVendor :: String,
  -- | String description of the GPU driver version.
  systemInfoGPUDeviceDriverVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoGPUDevice  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  SystemInfoGPUDevice where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'SystemInfo.Size'.
--   Describes the width and height dimensions of an entity.
data SystemInfoSize = SystemInfoSize {
  -- | Width in pixels.
  systemInfoSizeWidth :: Int,
  -- | Height in pixels.
  systemInfoSizeHeight :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoSize  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  SystemInfoSize where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'SystemInfo.VideoDecodeAcceleratorCapability'.
--   Describes a supported video decoding profile with its associated minimum and
--   maximum resolutions.
data SystemInfoVideoDecodeAcceleratorCapability = SystemInfoVideoDecodeAcceleratorCapability {
  -- | Video codec profile that is supported, e.g. VP9 Profile 2.
  systemInfoVideoDecodeAcceleratorCapabilityProfile :: String,
  -- | Maximum video dimensions in pixels supported for this |profile|.
  systemInfoVideoDecodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
  -- | Minimum video dimensions in pixels supported for this |profile|.
  systemInfoVideoDecodeAcceleratorCapabilityMinResolution :: SystemInfoSize
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoVideoDecodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoVideoDecodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'SystemInfo.VideoEncodeAcceleratorCapability'.
--   Describes a supported video encoding profile with its associated maximum
--   resolution and maximum framerate.
data SystemInfoVideoEncodeAcceleratorCapability = SystemInfoVideoEncodeAcceleratorCapability {
  -- | Video codec profile that is supported, e.g H264 Main.
  systemInfoVideoEncodeAcceleratorCapabilityProfile :: String,
  -- | Maximum video dimensions in pixels supported for this |profile|.
  systemInfoVideoEncodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
  -- | Maximum encoding framerate in frames per second supported for this
  --   |profile|, as fraction's numerator and denominator, e.g. 24/1 fps,
  --   24000/1001 fps, etc.
  systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateNumerator :: Int,
  systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateDenominator :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoVideoEncodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoVideoEncodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'SystemInfo.SubsamplingFormat'.
--   YUV subsampling type of the pixels of a given image.
data SystemInfoSubsamplingFormat = SystemInfoSubsamplingFormatYuv420 | SystemInfoSubsamplingFormatYuv422 | SystemInfoSubsamplingFormatYuv444
   deriving (Ord, Eq, Show, Read)
instance FromJSON SystemInfoSubsamplingFormat where
   parseJSON = A.withText  "SystemInfoSubsamplingFormat"  $ \v -> do
      case v of
         "yuv420" -> pure SystemInfoSubsamplingFormatYuv420
         "yuv422" -> pure SystemInfoSubsamplingFormatYuv422
         "yuv444" -> pure SystemInfoSubsamplingFormatYuv444
         _ -> fail "failed to parse SystemInfoSubsamplingFormat"

instance ToJSON SystemInfoSubsamplingFormat where
   toJSON v = A.String $
      case v of
         SystemInfoSubsamplingFormatYuv420 -> "yuv420"
         SystemInfoSubsamplingFormatYuv422 -> "yuv422"
         SystemInfoSubsamplingFormatYuv444 -> "yuv444"



-- | Type 'SystemInfo.ImageType'.
--   Image format of a given image.
data SystemInfoImageType = SystemInfoImageTypeJpeg | SystemInfoImageTypeWebp | SystemInfoImageTypeUnknown
   deriving (Ord, Eq, Show, Read)
instance FromJSON SystemInfoImageType where
   parseJSON = A.withText  "SystemInfoImageType"  $ \v -> do
      case v of
         "jpeg" -> pure SystemInfoImageTypeJpeg
         "webp" -> pure SystemInfoImageTypeWebp
         "unknown" -> pure SystemInfoImageTypeUnknown
         _ -> fail "failed to parse SystemInfoImageType"

instance ToJSON SystemInfoImageType where
   toJSON v = A.String $
      case v of
         SystemInfoImageTypeJpeg -> "jpeg"
         SystemInfoImageTypeWebp -> "webp"
         SystemInfoImageTypeUnknown -> "unknown"



-- | Type 'SystemInfo.ImageDecodeAcceleratorCapability'.
--   Describes a supported image decoding profile with its associated minimum and
--   maximum resolutions and subsampling.
data SystemInfoImageDecodeAcceleratorCapability = SystemInfoImageDecodeAcceleratorCapability {
  -- | Image coded, e.g. Jpeg.
  systemInfoImageDecodeAcceleratorCapabilityImageType :: SystemInfoImageType,
  -- | Maximum supported dimensions of the image in pixels.
  systemInfoImageDecodeAcceleratorCapabilityMaxDimensions :: SystemInfoSize,
  -- | Minimum supported dimensions of the image in pixels.
  systemInfoImageDecodeAcceleratorCapabilityMinDimensions :: SystemInfoSize,
  -- | Optional array of supported subsampling formats, e.g. 4:2:0, if known.
  systemInfoImageDecodeAcceleratorCapabilitySubsamplings :: [SystemInfoSubsamplingFormat]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoImageDecodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoImageDecodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'SystemInfo.GPUInfo'.
--   Provides information about the GPU(s) on the system.
data SystemInfoGPUInfo = SystemInfoGPUInfo {
  -- | The graphics devices on the system. Element 0 is the primary GPU.
  systemInfoGPUInfoDevices :: [SystemInfoGPUDevice],
  -- | An optional dictionary of additional GPU related attributes.
  systemInfoGPUInfoAuxAttributes :: Maybe [(String, String)],
  -- | An optional dictionary of graphics features and their status.
  systemInfoGPUInfoFeatureStatus :: Maybe [(String, String)],
  -- | An optional array of GPU driver bug workarounds.
  systemInfoGPUInfoDriverBugWorkarounds :: [String],
  -- | Supported accelerated video decoding capabilities.
  systemInfoGPUInfoVideoDecoding :: [SystemInfoVideoDecodeAcceleratorCapability],
  -- | Supported accelerated video encoding capabilities.
  systemInfoGPUInfoVideoEncoding :: [SystemInfoVideoEncodeAcceleratorCapability],
  -- | Supported accelerated image decoding capabilities.
  systemInfoGPUInfoImageDecoding :: [SystemInfoImageDecodeAcceleratorCapability]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoGPUInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  SystemInfoGPUInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'SystemInfo.ProcessInfo'.
--   Represents process info.
data SystemInfoProcessInfo = SystemInfoProcessInfo {
  -- | Specifies process type.
  systemInfoProcessInfoType :: String,
  -- | Specifies process id.
  systemInfoProcessInfoId :: Int,
  -- | Specifies cumulative CPU usage in seconds across all threads of the
  --   process since the process start.
  systemInfoProcessInfoCpuTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoProcessInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  SystemInfoProcessInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }







-- | Function for the 'SystemInfo.getInfo' command.
--   Returns information about the system.
--   Returns: 'SystemInfoGetInfo'
systemInfoGetInfo :: Handle -> IO SystemInfoGetInfo
systemInfoGetInfo handle = sendReceiveCommandResult handle "SystemInfo.getInfo" (Nothing :: Maybe ())

-- | Return type of the 'systemInfoGetInfo' command.
data SystemInfoGetInfo = SystemInfoGetInfo {
  -- | Information about the GPUs on the system.
  systemInfoGetInfoGpu :: SystemInfoGPUInfo,
  -- | A platform-dependent description of the model of the machine. On Mac OS, this is, for
  --   example, 'MacBookPro'. Will be the empty string if not supported.
  systemInfoGetInfoModelName :: String,
  -- | A platform-dependent description of the version of the machine. On Mac OS, this is, for
  --   example, '10.1'. Will be the empty string if not supported.
  systemInfoGetInfoModelVersion :: String,
  -- | The command line string used to launch the browser. Will be the empty string if not
  --   supported.
  systemInfoGetInfoCommandLine :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  SystemInfoGetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command SystemInfoGetInfo where
   commandName _ = "SystemInfo.getInfo"



-- | Function for the 'SystemInfo.getProcessInfo' command.
--   Returns information about all running processes.
--   Returns: 'SystemInfoGetProcessInfo'
systemInfoGetProcessInfo :: Handle -> IO SystemInfoGetProcessInfo
systemInfoGetProcessInfo handle = sendReceiveCommandResult handle "SystemInfo.getProcessInfo" (Nothing :: Maybe ())

-- | Return type of the 'systemInfoGetProcessInfo' command.
data SystemInfoGetProcessInfo = SystemInfoGetProcessInfo {
  -- | An array of process info blocks.
  systemInfoGetProcessInfoProcessInfo :: [SystemInfoProcessInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  SystemInfoGetProcessInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command SystemInfoGetProcessInfo where
   commandName _ = "SystemInfo.getProcessInfo"




