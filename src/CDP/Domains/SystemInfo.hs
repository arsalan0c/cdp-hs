{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= SystemInfo

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

import CDP.Internal.Utils




-- | Type 'SystemInfo.GPUDevice'.
--   Describes a single graphics processor (GPU).
data SystemInfoGPUDevice = SystemInfoGPUDevice
  {
    -- | PCI ID of the GPU vendor, if available; 0 otherwise.
    systemInfoGPUDeviceVendorId :: Double,
    -- | PCI ID of the GPU device, if available; 0 otherwise.
    systemInfoGPUDeviceDeviceId :: Double,
    -- | Sub sys ID of the GPU, only available on Windows.
    systemInfoGPUDeviceSubSysId :: Maybe Double,
    -- | Revision of the GPU, only available on Windows.
    systemInfoGPUDeviceRevision :: Maybe Double,
    -- | String description of the GPU vendor, if the PCI ID is not available.
    systemInfoGPUDeviceVendorString :: T.Text,
    -- | String description of the GPU device, if the PCI ID is not available.
    systemInfoGPUDeviceDeviceString :: T.Text,
    -- | String description of the GPU driver vendor.
    systemInfoGPUDeviceDriverVendor :: T.Text,
    -- | String description of the GPU driver version.
    systemInfoGPUDeviceDriverVersion :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoGPUDevice where
  parseJSON = A.withObject "SystemInfoGPUDevice" $ \o -> SystemInfoGPUDevice
    <$> o A..: "vendorId"
    <*> o A..: "deviceId"
    <*> o A..:? "subSysId"
    <*> o A..:? "revision"
    <*> o A..: "vendorString"
    <*> o A..: "deviceString"
    <*> o A..: "driverVendor"
    <*> o A..: "driverVersion"
instance ToJSON SystemInfoGPUDevice where
  toJSON p = A.object $ catMaybes [
    ("vendorId" A..=) <$> Just (systemInfoGPUDeviceVendorId p),
    ("deviceId" A..=) <$> Just (systemInfoGPUDeviceDeviceId p),
    ("subSysId" A..=) <$> (systemInfoGPUDeviceSubSysId p),
    ("revision" A..=) <$> (systemInfoGPUDeviceRevision p),
    ("vendorString" A..=) <$> Just (systemInfoGPUDeviceVendorString p),
    ("deviceString" A..=) <$> Just (systemInfoGPUDeviceDeviceString p),
    ("driverVendor" A..=) <$> Just (systemInfoGPUDeviceDriverVendor p),
    ("driverVersion" A..=) <$> Just (systemInfoGPUDeviceDriverVersion p)
    ]

-- | Type 'SystemInfo.Size'.
--   Describes the width and height dimensions of an entity.
data SystemInfoSize = SystemInfoSize
  {
    -- | Width in pixels.
    systemInfoSizeWidth :: Int,
    -- | Height in pixels.
    systemInfoSizeHeight :: Int
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoSize where
  parseJSON = A.withObject "SystemInfoSize" $ \o -> SystemInfoSize
    <$> o A..: "width"
    <*> o A..: "height"
instance ToJSON SystemInfoSize where
  toJSON p = A.object $ catMaybes [
    ("width" A..=) <$> Just (systemInfoSizeWidth p),
    ("height" A..=) <$> Just (systemInfoSizeHeight p)
    ]

-- | Type 'SystemInfo.VideoDecodeAcceleratorCapability'.
--   Describes a supported video decoding profile with its associated minimum and
--   maximum resolutions.
data SystemInfoVideoDecodeAcceleratorCapability = SystemInfoVideoDecodeAcceleratorCapability
  {
    -- | Video codec profile that is supported, e.g. VP9 Profile 2.
    systemInfoVideoDecodeAcceleratorCapabilityProfile :: T.Text,
    -- | Maximum video dimensions in pixels supported for this |profile|.
    systemInfoVideoDecodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
    -- | Minimum video dimensions in pixels supported for this |profile|.
    systemInfoVideoDecodeAcceleratorCapabilityMinResolution :: SystemInfoSize
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoVideoDecodeAcceleratorCapability where
  parseJSON = A.withObject "SystemInfoVideoDecodeAcceleratorCapability" $ \o -> SystemInfoVideoDecodeAcceleratorCapability
    <$> o A..: "profile"
    <*> o A..: "maxResolution"
    <*> o A..: "minResolution"
instance ToJSON SystemInfoVideoDecodeAcceleratorCapability where
  toJSON p = A.object $ catMaybes [
    ("profile" A..=) <$> Just (systemInfoVideoDecodeAcceleratorCapabilityProfile p),
    ("maxResolution" A..=) <$> Just (systemInfoVideoDecodeAcceleratorCapabilityMaxResolution p),
    ("minResolution" A..=) <$> Just (systemInfoVideoDecodeAcceleratorCapabilityMinResolution p)
    ]

-- | Type 'SystemInfo.VideoEncodeAcceleratorCapability'.
--   Describes a supported video encoding profile with its associated maximum
--   resolution and maximum framerate.
data SystemInfoVideoEncodeAcceleratorCapability = SystemInfoVideoEncodeAcceleratorCapability
  {
    -- | Video codec profile that is supported, e.g H264 Main.
    systemInfoVideoEncodeAcceleratorCapabilityProfile :: T.Text,
    -- | Maximum video dimensions in pixels supported for this |profile|.
    systemInfoVideoEncodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
    -- | Maximum encoding framerate in frames per second supported for this
    --   |profile|, as fraction's numerator and denominator, e.g. 24/1 fps,
    --   24000/1001 fps, etc.
    systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateNumerator :: Int,
    systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateDenominator :: Int
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoVideoEncodeAcceleratorCapability where
  parseJSON = A.withObject "SystemInfoVideoEncodeAcceleratorCapability" $ \o -> SystemInfoVideoEncodeAcceleratorCapability
    <$> o A..: "profile"
    <*> o A..: "maxResolution"
    <*> o A..: "maxFramerateNumerator"
    <*> o A..: "maxFramerateDenominator"
instance ToJSON SystemInfoVideoEncodeAcceleratorCapability where
  toJSON p = A.object $ catMaybes [
    ("profile" A..=) <$> Just (systemInfoVideoEncodeAcceleratorCapabilityProfile p),
    ("maxResolution" A..=) <$> Just (systemInfoVideoEncodeAcceleratorCapabilityMaxResolution p),
    ("maxFramerateNumerator" A..=) <$> Just (systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateNumerator p),
    ("maxFramerateDenominator" A..=) <$> Just (systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateDenominator p)
    ]

-- | Type 'SystemInfo.SubsamplingFormat'.
--   YUV subsampling type of the pixels of a given image.
data SystemInfoSubsamplingFormat = SystemInfoSubsamplingFormatYuv420 | SystemInfoSubsamplingFormatYuv422 | SystemInfoSubsamplingFormatYuv444
  deriving (Ord, Eq, Show, Read)
instance FromJSON SystemInfoSubsamplingFormat where
  parseJSON = A.withText "SystemInfoSubsamplingFormat" $ \v -> case v of
    "yuv420" -> pure SystemInfoSubsamplingFormatYuv420
    "yuv422" -> pure SystemInfoSubsamplingFormatYuv422
    "yuv444" -> pure SystemInfoSubsamplingFormatYuv444
    "_" -> fail "failed to parse SystemInfoSubsamplingFormat"
instance ToJSON SystemInfoSubsamplingFormat where
  toJSON v = A.String $ case v of
    SystemInfoSubsamplingFormatYuv420 -> "yuv420"
    SystemInfoSubsamplingFormatYuv422 -> "yuv422"
    SystemInfoSubsamplingFormatYuv444 -> "yuv444"

-- | Type 'SystemInfo.ImageType'.
--   Image format of a given image.
data SystemInfoImageType = SystemInfoImageTypeJpeg | SystemInfoImageTypeWebp | SystemInfoImageTypeUnknown
  deriving (Ord, Eq, Show, Read)
instance FromJSON SystemInfoImageType where
  parseJSON = A.withText "SystemInfoImageType" $ \v -> case v of
    "jpeg" -> pure SystemInfoImageTypeJpeg
    "webp" -> pure SystemInfoImageTypeWebp
    "unknown" -> pure SystemInfoImageTypeUnknown
    "_" -> fail "failed to parse SystemInfoImageType"
instance ToJSON SystemInfoImageType where
  toJSON v = A.String $ case v of
    SystemInfoImageTypeJpeg -> "jpeg"
    SystemInfoImageTypeWebp -> "webp"
    SystemInfoImageTypeUnknown -> "unknown"

-- | Type 'SystemInfo.ImageDecodeAcceleratorCapability'.
--   Describes a supported image decoding profile with its associated minimum and
--   maximum resolutions and subsampling.
data SystemInfoImageDecodeAcceleratorCapability = SystemInfoImageDecodeAcceleratorCapability
  {
    -- | Image coded, e.g. Jpeg.
    systemInfoImageDecodeAcceleratorCapabilityImageType :: SystemInfoImageType,
    -- | Maximum supported dimensions of the image in pixels.
    systemInfoImageDecodeAcceleratorCapabilityMaxDimensions :: SystemInfoSize,
    -- | Minimum supported dimensions of the image in pixels.
    systemInfoImageDecodeAcceleratorCapabilityMinDimensions :: SystemInfoSize,
    -- | Optional array of supported subsampling formats, e.g. 4:2:0, if known.
    systemInfoImageDecodeAcceleratorCapabilitySubsamplings :: [SystemInfoSubsamplingFormat]
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoImageDecodeAcceleratorCapability where
  parseJSON = A.withObject "SystemInfoImageDecodeAcceleratorCapability" $ \o -> SystemInfoImageDecodeAcceleratorCapability
    <$> o A..: "imageType"
    <*> o A..: "maxDimensions"
    <*> o A..: "minDimensions"
    <*> o A..: "subsamplings"
instance ToJSON SystemInfoImageDecodeAcceleratorCapability where
  toJSON p = A.object $ catMaybes [
    ("imageType" A..=) <$> Just (systemInfoImageDecodeAcceleratorCapabilityImageType p),
    ("maxDimensions" A..=) <$> Just (systemInfoImageDecodeAcceleratorCapabilityMaxDimensions p),
    ("minDimensions" A..=) <$> Just (systemInfoImageDecodeAcceleratorCapabilityMinDimensions p),
    ("subsamplings" A..=) <$> Just (systemInfoImageDecodeAcceleratorCapabilitySubsamplings p)
    ]

-- | Type 'SystemInfo.GPUInfo'.
--   Provides information about the GPU(s) on the system.
data SystemInfoGPUInfo = SystemInfoGPUInfo
  {
    -- | The graphics devices on the system. Element 0 is the primary GPU.
    systemInfoGPUInfoDevices :: [SystemInfoGPUDevice],
    -- | An optional dictionary of additional GPU related attributes.
    systemInfoGPUInfoAuxAttributes :: Maybe [(T.Text, T.Text)],
    -- | An optional dictionary of graphics features and their status.
    systemInfoGPUInfoFeatureStatus :: Maybe [(T.Text, T.Text)],
    -- | An optional array of GPU driver bug workarounds.
    systemInfoGPUInfoDriverBugWorkarounds :: [T.Text],
    -- | Supported accelerated video decoding capabilities.
    systemInfoGPUInfoVideoDecoding :: [SystemInfoVideoDecodeAcceleratorCapability],
    -- | Supported accelerated video encoding capabilities.
    systemInfoGPUInfoVideoEncoding :: [SystemInfoVideoEncodeAcceleratorCapability],
    -- | Supported accelerated image decoding capabilities.
    systemInfoGPUInfoImageDecoding :: [SystemInfoImageDecodeAcceleratorCapability]
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoGPUInfo where
  parseJSON = A.withObject "SystemInfoGPUInfo" $ \o -> SystemInfoGPUInfo
    <$> o A..: "devices"
    <*> o A..:? "auxAttributes"
    <*> o A..:? "featureStatus"
    <*> o A..: "driverBugWorkarounds"
    <*> o A..: "videoDecoding"
    <*> o A..: "videoEncoding"
    <*> o A..: "imageDecoding"
instance ToJSON SystemInfoGPUInfo where
  toJSON p = A.object $ catMaybes [
    ("devices" A..=) <$> Just (systemInfoGPUInfoDevices p),
    ("auxAttributes" A..=) <$> (systemInfoGPUInfoAuxAttributes p),
    ("featureStatus" A..=) <$> (systemInfoGPUInfoFeatureStatus p),
    ("driverBugWorkarounds" A..=) <$> Just (systemInfoGPUInfoDriverBugWorkarounds p),
    ("videoDecoding" A..=) <$> Just (systemInfoGPUInfoVideoDecoding p),
    ("videoEncoding" A..=) <$> Just (systemInfoGPUInfoVideoEncoding p),
    ("imageDecoding" A..=) <$> Just (systemInfoGPUInfoImageDecoding p)
    ]

-- | Type 'SystemInfo.ProcessInfo'.
--   Represents process info.
data SystemInfoProcessInfo = SystemInfoProcessInfo
  {
    -- | Specifies process type.
    systemInfoProcessInfoType :: T.Text,
    -- | Specifies process id.
    systemInfoProcessInfoId :: Int,
    -- | Specifies cumulative CPU usage in seconds across all threads of the
    --   process since the process start.
    systemInfoProcessInfoCpuTime :: Double
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoProcessInfo where
  parseJSON = A.withObject "SystemInfoProcessInfo" $ \o -> SystemInfoProcessInfo
    <$> o A..: "type"
    <*> o A..: "id"
    <*> o A..: "cpuTime"
instance ToJSON SystemInfoProcessInfo where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (systemInfoProcessInfoType p),
    ("id" A..=) <$> Just (systemInfoProcessInfoId p),
    ("cpuTime" A..=) <$> Just (systemInfoProcessInfoCpuTime p)
    ]

-- | Returns information about the system.

-- | Parameters of the 'SystemInfo.getInfo' command.
data PSystemInfoGetInfo = PSystemInfoGetInfo
  deriving (Eq, Show)
pSystemInfoGetInfo
  :: PSystemInfoGetInfo
pSystemInfoGetInfo
  = PSystemInfoGetInfo
instance ToJSON PSystemInfoGetInfo where
  toJSON _ = A.Null
data SystemInfoGetInfo = SystemInfoGetInfo
  {
    -- | Information about the GPUs on the system.
    systemInfoGetInfoGpu :: SystemInfoGPUInfo,
    -- | A platform-dependent description of the model of the machine. On Mac OS, this is, for
    --   example, 'MacBookPro'. Will be the empty string if not supported.
    systemInfoGetInfoModelName :: T.Text,
    -- | A platform-dependent description of the version of the machine. On Mac OS, this is, for
    --   example, '10.1'. Will be the empty string if not supported.
    systemInfoGetInfoModelVersion :: T.Text,
    -- | The command line string used to launch the browser. Will be the empty string if not
    --   supported.
    systemInfoGetInfoCommandLine :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoGetInfo where
  parseJSON = A.withObject "SystemInfoGetInfo" $ \o -> SystemInfoGetInfo
    <$> o A..: "gpu"
    <*> o A..: "modelName"
    <*> o A..: "modelVersion"
    <*> o A..: "commandLine"
instance Command PSystemInfoGetInfo where
  type CommandResponse PSystemInfoGetInfo = SystemInfoGetInfo
  commandName _ = "SystemInfo.getInfo"

-- | Returns information about all running processes.

-- | Parameters of the 'SystemInfo.getProcessInfo' command.
data PSystemInfoGetProcessInfo = PSystemInfoGetProcessInfo
  deriving (Eq, Show)
pSystemInfoGetProcessInfo
  :: PSystemInfoGetProcessInfo
pSystemInfoGetProcessInfo
  = PSystemInfoGetProcessInfo
instance ToJSON PSystemInfoGetProcessInfo where
  toJSON _ = A.Null
data SystemInfoGetProcessInfo = SystemInfoGetProcessInfo
  {
    -- | An array of process info blocks.
    systemInfoGetProcessInfoProcessInfo :: [SystemInfoProcessInfo]
  }
  deriving (Eq, Show)
instance FromJSON SystemInfoGetProcessInfo where
  parseJSON = A.withObject "SystemInfoGetProcessInfo" $ \o -> SystemInfoGetProcessInfo
    <$> o A..: "processInfo"
instance Command PSystemInfoGetProcessInfo where
  type CommandResponse PSystemInfoGetProcessInfo = SystemInfoGetProcessInfo
  commandName _ = "SystemInfo.getProcessInfo"

