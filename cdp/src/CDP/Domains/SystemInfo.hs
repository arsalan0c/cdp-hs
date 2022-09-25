{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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




data SystemInfoGpuDevice = SystemInfoGpuDevice {
   systemInfoGpuDeviceVendorId :: Double,
   systemInfoGpuDeviceDeviceId :: Double,
   systemInfoGpuDeviceSubSysId :: Maybe Double,
   systemInfoGpuDeviceRevision :: Maybe Double,
   systemInfoGpuDeviceVendorString :: String,
   systemInfoGpuDeviceDeviceString :: String,
   systemInfoGpuDeviceDriverVendor :: String,
   systemInfoGpuDeviceDriverVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoGpuDevice  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  SystemInfoGpuDevice where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data SystemInfoSize = SystemInfoSize {
   systemInfoSizeWidth :: Int,
   systemInfoSizeHeight :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoSize  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  SystemInfoSize where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data SystemInfoVideoDecodeAcceleratorCapability = SystemInfoVideoDecodeAcceleratorCapability {
   systemInfoVideoDecodeAcceleratorCapabilityProfile :: String,
   systemInfoVideoDecodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
   systemInfoVideoDecodeAcceleratorCapabilityMinResolution :: SystemInfoSize
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoVideoDecodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoVideoDecodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



data SystemInfoVideoEncodeAcceleratorCapability = SystemInfoVideoEncodeAcceleratorCapability {
   systemInfoVideoEncodeAcceleratorCapabilityProfile :: String,
   systemInfoVideoEncodeAcceleratorCapabilityMaxResolution :: SystemInfoSize,
   systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateNumerator :: Int,
   systemInfoVideoEncodeAcceleratorCapabilityMaxFramerateDenominator :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoVideoEncodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoVideoEncodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }


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



data SystemInfoImageDecodeAcceleratorCapability = SystemInfoImageDecodeAcceleratorCapability {
   systemInfoImageDecodeAcceleratorCapabilityImageType :: SystemInfoImageType,
   systemInfoImageDecodeAcceleratorCapabilityMaxDimensions :: SystemInfoSize,
   systemInfoImageDecodeAcceleratorCapabilityMinDimensions :: SystemInfoSize,
   systemInfoImageDecodeAcceleratorCapabilitySubsamplings :: [SystemInfoSubsamplingFormat]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoImageDecodeAcceleratorCapability  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  SystemInfoImageDecodeAcceleratorCapability where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



data SystemInfoGpuInfo = SystemInfoGpuInfo {
   systemInfoGpuInfoDevices :: [SystemInfoGpuDevice],
   systemInfoGpuInfoAuxAttributes :: Maybe [(String, String)],
   systemInfoGpuInfoFeatureStatus :: Maybe [(String, String)],
   systemInfoGpuInfoDriverBugWorkarounds :: [String],
   systemInfoGpuInfoVideoDecoding :: [SystemInfoVideoDecodeAcceleratorCapability],
   systemInfoGpuInfoVideoEncoding :: [SystemInfoVideoEncodeAcceleratorCapability],
   systemInfoGpuInfoImageDecoding :: [SystemInfoImageDecodeAcceleratorCapability]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoGpuInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  SystemInfoGpuInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data SystemInfoProcessInfo = SystemInfoProcessInfo {
   systemInfoProcessInfoType :: String,
   systemInfoProcessInfoId :: Int,
   systemInfoProcessInfoCpuTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON SystemInfoProcessInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  SystemInfoProcessInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }






systemInfoGetInfo :: Handle ev -> IO (Either Error SystemInfoGetInfo)
systemInfoGetInfo handle = sendReceiveCommandResult handle "SystemInfo.getInfo" (Nothing :: Maybe ())

data SystemInfoGetInfo = SystemInfoGetInfo {
   systemInfoGetInfoGpu :: SystemInfoGpuInfo,
   systemInfoGetInfoModelName :: String,
   systemInfoGetInfoModelVersion :: String,
   systemInfoGetInfoCommandLine :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  SystemInfoGetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command SystemInfoGetInfo where
   commandName _ = "SystemInfo.getInfo"



systemInfoGetProcessInfo :: Handle ev -> IO (Either Error SystemInfoGetProcessInfo)
systemInfoGetProcessInfo handle = sendReceiveCommandResult handle "SystemInfo.getProcessInfo" (Nothing :: Maybe ())

data SystemInfoGetProcessInfo = SystemInfoGetProcessInfo {
   systemInfoGetProcessInfoProcessInfo :: [SystemInfoProcessInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  SystemInfoGetProcessInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command SystemInfoGetProcessInfo where
   commandName _ = "SystemInfo.getProcessInfo"




