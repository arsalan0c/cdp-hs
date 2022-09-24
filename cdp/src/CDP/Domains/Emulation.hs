{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Emulation (module CDP.Domains.Emulation) where

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

import CDP.Domains.DOMPageNetwork as DOMPageNetwork


data EmulationScreenOrientationType = EmulationScreenOrientationTypePortraitPrimary | EmulationScreenOrientationTypePortraitSecondary | EmulationScreenOrientationTypeLandscapePrimary | EmulationScreenOrientationTypeLandscapeSecondary
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationScreenOrientationType where
   parseJSON = A.withText  "EmulationScreenOrientationType"  $ \v -> do
      case v of
         "portraitPrimary" -> pure EmulationScreenOrientationTypePortraitPrimary
         "portraitSecondary" -> pure EmulationScreenOrientationTypePortraitSecondary
         "landscapePrimary" -> pure EmulationScreenOrientationTypeLandscapePrimary
         "landscapeSecondary" -> pure EmulationScreenOrientationTypeLandscapeSecondary
         _ -> fail "failed to parse EmulationScreenOrientationType"

instance ToJSON EmulationScreenOrientationType where
   toJSON v = A.String $
      case v of
         EmulationScreenOrientationTypePortraitPrimary -> "portraitPrimary"
         EmulationScreenOrientationTypePortraitSecondary -> "portraitSecondary"
         EmulationScreenOrientationTypeLandscapePrimary -> "landscapePrimary"
         EmulationScreenOrientationTypeLandscapeSecondary -> "landscapeSecondary"



data EmulationScreenOrientation = EmulationScreenOrientation {
   emulationScreenOrientationType :: EmulationScreenOrientationType,
   emulationScreenOrientationAngle :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationScreenOrientation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  EmulationScreenOrientation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


data EmulationDisplayFeatureOrientation = EmulationDisplayFeatureOrientationVertical | EmulationDisplayFeatureOrientationHorizontal
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisplayFeatureOrientation where
   parseJSON = A.withText  "EmulationDisplayFeatureOrientation"  $ \v -> do
      case v of
         "vertical" -> pure EmulationDisplayFeatureOrientationVertical
         "horizontal" -> pure EmulationDisplayFeatureOrientationHorizontal
         _ -> fail "failed to parse EmulationDisplayFeatureOrientation"

instance ToJSON EmulationDisplayFeatureOrientation where
   toJSON v = A.String $
      case v of
         EmulationDisplayFeatureOrientationVertical -> "vertical"
         EmulationDisplayFeatureOrientationHorizontal -> "horizontal"



data EmulationDisplayFeature = EmulationDisplayFeature {
   emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation,
   emulationDisplayFeatureOffset :: Int,
   emulationDisplayFeatureMaskLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data EmulationMediaFeature = EmulationMediaFeature {
   emulationMediaFeatureName :: String,
   emulationMediaFeatureValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationMediaFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  EmulationMediaFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }






emulationCanEmulate :: Handle ev -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())

data EmulationCanEmulate = EmulationCanEmulate {
   emulationCanEmulateResult :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command EmulationCanEmulate where
   commandName _ = "Emulation.canEmulate"



emulationClearDeviceMetricsOverride :: Handle ev -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())


emulationClearGeolocationOverride :: Handle ev -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())



data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
   pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMPageNetwork.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


emulationSetDefaultBackgroundColorOverride :: Handle ev -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)



data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
   pEmulationSetDeviceMetricsOverrideWidth :: Int,
   pEmulationSetDeviceMetricsOverrideHeight :: Int,
   pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
   pEmulationSetDeviceMetricsOverrideMobile :: Bool,
   pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetDeviceMetricsOverride :: Handle ev -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)



data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
   pEmulationSetEmulatedMediaMedia :: Maybe String,
   pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


emulationSetEmulatedMedia :: Handle ev -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia handle params = sendReceiveCommand handle "Emulation.setEmulatedMedia" (Just params)



data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
   pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
   pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
   pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetGeolocationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetGeolocationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


emulationSetGeolocationOverride :: Handle ev -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)



data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
   pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


emulationSetScriptExecutionDisabled :: Handle ev -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)



data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
   pEmulationSetTouchEmulationEnabledEnabled :: Bool,
   pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTouchEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTouchEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetTouchEmulationEnabled :: Handle ev -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)



data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
   pEmulationSetUserAgentOverrideUserAgent :: String,
   pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pEmulationSetUserAgentOverridePlatform :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


emulationSetUserAgentOverride :: Handle ev -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)



