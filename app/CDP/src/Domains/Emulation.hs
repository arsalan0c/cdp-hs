{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.Emulation (module Domains.Emulation) where

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

import Utils

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema




data EmulationScreenOrientation = EmulationScreenOrientation {
    emulationScreenOrientationType :: String,
    emulationScreenOrientationAngle :: Int
} deriving (Eq, Show, Read)
instance FromJSON  EmulationScreenOrientation where
    parseJSON = A.withObject "EmulationScreenOrientation" $ \v ->
         EmulationScreenOrientation <$> v .:  "type"
            <*> v  .:  "angle"


instance ToJSON EmulationScreenOrientation  where
    toJSON v = A.object
        [ "type" .= emulationScreenOrientationType v
        , "angle" .= emulationScreenOrientationAngle v
        ]



data EmulationDisplayFeature = EmulationDisplayFeature {
    emulationDisplayFeatureOrientation :: String,
    emulationDisplayFeatureOffset :: Int,
    emulationDisplayFeatureMaskLength :: Int
} deriving (Eq, Show, Read)
instance FromJSON  EmulationDisplayFeature where
    parseJSON = A.withObject "EmulationDisplayFeature" $ \v ->
         EmulationDisplayFeature <$> v .:  "orientation"
            <*> v  .:  "offset"
            <*> v  .:  "maskLength"


instance ToJSON EmulationDisplayFeature  where
    toJSON v = A.object
        [ "orientation" .= emulationDisplayFeatureOrientation v
        , "offset" .= emulationDisplayFeatureOffset v
        , "maskLength" .= emulationDisplayFeatureMaskLength v
        ]



data EmulationMediaFeature = EmulationMediaFeature {
    emulationMediaFeatureName :: String,
    emulationMediaFeatureValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  EmulationMediaFeature where
    parseJSON = A.withObject "EmulationMediaFeature" $ \v ->
         EmulationMediaFeature <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON EmulationMediaFeature  where
    toJSON v = A.object
        [ "name" .= emulationMediaFeatureName v
        , "value" .= emulationMediaFeatureValue v
        ]



data EmulationCanEmulate = EmulationCanEmulate {
    emulationCanEmulateResult :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  EmulationCanEmulate where
    parseJSON = A.withObject "EmulationCanEmulate" $ \v ->
         EmulationCanEmulate <$> v .:  "result"



instance Command  EmulationCanEmulate where
    commandName _ = "Emulation.canEmulate"


emulationCanEmulate :: Session -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate session = sendReceiveCommandResult session "Emulation.canEmulate" (Nothing :: Maybe ())




emulationClearDeviceMetricsOverride :: Session -> IO (Maybe Error)
emulationClearDeviceMetricsOverride session = sendReceiveCommand session "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())




emulationClearGeolocationOverride :: Session -> IO (Maybe Error)
emulationClearGeolocationOverride session = sendReceiveCommand session "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())



data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
    pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMRGBA
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
    parseJSON = A.withObject "PEmulationSetDefaultBackgroundColorOverride" $ \v ->
         PEmulationSetDefaultBackgroundColorOverride <$> v .:?  "color"


instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
    toJSON v = A.object
        [ "color" .= pEmulationSetDefaultBackgroundColorOverrideColor v
        ]


emulationSetDefaultBackgroundColorOverride :: Session -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride session params = sendReceiveCommand session "Emulation.setDefaultBackgroundColorOverride" (Just params)



data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
    pEmulationSetDeviceMetricsOverrideWidth :: Int,
    pEmulationSetDeviceMetricsOverrideHeight :: Int,
    pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Int,
    pEmulationSetDeviceMetricsOverrideMobile :: Bool,
    pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetDeviceMetricsOverride where
    parseJSON = A.withObject "PEmulationSetDeviceMetricsOverride" $ \v ->
         PEmulationSetDeviceMetricsOverride <$> v .:  "width"
            <*> v  .:  "height"
            <*> v  .:  "deviceScaleFactor"
            <*> v  .:  "mobile"
            <*> v  .:?  "screenOrientation"


instance ToJSON PEmulationSetDeviceMetricsOverride  where
    toJSON v = A.object
        [ "width" .= pEmulationSetDeviceMetricsOverrideWidth v
        , "height" .= pEmulationSetDeviceMetricsOverrideHeight v
        , "deviceScaleFactor" .= pEmulationSetDeviceMetricsOverrideDeviceScaleFactor v
        , "mobile" .= pEmulationSetDeviceMetricsOverrideMobile v
        , "screenOrientation" .= pEmulationSetDeviceMetricsOverrideScreenOrientation v
        ]


emulationSetDeviceMetricsOverride :: Session -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride session params = sendReceiveCommand session "Emulation.setDeviceMetricsOverride" (Just params)



data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
    pEmulationSetEmulatedMediaMedia :: Maybe String,
    pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetEmulatedMedia where
    parseJSON = A.withObject "PEmulationSetEmulatedMedia" $ \v ->
         PEmulationSetEmulatedMedia <$> v .:?  "media"
            <*> v  .:?  "features"


instance ToJSON PEmulationSetEmulatedMedia  where
    toJSON v = A.object
        [ "media" .= pEmulationSetEmulatedMediaMedia v
        , "features" .= pEmulationSetEmulatedMediaFeatures v
        ]


emulationSetEmulatedMedia :: Session -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia session params = sendReceiveCommand session "Emulation.setEmulatedMedia" (Just params)



data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
    pEmulationSetGeolocationOverrideLatitude :: Maybe Int,
    pEmulationSetGeolocationOverrideLongitude :: Maybe Int,
    pEmulationSetGeolocationOverrideAccuracy :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetGeolocationOverride where
    parseJSON = A.withObject "PEmulationSetGeolocationOverride" $ \v ->
         PEmulationSetGeolocationOverride <$> v .:?  "latitude"
            <*> v  .:?  "longitude"
            <*> v  .:?  "accuracy"


instance ToJSON PEmulationSetGeolocationOverride  where
    toJSON v = A.object
        [ "latitude" .= pEmulationSetGeolocationOverrideLatitude v
        , "longitude" .= pEmulationSetGeolocationOverrideLongitude v
        , "accuracy" .= pEmulationSetGeolocationOverrideAccuracy v
        ]


emulationSetGeolocationOverride :: Session -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride session params = sendReceiveCommand session "Emulation.setGeolocationOverride" (Just params)



data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
    pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetScriptExecutionDisabled where
    parseJSON = A.withObject "PEmulationSetScriptExecutionDisabled" $ \v ->
         PEmulationSetScriptExecutionDisabled <$> v .:  "value"


instance ToJSON PEmulationSetScriptExecutionDisabled  where
    toJSON v = A.object
        [ "value" .= pEmulationSetScriptExecutionDisabledValue v
        ]


emulationSetScriptExecutionDisabled :: Session -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled session params = sendReceiveCommand session "Emulation.setScriptExecutionDisabled" (Just params)



data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
    pEmulationSetTouchEmulationEnabledEnabled :: Bool,
    pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetTouchEmulationEnabled where
    parseJSON = A.withObject "PEmulationSetTouchEmulationEnabled" $ \v ->
         PEmulationSetTouchEmulationEnabled <$> v .:  "enabled"
            <*> v  .:?  "maxTouchPoints"


instance ToJSON PEmulationSetTouchEmulationEnabled  where
    toJSON v = A.object
        [ "enabled" .= pEmulationSetTouchEmulationEnabledEnabled v
        , "maxTouchPoints" .= pEmulationSetTouchEmulationEnabledMaxTouchPoints v
        ]


emulationSetTouchEmulationEnabled :: Session -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled session params = sendReceiveCommand session "Emulation.setTouchEmulationEnabled" (Just params)



data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
    pEmulationSetUserAgentOverrideUserAgent :: String,
    pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pEmulationSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PEmulationSetUserAgentOverride where
    parseJSON = A.withObject "PEmulationSetUserAgentOverride" $ \v ->
         PEmulationSetUserAgentOverride <$> v .:  "userAgent"
            <*> v  .:?  "acceptLanguage"
            <*> v  .:?  "platform"


instance ToJSON PEmulationSetUserAgentOverride  where
    toJSON v = A.object
        [ "userAgent" .= pEmulationSetUserAgentOverrideUserAgent v
        , "acceptLanguage" .= pEmulationSetUserAgentOverrideAcceptLanguage v
        , "platform" .= pEmulationSetUserAgentOverridePlatform v
        ]


emulationSetUserAgentOverride :: Session -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride session params = sendReceiveCommand session "Emulation.setUserAgentOverride" (Just params)

