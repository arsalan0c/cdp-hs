{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Emulation (module Domains.Emulation) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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

import qualified Domains.DOM as DOM
import qualified Domains.Page as Page
import qualified Domains.Runtime as Runtime


import Utils


data ScreenOrientation = ScreenOrientation {
    screenOrientationType :: String,
    screenOrientationAngle :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ScreenOrientation where
    parseJSON = A.withObject "ScreenOrientation" $ \v ->
         ScreenOrientation <$> v .:  "type"
            <*> v  .:  "angle"


instance ToJSON ScreenOrientation  where
    toJSON v = A.object
        [ "type" .= screenOrientationType v
        , "angle" .= screenOrientationAngle v
        ]



data DisplayFeature = DisplayFeature {
    displayFeatureOrientation :: String,
    displayFeatureOffset :: Int,
    displayFeatureMaskLength :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  DisplayFeature where
    parseJSON = A.withObject "DisplayFeature" $ \v ->
         DisplayFeature <$> v .:  "orientation"
            <*> v  .:  "offset"
            <*> v  .:  "maskLength"


instance ToJSON DisplayFeature  where
    toJSON v = A.object
        [ "orientation" .= displayFeatureOrientation v
        , "offset" .= displayFeatureOffset v
        , "maskLength" .= displayFeatureMaskLength v
        ]



data MediaFeature = MediaFeature {
    mediaFeatureName :: String,
    mediaFeatureValue :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  MediaFeature where
    parseJSON = A.withObject "MediaFeature" $ \v ->
         MediaFeature <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON MediaFeature  where
    toJSON v = A.object
        [ "name" .= mediaFeatureName v
        , "value" .= mediaFeatureValue v
        ]


data CanEmulate = CanEmulate {
    canEmulateResult :: Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CanEmulate where
    parseJSON = A.withObject "CanEmulate" $ \v ->
         CanEmulate <$> v .:  "result"



canEmulate :: Session a -> IO (Either Error CanEmulate)
canEmulate session  = sendReceiveCommandResult (conn session) ("Emulation","canEmulate") ([] ++ (catMaybes []))


clearDeviceMetricsOverride :: Session a -> IO (Maybe Error)
clearDeviceMetricsOverride session  = sendReceiveCommand (conn session) ("Emulation","clearDeviceMetricsOverride") ([] ++ (catMaybes []))


clearGeolocationOverride :: Session a -> IO (Maybe Error)
clearGeolocationOverride session  = sendReceiveCommand (conn session) ("Emulation","clearGeolocationOverride") ([] ++ (catMaybes []))


setDefaultBackgroundColorOverride :: Session a -> Maybe DOM.RGBA -> IO (Maybe Error)
setDefaultBackgroundColorOverride session setDefaultBackgroundColorOverrideColor = sendReceiveCommand (conn session) ("Emulation","setDefaultBackgroundColorOverride") ([] ++ (catMaybes [fmap (("color",) . ToJSONEx) setDefaultBackgroundColorOverrideColor]))


setDeviceMetricsOverride :: Session a -> Int -> Int -> Int -> Bool -> Maybe ScreenOrientation -> IO (Maybe Error)
setDeviceMetricsOverride session setDeviceMetricsOverrideWidth setDeviceMetricsOverrideHeight setDeviceMetricsOverrideDeviceScaleFactor setDeviceMetricsOverrideMobile setDeviceMetricsOverrideScreenOrientation = sendReceiveCommand (conn session) ("Emulation","setDeviceMetricsOverride") ([("width", ToJSONEx setDeviceMetricsOverrideWidth), ("height", ToJSONEx setDeviceMetricsOverrideHeight), ("deviceScaleFactor", ToJSONEx setDeviceMetricsOverrideDeviceScaleFactor), ("mobile", ToJSONEx setDeviceMetricsOverrideMobile)] ++ (catMaybes [fmap (("screenOrientation",) . ToJSONEx) setDeviceMetricsOverrideScreenOrientation]))


setEmulatedMedia :: Session a -> Maybe String -> Maybe [MediaFeature] -> IO (Maybe Error)
setEmulatedMedia session setEmulatedMediaMedia setEmulatedMediaFeatures = sendReceiveCommand (conn session) ("Emulation","setEmulatedMedia") ([] ++ (catMaybes [fmap (("media",) . ToJSONEx) setEmulatedMediaMedia, fmap (("features",) . ToJSONEx) setEmulatedMediaFeatures]))


setGeolocationOverride :: Session a -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Maybe Error)
setGeolocationOverride session setGeolocationOverrideLatitude setGeolocationOverrideLongitude setGeolocationOverrideAccuracy = sendReceiveCommand (conn session) ("Emulation","setGeolocationOverride") ([] ++ (catMaybes [fmap (("latitude",) . ToJSONEx) setGeolocationOverrideLatitude, fmap (("longitude",) . ToJSONEx) setGeolocationOverrideLongitude, fmap (("accuracy",) . ToJSONEx) setGeolocationOverrideAccuracy]))


setScriptExecutionDisabled :: Session a -> Bool -> IO (Maybe Error)
setScriptExecutionDisabled session setScriptExecutionDisabledValue = sendReceiveCommand (conn session) ("Emulation","setScriptExecutionDisabled") ([("value", ToJSONEx setScriptExecutionDisabledValue)] ++ (catMaybes []))


setTouchEmulationEnabled :: Session a -> Bool -> Maybe Int -> IO (Maybe Error)
setTouchEmulationEnabled session setTouchEmulationEnabledEnabled setTouchEmulationEnabledMaxTouchPoints = sendReceiveCommand (conn session) ("Emulation","setTouchEmulationEnabled") ([("enabled", ToJSONEx setTouchEmulationEnabledEnabled)] ++ (catMaybes [fmap (("maxTouchPoints",) . ToJSONEx) setTouchEmulationEnabledMaxTouchPoints]))


setUserAgentOverride :: Session a -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
setUserAgentOverride session setUserAgentOverrideUserAgent setUserAgentOverrideAcceptLanguage setUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Emulation","setUserAgentOverride") ([("userAgent", ToJSONEx setUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("acceptLanguage",) . ToJSONEx) setUserAgentOverrideAcceptLanguage, fmap (("platform",) . ToJSONEx) setUserAgentOverridePlatform]))


