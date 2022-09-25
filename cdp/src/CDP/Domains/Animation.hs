{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Animation (module CDP.Domains.Animation) where

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


data AnimationAnimationType = AnimationAnimationTypeCssTransition | AnimationAnimationTypeCssAnimation | AnimationAnimationTypeWebAnimation
   deriving (Ord, Eq, Show, Read)
instance FromJSON AnimationAnimationType where
   parseJSON = A.withText  "AnimationAnimationType"  $ \v -> do
      case v of
         "CSSTransition" -> pure AnimationAnimationTypeCssTransition
         "CSSAnimation" -> pure AnimationAnimationTypeCssAnimation
         "WebAnimation" -> pure AnimationAnimationTypeWebAnimation
         _ -> fail "failed to parse AnimationAnimationType"

instance ToJSON AnimationAnimationType where
   toJSON v = A.String $
      case v of
         AnimationAnimationTypeCssTransition -> "CSSTransition"
         AnimationAnimationTypeCssAnimation -> "CSSAnimation"
         AnimationAnimationTypeWebAnimation -> "WebAnimation"



data AnimationAnimation = AnimationAnimation {
   animationAnimationId :: String,
   animationAnimationName :: String,
   animationAnimationPausedState :: Bool,
   animationAnimationPlayState :: String,
   animationAnimationPlaybackRate :: Double,
   animationAnimationStartTime :: Double,
   animationAnimationCurrentTime :: Double,
   animationAnimationType :: AnimationAnimationType,
   animationAnimationSource :: Maybe AnimationAnimationEffect,
   animationAnimationCssId :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data AnimationAnimationEffect = AnimationAnimationEffect {
   animationAnimationEffectDelay :: Double,
   animationAnimationEffectEndDelay :: Double,
   animationAnimationEffectIterationStart :: Double,
   animationAnimationEffectIterations :: Double,
   animationAnimationEffectDuration :: Double,
   animationAnimationEffectDirection :: String,
   animationAnimationEffectFill :: String,
   animationAnimationEffectBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   animationAnimationEffectKeyframesRule :: Maybe AnimationKeyframesRule,
   animationAnimationEffectEasing :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationEffect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationEffect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data AnimationKeyframesRule = AnimationKeyframesRule {
   animationKeyframesRuleName :: Maybe String,
   animationKeyframesRuleKeyframes :: [AnimationKeyframeStyle]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data AnimationKeyframeStyle = AnimationKeyframeStyle {
   animationKeyframeStyleOffset :: String,
   animationKeyframeStyleEasing :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframeStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframeStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





data AnimationAnimationCanceled = AnimationAnimationCanceled {
   animationAnimationCanceledId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCanceled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCanceled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data AnimationAnimationCreated = AnimationAnimationCreated {
   animationAnimationCreatedId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data AnimationAnimationStarted = AnimationAnimationStarted {
   animationAnimationStartedAnimation :: AnimationAnimation
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }




animationDisable :: Handle ev -> IO (Maybe Error)
animationDisable handle = sendReceiveCommand handle "Animation.disable" (Nothing :: Maybe ())


animationEnable :: Handle ev -> IO (Maybe Error)
animationEnable handle = sendReceiveCommand handle "Animation.enable" (Nothing :: Maybe ())



data PAnimationGetCurrentTime = PAnimationGetCurrentTime {
   pAnimationGetCurrentTimeId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationGetCurrentTime  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


animationGetCurrentTime :: Handle ev -> PAnimationGetCurrentTime -> IO (Either Error AnimationGetCurrentTime)
animationGetCurrentTime handle params = sendReceiveCommandResult handle "Animation.getCurrentTime" (Just params)

data AnimationGetCurrentTime = AnimationGetCurrentTime {
   animationGetCurrentTimeCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command AnimationGetCurrentTime where
   commandName _ = "Animation.getCurrentTime"



animationGetPlaybackRate :: Handle ev -> IO (Either Error AnimationGetPlaybackRate)
animationGetPlaybackRate handle = sendReceiveCommandResult handle "Animation.getPlaybackRate" (Nothing :: Maybe ())

data AnimationGetPlaybackRate = AnimationGetPlaybackRate {
   animationGetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AnimationGetPlaybackRate where
   commandName _ = "Animation.getPlaybackRate"




data PAnimationReleaseAnimations = PAnimationReleaseAnimations {
   pAnimationReleaseAnimationsAnimations :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationReleaseAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAnimationReleaseAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


animationReleaseAnimations :: Handle ev -> PAnimationReleaseAnimations -> IO (Maybe Error)
animationReleaseAnimations handle params = sendReceiveCommand handle "Animation.releaseAnimations" (Just params)



data PAnimationResolveAnimation = PAnimationResolveAnimation {
   pAnimationResolveAnimationAnimationId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationResolveAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PAnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


animationResolveAnimation :: Handle ev -> PAnimationResolveAnimation -> IO (Either Error AnimationResolveAnimation)
animationResolveAnimation handle params = sendReceiveCommandResult handle "Animation.resolveAnimation" (Just params)

data AnimationResolveAnimation = AnimationResolveAnimation {
   animationResolveAnimationRemoteObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command AnimationResolveAnimation where
   commandName _ = "Animation.resolveAnimation"




data PAnimationSeekAnimations = PAnimationSeekAnimations {
   pAnimationSeekAnimationsAnimations :: [String],
   pAnimationSeekAnimationsCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSeekAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationSeekAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


animationSeekAnimations :: Handle ev -> PAnimationSeekAnimations -> IO (Maybe Error)
animationSeekAnimations handle params = sendReceiveCommand handle "Animation.seekAnimations" (Just params)



data PAnimationSetPaused = PAnimationSetPaused {
   pAnimationSetPausedAnimations :: [String],
   pAnimationSetPausedPaused :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


animationSetPaused :: Handle ev -> PAnimationSetPaused -> IO (Maybe Error)
animationSetPaused handle params = sendReceiveCommand handle "Animation.setPaused" (Just params)



data PAnimationSetPlaybackRate = PAnimationSetPlaybackRate {
   pAnimationSetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPlaybackRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


animationSetPlaybackRate :: Handle ev -> PAnimationSetPlaybackRate -> IO (Maybe Error)
animationSetPlaybackRate handle params = sendReceiveCommand handle "Animation.setPlaybackRate" (Just params)



data PAnimationSetTiming = PAnimationSetTiming {
   pAnimationSetTimingAnimationId :: String,
   pAnimationSetTimingDuration :: Double,
   pAnimationSetTimingDelay :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


animationSetTiming :: Handle ev -> PAnimationSetTiming -> IO (Maybe Error)
animationSetTiming handle params = sendReceiveCommand handle "Animation.setTiming" (Just params)



