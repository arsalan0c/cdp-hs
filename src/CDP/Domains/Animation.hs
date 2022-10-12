{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Animation 
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'Animation.Animation'.
--   Animation instance.
data AnimationAnimationType = AnimationAnimationTypeCSSTransition | AnimationAnimationTypeCSSAnimation | AnimationAnimationTypeWebAnimation
   deriving (Ord, Eq, Show, Read)
instance FromJSON AnimationAnimationType where
   parseJSON = A.withText  "AnimationAnimationType"  $ \v -> do
      case v of
         "CSSTransition" -> pure AnimationAnimationTypeCSSTransition
         "CSSAnimation" -> pure AnimationAnimationTypeCSSAnimation
         "WebAnimation" -> pure AnimationAnimationTypeWebAnimation
         _ -> fail "failed to parse AnimationAnimationType"

instance ToJSON AnimationAnimationType where
   toJSON v = A.String $
      case v of
         AnimationAnimationTypeCSSTransition -> "CSSTransition"
         AnimationAnimationTypeCSSAnimation -> "CSSAnimation"
         AnimationAnimationTypeWebAnimation -> "WebAnimation"



data AnimationAnimation = AnimationAnimation {
  -- | `Animation`'s id.
  animationAnimationId :: String,
  -- | `Animation`'s name.
  animationAnimationName :: String,
  -- | `Animation`'s internal paused state.
  animationAnimationPausedState :: Bool,
  -- | `Animation`'s play state.
  animationAnimationPlayState :: String,
  -- | `Animation`'s playback rate.
  animationAnimationPlaybackRate :: Double,
  -- | `Animation`'s start time.
  animationAnimationStartTime :: Double,
  -- | `Animation`'s current time.
  animationAnimationCurrentTime :: Double,
  -- | Animation type of `Animation`.
  animationAnimationType :: AnimationAnimationType,
  -- | `Animation`'s source animation node.
  animationAnimationSource :: Maybe AnimationAnimationEffect,
  -- | A unique ID for `Animation` representing the sources that triggered this CSS
  --   animation/transition.
  animationAnimationCssId :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Animation.AnimationEffect'.
--   AnimationEffect instance
data AnimationAnimationEffect = AnimationAnimationEffect {
  -- | `AnimationEffect`'s delay.
  animationAnimationEffectDelay :: Double,
  -- | `AnimationEffect`'s end delay.
  animationAnimationEffectEndDelay :: Double,
  -- | `AnimationEffect`'s iteration start.
  animationAnimationEffectIterationStart :: Double,
  -- | `AnimationEffect`'s iterations.
  animationAnimationEffectIterations :: Double,
  -- | `AnimationEffect`'s iteration duration.
  animationAnimationEffectDuration :: Double,
  -- | `AnimationEffect`'s playback direction.
  animationAnimationEffectDirection :: String,
  -- | `AnimationEffect`'s fill mode.
  animationAnimationEffectFill :: String,
  -- | `AnimationEffect`'s target node.
  animationAnimationEffectBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | `AnimationEffect`'s keyframes.
  animationAnimationEffectKeyframesRule :: Maybe AnimationKeyframesRule,
  -- | `AnimationEffect`'s timing function.
  animationAnimationEffectEasing :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationEffect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationEffect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Animation.KeyframesRule'.
--   Keyframes Rule
data AnimationKeyframesRule = AnimationKeyframesRule {
  -- | CSS keyframed animation's name.
  animationKeyframesRuleName :: Maybe String,
  -- | List of animation keyframes.
  animationKeyframesRuleKeyframes :: [AnimationKeyframeStyle]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Animation.KeyframeStyle'.
--   Keyframe Style
data AnimationKeyframeStyle = AnimationKeyframeStyle {
  -- | Keyframe's time offset.
  animationKeyframeStyleOffset :: String,
  -- | `AnimationEffect`'s timing function.
  animationKeyframeStyleEasing :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframeStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframeStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





-- | Type of the 'Animation.animationCanceled' event.
data AnimationAnimationCanceled = AnimationAnimationCanceled {
  -- | Id of the animation that was cancelled.
  animationAnimationCanceledId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCanceled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCanceled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Event AnimationAnimationCanceled where
    eventName _ = "Animation.animationCanceled"

-- | Type of the 'Animation.animationCreated' event.
data AnimationAnimationCreated = AnimationAnimationCreated {
  -- | Id of the animation that was created.
  animationAnimationCreatedId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event AnimationAnimationCreated where
    eventName _ = "Animation.animationCreated"

-- | Type of the 'Animation.animationStarted' event.
data AnimationAnimationStarted = AnimationAnimationStarted {
  -- | Animation that was started.
  animationAnimationStartedAnimation :: AnimationAnimation
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event AnimationAnimationStarted where
    eventName _ = "Animation.animationStarted"



-- | Animation.disable
--   Disables animation domain notifications.

-- | Parameters of the 'Animation.disable' command.
data PAnimationDisable = PAnimationDisable
instance ToJSON PAnimationDisable where toJSON _ = A.Null

instance Command PAnimationDisable where
   type CommandResponse PAnimationDisable = ()
   commandName _ = "Animation.disable"
   fromJSON = const . A.Success . const ()


-- | Animation.enable
--   Enables animation domain notifications.

-- | Parameters of the 'Animation.enable' command.
data PAnimationEnable = PAnimationEnable
instance ToJSON PAnimationEnable where toJSON _ = A.Null

instance Command PAnimationEnable where
   type CommandResponse PAnimationEnable = ()
   commandName _ = "Animation.enable"
   fromJSON = const . A.Success . const ()


-- | Animation.getCurrentTime
--   Returns the current time of the an animation.

-- | Parameters of the 'Animation.getCurrentTime' command.
data PAnimationGetCurrentTime = PAnimationGetCurrentTime {
  -- | Id of animation.
  pAnimationGetCurrentTimeId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationGetCurrentTime  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Animation.getCurrentTime' command.
data AnimationGetCurrentTime = AnimationGetCurrentTime {
  -- | Current time of the page.
  animationGetCurrentTimeCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PAnimationGetCurrentTime where
   type CommandResponse PAnimationGetCurrentTime = AnimationGetCurrentTime
   commandName _ = "Animation.getCurrentTime"



-- | Animation.getPlaybackRate
--   Gets the playback rate of the document timeline.

-- | Parameters of the 'Animation.getPlaybackRate' command.
data PAnimationGetPlaybackRate = PAnimationGetPlaybackRate
instance ToJSON PAnimationGetPlaybackRate where toJSON _ = A.Null

-- | Return type of the 'Animation.getPlaybackRate' command.
data AnimationGetPlaybackRate = AnimationGetPlaybackRate {
  -- | Playback rate for animations on page.
  animationGetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PAnimationGetPlaybackRate where
   type CommandResponse PAnimationGetPlaybackRate = AnimationGetPlaybackRate
   commandName _ = "Animation.getPlaybackRate"



-- | Animation.releaseAnimations
--   Releases a set of animations to no longer be manipulated.

-- | Parameters of the 'Animation.releaseAnimations' command.
data PAnimationReleaseAnimations = PAnimationReleaseAnimations {
  -- | List of animation ids to seek.
  pAnimationReleaseAnimationsAnimations :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationReleaseAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAnimationReleaseAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PAnimationReleaseAnimations where
   type CommandResponse PAnimationReleaseAnimations = ()
   commandName _ = "Animation.releaseAnimations"
   fromJSON = const . A.Success . const ()


-- | Animation.resolveAnimation
--   Gets the remote object of the Animation.

-- | Parameters of the 'Animation.resolveAnimation' command.
data PAnimationResolveAnimation = PAnimationResolveAnimation {
  -- | Animation id.
  pAnimationResolveAnimationAnimationId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationResolveAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PAnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Return type of the 'Animation.resolveAnimation' command.
data AnimationResolveAnimation = AnimationResolveAnimation {
  -- | Corresponding remote object.
  animationResolveAnimationRemoteObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command PAnimationResolveAnimation where
   type CommandResponse PAnimationResolveAnimation = AnimationResolveAnimation
   commandName _ = "Animation.resolveAnimation"



-- | Animation.seekAnimations
--   Seek a set of animations to a particular time within each animation.

-- | Parameters of the 'Animation.seekAnimations' command.
data PAnimationSeekAnimations = PAnimationSeekAnimations {
  -- | List of animation ids to seek.
  pAnimationSeekAnimationsAnimations :: [String],
  -- | Set the current time of each animation.
  pAnimationSeekAnimationsCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSeekAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationSeekAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PAnimationSeekAnimations where
   type CommandResponse PAnimationSeekAnimations = ()
   commandName _ = "Animation.seekAnimations"
   fromJSON = const . A.Success . const ()


-- | Animation.setPaused
--   Sets the paused state of a set of animations.

-- | Parameters of the 'Animation.setPaused' command.
data PAnimationSetPaused = PAnimationSetPaused {
  -- | Animations to set the pause state of.
  pAnimationSetPausedAnimations :: [String],
  -- | Paused state to set to.
  pAnimationSetPausedPaused :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command PAnimationSetPaused where
   type CommandResponse PAnimationSetPaused = ()
   commandName _ = "Animation.setPaused"
   fromJSON = const . A.Success . const ()


-- | Animation.setPlaybackRate
--   Sets the playback rate of the document timeline.

-- | Parameters of the 'Animation.setPlaybackRate' command.
data PAnimationSetPlaybackRate = PAnimationSetPlaybackRate {
  -- | Playback rate for animations on page
  pAnimationSetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPlaybackRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PAnimationSetPlaybackRate where
   type CommandResponse PAnimationSetPlaybackRate = ()
   commandName _ = "Animation.setPlaybackRate"
   fromJSON = const . A.Success . const ()


-- | Animation.setTiming
--   Sets the timing of an animation node.

-- | Parameters of the 'Animation.setTiming' command.
data PAnimationSetTiming = PAnimationSetTiming {
  -- | Animation id.
  pAnimationSetTimingAnimationId :: String,
  -- | Duration of the animation.
  pAnimationSetTimingDuration :: Double,
  -- | Delay of the animation.
  pAnimationSetTimingDelay :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command PAnimationSetTiming where
   type CommandResponse PAnimationSetTiming = ()
   commandName _ = "Animation.setTiming"
   fromJSON = const . A.Success . const ()



