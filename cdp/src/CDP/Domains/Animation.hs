{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Text (Text(..))
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


-- | Type 'Animation.Animation'.
--   Animation instance.
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
  -- | `Animation`'s id.
  animationAnimationId :: Text,
  -- | `Animation`'s name.
  animationAnimationName :: Text,
  -- | `Animation`'s internal paused state.
  animationAnimationPausedState :: Bool,
  -- | `Animation`'s play state.
  animationAnimationPlayState :: Text,
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
  animationAnimationCssId :: Maybe Text
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
  animationAnimationEffectDirection :: Text,
  -- | `AnimationEffect`'s fill mode.
  animationAnimationEffectFill :: Text,
  -- | `AnimationEffect`'s target node.
  animationAnimationEffectBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  -- | `AnimationEffect`'s keyframes.
  animationAnimationEffectKeyframesRule :: Maybe AnimationKeyframesRule,
  -- | `AnimationEffect`'s timing function.
  animationAnimationEffectEasing :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationEffect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationEffect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Animation.KeyframesRule'.
--   Keyframes Rule
data AnimationKeyframesRule = AnimationKeyframesRule {
  -- | CSS keyframed animation's name.
  animationKeyframesRuleName :: Maybe Text,
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
  animationKeyframeStyleOffset :: Text,
  -- | `AnimationEffect`'s timing function.
  animationKeyframeStyleEasing :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframeStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframeStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





-- | Type of the 'Animation.animationCanceled' event.
data AnimationAnimationCanceled = AnimationAnimationCanceled {
  -- | Id of the animation that was cancelled.
  animationAnimationCanceledId :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCanceled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCanceled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Animation.animationCreated' event.
data AnimationAnimationCreated = AnimationAnimationCreated {
  -- | Id of the animation that was created.
  animationAnimationCreatedId :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Animation.animationStarted' event.
data AnimationAnimationStarted = AnimationAnimationStarted {
  -- | Animation that was started.
  animationAnimationStartedAnimation :: AnimationAnimation
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }





-- | Function for the 'Animation.disable' command.
--   Disables animation domain notifications.
animationDisable :: Handle ev -> IO ()
animationDisable handle = sendReceiveCommand handle "Animation.disable" (Nothing :: Maybe ())


-- | Function for the 'Animation.enable' command.
--   Enables animation domain notifications.
animationEnable :: Handle ev -> IO ()
animationEnable handle = sendReceiveCommand handle "Animation.enable" (Nothing :: Maybe ())


-- | Parameters of the 'animationGetCurrentTime' command.
data PAnimationGetCurrentTime = PAnimationGetCurrentTime {
  -- | Id of animation.
  pAnimationGetCurrentTimeId :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationGetCurrentTime  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Animation.getCurrentTime' command.
--   Returns the current time of the an animation.
--   Parameters: 'PAnimationGetCurrentTime'
--   Returns: 'AnimationGetCurrentTime'
animationGetCurrentTime :: Handle ev -> PAnimationGetCurrentTime -> IO AnimationGetCurrentTime
animationGetCurrentTime handle params = sendReceiveCommandResult handle "Animation.getCurrentTime" (Just params)

-- | Return type of the 'animationGetCurrentTime' command.
data AnimationGetCurrentTime = AnimationGetCurrentTime {
  -- | Current time of the page.
  animationGetCurrentTimeCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command AnimationGetCurrentTime where
   commandName _ = "Animation.getCurrentTime"



-- | Function for the 'Animation.getPlaybackRate' command.
--   Gets the playback rate of the document timeline.
--   Returns: 'AnimationGetPlaybackRate'
animationGetPlaybackRate :: Handle ev -> IO AnimationGetPlaybackRate
animationGetPlaybackRate handle = sendReceiveCommandResult handle "Animation.getPlaybackRate" (Nothing :: Maybe ())

-- | Return type of the 'animationGetPlaybackRate' command.
data AnimationGetPlaybackRate = AnimationGetPlaybackRate {
  -- | Playback rate for animations on page.
  animationGetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AnimationGetPlaybackRate where
   commandName _ = "Animation.getPlaybackRate"



-- | Parameters of the 'animationReleaseAnimations' command.
data PAnimationReleaseAnimations = PAnimationReleaseAnimations {
  -- | List of animation ids to seek.
  pAnimationReleaseAnimationsAnimations :: [Text]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationReleaseAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAnimationReleaseAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Animation.releaseAnimations' command.
--   Releases a set of animations to no longer be manipulated.
--   Parameters: 'PAnimationReleaseAnimations'
animationReleaseAnimations :: Handle ev -> PAnimationReleaseAnimations -> IO ()
animationReleaseAnimations handle params = sendReceiveCommand handle "Animation.releaseAnimations" (Just params)


-- | Parameters of the 'animationResolveAnimation' command.
data PAnimationResolveAnimation = PAnimationResolveAnimation {
  -- | Animation id.
  pAnimationResolveAnimationAnimationId :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationResolveAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PAnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Animation.resolveAnimation' command.
--   Gets the remote object of the Animation.
--   Parameters: 'PAnimationResolveAnimation'
--   Returns: 'AnimationResolveAnimation'
animationResolveAnimation :: Handle ev -> PAnimationResolveAnimation -> IO AnimationResolveAnimation
animationResolveAnimation handle params = sendReceiveCommandResult handle "Animation.resolveAnimation" (Just params)

-- | Return type of the 'animationResolveAnimation' command.
data AnimationResolveAnimation = AnimationResolveAnimation {
  -- | Corresponding remote object.
  animationResolveAnimationRemoteObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command AnimationResolveAnimation where
   commandName _ = "Animation.resolveAnimation"



-- | Parameters of the 'animationSeekAnimations' command.
data PAnimationSeekAnimations = PAnimationSeekAnimations {
  -- | List of animation ids to seek.
  pAnimationSeekAnimationsAnimations :: [Text],
  -- | Set the current time of each animation.
  pAnimationSeekAnimationsCurrentTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSeekAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationSeekAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Animation.seekAnimations' command.
--   Seek a set of animations to a particular time within each animation.
--   Parameters: 'PAnimationSeekAnimations'
animationSeekAnimations :: Handle ev -> PAnimationSeekAnimations -> IO ()
animationSeekAnimations handle params = sendReceiveCommand handle "Animation.seekAnimations" (Just params)


-- | Parameters of the 'animationSetPaused' command.
data PAnimationSetPaused = PAnimationSetPaused {
  -- | Animations to set the pause state of.
  pAnimationSetPausedAnimations :: [Text],
  -- | Paused state to set to.
  pAnimationSetPausedPaused :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'Animation.setPaused' command.
--   Sets the paused state of a set of animations.
--   Parameters: 'PAnimationSetPaused'
animationSetPaused :: Handle ev -> PAnimationSetPaused -> IO ()
animationSetPaused handle params = sendReceiveCommand handle "Animation.setPaused" (Just params)


-- | Parameters of the 'animationSetPlaybackRate' command.
data PAnimationSetPlaybackRate = PAnimationSetPlaybackRate {
  -- | Playback rate for animations on page
  pAnimationSetPlaybackRatePlaybackRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPlaybackRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Animation.setPlaybackRate' command.
--   Sets the playback rate of the document timeline.
--   Parameters: 'PAnimationSetPlaybackRate'
animationSetPlaybackRate :: Handle ev -> PAnimationSetPlaybackRate -> IO ()
animationSetPlaybackRate handle params = sendReceiveCommand handle "Animation.setPlaybackRate" (Just params)


-- | Parameters of the 'animationSetTiming' command.
data PAnimationSetTiming = PAnimationSetTiming {
  -- | Animation id.
  pAnimationSetTimingAnimationId :: Text,
  -- | Duration of the animation.
  pAnimationSetTimingDuration :: Double,
  -- | Delay of the animation.
  pAnimationSetTimingDelay :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'Animation.setTiming' command.
--   Sets the timing of an animation node.
--   Parameters: 'PAnimationSetTiming'
animationSetTiming :: Handle ev -> PAnimationSetTiming -> IO ()
animationSetTiming handle params = sendReceiveCommand handle "Animation.setTiming" (Just params)



