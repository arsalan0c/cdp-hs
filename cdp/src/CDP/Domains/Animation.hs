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


-- | Animation instance.
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
   animationAnimationId :: AnimationAnimationId, -- ^ `Animation`'s id.
   animationAnimationName :: AnimationAnimationName, -- ^ `Animation`'s name.
   animationAnimationPausedState :: AnimationAnimationPausedState, -- ^ `Animation`'s internal paused state.
   animationAnimationPlayState :: AnimationAnimationPlayState, -- ^ `Animation`'s play state.
   animationAnimationPlaybackRate :: AnimationAnimationPlaybackRate, -- ^ `Animation`'s playback rate.
   animationAnimationStartTime :: AnimationAnimationStartTime, -- ^ `Animation`'s start time.
   animationAnimationCurrentTime :: AnimationAnimationCurrentTime, -- ^ `Animation`'s current time.
   animationAnimationType :: AnimationAnimationType, -- ^ Animation type of `Animation`.
   animationAnimationSource :: AnimationAnimationSource, -- ^ `Animation`'s source animation node.
   animationAnimationCssId :: AnimationAnimationCssId -- ^ A unique ID for `Animation` representing the sources that triggered this CSS
animation/transition.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | AnimationEffect instance
data AnimationAnimationEffect = AnimationAnimationEffect {
   animationAnimationEffectDelay :: AnimationAnimationEffectDelay, -- ^ `AnimationEffect`'s delay.
   animationAnimationEffectEndDelay :: AnimationAnimationEffectEndDelay, -- ^ `AnimationEffect`'s end delay.
   animationAnimationEffectIterationStart :: AnimationAnimationEffectIterationStart, -- ^ `AnimationEffect`'s iteration start.
   animationAnimationEffectIterations :: AnimationAnimationEffectIterations, -- ^ `AnimationEffect`'s iterations.
   animationAnimationEffectDuration :: AnimationAnimationEffectDuration, -- ^ `AnimationEffect`'s iteration duration.
   animationAnimationEffectDirection :: AnimationAnimationEffectDirection, -- ^ `AnimationEffect`'s playback direction.
   animationAnimationEffectFill :: AnimationAnimationEffectFill, -- ^ `AnimationEffect`'s fill mode.
   animationAnimationEffectBackendNodeId :: AnimationAnimationEffectBackendNodeId, -- ^ `AnimationEffect`'s target node.
   animationAnimationEffectKeyframesRule :: AnimationAnimationEffectKeyframesRule, -- ^ `AnimationEffect`'s keyframes.
   animationAnimationEffectEasing :: AnimationAnimationEffectEasing -- ^ `AnimationEffect`'s timing function.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationEffect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationEffect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Keyframes Rule
data AnimationKeyframesRule = AnimationKeyframesRule {
   animationKeyframesRuleName :: AnimationKeyframesRuleName, -- ^ CSS keyframed animation's name.
   animationKeyframesRuleKeyframes :: AnimationKeyframesRuleKeyframes -- ^ List of animation keyframes.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Keyframe Style
data AnimationKeyframeStyle = AnimationKeyframeStyle {
   animationKeyframeStyleOffset :: AnimationKeyframeStyleOffset, -- ^ Keyframe's time offset.
   animationKeyframeStyleEasing :: AnimationKeyframeStyleEasing -- ^ `AnimationEffect`'s timing function.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationKeyframeStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AnimationKeyframeStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }





-- | Type of the 'Animation.animationCanceled' event.
data AnimationAnimationCanceled = AnimationAnimationCanceled {
   animationAnimationCanceledId :: AnimationAnimationCanceledId -- ^ Id of the animation that was cancelled.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCanceled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCanceled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Animation.animationCreated' event.
data AnimationAnimationCreated = AnimationAnimationCreated {
   animationAnimationCreatedId :: AnimationAnimationCreatedId -- ^ Id of the animation that was created.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Animation.animationStarted' event.
data AnimationAnimationStarted = AnimationAnimationStarted {
   animationAnimationStartedAnimation :: AnimationAnimationStartedAnimation -- ^ Animation that was started.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AnimationAnimationStarted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AnimationAnimationStarted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }





-- | Function for the command 'Animation.disable'.
-- Disables animation domain notifications.
animationDisable :: Handle ev -> IO (Maybe Error)
animationDisable handle = sendReceiveCommand handle "Animation.disable" (Nothing :: Maybe ())


-- | Function for the command 'Animation.enable'.
-- Enables animation domain notifications.
animationEnable :: Handle ev -> IO (Maybe Error)
animationEnable handle = sendReceiveCommand handle "Animation.enable" (Nothing :: Maybe ())


-- | Parameters of the 'animationGetCurrentTime' command.
data PAnimationGetCurrentTime = PAnimationGetCurrentTime {
   pAnimationGetCurrentTimeId :: PAnimationGetCurrentTimeId -- ^ Id of animation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationGetCurrentTime  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Animation.getCurrentTime'.
-- Returns the current time of the an animation.
-- Parameters: 'PAnimationGetCurrentTime'
-- Returns: 'AnimationGetCurrentTime'
animationGetCurrentTime :: Handle ev -> PAnimationGetCurrentTime -> IO (Either Error AnimationGetCurrentTime)
animationGetCurrentTime handle params = sendReceiveCommandResult handle "Animation.getCurrentTime" (Just params)

-- | Return type of the 'animationGetCurrentTime' command.
data AnimationGetCurrentTime = AnimationGetCurrentTime {
   animationGetCurrentTimeCurrentTime :: Double -- ^ Current time of the page.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetCurrentTime where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command AnimationGetCurrentTime where
   commandName _ = "Animation.getCurrentTime"



-- | Function for the command 'Animation.getPlaybackRate'.
-- Gets the playback rate of the document timeline.
-- Returns: 'AnimationGetPlaybackRate'
animationGetPlaybackRate :: Handle ev -> IO (Either Error AnimationGetPlaybackRate)
animationGetPlaybackRate handle = sendReceiveCommandResult handle "Animation.getPlaybackRate" (Nothing :: Maybe ())

-- | Return type of the 'animationGetPlaybackRate' command.
data AnimationGetPlaybackRate = AnimationGetPlaybackRate {
   animationGetPlaybackRatePlaybackRate :: Double -- ^ Playback rate for animations on page.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationGetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AnimationGetPlaybackRate where
   commandName _ = "Animation.getPlaybackRate"



-- | Parameters of the 'animationReleaseAnimations' command.
data PAnimationReleaseAnimations = PAnimationReleaseAnimations {
   pAnimationReleaseAnimationsAnimations :: PAnimationReleaseAnimationsAnimations -- ^ List of animation ids to seek.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationReleaseAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAnimationReleaseAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Animation.releaseAnimations'.
-- Releases a set of animations to no longer be manipulated.
-- Parameters: 'PAnimationReleaseAnimations'
animationReleaseAnimations :: Handle ev -> PAnimationReleaseAnimations -> IO (Maybe Error)
animationReleaseAnimations handle params = sendReceiveCommand handle "Animation.releaseAnimations" (Just params)


-- | Parameters of the 'animationResolveAnimation' command.
data PAnimationResolveAnimation = PAnimationResolveAnimation {
   pAnimationResolveAnimationAnimationId :: PAnimationResolveAnimationAnimationId -- ^ Animation id.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationResolveAnimation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PAnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Animation.resolveAnimation'.
-- Gets the remote object of the Animation.
-- Parameters: 'PAnimationResolveAnimation'
-- Returns: 'AnimationResolveAnimation'
animationResolveAnimation :: Handle ev -> PAnimationResolveAnimation -> IO (Either Error AnimationResolveAnimation)
animationResolveAnimation handle params = sendReceiveCommandResult handle "Animation.resolveAnimation" (Just params)

-- | Return type of the 'animationResolveAnimation' command.
data AnimationResolveAnimation = AnimationResolveAnimation {
   animationResolveAnimationRemoteObject :: Runtime.RuntimeRemoteObject -- ^ Corresponding remote object.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AnimationResolveAnimation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command AnimationResolveAnimation where
   commandName _ = "Animation.resolveAnimation"



-- | Parameters of the 'animationSeekAnimations' command.
data PAnimationSeekAnimations = PAnimationSeekAnimations {
   pAnimationSeekAnimationsAnimations :: PAnimationSeekAnimationsAnimations, -- ^ List of animation ids to seek.
   pAnimationSeekAnimationsCurrentTime :: PAnimationSeekAnimationsCurrentTime -- ^ Set the current time of each animation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSeekAnimations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PAnimationSeekAnimations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Animation.seekAnimations'.
-- Seek a set of animations to a particular time within each animation.
-- Parameters: 'PAnimationSeekAnimations'
animationSeekAnimations :: Handle ev -> PAnimationSeekAnimations -> IO (Maybe Error)
animationSeekAnimations handle params = sendReceiveCommand handle "Animation.seekAnimations" (Just params)


-- | Parameters of the 'animationSetPaused' command.
data PAnimationSetPaused = PAnimationSetPaused {
   pAnimationSetPausedAnimations :: PAnimationSetPausedAnimations, -- ^ Animations to set the pause state of.
   pAnimationSetPausedPaused :: PAnimationSetPausedPaused -- ^ Paused state to set to.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'Animation.setPaused'.
-- Sets the paused state of a set of animations.
-- Parameters: 'PAnimationSetPaused'
animationSetPaused :: Handle ev -> PAnimationSetPaused -> IO (Maybe Error)
animationSetPaused handle params = sendReceiveCommand handle "Animation.setPaused" (Just params)


-- | Parameters of the 'animationSetPlaybackRate' command.
data PAnimationSetPlaybackRate = PAnimationSetPlaybackRate {
   pAnimationSetPlaybackRatePlaybackRate :: PAnimationSetPlaybackRatePlaybackRate -- ^ Playback rate for animations on page
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetPlaybackRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetPlaybackRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Animation.setPlaybackRate'.
-- Sets the playback rate of the document timeline.
-- Parameters: 'PAnimationSetPlaybackRate'
animationSetPlaybackRate :: Handle ev -> PAnimationSetPlaybackRate -> IO (Maybe Error)
animationSetPlaybackRate handle params = sendReceiveCommand handle "Animation.setPlaybackRate" (Just params)


-- | Parameters of the 'animationSetTiming' command.
data PAnimationSetTiming = PAnimationSetTiming {
   pAnimationSetTimingAnimationId :: PAnimationSetTimingAnimationId, -- ^ Animation id.
   pAnimationSetTimingDuration :: PAnimationSetTimingDuration, -- ^ Duration of the animation.
   pAnimationSetTimingDelay :: PAnimationSetTimingDelay -- ^ Delay of the animation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAnimationSetTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PAnimationSetTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'Animation.setTiming'.
-- Sets the timing of an animation node.
-- Parameters: 'PAnimationSetTiming'
animationSetTiming :: Handle ev -> PAnimationSetTiming -> IO (Maybe Error)
animationSetTiming handle params = sendReceiveCommand handle "Animation.setTiming" (Just params)



