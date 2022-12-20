{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Animation

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
  parseJSON = A.withText "AnimationAnimationType" $ \v -> case v of
    "CSSTransition" -> pure AnimationAnimationTypeCSSTransition
    "CSSAnimation" -> pure AnimationAnimationTypeCSSAnimation
    "WebAnimation" -> pure AnimationAnimationTypeWebAnimation
    "_" -> fail "failed to parse AnimationAnimationType"
instance ToJSON AnimationAnimationType where
  toJSON v = A.String $ case v of
    AnimationAnimationTypeCSSTransition -> "CSSTransition"
    AnimationAnimationTypeCSSAnimation -> "CSSAnimation"
    AnimationAnimationTypeWebAnimation -> "WebAnimation"
data AnimationAnimation = AnimationAnimation
  {
    -- | `Animation`'s id.
    animationAnimationId :: T.Text,
    -- | `Animation`'s name.
    animationAnimationName :: T.Text,
    -- | `Animation`'s internal paused state.
    animationAnimationPausedState :: Bool,
    -- | `Animation`'s play state.
    animationAnimationPlayState :: T.Text,
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
    animationAnimationCssId :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AnimationAnimation where
  parseJSON = A.withObject "AnimationAnimation" $ \o -> AnimationAnimation
    <$> o A..: "id"
    <*> o A..: "name"
    <*> o A..: "pausedState"
    <*> o A..: "playState"
    <*> o A..: "playbackRate"
    <*> o A..: "startTime"
    <*> o A..: "currentTime"
    <*> o A..: "type"
    <*> o A..:? "source"
    <*> o A..:? "cssId"
instance ToJSON AnimationAnimation where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (animationAnimationId p),
    ("name" A..=) <$> Just (animationAnimationName p),
    ("pausedState" A..=) <$> Just (animationAnimationPausedState p),
    ("playState" A..=) <$> Just (animationAnimationPlayState p),
    ("playbackRate" A..=) <$> Just (animationAnimationPlaybackRate p),
    ("startTime" A..=) <$> Just (animationAnimationStartTime p),
    ("currentTime" A..=) <$> Just (animationAnimationCurrentTime p),
    ("type" A..=) <$> Just (animationAnimationType p),
    ("source" A..=) <$> (animationAnimationSource p),
    ("cssId" A..=) <$> (animationAnimationCssId p)
    ]

-- | Type 'Animation.AnimationEffect'.
--   AnimationEffect instance
data AnimationAnimationEffect = AnimationAnimationEffect
  {
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
    animationAnimationEffectDirection :: T.Text,
    -- | `AnimationEffect`'s fill mode.
    animationAnimationEffectFill :: T.Text,
    -- | `AnimationEffect`'s target node.
    animationAnimationEffectBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | `AnimationEffect`'s keyframes.
    animationAnimationEffectKeyframesRule :: Maybe AnimationKeyframesRule,
    -- | `AnimationEffect`'s timing function.
    animationAnimationEffectEasing :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AnimationAnimationEffect where
  parseJSON = A.withObject "AnimationAnimationEffect" $ \o -> AnimationAnimationEffect
    <$> o A..: "delay"
    <*> o A..: "endDelay"
    <*> o A..: "iterationStart"
    <*> o A..: "iterations"
    <*> o A..: "duration"
    <*> o A..: "direction"
    <*> o A..: "fill"
    <*> o A..:? "backendNodeId"
    <*> o A..:? "keyframesRule"
    <*> o A..: "easing"
instance ToJSON AnimationAnimationEffect where
  toJSON p = A.object $ catMaybes [
    ("delay" A..=) <$> Just (animationAnimationEffectDelay p),
    ("endDelay" A..=) <$> Just (animationAnimationEffectEndDelay p),
    ("iterationStart" A..=) <$> Just (animationAnimationEffectIterationStart p),
    ("iterations" A..=) <$> Just (animationAnimationEffectIterations p),
    ("duration" A..=) <$> Just (animationAnimationEffectDuration p),
    ("direction" A..=) <$> Just (animationAnimationEffectDirection p),
    ("fill" A..=) <$> Just (animationAnimationEffectFill p),
    ("backendNodeId" A..=) <$> (animationAnimationEffectBackendNodeId p),
    ("keyframesRule" A..=) <$> (animationAnimationEffectKeyframesRule p),
    ("easing" A..=) <$> Just (animationAnimationEffectEasing p)
    ]

-- | Type 'Animation.KeyframesRule'.
--   Keyframes Rule
data AnimationKeyframesRule = AnimationKeyframesRule
  {
    -- | CSS keyframed animation's name.
    animationKeyframesRuleName :: Maybe T.Text,
    -- | List of animation keyframes.
    animationKeyframesRuleKeyframes :: [AnimationKeyframeStyle]
  }
  deriving (Eq, Show)
instance FromJSON AnimationKeyframesRule where
  parseJSON = A.withObject "AnimationKeyframesRule" $ \o -> AnimationKeyframesRule
    <$> o A..:? "name"
    <*> o A..: "keyframes"
instance ToJSON AnimationKeyframesRule where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> (animationKeyframesRuleName p),
    ("keyframes" A..=) <$> Just (animationKeyframesRuleKeyframes p)
    ]

-- | Type 'Animation.KeyframeStyle'.
--   Keyframe Style
data AnimationKeyframeStyle = AnimationKeyframeStyle
  {
    -- | Keyframe's time offset.
    animationKeyframeStyleOffset :: T.Text,
    -- | `AnimationEffect`'s timing function.
    animationKeyframeStyleEasing :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AnimationKeyframeStyle where
  parseJSON = A.withObject "AnimationKeyframeStyle" $ \o -> AnimationKeyframeStyle
    <$> o A..: "offset"
    <*> o A..: "easing"
instance ToJSON AnimationKeyframeStyle where
  toJSON p = A.object $ catMaybes [
    ("offset" A..=) <$> Just (animationKeyframeStyleOffset p),
    ("easing" A..=) <$> Just (animationKeyframeStyleEasing p)
    ]

-- | Type of the 'Animation.animationCanceled' event.
data AnimationAnimationCanceled = AnimationAnimationCanceled
  {
    -- | Id of the animation that was cancelled.
    animationAnimationCanceledId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AnimationAnimationCanceled where
  parseJSON = A.withObject "AnimationAnimationCanceled" $ \o -> AnimationAnimationCanceled
    <$> o A..: "id"
instance Event AnimationAnimationCanceled where
  eventName _ = "Animation.animationCanceled"

-- | Type of the 'Animation.animationCreated' event.
data AnimationAnimationCreated = AnimationAnimationCreated
  {
    -- | Id of the animation that was created.
    animationAnimationCreatedId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON AnimationAnimationCreated where
  parseJSON = A.withObject "AnimationAnimationCreated" $ \o -> AnimationAnimationCreated
    <$> o A..: "id"
instance Event AnimationAnimationCreated where
  eventName _ = "Animation.animationCreated"

-- | Type of the 'Animation.animationStarted' event.
data AnimationAnimationStarted = AnimationAnimationStarted
  {
    -- | Animation that was started.
    animationAnimationStartedAnimation :: AnimationAnimation
  }
  deriving (Eq, Show)
instance FromJSON AnimationAnimationStarted where
  parseJSON = A.withObject "AnimationAnimationStarted" $ \o -> AnimationAnimationStarted
    <$> o A..: "animation"
instance Event AnimationAnimationStarted where
  eventName _ = "Animation.animationStarted"

-- | Disables animation domain notifications.

-- | Parameters of the 'Animation.disable' command.
data PAnimationDisable = PAnimationDisable
  deriving (Eq, Show)
pAnimationDisable
  :: PAnimationDisable
pAnimationDisable
  = PAnimationDisable
instance ToJSON PAnimationDisable where
  toJSON _ = A.Null
instance Command PAnimationDisable where
  type CommandResponse PAnimationDisable = ()
  commandName _ = "Animation.disable"
  fromJSON = const . A.Success . const ()

-- | Enables animation domain notifications.

-- | Parameters of the 'Animation.enable' command.
data PAnimationEnable = PAnimationEnable
  deriving (Eq, Show)
pAnimationEnable
  :: PAnimationEnable
pAnimationEnable
  = PAnimationEnable
instance ToJSON PAnimationEnable where
  toJSON _ = A.Null
instance Command PAnimationEnable where
  type CommandResponse PAnimationEnable = ()
  commandName _ = "Animation.enable"
  fromJSON = const . A.Success . const ()

-- | Returns the current time of the an animation.

-- | Parameters of the 'Animation.getCurrentTime' command.
data PAnimationGetCurrentTime = PAnimationGetCurrentTime
  {
    -- | Id of animation.
    pAnimationGetCurrentTimeId :: T.Text
  }
  deriving (Eq, Show)
pAnimationGetCurrentTime
  {-
  -- | Id of animation.
  -}
  :: T.Text
  -> PAnimationGetCurrentTime
pAnimationGetCurrentTime
  arg_pAnimationGetCurrentTimeId
  = PAnimationGetCurrentTime
    arg_pAnimationGetCurrentTimeId
instance ToJSON PAnimationGetCurrentTime where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pAnimationGetCurrentTimeId p)
    ]
data AnimationGetCurrentTime = AnimationGetCurrentTime
  {
    -- | Current time of the page.
    animationGetCurrentTimeCurrentTime :: Double
  }
  deriving (Eq, Show)
instance FromJSON AnimationGetCurrentTime where
  parseJSON = A.withObject "AnimationGetCurrentTime" $ \o -> AnimationGetCurrentTime
    <$> o A..: "currentTime"
instance Command PAnimationGetCurrentTime where
  type CommandResponse PAnimationGetCurrentTime = AnimationGetCurrentTime
  commandName _ = "Animation.getCurrentTime"

-- | Gets the playback rate of the document timeline.

-- | Parameters of the 'Animation.getPlaybackRate' command.
data PAnimationGetPlaybackRate = PAnimationGetPlaybackRate
  deriving (Eq, Show)
pAnimationGetPlaybackRate
  :: PAnimationGetPlaybackRate
pAnimationGetPlaybackRate
  = PAnimationGetPlaybackRate
instance ToJSON PAnimationGetPlaybackRate where
  toJSON _ = A.Null
data AnimationGetPlaybackRate = AnimationGetPlaybackRate
  {
    -- | Playback rate for animations on page.
    animationGetPlaybackRatePlaybackRate :: Double
  }
  deriving (Eq, Show)
instance FromJSON AnimationGetPlaybackRate where
  parseJSON = A.withObject "AnimationGetPlaybackRate" $ \o -> AnimationGetPlaybackRate
    <$> o A..: "playbackRate"
instance Command PAnimationGetPlaybackRate where
  type CommandResponse PAnimationGetPlaybackRate = AnimationGetPlaybackRate
  commandName _ = "Animation.getPlaybackRate"

-- | Releases a set of animations to no longer be manipulated.

-- | Parameters of the 'Animation.releaseAnimations' command.
data PAnimationReleaseAnimations = PAnimationReleaseAnimations
  {
    -- | List of animation ids to seek.
    pAnimationReleaseAnimationsAnimations :: [T.Text]
  }
  deriving (Eq, Show)
pAnimationReleaseAnimations
  {-
  -- | List of animation ids to seek.
  -}
  :: [T.Text]
  -> PAnimationReleaseAnimations
pAnimationReleaseAnimations
  arg_pAnimationReleaseAnimationsAnimations
  = PAnimationReleaseAnimations
    arg_pAnimationReleaseAnimationsAnimations
instance ToJSON PAnimationReleaseAnimations where
  toJSON p = A.object $ catMaybes [
    ("animations" A..=) <$> Just (pAnimationReleaseAnimationsAnimations p)
    ]
instance Command PAnimationReleaseAnimations where
  type CommandResponse PAnimationReleaseAnimations = ()
  commandName _ = "Animation.releaseAnimations"
  fromJSON = const . A.Success . const ()

-- | Gets the remote object of the Animation.

-- | Parameters of the 'Animation.resolveAnimation' command.
data PAnimationResolveAnimation = PAnimationResolveAnimation
  {
    -- | Animation id.
    pAnimationResolveAnimationAnimationId :: T.Text
  }
  deriving (Eq, Show)
pAnimationResolveAnimation
  {-
  -- | Animation id.
  -}
  :: T.Text
  -> PAnimationResolveAnimation
pAnimationResolveAnimation
  arg_pAnimationResolveAnimationAnimationId
  = PAnimationResolveAnimation
    arg_pAnimationResolveAnimationAnimationId
instance ToJSON PAnimationResolveAnimation where
  toJSON p = A.object $ catMaybes [
    ("animationId" A..=) <$> Just (pAnimationResolveAnimationAnimationId p)
    ]
data AnimationResolveAnimation = AnimationResolveAnimation
  {
    -- | Corresponding remote object.
    animationResolveAnimationRemoteObject :: Runtime.RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON AnimationResolveAnimation where
  parseJSON = A.withObject "AnimationResolveAnimation" $ \o -> AnimationResolveAnimation
    <$> o A..: "remoteObject"
instance Command PAnimationResolveAnimation where
  type CommandResponse PAnimationResolveAnimation = AnimationResolveAnimation
  commandName _ = "Animation.resolveAnimation"

-- | Seek a set of animations to a particular time within each animation.

-- | Parameters of the 'Animation.seekAnimations' command.
data PAnimationSeekAnimations = PAnimationSeekAnimations
  {
    -- | List of animation ids to seek.
    pAnimationSeekAnimationsAnimations :: [T.Text],
    -- | Set the current time of each animation.
    pAnimationSeekAnimationsCurrentTime :: Double
  }
  deriving (Eq, Show)
pAnimationSeekAnimations
  {-
  -- | List of animation ids to seek.
  -}
  :: [T.Text]
  {-
  -- | Set the current time of each animation.
  -}
  -> Double
  -> PAnimationSeekAnimations
pAnimationSeekAnimations
  arg_pAnimationSeekAnimationsAnimations
  arg_pAnimationSeekAnimationsCurrentTime
  = PAnimationSeekAnimations
    arg_pAnimationSeekAnimationsAnimations
    arg_pAnimationSeekAnimationsCurrentTime
instance ToJSON PAnimationSeekAnimations where
  toJSON p = A.object $ catMaybes [
    ("animations" A..=) <$> Just (pAnimationSeekAnimationsAnimations p),
    ("currentTime" A..=) <$> Just (pAnimationSeekAnimationsCurrentTime p)
    ]
instance Command PAnimationSeekAnimations where
  type CommandResponse PAnimationSeekAnimations = ()
  commandName _ = "Animation.seekAnimations"
  fromJSON = const . A.Success . const ()

-- | Sets the paused state of a set of animations.

-- | Parameters of the 'Animation.setPaused' command.
data PAnimationSetPaused = PAnimationSetPaused
  {
    -- | Animations to set the pause state of.
    pAnimationSetPausedAnimations :: [T.Text],
    -- | Paused state to set to.
    pAnimationSetPausedPaused :: Bool
  }
  deriving (Eq, Show)
pAnimationSetPaused
  {-
  -- | Animations to set the pause state of.
  -}
  :: [T.Text]
  {-
  -- | Paused state to set to.
  -}
  -> Bool
  -> PAnimationSetPaused
pAnimationSetPaused
  arg_pAnimationSetPausedAnimations
  arg_pAnimationSetPausedPaused
  = PAnimationSetPaused
    arg_pAnimationSetPausedAnimations
    arg_pAnimationSetPausedPaused
instance ToJSON PAnimationSetPaused where
  toJSON p = A.object $ catMaybes [
    ("animations" A..=) <$> Just (pAnimationSetPausedAnimations p),
    ("paused" A..=) <$> Just (pAnimationSetPausedPaused p)
    ]
instance Command PAnimationSetPaused where
  type CommandResponse PAnimationSetPaused = ()
  commandName _ = "Animation.setPaused"
  fromJSON = const . A.Success . const ()

-- | Sets the playback rate of the document timeline.

-- | Parameters of the 'Animation.setPlaybackRate' command.
data PAnimationSetPlaybackRate = PAnimationSetPlaybackRate
  {
    -- | Playback rate for animations on page
    pAnimationSetPlaybackRatePlaybackRate :: Double
  }
  deriving (Eq, Show)
pAnimationSetPlaybackRate
  {-
  -- | Playback rate for animations on page
  -}
  :: Double
  -> PAnimationSetPlaybackRate
pAnimationSetPlaybackRate
  arg_pAnimationSetPlaybackRatePlaybackRate
  = PAnimationSetPlaybackRate
    arg_pAnimationSetPlaybackRatePlaybackRate
instance ToJSON PAnimationSetPlaybackRate where
  toJSON p = A.object $ catMaybes [
    ("playbackRate" A..=) <$> Just (pAnimationSetPlaybackRatePlaybackRate p)
    ]
instance Command PAnimationSetPlaybackRate where
  type CommandResponse PAnimationSetPlaybackRate = ()
  commandName _ = "Animation.setPlaybackRate"
  fromJSON = const . A.Success . const ()

-- | Sets the timing of an animation node.

-- | Parameters of the 'Animation.setTiming' command.
data PAnimationSetTiming = PAnimationSetTiming
  {
    -- | Animation id.
    pAnimationSetTimingAnimationId :: T.Text,
    -- | Duration of the animation.
    pAnimationSetTimingDuration :: Double,
    -- | Delay of the animation.
    pAnimationSetTimingDelay :: Double
  }
  deriving (Eq, Show)
pAnimationSetTiming
  {-
  -- | Animation id.
  -}
  :: T.Text
  {-
  -- | Duration of the animation.
  -}
  -> Double
  {-
  -- | Delay of the animation.
  -}
  -> Double
  -> PAnimationSetTiming
pAnimationSetTiming
  arg_pAnimationSetTimingAnimationId
  arg_pAnimationSetTimingDuration
  arg_pAnimationSetTimingDelay
  = PAnimationSetTiming
    arg_pAnimationSetTimingAnimationId
    arg_pAnimationSetTimingDuration
    arg_pAnimationSetTimingDelay
instance ToJSON PAnimationSetTiming where
  toJSON p = A.object $ catMaybes [
    ("animationId" A..=) <$> Just (pAnimationSetTimingAnimationId p),
    ("duration" A..=) <$> Just (pAnimationSetTimingDuration p),
    ("delay" A..=) <$> Just (pAnimationSetTimingDelay p)
    ]
instance Command PAnimationSetTiming where
  type CommandResponse PAnimationSetTiming = ()
  commandName _ = "Animation.setTiming"
  fromJSON = const . A.Success . const ()

