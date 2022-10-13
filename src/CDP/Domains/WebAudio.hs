{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= WebAudio

This domain allows inspection of Web Audio API.
https://webaudio.github.io/web-audio-api/
-}


module CDP.Domains.WebAudio (module CDP.Domains.WebAudio) where

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




-- | Type 'WebAudio.GraphObjectId'.
--   An unique ID for a graph object (AudioContext, AudioNode, AudioParam) in Web Audio API
type WebAudioGraphObjectId = String

-- | Type 'WebAudio.ContextType'.
--   Enum of BaseAudioContext types
data WebAudioContextType = WebAudioContextTypeRealtime | WebAudioContextTypeOffline
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioContextType where
  parseJSON = A.withText "WebAudioContextType" $ \v -> case v of
    "realtime" -> pure WebAudioContextTypeRealtime
    "offline" -> pure WebAudioContextTypeOffline
    "_" -> fail "failed to parse WebAudioContextType"
instance ToJSON WebAudioContextType where
  toJSON v = A.String $ case v of
    WebAudioContextTypeRealtime -> "realtime"
    WebAudioContextTypeOffline -> "offline"

-- | Type 'WebAudio.ContextState'.
--   Enum of AudioContextState from the spec
data WebAudioContextState = WebAudioContextStateSuspended | WebAudioContextStateRunning | WebAudioContextStateClosed
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioContextState where
  parseJSON = A.withText "WebAudioContextState" $ \v -> case v of
    "suspended" -> pure WebAudioContextStateSuspended
    "running" -> pure WebAudioContextStateRunning
    "closed" -> pure WebAudioContextStateClosed
    "_" -> fail "failed to parse WebAudioContextState"
instance ToJSON WebAudioContextState where
  toJSON v = A.String $ case v of
    WebAudioContextStateSuspended -> "suspended"
    WebAudioContextStateRunning -> "running"
    WebAudioContextStateClosed -> "closed"

-- | Type 'WebAudio.NodeType'.
--   Enum of AudioNode types
type WebAudioNodeType = String

-- | Type 'WebAudio.ChannelCountMode'.
--   Enum of AudioNode::ChannelCountMode from the spec
data WebAudioChannelCountMode = WebAudioChannelCountModeClampedMax | WebAudioChannelCountModeExplicit | WebAudioChannelCountModeMax
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioChannelCountMode where
  parseJSON = A.withText "WebAudioChannelCountMode" $ \v -> case v of
    "clamped-max" -> pure WebAudioChannelCountModeClampedMax
    "explicit" -> pure WebAudioChannelCountModeExplicit
    "max" -> pure WebAudioChannelCountModeMax
    "_" -> fail "failed to parse WebAudioChannelCountMode"
instance ToJSON WebAudioChannelCountMode where
  toJSON v = A.String $ case v of
    WebAudioChannelCountModeClampedMax -> "clamped-max"
    WebAudioChannelCountModeExplicit -> "explicit"
    WebAudioChannelCountModeMax -> "max"

-- | Type 'WebAudio.ChannelInterpretation'.
--   Enum of AudioNode::ChannelInterpretation from the spec
data WebAudioChannelInterpretation = WebAudioChannelInterpretationDiscrete | WebAudioChannelInterpretationSpeakers
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioChannelInterpretation where
  parseJSON = A.withText "WebAudioChannelInterpretation" $ \v -> case v of
    "discrete" -> pure WebAudioChannelInterpretationDiscrete
    "speakers" -> pure WebAudioChannelInterpretationSpeakers
    "_" -> fail "failed to parse WebAudioChannelInterpretation"
instance ToJSON WebAudioChannelInterpretation where
  toJSON v = A.String $ case v of
    WebAudioChannelInterpretationDiscrete -> "discrete"
    WebAudioChannelInterpretationSpeakers -> "speakers"

-- | Type 'WebAudio.ParamType'.
--   Enum of AudioParam types
type WebAudioParamType = String

-- | Type 'WebAudio.AutomationRate'.
--   Enum of AudioParam::AutomationRate from the spec
data WebAudioAutomationRate = WebAudioAutomationRateARate | WebAudioAutomationRateKRate
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioAutomationRate where
  parseJSON = A.withText "WebAudioAutomationRate" $ \v -> case v of
    "a-rate" -> pure WebAudioAutomationRateARate
    "k-rate" -> pure WebAudioAutomationRateKRate
    "_" -> fail "failed to parse WebAudioAutomationRate"
instance ToJSON WebAudioAutomationRate where
  toJSON v = A.String $ case v of
    WebAudioAutomationRateARate -> "a-rate"
    WebAudioAutomationRateKRate -> "k-rate"

-- | Type 'WebAudio.ContextRealtimeData'.
--   Fields in AudioContext that change in real-time.
data WebAudioContextRealtimeData = WebAudioContextRealtimeData
  {
    -- | The current context time in second in BaseAudioContext.
    webAudioContextRealtimeDataCurrentTime :: Double,
    -- | The time spent on rendering graph divided by render quantum duration,
    --   and multiplied by 100. 100 means the audio renderer reached the full
    --   capacity and glitch may occur.
    webAudioContextRealtimeDataRenderCapacity :: Double,
    -- | A running mean of callback interval.
    webAudioContextRealtimeDataCallbackIntervalMean :: Double,
    -- | A running variance of callback interval.
    webAudioContextRealtimeDataCallbackIntervalVariance :: Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioContextRealtimeData where
  parseJSON = A.withObject "WebAudioContextRealtimeData" $ \o -> WebAudioContextRealtimeData
    <$> o A..: "currentTime"
    <*> o A..: "renderCapacity"
    <*> o A..: "callbackIntervalMean"
    <*> o A..: "callbackIntervalVariance"
instance ToJSON WebAudioContextRealtimeData where
  toJSON p = A.object $ catMaybes [
    ("currentTime" A..=) <$> Just (webAudioContextRealtimeDataCurrentTime p),
    ("renderCapacity" A..=) <$> Just (webAudioContextRealtimeDataRenderCapacity p),
    ("callbackIntervalMean" A..=) <$> Just (webAudioContextRealtimeDataCallbackIntervalMean p),
    ("callbackIntervalVariance" A..=) <$> Just (webAudioContextRealtimeDataCallbackIntervalVariance p)
    ]

-- | Type 'WebAudio.BaseAudioContext'.
--   Protocol object for BaseAudioContext
data WebAudioBaseAudioContext = WebAudioBaseAudioContext
  {
    webAudioBaseAudioContextContextId :: WebAudioGraphObjectId,
    webAudioBaseAudioContextContextType :: WebAudioContextType,
    webAudioBaseAudioContextContextState :: WebAudioContextState,
    webAudioBaseAudioContextRealtimeData :: Maybe WebAudioContextRealtimeData,
    -- | Platform-dependent callback buffer size.
    webAudioBaseAudioContextCallbackBufferSize :: Double,
    -- | Number of output channels supported by audio hardware in use.
    webAudioBaseAudioContextMaxOutputChannelCount :: Double,
    -- | Context sample rate.
    webAudioBaseAudioContextSampleRate :: Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioBaseAudioContext where
  parseJSON = A.withObject "WebAudioBaseAudioContext" $ \o -> WebAudioBaseAudioContext
    <$> o A..: "contextId"
    <*> o A..: "contextType"
    <*> o A..: "contextState"
    <*> o A..:? "realtimeData"
    <*> o A..: "callbackBufferSize"
    <*> o A..: "maxOutputChannelCount"
    <*> o A..: "sampleRate"
instance ToJSON WebAudioBaseAudioContext where
  toJSON p = A.object $ catMaybes [
    ("contextId" A..=) <$> Just (webAudioBaseAudioContextContextId p),
    ("contextType" A..=) <$> Just (webAudioBaseAudioContextContextType p),
    ("contextState" A..=) <$> Just (webAudioBaseAudioContextContextState p),
    ("realtimeData" A..=) <$> (webAudioBaseAudioContextRealtimeData p),
    ("callbackBufferSize" A..=) <$> Just (webAudioBaseAudioContextCallbackBufferSize p),
    ("maxOutputChannelCount" A..=) <$> Just (webAudioBaseAudioContextMaxOutputChannelCount p),
    ("sampleRate" A..=) <$> Just (webAudioBaseAudioContextSampleRate p)
    ]

-- | Type 'WebAudio.AudioListener'.
--   Protocol object for AudioListener
data WebAudioAudioListener = WebAudioAudioListener
  {
    webAudioAudioListenerListenerId :: WebAudioGraphObjectId,
    webAudioAudioListenerContextId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioListener where
  parseJSON = A.withObject "WebAudioAudioListener" $ \o -> WebAudioAudioListener
    <$> o A..: "listenerId"
    <*> o A..: "contextId"
instance ToJSON WebAudioAudioListener where
  toJSON p = A.object $ catMaybes [
    ("listenerId" A..=) <$> Just (webAudioAudioListenerListenerId p),
    ("contextId" A..=) <$> Just (webAudioAudioListenerContextId p)
    ]

-- | Type 'WebAudio.AudioNode'.
--   Protocol object for AudioNode
data WebAudioAudioNode = WebAudioAudioNode
  {
    webAudioAudioNodeNodeId :: WebAudioGraphObjectId,
    webAudioAudioNodeContextId :: WebAudioGraphObjectId,
    webAudioAudioNodeNodeType :: WebAudioNodeType,
    webAudioAudioNodeNumberOfInputs :: Double,
    webAudioAudioNodeNumberOfOutputs :: Double,
    webAudioAudioNodeChannelCount :: Double,
    webAudioAudioNodeChannelCountMode :: WebAudioChannelCountMode,
    webAudioAudioNodeChannelInterpretation :: WebAudioChannelInterpretation
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioNode where
  parseJSON = A.withObject "WebAudioAudioNode" $ \o -> WebAudioAudioNode
    <$> o A..: "nodeId"
    <*> o A..: "contextId"
    <*> o A..: "nodeType"
    <*> o A..: "numberOfInputs"
    <*> o A..: "numberOfOutputs"
    <*> o A..: "channelCount"
    <*> o A..: "channelCountMode"
    <*> o A..: "channelInterpretation"
instance ToJSON WebAudioAudioNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (webAudioAudioNodeNodeId p),
    ("contextId" A..=) <$> Just (webAudioAudioNodeContextId p),
    ("nodeType" A..=) <$> Just (webAudioAudioNodeNodeType p),
    ("numberOfInputs" A..=) <$> Just (webAudioAudioNodeNumberOfInputs p),
    ("numberOfOutputs" A..=) <$> Just (webAudioAudioNodeNumberOfOutputs p),
    ("channelCount" A..=) <$> Just (webAudioAudioNodeChannelCount p),
    ("channelCountMode" A..=) <$> Just (webAudioAudioNodeChannelCountMode p),
    ("channelInterpretation" A..=) <$> Just (webAudioAudioNodeChannelInterpretation p)
    ]

-- | Type 'WebAudio.AudioParam'.
--   Protocol object for AudioParam
data WebAudioAudioParam = WebAudioAudioParam
  {
    webAudioAudioParamParamId :: WebAudioGraphObjectId,
    webAudioAudioParamNodeId :: WebAudioGraphObjectId,
    webAudioAudioParamContextId :: WebAudioGraphObjectId,
    webAudioAudioParamParamType :: WebAudioParamType,
    webAudioAudioParamRate :: WebAudioAutomationRate,
    webAudioAudioParamDefaultValue :: Double,
    webAudioAudioParamMinValue :: Double,
    webAudioAudioParamMaxValue :: Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioParam where
  parseJSON = A.withObject "WebAudioAudioParam" $ \o -> WebAudioAudioParam
    <$> o A..: "paramId"
    <*> o A..: "nodeId"
    <*> o A..: "contextId"
    <*> o A..: "paramType"
    <*> o A..: "rate"
    <*> o A..: "defaultValue"
    <*> o A..: "minValue"
    <*> o A..: "maxValue"
instance ToJSON WebAudioAudioParam where
  toJSON p = A.object $ catMaybes [
    ("paramId" A..=) <$> Just (webAudioAudioParamParamId p),
    ("nodeId" A..=) <$> Just (webAudioAudioParamNodeId p),
    ("contextId" A..=) <$> Just (webAudioAudioParamContextId p),
    ("paramType" A..=) <$> Just (webAudioAudioParamParamType p),
    ("rate" A..=) <$> Just (webAudioAudioParamRate p),
    ("defaultValue" A..=) <$> Just (webAudioAudioParamDefaultValue p),
    ("minValue" A..=) <$> Just (webAudioAudioParamMinValue p),
    ("maxValue" A..=) <$> Just (webAudioAudioParamMaxValue p)
    ]

-- | Type of the 'WebAudio.contextCreated' event.
data WebAudioContextCreated = WebAudioContextCreated
  {
    webAudioContextCreatedContext :: WebAudioBaseAudioContext
  }
  deriving (Eq, Show)
instance FromJSON WebAudioContextCreated where
  parseJSON = A.withObject "WebAudioContextCreated" $ \o -> WebAudioContextCreated
    <$> o A..: "context"
instance Event WebAudioContextCreated where
  eventName _ = "WebAudio.contextCreated"

-- | Type of the 'WebAudio.contextWillBeDestroyed' event.
data WebAudioContextWillBeDestroyed = WebAudioContextWillBeDestroyed
  {
    webAudioContextWillBeDestroyedContextId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
instance FromJSON WebAudioContextWillBeDestroyed where
  parseJSON = A.withObject "WebAudioContextWillBeDestroyed" $ \o -> WebAudioContextWillBeDestroyed
    <$> o A..: "contextId"
instance Event WebAudioContextWillBeDestroyed where
  eventName _ = "WebAudio.contextWillBeDestroyed"

-- | Type of the 'WebAudio.contextChanged' event.
data WebAudioContextChanged = WebAudioContextChanged
  {
    webAudioContextChangedContext :: WebAudioBaseAudioContext
  }
  deriving (Eq, Show)
instance FromJSON WebAudioContextChanged where
  parseJSON = A.withObject "WebAudioContextChanged" $ \o -> WebAudioContextChanged
    <$> o A..: "context"
instance Event WebAudioContextChanged where
  eventName _ = "WebAudio.contextChanged"

-- | Type of the 'WebAudio.audioListenerCreated' event.
data WebAudioAudioListenerCreated = WebAudioAudioListenerCreated
  {
    webAudioAudioListenerCreatedListener :: WebAudioAudioListener
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioListenerCreated where
  parseJSON = A.withObject "WebAudioAudioListenerCreated" $ \o -> WebAudioAudioListenerCreated
    <$> o A..: "listener"
instance Event WebAudioAudioListenerCreated where
  eventName _ = "WebAudio.audioListenerCreated"

-- | Type of the 'WebAudio.audioListenerWillBeDestroyed' event.
data WebAudioAudioListenerWillBeDestroyed = WebAudioAudioListenerWillBeDestroyed
  {
    webAudioAudioListenerWillBeDestroyedContextId :: WebAudioGraphObjectId,
    webAudioAudioListenerWillBeDestroyedListenerId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioListenerWillBeDestroyed where
  parseJSON = A.withObject "WebAudioAudioListenerWillBeDestroyed" $ \o -> WebAudioAudioListenerWillBeDestroyed
    <$> o A..: "contextId"
    <*> o A..: "listenerId"
instance Event WebAudioAudioListenerWillBeDestroyed where
  eventName _ = "WebAudio.audioListenerWillBeDestroyed"

-- | Type of the 'WebAudio.audioNodeCreated' event.
data WebAudioAudioNodeCreated = WebAudioAudioNodeCreated
  {
    webAudioAudioNodeCreatedNode :: WebAudioAudioNode
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioNodeCreated where
  parseJSON = A.withObject "WebAudioAudioNodeCreated" $ \o -> WebAudioAudioNodeCreated
    <$> o A..: "node"
instance Event WebAudioAudioNodeCreated where
  eventName _ = "WebAudio.audioNodeCreated"

-- | Type of the 'WebAudio.audioNodeWillBeDestroyed' event.
data WebAudioAudioNodeWillBeDestroyed = WebAudioAudioNodeWillBeDestroyed
  {
    webAudioAudioNodeWillBeDestroyedContextId :: WebAudioGraphObjectId,
    webAudioAudioNodeWillBeDestroyedNodeId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioNodeWillBeDestroyed where
  parseJSON = A.withObject "WebAudioAudioNodeWillBeDestroyed" $ \o -> WebAudioAudioNodeWillBeDestroyed
    <$> o A..: "contextId"
    <*> o A..: "nodeId"
instance Event WebAudioAudioNodeWillBeDestroyed where
  eventName _ = "WebAudio.audioNodeWillBeDestroyed"

-- | Type of the 'WebAudio.audioParamCreated' event.
data WebAudioAudioParamCreated = WebAudioAudioParamCreated
  {
    webAudioAudioParamCreatedParam :: WebAudioAudioParam
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioParamCreated where
  parseJSON = A.withObject "WebAudioAudioParamCreated" $ \o -> WebAudioAudioParamCreated
    <$> o A..: "param"
instance Event WebAudioAudioParamCreated where
  eventName _ = "WebAudio.audioParamCreated"

-- | Type of the 'WebAudio.audioParamWillBeDestroyed' event.
data WebAudioAudioParamWillBeDestroyed = WebAudioAudioParamWillBeDestroyed
  {
    webAudioAudioParamWillBeDestroyedContextId :: WebAudioGraphObjectId,
    webAudioAudioParamWillBeDestroyedNodeId :: WebAudioGraphObjectId,
    webAudioAudioParamWillBeDestroyedParamId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
instance FromJSON WebAudioAudioParamWillBeDestroyed where
  parseJSON = A.withObject "WebAudioAudioParamWillBeDestroyed" $ \o -> WebAudioAudioParamWillBeDestroyed
    <$> o A..: "contextId"
    <*> o A..: "nodeId"
    <*> o A..: "paramId"
instance Event WebAudioAudioParamWillBeDestroyed where
  eventName _ = "WebAudio.audioParamWillBeDestroyed"

-- | Type of the 'WebAudio.nodesConnected' event.
data WebAudioNodesConnected = WebAudioNodesConnected
  {
    webAudioNodesConnectedContextId :: WebAudioGraphObjectId,
    webAudioNodesConnectedSourceId :: WebAudioGraphObjectId,
    webAudioNodesConnectedDestinationId :: WebAudioGraphObjectId,
    webAudioNodesConnectedSourceOutputIndex :: Maybe Double,
    webAudioNodesConnectedDestinationInputIndex :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioNodesConnected where
  parseJSON = A.withObject "WebAudioNodesConnected" $ \o -> WebAudioNodesConnected
    <$> o A..: "contextId"
    <*> o A..: "sourceId"
    <*> o A..: "destinationId"
    <*> o A..:? "sourceOutputIndex"
    <*> o A..:? "destinationInputIndex"
instance Event WebAudioNodesConnected where
  eventName _ = "WebAudio.nodesConnected"

-- | Type of the 'WebAudio.nodesDisconnected' event.
data WebAudioNodesDisconnected = WebAudioNodesDisconnected
  {
    webAudioNodesDisconnectedContextId :: WebAudioGraphObjectId,
    webAudioNodesDisconnectedSourceId :: WebAudioGraphObjectId,
    webAudioNodesDisconnectedDestinationId :: WebAudioGraphObjectId,
    webAudioNodesDisconnectedSourceOutputIndex :: Maybe Double,
    webAudioNodesDisconnectedDestinationInputIndex :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioNodesDisconnected where
  parseJSON = A.withObject "WebAudioNodesDisconnected" $ \o -> WebAudioNodesDisconnected
    <$> o A..: "contextId"
    <*> o A..: "sourceId"
    <*> o A..: "destinationId"
    <*> o A..:? "sourceOutputIndex"
    <*> o A..:? "destinationInputIndex"
instance Event WebAudioNodesDisconnected where
  eventName _ = "WebAudio.nodesDisconnected"

-- | Type of the 'WebAudio.nodeParamConnected' event.
data WebAudioNodeParamConnected = WebAudioNodeParamConnected
  {
    webAudioNodeParamConnectedContextId :: WebAudioGraphObjectId,
    webAudioNodeParamConnectedSourceId :: WebAudioGraphObjectId,
    webAudioNodeParamConnectedDestinationId :: WebAudioGraphObjectId,
    webAudioNodeParamConnectedSourceOutputIndex :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioNodeParamConnected where
  parseJSON = A.withObject "WebAudioNodeParamConnected" $ \o -> WebAudioNodeParamConnected
    <$> o A..: "contextId"
    <*> o A..: "sourceId"
    <*> o A..: "destinationId"
    <*> o A..:? "sourceOutputIndex"
instance Event WebAudioNodeParamConnected where
  eventName _ = "WebAudio.nodeParamConnected"

-- | Type of the 'WebAudio.nodeParamDisconnected' event.
data WebAudioNodeParamDisconnected = WebAudioNodeParamDisconnected
  {
    webAudioNodeParamDisconnectedContextId :: WebAudioGraphObjectId,
    webAudioNodeParamDisconnectedSourceId :: WebAudioGraphObjectId,
    webAudioNodeParamDisconnectedDestinationId :: WebAudioGraphObjectId,
    webAudioNodeParamDisconnectedSourceOutputIndex :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON WebAudioNodeParamDisconnected where
  parseJSON = A.withObject "WebAudioNodeParamDisconnected" $ \o -> WebAudioNodeParamDisconnected
    <$> o A..: "contextId"
    <*> o A..: "sourceId"
    <*> o A..: "destinationId"
    <*> o A..:? "sourceOutputIndex"
instance Event WebAudioNodeParamDisconnected where
  eventName _ = "WebAudio.nodeParamDisconnected"

-- | Enables the WebAudio domain and starts sending context lifetime events.

-- | Parameters of the 'WebAudio.enable' command.
data PWebAudioEnable = PWebAudioEnable
  deriving (Eq, Show)
pWebAudioEnable
  :: PWebAudioEnable
pWebAudioEnable
  = PWebAudioEnable
instance ToJSON PWebAudioEnable where
  toJSON _ = A.Null
instance Command PWebAudioEnable where
  type CommandResponse PWebAudioEnable = ()
  commandName _ = "WebAudio.enable"
  fromJSON = const . A.Success . const ()

-- | Disables the WebAudio domain.

-- | Parameters of the 'WebAudio.disable' command.
data PWebAudioDisable = PWebAudioDisable
  deriving (Eq, Show)
pWebAudioDisable
  :: PWebAudioDisable
pWebAudioDisable
  = PWebAudioDisable
instance ToJSON PWebAudioDisable where
  toJSON _ = A.Null
instance Command PWebAudioDisable where
  type CommandResponse PWebAudioDisable = ()
  commandName _ = "WebAudio.disable"
  fromJSON = const . A.Success . const ()

-- | Fetch the realtime data from the registered contexts.

-- | Parameters of the 'WebAudio.getRealtimeData' command.
data PWebAudioGetRealtimeData = PWebAudioGetRealtimeData
  {
    pWebAudioGetRealtimeDataContextId :: WebAudioGraphObjectId
  }
  deriving (Eq, Show)
pWebAudioGetRealtimeData
  :: WebAudioGraphObjectId
  -> PWebAudioGetRealtimeData
pWebAudioGetRealtimeData
  arg_pWebAudioGetRealtimeDataContextId
  = PWebAudioGetRealtimeData
    arg_pWebAudioGetRealtimeDataContextId
instance ToJSON PWebAudioGetRealtimeData where
  toJSON p = A.object $ catMaybes [
    ("contextId" A..=) <$> Just (pWebAudioGetRealtimeDataContextId p)
    ]
data WebAudioGetRealtimeData = WebAudioGetRealtimeData
  {
    webAudioGetRealtimeDataRealtimeData :: WebAudioContextRealtimeData
  }
  deriving (Eq, Show)
instance FromJSON WebAudioGetRealtimeData where
  parseJSON = A.withObject "WebAudioGetRealtimeData" $ \o -> WebAudioGetRealtimeData
    <$> o A..: "realtimeData"
instance Command PWebAudioGetRealtimeData where
  type CommandResponse PWebAudioGetRealtimeData = WebAudioGetRealtimeData
  commandName _ = "WebAudio.getRealtimeData"

