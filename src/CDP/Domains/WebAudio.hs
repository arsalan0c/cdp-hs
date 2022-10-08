{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  WebAudio :
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




-- | Type 'WebAudio.GraphObjectId'.
--   An unique ID for a graph object (AudioContext, AudioNode, AudioParam) in Web Audio API
type WebAudioGraphObjectId = String

-- | Type 'WebAudio.ContextType'.
--   Enum of BaseAudioContext types
data WebAudioContextType = WebAudioContextTypeRealtime | WebAudioContextTypeOffline
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioContextType where
   parseJSON = A.withText  "WebAudioContextType"  $ \v -> do
      case v of
         "realtime" -> pure WebAudioContextTypeRealtime
         "offline" -> pure WebAudioContextTypeOffline
         _ -> fail "failed to parse WebAudioContextType"

instance ToJSON WebAudioContextType where
   toJSON v = A.String $
      case v of
         WebAudioContextTypeRealtime -> "realtime"
         WebAudioContextTypeOffline -> "offline"



-- | Type 'WebAudio.ContextState'.
--   Enum of AudioContextState from the spec
data WebAudioContextState = WebAudioContextStateSuspended | WebAudioContextStateRunning | WebAudioContextStateClosed
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioContextState where
   parseJSON = A.withText  "WebAudioContextState"  $ \v -> do
      case v of
         "suspended" -> pure WebAudioContextStateSuspended
         "running" -> pure WebAudioContextStateRunning
         "closed" -> pure WebAudioContextStateClosed
         _ -> fail "failed to parse WebAudioContextState"

instance ToJSON WebAudioContextState where
   toJSON v = A.String $
      case v of
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
   parseJSON = A.withText  "WebAudioChannelCountMode"  $ \v -> do
      case v of
         "clamped-max" -> pure WebAudioChannelCountModeClampedMax
         "explicit" -> pure WebAudioChannelCountModeExplicit
         "max" -> pure WebAudioChannelCountModeMax
         _ -> fail "failed to parse WebAudioChannelCountMode"

instance ToJSON WebAudioChannelCountMode where
   toJSON v = A.String $
      case v of
         WebAudioChannelCountModeClampedMax -> "clamped-max"
         WebAudioChannelCountModeExplicit -> "explicit"
         WebAudioChannelCountModeMax -> "max"



-- | Type 'WebAudio.ChannelInterpretation'.
--   Enum of AudioNode::ChannelInterpretation from the spec
data WebAudioChannelInterpretation = WebAudioChannelInterpretationDiscrete | WebAudioChannelInterpretationSpeakers
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAudioChannelInterpretation where
   parseJSON = A.withText  "WebAudioChannelInterpretation"  $ \v -> do
      case v of
         "discrete" -> pure WebAudioChannelInterpretationDiscrete
         "speakers" -> pure WebAudioChannelInterpretationSpeakers
         _ -> fail "failed to parse WebAudioChannelInterpretation"

instance ToJSON WebAudioChannelInterpretation where
   toJSON v = A.String $
      case v of
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
   parseJSON = A.withText  "WebAudioAutomationRate"  $ \v -> do
      case v of
         "a-rate" -> pure WebAudioAutomationRateARate
         "k-rate" -> pure WebAudioAutomationRateKRate
         _ -> fail "failed to parse WebAudioAutomationRate"

instance ToJSON WebAudioAutomationRate where
   toJSON v = A.String $
      case v of
         WebAudioAutomationRateARate -> "a-rate"
         WebAudioAutomationRateKRate -> "k-rate"



-- | Type 'WebAudio.ContextRealtimeData'.
--   Fields in AudioContext that change in real-time.
data WebAudioContextRealtimeData = WebAudioContextRealtimeData {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextRealtimeData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'WebAudio.BaseAudioContext'.
--   Protocol object for BaseAudioContext
data WebAudioBaseAudioContext = WebAudioBaseAudioContext {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioBaseAudioContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  WebAudioBaseAudioContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'WebAudio.AudioListener'.
--   Protocol object for AudioListener
data WebAudioAudioListener = WebAudioAudioListener {
  webAudioAudioListenerListenerId :: WebAudioGraphObjectId,
  webAudioAudioListenerContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'WebAudio.AudioNode'.
--   Protocol object for AudioNode
data WebAudioAudioNode = WebAudioAudioNode {
  webAudioAudioNodeNodeId :: WebAudioGraphObjectId,
  webAudioAudioNodeContextId :: WebAudioGraphObjectId,
  webAudioAudioNodeNodeType :: WebAudioNodeType,
  webAudioAudioNodeNumberOfInputs :: Double,
  webAudioAudioNodeNumberOfOutputs :: Double,
  webAudioAudioNodeChannelCount :: Double,
  webAudioAudioNodeChannelCountMode :: WebAudioChannelCountMode,
  webAudioAudioNodeChannelInterpretation :: WebAudioChannelInterpretation
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'WebAudio.AudioParam'.
--   Protocol object for AudioParam
data WebAudioAudioParam = WebAudioAudioParam {
  webAudioAudioParamParamId :: WebAudioGraphObjectId,
  webAudioAudioParamNodeId :: WebAudioGraphObjectId,
  webAudioAudioParamContextId :: WebAudioGraphObjectId,
  webAudioAudioParamParamType :: WebAudioParamType,
  webAudioAudioParamRate :: WebAudioAutomationRate,
  webAudioAudioParamDefaultValue :: Double,
  webAudioAudioParamMinValue :: Double,
  webAudioAudioParamMaxValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }





-- | Type of the 'WebAudio.contextCreated' event.
data WebAudioContextCreated = WebAudioContextCreated {
  webAudioContextCreatedContext :: WebAudioBaseAudioContext
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Event WebAudioContextCreated where
    eventName _ = "WebAudio.contextCreated"

-- | Type of the 'WebAudio.contextWillBeDestroyed' event.
data WebAudioContextWillBeDestroyed = WebAudioContextWillBeDestroyed {
  webAudioContextWillBeDestroyedContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Event WebAudioContextWillBeDestroyed where
    eventName _ = "WebAudio.contextWillBeDestroyed"

-- | Type of the 'WebAudio.contextChanged' event.
data WebAudioContextChanged = WebAudioContextChanged {
  webAudioContextChangedContext :: WebAudioBaseAudioContext
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Event WebAudioContextChanged where
    eventName _ = "WebAudio.contextChanged"

-- | Type of the 'WebAudio.audioListenerCreated' event.
data WebAudioAudioListenerCreated = WebAudioAudioListenerCreated {
  webAudioAudioListenerCreatedListener :: WebAudioAudioListener
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListenerCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListenerCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Event WebAudioAudioListenerCreated where
    eventName _ = "WebAudio.audioListenerCreated"

-- | Type of the 'WebAudio.audioListenerWillBeDestroyed' event.
data WebAudioAudioListenerWillBeDestroyed = WebAudioAudioListenerWillBeDestroyed {
  webAudioAudioListenerWillBeDestroyedContextId :: WebAudioGraphObjectId,
  webAudioAudioListenerWillBeDestroyedListenerId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListenerWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListenerWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Event WebAudioAudioListenerWillBeDestroyed where
    eventName _ = "WebAudio.audioListenerWillBeDestroyed"

-- | Type of the 'WebAudio.audioNodeCreated' event.
data WebAudioAudioNodeCreated = WebAudioAudioNodeCreated {
  webAudioAudioNodeCreatedNode :: WebAudioAudioNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioNodeCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioNodeCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event WebAudioAudioNodeCreated where
    eventName _ = "WebAudio.audioNodeCreated"

-- | Type of the 'WebAudio.audioNodeWillBeDestroyed' event.
data WebAudioAudioNodeWillBeDestroyed = WebAudioAudioNodeWillBeDestroyed {
  webAudioAudioNodeWillBeDestroyedContextId :: WebAudioGraphObjectId,
  webAudioAudioNodeWillBeDestroyedNodeId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioNodeWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioNodeWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event WebAudioAudioNodeWillBeDestroyed where
    eventName _ = "WebAudio.audioNodeWillBeDestroyed"

-- | Type of the 'WebAudio.audioParamCreated' event.
data WebAudioAudioParamCreated = WebAudioAudioParamCreated {
  webAudioAudioParamCreatedParam :: WebAudioAudioParam
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioParamCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioParamCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event WebAudioAudioParamCreated where
    eventName _ = "WebAudio.audioParamCreated"

-- | Type of the 'WebAudio.audioParamWillBeDestroyed' event.
data WebAudioAudioParamWillBeDestroyed = WebAudioAudioParamWillBeDestroyed {
  webAudioAudioParamWillBeDestroyedContextId :: WebAudioGraphObjectId,
  webAudioAudioParamWillBeDestroyedNodeId :: WebAudioGraphObjectId,
  webAudioAudioParamWillBeDestroyedParamId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioParamWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioParamWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Event WebAudioAudioParamWillBeDestroyed where
    eventName _ = "WebAudio.audioParamWillBeDestroyed"

-- | Type of the 'WebAudio.nodesConnected' event.
data WebAudioNodesConnected = WebAudioNodesConnected {
  webAudioNodesConnectedContextId :: WebAudioGraphObjectId,
  webAudioNodesConnectedSourceId :: WebAudioGraphObjectId,
  webAudioNodesConnectedDestinationId :: WebAudioGraphObjectId,
  webAudioNodesConnectedSourceOutputIndex :: Maybe Double,
  webAudioNodesConnectedDestinationInputIndex :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioNodesConnected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  WebAudioNodesConnected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Event WebAudioNodesConnected where
    eventName _ = "WebAudio.nodesConnected"

-- | Type of the 'WebAudio.nodesDisconnected' event.
data WebAudioNodesDisconnected = WebAudioNodesDisconnected {
  webAudioNodesDisconnectedContextId :: WebAudioGraphObjectId,
  webAudioNodesDisconnectedSourceId :: WebAudioGraphObjectId,
  webAudioNodesDisconnectedDestinationId :: WebAudioGraphObjectId,
  webAudioNodesDisconnectedSourceOutputIndex :: Maybe Double,
  webAudioNodesDisconnectedDestinationInputIndex :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioNodesDisconnected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  WebAudioNodesDisconnected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event WebAudioNodesDisconnected where
    eventName _ = "WebAudio.nodesDisconnected"

-- | Type of the 'WebAudio.nodeParamConnected' event.
data WebAudioNodeParamConnected = WebAudioNodeParamConnected {
  webAudioNodeParamConnectedContextId :: WebAudioGraphObjectId,
  webAudioNodeParamConnectedSourceId :: WebAudioGraphObjectId,
  webAudioNodeParamConnectedDestinationId :: WebAudioGraphObjectId,
  webAudioNodeParamConnectedSourceOutputIndex :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioNodeParamConnected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  WebAudioNodeParamConnected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Event WebAudioNodeParamConnected where
    eventName _ = "WebAudio.nodeParamConnected"

-- | Type of the 'WebAudio.nodeParamDisconnected' event.
data WebAudioNodeParamDisconnected = WebAudioNodeParamDisconnected {
  webAudioNodeParamDisconnectedContextId :: WebAudioGraphObjectId,
  webAudioNodeParamDisconnectedSourceId :: WebAudioGraphObjectId,
  webAudioNodeParamDisconnectedDestinationId :: WebAudioGraphObjectId,
  webAudioNodeParamDisconnectedSourceOutputIndex :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioNodeParamDisconnected  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  WebAudioNodeParamDisconnected where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Event WebAudioNodeParamDisconnected where
    eventName _ = "WebAudio.nodeParamDisconnected"



-- | Function for the 'WebAudio.enable' command.
--   Enables the WebAudio domain and starts sending context lifetime events.
webAudioEnable :: Handle ev -> IO ()
webAudioEnable handle = sendReceiveCommand handle "WebAudio.enable" (Nothing :: Maybe ())


-- | Function for the 'WebAudio.disable' command.
--   Disables the WebAudio domain.
webAudioDisable :: Handle ev -> IO ()
webAudioDisable handle = sendReceiveCommand handle "WebAudio.disable" (Nothing :: Maybe ())


-- | Parameters of the 'webAudioGetRealtimeData' command.
data PWebAudioGetRealtimeData = PWebAudioGetRealtimeData {
  pWebAudioGetRealtimeDataContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAudioGetRealtimeData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PWebAudioGetRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'WebAudio.getRealtimeData' command.
--   Fetch the realtime data from the registered contexts.
--   Parameters: 'PWebAudioGetRealtimeData'
--   Returns: 'WebAudioGetRealtimeData'
webAudioGetRealtimeData :: Handle ev -> PWebAudioGetRealtimeData -> IO WebAudioGetRealtimeData
webAudioGetRealtimeData handle params = sendReceiveCommandResult handle "WebAudio.getRealtimeData" (Just params)

-- | Return type of the 'webAudioGetRealtimeData' command.
data WebAudioGetRealtimeData = WebAudioGetRealtimeData {
  webAudioGetRealtimeDataRealtimeData :: WebAudioContextRealtimeData
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAudioGetRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command WebAudioGetRealtimeData where
   commandName _ = "WebAudio.getRealtimeData"




