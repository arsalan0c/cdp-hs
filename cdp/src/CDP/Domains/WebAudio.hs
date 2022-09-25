{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



type WebAudioGraphObjectId = String
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


type WebAudioNodeType = String
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


type WebAudioParamType = String
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



data WebAudioContextRealtimeData = WebAudioContextRealtimeData {
   webAudioContextRealtimeDataCurrentTime :: Double,
   webAudioContextRealtimeDataRenderCapacity :: Double,
   webAudioContextRealtimeDataCallbackIntervalMean :: Double,
   webAudioContextRealtimeDataCallbackIntervalVariance :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextRealtimeData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data WebAudioBaseAudioContext = WebAudioBaseAudioContext {
   webAudioBaseAudioContextContextId :: WebAudioGraphObjectId,
   webAudioBaseAudioContextContextType :: WebAudioContextType,
   webAudioBaseAudioContextContextState :: WebAudioContextState,
   webAudioBaseAudioContextRealtimeData :: Maybe WebAudioContextRealtimeData,
   webAudioBaseAudioContextCallbackBufferSize :: Double,
   webAudioBaseAudioContextMaxOutputChannelCount :: Double,
   webAudioBaseAudioContextSampleRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioBaseAudioContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  WebAudioBaseAudioContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data WebAudioAudioListener = WebAudioAudioListener {
   webAudioAudioListenerListenerId :: WebAudioGraphObjectId,
   webAudioAudioListenerContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListener  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListener where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



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





data WebAudioContextCreated = WebAudioContextCreated {
   webAudioContextCreatedContext :: WebAudioBaseAudioContext
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data WebAudioContextWillBeDestroyed = WebAudioContextWillBeDestroyed {
   webAudioContextWillBeDestroyedContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data WebAudioContextChanged = WebAudioContextChanged {
   webAudioContextChangedContext :: WebAudioBaseAudioContext
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioContextChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  WebAudioContextChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data WebAudioAudioListenerCreated = WebAudioAudioListenerCreated {
   webAudioAudioListenerCreatedListener :: WebAudioAudioListener
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListenerCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListenerCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data WebAudioAudioListenerWillBeDestroyed = WebAudioAudioListenerWillBeDestroyed {
   webAudioAudioListenerWillBeDestroyedContextId :: WebAudioGraphObjectId,
   webAudioAudioListenerWillBeDestroyedListenerId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioListenerWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioListenerWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



data WebAudioAudioNodeCreated = WebAudioAudioNodeCreated {
   webAudioAudioNodeCreatedNode :: WebAudioAudioNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioNodeCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioNodeCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data WebAudioAudioNodeWillBeDestroyed = WebAudioAudioNodeWillBeDestroyed {
   webAudioAudioNodeWillBeDestroyedContextId :: WebAudioGraphObjectId,
   webAudioAudioNodeWillBeDestroyedNodeId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioNodeWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioNodeWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



data WebAudioAudioParamCreated = WebAudioAudioParamCreated {
   webAudioAudioParamCreatedParam :: WebAudioAudioParam
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioParamCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioParamCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data WebAudioAudioParamWillBeDestroyed = WebAudioAudioParamWillBeDestroyed {
   webAudioAudioParamWillBeDestroyedContextId :: WebAudioGraphObjectId,
   webAudioAudioParamWillBeDestroyedNodeId :: WebAudioGraphObjectId,
   webAudioAudioParamWillBeDestroyedParamId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAudioAudioParamWillBeDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  WebAudioAudioParamWillBeDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



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




webAudioEnable :: Handle ev -> IO (Maybe Error)
webAudioEnable handle = sendReceiveCommand handle "WebAudio.enable" (Nothing :: Maybe ())


webAudioDisable :: Handle ev -> IO (Maybe Error)
webAudioDisable handle = sendReceiveCommand handle "WebAudio.disable" (Nothing :: Maybe ())



data PWebAudioGetRealtimeData = PWebAudioGetRealtimeData {
   pWebAudioGetRealtimeDataContextId :: WebAudioGraphObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAudioGetRealtimeData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PWebAudioGetRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


webAudioGetRealtimeData :: Handle ev -> PWebAudioGetRealtimeData -> IO (Either Error WebAudioGetRealtimeData)
webAudioGetRealtimeData handle params = sendReceiveCommandResult handle "WebAudio.getRealtimeData" (Just params)

data WebAudioGetRealtimeData = WebAudioGetRealtimeData {
   webAudioGetRealtimeDataRealtimeData :: WebAudioContextRealtimeData
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAudioGetRealtimeData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command WebAudioGetRealtimeData where
   commandName _ = "WebAudio.getRealtimeData"




