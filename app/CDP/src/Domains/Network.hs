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

module Domains.Network (module Domains.Network) where

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

import qualified Domains.Debugger as Debugger
import qualified Domains.Runtime as Runtime
import qualified Domains.Security as Security


data NetworkEvent = EVNetworkDataReceived NetworkDataReceived | EVNetworkEventSourceMessageReceived NetworkEventSourceMessageReceived | EVNetworkLoadingFailed NetworkLoadingFailed | EVNetworkLoadingFinished NetworkLoadingFinished | EVNetworkRequestServedFromCache NetworkRequestServedFromCache | EVNetworkRequestWillBeSent NetworkRequestWillBeSent | EVNetworkResponseReceived NetworkResponseReceived | EVNetworkWebSocketClosed NetworkWebSocketClosed | EVNetworkWebSocketCreated NetworkWebSocketCreated | EVNetworkWebSocketFrameError NetworkWebSocketFrameError | EVNetworkWebSocketFrameReceived NetworkWebSocketFrameReceived | EVNetworkWebSocketFrameSent NetworkWebSocketFrameSent | EVNetworkWebSocketHandshakeResponseReceived NetworkWebSocketHandshakeResponseReceived | EVNetworkWebSocketWillSendHandshakeRequest NetworkWebSocketWillSendHandshakeRequest | EVNetworkWebTransportCreated NetworkWebTransportCreated | EVNetworkWebTransportConnectionEstablished NetworkWebTransportConnectionEstablished | EVNetworkWebTransportClosed NetworkWebTransportClosed
    deriving (Eq, Show, Read)

data NetworkDataReceived = NetworkDataReceived {
    networkDataReceivedRequestId :: NetworkRequestId,
    networkDataReceivedTimestamp :: NetworkMonotonicTime,
    networkDataReceivedDataLength :: Int,
    networkDataReceivedEncodedDataLength :: Int
} deriving (Eq, Show, Read)
instance FromJSON  NetworkDataReceived where
    parseJSON = A.withObject "NetworkDataReceived" $ \v ->
         NetworkDataReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "dataLength"
            <*> v  .:  "encodedDataLength"


instance ToJSON NetworkDataReceived  where
    toJSON v = A.object
        [ "requestId" .= networkDataReceivedRequestId v
        , "timestamp" .= networkDataReceivedTimestamp v
        , "dataLength" .= networkDataReceivedDataLength v
        , "encodedDataLength" .= networkDataReceivedEncodedDataLength v
        ]


instance FromEvent NetworkEvent NetworkDataReceived where
    eventName  _ _    =  "Network.dataReceived"
    fromEvent ev =  case ev of EVNetworkDataReceived v -> Just v; _ -> Nothing


data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
    networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
    networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
    networkEventSourceMessageReceivedEventName :: String,
    networkEventSourceMessageReceivedEventId :: String,
    networkEventSourceMessageReceivedData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkEventSourceMessageReceived where
    parseJSON = A.withObject "NetworkEventSourceMessageReceived" $ \v ->
         NetworkEventSourceMessageReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "eventName"
            <*> v  .:  "eventId"
            <*> v  .:  "data"


instance ToJSON NetworkEventSourceMessageReceived  where
    toJSON v = A.object
        [ "requestId" .= networkEventSourceMessageReceivedRequestId v
        , "timestamp" .= networkEventSourceMessageReceivedTimestamp v
        , "eventName" .= networkEventSourceMessageReceivedEventName v
        , "eventId" .= networkEventSourceMessageReceivedEventId v
        , "data" .= networkEventSourceMessageReceivedData v
        ]


instance FromEvent NetworkEvent NetworkEventSourceMessageReceived where
    eventName  _ _    =  "Network.eventSourceMessageReceived"
    fromEvent ev =  case ev of EVNetworkEventSourceMessageReceived v -> Just v; _ -> Nothing


data NetworkLoadingFailed = NetworkLoadingFailed {
    networkLoadingFailedRequestId :: NetworkRequestId,
    networkLoadingFailedTimestamp :: NetworkMonotonicTime,
    networkLoadingFailedType :: NetworkResourceType,
    networkLoadingFailedErrorText :: String,
    networkLoadingFailedCanceled :: Maybe Bool,
    networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
    networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
} deriving (Eq, Show, Read)
instance FromJSON  NetworkLoadingFailed where
    parseJSON = A.withObject "NetworkLoadingFailed" $ \v ->
         NetworkLoadingFailed <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "errorText"
            <*> v  .:?  "canceled"
            <*> v  .:?  "blockedReason"
            <*> v  .:?  "corsErrorStatus"


instance ToJSON NetworkLoadingFailed  where
    toJSON v = A.object
        [ "requestId" .= networkLoadingFailedRequestId v
        , "timestamp" .= networkLoadingFailedTimestamp v
        , "type" .= networkLoadingFailedType v
        , "errorText" .= networkLoadingFailedErrorText v
        , "canceled" .= networkLoadingFailedCanceled v
        , "blockedReason" .= networkLoadingFailedBlockedReason v
        , "corsErrorStatus" .= networkLoadingFailedCorsErrorStatus v
        ]


instance FromEvent NetworkEvent NetworkLoadingFailed where
    eventName  _ _    =  "Network.loadingFailed"
    fromEvent ev =  case ev of EVNetworkLoadingFailed v -> Just v; _ -> Nothing


data NetworkLoadingFinished = NetworkLoadingFinished {
    networkLoadingFinishedRequestId :: NetworkRequestId,
    networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
    networkLoadingFinishedEncodedDataLength :: Int,
    networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkLoadingFinished where
    parseJSON = A.withObject "NetworkLoadingFinished" $ \v ->
         NetworkLoadingFinished <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "encodedDataLength"
            <*> v  .:?  "shouldReportCorbBlocking"


instance ToJSON NetworkLoadingFinished  where
    toJSON v = A.object
        [ "requestId" .= networkLoadingFinishedRequestId v
        , "timestamp" .= networkLoadingFinishedTimestamp v
        , "encodedDataLength" .= networkLoadingFinishedEncodedDataLength v
        , "shouldReportCorbBlocking" .= networkLoadingFinishedShouldReportCorbBlocking v
        ]


instance FromEvent NetworkEvent NetworkLoadingFinished where
    eventName  _ _    =  "Network.loadingFinished"
    fromEvent ev =  case ev of EVNetworkLoadingFinished v -> Just v; _ -> Nothing


data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
    networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequestServedFromCache where
    parseJSON = A.withObject "NetworkRequestServedFromCache" $ \v ->
         NetworkRequestServedFromCache <$> v .:  "requestId"


instance ToJSON NetworkRequestServedFromCache  where
    toJSON v = A.object
        [ "requestId" .= networkRequestServedFromCacheRequestId v
        ]


instance FromEvent NetworkEvent NetworkRequestServedFromCache where
    eventName  _ _    =  "Network.requestServedFromCache"
    fromEvent ev =  case ev of EVNetworkRequestServedFromCache v -> Just v; _ -> Nothing


data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
    networkRequestWillBeSentRequestId :: NetworkRequestId,
    networkRequestWillBeSentLoaderId :: NetworkLoaderId,
    networkRequestWillBeSentDocumentUrl :: String,
    networkRequestWillBeSentRequest :: NetworkRequest,
    networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
    networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
    networkRequestWillBeSentInitiator :: NetworkInitiator,
    networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
    networkRequestWillBeSentType :: Maybe NetworkResourceType,
    networkRequestWillBeSentFrameId :: Maybe PageFrameId,
    networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequestWillBeSent where
    parseJSON = A.withObject "NetworkRequestWillBeSent" $ \v ->
         NetworkRequestWillBeSent <$> v .:  "requestId"
            <*> v  .:  "loaderId"
            <*> v  .:  "documentURL"
            <*> v  .:  "request"
            <*> v  .:  "timestamp"
            <*> v  .:  "wallTime"
            <*> v  .:  "initiator"
            <*> v  .:?  "redirectResponse"
            <*> v  .:?  "type"
            <*> v  .:?  "frameId"
            <*> v  .:?  "hasUserGesture"


instance ToJSON NetworkRequestWillBeSent  where
    toJSON v = A.object
        [ "requestId" .= networkRequestWillBeSentRequestId v
        , "loaderId" .= networkRequestWillBeSentLoaderId v
        , "documentURL" .= networkRequestWillBeSentDocumentUrl v
        , "request" .= networkRequestWillBeSentRequest v
        , "timestamp" .= networkRequestWillBeSentTimestamp v
        , "wallTime" .= networkRequestWillBeSentWallTime v
        , "initiator" .= networkRequestWillBeSentInitiator v
        , "redirectResponse" .= networkRequestWillBeSentRedirectResponse v
        , "type" .= networkRequestWillBeSentType v
        , "frameId" .= networkRequestWillBeSentFrameId v
        , "hasUserGesture" .= networkRequestWillBeSentHasUserGesture v
        ]


instance FromEvent NetworkEvent NetworkRequestWillBeSent where
    eventName  _ _    =  "Network.requestWillBeSent"
    fromEvent ev =  case ev of EVNetworkRequestWillBeSent v -> Just v; _ -> Nothing


data NetworkResponseReceived = NetworkResponseReceived {
    networkResponseReceivedRequestId :: NetworkRequestId,
    networkResponseReceivedLoaderId :: NetworkLoaderId,
    networkResponseReceivedTimestamp :: NetworkMonotonicTime,
    networkResponseReceivedType :: NetworkResourceType,
    networkResponseReceivedResponse :: NetworkResponse,
    networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResponseReceived where
    parseJSON = A.withObject "NetworkResponseReceived" $ \v ->
         NetworkResponseReceived <$> v .:  "requestId"
            <*> v  .:  "loaderId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "response"
            <*> v  .:?  "frameId"


instance ToJSON NetworkResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= networkResponseReceivedRequestId v
        , "loaderId" .= networkResponseReceivedLoaderId v
        , "timestamp" .= networkResponseReceivedTimestamp v
        , "type" .= networkResponseReceivedType v
        , "response" .= networkResponseReceivedResponse v
        , "frameId" .= networkResponseReceivedFrameId v
        ]


instance FromEvent NetworkEvent NetworkResponseReceived where
    eventName  _ _    =  "Network.responseReceived"
    fromEvent ev =  case ev of EVNetworkResponseReceived v -> Just v; _ -> Nothing


data NetworkWebSocketClosed = NetworkWebSocketClosed {
    networkWebSocketClosedRequestId :: NetworkRequestId,
    networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketClosed where
    parseJSON = A.withObject "NetworkWebSocketClosed" $ \v ->
         NetworkWebSocketClosed <$> v .:  "requestId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebSocketClosed  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketClosedRequestId v
        , "timestamp" .= networkWebSocketClosedTimestamp v
        ]


instance FromEvent NetworkEvent NetworkWebSocketClosed where
    eventName  _ _    =  "Network.webSocketClosed"
    fromEvent ev =  case ev of EVNetworkWebSocketClosed v -> Just v; _ -> Nothing


data NetworkWebSocketCreated = NetworkWebSocketCreated {
    networkWebSocketCreatedRequestId :: NetworkRequestId,
    networkWebSocketCreatedUrl :: String,
    networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketCreated where
    parseJSON = A.withObject "NetworkWebSocketCreated" $ \v ->
         NetworkWebSocketCreated <$> v .:  "requestId"
            <*> v  .:  "url"
            <*> v  .:?  "initiator"


instance ToJSON NetworkWebSocketCreated  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketCreatedRequestId v
        , "url" .= networkWebSocketCreatedUrl v
        , "initiator" .= networkWebSocketCreatedInitiator v
        ]


instance FromEvent NetworkEvent NetworkWebSocketCreated where
    eventName  _ _    =  "Network.webSocketCreated"
    fromEvent ev =  case ev of EVNetworkWebSocketCreated v -> Just v; _ -> Nothing


data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
    networkWebSocketFrameErrorRequestId :: NetworkRequestId,
    networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameErrorErrorMessage :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameError where
    parseJSON = A.withObject "NetworkWebSocketFrameError" $ \v ->
         NetworkWebSocketFrameError <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "errorMessage"


instance ToJSON NetworkWebSocketFrameError  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameErrorRequestId v
        , "timestamp" .= networkWebSocketFrameErrorTimestamp v
        , "errorMessage" .= networkWebSocketFrameErrorErrorMessage v
        ]


instance FromEvent NetworkEvent NetworkWebSocketFrameError where
    eventName  _ _    =  "Network.webSocketFrameError"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameError v -> Just v; _ -> Nothing


data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
    networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
    networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameReceived where
    parseJSON = A.withObject "NetworkWebSocketFrameReceived" $ \v ->
         NetworkWebSocketFrameReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketFrameReceived  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameReceivedRequestId v
        , "timestamp" .= networkWebSocketFrameReceivedTimestamp v
        , "response" .= networkWebSocketFrameReceivedResponse v
        ]


instance FromEvent NetworkEvent NetworkWebSocketFrameReceived where
    eventName  _ _    =  "Network.webSocketFrameReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameReceived v -> Just v; _ -> Nothing


data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
    networkWebSocketFrameSentRequestId :: NetworkRequestId,
    networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
    networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrameSent where
    parseJSON = A.withObject "NetworkWebSocketFrameSent" $ \v ->
         NetworkWebSocketFrameSent <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketFrameSent  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketFrameSentRequestId v
        , "timestamp" .= networkWebSocketFrameSentTimestamp v
        , "response" .= networkWebSocketFrameSentResponse v
        ]


instance FromEvent NetworkEvent NetworkWebSocketFrameSent where
    eventName  _ _    =  "Network.webSocketFrameSent"
    fromEvent ev =  case ev of EVNetworkWebSocketFrameSent v -> Just v; _ -> Nothing


data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
    networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
    networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
    networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
    parseJSON = A.withObject "NetworkWebSocketHandshakeResponseReceived" $ \v ->
         NetworkWebSocketHandshakeResponseReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketHandshakeResponseReceivedRequestId v
        , "timestamp" .= networkWebSocketHandshakeResponseReceivedTimestamp v
        , "response" .= networkWebSocketHandshakeResponseReceivedResponse v
        ]


instance FromEvent NetworkEvent NetworkWebSocketHandshakeResponseReceived where
    eventName  _ _    =  "Network.webSocketHandshakeResponseReceived"
    fromEvent ev =  case ev of EVNetworkWebSocketHandshakeResponseReceived v -> Just v; _ -> Nothing


data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
    networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
    networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
    networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
    networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
    parseJSON = A.withObject "NetworkWebSocketWillSendHandshakeRequest" $ \v ->
         NetworkWebSocketWillSendHandshakeRequest <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "wallTime"
            <*> v  .:  "request"


instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
    toJSON v = A.object
        [ "requestId" .= networkWebSocketWillSendHandshakeRequestRequestId v
        , "timestamp" .= networkWebSocketWillSendHandshakeRequestTimestamp v
        , "wallTime" .= networkWebSocketWillSendHandshakeRequestWallTime v
        , "request" .= networkWebSocketWillSendHandshakeRequestRequest v
        ]


instance FromEvent NetworkEvent NetworkWebSocketWillSendHandshakeRequest where
    eventName  _ _    =  "Network.webSocketWillSendHandshakeRequest"
    fromEvent ev =  case ev of EVNetworkWebSocketWillSendHandshakeRequest v -> Just v; _ -> Nothing


data NetworkWebTransportCreated = NetworkWebTransportCreated {
    networkWebTransportCreatedTransportId :: NetworkRequestId,
    networkWebTransportCreatedUrl :: String,
    networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
    networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportCreated where
    parseJSON = A.withObject "NetworkWebTransportCreated" $ \v ->
         NetworkWebTransportCreated <$> v .:  "transportId"
            <*> v  .:  "url"
            <*> v  .:  "timestamp"
            <*> v  .:?  "initiator"


instance ToJSON NetworkWebTransportCreated  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportCreatedTransportId v
        , "url" .= networkWebTransportCreatedUrl v
        , "timestamp" .= networkWebTransportCreatedTimestamp v
        , "initiator" .= networkWebTransportCreatedInitiator v
        ]


instance FromEvent NetworkEvent NetworkWebTransportCreated where
    eventName  _ _    =  "Network.webTransportCreated"
    fromEvent ev =  case ev of EVNetworkWebTransportCreated v -> Just v; _ -> Nothing


data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
    networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
    networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportConnectionEstablished where
    parseJSON = A.withObject "NetworkWebTransportConnectionEstablished" $ \v ->
         NetworkWebTransportConnectionEstablished <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebTransportConnectionEstablished  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportConnectionEstablishedTransportId v
        , "timestamp" .= networkWebTransportConnectionEstablishedTimestamp v
        ]


instance FromEvent NetworkEvent NetworkWebTransportConnectionEstablished where
    eventName  _ _    =  "Network.webTransportConnectionEstablished"
    fromEvent ev =  case ev of EVNetworkWebTransportConnectionEstablished v -> Just v; _ -> Nothing


data NetworkWebTransportClosed = NetworkWebTransportClosed {
    networkWebTransportClosedTransportId :: NetworkRequestId,
    networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebTransportClosed where
    parseJSON = A.withObject "NetworkWebTransportClosed" $ \v ->
         NetworkWebTransportClosed <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON NetworkWebTransportClosed  where
    toJSON v = A.object
        [ "transportId" .= networkWebTransportClosedTransportId v
        , "timestamp" .= networkWebTransportClosedTimestamp v
        ]


instance FromEvent NetworkEvent NetworkWebTransportClosed where
    eventName  _ _    =  "Network.webTransportClosed"
    fromEvent ev =  case ev of EVNetworkWebTransportClosed v -> Just v; _ -> Nothing




subscribe :: forall a. FromEvent NetworkEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy NetworkEvent
    pa       = Proxy :: Proxy a


data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
    deriving (Eq, Show, Read)
instance FromJSON NetworkResourceType where
    parseJSON = A.withText  "NetworkResourceType"  $ \v -> do
        case v of
                "Document" -> pure $ NetworkResourceTypeDocument
                "Stylesheet" -> pure $ NetworkResourceTypeStylesheet
                "Image" -> pure $ NetworkResourceTypeImage
                "Media" -> pure $ NetworkResourceTypeMedia
                "Font" -> pure $ NetworkResourceTypeFont
                "Script" -> pure $ NetworkResourceTypeScript
                "TextTrack" -> pure $ NetworkResourceTypeTextTrack
                "XHR" -> pure $ NetworkResourceTypeXhr
                "Fetch" -> pure $ NetworkResourceTypeFetch
                "EventSource" -> pure $ NetworkResourceTypeEventSource
                "WebSocket" -> pure $ NetworkResourceTypeWebSocket
                "Manifest" -> pure $ NetworkResourceTypeManifest
                "SignedExchange" -> pure $ NetworkResourceTypeSignedExchange
                "Ping" -> pure $ NetworkResourceTypePing
                "CSPViolationReport" -> pure $ NetworkResourceTypeCspViolationReport
                "Preflight" -> pure $ NetworkResourceTypePreflight
                "Other" -> pure $ NetworkResourceTypeOther
                _ -> fail "failed to parse NetworkResourceType"

instance ToJSON NetworkResourceType where
    toJSON v = A.String $
        case v of
                NetworkResourceTypeDocument -> "Document"
                NetworkResourceTypeStylesheet -> "Stylesheet"
                NetworkResourceTypeImage -> "Image"
                NetworkResourceTypeMedia -> "Media"
                NetworkResourceTypeFont -> "Font"
                NetworkResourceTypeScript -> "Script"
                NetworkResourceTypeTextTrack -> "TextTrack"
                NetworkResourceTypeXhr -> "XHR"
                NetworkResourceTypeFetch -> "Fetch"
                NetworkResourceTypeEventSource -> "EventSource"
                NetworkResourceTypeWebSocket -> "WebSocket"
                NetworkResourceTypeManifest -> "Manifest"
                NetworkResourceTypeSignedExchange -> "SignedExchange"
                NetworkResourceTypePing -> "Ping"
                NetworkResourceTypeCspViolationReport -> "CSPViolationReport"
                NetworkResourceTypePreflight -> "Preflight"
                NetworkResourceTypeOther -> "Other"



type NetworkLoaderId = String

type NetworkRequestId = String

type NetworkInterceptionId = String

data NetworkErrorReason = NetworkErrorReasonFailed | NetworkErrorReasonAborted | NetworkErrorReasonTimedOut | NetworkErrorReasonAccessDenied | NetworkErrorReasonConnectionClosed | NetworkErrorReasonConnectionReset | NetworkErrorReasonConnectionRefused | NetworkErrorReasonConnectionAborted | NetworkErrorReasonConnectionFailed | NetworkErrorReasonNameNotResolved | NetworkErrorReasonInternetDisconnected | NetworkErrorReasonAddressUnreachable | NetworkErrorReasonBlockedByClient | NetworkErrorReasonBlockedByResponse
    deriving (Eq, Show, Read)
instance FromJSON NetworkErrorReason where
    parseJSON = A.withText  "NetworkErrorReason"  $ \v -> do
        case v of
                "Failed" -> pure $ NetworkErrorReasonFailed
                "Aborted" -> pure $ NetworkErrorReasonAborted
                "TimedOut" -> pure $ NetworkErrorReasonTimedOut
                "AccessDenied" -> pure $ NetworkErrorReasonAccessDenied
                "ConnectionClosed" -> pure $ NetworkErrorReasonConnectionClosed
                "ConnectionReset" -> pure $ NetworkErrorReasonConnectionReset
                "ConnectionRefused" -> pure $ NetworkErrorReasonConnectionRefused
                "ConnectionAborted" -> pure $ NetworkErrorReasonConnectionAborted
                "ConnectionFailed" -> pure $ NetworkErrorReasonConnectionFailed
                "NameNotResolved" -> pure $ NetworkErrorReasonNameNotResolved
                "InternetDisconnected" -> pure $ NetworkErrorReasonInternetDisconnected
                "AddressUnreachable" -> pure $ NetworkErrorReasonAddressUnreachable
                "BlockedByClient" -> pure $ NetworkErrorReasonBlockedByClient
                "BlockedByResponse" -> pure $ NetworkErrorReasonBlockedByResponse
                _ -> fail "failed to parse NetworkErrorReason"

instance ToJSON NetworkErrorReason where
    toJSON v = A.String $
        case v of
                NetworkErrorReasonFailed -> "Failed"
                NetworkErrorReasonAborted -> "Aborted"
                NetworkErrorReasonTimedOut -> "TimedOut"
                NetworkErrorReasonAccessDenied -> "AccessDenied"
                NetworkErrorReasonConnectionClosed -> "ConnectionClosed"
                NetworkErrorReasonConnectionReset -> "ConnectionReset"
                NetworkErrorReasonConnectionRefused -> "ConnectionRefused"
                NetworkErrorReasonConnectionAborted -> "ConnectionAborted"
                NetworkErrorReasonConnectionFailed -> "ConnectionFailed"
                NetworkErrorReasonNameNotResolved -> "NameNotResolved"
                NetworkErrorReasonInternetDisconnected -> "InternetDisconnected"
                NetworkErrorReasonAddressUnreachable -> "AddressUnreachable"
                NetworkErrorReasonBlockedByClient -> "BlockedByClient"
                NetworkErrorReasonBlockedByResponse -> "BlockedByResponse"



type NetworkTimeSinceEpoch = Int

type NetworkMonotonicTime = Int

type NetworkHeaders = [(String, String)]

data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
    deriving (Eq, Show, Read)
instance FromJSON NetworkConnectionType where
    parseJSON = A.withText  "NetworkConnectionType"  $ \v -> do
        case v of
                "none" -> pure $ NetworkConnectionTypeNone
                "cellular2g" -> pure $ NetworkConnectionTypeCellular2g
                "cellular3g" -> pure $ NetworkConnectionTypeCellular3g
                "cellular4g" -> pure $ NetworkConnectionTypeCellular4g
                "bluetooth" -> pure $ NetworkConnectionTypeBluetooth
                "ethernet" -> pure $ NetworkConnectionTypeEthernet
                "wifi" -> pure $ NetworkConnectionTypeWifi
                "wimax" -> pure $ NetworkConnectionTypeWimax
                "other" -> pure $ NetworkConnectionTypeOther
                _ -> fail "failed to parse NetworkConnectionType"

instance ToJSON NetworkConnectionType where
    toJSON v = A.String $
        case v of
                NetworkConnectionTypeNone -> "none"
                NetworkConnectionTypeCellular2g -> "cellular2g"
                NetworkConnectionTypeCellular3g -> "cellular3g"
                NetworkConnectionTypeCellular4g -> "cellular4g"
                NetworkConnectionTypeBluetooth -> "bluetooth"
                NetworkConnectionTypeEthernet -> "ethernet"
                NetworkConnectionTypeWifi -> "wifi"
                NetworkConnectionTypeWimax -> "wimax"
                NetworkConnectionTypeOther -> "other"



data NetworkCookieSameSite = NetworkCookieSameSiteStrict | NetworkCookieSameSiteLax | NetworkCookieSameSiteNone
    deriving (Eq, Show, Read)
instance FromJSON NetworkCookieSameSite where
    parseJSON = A.withText  "NetworkCookieSameSite"  $ \v -> do
        case v of
                "Strict" -> pure $ NetworkCookieSameSiteStrict
                "Lax" -> pure $ NetworkCookieSameSiteLax
                "None" -> pure $ NetworkCookieSameSiteNone
                _ -> fail "failed to parse NetworkCookieSameSite"

instance ToJSON NetworkCookieSameSite where
    toJSON v = A.String $
        case v of
                NetworkCookieSameSiteStrict -> "Strict"
                NetworkCookieSameSiteLax -> "Lax"
                NetworkCookieSameSiteNone -> "None"



data NetworkResourceTiming = NetworkResourceTiming {
    networkResourceTimingRequestTime :: Int,
    networkResourceTimingProxyStart :: Int,
    networkResourceTimingProxyEnd :: Int,
    networkResourceTimingDnsStart :: Int,
    networkResourceTimingDnsEnd :: Int,
    networkResourceTimingConnectStart :: Int,
    networkResourceTimingConnectEnd :: Int,
    networkResourceTimingSslStart :: Int,
    networkResourceTimingSslEnd :: Int,
    networkResourceTimingSendStart :: Int,
    networkResourceTimingSendEnd :: Int,
    networkResourceTimingReceiveHeadersEnd :: Int
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResourceTiming where
    parseJSON = A.withObject "NetworkResourceTiming" $ \v ->
         NetworkResourceTiming <$> v .:  "requestTime"
            <*> v  .:  "proxyStart"
            <*> v  .:  "proxyEnd"
            <*> v  .:  "dnsStart"
            <*> v  .:  "dnsEnd"
            <*> v  .:  "connectStart"
            <*> v  .:  "connectEnd"
            <*> v  .:  "sslStart"
            <*> v  .:  "sslEnd"
            <*> v  .:  "sendStart"
            <*> v  .:  "sendEnd"
            <*> v  .:  "receiveHeadersEnd"


instance ToJSON NetworkResourceTiming  where
    toJSON v = A.object
        [ "requestTime" .= networkResourceTimingRequestTime v
        , "proxyStart" .= networkResourceTimingProxyStart v
        , "proxyEnd" .= networkResourceTimingProxyEnd v
        , "dnsStart" .= networkResourceTimingDnsStart v
        , "dnsEnd" .= networkResourceTimingDnsEnd v
        , "connectStart" .= networkResourceTimingConnectStart v
        , "connectEnd" .= networkResourceTimingConnectEnd v
        , "sslStart" .= networkResourceTimingSslStart v
        , "sslEnd" .= networkResourceTimingSslEnd v
        , "sendStart" .= networkResourceTimingSendStart v
        , "sendEnd" .= networkResourceTimingSendEnd v
        , "receiveHeadersEnd" .= networkResourceTimingReceiveHeadersEnd v
        ]



data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
    deriving (Eq, Show, Read)
instance FromJSON NetworkResourcePriority where
    parseJSON = A.withText  "NetworkResourcePriority"  $ \v -> do
        case v of
                "VeryLow" -> pure $ NetworkResourcePriorityVeryLow
                "Low" -> pure $ NetworkResourcePriorityLow
                "Medium" -> pure $ NetworkResourcePriorityMedium
                "High" -> pure $ NetworkResourcePriorityHigh
                "VeryHigh" -> pure $ NetworkResourcePriorityVeryHigh
                _ -> fail "failed to parse NetworkResourcePriority"

instance ToJSON NetworkResourcePriority where
    toJSON v = A.String $
        case v of
                NetworkResourcePriorityVeryLow -> "VeryLow"
                NetworkResourcePriorityLow -> "Low"
                NetworkResourcePriorityMedium -> "Medium"
                NetworkResourcePriorityHigh -> "High"
                NetworkResourcePriorityVeryHigh -> "VeryHigh"



data NetworkPostDataEntry = NetworkPostDataEntry {
    networkPostDataEntryBytes :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkPostDataEntry where
    parseJSON = A.withObject "NetworkPostDataEntry" $ \v ->
         NetworkPostDataEntry <$> v .:?  "bytes"


instance ToJSON NetworkPostDataEntry  where
    toJSON v = A.object
        [ "bytes" .= networkPostDataEntryBytes v
        ]



data NetworkRequest = NetworkRequest {
    networkRequestUrl :: String,
    networkRequestMethod :: String,
    networkRequestHeaders :: NetworkHeaders,
    networkRequestInitialPriority :: NetworkResourcePriority,
    networkRequestReferrerPolicy :: String,
    networkRequestUrlFragment :: Maybe String,
    networkRequestPostData :: Maybe String,
    networkRequestHasPostData :: Maybe Bool,
    networkRequestMixedContentType :: Maybe SecurityMixedContentType,
    networkRequestIsLinkPreload :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkRequest where
    parseJSON = A.withObject "NetworkRequest" $ \v ->
         NetworkRequest <$> v .:  "url"
            <*> v  .:  "method"
            <*> v  .:  "headers"
            <*> v  .:  "initialPriority"
            <*> v  .:  "referrerPolicy"
            <*> v  .:?  "urlFragment"
            <*> v  .:?  "postData"
            <*> v  .:?  "hasPostData"
            <*> v  .:?  "mixedContentType"
            <*> v  .:?  "isLinkPreload"


instance ToJSON NetworkRequest  where
    toJSON v = A.object
        [ "url" .= networkRequestUrl v
        , "method" .= networkRequestMethod v
        , "headers" .= networkRequestHeaders v
        , "initialPriority" .= networkRequestInitialPriority v
        , "referrerPolicy" .= networkRequestReferrerPolicy v
        , "urlFragment" .= networkRequestUrlFragment v
        , "postData" .= networkRequestPostData v
        , "hasPostData" .= networkRequestHasPostData v
        , "mixedContentType" .= networkRequestMixedContentType v
        , "isLinkPreload" .= networkRequestIsLinkPreload v
        ]



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
    networkSignedCertificateTimestampStatus :: String,
    networkSignedCertificateTimestampOrigin :: String,
    networkSignedCertificateTimestampLogDescription :: String,
    networkSignedCertificateTimestampLogId :: String,
    networkSignedCertificateTimestampTimestamp :: Int,
    networkSignedCertificateTimestampHashAlgorithm :: String,
    networkSignedCertificateTimestampSignatureAlgorithm :: String,
    networkSignedCertificateTimestampSignatureData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkSignedCertificateTimestamp where
    parseJSON = A.withObject "NetworkSignedCertificateTimestamp" $ \v ->
         NetworkSignedCertificateTimestamp <$> v .:  "status"
            <*> v  .:  "origin"
            <*> v  .:  "logDescription"
            <*> v  .:  "logId"
            <*> v  .:  "timestamp"
            <*> v  .:  "hashAlgorithm"
            <*> v  .:  "signatureAlgorithm"
            <*> v  .:  "signatureData"


instance ToJSON NetworkSignedCertificateTimestamp  where
    toJSON v = A.object
        [ "status" .= networkSignedCertificateTimestampStatus v
        , "origin" .= networkSignedCertificateTimestampOrigin v
        , "logDescription" .= networkSignedCertificateTimestampLogDescription v
        , "logId" .= networkSignedCertificateTimestampLogId v
        , "timestamp" .= networkSignedCertificateTimestampTimestamp v
        , "hashAlgorithm" .= networkSignedCertificateTimestampHashAlgorithm v
        , "signatureAlgorithm" .= networkSignedCertificateTimestampSignatureAlgorithm v
        , "signatureData" .= networkSignedCertificateTimestampSignatureData v
        ]



data NetworkSecurityDetails = NetworkSecurityDetails {
    networkSecurityDetailsProtocol :: String,
    networkSecurityDetailsKeyExchange :: String,
    networkSecurityDetailsCipher :: String,
    networkSecurityDetailsCertificateId :: SecurityCertificateId,
    networkSecurityDetailsSubjectName :: String,
    networkSecurityDetailsSanList :: [String],
    networkSecurityDetailsIssuer :: String,
    networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
    networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
    networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
    networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance,
    networkSecurityDetailsKeyExchangeGroup :: Maybe String,
    networkSecurityDetailsMac :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkSecurityDetails where
    parseJSON = A.withObject "NetworkSecurityDetails" $ \v ->
         NetworkSecurityDetails <$> v .:  "protocol"
            <*> v  .:  "keyExchange"
            <*> v  .:  "cipher"
            <*> v  .:  "certificateId"
            <*> v  .:  "subjectName"
            <*> v  .:  "sanList"
            <*> v  .:  "issuer"
            <*> v  .:  "validFrom"
            <*> v  .:  "validTo"
            <*> v  .:  "signedCertificateTimestampList"
            <*> v  .:  "certificateTransparencyCompliance"
            <*> v  .:?  "keyExchangeGroup"
            <*> v  .:?  "mac"


instance ToJSON NetworkSecurityDetails  where
    toJSON v = A.object
        [ "protocol" .= networkSecurityDetailsProtocol v
        , "keyExchange" .= networkSecurityDetailsKeyExchange v
        , "cipher" .= networkSecurityDetailsCipher v
        , "certificateId" .= networkSecurityDetailsCertificateId v
        , "subjectName" .= networkSecurityDetailsSubjectName v
        , "sanList" .= networkSecurityDetailsSanList v
        , "issuer" .= networkSecurityDetailsIssuer v
        , "validFrom" .= networkSecurityDetailsValidFrom v
        , "validTo" .= networkSecurityDetailsValidTo v
        , "signedCertificateTimestampList" .= networkSecurityDetailsSignedCertificateTimestampList v
        , "certificateTransparencyCompliance" .= networkSecurityDetailsCertificateTransparencyCompliance v
        , "keyExchangeGroup" .= networkSecurityDetailsKeyExchangeGroup v
        , "mac" .= networkSecurityDetailsMac v
        ]



data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
    deriving (Eq, Show, Read)
instance FromJSON NetworkCertificateTransparencyCompliance where
    parseJSON = A.withText  "NetworkCertificateTransparencyCompliance"  $ \v -> do
        case v of
                "unknown" -> pure $ NetworkCertificateTransparencyComplianceUnknown
                "not-compliant" -> pure $ NetworkCertificateTransparencyComplianceNotCompliant
                "compliant" -> pure $ NetworkCertificateTransparencyComplianceCompliant
                _ -> fail "failed to parse NetworkCertificateTransparencyCompliance"

instance ToJSON NetworkCertificateTransparencyCompliance where
    toJSON v = A.String $
        case v of
                NetworkCertificateTransparencyComplianceUnknown -> "unknown"
                NetworkCertificateTransparencyComplianceNotCompliant -> "not-compliant"
                NetworkCertificateTransparencyComplianceCompliant -> "compliant"



data NetworkBlockedReason = NetworkBlockedReasonOther | NetworkBlockedReasonCsp | NetworkBlockedReasonMixedContent | NetworkBlockedReasonOrigin | NetworkBlockedReasonInspector | NetworkBlockedReasonSubresourceFilter | NetworkBlockedReasonContentType | NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader | NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | NetworkBlockedReasonCorpNotSameOrigin | NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | NetworkBlockedReasonCorpNotSameSite
    deriving (Eq, Show, Read)
instance FromJSON NetworkBlockedReason where
    parseJSON = A.withText  "NetworkBlockedReason"  $ \v -> do
        case v of
                "other" -> pure $ NetworkBlockedReasonOther
                "csp" -> pure $ NetworkBlockedReasonCsp
                "mixed-content" -> pure $ NetworkBlockedReasonMixedContent
                "origin" -> pure $ NetworkBlockedReasonOrigin
                "inspector" -> pure $ NetworkBlockedReasonInspector
                "subresource-filter" -> pure $ NetworkBlockedReasonSubresourceFilter
                "content-type" -> pure $ NetworkBlockedReasonContentType
                "coep-frame-resource-needs-coep-header" -> pure $ NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader
                "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> pure $ NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
                "corp-not-same-origin" -> pure $ NetworkBlockedReasonCorpNotSameOrigin
                "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> pure $ NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
                "corp-not-same-site" -> pure $ NetworkBlockedReasonCorpNotSameSite
                _ -> fail "failed to parse NetworkBlockedReason"

instance ToJSON NetworkBlockedReason where
    toJSON v = A.String $
        case v of
                NetworkBlockedReasonOther -> "other"
                NetworkBlockedReasonCsp -> "csp"
                NetworkBlockedReasonMixedContent -> "mixed-content"
                NetworkBlockedReasonOrigin -> "origin"
                NetworkBlockedReasonInspector -> "inspector"
                NetworkBlockedReasonSubresourceFilter -> "subresource-filter"
                NetworkBlockedReasonContentType -> "content-type"
                NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader -> "coep-frame-resource-needs-coep-header"
                NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage -> "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
                NetworkBlockedReasonCorpNotSameOrigin -> "corp-not-same-origin"
                NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
                NetworkBlockedReasonCorpNotSameSite -> "corp-not-same-site"



data NetworkCorsError = NetworkCorsErrorDisallowedByMode | NetworkCorsErrorInvalidResponse | NetworkCorsErrorWildcardOriginNotAllowed | NetworkCorsErrorMissingAllowOriginHeader | NetworkCorsErrorMultipleAllowOriginValues | NetworkCorsErrorInvalidAllowOriginValue | NetworkCorsErrorAllowOriginMismatch | NetworkCorsErrorInvalidAllowCredentials | NetworkCorsErrorCorsDisabledScheme | NetworkCorsErrorPreflightInvalidStatus | NetworkCorsErrorPreflightDisallowedRedirect | NetworkCorsErrorPreflightWildcardOriginNotAllowed | NetworkCorsErrorPreflightMissingAllowOriginHeader | NetworkCorsErrorPreflightMultipleAllowOriginValues | NetworkCorsErrorPreflightInvalidAllowOriginValue | NetworkCorsErrorPreflightAllowOriginMismatch | NetworkCorsErrorPreflightInvalidAllowCredentials | NetworkCorsErrorPreflightMissingAllowExternal | NetworkCorsErrorPreflightInvalidAllowExternal | NetworkCorsErrorPreflightMissingAllowPrivateNetwork | NetworkCorsErrorPreflightInvalidAllowPrivateNetwork | NetworkCorsErrorInvalidAllowMethodsPreflightResponse | NetworkCorsErrorInvalidAllowHeadersPreflightResponse | NetworkCorsErrorMethodDisallowedByPreflightResponse | NetworkCorsErrorHeaderDisallowedByPreflightResponse | NetworkCorsErrorRedirectContainsCredentials | NetworkCorsErrorInsecurePrivateNetwork | NetworkCorsErrorInvalidPrivateNetworkAccess | NetworkCorsErrorUnexpectedPrivateNetworkAccess | NetworkCorsErrorNoCorsRedirectModeNotFollow
    deriving (Eq, Show, Read)
instance FromJSON NetworkCorsError where
    parseJSON = A.withText  "NetworkCorsError"  $ \v -> do
        case v of
                "DisallowedByMode" -> pure $ NetworkCorsErrorDisallowedByMode
                "InvalidResponse" -> pure $ NetworkCorsErrorInvalidResponse
                "WildcardOriginNotAllowed" -> pure $ NetworkCorsErrorWildcardOriginNotAllowed
                "MissingAllowOriginHeader" -> pure $ NetworkCorsErrorMissingAllowOriginHeader
                "MultipleAllowOriginValues" -> pure $ NetworkCorsErrorMultipleAllowOriginValues
                "InvalidAllowOriginValue" -> pure $ NetworkCorsErrorInvalidAllowOriginValue
                "AllowOriginMismatch" -> pure $ NetworkCorsErrorAllowOriginMismatch
                "InvalidAllowCredentials" -> pure $ NetworkCorsErrorInvalidAllowCredentials
                "CorsDisabledScheme" -> pure $ NetworkCorsErrorCorsDisabledScheme
                "PreflightInvalidStatus" -> pure $ NetworkCorsErrorPreflightInvalidStatus
                "PreflightDisallowedRedirect" -> pure $ NetworkCorsErrorPreflightDisallowedRedirect
                "PreflightWildcardOriginNotAllowed" -> pure $ NetworkCorsErrorPreflightWildcardOriginNotAllowed
                "PreflightMissingAllowOriginHeader" -> pure $ NetworkCorsErrorPreflightMissingAllowOriginHeader
                "PreflightMultipleAllowOriginValues" -> pure $ NetworkCorsErrorPreflightMultipleAllowOriginValues
                "PreflightInvalidAllowOriginValue" -> pure $ NetworkCorsErrorPreflightInvalidAllowOriginValue
                "PreflightAllowOriginMismatch" -> pure $ NetworkCorsErrorPreflightAllowOriginMismatch
                "PreflightInvalidAllowCredentials" -> pure $ NetworkCorsErrorPreflightInvalidAllowCredentials
                "PreflightMissingAllowExternal" -> pure $ NetworkCorsErrorPreflightMissingAllowExternal
                "PreflightInvalidAllowExternal" -> pure $ NetworkCorsErrorPreflightInvalidAllowExternal
                "PreflightMissingAllowPrivateNetwork" -> pure $ NetworkCorsErrorPreflightMissingAllowPrivateNetwork
                "PreflightInvalidAllowPrivateNetwork" -> pure $ NetworkCorsErrorPreflightInvalidAllowPrivateNetwork
                "InvalidAllowMethodsPreflightResponse" -> pure $ NetworkCorsErrorInvalidAllowMethodsPreflightResponse
                "InvalidAllowHeadersPreflightResponse" -> pure $ NetworkCorsErrorInvalidAllowHeadersPreflightResponse
                "MethodDisallowedByPreflightResponse" -> pure $ NetworkCorsErrorMethodDisallowedByPreflightResponse
                "HeaderDisallowedByPreflightResponse" -> pure $ NetworkCorsErrorHeaderDisallowedByPreflightResponse
                "RedirectContainsCredentials" -> pure $ NetworkCorsErrorRedirectContainsCredentials
                "InsecurePrivateNetwork" -> pure $ NetworkCorsErrorInsecurePrivateNetwork
                "InvalidPrivateNetworkAccess" -> pure $ NetworkCorsErrorInvalidPrivateNetworkAccess
                "UnexpectedPrivateNetworkAccess" -> pure $ NetworkCorsErrorUnexpectedPrivateNetworkAccess
                "NoCorsRedirectModeNotFollow" -> pure $ NetworkCorsErrorNoCorsRedirectModeNotFollow
                _ -> fail "failed to parse NetworkCorsError"

instance ToJSON NetworkCorsError where
    toJSON v = A.String $
        case v of
                NetworkCorsErrorDisallowedByMode -> "DisallowedByMode"
                NetworkCorsErrorInvalidResponse -> "InvalidResponse"
                NetworkCorsErrorWildcardOriginNotAllowed -> "WildcardOriginNotAllowed"
                NetworkCorsErrorMissingAllowOriginHeader -> "MissingAllowOriginHeader"
                NetworkCorsErrorMultipleAllowOriginValues -> "MultipleAllowOriginValues"
                NetworkCorsErrorInvalidAllowOriginValue -> "InvalidAllowOriginValue"
                NetworkCorsErrorAllowOriginMismatch -> "AllowOriginMismatch"
                NetworkCorsErrorInvalidAllowCredentials -> "InvalidAllowCredentials"
                NetworkCorsErrorCorsDisabledScheme -> "CorsDisabledScheme"
                NetworkCorsErrorPreflightInvalidStatus -> "PreflightInvalidStatus"
                NetworkCorsErrorPreflightDisallowedRedirect -> "PreflightDisallowedRedirect"
                NetworkCorsErrorPreflightWildcardOriginNotAllowed -> "PreflightWildcardOriginNotAllowed"
                NetworkCorsErrorPreflightMissingAllowOriginHeader -> "PreflightMissingAllowOriginHeader"
                NetworkCorsErrorPreflightMultipleAllowOriginValues -> "PreflightMultipleAllowOriginValues"
                NetworkCorsErrorPreflightInvalidAllowOriginValue -> "PreflightInvalidAllowOriginValue"
                NetworkCorsErrorPreflightAllowOriginMismatch -> "PreflightAllowOriginMismatch"
                NetworkCorsErrorPreflightInvalidAllowCredentials -> "PreflightInvalidAllowCredentials"
                NetworkCorsErrorPreflightMissingAllowExternal -> "PreflightMissingAllowExternal"
                NetworkCorsErrorPreflightInvalidAllowExternal -> "PreflightInvalidAllowExternal"
                NetworkCorsErrorPreflightMissingAllowPrivateNetwork -> "PreflightMissingAllowPrivateNetwork"
                NetworkCorsErrorPreflightInvalidAllowPrivateNetwork -> "PreflightInvalidAllowPrivateNetwork"
                NetworkCorsErrorInvalidAllowMethodsPreflightResponse -> "InvalidAllowMethodsPreflightResponse"
                NetworkCorsErrorInvalidAllowHeadersPreflightResponse -> "InvalidAllowHeadersPreflightResponse"
                NetworkCorsErrorMethodDisallowedByPreflightResponse -> "MethodDisallowedByPreflightResponse"
                NetworkCorsErrorHeaderDisallowedByPreflightResponse -> "HeaderDisallowedByPreflightResponse"
                NetworkCorsErrorRedirectContainsCredentials -> "RedirectContainsCredentials"
                NetworkCorsErrorInsecurePrivateNetwork -> "InsecurePrivateNetwork"
                NetworkCorsErrorInvalidPrivateNetworkAccess -> "InvalidPrivateNetworkAccess"
                NetworkCorsErrorUnexpectedPrivateNetworkAccess -> "UnexpectedPrivateNetworkAccess"
                NetworkCorsErrorNoCorsRedirectModeNotFollow -> "NoCorsRedirectModeNotFollow"



data NetworkCorsErrorStatus = NetworkCorsErrorStatus {
    networkCorsErrorStatusCorsError :: NetworkCorsError,
    networkCorsErrorStatusFailedParameter :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCorsErrorStatus where
    parseJSON = A.withObject "NetworkCorsErrorStatus" $ \v ->
         NetworkCorsErrorStatus <$> v .:  "corsError"
            <*> v  .:  "failedParameter"


instance ToJSON NetworkCorsErrorStatus  where
    toJSON v = A.object
        [ "corsError" .= networkCorsErrorStatusCorsError v
        , "failedParameter" .= networkCorsErrorStatusFailedParameter v
        ]



data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
    deriving (Eq, Show, Read)
instance FromJSON NetworkServiceWorkerResponseSource where
    parseJSON = A.withText  "NetworkServiceWorkerResponseSource"  $ \v -> do
        case v of
                "cache-storage" -> pure $ NetworkServiceWorkerResponseSourceCacheStorage
                "http-cache" -> pure $ NetworkServiceWorkerResponseSourceHttpCache
                "fallback-code" -> pure $ NetworkServiceWorkerResponseSourceFallbackCode
                "network" -> pure $ NetworkServiceWorkerResponseSourceNetwork
                _ -> fail "failed to parse NetworkServiceWorkerResponseSource"

instance ToJSON NetworkServiceWorkerResponseSource where
    toJSON v = A.String $
        case v of
                NetworkServiceWorkerResponseSourceCacheStorage -> "cache-storage"
                NetworkServiceWorkerResponseSourceHttpCache -> "http-cache"
                NetworkServiceWorkerResponseSourceFallbackCode -> "fallback-code"
                NetworkServiceWorkerResponseSourceNetwork -> "network"



data NetworkResponse = NetworkResponse {
    networkResponseUrl :: String,
    networkResponseStatus :: Int,
    networkResponseStatusText :: String,
    networkResponseHeaders :: NetworkHeaders,
    networkResponseMimeType :: String,
    networkResponseConnectionReused :: Bool,
    networkResponseConnectionId :: Int,
    networkResponseEncodedDataLength :: Int,
    networkResponseSecurityState :: SecuritySecurityState,
    networkResponseRequestHeaders :: Maybe NetworkHeaders,
    networkResponseRemoteIpAddress :: Maybe String,
    networkResponseRemotePort :: Maybe Int,
    networkResponseFromDiskCache :: Maybe Bool,
    networkResponseFromServiceWorker :: Maybe Bool,
    networkResponseFromPrefetchCache :: Maybe Bool,
    networkResponseTiming :: Maybe NetworkResourceTiming,
    networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
    networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
    networkResponseCacheStorageCacheName :: Maybe String,
    networkResponseProtocol :: Maybe String,
    networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving (Eq, Show, Read)
instance FromJSON  NetworkResponse where
    parseJSON = A.withObject "NetworkResponse" $ \v ->
         NetworkResponse <$> v .:  "url"
            <*> v  .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:  "mimeType"
            <*> v  .:  "connectionReused"
            <*> v  .:  "connectionId"
            <*> v  .:  "encodedDataLength"
            <*> v  .:  "securityState"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "remoteIPAddress"
            <*> v  .:?  "remotePort"
            <*> v  .:?  "fromDiskCache"
            <*> v  .:?  "fromServiceWorker"
            <*> v  .:?  "fromPrefetchCache"
            <*> v  .:?  "timing"
            <*> v  .:?  "serviceWorkerResponseSource"
            <*> v  .:?  "responseTime"
            <*> v  .:?  "cacheStorageCacheName"
            <*> v  .:?  "protocol"
            <*> v  .:?  "securityDetails"


instance ToJSON NetworkResponse  where
    toJSON v = A.object
        [ "url" .= networkResponseUrl v
        , "status" .= networkResponseStatus v
        , "statusText" .= networkResponseStatusText v
        , "headers" .= networkResponseHeaders v
        , "mimeType" .= networkResponseMimeType v
        , "connectionReused" .= networkResponseConnectionReused v
        , "connectionId" .= networkResponseConnectionId v
        , "encodedDataLength" .= networkResponseEncodedDataLength v
        , "securityState" .= networkResponseSecurityState v
        , "requestHeaders" .= networkResponseRequestHeaders v
        , "remoteIPAddress" .= networkResponseRemoteIpAddress v
        , "remotePort" .= networkResponseRemotePort v
        , "fromDiskCache" .= networkResponseFromDiskCache v
        , "fromServiceWorker" .= networkResponseFromServiceWorker v
        , "fromPrefetchCache" .= networkResponseFromPrefetchCache v
        , "timing" .= networkResponseTiming v
        , "serviceWorkerResponseSource" .= networkResponseServiceWorkerResponseSource v
        , "responseTime" .= networkResponseResponseTime v
        , "cacheStorageCacheName" .= networkResponseCacheStorageCacheName v
        , "protocol" .= networkResponseProtocol v
        , "securityDetails" .= networkResponseSecurityDetails v
        ]



data NetworkWebSocketRequest = NetworkWebSocketRequest {
    networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketRequest where
    parseJSON = A.withObject "NetworkWebSocketRequest" $ \v ->
         NetworkWebSocketRequest <$> v .:  "headers"


instance ToJSON NetworkWebSocketRequest  where
    toJSON v = A.object
        [ "headers" .= networkWebSocketRequestHeaders v
        ]



data NetworkWebSocketResponse = NetworkWebSocketResponse {
    networkWebSocketResponseStatus :: Int,
    networkWebSocketResponseStatusText :: String,
    networkWebSocketResponseHeaders :: NetworkHeaders,
    networkWebSocketResponseHeadersText :: Maybe String,
    networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
    networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketResponse where
    parseJSON = A.withObject "NetworkWebSocketResponse" $ \v ->
         NetworkWebSocketResponse <$> v .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:?  "headersText"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "requestHeadersText"


instance ToJSON NetworkWebSocketResponse  where
    toJSON v = A.object
        [ "status" .= networkWebSocketResponseStatus v
        , "statusText" .= networkWebSocketResponseStatusText v
        , "headers" .= networkWebSocketResponseHeaders v
        , "headersText" .= networkWebSocketResponseHeadersText v
        , "requestHeaders" .= networkWebSocketResponseRequestHeaders v
        , "requestHeadersText" .= networkWebSocketResponseRequestHeadersText v
        ]



data NetworkWebSocketFrame = NetworkWebSocketFrame {
    networkWebSocketFrameOpcode :: Int,
    networkWebSocketFrameMask :: Bool,
    networkWebSocketFramePayloadData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkWebSocketFrame where
    parseJSON = A.withObject "NetworkWebSocketFrame" $ \v ->
         NetworkWebSocketFrame <$> v .:  "opcode"
            <*> v  .:  "mask"
            <*> v  .:  "payloadData"


instance ToJSON NetworkWebSocketFrame  where
    toJSON v = A.object
        [ "opcode" .= networkWebSocketFrameOpcode v
        , "mask" .= networkWebSocketFrameMask v
        , "payloadData" .= networkWebSocketFramePayloadData v
        ]



data NetworkCachedResource = NetworkCachedResource {
    networkCachedResourceUrl :: String,
    networkCachedResourceType :: NetworkResourceType,
    networkCachedResourceBodySize :: Int,
    networkCachedResourceResponse :: Maybe NetworkResponse
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCachedResource where
    parseJSON = A.withObject "NetworkCachedResource" $ \v ->
         NetworkCachedResource <$> v .:  "url"
            <*> v  .:  "type"
            <*> v  .:  "bodySize"
            <*> v  .:?  "response"


instance ToJSON NetworkCachedResource  where
    toJSON v = A.object
        [ "url" .= networkCachedResourceUrl v
        , "type" .= networkCachedResourceType v
        , "bodySize" .= networkCachedResourceBodySize v
        , "response" .= networkCachedResourceResponse v
        ]



data NetworkInitiator = NetworkInitiator {
    networkInitiatorType :: String,
    networkInitiatorStack :: Maybe RuntimeStackTrace,
    networkInitiatorUrl :: Maybe String,
    networkInitiatorLineNumber :: Maybe Int,
    networkInitiatorColumnNumber :: Maybe Int,
    networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  NetworkInitiator where
    parseJSON = A.withObject "NetworkInitiator" $ \v ->
         NetworkInitiator <$> v .:  "type"
            <*> v  .:?  "stack"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "requestId"


instance ToJSON NetworkInitiator  where
    toJSON v = A.object
        [ "type" .= networkInitiatorType v
        , "stack" .= networkInitiatorStack v
        , "url" .= networkInitiatorUrl v
        , "lineNumber" .= networkInitiatorLineNumber v
        , "columnNumber" .= networkInitiatorColumnNumber v
        , "requestId" .= networkInitiatorRequestId v
        ]



data NetworkCookie = NetworkCookie {
    networkCookieName :: String,
    networkCookieValue :: String,
    networkCookieDomain :: String,
    networkCookiePath :: String,
    networkCookieExpires :: Int,
    networkCookieSize :: Int,
    networkCookieHttpOnly :: Bool,
    networkCookieSecure :: Bool,
    networkCookieSession :: Bool,
    networkCookieSameSite :: Maybe NetworkCookieSameSite
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCookie where
    parseJSON = A.withObject "NetworkCookie" $ \v ->
         NetworkCookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:  "domain"
            <*> v  .:  "path"
            <*> v  .:  "expires"
            <*> v  .:  "size"
            <*> v  .:  "httpOnly"
            <*> v  .:  "secure"
            <*> v  .:  "session"
            <*> v  .:?  "sameSite"


instance ToJSON NetworkCookie  where
    toJSON v = A.object
        [ "name" .= networkCookieName v
        , "value" .= networkCookieValue v
        , "domain" .= networkCookieDomain v
        , "path" .= networkCookiePath v
        , "expires" .= networkCookieExpires v
        , "size" .= networkCookieSize v
        , "httpOnly" .= networkCookieHttpOnly v
        , "secure" .= networkCookieSecure v
        , "session" .= networkCookieSession v
        , "sameSite" .= networkCookieSameSite v
        ]



data NetworkCookieParam = NetworkCookieParam {
    networkCookieParamName :: String,
    networkCookieParamValue :: String,
    networkCookieParamUrl :: Maybe String,
    networkCookieParamDomain :: Maybe String,
    networkCookieParamPath :: Maybe String,
    networkCookieParamSecure :: Maybe Bool,
    networkCookieParamHttpOnly :: Maybe Bool,
    networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
    networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  NetworkCookieParam where
    parseJSON = A.withObject "NetworkCookieParam" $ \v ->
         NetworkCookieParam <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON NetworkCookieParam  where
    toJSON v = A.object
        [ "name" .= networkCookieParamName v
        , "value" .= networkCookieParamValue v
        , "url" .= networkCookieParamUrl v
        , "domain" .= networkCookieParamDomain v
        , "path" .= networkCookieParamPath v
        , "secure" .= networkCookieParamSecure v
        , "httpOnly" .= networkCookieParamHttpOnly v
        , "sameSite" .= networkCookieParamSameSite v
        , "expires" .= networkCookieParamExpires v
        ]






networkClearBrowserCache :: Session -> IO (Maybe Error)
networkClearBrowserCache session = sendReceiveCommand session "Network.clearBrowserCache" (Nothing :: Maybe ())




networkClearBrowserCookies :: Session -> IO (Maybe Error)
networkClearBrowserCookies session = sendReceiveCommand session "Network.clearBrowserCookies" (Nothing :: Maybe ())



data PNetworkDeleteCookies = PNetworkDeleteCookies {
    pNetworkDeleteCookiesName :: String,
    pNetworkDeleteCookiesUrl :: Maybe String,
    pNetworkDeleteCookiesDomain :: Maybe String,
    pNetworkDeleteCookiesPath :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkDeleteCookies where
    parseJSON = A.withObject "PNetworkDeleteCookies" $ \v ->
         PNetworkDeleteCookies <$> v .:  "name"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"


instance ToJSON PNetworkDeleteCookies  where
    toJSON v = A.object
        [ "name" .= pNetworkDeleteCookiesName v
        , "url" .= pNetworkDeleteCookiesUrl v
        , "domain" .= pNetworkDeleteCookiesDomain v
        , "path" .= pNetworkDeleteCookiesPath v
        ]


networkDeleteCookies :: Session -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies session params = sendReceiveCommand session "Network.deleteCookies" (Just params)




networkDisable :: Session -> IO (Maybe Error)
networkDisable session = sendReceiveCommand session "Network.disable" (Nothing :: Maybe ())



data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
    pNetworkEmulateNetworkConditionsOffline :: Bool,
    pNetworkEmulateNetworkConditionsLatency :: Int,
    pNetworkEmulateNetworkConditionsDownloadThroughput :: Int,
    pNetworkEmulateNetworkConditionsUploadThroughput :: Int,
    pNetworkEmulateNetworkConditionsConnectionType :: Maybe NetworkConnectionType
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkEmulateNetworkConditions where
    parseJSON = A.withObject "PNetworkEmulateNetworkConditions" $ \v ->
         PNetworkEmulateNetworkConditions <$> v .:  "offline"
            <*> v  .:  "latency"
            <*> v  .:  "downloadThroughput"
            <*> v  .:  "uploadThroughput"
            <*> v  .:?  "connectionType"


instance ToJSON PNetworkEmulateNetworkConditions  where
    toJSON v = A.object
        [ "offline" .= pNetworkEmulateNetworkConditionsOffline v
        , "latency" .= pNetworkEmulateNetworkConditionsLatency v
        , "downloadThroughput" .= pNetworkEmulateNetworkConditionsDownloadThroughput v
        , "uploadThroughput" .= pNetworkEmulateNetworkConditionsUploadThroughput v
        , "connectionType" .= pNetworkEmulateNetworkConditionsConnectionType v
        ]


networkEmulateNetworkConditions :: Session -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions session params = sendReceiveCommand session "Network.emulateNetworkConditions" (Just params)



data PNetworkEnable = PNetworkEnable {
    pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkEnable where
    parseJSON = A.withObject "PNetworkEnable" $ \v ->
         PNetworkEnable <$> v .:?  "maxPostDataSize"


instance ToJSON PNetworkEnable  where
    toJSON v = A.object
        [ "maxPostDataSize" .= pNetworkEnableMaxPostDataSize v
        ]


networkEnable :: Session -> PNetworkEnable -> IO (Maybe Error)
networkEnable session params = sendReceiveCommand session "Network.enable" (Just params)

data NetworkGetAllCookies = NetworkGetAllCookies {
    networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetAllCookies where
    parseJSON = A.withObject "NetworkGetAllCookies" $ \v ->
         NetworkGetAllCookies <$> v .:  "cookies"



instance Command  NetworkGetAllCookies where
    commandName _ = "Network.getAllCookies"


networkGetAllCookies :: Session -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies session = sendReceiveCommandResult session "Network.getAllCookies" (Nothing :: Maybe ())

data NetworkGetCookies = NetworkGetCookies {
    networkGetCookiesCookies :: [NetworkCookie]
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetCookies where
    parseJSON = A.withObject "NetworkGetCookies" $ \v ->
         NetworkGetCookies <$> v .:  "cookies"



instance Command  NetworkGetCookies where
    commandName _ = "Network.getCookies"

data PNetworkGetCookies = PNetworkGetCookies {
    pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetCookies where
    parseJSON = A.withObject "PNetworkGetCookies" $ \v ->
         PNetworkGetCookies <$> v .:?  "urls"


instance ToJSON PNetworkGetCookies  where
    toJSON v = A.object
        [ "urls" .= pNetworkGetCookiesUrls v
        ]


networkGetCookies :: Session -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies session params = sendReceiveCommandResult session "Network.getCookies" (Just params)

data NetworkGetResponseBody = NetworkGetResponseBody {
    networkGetResponseBodyBody :: String,
    networkGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetResponseBody where
    parseJSON = A.withObject "NetworkGetResponseBody" $ \v ->
         NetworkGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



instance Command  NetworkGetResponseBody where
    commandName _ = "Network.getResponseBody"

data PNetworkGetResponseBody = PNetworkGetResponseBody {
    pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetResponseBody where
    parseJSON = A.withObject "PNetworkGetResponseBody" $ \v ->
         PNetworkGetResponseBody <$> v .:  "requestId"


instance ToJSON PNetworkGetResponseBody  where
    toJSON v = A.object
        [ "requestId" .= pNetworkGetResponseBodyRequestId v
        ]


networkGetResponseBody :: Session -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody session params = sendReceiveCommandResult session "Network.getResponseBody" (Just params)

data NetworkGetRequestPostData = NetworkGetRequestPostData {
    networkGetRequestPostDataPostData :: String
} deriving (Eq, Show, Read)
instance FromJSON  NetworkGetRequestPostData where
    parseJSON = A.withObject "NetworkGetRequestPostData" $ \v ->
         NetworkGetRequestPostData <$> v .:  "postData"



instance Command  NetworkGetRequestPostData where
    commandName _ = "Network.getRequestPostData"

data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
    pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkGetRequestPostData where
    parseJSON = A.withObject "PNetworkGetRequestPostData" $ \v ->
         PNetworkGetRequestPostData <$> v .:  "requestId"


instance ToJSON PNetworkGetRequestPostData  where
    toJSON v = A.object
        [ "requestId" .= pNetworkGetRequestPostDataRequestId v
        ]


networkGetRequestPostData :: Session -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData session params = sendReceiveCommandResult session "Network.getRequestPostData" (Just params)



data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
    pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCacheDisabled where
    parseJSON = A.withObject "PNetworkSetCacheDisabled" $ \v ->
         PNetworkSetCacheDisabled <$> v .:  "cacheDisabled"


instance ToJSON PNetworkSetCacheDisabled  where
    toJSON v = A.object
        [ "cacheDisabled" .= pNetworkSetCacheDisabledCacheDisabled v
        ]


networkSetCacheDisabled :: Session -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled session params = sendReceiveCommand session "Network.setCacheDisabled" (Just params)



data PNetworkSetCookie = PNetworkSetCookie {
    pNetworkSetCookieName :: String,
    pNetworkSetCookieValue :: String,
    pNetworkSetCookieUrl :: Maybe String,
    pNetworkSetCookieDomain :: Maybe String,
    pNetworkSetCookiePath :: Maybe String,
    pNetworkSetCookieSecure :: Maybe Bool,
    pNetworkSetCookieHttpOnly :: Maybe Bool,
    pNetworkSetCookieSameSite :: Maybe NetworkCookieSameSite,
    pNetworkSetCookieExpires :: Maybe NetworkTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCookie where
    parseJSON = A.withObject "PNetworkSetCookie" $ \v ->
         PNetworkSetCookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON PNetworkSetCookie  where
    toJSON v = A.object
        [ "name" .= pNetworkSetCookieName v
        , "value" .= pNetworkSetCookieValue v
        , "url" .= pNetworkSetCookieUrl v
        , "domain" .= pNetworkSetCookieDomain v
        , "path" .= pNetworkSetCookiePath v
        , "secure" .= pNetworkSetCookieSecure v
        , "httpOnly" .= pNetworkSetCookieHttpOnly v
        , "sameSite" .= pNetworkSetCookieSameSite v
        , "expires" .= pNetworkSetCookieExpires v
        ]


networkSetCookie :: Session -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie session params = sendReceiveCommand session "Network.setCookie" (Just params)



data PNetworkSetCookies = PNetworkSetCookies {
    pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetCookies where
    parseJSON = A.withObject "PNetworkSetCookies" $ \v ->
         PNetworkSetCookies <$> v .:  "cookies"


instance ToJSON PNetworkSetCookies  where
    toJSON v = A.object
        [ "cookies" .= pNetworkSetCookiesCookies v
        ]


networkSetCookies :: Session -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies session params = sendReceiveCommand session "Network.setCookies" (Just params)



data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
    pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetExtraHttpHeaders where
    parseJSON = A.withObject "PNetworkSetExtraHttpHeaders" $ \v ->
         PNetworkSetExtraHttpHeaders <$> v .:  "headers"


instance ToJSON PNetworkSetExtraHttpHeaders  where
    toJSON v = A.object
        [ "headers" .= pNetworkSetExtraHttpHeadersHeaders v
        ]


networkSetExtraHttpHeaders :: Session -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders session params = sendReceiveCommand session "Network.setExtraHTTPHeaders" (Just params)



data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
    pNetworkSetUserAgentOverrideUserAgent :: String,
    pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
    pNetworkSetUserAgentOverridePlatform :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PNetworkSetUserAgentOverride where
    parseJSON = A.withObject "PNetworkSetUserAgentOverride" $ \v ->
         PNetworkSetUserAgentOverride <$> v .:  "userAgent"
            <*> v  .:?  "acceptLanguage"
            <*> v  .:?  "platform"


instance ToJSON PNetworkSetUserAgentOverride  where
    toJSON v = A.object
        [ "userAgent" .= pNetworkSetUserAgentOverrideUserAgent v
        , "acceptLanguage" .= pNetworkSetUserAgentOverrideAcceptLanguage v
        , "platform" .= pNetworkSetUserAgentOverridePlatform v
        ]


networkSetUserAgentOverride :: Session -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride session params = sendReceiveCommand session "Network.setUserAgentOverride" (Just params)

