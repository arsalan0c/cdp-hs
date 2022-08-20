{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Network (module Domains.Network) where
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

import qualified Domains.Debugger as Debugger
import qualified Domains.Runtime as Runtime
import qualified Domains.Security as Security


import Utils

data DataReceived = DataReceived {
    dataReceivedRequestId :: RequestId,
    dataReceivedTimestamp :: MonotonicTime,
    dataReceivedDataLength :: Int,
    dataReceivedEncodedDataLength :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  DataReceived where
    parseJSON = A.withObject "DataReceived" $ \v ->
         DataReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "dataLength"
            <*> v  .:  "encodedDataLength"


instance ToJSON DataReceived  where
    toJSON v = A.object
        [ "requestId" .= dataReceivedRequestId v
        , "timestamp" .= dataReceivedTimestamp v
        , "dataLength" .= dataReceivedDataLength v
        , "encodedDataLength" .= dataReceivedEncodedDataLength v
        ]


data EventSourceMessageReceived = EventSourceMessageReceived {
    eventSourceMessageReceivedRequestId :: RequestId,
    eventSourceMessageReceivedTimestamp :: MonotonicTime,
    eventSourceMessageReceivedEventName :: String,
    eventSourceMessageReceivedEventId :: String,
    eventSourceMessageReceivedData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  EventSourceMessageReceived where
    parseJSON = A.withObject "EventSourceMessageReceived" $ \v ->
         EventSourceMessageReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "eventName"
            <*> v  .:  "eventId"
            <*> v  .:  "data"


instance ToJSON EventSourceMessageReceived  where
    toJSON v = A.object
        [ "requestId" .= eventSourceMessageReceivedRequestId v
        , "timestamp" .= eventSourceMessageReceivedTimestamp v
        , "eventName" .= eventSourceMessageReceivedEventName v
        , "eventId" .= eventSourceMessageReceivedEventId v
        , "data" .= eventSourceMessageReceivedData v
        ]


data LoadingFailed = LoadingFailed {
    loadingFailedRequestId :: RequestId,
    loadingFailedTimestamp :: MonotonicTime,
    loadingFailedType :: ResourceType,
    loadingFailedErrorText :: String,
    loadingFailedCanceled :: Maybe Bool,
    loadingFailedBlockedReason :: Maybe BlockedReason,
    loadingFailedCorsErrorStatus :: Maybe CorsErrorStatus
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LoadingFailed where
    parseJSON = A.withObject "LoadingFailed" $ \v ->
         LoadingFailed <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "errorText"
            <*> v  .:?  "canceled"
            <*> v  .:?  "blockedReason"
            <*> v  .:?  "corsErrorStatus"


instance ToJSON LoadingFailed  where
    toJSON v = A.object
        [ "requestId" .= loadingFailedRequestId v
        , "timestamp" .= loadingFailedTimestamp v
        , "type" .= loadingFailedType v
        , "errorText" .= loadingFailedErrorText v
        , "canceled" .= loadingFailedCanceled v
        , "blockedReason" .= loadingFailedBlockedReason v
        , "corsErrorStatus" .= loadingFailedCorsErrorStatus v
        ]


data LoadingFinished = LoadingFinished {
    loadingFinishedRequestId :: RequestId,
    loadingFinishedTimestamp :: MonotonicTime,
    loadingFinishedEncodedDataLength :: Int,
    loadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  LoadingFinished where
    parseJSON = A.withObject "LoadingFinished" $ \v ->
         LoadingFinished <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "encodedDataLength"
            <*> v  .:?  "shouldReportCorbBlocking"


instance ToJSON LoadingFinished  where
    toJSON v = A.object
        [ "requestId" .= loadingFinishedRequestId v
        , "timestamp" .= loadingFinishedTimestamp v
        , "encodedDataLength" .= loadingFinishedEncodedDataLength v
        , "shouldReportCorbBlocking" .= loadingFinishedShouldReportCorbBlocking v
        ]


data RequestServedFromCache = RequestServedFromCache {
    requestServedFromCacheRequestId :: RequestId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RequestServedFromCache where
    parseJSON = A.withObject "RequestServedFromCache" $ \v ->
         RequestServedFromCache <$> v .:  "requestId"


instance ToJSON RequestServedFromCache  where
    toJSON v = A.object
        [ "requestId" .= requestServedFromCacheRequestId v
        ]


data RequestWillBeSent = RequestWillBeSent {
    requestWillBeSentRequestId :: RequestId,
    requestWillBeSentLoaderId :: LoaderId,
    requestWillBeSentDocumentUrl :: String,
    requestWillBeSentRequest :: Request,
    requestWillBeSentTimestamp :: MonotonicTime,
    requestWillBeSentWallTime :: TimeSinceEpoch,
    requestWillBeSentInitiator :: Initiator,
    requestWillBeSentRedirectResponse :: Maybe Response,
    requestWillBeSentType :: Maybe ResourceType,
    requestWillBeSentFrameId :: Maybe Page.FrameId,
    requestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RequestWillBeSent where
    parseJSON = A.withObject "RequestWillBeSent" $ \v ->
         RequestWillBeSent <$> v .:  "requestId"
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


instance ToJSON RequestWillBeSent  where
    toJSON v = A.object
        [ "requestId" .= requestWillBeSentRequestId v
        , "loaderId" .= requestWillBeSentLoaderId v
        , "documentURL" .= requestWillBeSentDocumentUrl v
        , "request" .= requestWillBeSentRequest v
        , "timestamp" .= requestWillBeSentTimestamp v
        , "wallTime" .= requestWillBeSentWallTime v
        , "initiator" .= requestWillBeSentInitiator v
        , "redirectResponse" .= requestWillBeSentRedirectResponse v
        , "type" .= requestWillBeSentType v
        , "frameId" .= requestWillBeSentFrameId v
        , "hasUserGesture" .= requestWillBeSentHasUserGesture v
        ]


data ResponseReceived = ResponseReceived {
    responseReceivedRequestId :: RequestId,
    responseReceivedLoaderId :: LoaderId,
    responseReceivedTimestamp :: MonotonicTime,
    responseReceivedType :: ResourceType,
    responseReceivedResponse :: Response,
    responseReceivedFrameId :: Maybe Page.FrameId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ResponseReceived where
    parseJSON = A.withObject "ResponseReceived" $ \v ->
         ResponseReceived <$> v .:  "requestId"
            <*> v  .:  "loaderId"
            <*> v  .:  "timestamp"
            <*> v  .:  "type"
            <*> v  .:  "response"
            <*> v  .:?  "frameId"


instance ToJSON ResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= responseReceivedRequestId v
        , "loaderId" .= responseReceivedLoaderId v
        , "timestamp" .= responseReceivedTimestamp v
        , "type" .= responseReceivedType v
        , "response" .= responseReceivedResponse v
        , "frameId" .= responseReceivedFrameId v
        ]


data WebSocketClosed = WebSocketClosed {
    webSocketClosedRequestId :: RequestId,
    webSocketClosedTimestamp :: MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketClosed where
    parseJSON = A.withObject "WebSocketClosed" $ \v ->
         WebSocketClosed <$> v .:  "requestId"
            <*> v  .:  "timestamp"


instance ToJSON WebSocketClosed  where
    toJSON v = A.object
        [ "requestId" .= webSocketClosedRequestId v
        , "timestamp" .= webSocketClosedTimestamp v
        ]


data WebSocketCreated = WebSocketCreated {
    webSocketCreatedRequestId :: RequestId,
    webSocketCreatedUrl :: String,
    webSocketCreatedInitiator :: Maybe Initiator
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketCreated where
    parseJSON = A.withObject "WebSocketCreated" $ \v ->
         WebSocketCreated <$> v .:  "requestId"
            <*> v  .:  "url"
            <*> v  .:?  "initiator"


instance ToJSON WebSocketCreated  where
    toJSON v = A.object
        [ "requestId" .= webSocketCreatedRequestId v
        , "url" .= webSocketCreatedUrl v
        , "initiator" .= webSocketCreatedInitiator v
        ]


data WebSocketFrameError = WebSocketFrameError {
    webSocketFrameErrorRequestId :: RequestId,
    webSocketFrameErrorTimestamp :: MonotonicTime,
    webSocketFrameErrorErrorMessage :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketFrameError where
    parseJSON = A.withObject "WebSocketFrameError" $ \v ->
         WebSocketFrameError <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "errorMessage"


instance ToJSON WebSocketFrameError  where
    toJSON v = A.object
        [ "requestId" .= webSocketFrameErrorRequestId v
        , "timestamp" .= webSocketFrameErrorTimestamp v
        , "errorMessage" .= webSocketFrameErrorErrorMessage v
        ]


data WebSocketFrameReceived = WebSocketFrameReceived {
    webSocketFrameReceivedRequestId :: RequestId,
    webSocketFrameReceivedTimestamp :: MonotonicTime,
    webSocketFrameReceivedResponse :: WebSocketFrame
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketFrameReceived where
    parseJSON = A.withObject "WebSocketFrameReceived" $ \v ->
         WebSocketFrameReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON WebSocketFrameReceived  where
    toJSON v = A.object
        [ "requestId" .= webSocketFrameReceivedRequestId v
        , "timestamp" .= webSocketFrameReceivedTimestamp v
        , "response" .= webSocketFrameReceivedResponse v
        ]


data WebSocketFrameSent = WebSocketFrameSent {
    webSocketFrameSentRequestId :: RequestId,
    webSocketFrameSentTimestamp :: MonotonicTime,
    webSocketFrameSentResponse :: WebSocketFrame
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketFrameSent where
    parseJSON = A.withObject "WebSocketFrameSent" $ \v ->
         WebSocketFrameSent <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON WebSocketFrameSent  where
    toJSON v = A.object
        [ "requestId" .= webSocketFrameSentRequestId v
        , "timestamp" .= webSocketFrameSentTimestamp v
        , "response" .= webSocketFrameSentResponse v
        ]


data WebSocketHandshakeResponseReceived = WebSocketHandshakeResponseReceived {
    webSocketHandshakeResponseReceivedRequestId :: RequestId,
    webSocketHandshakeResponseReceivedTimestamp :: MonotonicTime,
    webSocketHandshakeResponseReceivedResponse :: WebSocketResponse
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketHandshakeResponseReceived where
    parseJSON = A.withObject "WebSocketHandshakeResponseReceived" $ \v ->
         WebSocketHandshakeResponseReceived <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "response"


instance ToJSON WebSocketHandshakeResponseReceived  where
    toJSON v = A.object
        [ "requestId" .= webSocketHandshakeResponseReceivedRequestId v
        , "timestamp" .= webSocketHandshakeResponseReceivedTimestamp v
        , "response" .= webSocketHandshakeResponseReceivedResponse v
        ]


data WebSocketWillSendHandshakeRequest = WebSocketWillSendHandshakeRequest {
    webSocketWillSendHandshakeRequestRequestId :: RequestId,
    webSocketWillSendHandshakeRequestTimestamp :: MonotonicTime,
    webSocketWillSendHandshakeRequestWallTime :: TimeSinceEpoch,
    webSocketWillSendHandshakeRequestRequest :: WebSocketRequest
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketWillSendHandshakeRequest where
    parseJSON = A.withObject "WebSocketWillSendHandshakeRequest" $ \v ->
         WebSocketWillSendHandshakeRequest <$> v .:  "requestId"
            <*> v  .:  "timestamp"
            <*> v  .:  "wallTime"
            <*> v  .:  "request"


instance ToJSON WebSocketWillSendHandshakeRequest  where
    toJSON v = A.object
        [ "requestId" .= webSocketWillSendHandshakeRequestRequestId v
        , "timestamp" .= webSocketWillSendHandshakeRequestTimestamp v
        , "wallTime" .= webSocketWillSendHandshakeRequestWallTime v
        , "request" .= webSocketWillSendHandshakeRequestRequest v
        ]


data WebTransportCreated = WebTransportCreated {
    webTransportCreatedTransportId :: RequestId,
    webTransportCreatedUrl :: String,
    webTransportCreatedTimestamp :: MonotonicTime,
    webTransportCreatedInitiator :: Maybe Initiator
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebTransportCreated where
    parseJSON = A.withObject "WebTransportCreated" $ \v ->
         WebTransportCreated <$> v .:  "transportId"
            <*> v  .:  "url"
            <*> v  .:  "timestamp"
            <*> v  .:?  "initiator"


instance ToJSON WebTransportCreated  where
    toJSON v = A.object
        [ "transportId" .= webTransportCreatedTransportId v
        , "url" .= webTransportCreatedUrl v
        , "timestamp" .= webTransportCreatedTimestamp v
        , "initiator" .= webTransportCreatedInitiator v
        ]


data WebTransportConnectionEstablished = WebTransportConnectionEstablished {
    webTransportConnectionEstablishedTransportId :: RequestId,
    webTransportConnectionEstablishedTimestamp :: MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebTransportConnectionEstablished where
    parseJSON = A.withObject "WebTransportConnectionEstablished" $ \v ->
         WebTransportConnectionEstablished <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON WebTransportConnectionEstablished  where
    toJSON v = A.object
        [ "transportId" .= webTransportConnectionEstablishedTransportId v
        , "timestamp" .= webTransportConnectionEstablishedTimestamp v
        ]


data WebTransportClosed = WebTransportClosed {
    webTransportClosedTransportId :: RequestId,
    webTransportClosedTimestamp :: MonotonicTime
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebTransportClosed where
    parseJSON = A.withObject "WebTransportClosed" $ \v ->
         WebTransportClosed <$> v .:  "transportId"
            <*> v  .:  "timestamp"


instance ToJSON WebTransportClosed  where
    toJSON v = A.object
        [ "transportId" .= webTransportClosedTransportId v
        , "timestamp" .= webTransportClosedTimestamp v
        ]



data ResourceType = ResourceTypeDocument | ResourceTypeStylesheet | ResourceTypeImage | ResourceTypeMedia | ResourceTypeFont | ResourceTypeScript | ResourceTypeTextTrack | ResourceTypeXhr | ResourceTypeFetch | ResourceTypeEventSource | ResourceTypeWebSocket | ResourceTypeManifest | ResourceTypeSignedExchange | ResourceTypePing | ResourceTypeCspViolationReport | ResourceTypePreflight | ResourceTypeOther
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ResourceType where
    parseJSON = A.withText  "ResourceType"  $ \v -> do
        pure $ case v of
                "Document" -> ResourceTypeDocument
                "Stylesheet" -> ResourceTypeStylesheet
                "Image" -> ResourceTypeImage
                "Media" -> ResourceTypeMedia
                "Font" -> ResourceTypeFont
                "Script" -> ResourceTypeScript
                "TextTrack" -> ResourceTypeTextTrack
                "XHR" -> ResourceTypeXhr
                "Fetch" -> ResourceTypeFetch
                "EventSource" -> ResourceTypeEventSource
                "WebSocket" -> ResourceTypeWebSocket
                "Manifest" -> ResourceTypeManifest
                "SignedExchange" -> ResourceTypeSignedExchange
                "Ping" -> ResourceTypePing
                "CSPViolationReport" -> ResourceTypeCspViolationReport
                "Preflight" -> ResourceTypePreflight
                "Other" -> ResourceTypeOther
                _ -> error "failed to parse ResourceType"

instance ToJSON ResourceType where
    toJSON v = A.String $
        case v of
                ResourceTypeDocument -> "Document"
                ResourceTypeStylesheet -> "Stylesheet"
                ResourceTypeImage -> "Image"
                ResourceTypeMedia -> "Media"
                ResourceTypeFont -> "Font"
                ResourceTypeScript -> "Script"
                ResourceTypeTextTrack -> "TextTrack"
                ResourceTypeXhr -> "XHR"
                ResourceTypeFetch -> "Fetch"
                ResourceTypeEventSource -> "EventSource"
                ResourceTypeWebSocket -> "WebSocket"
                ResourceTypeManifest -> "Manifest"
                ResourceTypeSignedExchange -> "SignedExchange"
                ResourceTypePing -> "Ping"
                ResourceTypeCspViolationReport -> "CSPViolationReport"
                ResourceTypePreflight -> "Preflight"
                ResourceTypeOther -> "Other"



type LoaderId = String

type RequestId = String

type InterceptionId = String

data ErrorReason = ErrorReasonFailed | ErrorReasonAborted | ErrorReasonTimedOut | ErrorReasonAccessDenied | ErrorReasonConnectionClosed | ErrorReasonConnectionReset | ErrorReasonConnectionRefused | ErrorReasonConnectionAborted | ErrorReasonConnectionFailed | ErrorReasonNameNotResolved | ErrorReasonInternetDisconnected | ErrorReasonAddressUnreachable | ErrorReasonBlockedByClient | ErrorReasonBlockedByResponse
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ErrorReason where
    parseJSON = A.withText  "ErrorReason"  $ \v -> do
        pure $ case v of
                "Failed" -> ErrorReasonFailed
                "Aborted" -> ErrorReasonAborted
                "TimedOut" -> ErrorReasonTimedOut
                "AccessDenied" -> ErrorReasonAccessDenied
                "ConnectionClosed" -> ErrorReasonConnectionClosed
                "ConnectionReset" -> ErrorReasonConnectionReset
                "ConnectionRefused" -> ErrorReasonConnectionRefused
                "ConnectionAborted" -> ErrorReasonConnectionAborted
                "ConnectionFailed" -> ErrorReasonConnectionFailed
                "NameNotResolved" -> ErrorReasonNameNotResolved
                "InternetDisconnected" -> ErrorReasonInternetDisconnected
                "AddressUnreachable" -> ErrorReasonAddressUnreachable
                "BlockedByClient" -> ErrorReasonBlockedByClient
                "BlockedByResponse" -> ErrorReasonBlockedByResponse
                _ -> error "failed to parse ErrorReason"

instance ToJSON ErrorReason where
    toJSON v = A.String $
        case v of
                ErrorReasonFailed -> "Failed"
                ErrorReasonAborted -> "Aborted"
                ErrorReasonTimedOut -> "TimedOut"
                ErrorReasonAccessDenied -> "AccessDenied"
                ErrorReasonConnectionClosed -> "ConnectionClosed"
                ErrorReasonConnectionReset -> "ConnectionReset"
                ErrorReasonConnectionRefused -> "ConnectionRefused"
                ErrorReasonConnectionAborted -> "ConnectionAborted"
                ErrorReasonConnectionFailed -> "ConnectionFailed"
                ErrorReasonNameNotResolved -> "NameNotResolved"
                ErrorReasonInternetDisconnected -> "InternetDisconnected"
                ErrorReasonAddressUnreachable -> "AddressUnreachable"
                ErrorReasonBlockedByClient -> "BlockedByClient"
                ErrorReasonBlockedByResponse -> "BlockedByResponse"



type TimeSinceEpoch = Int

type MonotonicTime = Int

type Headers = [(String, String)]

data ConnectionType = ConnectionTypeNone | ConnectionTypeCellular2g | ConnectionTypeCellular3g | ConnectionTypeCellular4g | ConnectionTypeBluetooth | ConnectionTypeEthernet | ConnectionTypeWifi | ConnectionTypeWimax | ConnectionTypeOther
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ConnectionType where
    parseJSON = A.withText  "ConnectionType"  $ \v -> do
        pure $ case v of
                "none" -> ConnectionTypeNone
                "cellular2g" -> ConnectionTypeCellular2g
                "cellular3g" -> ConnectionTypeCellular3g
                "cellular4g" -> ConnectionTypeCellular4g
                "bluetooth" -> ConnectionTypeBluetooth
                "ethernet" -> ConnectionTypeEthernet
                "wifi" -> ConnectionTypeWifi
                "wimax" -> ConnectionTypeWimax
                "other" -> ConnectionTypeOther
                _ -> error "failed to parse ConnectionType"

instance ToJSON ConnectionType where
    toJSON v = A.String $
        case v of
                ConnectionTypeNone -> "none"
                ConnectionTypeCellular2g -> "cellular2g"
                ConnectionTypeCellular3g -> "cellular3g"
                ConnectionTypeCellular4g -> "cellular4g"
                ConnectionTypeBluetooth -> "bluetooth"
                ConnectionTypeEthernet -> "ethernet"
                ConnectionTypeWifi -> "wifi"
                ConnectionTypeWimax -> "wimax"
                ConnectionTypeOther -> "other"



data CookieSameSite = CookieSameSiteStrict | CookieSameSiteLax | CookieSameSiteNone
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON CookieSameSite where
    parseJSON = A.withText  "CookieSameSite"  $ \v -> do
        pure $ case v of
                "Strict" -> CookieSameSiteStrict
                "Lax" -> CookieSameSiteLax
                "None" -> CookieSameSiteNone
                _ -> error "failed to parse CookieSameSite"

instance ToJSON CookieSameSite where
    toJSON v = A.String $
        case v of
                CookieSameSiteStrict -> "Strict"
                CookieSameSiteLax -> "Lax"
                CookieSameSiteNone -> "None"



data ResourceTiming = ResourceTiming {
    resourceTimingRequestTime :: Int,
    resourceTimingProxyStart :: Int,
    resourceTimingProxyEnd :: Int,
    resourceTimingDnsStart :: Int,
    resourceTimingDnsEnd :: Int,
    resourceTimingConnectStart :: Int,
    resourceTimingConnectEnd :: Int,
    resourceTimingSslStart :: Int,
    resourceTimingSslEnd :: Int,
    resourceTimingSendStart :: Int,
    resourceTimingSendEnd :: Int,
    resourceTimingReceiveHeadersEnd :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ResourceTiming where
    parseJSON = A.withObject "ResourceTiming" $ \v ->
         ResourceTiming <$> v .:  "requestTime"
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


instance ToJSON ResourceTiming  where
    toJSON v = A.object
        [ "requestTime" .= resourceTimingRequestTime v
        , "proxyStart" .= resourceTimingProxyStart v
        , "proxyEnd" .= resourceTimingProxyEnd v
        , "dnsStart" .= resourceTimingDnsStart v
        , "dnsEnd" .= resourceTimingDnsEnd v
        , "connectStart" .= resourceTimingConnectStart v
        , "connectEnd" .= resourceTimingConnectEnd v
        , "sslStart" .= resourceTimingSslStart v
        , "sslEnd" .= resourceTimingSslEnd v
        , "sendStart" .= resourceTimingSendStart v
        , "sendEnd" .= resourceTimingSendEnd v
        , "receiveHeadersEnd" .= resourceTimingReceiveHeadersEnd v
        ]



data ResourcePriority = ResourcePriorityVeryLow | ResourcePriorityLow | ResourcePriorityMedium | ResourcePriorityHigh | ResourcePriorityVeryHigh
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ResourcePriority where
    parseJSON = A.withText  "ResourcePriority"  $ \v -> do
        pure $ case v of
                "VeryLow" -> ResourcePriorityVeryLow
                "Low" -> ResourcePriorityLow
                "Medium" -> ResourcePriorityMedium
                "High" -> ResourcePriorityHigh
                "VeryHigh" -> ResourcePriorityVeryHigh
                _ -> error "failed to parse ResourcePriority"

instance ToJSON ResourcePriority where
    toJSON v = A.String $
        case v of
                ResourcePriorityVeryLow -> "VeryLow"
                ResourcePriorityLow -> "Low"
                ResourcePriorityMedium -> "Medium"
                ResourcePriorityHigh -> "High"
                ResourcePriorityVeryHigh -> "VeryHigh"



data PostDataEntry = PostDataEntry {
    postDataEntryBytes :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  PostDataEntry where
    parseJSON = A.withObject "PostDataEntry" $ \v ->
         PostDataEntry <$> v .:?  "bytes"


instance ToJSON PostDataEntry  where
    toJSON v = A.object
        [ "bytes" .= postDataEntryBytes v
        ]



data Request = Request {
    requestUrl :: String,
    requestMethod :: String,
    requestHeaders :: Headers,
    requestInitialPriority :: ResourcePriority,
    requestReferrerPolicy :: String,
    requestUrlFragment :: Maybe String,
    requestPostData :: Maybe String,
    requestHasPostData :: Maybe Bool,
    requestMixedContentType :: Maybe Security.MixedContentType,
    requestIsLinkPreload :: Maybe Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Request where
    parseJSON = A.withObject "Request" $ \v ->
         Request <$> v .:  "url"
            <*> v  .:  "method"
            <*> v  .:  "headers"
            <*> v  .:  "initialPriority"
            <*> v  .:  "referrerPolicy"
            <*> v  .:?  "urlFragment"
            <*> v  .:?  "postData"
            <*> v  .:?  "hasPostData"
            <*> v  .:?  "mixedContentType"
            <*> v  .:?  "isLinkPreload"


instance ToJSON Request  where
    toJSON v = A.object
        [ "url" .= requestUrl v
        , "method" .= requestMethod v
        , "headers" .= requestHeaders v
        , "initialPriority" .= requestInitialPriority v
        , "referrerPolicy" .= requestReferrerPolicy v
        , "urlFragment" .= requestUrlFragment v
        , "postData" .= requestPostData v
        , "hasPostData" .= requestHasPostData v
        , "mixedContentType" .= requestMixedContentType v
        , "isLinkPreload" .= requestIsLinkPreload v
        ]



data SignedCertificateTimestamp = SignedCertificateTimestamp {
    signedCertificateTimestampStatus :: String,
    signedCertificateTimestampOrigin :: String,
    signedCertificateTimestampLogDescription :: String,
    signedCertificateTimestampLogId :: String,
    signedCertificateTimestampTimestamp :: Int,
    signedCertificateTimestampHashAlgorithm :: String,
    signedCertificateTimestampSignatureAlgorithm :: String,
    signedCertificateTimestampSignatureData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SignedCertificateTimestamp where
    parseJSON = A.withObject "SignedCertificateTimestamp" $ \v ->
         SignedCertificateTimestamp <$> v .:  "status"
            <*> v  .:  "origin"
            <*> v  .:  "logDescription"
            <*> v  .:  "logId"
            <*> v  .:  "timestamp"
            <*> v  .:  "hashAlgorithm"
            <*> v  .:  "signatureAlgorithm"
            <*> v  .:  "signatureData"


instance ToJSON SignedCertificateTimestamp  where
    toJSON v = A.object
        [ "status" .= signedCertificateTimestampStatus v
        , "origin" .= signedCertificateTimestampOrigin v
        , "logDescription" .= signedCertificateTimestampLogDescription v
        , "logId" .= signedCertificateTimestampLogId v
        , "timestamp" .= signedCertificateTimestampTimestamp v
        , "hashAlgorithm" .= signedCertificateTimestampHashAlgorithm v
        , "signatureAlgorithm" .= signedCertificateTimestampSignatureAlgorithm v
        , "signatureData" .= signedCertificateTimestampSignatureData v
        ]



data SecurityDetails = SecurityDetails {
    securityDetailsProtocol :: String,
    securityDetailsKeyExchange :: String,
    securityDetailsCipher :: String,
    securityDetailsCertificateId :: Security.CertificateId,
    securityDetailsSubjectName :: String,
    securityDetailsSanList :: [String],
    securityDetailsIssuer :: String,
    securityDetailsValidFrom :: TimeSinceEpoch,
    securityDetailsValidTo :: TimeSinceEpoch,
    securityDetailsSignedCertificateTimestampList :: [SignedCertificateTimestamp],
    securityDetailsCertificateTransparencyCompliance :: CertificateTransparencyCompliance,
    securityDetailsKeyExchangeGroup :: Maybe String,
    securityDetailsMac :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SecurityDetails where
    parseJSON = A.withObject "SecurityDetails" $ \v ->
         SecurityDetails <$> v .:  "protocol"
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


instance ToJSON SecurityDetails  where
    toJSON v = A.object
        [ "protocol" .= securityDetailsProtocol v
        , "keyExchange" .= securityDetailsKeyExchange v
        , "cipher" .= securityDetailsCipher v
        , "certificateId" .= securityDetailsCertificateId v
        , "subjectName" .= securityDetailsSubjectName v
        , "sanList" .= securityDetailsSanList v
        , "issuer" .= securityDetailsIssuer v
        , "validFrom" .= securityDetailsValidFrom v
        , "validTo" .= securityDetailsValidTo v
        , "signedCertificateTimestampList" .= securityDetailsSignedCertificateTimestampList v
        , "certificateTransparencyCompliance" .= securityDetailsCertificateTransparencyCompliance v
        , "keyExchangeGroup" .= securityDetailsKeyExchangeGroup v
        , "mac" .= securityDetailsMac v
        ]



data CertificateTransparencyCompliance = CertificateTransparencyComplianceUnknown | CertificateTransparencyComplianceNotCompliant | CertificateTransparencyComplianceCompliant
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON CertificateTransparencyCompliance where
    parseJSON = A.withText  "CertificateTransparencyCompliance"  $ \v -> do
        pure $ case v of
                "unknown" -> CertificateTransparencyComplianceUnknown
                "not-compliant" -> CertificateTransparencyComplianceNotCompliant
                "compliant" -> CertificateTransparencyComplianceCompliant
                _ -> error "failed to parse CertificateTransparencyCompliance"

instance ToJSON CertificateTransparencyCompliance where
    toJSON v = A.String $
        case v of
                CertificateTransparencyComplianceUnknown -> "unknown"
                CertificateTransparencyComplianceNotCompliant -> "not-compliant"
                CertificateTransparencyComplianceCompliant -> "compliant"



data BlockedReason = BlockedReasonOther | BlockedReasonCsp | BlockedReasonMixedContent | BlockedReasonOrigin | BlockedReasonInspector | BlockedReasonSubresourceFilter | BlockedReasonContentType | BlockedReasonCoepFrameResourceNeedsCoepHeader | BlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | BlockedReasonCorpNotSameOrigin | BlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | BlockedReasonCorpNotSameSite
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON BlockedReason where
    parseJSON = A.withText  "BlockedReason"  $ \v -> do
        pure $ case v of
                "other" -> BlockedReasonOther
                "csp" -> BlockedReasonCsp
                "mixed-content" -> BlockedReasonMixedContent
                "origin" -> BlockedReasonOrigin
                "inspector" -> BlockedReasonInspector
                "subresource-filter" -> BlockedReasonSubresourceFilter
                "content-type" -> BlockedReasonContentType
                "coep-frame-resource-needs-coep-header" -> BlockedReasonCoepFrameResourceNeedsCoepHeader
                "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> BlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
                "corp-not-same-origin" -> BlockedReasonCorpNotSameOrigin
                "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> BlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
                "corp-not-same-site" -> BlockedReasonCorpNotSameSite
                _ -> error "failed to parse BlockedReason"

instance ToJSON BlockedReason where
    toJSON v = A.String $
        case v of
                BlockedReasonOther -> "other"
                BlockedReasonCsp -> "csp"
                BlockedReasonMixedContent -> "mixed-content"
                BlockedReasonOrigin -> "origin"
                BlockedReasonInspector -> "inspector"
                BlockedReasonSubresourceFilter -> "subresource-filter"
                BlockedReasonContentType -> "content-type"
                BlockedReasonCoepFrameResourceNeedsCoepHeader -> "coep-frame-resource-needs-coep-header"
                BlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage -> "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
                BlockedReasonCorpNotSameOrigin -> "corp-not-same-origin"
                BlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
                BlockedReasonCorpNotSameSite -> "corp-not-same-site"



data CorsError = CorsErrorDisallowedByMode | CorsErrorInvalidResponse | CorsErrorWildcardOriginNotAllowed | CorsErrorMissingAllowOriginHeader | CorsErrorMultipleAllowOriginValues | CorsErrorInvalidAllowOriginValue | CorsErrorAllowOriginMismatch | CorsErrorInvalidAllowCredentials | CorsErrorCorsDisabledScheme | CorsErrorPreflightInvalidStatus | CorsErrorPreflightDisallowedRedirect | CorsErrorPreflightWildcardOriginNotAllowed | CorsErrorPreflightMissingAllowOriginHeader | CorsErrorPreflightMultipleAllowOriginValues | CorsErrorPreflightInvalidAllowOriginValue | CorsErrorPreflightAllowOriginMismatch | CorsErrorPreflightInvalidAllowCredentials | CorsErrorPreflightMissingAllowExternal | CorsErrorPreflightInvalidAllowExternal | CorsErrorPreflightMissingAllowPrivateNetwork | CorsErrorPreflightInvalidAllowPrivateNetwork | CorsErrorInvalidAllowMethodsPreflightResponse | CorsErrorInvalidAllowHeadersPreflightResponse | CorsErrorMethodDisallowedByPreflightResponse | CorsErrorHeaderDisallowedByPreflightResponse | CorsErrorRedirectContainsCredentials | CorsErrorInsecurePrivateNetwork | CorsErrorInvalidPrivateNetworkAccess | CorsErrorUnexpectedPrivateNetworkAccess | CorsErrorNoCorsRedirectModeNotFollow
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON CorsError where
    parseJSON = A.withText  "CorsError"  $ \v -> do
        pure $ case v of
                "DisallowedByMode" -> CorsErrorDisallowedByMode
                "InvalidResponse" -> CorsErrorInvalidResponse
                "WildcardOriginNotAllowed" -> CorsErrorWildcardOriginNotAllowed
                "MissingAllowOriginHeader" -> CorsErrorMissingAllowOriginHeader
                "MultipleAllowOriginValues" -> CorsErrorMultipleAllowOriginValues
                "InvalidAllowOriginValue" -> CorsErrorInvalidAllowOriginValue
                "AllowOriginMismatch" -> CorsErrorAllowOriginMismatch
                "InvalidAllowCredentials" -> CorsErrorInvalidAllowCredentials
                "CorsDisabledScheme" -> CorsErrorCorsDisabledScheme
                "PreflightInvalidStatus" -> CorsErrorPreflightInvalidStatus
                "PreflightDisallowedRedirect" -> CorsErrorPreflightDisallowedRedirect
                "PreflightWildcardOriginNotAllowed" -> CorsErrorPreflightWildcardOriginNotAllowed
                "PreflightMissingAllowOriginHeader" -> CorsErrorPreflightMissingAllowOriginHeader
                "PreflightMultipleAllowOriginValues" -> CorsErrorPreflightMultipleAllowOriginValues
                "PreflightInvalidAllowOriginValue" -> CorsErrorPreflightInvalidAllowOriginValue
                "PreflightAllowOriginMismatch" -> CorsErrorPreflightAllowOriginMismatch
                "PreflightInvalidAllowCredentials" -> CorsErrorPreflightInvalidAllowCredentials
                "PreflightMissingAllowExternal" -> CorsErrorPreflightMissingAllowExternal
                "PreflightInvalidAllowExternal" -> CorsErrorPreflightInvalidAllowExternal
                "PreflightMissingAllowPrivateNetwork" -> CorsErrorPreflightMissingAllowPrivateNetwork
                "PreflightInvalidAllowPrivateNetwork" -> CorsErrorPreflightInvalidAllowPrivateNetwork
                "InvalidAllowMethodsPreflightResponse" -> CorsErrorInvalidAllowMethodsPreflightResponse
                "InvalidAllowHeadersPreflightResponse" -> CorsErrorInvalidAllowHeadersPreflightResponse
                "MethodDisallowedByPreflightResponse" -> CorsErrorMethodDisallowedByPreflightResponse
                "HeaderDisallowedByPreflightResponse" -> CorsErrorHeaderDisallowedByPreflightResponse
                "RedirectContainsCredentials" -> CorsErrorRedirectContainsCredentials
                "InsecurePrivateNetwork" -> CorsErrorInsecurePrivateNetwork
                "InvalidPrivateNetworkAccess" -> CorsErrorInvalidPrivateNetworkAccess
                "UnexpectedPrivateNetworkAccess" -> CorsErrorUnexpectedPrivateNetworkAccess
                "NoCorsRedirectModeNotFollow" -> CorsErrorNoCorsRedirectModeNotFollow
                _ -> error "failed to parse CorsError"

instance ToJSON CorsError where
    toJSON v = A.String $
        case v of
                CorsErrorDisallowedByMode -> "DisallowedByMode"
                CorsErrorInvalidResponse -> "InvalidResponse"
                CorsErrorWildcardOriginNotAllowed -> "WildcardOriginNotAllowed"
                CorsErrorMissingAllowOriginHeader -> "MissingAllowOriginHeader"
                CorsErrorMultipleAllowOriginValues -> "MultipleAllowOriginValues"
                CorsErrorInvalidAllowOriginValue -> "InvalidAllowOriginValue"
                CorsErrorAllowOriginMismatch -> "AllowOriginMismatch"
                CorsErrorInvalidAllowCredentials -> "InvalidAllowCredentials"
                CorsErrorCorsDisabledScheme -> "CorsDisabledScheme"
                CorsErrorPreflightInvalidStatus -> "PreflightInvalidStatus"
                CorsErrorPreflightDisallowedRedirect -> "PreflightDisallowedRedirect"
                CorsErrorPreflightWildcardOriginNotAllowed -> "PreflightWildcardOriginNotAllowed"
                CorsErrorPreflightMissingAllowOriginHeader -> "PreflightMissingAllowOriginHeader"
                CorsErrorPreflightMultipleAllowOriginValues -> "PreflightMultipleAllowOriginValues"
                CorsErrorPreflightInvalidAllowOriginValue -> "PreflightInvalidAllowOriginValue"
                CorsErrorPreflightAllowOriginMismatch -> "PreflightAllowOriginMismatch"
                CorsErrorPreflightInvalidAllowCredentials -> "PreflightInvalidAllowCredentials"
                CorsErrorPreflightMissingAllowExternal -> "PreflightMissingAllowExternal"
                CorsErrorPreflightInvalidAllowExternal -> "PreflightInvalidAllowExternal"
                CorsErrorPreflightMissingAllowPrivateNetwork -> "PreflightMissingAllowPrivateNetwork"
                CorsErrorPreflightInvalidAllowPrivateNetwork -> "PreflightInvalidAllowPrivateNetwork"
                CorsErrorInvalidAllowMethodsPreflightResponse -> "InvalidAllowMethodsPreflightResponse"
                CorsErrorInvalidAllowHeadersPreflightResponse -> "InvalidAllowHeadersPreflightResponse"
                CorsErrorMethodDisallowedByPreflightResponse -> "MethodDisallowedByPreflightResponse"
                CorsErrorHeaderDisallowedByPreflightResponse -> "HeaderDisallowedByPreflightResponse"
                CorsErrorRedirectContainsCredentials -> "RedirectContainsCredentials"
                CorsErrorInsecurePrivateNetwork -> "InsecurePrivateNetwork"
                CorsErrorInvalidPrivateNetworkAccess -> "InvalidPrivateNetworkAccess"
                CorsErrorUnexpectedPrivateNetworkAccess -> "UnexpectedPrivateNetworkAccess"
                CorsErrorNoCorsRedirectModeNotFollow -> "NoCorsRedirectModeNotFollow"



data CorsErrorStatus = CorsErrorStatus {
    corsErrorStatusCorsError :: CorsError,
    corsErrorStatusFailedParameter :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CorsErrorStatus where
    parseJSON = A.withObject "CorsErrorStatus" $ \v ->
         CorsErrorStatus <$> v .:  "corsError"
            <*> v  .:  "failedParameter"


instance ToJSON CorsErrorStatus  where
    toJSON v = A.object
        [ "corsError" .= corsErrorStatusCorsError v
        , "failedParameter" .= corsErrorStatusFailedParameter v
        ]



data ServiceWorkerResponseSource = ServiceWorkerResponseSourceCacheStorage | ServiceWorkerResponseSourceHttpCache | ServiceWorkerResponseSourceFallbackCode | ServiceWorkerResponseSourceNetwork
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ServiceWorkerResponseSource where
    parseJSON = A.withText  "ServiceWorkerResponseSource"  $ \v -> do
        pure $ case v of
                "cache-storage" -> ServiceWorkerResponseSourceCacheStorage
                "http-cache" -> ServiceWorkerResponseSourceHttpCache
                "fallback-code" -> ServiceWorkerResponseSourceFallbackCode
                "network" -> ServiceWorkerResponseSourceNetwork
                _ -> error "failed to parse ServiceWorkerResponseSource"

instance ToJSON ServiceWorkerResponseSource where
    toJSON v = A.String $
        case v of
                ServiceWorkerResponseSourceCacheStorage -> "cache-storage"
                ServiceWorkerResponseSourceHttpCache -> "http-cache"
                ServiceWorkerResponseSourceFallbackCode -> "fallback-code"
                ServiceWorkerResponseSourceNetwork -> "network"



data Response = Response {
    responseUrl :: String,
    responseStatus :: Int,
    responseStatusText :: String,
    responseHeaders :: Headers,
    responseMimeType :: String,
    responseConnectionReused :: Bool,
    responseConnectionId :: Int,
    responseEncodedDataLength :: Int,
    responseSecurityState :: Security.SecurityState,
    responseRequestHeaders :: Maybe Headers,
    responseRemoteIpAddress :: Maybe String,
    responseRemotePort :: Maybe Int,
    responseFromDiskCache :: Maybe Bool,
    responseFromServiceWorker :: Maybe Bool,
    responseFromPrefetchCache :: Maybe Bool,
    responseTiming :: Maybe ResourceTiming,
    responseServiceWorkerResponseSource :: Maybe ServiceWorkerResponseSource,
    responseResponseTime :: Maybe TimeSinceEpoch,
    responseCacheStorageCacheName :: Maybe String,
    responseProtocol :: Maybe String,
    responseSecurityDetails :: Maybe SecurityDetails
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Response where
    parseJSON = A.withObject "Response" $ \v ->
         Response <$> v .:  "url"
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


instance ToJSON Response  where
    toJSON v = A.object
        [ "url" .= responseUrl v
        , "status" .= responseStatus v
        , "statusText" .= responseStatusText v
        , "headers" .= responseHeaders v
        , "mimeType" .= responseMimeType v
        , "connectionReused" .= responseConnectionReused v
        , "connectionId" .= responseConnectionId v
        , "encodedDataLength" .= responseEncodedDataLength v
        , "securityState" .= responseSecurityState v
        , "requestHeaders" .= responseRequestHeaders v
        , "remoteIPAddress" .= responseRemoteIpAddress v
        , "remotePort" .= responseRemotePort v
        , "fromDiskCache" .= responseFromDiskCache v
        , "fromServiceWorker" .= responseFromServiceWorker v
        , "fromPrefetchCache" .= responseFromPrefetchCache v
        , "timing" .= responseTiming v
        , "serviceWorkerResponseSource" .= responseServiceWorkerResponseSource v
        , "responseTime" .= responseResponseTime v
        , "cacheStorageCacheName" .= responseCacheStorageCacheName v
        , "protocol" .= responseProtocol v
        , "securityDetails" .= responseSecurityDetails v
        ]



data WebSocketRequest = WebSocketRequest {
    webSocketRequestHeaders :: Headers
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketRequest where
    parseJSON = A.withObject "WebSocketRequest" $ \v ->
         WebSocketRequest <$> v .:  "headers"


instance ToJSON WebSocketRequest  where
    toJSON v = A.object
        [ "headers" .= webSocketRequestHeaders v
        ]



data WebSocketResponse = WebSocketResponse {
    webSocketResponseStatus :: Int,
    webSocketResponseStatusText :: String,
    webSocketResponseHeaders :: Headers,
    webSocketResponseHeadersText :: Maybe String,
    webSocketResponseRequestHeaders :: Maybe Headers,
    webSocketResponseRequestHeadersText :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketResponse where
    parseJSON = A.withObject "WebSocketResponse" $ \v ->
         WebSocketResponse <$> v .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:?  "headersText"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "requestHeadersText"


instance ToJSON WebSocketResponse  where
    toJSON v = A.object
        [ "status" .= webSocketResponseStatus v
        , "statusText" .= webSocketResponseStatusText v
        , "headers" .= webSocketResponseHeaders v
        , "headersText" .= webSocketResponseHeadersText v
        , "requestHeaders" .= webSocketResponseRequestHeaders v
        , "requestHeadersText" .= webSocketResponseRequestHeadersText v
        ]



data WebSocketFrame = WebSocketFrame {
    webSocketFrameOpcode :: Int,
    webSocketFrameMask :: Bool,
    webSocketFramePayloadData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  WebSocketFrame where
    parseJSON = A.withObject "WebSocketFrame" $ \v ->
         WebSocketFrame <$> v .:  "opcode"
            <*> v  .:  "mask"
            <*> v  .:  "payloadData"


instance ToJSON WebSocketFrame  where
    toJSON v = A.object
        [ "opcode" .= webSocketFrameOpcode v
        , "mask" .= webSocketFrameMask v
        , "payloadData" .= webSocketFramePayloadData v
        ]



data CachedResource = CachedResource {
    cachedResourceUrl :: String,
    cachedResourceType :: ResourceType,
    cachedResourceBodySize :: Int,
    cachedResourceResponse :: Maybe Response
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CachedResource where
    parseJSON = A.withObject "CachedResource" $ \v ->
         CachedResource <$> v .:  "url"
            <*> v  .:  "type"
            <*> v  .:  "bodySize"
            <*> v  .:?  "response"


instance ToJSON CachedResource  where
    toJSON v = A.object
        [ "url" .= cachedResourceUrl v
        , "type" .= cachedResourceType v
        , "bodySize" .= cachedResourceBodySize v
        , "response" .= cachedResourceResponse v
        ]



data Initiator = Initiator {
    initiatorType :: String,
    initiatorStack :: Maybe Runtime.StackTrace,
    initiatorUrl :: Maybe String,
    initiatorLineNumber :: Maybe Int,
    initiatorColumnNumber :: Maybe Int,
    initiatorRequestId :: Maybe RequestId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Initiator where
    parseJSON = A.withObject "Initiator" $ \v ->
         Initiator <$> v .:  "type"
            <*> v  .:?  "stack"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "requestId"


instance ToJSON Initiator  where
    toJSON v = A.object
        [ "type" .= initiatorType v
        , "stack" .= initiatorStack v
        , "url" .= initiatorUrl v
        , "lineNumber" .= initiatorLineNumber v
        , "columnNumber" .= initiatorColumnNumber v
        , "requestId" .= initiatorRequestId v
        ]



data Cookie = Cookie {
    cookieName :: String,
    cookieValue :: String,
    cookieDomain :: String,
    cookiePath :: String,
    cookieExpires :: Int,
    cookieSize :: Int,
    cookieHttpOnly :: Bool,
    cookieSecure :: Bool,
    cookieSession :: Bool,
    cookieSameSite :: Maybe CookieSameSite
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Cookie where
    parseJSON = A.withObject "Cookie" $ \v ->
         Cookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:  "domain"
            <*> v  .:  "path"
            <*> v  .:  "expires"
            <*> v  .:  "size"
            <*> v  .:  "httpOnly"
            <*> v  .:  "secure"
            <*> v  .:  "session"
            <*> v  .:?  "sameSite"


instance ToJSON Cookie  where
    toJSON v = A.object
        [ "name" .= cookieName v
        , "value" .= cookieValue v
        , "domain" .= cookieDomain v
        , "path" .= cookiePath v
        , "expires" .= cookieExpires v
        , "size" .= cookieSize v
        , "httpOnly" .= cookieHttpOnly v
        , "secure" .= cookieSecure v
        , "session" .= cookieSession v
        , "sameSite" .= cookieSameSite v
        ]



data CookieParam = CookieParam {
    cookieParamName :: String,
    cookieParamValue :: String,
    cookieParamUrl :: Maybe String,
    cookieParamDomain :: Maybe String,
    cookieParamPath :: Maybe String,
    cookieParamSecure :: Maybe Bool,
    cookieParamHttpOnly :: Maybe Bool,
    cookieParamSameSite :: Maybe CookieSameSite,
    cookieParamExpires :: Maybe TimeSinceEpoch
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CookieParam where
    parseJSON = A.withObject "CookieParam" $ \v ->
         CookieParam <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON CookieParam  where
    toJSON v = A.object
        [ "name" .= cookieParamName v
        , "value" .= cookieParamValue v
        , "url" .= cookieParamUrl v
        , "domain" .= cookieParamDomain v
        , "path" .= cookieParamPath v
        , "secure" .= cookieParamSecure v
        , "httpOnly" .= cookieParamHttpOnly v
        , "sameSite" .= cookieParamSameSite v
        , "expires" .= cookieParamExpires v
        ]



clearBrowserCache :: Session a -> IO (Maybe Error)
clearBrowserCache session  = sendReceiveCommand (conn session) ("Network","clearBrowserCache") ([] ++ (catMaybes []))


clearBrowserCookies :: Session a -> IO (Maybe Error)
clearBrowserCookies session  = sendReceiveCommand (conn session) ("Network","clearBrowserCookies") ([] ++ (catMaybes []))


deleteCookies :: Session a -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
deleteCookies session deleteCookiesName deleteCookiesUrl deleteCookiesDomain deleteCookiesPath = sendReceiveCommand (conn session) ("Network","deleteCookies") ([("name", ToJSONEx deleteCookiesName)] ++ (catMaybes [fmap (("url",) . ToJSONEx) deleteCookiesUrl, fmap (("domain",) . ToJSONEx) deleteCookiesDomain, fmap (("path",) . ToJSONEx) deleteCookiesPath]))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Network","disable") ([] ++ (catMaybes []))


emulateNetworkConditions :: Session a -> Bool -> Int -> Int -> Int -> Maybe ConnectionType -> IO (Maybe Error)
emulateNetworkConditions session emulateNetworkConditionsOffline emulateNetworkConditionsLatency emulateNetworkConditionsDownloadThroughput emulateNetworkConditionsUploadThroughput emulateNetworkConditionsConnectionType = sendReceiveCommand (conn session) ("Network","emulateNetworkConditions") ([("offline", ToJSONEx emulateNetworkConditionsOffline), ("latency", ToJSONEx emulateNetworkConditionsLatency), ("downloadThroughput", ToJSONEx emulateNetworkConditionsDownloadThroughput), ("uploadThroughput", ToJSONEx emulateNetworkConditionsUploadThroughput)] ++ (catMaybes [fmap (("connectionType",) . ToJSONEx) emulateNetworkConditionsConnectionType]))


enable :: Session a -> Maybe Int -> IO (Maybe Error)
enable session enableMaxPostDataSize = sendReceiveCommand (conn session) ("Network","enable") ([] ++ (catMaybes [fmap (("maxPostDataSize",) . ToJSONEx) enableMaxPostDataSize]))

data GetAllCookies = GetAllCookies {
    getAllCookiesCookies :: [Cookie]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetAllCookies where
    parseJSON = A.withObject "GetAllCookies" $ \v ->
         GetAllCookies <$> v .:  "cookies"



getAllCookies :: Session a -> IO (Either Error GetAllCookies)
getAllCookies session  = sendReceiveCommandResult (conn session) ("Network","getAllCookies") ([] ++ (catMaybes []))

data GetCookies = GetCookies {
    getCookiesCookies :: [Cookie]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetCookies where
    parseJSON = A.withObject "GetCookies" $ \v ->
         GetCookies <$> v .:  "cookies"



getCookies :: Session a -> Maybe [String] -> IO (Either Error GetCookies)
getCookies session getCookiesUrls = sendReceiveCommandResult (conn session) ("Network","getCookies") ([] ++ (catMaybes [fmap (("urls",) . ToJSONEx) getCookiesUrls]))

data GetResponseBody = GetResponseBody {
    getResponseBodyBody :: String,
    getResponseBodyBase64Encoded :: Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetResponseBody where
    parseJSON = A.withObject "GetResponseBody" $ \v ->
         GetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



getResponseBody :: Session a -> RequestId -> IO (Either Error GetResponseBody)
getResponseBody session getResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Network","getResponseBody") ([("requestId", ToJSONEx getResponseBodyRequestId)] ++ (catMaybes []))

data GetRequestPostData = GetRequestPostData {
    getRequestPostDataPostData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetRequestPostData where
    parseJSON = A.withObject "GetRequestPostData" $ \v ->
         GetRequestPostData <$> v .:  "postData"



getRequestPostData :: Session a -> RequestId -> IO (Either Error GetRequestPostData)
getRequestPostData session getRequestPostDataRequestId = sendReceiveCommandResult (conn session) ("Network","getRequestPostData") ([("requestId", ToJSONEx getRequestPostDataRequestId)] ++ (catMaybes []))


setCacheDisabled :: Session a -> Bool -> IO (Maybe Error)
setCacheDisabled session setCacheDisabledCacheDisabled = sendReceiveCommand (conn session) ("Network","setCacheDisabled") ([("cacheDisabled", ToJSONEx setCacheDisabledCacheDisabled)] ++ (catMaybes []))


setCookie :: Session a -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe CookieSameSite -> Maybe TimeSinceEpoch -> IO (Maybe Error)
setCookie session setCookieName setCookieValue setCookieUrl setCookieDomain setCookiePath setCookieSecure setCookieHttpOnly setCookieSameSite setCookieExpires = sendReceiveCommand (conn session) ("Network","setCookie") ([("name", ToJSONEx setCookieName), ("value", ToJSONEx setCookieValue)] ++ (catMaybes [fmap (("url",) . ToJSONEx) setCookieUrl, fmap (("domain",) . ToJSONEx) setCookieDomain, fmap (("path",) . ToJSONEx) setCookiePath, fmap (("secure",) . ToJSONEx) setCookieSecure, fmap (("httpOnly",) . ToJSONEx) setCookieHttpOnly, fmap (("sameSite",) . ToJSONEx) setCookieSameSite, fmap (("expires",) . ToJSONEx) setCookieExpires]))


setCookies :: Session a -> [CookieParam] -> IO (Maybe Error)
setCookies session setCookiesCookies = sendReceiveCommand (conn session) ("Network","setCookies") ([("cookies", ToJSONEx setCookiesCookies)] ++ (catMaybes []))


setExtraHTTPHeaders :: Session a -> Headers -> IO (Maybe Error)
setExtraHTTPHeaders session setExtraHttpHeadersHeaders = sendReceiveCommand (conn session) ("Network","setExtraHTTPHeaders") ([("headers", ToJSONEx setExtraHttpHeadersHeaders)] ++ (catMaybes []))


setUserAgentOverride :: Session a -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
setUserAgentOverride session setUserAgentOverrideUserAgent setUserAgentOverrideAcceptLanguage setUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Network","setUserAgentOverride") ([("userAgent", ToJSONEx setUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("acceptLanguage",) . ToJSONEx) setUserAgentOverrideAcceptLanguage, fmap (("platform",) . ToJSONEx) setUserAgentOverridePlatform]))


