{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Fetch (module Domains.Fetch) where
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

import qualified Domains.Network as Network
import qualified Domains.IO as IO
import qualified Domains.Page as Page


import Utils

data RequestPaused = RequestPaused {
    requestPausedRequestId :: RequestId,
    requestPausedRequest :: Network.Request,
    requestPausedFrameId :: Page.FrameId,
    requestPausedResourceType :: Network.ResourceType,
    requestPausedResponseErrorReason :: Maybe Network.ErrorReason,
    requestPausedResponseStatusCode :: Maybe Int,
    requestPausedResponseStatusText :: Maybe String,
    requestPausedResponseHeaders :: Maybe [HeaderEntry],
    requestPausedNetworkId :: Maybe RequestId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RequestPaused where
    parseJSON = A.withObject "RequestPaused" $ \v ->
         RequestPaused <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:?  "responseErrorReason"
            <*> v  .:?  "responseStatusCode"
            <*> v  .:?  "responseStatusText"
            <*> v  .:?  "responseHeaders"
            <*> v  .:?  "networkId"


instance ToJSON RequestPaused  where
    toJSON v = A.object
        [ "requestId" .= requestPausedRequestId v
        , "request" .= requestPausedRequest v
        , "frameId" .= requestPausedFrameId v
        , "resourceType" .= requestPausedResourceType v
        , "responseErrorReason" .= requestPausedResponseErrorReason v
        , "responseStatusCode" .= requestPausedResponseStatusCode v
        , "responseStatusText" .= requestPausedResponseStatusText v
        , "responseHeaders" .= requestPausedResponseHeaders v
        , "networkId" .= requestPausedNetworkId v
        ]


data AuthRequired = AuthRequired {
    authRequiredRequestId :: RequestId,
    authRequiredRequest :: Network.Request,
    authRequiredFrameId :: Page.FrameId,
    authRequiredResourceType :: Network.ResourceType,
    authRequiredAuthChallenge :: AuthChallenge
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AuthRequired where
    parseJSON = A.withObject "AuthRequired" $ \v ->
         AuthRequired <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:  "authChallenge"


instance ToJSON AuthRequired  where
    toJSON v = A.object
        [ "requestId" .= authRequiredRequestId v
        , "request" .= authRequiredRequest v
        , "frameId" .= authRequiredFrameId v
        , "resourceType" .= authRequiredResourceType v
        , "authChallenge" .= authRequiredAuthChallenge v
        ]



type RequestId = String

data RequestStage = RequestStageRequest | RequestStageResponse
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON RequestStage where
    parseJSON = A.withText  "RequestStage"  $ \v -> do
        pure $ case v of
                "Request" -> RequestStageRequest
                "Response" -> RequestStageResponse
                _ -> error "failed to parse RequestStage"

instance ToJSON RequestStage where
    toJSON v = A.String $
        case v of
                RequestStageRequest -> "Request"
                RequestStageResponse -> "Response"



data RequestPattern = RequestPattern {
    requestPatternUrlPattern :: Maybe String,
    requestPatternResourceType :: Maybe Network.ResourceType,
    requestPatternRequestStage :: Maybe RequestStage
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RequestPattern where
    parseJSON = A.withObject "RequestPattern" $ \v ->
         RequestPattern <$> v .:?  "urlPattern"
            <*> v  .:?  "resourceType"
            <*> v  .:?  "requestStage"


instance ToJSON RequestPattern  where
    toJSON v = A.object
        [ "urlPattern" .= requestPatternUrlPattern v
        , "resourceType" .= requestPatternResourceType v
        , "requestStage" .= requestPatternRequestStage v
        ]



data HeaderEntry = HeaderEntry {
    headerEntryName :: String,
    headerEntryValue :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  HeaderEntry where
    parseJSON = A.withObject "HeaderEntry" $ \v ->
         HeaderEntry <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON HeaderEntry  where
    toJSON v = A.object
        [ "name" .= headerEntryName v
        , "value" .= headerEntryValue v
        ]



data AuthChallenge = AuthChallenge {
    authChallengeOrigin :: String,
    authChallengeScheme :: String,
    authChallengeRealm :: String,
    authChallengeSource :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AuthChallenge where
    parseJSON = A.withObject "AuthChallenge" $ \v ->
         AuthChallenge <$> v .:  "origin"
            <*> v  .:  "scheme"
            <*> v  .:  "realm"
            <*> v  .:?  "source"


instance ToJSON AuthChallenge  where
    toJSON v = A.object
        [ "origin" .= authChallengeOrigin v
        , "scheme" .= authChallengeScheme v
        , "realm" .= authChallengeRealm v
        , "source" .= authChallengeSource v
        ]



data AuthChallengeResponse = AuthChallengeResponse {
    authChallengeResponseResponse :: String,
    authChallengeResponseUsername :: Maybe String,
    authChallengeResponsePassword :: Maybe String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AuthChallengeResponse where
    parseJSON = A.withObject "AuthChallengeResponse" $ \v ->
         AuthChallengeResponse <$> v .:  "response"
            <*> v  .:?  "username"
            <*> v  .:?  "password"


instance ToJSON AuthChallengeResponse  where
    toJSON v = A.object
        [ "response" .= authChallengeResponseResponse v
        , "username" .= authChallengeResponseUsername v
        , "password" .= authChallengeResponsePassword v
        ]



disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Fetch","disable") ([] ++ (catMaybes []))


enable :: Session a -> Maybe [RequestPattern] -> Maybe Bool -> IO (Maybe Error)
enable session enablePatterns enableHandleAuthRequests = sendReceiveCommand (conn session) ("Fetch","enable") ([] ++ (catMaybes [fmap (("patterns",) . ToJSONEx) enablePatterns, fmap (("handleAuthRequests",) . ToJSONEx) enableHandleAuthRequests]))


failRequest :: Session a -> RequestId -> Network.ErrorReason -> IO (Maybe Error)
failRequest session failRequestRequestId failRequestErrorReason = sendReceiveCommand (conn session) ("Fetch","failRequest") ([("requestId", ToJSONEx failRequestRequestId), ("errorReason", ToJSONEx failRequestErrorReason)] ++ (catMaybes []))


fulfillRequest :: Session a -> RequestId -> Int -> Maybe [HeaderEntry] -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
fulfillRequest session fulfillRequestRequestId fulfillRequestResponseCode fulfillRequestResponseHeaders fulfillRequestBinaryResponseHeaders fulfillRequestBody fulfillRequestResponsePhrase = sendReceiveCommand (conn session) ("Fetch","fulfillRequest") ([("requestId", ToJSONEx fulfillRequestRequestId), ("responseCode", ToJSONEx fulfillRequestResponseCode)] ++ (catMaybes [fmap (("responseHeaders",) . ToJSONEx) fulfillRequestResponseHeaders, fmap (("binaryResponseHeaders",) . ToJSONEx) fulfillRequestBinaryResponseHeaders, fmap (("body",) . ToJSONEx) fulfillRequestBody, fmap (("responsePhrase",) . ToJSONEx) fulfillRequestResponsePhrase]))


continueRequest :: Session a -> RequestId -> Maybe String -> Maybe String -> Maybe String -> Maybe [HeaderEntry] -> IO (Maybe Error)
continueRequest session continueRequestRequestId continueRequestUrl continueRequestMethod continueRequestPostData continueRequestHeaders = sendReceiveCommand (conn session) ("Fetch","continueRequest") ([("requestId", ToJSONEx continueRequestRequestId)] ++ (catMaybes [fmap (("url",) . ToJSONEx) continueRequestUrl, fmap (("method",) . ToJSONEx) continueRequestMethod, fmap (("postData",) . ToJSONEx) continueRequestPostData, fmap (("headers",) . ToJSONEx) continueRequestHeaders]))


continueWithAuth :: Session a -> RequestId -> AuthChallengeResponse -> IO (Maybe Error)
continueWithAuth session continueWithAuthRequestId continueWithAuthAuthChallengeResponse = sendReceiveCommand (conn session) ("Fetch","continueWithAuth") ([("requestId", ToJSONEx continueWithAuthRequestId), ("authChallengeResponse", ToJSONEx continueWithAuthAuthChallengeResponse)] ++ (catMaybes []))

data GetResponseBody = GetResponseBody {
    getResponseBodyBody :: String,
    getResponseBodyBase64Encoded :: Bool
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetResponseBody where
    parseJSON = A.withObject "GetResponseBody" $ \v ->
         GetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



getResponseBody :: Session a -> RequestId -> IO (Either Error GetResponseBody)
getResponseBody session getResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Fetch","getResponseBody") ([("requestId", ToJSONEx getResponseBodyRequestId)] ++ (catMaybes []))

data TakeResponseBodyAsStream = TakeResponseBodyAsStream {
    takeResponseBodyAsStreamStream :: IO.StreamHandle
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TakeResponseBodyAsStream where
    parseJSON = A.withObject "TakeResponseBodyAsStream" $ \v ->
         TakeResponseBodyAsStream <$> v .:  "stream"



takeResponseBodyAsStream :: Session a -> RequestId -> IO (Either Error TakeResponseBodyAsStream)
takeResponseBodyAsStream session takeResponseBodyAsStreamRequestId = sendReceiveCommandResult (conn session) ("Fetch","takeResponseBodyAsStream") ([("requestId", ToJSONEx takeResponseBodyAsStreamRequestId)] ++ (catMaybes []))


