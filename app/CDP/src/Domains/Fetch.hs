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

module Domains.Fetch (module Domains.Fetch) where

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

import qualified Domains.Network as Network
import qualified Domains.IO as IO
import qualified Domains.Page as Page


data FetchEvent = EVFetchRequestPaused FetchRequestPaused | EVFetchAuthRequired FetchAuthRequired
    deriving (Eq, Show, Read)

data FetchRequestPaused = FetchRequestPaused {
    fetchRequestPausedRequestId :: FetchRequestId,
    fetchRequestPausedRequest :: NetworkRequest,
    fetchRequestPausedFrameId :: PageFrameId,
    fetchRequestPausedResourceType :: NetworkResourceType,
    fetchRequestPausedResponseErrorReason :: Maybe NetworkErrorReason,
    fetchRequestPausedResponseStatusCode :: Maybe Int,
    fetchRequestPausedResponseStatusText :: Maybe String,
    fetchRequestPausedResponseHeaders :: Maybe [FetchHeaderEntry],
    fetchRequestPausedNetworkId :: Maybe FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  FetchRequestPaused where
    parseJSON = A.withObject "FetchRequestPaused" $ \v ->
         FetchRequestPaused <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:?  "responseErrorReason"
            <*> v  .:?  "responseStatusCode"
            <*> v  .:?  "responseStatusText"
            <*> v  .:?  "responseHeaders"
            <*> v  .:?  "networkId"


instance ToJSON FetchRequestPaused  where
    toJSON v = A.object
        [ "requestId" .= fetchRequestPausedRequestId v
        , "request" .= fetchRequestPausedRequest v
        , "frameId" .= fetchRequestPausedFrameId v
        , "resourceType" .= fetchRequestPausedResourceType v
        , "responseErrorReason" .= fetchRequestPausedResponseErrorReason v
        , "responseStatusCode" .= fetchRequestPausedResponseStatusCode v
        , "responseStatusText" .= fetchRequestPausedResponseStatusText v
        , "responseHeaders" .= fetchRequestPausedResponseHeaders v
        , "networkId" .= fetchRequestPausedNetworkId v
        ]


instance FromEvent FetchEvent FetchRequestPaused where
    eventName  _ _    =  "Fetch.requestPaused"
    fromEvent ev =  case ev of EVFetchRequestPaused v -> Just v; _ -> Nothing


data FetchAuthRequired = FetchAuthRequired {
    fetchAuthRequiredRequestId :: FetchRequestId,
    fetchAuthRequiredRequest :: NetworkRequest,
    fetchAuthRequiredFrameId :: PageFrameId,
    fetchAuthRequiredResourceType :: NetworkResourceType,
    fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthRequired where
    parseJSON = A.withObject "FetchAuthRequired" $ \v ->
         FetchAuthRequired <$> v .:  "requestId"
            <*> v  .:  "request"
            <*> v  .:  "frameId"
            <*> v  .:  "resourceType"
            <*> v  .:  "authChallenge"


instance ToJSON FetchAuthRequired  where
    toJSON v = A.object
        [ "requestId" .= fetchAuthRequiredRequestId v
        , "request" .= fetchAuthRequiredRequest v
        , "frameId" .= fetchAuthRequiredFrameId v
        , "resourceType" .= fetchAuthRequiredResourceType v
        , "authChallenge" .= fetchAuthRequiredAuthChallenge v
        ]


instance FromEvent FetchEvent FetchAuthRequired where
    eventName  _ _    =  "Fetch.authRequired"
    fromEvent ev =  case ev of EVFetchAuthRequired v -> Just v; _ -> Nothing




subscribe :: forall a. FromEvent FetchEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy FetchEvent
    pa       = Proxy :: Proxy a


type FetchRequestId = String

data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
    deriving (Eq, Show, Read)
instance FromJSON FetchRequestStage where
    parseJSON = A.withText  "FetchRequestStage"  $ \v -> do
        case v of
                "Request" -> pure $ FetchRequestStageRequest
                "Response" -> pure $ FetchRequestStageResponse
                _ -> fail "failed to parse FetchRequestStage"

instance ToJSON FetchRequestStage where
    toJSON v = A.String $
        case v of
                FetchRequestStageRequest -> "Request"
                FetchRequestStageResponse -> "Response"



data FetchRequestPattern = FetchRequestPattern {
    fetchRequestPatternUrlPattern :: Maybe String,
    fetchRequestPatternResourceType :: Maybe NetworkResourceType,
    fetchRequestPatternRequestStage :: Maybe FetchRequestStage
} deriving (Eq, Show, Read)
instance FromJSON  FetchRequestPattern where
    parseJSON = A.withObject "FetchRequestPattern" $ \v ->
         FetchRequestPattern <$> v .:?  "urlPattern"
            <*> v  .:?  "resourceType"
            <*> v  .:?  "requestStage"


instance ToJSON FetchRequestPattern  where
    toJSON v = A.object
        [ "urlPattern" .= fetchRequestPatternUrlPattern v
        , "resourceType" .= fetchRequestPatternResourceType v
        , "requestStage" .= fetchRequestPatternRequestStage v
        ]



data FetchHeaderEntry = FetchHeaderEntry {
    fetchHeaderEntryName :: String,
    fetchHeaderEntryValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  FetchHeaderEntry where
    parseJSON = A.withObject "FetchHeaderEntry" $ \v ->
         FetchHeaderEntry <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON FetchHeaderEntry  where
    toJSON v = A.object
        [ "name" .= fetchHeaderEntryName v
        , "value" .= fetchHeaderEntryValue v
        ]



data FetchAuthChallenge = FetchAuthChallenge {
    fetchAuthChallengeOrigin :: String,
    fetchAuthChallengeScheme :: String,
    fetchAuthChallengeRealm :: String,
    fetchAuthChallengeSource :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthChallenge where
    parseJSON = A.withObject "FetchAuthChallenge" $ \v ->
         FetchAuthChallenge <$> v .:  "origin"
            <*> v  .:  "scheme"
            <*> v  .:  "realm"
            <*> v  .:?  "source"


instance ToJSON FetchAuthChallenge  where
    toJSON v = A.object
        [ "origin" .= fetchAuthChallengeOrigin v
        , "scheme" .= fetchAuthChallengeScheme v
        , "realm" .= fetchAuthChallengeRealm v
        , "source" .= fetchAuthChallengeSource v
        ]



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
    fetchAuthChallengeResponseResponse :: String,
    fetchAuthChallengeResponseUsername :: Maybe String,
    fetchAuthChallengeResponsePassword :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  FetchAuthChallengeResponse where
    parseJSON = A.withObject "FetchAuthChallengeResponse" $ \v ->
         FetchAuthChallengeResponse <$> v .:  "response"
            <*> v  .:?  "username"
            <*> v  .:?  "password"


instance ToJSON FetchAuthChallengeResponse  where
    toJSON v = A.object
        [ "response" .= fetchAuthChallengeResponseResponse v
        , "username" .= fetchAuthChallengeResponseUsername v
        , "password" .= fetchAuthChallengeResponsePassword v
        ]






fetchDisable :: Session -> IO (Maybe Error)
fetchDisable session = sendReceiveCommand session "Fetch.disable" (Nothing :: Maybe ())



data PFetchEnable = PFetchEnable {
    pFetchEnablePatterns :: Maybe [FetchRequestPattern],
    pFetchEnableHandleAuthRequests :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PFetchEnable where
    parseJSON = A.withObject "PFetchEnable" $ \v ->
         PFetchEnable <$> v .:?  "patterns"
            <*> v  .:?  "handleAuthRequests"


instance ToJSON PFetchEnable  where
    toJSON v = A.object
        [ "patterns" .= pFetchEnablePatterns v
        , "handleAuthRequests" .= pFetchEnableHandleAuthRequests v
        ]


fetchEnable :: Session -> PFetchEnable -> IO (Maybe Error)
fetchEnable session params = sendReceiveCommand session "Fetch.enable" (Just params)



data PFetchFailRequest = PFetchFailRequest {
    pFetchFailRequestRequestId :: FetchRequestId,
    pFetchFailRequestErrorReason :: NetworkErrorReason
} deriving (Eq, Show, Read)
instance FromJSON  PFetchFailRequest where
    parseJSON = A.withObject "PFetchFailRequest" $ \v ->
         PFetchFailRequest <$> v .:  "requestId"
            <*> v  .:  "errorReason"


instance ToJSON PFetchFailRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchFailRequestRequestId v
        , "errorReason" .= pFetchFailRequestErrorReason v
        ]


fetchFailRequest :: Session -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest session params = sendReceiveCommand session "Fetch.failRequest" (Just params)



data PFetchFulfillRequest = PFetchFulfillRequest {
    pFetchFulfillRequestRequestId :: FetchRequestId,
    pFetchFulfillRequestResponseCode :: Int,
    pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
    pFetchFulfillRequestBinaryResponseHeaders :: Maybe String,
    pFetchFulfillRequestBody :: Maybe String,
    pFetchFulfillRequestResponsePhrase :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PFetchFulfillRequest where
    parseJSON = A.withObject "PFetchFulfillRequest" $ \v ->
         PFetchFulfillRequest <$> v .:  "requestId"
            <*> v  .:  "responseCode"
            <*> v  .:?  "responseHeaders"
            <*> v  .:?  "binaryResponseHeaders"
            <*> v  .:?  "body"
            <*> v  .:?  "responsePhrase"


instance ToJSON PFetchFulfillRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchFulfillRequestRequestId v
        , "responseCode" .= pFetchFulfillRequestResponseCode v
        , "responseHeaders" .= pFetchFulfillRequestResponseHeaders v
        , "binaryResponseHeaders" .= pFetchFulfillRequestBinaryResponseHeaders v
        , "body" .= pFetchFulfillRequestBody v
        , "responsePhrase" .= pFetchFulfillRequestResponsePhrase v
        ]


fetchFulfillRequest :: Session -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest session params = sendReceiveCommand session "Fetch.fulfillRequest" (Just params)



data PFetchContinueRequest = PFetchContinueRequest {
    pFetchContinueRequestRequestId :: FetchRequestId,
    pFetchContinueRequestUrl :: Maybe String,
    pFetchContinueRequestMethod :: Maybe String,
    pFetchContinueRequestPostData :: Maybe String,
    pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry]
} deriving (Eq, Show, Read)
instance FromJSON  PFetchContinueRequest where
    parseJSON = A.withObject "PFetchContinueRequest" $ \v ->
         PFetchContinueRequest <$> v .:  "requestId"
            <*> v  .:?  "url"
            <*> v  .:?  "method"
            <*> v  .:?  "postData"
            <*> v  .:?  "headers"


instance ToJSON PFetchContinueRequest  where
    toJSON v = A.object
        [ "requestId" .= pFetchContinueRequestRequestId v
        , "url" .= pFetchContinueRequestUrl v
        , "method" .= pFetchContinueRequestMethod v
        , "postData" .= pFetchContinueRequestPostData v
        , "headers" .= pFetchContinueRequestHeaders v
        ]


fetchContinueRequest :: Session -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest session params = sendReceiveCommand session "Fetch.continueRequest" (Just params)



data PFetchContinueWithAuth = PFetchContinueWithAuth {
    pFetchContinueWithAuthRequestId :: FetchRequestId,
    pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
} deriving (Eq, Show, Read)
instance FromJSON  PFetchContinueWithAuth where
    parseJSON = A.withObject "PFetchContinueWithAuth" $ \v ->
         PFetchContinueWithAuth <$> v .:  "requestId"
            <*> v  .:  "authChallengeResponse"


instance ToJSON PFetchContinueWithAuth  where
    toJSON v = A.object
        [ "requestId" .= pFetchContinueWithAuthRequestId v
        , "authChallengeResponse" .= pFetchContinueWithAuthAuthChallengeResponse v
        ]


fetchContinueWithAuth :: Session -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth session params = sendReceiveCommand session "Fetch.continueWithAuth" (Just params)

data FetchGetResponseBody = FetchGetResponseBody {
    fetchGetResponseBodyBody :: String,
    fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  FetchGetResponseBody where
    parseJSON = A.withObject "FetchGetResponseBody" $ \v ->
         FetchGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



instance Command  FetchGetResponseBody where
    commandName _ = "Fetch.getResponseBody"

data PFetchGetResponseBody = PFetchGetResponseBody {
    pFetchGetResponseBodyRequestId :: FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PFetchGetResponseBody where
    parseJSON = A.withObject "PFetchGetResponseBody" $ \v ->
         PFetchGetResponseBody <$> v .:  "requestId"


instance ToJSON PFetchGetResponseBody  where
    toJSON v = A.object
        [ "requestId" .= pFetchGetResponseBodyRequestId v
        ]


fetchGetResponseBody :: Session -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody session params = sendReceiveCommandResult session "Fetch.getResponseBody" (Just params)

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
    fetchTakeResponseBodyAsStreamStream :: IOStreamHandle
} deriving (Eq, Show, Read)
instance FromJSON  FetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "FetchTakeResponseBodyAsStream" $ \v ->
         FetchTakeResponseBodyAsStream <$> v .:  "stream"



instance Command  FetchTakeResponseBodyAsStream where
    commandName _ = "Fetch.takeResponseBodyAsStream"

data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
    pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
} deriving (Eq, Show, Read)
instance FromJSON  PFetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "PFetchTakeResponseBodyAsStream" $ \v ->
         PFetchTakeResponseBodyAsStream <$> v .:  "requestId"


instance ToJSON PFetchTakeResponseBodyAsStream  where
    toJSON v = A.object
        [ "requestId" .= pFetchTakeResponseBodyAsStreamRequestId v
        ]


fetchTakeResponseBodyAsStream :: Session -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream session params = sendReceiveCommandResult session "Fetch.takeResponseBodyAsStream" (Just params)

