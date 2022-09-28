{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Fetch :
     A domain for letting clients substitute browser's network layer with client code.

-}


module CDP.Domains.Fetch (module CDP.Domains.Fetch) where

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
import CDP.Domains.IO as IO


-- | Unique request identifier.
type FetchRequestId = String

-- | Stages of the request to handle. Request will intercept before the request is
-- sent. Response will intercept after the response is received (but before response
-- body is received).
data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
   deriving (Ord, Eq, Show, Read)
instance FromJSON FetchRequestStage where
   parseJSON = A.withText  "FetchRequestStage"  $ \v -> do
      case v of
         "Request" -> pure FetchRequestStageRequest
         "Response" -> pure FetchRequestStageResponse
         _ -> fail "failed to parse FetchRequestStage"

instance ToJSON FetchRequestStage where
   toJSON v = A.String $
      case v of
         FetchRequestStageRequest -> "Request"
         FetchRequestStageResponse -> "Response"



-- | Type 'Fetch.RequestPattern' .
data FetchRequestPattern = FetchRequestPattern {
   fetchRequestPatternUrlPattern :: FetchRequestPatternUrlPattern, -- ^ Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
backslash. Omitting is equivalent to `"*"`.
   fetchRequestPatternResourceType :: FetchRequestPatternResourceType, -- ^ If set, only requests for matching resource types will be intercepted.
   fetchRequestPatternRequestStage :: FetchRequestPatternRequestStage -- ^ Stage at which to begin intercepting requests. Default is Request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Response HTTP header entry
data FetchHeaderEntry = FetchHeaderEntry {


} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchHeaderEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  FetchHeaderEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Authorization challenge for HTTP status code 401 or 407.
data FetchAuthChallengeSource = FetchAuthChallengeSourceServer | FetchAuthChallengeSourceProxy
   deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeSource where
   parseJSON = A.withText  "FetchAuthChallengeSource"  $ \v -> do
      case v of
         "Server" -> pure FetchAuthChallengeSourceServer
         "Proxy" -> pure FetchAuthChallengeSourceProxy
         _ -> fail "failed to parse FetchAuthChallengeSource"

instance ToJSON FetchAuthChallengeSource where
   toJSON v = A.String $
      case v of
         FetchAuthChallengeSourceServer -> "Server"
         FetchAuthChallengeSourceProxy -> "Proxy"



data FetchAuthChallenge = FetchAuthChallenge {
   fetchAuthChallengeSource :: FetchAuthChallengeSource, -- ^ Source of the authentication challenge.
   fetchAuthChallengeOrigin :: FetchAuthChallengeOrigin, -- ^ Origin of the challenger.
   fetchAuthChallengeScheme :: FetchAuthChallengeScheme, -- ^ The authentication scheme used, such as basic or digest
   fetchAuthChallengeRealm :: FetchAuthChallengeRealm -- ^ The realm of the challenge. May be empty.
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallenge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallenge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Response to an AuthChallenge.
data FetchAuthChallengeResponseResponse = FetchAuthChallengeResponseResponseDefault | FetchAuthChallengeResponseResponseCancelAuth | FetchAuthChallengeResponseResponseProvideCredentials
   deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeResponseResponse where
   parseJSON = A.withText  "FetchAuthChallengeResponseResponse"  $ \v -> do
      case v of
         "Default" -> pure FetchAuthChallengeResponseResponseDefault
         "CancelAuth" -> pure FetchAuthChallengeResponseResponseCancelAuth
         "ProvideCredentials" -> pure FetchAuthChallengeResponseResponseProvideCredentials
         _ -> fail "failed to parse FetchAuthChallengeResponseResponse"

instance ToJSON FetchAuthChallengeResponseResponse where
   toJSON v = A.String $
      case v of
         FetchAuthChallengeResponseResponseDefault -> "Default"
         FetchAuthChallengeResponseResponseCancelAuth -> "CancelAuth"
         FetchAuthChallengeResponseResponseProvideCredentials -> "ProvideCredentials"



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
   fetchAuthChallengeResponseResponse :: FetchAuthChallengeResponseResponse, -- ^ The decision on what to do in response to the authorization challenge.  Default means
deferring to the default behavior of the net stack, which will likely either the Cancel
authentication or display a popup dialog box.
   fetchAuthChallengeResponseUsername :: FetchAuthChallengeResponseUsername, -- ^ The username to provide, possibly empty. Should only be set if response is
ProvideCredentials.
   fetchAuthChallengeResponsePassword :: FetchAuthChallengeResponsePassword -- ^ The password to provide, possibly empty. Should only be set if response is
ProvideCredentials.
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }





-- | Type of the 'Fetch.requestPaused' event.
data FetchRequestPaused = FetchRequestPaused {
   fetchRequestPausedRequestId :: FetchRequestPausedRequestId, -- ^ Each request the page makes will have a unique id.
   fetchRequestPausedRequest :: FetchRequestPausedRequest, -- ^ The details of the request.
   fetchRequestPausedFrameId :: FetchRequestPausedFrameId, -- ^ The id of the frame that initiated the request.
   fetchRequestPausedResourceType :: FetchRequestPausedResourceType, -- ^ How the requested resource will be used.
   fetchRequestPausedResponseErrorReason :: FetchRequestPausedResponseErrorReason, -- ^ Response error if intercepted at response stage.
   fetchRequestPausedResponseStatusCode :: FetchRequestPausedResponseStatusCode, -- ^ Response code if intercepted at response stage.
   fetchRequestPausedResponseStatusText :: FetchRequestPausedResponseStatusText, -- ^ Response status text if intercepted at response stage.
   fetchRequestPausedResponseHeaders :: FetchRequestPausedResponseHeaders, -- ^ Response headers if intercepted at the response stage.
   fetchRequestPausedNetworkId :: FetchRequestPausedNetworkId -- ^ If the intercepted request had a corresponding Network.requestWillBeSent event fired for it,
then this networkId will be the same as the requestId present in the requestWillBeSent event.
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Fetch.authRequired' event.
data FetchAuthRequired = FetchAuthRequired {
   fetchAuthRequiredRequestId :: FetchAuthRequiredRequestId, -- ^ Each request the page makes will have a unique id.
   fetchAuthRequiredRequest :: FetchAuthRequiredRequest, -- ^ The details of the request.
   fetchAuthRequiredFrameId :: FetchAuthRequiredFrameId, -- ^ The id of the frame that initiated the request.
   fetchAuthRequiredResourceType :: FetchAuthRequiredResourceType, -- ^ How the requested resource will be used.
   fetchAuthRequiredAuthChallenge :: FetchAuthRequiredAuthChallenge -- ^ Details of the Authorization Challenge encountered.
If this is set, client should respond with continueRequest that
contains AuthChallengeResponse.
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthRequired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  FetchAuthRequired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }





-- | Function for the command 'Fetch.disable'.
-- Disables the fetch domain.
fetchDisable :: Handle ev -> IO (Maybe Error)
fetchDisable handle = sendReceiveCommand handle "Fetch.disable" (Nothing :: Maybe ())


-- | Parameters of the 'fetchEnable' command.
data PFetchEnable = PFetchEnable {
   pFetchEnablePatterns :: PFetchEnablePatterns, -- ^ If specified, only requests matching any of these patterns will produce
fetchRequested event and will be paused until clients response. If not set,
all requests will be affected.
   pFetchEnableHandleAuthRequests :: PFetchEnableHandleAuthRequests -- ^ If true, authRequired events will be issued and requests will be paused
expecting a call to continueWithAuth.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PFetchEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


-- | Function for the command 'Fetch.enable'.
-- Enables issuing of requestPaused events. A request will be paused until client
-- calls one of failRequest, fulfillRequest or continueRequest/continueWithAuth.
-- Parameters: 'PFetchEnable'
fetchEnable :: Handle ev -> PFetchEnable -> IO (Maybe Error)
fetchEnable handle params = sendReceiveCommand handle "Fetch.enable" (Just params)


-- | Parameters of the 'fetchFailRequest' command.
data PFetchFailRequest = PFetchFailRequest {
   pFetchFailRequestRequestId :: PFetchFailRequestRequestId, -- ^ An id the client received in requestPaused event.
   pFetchFailRequestErrorReason :: PFetchFailRequestErrorReason -- ^ Causes the request to fail with the given reason.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFailRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PFetchFailRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Fetch.failRequest'.
-- Causes the request to fail with specified reason.
-- Parameters: 'PFetchFailRequest'
fetchFailRequest :: Handle ev -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest handle params = sendReceiveCommand handle "Fetch.failRequest" (Just params)


-- | Parameters of the 'fetchFulfillRequest' command.
data PFetchFulfillRequest = PFetchFulfillRequest {
   pFetchFulfillRequestRequestId :: PFetchFulfillRequestRequestId, -- ^ An id the client received in requestPaused event.
   pFetchFulfillRequestResponseCode :: PFetchFulfillRequestResponseCode, -- ^ An HTTP response code.
   pFetchFulfillRequestResponseHeaders :: PFetchFulfillRequestResponseHeaders, -- ^ Response headers.
   pFetchFulfillRequestBinaryResponseHeaders :: PFetchFulfillRequestBinaryResponseHeaders, -- ^ Alternative way of specifying response headers as a \0-separated
series of name: value pairs. Prefer the above method unless you
need to represent some non-UTF8 values that can't be transmitted
over the protocol as text. (Encoded as a base64 string when passed over JSON)
   pFetchFulfillRequestBody :: PFetchFulfillRequestBody, -- ^ A response body. If absent, original response body will be used if
the request is intercepted at the response stage and empty body
will be used if the request is intercepted at the request stage. (Encoded as a base64 string when passed over JSON)
   pFetchFulfillRequestResponsePhrase :: PFetchFulfillRequestResponsePhrase -- ^ A textual representation of responseCode.
If absent, a standard phrase matching responseCode is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFulfillRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PFetchFulfillRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Fetch.fulfillRequest'.
-- Provides response to the request.
-- Parameters: 'PFetchFulfillRequest'
fetchFulfillRequest :: Handle ev -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest handle params = sendReceiveCommand handle "Fetch.fulfillRequest" (Just params)


-- | Parameters of the 'fetchContinueRequest' command.
data PFetchContinueRequest = PFetchContinueRequest {
   pFetchContinueRequestRequestId :: PFetchContinueRequestRequestId, -- ^ An id the client received in requestPaused event.
   pFetchContinueRequestUrl :: PFetchContinueRequestUrl, -- ^ If set, the request url will be modified in a way that's not observable by page.
   pFetchContinueRequestMethod :: PFetchContinueRequestMethod, -- ^ If set, the request method is overridden.
   pFetchContinueRequestPostData :: PFetchContinueRequestPostData, -- ^ If set, overrides the post data in the request. (Encoded as a base64 string when passed over JSON)
   pFetchContinueRequestHeaders :: PFetchContinueRequestHeaders, -- ^ If set, overrides the request headers.
   pFetchContinueRequestInterceptResponse :: PFetchContinueRequestInterceptResponse -- ^ If set, overrides response interception behavior for this request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Fetch.continueRequest'.
-- Continues the request, optionally modifying some of its parameters.
-- Parameters: 'PFetchContinueRequest'
fetchContinueRequest :: Handle ev -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest handle params = sendReceiveCommand handle "Fetch.continueRequest" (Just params)


-- | Parameters of the 'fetchContinueWithAuth' command.
data PFetchContinueWithAuth = PFetchContinueWithAuth {
   pFetchContinueWithAuthRequestId :: PFetchContinueWithAuthRequestId, -- ^ An id the client received in authRequired event.
   pFetchContinueWithAuthAuthChallengeResponse :: PFetchContinueWithAuthAuthChallengeResponse -- ^ Response to  with an authChallenge.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueWithAuth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueWithAuth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Fetch.continueWithAuth'.
-- Continues a request supplying authChallengeResponse following authRequired event.
-- Parameters: 'PFetchContinueWithAuth'
fetchContinueWithAuth :: Handle ev -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth handle params = sendReceiveCommand handle "Fetch.continueWithAuth" (Just params)


-- | Parameters of the 'fetchContinueResponse' command.
data PFetchContinueResponse = PFetchContinueResponse {
   pFetchContinueResponseRequestId :: PFetchContinueResponseRequestId, -- ^ An id the client received in requestPaused event.
   pFetchContinueResponseResponseCode :: PFetchContinueResponseResponseCode, -- ^ An HTTP response code. If absent, original response code will be used.
   pFetchContinueResponseResponsePhrase :: PFetchContinueResponseResponsePhrase, -- ^ A textual representation of responseCode.
If absent, a standard phrase matching responseCode is used.
   pFetchContinueResponseResponseHeaders :: PFetchContinueResponseResponseHeaders, -- ^ Response headers. If absent, original response headers will be used.
   pFetchContinueResponseBinaryResponseHeaders :: PFetchContinueResponseBinaryResponseHeaders -- ^ Alternative way of specifying response headers as a \0-separated
series of name: value pairs. Prefer the above method unless you
need to represent some non-UTF8 values that can't be transmitted
over the protocol as text. (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Fetch.continueResponse'.
-- Continues loading of the paused response, optionally modifying the
-- response headers. If either responseCode or headers are modified, all of them
-- must be present.
-- Parameters: 'PFetchContinueResponse'
fetchContinueResponse :: Handle ev -> PFetchContinueResponse -> IO (Maybe Error)
fetchContinueResponse handle params = sendReceiveCommand handle "Fetch.continueResponse" (Just params)


-- | Parameters of the 'fetchGetResponseBody' command.
data PFetchGetResponseBody = PFetchGetResponseBody {
   pFetchGetResponseBodyRequestId :: PFetchGetResponseBodyRequestId -- ^ Identifier for the intercepted request to get body for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Fetch.getResponseBody'.
-- Causes the body of the response to be received from the server and
-- returned as a single string. May only be issued for a request that
-- is paused in the Response stage and is mutually exclusive with
-- takeResponseBodyForInterceptionAsStream. Calling other methods that
-- affect the request or disabling fetch domain before body is received
-- results in an undefined behavior.
-- Parameters: 'PFetchGetResponseBody'
-- Returns: 'FetchGetResponseBody'
fetchGetResponseBody :: Handle ev -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody handle params = sendReceiveCommandResult handle "Fetch.getResponseBody" (Just params)

-- | Return type of the 'fetchGetResponseBody' command.
data FetchGetResponseBody = FetchGetResponseBody {
   fetchGetResponseBodyBody :: String, -- ^ Response body.
   fetchGetResponseBodyBase64Encoded :: Bool -- ^ True, if content was sent as base64.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command FetchGetResponseBody where
   commandName _ = "Fetch.getResponseBody"



-- | Parameters of the 'fetchTakeResponseBodyAsStream' command.
data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchTakeResponseBodyAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PFetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Fetch.takeResponseBodyAsStream'.
-- Returns a handle to the stream representing the response body.
-- The request must be paused in the HeadersReceived stage.
-- Note that after this command the request can't be continued
-- as is -- client either needs to cancel it or to provide the
-- response body.
-- The stream only supports sequential read, IO.read will fail if the position
-- is specified.
-- This method is mutually exclusive with getResponseBody.
-- Calling other methods that affect the request or disabling fetch
-- domain before body is received results in an undefined behavior.
-- Parameters: 'PFetchTakeResponseBodyAsStream'
-- Returns: 'FetchTakeResponseBodyAsStream'
fetchTakeResponseBodyAsStream :: Handle ev -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream handle params = sendReceiveCommandResult handle "Fetch.takeResponseBodyAsStream" (Just params)

-- | Return type of the 'fetchTakeResponseBodyAsStream' command.
data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command FetchTakeResponseBodyAsStream where
   commandName _ = "Fetch.takeResponseBodyAsStream"




