{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Fetch

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.IO as IO


-- | Type 'Fetch.RequestId'.
--   Unique request identifier.
type FetchRequestId = T.Text

-- | Type 'Fetch.RequestStage'.
--   Stages of the request to handle. Request will intercept before the request is
--   sent. Response will intercept after the response is received (but before response
--   body is received).
data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
  deriving (Ord, Eq, Show, Read)
instance FromJSON FetchRequestStage where
  parseJSON = A.withText "FetchRequestStage" $ \v -> case v of
    "Request" -> pure FetchRequestStageRequest
    "Response" -> pure FetchRequestStageResponse
    "_" -> fail "failed to parse FetchRequestStage"
instance ToJSON FetchRequestStage where
  toJSON v = A.String $ case v of
    FetchRequestStageRequest -> "Request"
    FetchRequestStageResponse -> "Response"

-- | Type 'Fetch.RequestPattern'.
data FetchRequestPattern = FetchRequestPattern
  {
    -- | Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
    --   backslash. Omitting is equivalent to `"*"`.
    fetchRequestPatternUrlPattern :: Maybe T.Text,
    -- | If set, only requests for matching resource types will be intercepted.
    fetchRequestPatternResourceType :: Maybe DOMPageNetworkEmulationSecurity.NetworkResourceType,
    -- | Stage at which to begin intercepting requests. Default is Request.
    fetchRequestPatternRequestStage :: Maybe FetchRequestStage
  }
  deriving (Eq, Show)
instance FromJSON FetchRequestPattern where
  parseJSON = A.withObject "FetchRequestPattern" $ \o -> FetchRequestPattern
    <$> o A..:? "urlPattern"
    <*> o A..:? "resourceType"
    <*> o A..:? "requestStage"
instance ToJSON FetchRequestPattern where
  toJSON p = A.object $ catMaybes [
    ("urlPattern" A..=) <$> (fetchRequestPatternUrlPattern p),
    ("resourceType" A..=) <$> (fetchRequestPatternResourceType p),
    ("requestStage" A..=) <$> (fetchRequestPatternRequestStage p)
    ]

-- | Type 'Fetch.HeaderEntry'.
--   Response HTTP header entry
data FetchHeaderEntry = FetchHeaderEntry
  {
    fetchHeaderEntryName :: T.Text,
    fetchHeaderEntryValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON FetchHeaderEntry where
  parseJSON = A.withObject "FetchHeaderEntry" $ \o -> FetchHeaderEntry
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON FetchHeaderEntry where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (fetchHeaderEntryName p),
    ("value" A..=) <$> Just (fetchHeaderEntryValue p)
    ]

-- | Type 'Fetch.AuthChallenge'.
--   Authorization challenge for HTTP status code 401 or 407.
data FetchAuthChallengeSource = FetchAuthChallengeSourceServer | FetchAuthChallengeSourceProxy
  deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeSource where
  parseJSON = A.withText "FetchAuthChallengeSource" $ \v -> case v of
    "Server" -> pure FetchAuthChallengeSourceServer
    "Proxy" -> pure FetchAuthChallengeSourceProxy
    "_" -> fail "failed to parse FetchAuthChallengeSource"
instance ToJSON FetchAuthChallengeSource where
  toJSON v = A.String $ case v of
    FetchAuthChallengeSourceServer -> "Server"
    FetchAuthChallengeSourceProxy -> "Proxy"
data FetchAuthChallenge = FetchAuthChallenge
  {
    -- | Source of the authentication challenge.
    fetchAuthChallengeSource :: Maybe FetchAuthChallengeSource,
    -- | Origin of the challenger.
    fetchAuthChallengeOrigin :: T.Text,
    -- | The authentication scheme used, such as basic or digest
    fetchAuthChallengeScheme :: T.Text,
    -- | The realm of the challenge. May be empty.
    fetchAuthChallengeRealm :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON FetchAuthChallenge where
  parseJSON = A.withObject "FetchAuthChallenge" $ \o -> FetchAuthChallenge
    <$> o A..:? "source"
    <*> o A..: "origin"
    <*> o A..: "scheme"
    <*> o A..: "realm"
instance ToJSON FetchAuthChallenge where
  toJSON p = A.object $ catMaybes [
    ("source" A..=) <$> (fetchAuthChallengeSource p),
    ("origin" A..=) <$> Just (fetchAuthChallengeOrigin p),
    ("scheme" A..=) <$> Just (fetchAuthChallengeScheme p),
    ("realm" A..=) <$> Just (fetchAuthChallengeRealm p)
    ]

-- | Type 'Fetch.AuthChallengeResponse'.
--   Response to an AuthChallenge.
data FetchAuthChallengeResponseResponse = FetchAuthChallengeResponseResponseDefault | FetchAuthChallengeResponseResponseCancelAuth | FetchAuthChallengeResponseResponseProvideCredentials
  deriving (Ord, Eq, Show, Read)
instance FromJSON FetchAuthChallengeResponseResponse where
  parseJSON = A.withText "FetchAuthChallengeResponseResponse" $ \v -> case v of
    "Default" -> pure FetchAuthChallengeResponseResponseDefault
    "CancelAuth" -> pure FetchAuthChallengeResponseResponseCancelAuth
    "ProvideCredentials" -> pure FetchAuthChallengeResponseResponseProvideCredentials
    "_" -> fail "failed to parse FetchAuthChallengeResponseResponse"
instance ToJSON FetchAuthChallengeResponseResponse where
  toJSON v = A.String $ case v of
    FetchAuthChallengeResponseResponseDefault -> "Default"
    FetchAuthChallengeResponseResponseCancelAuth -> "CancelAuth"
    FetchAuthChallengeResponseResponseProvideCredentials -> "ProvideCredentials"
data FetchAuthChallengeResponse = FetchAuthChallengeResponse
  {
    -- | The decision on what to do in response to the authorization challenge.  Default means
    --   deferring to the default behavior of the net stack, which will likely either the Cancel
    --   authentication or display a popup dialog box.
    fetchAuthChallengeResponseResponse :: FetchAuthChallengeResponseResponse,
    -- | The username to provide, possibly empty. Should only be set if response is
    --   ProvideCredentials.
    fetchAuthChallengeResponseUsername :: Maybe T.Text,
    -- | The password to provide, possibly empty. Should only be set if response is
    --   ProvideCredentials.
    fetchAuthChallengeResponsePassword :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON FetchAuthChallengeResponse where
  parseJSON = A.withObject "FetchAuthChallengeResponse" $ \o -> FetchAuthChallengeResponse
    <$> o A..: "response"
    <*> o A..:? "username"
    <*> o A..:? "password"
instance ToJSON FetchAuthChallengeResponse where
  toJSON p = A.object $ catMaybes [
    ("response" A..=) <$> Just (fetchAuthChallengeResponseResponse p),
    ("username" A..=) <$> (fetchAuthChallengeResponseUsername p),
    ("password" A..=) <$> (fetchAuthChallengeResponsePassword p)
    ]

-- | Type of the 'Fetch.requestPaused' event.
data FetchRequestPaused = FetchRequestPaused
  {
    -- | Each request the page makes will have a unique id.
    fetchRequestPausedRequestId :: FetchRequestId,
    -- | The details of the request.
    fetchRequestPausedRequest :: DOMPageNetworkEmulationSecurity.NetworkRequest,
    -- | The id of the frame that initiated the request.
    fetchRequestPausedFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | How the requested resource will be used.
    fetchRequestPausedResourceType :: DOMPageNetworkEmulationSecurity.NetworkResourceType,
    -- | Response error if intercepted at response stage.
    fetchRequestPausedResponseErrorReason :: Maybe DOMPageNetworkEmulationSecurity.NetworkErrorReason,
    -- | Response code if intercepted at response stage.
    fetchRequestPausedResponseStatusCode :: Maybe Int,
    -- | Response status text if intercepted at response stage.
    fetchRequestPausedResponseStatusText :: Maybe T.Text,
    -- | Response headers if intercepted at the response stage.
    fetchRequestPausedResponseHeaders :: Maybe [FetchHeaderEntry],
    -- | If the intercepted request had a corresponding Network.requestWillBeSent event fired for it,
    --   then this networkId will be the same as the requestId present in the requestWillBeSent event.
    fetchRequestPausedNetworkId :: Maybe DOMPageNetworkEmulationSecurity.NetworkRequestId,
    -- | If the request is due to a redirect response from the server, the id of the request that
    --   has caused the redirect.
    fetchRequestPausedRedirectedRequestId :: Maybe FetchRequestId
  }
  deriving (Eq, Show)
instance FromJSON FetchRequestPaused where
  parseJSON = A.withObject "FetchRequestPaused" $ \o -> FetchRequestPaused
    <$> o A..: "requestId"
    <*> o A..: "request"
    <*> o A..: "frameId"
    <*> o A..: "resourceType"
    <*> o A..:? "responseErrorReason"
    <*> o A..:? "responseStatusCode"
    <*> o A..:? "responseStatusText"
    <*> o A..:? "responseHeaders"
    <*> o A..:? "networkId"
    <*> o A..:? "redirectedRequestId"
instance Event FetchRequestPaused where
  eventName _ = "Fetch.requestPaused"

-- | Type of the 'Fetch.authRequired' event.
data FetchAuthRequired = FetchAuthRequired
  {
    -- | Each request the page makes will have a unique id.
    fetchAuthRequiredRequestId :: FetchRequestId,
    -- | The details of the request.
    fetchAuthRequiredRequest :: DOMPageNetworkEmulationSecurity.NetworkRequest,
    -- | The id of the frame that initiated the request.
    fetchAuthRequiredFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | How the requested resource will be used.
    fetchAuthRequiredResourceType :: DOMPageNetworkEmulationSecurity.NetworkResourceType,
    -- | Details of the Authorization Challenge encountered.
    --   If this is set, client should respond with continueRequest that
    --   contains AuthChallengeResponse.
    fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
  }
  deriving (Eq, Show)
instance FromJSON FetchAuthRequired where
  parseJSON = A.withObject "FetchAuthRequired" $ \o -> FetchAuthRequired
    <$> o A..: "requestId"
    <*> o A..: "request"
    <*> o A..: "frameId"
    <*> o A..: "resourceType"
    <*> o A..: "authChallenge"
instance Event FetchAuthRequired where
  eventName _ = "Fetch.authRequired"

-- | Disables the fetch domain.

-- | Parameters of the 'Fetch.disable' command.
data PFetchDisable = PFetchDisable
  deriving (Eq, Show)
pFetchDisable
  :: PFetchDisable
pFetchDisable
  = PFetchDisable
instance ToJSON PFetchDisable where
  toJSON _ = A.Null
instance Command PFetchDisable where
  type CommandResponse PFetchDisable = ()
  commandName _ = "Fetch.disable"
  fromJSON = const . A.Success . const ()

-- | Enables issuing of requestPaused events. A request will be paused until client
--   calls one of failRequest, fulfillRequest or continueRequest/continueWithAuth.

-- | Parameters of the 'Fetch.enable' command.
data PFetchEnable = PFetchEnable
  {
    -- | If specified, only requests matching any of these patterns will produce
    --   fetchRequested event and will be paused until clients response. If not set,
    --   all requests will be affected.
    pFetchEnablePatterns :: Maybe [FetchRequestPattern],
    -- | If true, authRequired events will be issued and requests will be paused
    --   expecting a call to continueWithAuth.
    pFetchEnableHandleAuthRequests :: Maybe Bool
  }
  deriving (Eq, Show)
pFetchEnable
  :: PFetchEnable
pFetchEnable
  = PFetchEnable
    Nothing
    Nothing
instance ToJSON PFetchEnable where
  toJSON p = A.object $ catMaybes [
    ("patterns" A..=) <$> (pFetchEnablePatterns p),
    ("handleAuthRequests" A..=) <$> (pFetchEnableHandleAuthRequests p)
    ]
instance Command PFetchEnable where
  type CommandResponse PFetchEnable = ()
  commandName _ = "Fetch.enable"
  fromJSON = const . A.Success . const ()

-- | Causes the request to fail with specified reason.

-- | Parameters of the 'Fetch.failRequest' command.
data PFetchFailRequest = PFetchFailRequest
  {
    -- | An id the client received in requestPaused event.
    pFetchFailRequestRequestId :: FetchRequestId,
    -- | Causes the request to fail with the given reason.
    pFetchFailRequestErrorReason :: DOMPageNetworkEmulationSecurity.NetworkErrorReason
  }
  deriving (Eq, Show)
pFetchFailRequest
  {-
  -- | An id the client received in requestPaused event.
  -}
  :: FetchRequestId
  {-
  -- | Causes the request to fail with the given reason.
  -}
  -> DOMPageNetworkEmulationSecurity.NetworkErrorReason
  -> PFetchFailRequest
pFetchFailRequest
  arg_pFetchFailRequestRequestId
  arg_pFetchFailRequestErrorReason
  = PFetchFailRequest
    arg_pFetchFailRequestRequestId
    arg_pFetchFailRequestErrorReason
instance ToJSON PFetchFailRequest where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchFailRequestRequestId p),
    ("errorReason" A..=) <$> Just (pFetchFailRequestErrorReason p)
    ]
instance Command PFetchFailRequest where
  type CommandResponse PFetchFailRequest = ()
  commandName _ = "Fetch.failRequest"
  fromJSON = const . A.Success . const ()

-- | Provides response to the request.

-- | Parameters of the 'Fetch.fulfillRequest' command.
data PFetchFulfillRequest = PFetchFulfillRequest
  {
    -- | An id the client received in requestPaused event.
    pFetchFulfillRequestRequestId :: FetchRequestId,
    -- | An HTTP response code.
    pFetchFulfillRequestResponseCode :: Int,
    -- | Response headers.
    pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
    -- | Alternative way of specifying response headers as a \0-separated
    --   series of name: value pairs. Prefer the above method unless you
    --   need to represent some non-UTF8 values that can't be transmitted
    --   over the protocol as text. (Encoded as a base64 string when passed over JSON)
    pFetchFulfillRequestBinaryResponseHeaders :: Maybe T.Text,
    -- | A response body. If absent, original response body will be used if
    --   the request is intercepted at the response stage and empty body
    --   will be used if the request is intercepted at the request stage. (Encoded as a base64 string when passed over JSON)
    pFetchFulfillRequestBody :: Maybe T.Text,
    -- | A textual representation of responseCode.
    --   If absent, a standard phrase matching responseCode is used.
    pFetchFulfillRequestResponsePhrase :: Maybe T.Text
  }
  deriving (Eq, Show)
pFetchFulfillRequest
  {-
  -- | An id the client received in requestPaused event.
  -}
  :: FetchRequestId
  {-
  -- | An HTTP response code.
  -}
  -> Int
  -> PFetchFulfillRequest
pFetchFulfillRequest
  arg_pFetchFulfillRequestRequestId
  arg_pFetchFulfillRequestResponseCode
  = PFetchFulfillRequest
    arg_pFetchFulfillRequestRequestId
    arg_pFetchFulfillRequestResponseCode
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PFetchFulfillRequest where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchFulfillRequestRequestId p),
    ("responseCode" A..=) <$> Just (pFetchFulfillRequestResponseCode p),
    ("responseHeaders" A..=) <$> (pFetchFulfillRequestResponseHeaders p),
    ("binaryResponseHeaders" A..=) <$> (pFetchFulfillRequestBinaryResponseHeaders p),
    ("body" A..=) <$> (pFetchFulfillRequestBody p),
    ("responsePhrase" A..=) <$> (pFetchFulfillRequestResponsePhrase p)
    ]
instance Command PFetchFulfillRequest where
  type CommandResponse PFetchFulfillRequest = ()
  commandName _ = "Fetch.fulfillRequest"
  fromJSON = const . A.Success . const ()

-- | Continues the request, optionally modifying some of its parameters.

-- | Parameters of the 'Fetch.continueRequest' command.
data PFetchContinueRequest = PFetchContinueRequest
  {
    -- | An id the client received in requestPaused event.
    pFetchContinueRequestRequestId :: FetchRequestId,
    -- | If set, the request url will be modified in a way that's not observable by page.
    pFetchContinueRequestUrl :: Maybe T.Text,
    -- | If set, the request method is overridden.
    pFetchContinueRequestMethod :: Maybe T.Text,
    -- | If set, overrides the post data in the request. (Encoded as a base64 string when passed over JSON)
    pFetchContinueRequestPostData :: Maybe T.Text,
    -- | If set, overrides the request headers. Note that the overrides do not
    --   extend to subsequent redirect hops, if a redirect happens. Another override
    --   may be applied to a different request produced by a redirect.
    pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry],
    -- | If set, overrides response interception behavior for this request.
    pFetchContinueRequestInterceptResponse :: Maybe Bool
  }
  deriving (Eq, Show)
pFetchContinueRequest
  {-
  -- | An id the client received in requestPaused event.
  -}
  :: FetchRequestId
  -> PFetchContinueRequest
pFetchContinueRequest
  arg_pFetchContinueRequestRequestId
  = PFetchContinueRequest
    arg_pFetchContinueRequestRequestId
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PFetchContinueRequest where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchContinueRequestRequestId p),
    ("url" A..=) <$> (pFetchContinueRequestUrl p),
    ("method" A..=) <$> (pFetchContinueRequestMethod p),
    ("postData" A..=) <$> (pFetchContinueRequestPostData p),
    ("headers" A..=) <$> (pFetchContinueRequestHeaders p),
    ("interceptResponse" A..=) <$> (pFetchContinueRequestInterceptResponse p)
    ]
instance Command PFetchContinueRequest where
  type CommandResponse PFetchContinueRequest = ()
  commandName _ = "Fetch.continueRequest"
  fromJSON = const . A.Success . const ()

-- | Continues a request supplying authChallengeResponse following authRequired event.

-- | Parameters of the 'Fetch.continueWithAuth' command.
data PFetchContinueWithAuth = PFetchContinueWithAuth
  {
    -- | An id the client received in authRequired event.
    pFetchContinueWithAuthRequestId :: FetchRequestId,
    -- | Response to  with an authChallenge.
    pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
  }
  deriving (Eq, Show)
pFetchContinueWithAuth
  {-
  -- | An id the client received in authRequired event.
  -}
  :: FetchRequestId
  {-
  -- | Response to  with an authChallenge.
  -}
  -> FetchAuthChallengeResponse
  -> PFetchContinueWithAuth
pFetchContinueWithAuth
  arg_pFetchContinueWithAuthRequestId
  arg_pFetchContinueWithAuthAuthChallengeResponse
  = PFetchContinueWithAuth
    arg_pFetchContinueWithAuthRequestId
    arg_pFetchContinueWithAuthAuthChallengeResponse
instance ToJSON PFetchContinueWithAuth where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchContinueWithAuthRequestId p),
    ("authChallengeResponse" A..=) <$> Just (pFetchContinueWithAuthAuthChallengeResponse p)
    ]
instance Command PFetchContinueWithAuth where
  type CommandResponse PFetchContinueWithAuth = ()
  commandName _ = "Fetch.continueWithAuth"
  fromJSON = const . A.Success . const ()

-- | Continues loading of the paused response, optionally modifying the
--   response headers. If either responseCode or headers are modified, all of them
--   must be present.

-- | Parameters of the 'Fetch.continueResponse' command.
data PFetchContinueResponse = PFetchContinueResponse
  {
    -- | An id the client received in requestPaused event.
    pFetchContinueResponseRequestId :: FetchRequestId,
    -- | An HTTP response code. If absent, original response code will be used.
    pFetchContinueResponseResponseCode :: Maybe Int,
    -- | A textual representation of responseCode.
    --   If absent, a standard phrase matching responseCode is used.
    pFetchContinueResponseResponsePhrase :: Maybe T.Text,
    -- | Response headers. If absent, original response headers will be used.
    pFetchContinueResponseResponseHeaders :: Maybe [FetchHeaderEntry],
    -- | Alternative way of specifying response headers as a \0-separated
    --   series of name: value pairs. Prefer the above method unless you
    --   need to represent some non-UTF8 values that can't be transmitted
    --   over the protocol as text. (Encoded as a base64 string when passed over JSON)
    pFetchContinueResponseBinaryResponseHeaders :: Maybe T.Text
  }
  deriving (Eq, Show)
pFetchContinueResponse
  {-
  -- | An id the client received in requestPaused event.
  -}
  :: FetchRequestId
  -> PFetchContinueResponse
pFetchContinueResponse
  arg_pFetchContinueResponseRequestId
  = PFetchContinueResponse
    arg_pFetchContinueResponseRequestId
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PFetchContinueResponse where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchContinueResponseRequestId p),
    ("responseCode" A..=) <$> (pFetchContinueResponseResponseCode p),
    ("responsePhrase" A..=) <$> (pFetchContinueResponseResponsePhrase p),
    ("responseHeaders" A..=) <$> (pFetchContinueResponseResponseHeaders p),
    ("binaryResponseHeaders" A..=) <$> (pFetchContinueResponseBinaryResponseHeaders p)
    ]
instance Command PFetchContinueResponse where
  type CommandResponse PFetchContinueResponse = ()
  commandName _ = "Fetch.continueResponse"
  fromJSON = const . A.Success . const ()

-- | Causes the body of the response to be received from the server and
--   returned as a single string. May only be issued for a request that
--   is paused in the Response stage and is mutually exclusive with
--   takeResponseBodyForInterceptionAsStream. Calling other methods that
--   affect the request or disabling fetch domain before body is received
--   results in an undefined behavior.

-- | Parameters of the 'Fetch.getResponseBody' command.
data PFetchGetResponseBody = PFetchGetResponseBody
  {
    -- | Identifier for the intercepted request to get body for.
    pFetchGetResponseBodyRequestId :: FetchRequestId
  }
  deriving (Eq, Show)
pFetchGetResponseBody
  {-
  -- | Identifier for the intercepted request to get body for.
  -}
  :: FetchRequestId
  -> PFetchGetResponseBody
pFetchGetResponseBody
  arg_pFetchGetResponseBodyRequestId
  = PFetchGetResponseBody
    arg_pFetchGetResponseBodyRequestId
instance ToJSON PFetchGetResponseBody where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchGetResponseBodyRequestId p)
    ]
data FetchGetResponseBody = FetchGetResponseBody
  {
    -- | Response body.
    fetchGetResponseBodyBody :: T.Text,
    -- | True, if content was sent as base64.
    fetchGetResponseBodyBase64Encoded :: Bool
  }
  deriving (Eq, Show)
instance FromJSON FetchGetResponseBody where
  parseJSON = A.withObject "FetchGetResponseBody" $ \o -> FetchGetResponseBody
    <$> o A..: "body"
    <*> o A..: "base64Encoded"
instance Command PFetchGetResponseBody where
  type CommandResponse PFetchGetResponseBody = FetchGetResponseBody
  commandName _ = "Fetch.getResponseBody"

-- | Returns a handle to the stream representing the response body.
--   The request must be paused in the HeadersReceived stage.
--   Note that after this command the request can't be continued
--   as is -- client either needs to cancel it or to provide the
--   response body.
--   The stream only supports sequential read, IO.read will fail if the position
--   is specified.
--   This method is mutually exclusive with getResponseBody.
--   Calling other methods that affect the request or disabling fetch
--   domain before body is received results in an undefined behavior.

-- | Parameters of the 'Fetch.takeResponseBodyAsStream' command.
data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream
  {
    pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
  }
  deriving (Eq, Show)
pFetchTakeResponseBodyAsStream
  :: FetchRequestId
  -> PFetchTakeResponseBodyAsStream
pFetchTakeResponseBodyAsStream
  arg_pFetchTakeResponseBodyAsStreamRequestId
  = PFetchTakeResponseBodyAsStream
    arg_pFetchTakeResponseBodyAsStreamRequestId
instance ToJSON PFetchTakeResponseBodyAsStream where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pFetchTakeResponseBodyAsStreamRequestId p)
    ]
data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream
  {
    fetchTakeResponseBodyAsStreamStream :: IO.IOStreamHandle
  }
  deriving (Eq, Show)
instance FromJSON FetchTakeResponseBodyAsStream where
  parseJSON = A.withObject "FetchTakeResponseBodyAsStream" $ \o -> FetchTakeResponseBodyAsStream
    <$> o A..: "stream"
instance Command PFetchTakeResponseBodyAsStream where
  type CommandResponse PFetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream
  commandName _ = "Fetch.takeResponseBodyAsStream"

