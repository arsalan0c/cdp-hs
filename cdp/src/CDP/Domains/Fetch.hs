{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type FetchRequestId = String
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



data FetchRequestPattern = FetchRequestPattern {
   fetchRequestPatternUrlPattern :: Maybe String,
   fetchRequestPatternResourceType :: Maybe DOMPageNetworkEmulationSecurity.NetworkResourceType,
   fetchRequestPatternRequestStage :: Maybe FetchRequestStage
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data FetchHeaderEntry = FetchHeaderEntry {
   fetchHeaderEntryName :: String,
   fetchHeaderEntryValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchHeaderEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  FetchHeaderEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


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
   fetchAuthChallengeSource :: FetchAuthChallengeSource,
   fetchAuthChallengeOrigin :: String,
   fetchAuthChallengeScheme :: String,
   fetchAuthChallengeRealm :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallenge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallenge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


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
   fetchAuthChallengeResponseResponse :: FetchAuthChallengeResponseResponse,
   fetchAuthChallengeResponseUsername :: Maybe String,
   fetchAuthChallengeResponsePassword :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  FetchAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }





data FetchRequestPaused = FetchRequestPaused {
   fetchRequestPausedRequestId :: FetchRequestId,
   fetchRequestPausedRequest :: DOMPageNetworkEmulationSecurity.NetworkRequest,
   fetchRequestPausedFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   fetchRequestPausedResourceType :: DOMPageNetworkEmulationSecurity.NetworkResourceType,
   fetchRequestPausedResponseErrorReason :: Maybe DOMPageNetworkEmulationSecurity.NetworkErrorReason,
   fetchRequestPausedResponseStatusCode :: Maybe Int,
   fetchRequestPausedResponseStatusText :: Maybe String,
   fetchRequestPausedResponseHeaders :: Maybe [FetchHeaderEntry],
   fetchRequestPausedNetworkId :: Maybe FetchRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchRequestPaused  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  FetchRequestPaused where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data FetchAuthRequired = FetchAuthRequired {
   fetchAuthRequiredRequestId :: FetchRequestId,
   fetchAuthRequiredRequest :: DOMPageNetworkEmulationSecurity.NetworkRequest,
   fetchAuthRequiredFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   fetchAuthRequiredResourceType :: DOMPageNetworkEmulationSecurity.NetworkResourceType,
   fetchAuthRequiredAuthChallenge :: FetchAuthChallenge
} deriving (Generic, Eq, Show, Read)
instance ToJSON FetchAuthRequired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  FetchAuthRequired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }




fetchDisable :: Handle ev -> IO (Maybe Error)
fetchDisable handle = sendReceiveCommand handle "Fetch.disable" (Nothing :: Maybe ())



data PFetchEnable = PFetchEnable {
   pFetchEnablePatterns :: Maybe [FetchRequestPattern],
   pFetchEnableHandleAuthRequests :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PFetchEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }


fetchEnable :: Handle ev -> PFetchEnable -> IO (Maybe Error)
fetchEnable handle params = sendReceiveCommand handle "Fetch.enable" (Just params)



data PFetchFailRequest = PFetchFailRequest {
   pFetchFailRequestRequestId :: FetchRequestId,
   pFetchFailRequestErrorReason :: DOMPageNetworkEmulationSecurity.NetworkErrorReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFailRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PFetchFailRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


fetchFailRequest :: Handle ev -> PFetchFailRequest -> IO (Maybe Error)
fetchFailRequest handle params = sendReceiveCommand handle "Fetch.failRequest" (Just params)



data PFetchFulfillRequest = PFetchFulfillRequest {
   pFetchFulfillRequestRequestId :: FetchRequestId,
   pFetchFulfillRequestResponseCode :: Int,
   pFetchFulfillRequestResponseHeaders :: Maybe [FetchHeaderEntry],
   pFetchFulfillRequestBinaryResponseHeaders :: Maybe String,
   pFetchFulfillRequestBody :: Maybe String,
   pFetchFulfillRequestResponsePhrase :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchFulfillRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PFetchFulfillRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


fetchFulfillRequest :: Handle ev -> PFetchFulfillRequest -> IO (Maybe Error)
fetchFulfillRequest handle params = sendReceiveCommand handle "Fetch.fulfillRequest" (Just params)



data PFetchContinueRequest = PFetchContinueRequest {
   pFetchContinueRequestRequestId :: FetchRequestId,
   pFetchContinueRequestUrl :: Maybe String,
   pFetchContinueRequestMethod :: Maybe String,
   pFetchContinueRequestPostData :: Maybe String,
   pFetchContinueRequestHeaders :: Maybe [FetchHeaderEntry],
   pFetchContinueRequestInterceptResponse :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


fetchContinueRequest :: Handle ev -> PFetchContinueRequest -> IO (Maybe Error)
fetchContinueRequest handle params = sendReceiveCommand handle "Fetch.continueRequest" (Just params)



data PFetchContinueWithAuth = PFetchContinueWithAuth {
   pFetchContinueWithAuthRequestId :: FetchRequestId,
   pFetchContinueWithAuthAuthChallengeResponse :: FetchAuthChallengeResponse
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueWithAuth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueWithAuth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


fetchContinueWithAuth :: Handle ev -> PFetchContinueWithAuth -> IO (Maybe Error)
fetchContinueWithAuth handle params = sendReceiveCommand handle "Fetch.continueWithAuth" (Just params)



data PFetchContinueResponse = PFetchContinueResponse {
   pFetchContinueResponseRequestId :: FetchRequestId,
   pFetchContinueResponseResponseCode :: Maybe Int,
   pFetchContinueResponseResponsePhrase :: Maybe String,
   pFetchContinueResponseResponseHeaders :: Maybe [FetchHeaderEntry],
   pFetchContinueResponseBinaryResponseHeaders :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchContinueResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PFetchContinueResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


fetchContinueResponse :: Handle ev -> PFetchContinueResponse -> IO (Maybe Error)
fetchContinueResponse handle params = sendReceiveCommand handle "Fetch.continueResponse" (Just params)



data PFetchGetResponseBody = PFetchGetResponseBody {
   pFetchGetResponseBodyRequestId :: FetchRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PFetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


fetchGetResponseBody :: Handle ev -> PFetchGetResponseBody -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody handle params = sendReceiveCommandResult handle "Fetch.getResponseBody" (Just params)

data FetchGetResponseBody = FetchGetResponseBody {
   fetchGetResponseBodyBody :: String,
   fetchGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command FetchGetResponseBody where
   commandName _ = "Fetch.getResponseBody"




data PFetchTakeResponseBodyAsStream = PFetchTakeResponseBodyAsStream {
   pFetchTakeResponseBodyAsStreamRequestId :: FetchRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PFetchTakeResponseBodyAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PFetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


fetchTakeResponseBodyAsStream :: Handle ev -> PFetchTakeResponseBodyAsStream -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream handle params = sendReceiveCommandResult handle "Fetch.takeResponseBodyAsStream" (Just params)

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
   fetchTakeResponseBodyAsStreamStream :: IO.IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  FetchTakeResponseBodyAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command FetchTakeResponseBodyAsStream where
   commandName _ = "Fetch.takeResponseBodyAsStream"




