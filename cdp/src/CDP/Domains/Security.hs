{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Security (module CDP.Domains.Security) where

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



type SecurityCertificateId = Int
data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
   parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> do
      case v of
         "blockable" -> pure SecurityMixedContentTypeBlockable
         "optionally-blockable" -> pure SecurityMixedContentTypeOptionallyBlockable
         "none" -> pure SecurityMixedContentTypeNone
         _ -> fail "failed to parse SecurityMixedContentType"

instance ToJSON SecurityMixedContentType where
   toJSON v = A.String $
      case v of
         SecurityMixedContentTypeBlockable -> "blockable"
         SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
         SecurityMixedContentTypeNone -> "none"


data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecuritySecurityState where
   parseJSON = A.withText  "SecuritySecurityState"  $ \v -> do
      case v of
         "unknown" -> pure SecuritySecurityStateUnknown
         "neutral" -> pure SecuritySecurityStateNeutral
         "insecure" -> pure SecuritySecurityStateInsecure
         "secure" -> pure SecuritySecurityStateSecure
         "info" -> pure SecuritySecurityStateInfo
         "insecure-broken" -> pure SecuritySecurityStateInsecureBroken
         _ -> fail "failed to parse SecuritySecurityState"

instance ToJSON SecuritySecurityState where
   toJSON v = A.String $
      case v of
         SecuritySecurityStateUnknown -> "unknown"
         SecuritySecurityStateNeutral -> "neutral"
         SecuritySecurityStateInsecure -> "insecure"
         SecuritySecurityStateSecure -> "secure"
         SecuritySecurityStateInfo -> "info"
         SecuritySecurityStateInsecureBroken -> "insecure-broken"



data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
   securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
   securitySecurityStateExplanationTitle :: String,
   securitySecurityStateExplanationSummary :: String,
   securitySecurityStateExplanationDescription :: String,
   securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
   securitySecurityStateExplanationCertificate :: [String],
   securitySecurityStateExplanationRecommendations :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySecurityStateExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecuritySecurityStateExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityCertificateErrorAction where
   parseJSON = A.withText  "SecurityCertificateErrorAction"  $ \v -> do
      case v of
         "continue" -> pure SecurityCertificateErrorActionContinue
         "cancel" -> pure SecurityCertificateErrorActionCancel
         _ -> fail "failed to parse SecurityCertificateErrorAction"

instance ToJSON SecurityCertificateErrorAction where
   toJSON v = A.String $
      case v of
         SecurityCertificateErrorActionContinue -> "continue"
         SecurityCertificateErrorActionCancel -> "cancel"






securityDisable :: Handle ev -> IO (Maybe Error)
securityDisable handle = sendReceiveCommand handle "Security.disable" (Nothing :: Maybe ())


securityEnable :: Handle ev -> IO (Maybe Error)
securityEnable handle = sendReceiveCommand handle "Security.enable" (Nothing :: Maybe ())



