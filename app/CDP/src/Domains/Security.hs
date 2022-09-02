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

module Domains.Security (module Domains.Security) where

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



data SecurityEvent = 
    deriving (Eq, Show, Read)



subscribe :: forall a. FromEvent SecurityEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy SecurityEvent
    pa       = Proxy :: Proxy a


type SecurityCertificateId = Int

data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
    deriving (Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
    parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> do
        case v of
                "blockable" -> pure $ SecurityMixedContentTypeBlockable
                "optionally-blockable" -> pure $ SecurityMixedContentTypeOptionallyBlockable
                "none" -> pure $ SecurityMixedContentTypeNone
                _ -> fail "failed to parse SecurityMixedContentType"

instance ToJSON SecurityMixedContentType where
    toJSON v = A.String $
        case v of
                SecurityMixedContentTypeBlockable -> "blockable"
                SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
                SecurityMixedContentTypeNone -> "none"



data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
    deriving (Eq, Show, Read)
instance FromJSON SecuritySecurityState where
    parseJSON = A.withText  "SecuritySecurityState"  $ \v -> do
        case v of
                "unknown" -> pure $ SecuritySecurityStateUnknown
                "neutral" -> pure $ SecuritySecurityStateNeutral
                "insecure" -> pure $ SecuritySecurityStateInsecure
                "secure" -> pure $ SecuritySecurityStateSecure
                "info" -> pure $ SecuritySecurityStateInfo
                "insecure-broken" -> pure $ SecuritySecurityStateInsecureBroken
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
} deriving (Eq, Show, Read)
instance FromJSON  SecuritySecurityStateExplanation where
    parseJSON = A.withObject "SecuritySecurityStateExplanation" $ \v ->
         SecuritySecurityStateExplanation <$> v .:  "securityState"
            <*> v  .:  "title"
            <*> v  .:  "summary"
            <*> v  .:  "description"
            <*> v  .:  "mixedContentType"
            <*> v  .:  "certificate"
            <*> v  .:?  "recommendations"


instance ToJSON SecuritySecurityStateExplanation  where
    toJSON v = A.object
        [ "securityState" .= securitySecurityStateExplanationSecurityState v
        , "title" .= securitySecurityStateExplanationTitle v
        , "summary" .= securitySecurityStateExplanationSummary v
        , "description" .= securitySecurityStateExplanationDescription v
        , "mixedContentType" .= securitySecurityStateExplanationMixedContentType v
        , "certificate" .= securitySecurityStateExplanationCertificate v
        , "recommendations" .= securitySecurityStateExplanationRecommendations v
        ]



data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
    deriving (Eq, Show, Read)
instance FromJSON SecurityCertificateErrorAction where
    parseJSON = A.withText  "SecurityCertificateErrorAction"  $ \v -> do
        case v of
                "continue" -> pure $ SecurityCertificateErrorActionContinue
                "cancel" -> pure $ SecurityCertificateErrorActionCancel
                _ -> fail "failed to parse SecurityCertificateErrorAction"

instance ToJSON SecurityCertificateErrorAction where
    toJSON v = A.String $
        case v of
                SecurityCertificateErrorActionContinue -> "continue"
                SecurityCertificateErrorActionCancel -> "cancel"






securityDisable :: Session -> IO (Maybe Error)
securityDisable session = sendReceiveCommand session "Security.disable" (Nothing :: Maybe ())




securityEnable :: Session -> IO (Maybe Error)
securityEnable session = sendReceiveCommand session "Security.enable" (Nothing :: Maybe ())

