{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Security (module Domains.Security) where
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



import Utils


type CertificateId = Int

data MixedContentType = MixedContentTypeBlockable | MixedContentTypeOptionallyBlockable | MixedContentTypeNone
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON MixedContentType where
    parseJSON = A.withText  "MixedContentType"  $ \v -> do
        pure $ case v of
                "blockable" -> MixedContentTypeBlockable
                "optionally-blockable" -> MixedContentTypeOptionallyBlockable
                "none" -> MixedContentTypeNone
                _ -> error "failed to parse MixedContentType"

instance ToJSON MixedContentType where
    toJSON v = A.String $
        case v of
                MixedContentTypeBlockable -> "blockable"
                MixedContentTypeOptionallyBlockable -> "optionally-blockable"
                MixedContentTypeNone -> "none"



data SecurityState = SecurityStateUnknown | SecurityStateNeutral | SecurityStateInsecure | SecurityStateSecure | SecurityStateInfo | SecurityStateInsecureBroken
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON SecurityState where
    parseJSON = A.withText  "SecurityState"  $ \v -> do
        pure $ case v of
                "unknown" -> SecurityStateUnknown
                "neutral" -> SecurityStateNeutral
                "insecure" -> SecurityStateInsecure
                "secure" -> SecurityStateSecure
                "info" -> SecurityStateInfo
                "insecure-broken" -> SecurityStateInsecureBroken
                _ -> error "failed to parse SecurityState"

instance ToJSON SecurityState where
    toJSON v = A.String $
        case v of
                SecurityStateUnknown -> "unknown"
                SecurityStateNeutral -> "neutral"
                SecurityStateInsecure -> "insecure"
                SecurityStateSecure -> "secure"
                SecurityStateInfo -> "info"
                SecurityStateInsecureBroken -> "insecure-broken"



data SecurityStateExplanation = SecurityStateExplanation {
    securityStateExplanationSecurityState :: SecurityState,
    securityStateExplanationTitle :: String,
    securityStateExplanationSummary :: String,
    securityStateExplanationDescription :: String,
    securityStateExplanationMixedContentType :: MixedContentType,
    securityStateExplanationCertificate :: [String],
    securityStateExplanationRecommendations :: Maybe [String]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SecurityStateExplanation where
    parseJSON = A.withObject "SecurityStateExplanation" $ \v ->
         SecurityStateExplanation <$> v .:  "securityState"
            <*> v  .:  "title"
            <*> v  .:  "summary"
            <*> v  .:  "description"
            <*> v  .:  "mixedContentType"
            <*> v  .:  "certificate"
            <*> v  .:?  "recommendations"


instance ToJSON SecurityStateExplanation  where
    toJSON v = A.object
        [ "securityState" .= securityStateExplanationSecurityState v
        , "title" .= securityStateExplanationTitle v
        , "summary" .= securityStateExplanationSummary v
        , "description" .= securityStateExplanationDescription v
        , "mixedContentType" .= securityStateExplanationMixedContentType v
        , "certificate" .= securityStateExplanationCertificate v
        , "recommendations" .= securityStateExplanationRecommendations v
        ]



data CertificateErrorAction = CertificateErrorActionContinue | CertificateErrorActionCancel
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON CertificateErrorAction where
    parseJSON = A.withText  "CertificateErrorAction"  $ \v -> do
        pure $ case v of
                "continue" -> CertificateErrorActionContinue
                "cancel" -> CertificateErrorActionCancel
                _ -> error "failed to parse CertificateErrorAction"

instance ToJSON CertificateErrorAction where
    toJSON v = A.String $
        case v of
                CertificateErrorActionContinue -> "continue"
                CertificateErrorActionCancel -> "cancel"



disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("Security","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("Security","enable") ([] ++ (catMaybes []))


