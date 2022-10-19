{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Audits

Audits domain allows investigation of page violations and possible improvements.
-}


module CDP.Domains.Audits (module CDP.Domains.Audits) where

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
import CDP.Domains.Runtime as Runtime


-- | Type 'Audits.AffectedCookie'.
--   Information about a cookie that is affected by an inspector issue.
data AuditsAffectedCookie = AuditsAffectedCookie
  {
    -- | The following three properties uniquely identify a cookie
    auditsAffectedCookieName :: String,
    auditsAffectedCookiePath :: String,
    auditsAffectedCookieDomain :: String
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedCookie where
  parseJSON = A.withObject "AuditsAffectedCookie" $ \o -> AuditsAffectedCookie
    <$> o A..: "name"
    <*> o A..: "path"
    <*> o A..: "domain"
instance ToJSON AuditsAffectedCookie where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (auditsAffectedCookieName p),
    ("path" A..=) <$> Just (auditsAffectedCookiePath p),
    ("domain" A..=) <$> Just (auditsAffectedCookieDomain p)
    ]

-- | Type 'Audits.AffectedRequest'.
--   Information about a request that is affected by an inspector issue.
data AuditsAffectedRequest = AuditsAffectedRequest
  {
    -- | The unique request id.
    auditsAffectedRequestRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
    auditsAffectedRequestUrl :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedRequest where
  parseJSON = A.withObject "AuditsAffectedRequest" $ \o -> AuditsAffectedRequest
    <$> o A..: "requestId"
    <*> o A..:? "url"
instance ToJSON AuditsAffectedRequest where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (auditsAffectedRequestRequestId p),
    ("url" A..=) <$> (auditsAffectedRequestUrl p)
    ]

-- | Type 'Audits.AffectedFrame'.
--   Information about the frame affected by an inspector issue.
data AuditsAffectedFrame = AuditsAffectedFrame
  {
    auditsAffectedFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON AuditsAffectedFrame where
  parseJSON = A.withObject "AuditsAffectedFrame" $ \o -> AuditsAffectedFrame
    <$> o A..: "frameId"
instance ToJSON AuditsAffectedFrame where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (auditsAffectedFrameFrameId p)
    ]

-- | Type 'Audits.CookieExclusionReason'.
data AuditsCookieExclusionReason = AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax | AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure | AuditsCookieExclusionReasonExcludeSameSiteLax | AuditsCookieExclusionReasonExcludeSameSiteStrict | AuditsCookieExclusionReasonExcludeInvalidSameParty | AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext | AuditsCookieExclusionReasonExcludeDomainNonASCII
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieExclusionReason where
  parseJSON = A.withText "AuditsCookieExclusionReason" $ \v -> case v of
    "ExcludeSameSiteUnspecifiedTreatedAsLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax
    "ExcludeSameSiteNoneInsecure" -> pure AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure
    "ExcludeSameSiteLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteLax
    "ExcludeSameSiteStrict" -> pure AuditsCookieExclusionReasonExcludeSameSiteStrict
    "ExcludeInvalidSameParty" -> pure AuditsCookieExclusionReasonExcludeInvalidSameParty
    "ExcludeSamePartyCrossPartyContext" -> pure AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext
    "ExcludeDomainNonASCII" -> pure AuditsCookieExclusionReasonExcludeDomainNonASCII
    "_" -> fail "failed to parse AuditsCookieExclusionReason"
instance ToJSON AuditsCookieExclusionReason where
  toJSON v = A.String $ case v of
    AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax -> "ExcludeSameSiteUnspecifiedTreatedAsLax"
    AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure -> "ExcludeSameSiteNoneInsecure"
    AuditsCookieExclusionReasonExcludeSameSiteLax -> "ExcludeSameSiteLax"
    AuditsCookieExclusionReasonExcludeSameSiteStrict -> "ExcludeSameSiteStrict"
    AuditsCookieExclusionReasonExcludeInvalidSameParty -> "ExcludeInvalidSameParty"
    AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext -> "ExcludeSamePartyCrossPartyContext"
    AuditsCookieExclusionReasonExcludeDomainNonASCII -> "ExcludeDomainNonASCII"

-- | Type 'Audits.CookieWarningReason'.
data AuditsCookieWarningReason = AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext | AuditsCookieWarningReasonWarnSameSiteNoneInsecure | AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe | AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax | AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize | AuditsCookieWarningReasonWarnDomainNonASCII
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieWarningReason where
  parseJSON = A.withText "AuditsCookieWarningReason" $ \v -> case v of
    "WarnSameSiteUnspecifiedCrossSiteContext" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext
    "WarnSameSiteNoneInsecure" -> pure AuditsCookieWarningReasonWarnSameSiteNoneInsecure
    "WarnSameSiteUnspecifiedLaxAllowUnsafe" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe
    "WarnSameSiteStrictLaxDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict
    "WarnSameSiteStrictCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict
    "WarnSameSiteStrictCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax
    "WarnSameSiteLaxCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict
    "WarnSameSiteLaxCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax
    "WarnAttributeValueExceedsMaxSize" -> pure AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize
    "WarnDomainNonASCII" -> pure AuditsCookieWarningReasonWarnDomainNonASCII
    "_" -> fail "failed to parse AuditsCookieWarningReason"
instance ToJSON AuditsCookieWarningReason where
  toJSON v = A.String $ case v of
    AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext -> "WarnSameSiteUnspecifiedCrossSiteContext"
    AuditsCookieWarningReasonWarnSameSiteNoneInsecure -> "WarnSameSiteNoneInsecure"
    AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe -> "WarnSameSiteUnspecifiedLaxAllowUnsafe"
    AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict -> "WarnSameSiteStrictLaxDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict -> "WarnSameSiteStrictCrossDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax -> "WarnSameSiteStrictCrossDowngradeLax"
    AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict -> "WarnSameSiteLaxCrossDowngradeStrict"
    AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax -> "WarnSameSiteLaxCrossDowngradeLax"
    AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize -> "WarnAttributeValueExceedsMaxSize"
    AuditsCookieWarningReasonWarnDomainNonASCII -> "WarnDomainNonASCII"

-- | Type 'Audits.CookieOperation'.
data AuditsCookieOperation = AuditsCookieOperationSetCookie | AuditsCookieOperationReadCookie
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieOperation where
  parseJSON = A.withText "AuditsCookieOperation" $ \v -> case v of
    "SetCookie" -> pure AuditsCookieOperationSetCookie
    "ReadCookie" -> pure AuditsCookieOperationReadCookie
    "_" -> fail "failed to parse AuditsCookieOperation"
instance ToJSON AuditsCookieOperation where
  toJSON v = A.String $ case v of
    AuditsCookieOperationSetCookie -> "SetCookie"
    AuditsCookieOperationReadCookie -> "ReadCookie"

-- | Type 'Audits.CookieIssueDetails'.
--   This information is currently necessary, as the front-end has a difficult
--   time finding a specific cookie. With this, we can convey specific error
--   information without the cookie.
data AuditsCookieIssueDetails = AuditsCookieIssueDetails
  {
    -- | If AffectedCookie is not set then rawCookieLine contains the raw
    --   Set-Cookie header string. This hints at a problem where the
    --   cookie line is syntactically or semantically malformed in a way
    --   that no valid cookie could be created.
    auditsCookieIssueDetailsCookie :: Maybe AuditsAffectedCookie,
    auditsCookieIssueDetailsRawCookieLine :: Maybe String,
    auditsCookieIssueDetailsCookieWarningReasons :: [AuditsCookieWarningReason],
    auditsCookieIssueDetailsCookieExclusionReasons :: [AuditsCookieExclusionReason],
    -- | Optionally identifies the site-for-cookies and the cookie url, which
    --   may be used by the front-end as additional context.
    auditsCookieIssueDetailsOperation :: AuditsCookieOperation,
    auditsCookieIssueDetailsSiteForCookies :: Maybe String,
    auditsCookieIssueDetailsCookieUrl :: Maybe String,
    auditsCookieIssueDetailsRequest :: Maybe AuditsAffectedRequest
  }
  deriving (Eq, Show)
instance FromJSON AuditsCookieIssueDetails where
  parseJSON = A.withObject "AuditsCookieIssueDetails" $ \o -> AuditsCookieIssueDetails
    <$> o A..:? "cookie"
    <*> o A..:? "rawCookieLine"
    <*> o A..: "cookieWarningReasons"
    <*> o A..: "cookieExclusionReasons"
    <*> o A..: "operation"
    <*> o A..:? "siteForCookies"
    <*> o A..:? "cookieUrl"
    <*> o A..:? "request"
instance ToJSON AuditsCookieIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("cookie" A..=) <$> (auditsCookieIssueDetailsCookie p),
    ("rawCookieLine" A..=) <$> (auditsCookieIssueDetailsRawCookieLine p),
    ("cookieWarningReasons" A..=) <$> Just (auditsCookieIssueDetailsCookieWarningReasons p),
    ("cookieExclusionReasons" A..=) <$> Just (auditsCookieIssueDetailsCookieExclusionReasons p),
    ("operation" A..=) <$> Just (auditsCookieIssueDetailsOperation p),
    ("siteForCookies" A..=) <$> (auditsCookieIssueDetailsSiteForCookies p),
    ("cookieUrl" A..=) <$> (auditsCookieIssueDetailsCookieUrl p),
    ("request" A..=) <$> (auditsCookieIssueDetailsRequest p)
    ]

-- | Type 'Audits.MixedContentResolutionStatus'.
data AuditsMixedContentResolutionStatus = AuditsMixedContentResolutionStatusMixedContentBlocked | AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded | AuditsMixedContentResolutionStatusMixedContentWarning
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResolutionStatus where
  parseJSON = A.withText "AuditsMixedContentResolutionStatus" $ \v -> case v of
    "MixedContentBlocked" -> pure AuditsMixedContentResolutionStatusMixedContentBlocked
    "MixedContentAutomaticallyUpgraded" -> pure AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded
    "MixedContentWarning" -> pure AuditsMixedContentResolutionStatusMixedContentWarning
    "_" -> fail "failed to parse AuditsMixedContentResolutionStatus"
instance ToJSON AuditsMixedContentResolutionStatus where
  toJSON v = A.String $ case v of
    AuditsMixedContentResolutionStatusMixedContentBlocked -> "MixedContentBlocked"
    AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded -> "MixedContentAutomaticallyUpgraded"
    AuditsMixedContentResolutionStatusMixedContentWarning -> "MixedContentWarning"

-- | Type 'Audits.MixedContentResourceType'.
data AuditsMixedContentResourceType = AuditsMixedContentResourceTypeAttributionSrc | AuditsMixedContentResourceTypeAudio | AuditsMixedContentResourceTypeBeacon | AuditsMixedContentResourceTypeCSPReport | AuditsMixedContentResourceTypeDownload | AuditsMixedContentResourceTypeEventSource | AuditsMixedContentResourceTypeFavicon | AuditsMixedContentResourceTypeFont | AuditsMixedContentResourceTypeForm | AuditsMixedContentResourceTypeFrame | AuditsMixedContentResourceTypeImage | AuditsMixedContentResourceTypeImport | AuditsMixedContentResourceTypeManifest | AuditsMixedContentResourceTypePing | AuditsMixedContentResourceTypePluginData | AuditsMixedContentResourceTypePluginResource | AuditsMixedContentResourceTypePrefetch | AuditsMixedContentResourceTypeResource | AuditsMixedContentResourceTypeScript | AuditsMixedContentResourceTypeServiceWorker | AuditsMixedContentResourceTypeSharedWorker | AuditsMixedContentResourceTypeStylesheet | AuditsMixedContentResourceTypeTrack | AuditsMixedContentResourceTypeVideo | AuditsMixedContentResourceTypeWorker | AuditsMixedContentResourceTypeXMLHttpRequest | AuditsMixedContentResourceTypeXSLT
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResourceType where
  parseJSON = A.withText "AuditsMixedContentResourceType" $ \v -> case v of
    "AttributionSrc" -> pure AuditsMixedContentResourceTypeAttributionSrc
    "Audio" -> pure AuditsMixedContentResourceTypeAudio
    "Beacon" -> pure AuditsMixedContentResourceTypeBeacon
    "CSPReport" -> pure AuditsMixedContentResourceTypeCSPReport
    "Download" -> pure AuditsMixedContentResourceTypeDownload
    "EventSource" -> pure AuditsMixedContentResourceTypeEventSource
    "Favicon" -> pure AuditsMixedContentResourceTypeFavicon
    "Font" -> pure AuditsMixedContentResourceTypeFont
    "Form" -> pure AuditsMixedContentResourceTypeForm
    "Frame" -> pure AuditsMixedContentResourceTypeFrame
    "Image" -> pure AuditsMixedContentResourceTypeImage
    "Import" -> pure AuditsMixedContentResourceTypeImport
    "Manifest" -> pure AuditsMixedContentResourceTypeManifest
    "Ping" -> pure AuditsMixedContentResourceTypePing
    "PluginData" -> pure AuditsMixedContentResourceTypePluginData
    "PluginResource" -> pure AuditsMixedContentResourceTypePluginResource
    "Prefetch" -> pure AuditsMixedContentResourceTypePrefetch
    "Resource" -> pure AuditsMixedContentResourceTypeResource
    "Script" -> pure AuditsMixedContentResourceTypeScript
    "ServiceWorker" -> pure AuditsMixedContentResourceTypeServiceWorker
    "SharedWorker" -> pure AuditsMixedContentResourceTypeSharedWorker
    "Stylesheet" -> pure AuditsMixedContentResourceTypeStylesheet
    "Track" -> pure AuditsMixedContentResourceTypeTrack
    "Video" -> pure AuditsMixedContentResourceTypeVideo
    "Worker" -> pure AuditsMixedContentResourceTypeWorker
    "XMLHttpRequest" -> pure AuditsMixedContentResourceTypeXMLHttpRequest
    "XSLT" -> pure AuditsMixedContentResourceTypeXSLT
    "_" -> fail "failed to parse AuditsMixedContentResourceType"
instance ToJSON AuditsMixedContentResourceType where
  toJSON v = A.String $ case v of
    AuditsMixedContentResourceTypeAttributionSrc -> "AttributionSrc"
    AuditsMixedContentResourceTypeAudio -> "Audio"
    AuditsMixedContentResourceTypeBeacon -> "Beacon"
    AuditsMixedContentResourceTypeCSPReport -> "CSPReport"
    AuditsMixedContentResourceTypeDownload -> "Download"
    AuditsMixedContentResourceTypeEventSource -> "EventSource"
    AuditsMixedContentResourceTypeFavicon -> "Favicon"
    AuditsMixedContentResourceTypeFont -> "Font"
    AuditsMixedContentResourceTypeForm -> "Form"
    AuditsMixedContentResourceTypeFrame -> "Frame"
    AuditsMixedContentResourceTypeImage -> "Image"
    AuditsMixedContentResourceTypeImport -> "Import"
    AuditsMixedContentResourceTypeManifest -> "Manifest"
    AuditsMixedContentResourceTypePing -> "Ping"
    AuditsMixedContentResourceTypePluginData -> "PluginData"
    AuditsMixedContentResourceTypePluginResource -> "PluginResource"
    AuditsMixedContentResourceTypePrefetch -> "Prefetch"
    AuditsMixedContentResourceTypeResource -> "Resource"
    AuditsMixedContentResourceTypeScript -> "Script"
    AuditsMixedContentResourceTypeServiceWorker -> "ServiceWorker"
    AuditsMixedContentResourceTypeSharedWorker -> "SharedWorker"
    AuditsMixedContentResourceTypeStylesheet -> "Stylesheet"
    AuditsMixedContentResourceTypeTrack -> "Track"
    AuditsMixedContentResourceTypeVideo -> "Video"
    AuditsMixedContentResourceTypeWorker -> "Worker"
    AuditsMixedContentResourceTypeXMLHttpRequest -> "XMLHttpRequest"
    AuditsMixedContentResourceTypeXSLT -> "XSLT"

-- | Type 'Audits.MixedContentIssueDetails'.
data AuditsMixedContentIssueDetails = AuditsMixedContentIssueDetails
  {
    -- | The type of resource causing the mixed content issue (css, js, iframe,
    --   form,...). Marked as optional because it is mapped to from
    --   blink::mojom::RequestContextType, which will be replaced
    --   by network::mojom::RequestDestination
    auditsMixedContentIssueDetailsResourceType :: Maybe AuditsMixedContentResourceType,
    -- | The way the mixed content issue is being resolved.
    auditsMixedContentIssueDetailsResolutionStatus :: AuditsMixedContentResolutionStatus,
    -- | The unsafe http url causing the mixed content issue.
    auditsMixedContentIssueDetailsInsecureURL :: String,
    -- | The url responsible for the call to an unsafe url.
    auditsMixedContentIssueDetailsMainResourceURL :: String,
    -- | The mixed content request.
    --   Does not always exist (e.g. for unsafe form submission urls).
    auditsMixedContentIssueDetailsRequest :: Maybe AuditsAffectedRequest,
    -- | Optional because not every mixed content issue is necessarily linked to a frame.
    auditsMixedContentIssueDetailsFrame :: Maybe AuditsAffectedFrame
  }
  deriving (Eq, Show)
instance FromJSON AuditsMixedContentIssueDetails where
  parseJSON = A.withObject "AuditsMixedContentIssueDetails" $ \o -> AuditsMixedContentIssueDetails
    <$> o A..:? "resourceType"
    <*> o A..: "resolutionStatus"
    <*> o A..: "insecureURL"
    <*> o A..: "mainResourceURL"
    <*> o A..:? "request"
    <*> o A..:? "frame"
instance ToJSON AuditsMixedContentIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("resourceType" A..=) <$> (auditsMixedContentIssueDetailsResourceType p),
    ("resolutionStatus" A..=) <$> Just (auditsMixedContentIssueDetailsResolutionStatus p),
    ("insecureURL" A..=) <$> Just (auditsMixedContentIssueDetailsInsecureURL p),
    ("mainResourceURL" A..=) <$> Just (auditsMixedContentIssueDetailsMainResourceURL p),
    ("request" A..=) <$> (auditsMixedContentIssueDetailsRequest p),
    ("frame" A..=) <$> (auditsMixedContentIssueDetailsFrame p)
    ]

-- | Type 'Audits.BlockedByResponseReason'.
--   Enum indicating the reason a response has been blocked. These reasons are
--   refinements of the net error BLOCKED_BY_RESPONSE.
data AuditsBlockedByResponseReason = AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader | AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage | AuditsBlockedByResponseReasonCorpNotSameOrigin | AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | AuditsBlockedByResponseReasonCorpNotSameSite
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsBlockedByResponseReason where
  parseJSON = A.withText "AuditsBlockedByResponseReason" $ \v -> case v of
    "CoepFrameResourceNeedsCoepHeader" -> pure AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader
    "CoopSandboxedIFrameCannotNavigateToCoopPage" -> pure AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage
    "CorpNotSameOrigin" -> pure AuditsBlockedByResponseReasonCorpNotSameOrigin
    "CorpNotSameOriginAfterDefaultedToSameOriginByCoep" -> pure AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
    "CorpNotSameSite" -> pure AuditsBlockedByResponseReasonCorpNotSameSite
    "_" -> fail "failed to parse AuditsBlockedByResponseReason"
instance ToJSON AuditsBlockedByResponseReason where
  toJSON v = A.String $ case v of
    AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader -> "CoepFrameResourceNeedsCoepHeader"
    AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage -> "CoopSandboxedIFrameCannotNavigateToCoopPage"
    AuditsBlockedByResponseReasonCorpNotSameOrigin -> "CorpNotSameOrigin"
    AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "CorpNotSameOriginAfterDefaultedToSameOriginByCoep"
    AuditsBlockedByResponseReasonCorpNotSameSite -> "CorpNotSameSite"

-- | Type 'Audits.BlockedByResponseIssueDetails'.
--   Details for a request that has been blocked with the BLOCKED_BY_RESPONSE
--   code. Currently only used for COEP/COOP, but may be extended to include
--   some CSP errors in the future.
data AuditsBlockedByResponseIssueDetails = AuditsBlockedByResponseIssueDetails
  {
    auditsBlockedByResponseIssueDetailsRequest :: AuditsAffectedRequest,
    auditsBlockedByResponseIssueDetailsParentFrame :: Maybe AuditsAffectedFrame,
    auditsBlockedByResponseIssueDetailsBlockedFrame :: Maybe AuditsAffectedFrame,
    auditsBlockedByResponseIssueDetailsReason :: AuditsBlockedByResponseReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsBlockedByResponseIssueDetails where
  parseJSON = A.withObject "AuditsBlockedByResponseIssueDetails" $ \o -> AuditsBlockedByResponseIssueDetails
    <$> o A..: "request"
    <*> o A..:? "parentFrame"
    <*> o A..:? "blockedFrame"
    <*> o A..: "reason"
instance ToJSON AuditsBlockedByResponseIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("request" A..=) <$> Just (auditsBlockedByResponseIssueDetailsRequest p),
    ("parentFrame" A..=) <$> (auditsBlockedByResponseIssueDetailsParentFrame p),
    ("blockedFrame" A..=) <$> (auditsBlockedByResponseIssueDetailsBlockedFrame p),
    ("reason" A..=) <$> Just (auditsBlockedByResponseIssueDetailsReason p)
    ]

-- | Type 'Audits.HeavyAdResolutionStatus'.
data AuditsHeavyAdResolutionStatus = AuditsHeavyAdResolutionStatusHeavyAdBlocked | AuditsHeavyAdResolutionStatusHeavyAdWarning
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdResolutionStatus where
  parseJSON = A.withText "AuditsHeavyAdResolutionStatus" $ \v -> case v of
    "HeavyAdBlocked" -> pure AuditsHeavyAdResolutionStatusHeavyAdBlocked
    "HeavyAdWarning" -> pure AuditsHeavyAdResolutionStatusHeavyAdWarning
    "_" -> fail "failed to parse AuditsHeavyAdResolutionStatus"
instance ToJSON AuditsHeavyAdResolutionStatus where
  toJSON v = A.String $ case v of
    AuditsHeavyAdResolutionStatusHeavyAdBlocked -> "HeavyAdBlocked"
    AuditsHeavyAdResolutionStatusHeavyAdWarning -> "HeavyAdWarning"

-- | Type 'Audits.HeavyAdReason'.
data AuditsHeavyAdReason = AuditsHeavyAdReasonNetworkTotalLimit | AuditsHeavyAdReasonCpuTotalLimit | AuditsHeavyAdReasonCpuPeakLimit
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdReason where
  parseJSON = A.withText "AuditsHeavyAdReason" $ \v -> case v of
    "NetworkTotalLimit" -> pure AuditsHeavyAdReasonNetworkTotalLimit
    "CpuTotalLimit" -> pure AuditsHeavyAdReasonCpuTotalLimit
    "CpuPeakLimit" -> pure AuditsHeavyAdReasonCpuPeakLimit
    "_" -> fail "failed to parse AuditsHeavyAdReason"
instance ToJSON AuditsHeavyAdReason where
  toJSON v = A.String $ case v of
    AuditsHeavyAdReasonNetworkTotalLimit -> "NetworkTotalLimit"
    AuditsHeavyAdReasonCpuTotalLimit -> "CpuTotalLimit"
    AuditsHeavyAdReasonCpuPeakLimit -> "CpuPeakLimit"

-- | Type 'Audits.HeavyAdIssueDetails'.
data AuditsHeavyAdIssueDetails = AuditsHeavyAdIssueDetails
  {
    -- | The resolution status, either blocking the content or warning.
    auditsHeavyAdIssueDetailsResolution :: AuditsHeavyAdResolutionStatus,
    -- | The reason the ad was blocked, total network or cpu or peak cpu.
    auditsHeavyAdIssueDetailsReason :: AuditsHeavyAdReason,
    -- | The frame that was blocked.
    auditsHeavyAdIssueDetailsFrame :: AuditsAffectedFrame
  }
  deriving (Eq, Show)
instance FromJSON AuditsHeavyAdIssueDetails where
  parseJSON = A.withObject "AuditsHeavyAdIssueDetails" $ \o -> AuditsHeavyAdIssueDetails
    <$> o A..: "resolution"
    <*> o A..: "reason"
    <*> o A..: "frame"
instance ToJSON AuditsHeavyAdIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("resolution" A..=) <$> Just (auditsHeavyAdIssueDetailsResolution p),
    ("reason" A..=) <$> Just (auditsHeavyAdIssueDetailsReason p),
    ("frame" A..=) <$> Just (auditsHeavyAdIssueDetailsFrame p)
    ]

-- | Type 'Audits.ContentSecurityPolicyViolationType'.
data AuditsContentSecurityPolicyViolationType = AuditsContentSecurityPolicyViolationTypeKInlineViolation | AuditsContentSecurityPolicyViolationTypeKEvalViolation | AuditsContentSecurityPolicyViolationTypeKURLViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation | AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsContentSecurityPolicyViolationType where
  parseJSON = A.withText "AuditsContentSecurityPolicyViolationType" $ \v -> case v of
    "kInlineViolation" -> pure AuditsContentSecurityPolicyViolationTypeKInlineViolation
    "kEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKEvalViolation
    "kURLViolation" -> pure AuditsContentSecurityPolicyViolationTypeKURLViolation
    "kTrustedTypesSinkViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation
    "kTrustedTypesPolicyViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation
    "kWasmEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
    "_" -> fail "failed to parse AuditsContentSecurityPolicyViolationType"
instance ToJSON AuditsContentSecurityPolicyViolationType where
  toJSON v = A.String $ case v of
    AuditsContentSecurityPolicyViolationTypeKInlineViolation -> "kInlineViolation"
    AuditsContentSecurityPolicyViolationTypeKEvalViolation -> "kEvalViolation"
    AuditsContentSecurityPolicyViolationTypeKURLViolation -> "kURLViolation"
    AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation -> "kTrustedTypesSinkViolation"
    AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation -> "kTrustedTypesPolicyViolation"
    AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation -> "kWasmEvalViolation"

-- | Type 'Audits.SourceCodeLocation'.
data AuditsSourceCodeLocation = AuditsSourceCodeLocation
  {
    auditsSourceCodeLocationScriptId :: Maybe Runtime.RuntimeScriptId,
    auditsSourceCodeLocationUrl :: String,
    auditsSourceCodeLocationLineNumber :: Int,
    auditsSourceCodeLocationColumnNumber :: Int
  }
  deriving (Eq, Show)
instance FromJSON AuditsSourceCodeLocation where
  parseJSON = A.withObject "AuditsSourceCodeLocation" $ \o -> AuditsSourceCodeLocation
    <$> o A..:? "scriptId"
    <*> o A..: "url"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
instance ToJSON AuditsSourceCodeLocation where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> (auditsSourceCodeLocationScriptId p),
    ("url" A..=) <$> Just (auditsSourceCodeLocationUrl p),
    ("lineNumber" A..=) <$> Just (auditsSourceCodeLocationLineNumber p),
    ("columnNumber" A..=) <$> Just (auditsSourceCodeLocationColumnNumber p)
    ]

-- | Type 'Audits.ContentSecurityPolicyIssueDetails'.
data AuditsContentSecurityPolicyIssueDetails = AuditsContentSecurityPolicyIssueDetails
  {
    -- | The url not included in allowed sources.
    auditsContentSecurityPolicyIssueDetailsBlockedURL :: Maybe String,
    -- | Specific directive that is violated, causing the CSP issue.
    auditsContentSecurityPolicyIssueDetailsViolatedDirective :: String,
    auditsContentSecurityPolicyIssueDetailsIsReportOnly :: Bool,
    auditsContentSecurityPolicyIssueDetailsContentSecurityPolicyViolationType :: AuditsContentSecurityPolicyViolationType,
    auditsContentSecurityPolicyIssueDetailsFrameAncestor :: Maybe AuditsAffectedFrame,
    auditsContentSecurityPolicyIssueDetailsSourceCodeLocation :: Maybe AuditsSourceCodeLocation,
    auditsContentSecurityPolicyIssueDetailsViolatingNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON AuditsContentSecurityPolicyIssueDetails where
  parseJSON = A.withObject "AuditsContentSecurityPolicyIssueDetails" $ \o -> AuditsContentSecurityPolicyIssueDetails
    <$> o A..:? "blockedURL"
    <*> o A..: "violatedDirective"
    <*> o A..: "isReportOnly"
    <*> o A..: "contentSecurityPolicyViolationType"
    <*> o A..:? "frameAncestor"
    <*> o A..:? "sourceCodeLocation"
    <*> o A..:? "violatingNodeId"
instance ToJSON AuditsContentSecurityPolicyIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("blockedURL" A..=) <$> (auditsContentSecurityPolicyIssueDetailsBlockedURL p),
    ("violatedDirective" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsViolatedDirective p),
    ("isReportOnly" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsIsReportOnly p),
    ("contentSecurityPolicyViolationType" A..=) <$> Just (auditsContentSecurityPolicyIssueDetailsContentSecurityPolicyViolationType p),
    ("frameAncestor" A..=) <$> (auditsContentSecurityPolicyIssueDetailsFrameAncestor p),
    ("sourceCodeLocation" A..=) <$> (auditsContentSecurityPolicyIssueDetailsSourceCodeLocation p),
    ("violatingNodeId" A..=) <$> (auditsContentSecurityPolicyIssueDetailsViolatingNodeId p)
    ]

-- | Type 'Audits.SharedArrayBufferIssueType'.
data AuditsSharedArrayBufferIssueType = AuditsSharedArrayBufferIssueTypeTransferIssue | AuditsSharedArrayBufferIssueTypeCreationIssue
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsSharedArrayBufferIssueType where
  parseJSON = A.withText "AuditsSharedArrayBufferIssueType" $ \v -> case v of
    "TransferIssue" -> pure AuditsSharedArrayBufferIssueTypeTransferIssue
    "CreationIssue" -> pure AuditsSharedArrayBufferIssueTypeCreationIssue
    "_" -> fail "failed to parse AuditsSharedArrayBufferIssueType"
instance ToJSON AuditsSharedArrayBufferIssueType where
  toJSON v = A.String $ case v of
    AuditsSharedArrayBufferIssueTypeTransferIssue -> "TransferIssue"
    AuditsSharedArrayBufferIssueTypeCreationIssue -> "CreationIssue"

-- | Type 'Audits.SharedArrayBufferIssueDetails'.
--   Details for a issue arising from an SAB being instantiated in, or
--   transferred to a context that is not cross-origin isolated.
data AuditsSharedArrayBufferIssueDetails = AuditsSharedArrayBufferIssueDetails
  {
    auditsSharedArrayBufferIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    auditsSharedArrayBufferIssueDetailsIsWarning :: Bool,
    auditsSharedArrayBufferIssueDetailsType :: AuditsSharedArrayBufferIssueType
  }
  deriving (Eq, Show)
instance FromJSON AuditsSharedArrayBufferIssueDetails where
  parseJSON = A.withObject "AuditsSharedArrayBufferIssueDetails" $ \o -> AuditsSharedArrayBufferIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "isWarning"
    <*> o A..: "type"
instance ToJSON AuditsSharedArrayBufferIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsSourceCodeLocation p),
    ("isWarning" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsIsWarning p),
    ("type" A..=) <$> Just (auditsSharedArrayBufferIssueDetailsType p)
    ]

-- | Type 'Audits.TwaQualityEnforcementViolationType'.
data AuditsTwaQualityEnforcementViolationType = AuditsTwaQualityEnforcementViolationTypeKHttpError | AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline | AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsTwaQualityEnforcementViolationType where
  parseJSON = A.withText "AuditsTwaQualityEnforcementViolationType" $ \v -> case v of
    "kHttpError" -> pure AuditsTwaQualityEnforcementViolationTypeKHttpError
    "kUnavailableOffline" -> pure AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline
    "kDigitalAssetLinks" -> pure AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks
    "_" -> fail "failed to parse AuditsTwaQualityEnforcementViolationType"
instance ToJSON AuditsTwaQualityEnforcementViolationType where
  toJSON v = A.String $ case v of
    AuditsTwaQualityEnforcementViolationTypeKHttpError -> "kHttpError"
    AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline -> "kUnavailableOffline"
    AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks -> "kDigitalAssetLinks"

-- | Type 'Audits.TrustedWebActivityIssueDetails'.
data AuditsTrustedWebActivityIssueDetails = AuditsTrustedWebActivityIssueDetails
  {
    -- | The url that triggers the violation.
    auditsTrustedWebActivityIssueDetailsUrl :: String,
    auditsTrustedWebActivityIssueDetailsViolationType :: AuditsTwaQualityEnforcementViolationType,
    auditsTrustedWebActivityIssueDetailsHttpStatusCode :: Maybe Int,
    -- | The package name of the Trusted Web Activity client app. This field is
    --   only used when violation type is kDigitalAssetLinks.
    auditsTrustedWebActivityIssueDetailsPackageName :: Maybe String,
    -- | The signature of the Trusted Web Activity client app. This field is only
    --   used when violation type is kDigitalAssetLinks.
    auditsTrustedWebActivityIssueDetailsSignature :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON AuditsTrustedWebActivityIssueDetails where
  parseJSON = A.withObject "AuditsTrustedWebActivityIssueDetails" $ \o -> AuditsTrustedWebActivityIssueDetails
    <$> o A..: "url"
    <*> o A..: "violationType"
    <*> o A..:? "httpStatusCode"
    <*> o A..:? "packageName"
    <*> o A..:? "signature"
instance ToJSON AuditsTrustedWebActivityIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (auditsTrustedWebActivityIssueDetailsUrl p),
    ("violationType" A..=) <$> Just (auditsTrustedWebActivityIssueDetailsViolationType p),
    ("httpStatusCode" A..=) <$> (auditsTrustedWebActivityIssueDetailsHttpStatusCode p),
    ("packageName" A..=) <$> (auditsTrustedWebActivityIssueDetailsPackageName p),
    ("signature" A..=) <$> (auditsTrustedWebActivityIssueDetailsSignature p)
    ]

-- | Type 'Audits.LowTextContrastIssueDetails'.
data AuditsLowTextContrastIssueDetails = AuditsLowTextContrastIssueDetails
  {
    auditsLowTextContrastIssueDetailsViolatingNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    auditsLowTextContrastIssueDetailsViolatingNodeSelector :: String,
    auditsLowTextContrastIssueDetailsContrastRatio :: Double,
    auditsLowTextContrastIssueDetailsThresholdAA :: Double,
    auditsLowTextContrastIssueDetailsThresholdAAA :: Double,
    auditsLowTextContrastIssueDetailsFontSize :: String,
    auditsLowTextContrastIssueDetailsFontWeight :: String
  }
  deriving (Eq, Show)
instance FromJSON AuditsLowTextContrastIssueDetails where
  parseJSON = A.withObject "AuditsLowTextContrastIssueDetails" $ \o -> AuditsLowTextContrastIssueDetails
    <$> o A..: "violatingNodeId"
    <*> o A..: "violatingNodeSelector"
    <*> o A..: "contrastRatio"
    <*> o A..: "thresholdAA"
    <*> o A..: "thresholdAAA"
    <*> o A..: "fontSize"
    <*> o A..: "fontWeight"
instance ToJSON AuditsLowTextContrastIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("violatingNodeId" A..=) <$> Just (auditsLowTextContrastIssueDetailsViolatingNodeId p),
    ("violatingNodeSelector" A..=) <$> Just (auditsLowTextContrastIssueDetailsViolatingNodeSelector p),
    ("contrastRatio" A..=) <$> Just (auditsLowTextContrastIssueDetailsContrastRatio p),
    ("thresholdAA" A..=) <$> Just (auditsLowTextContrastIssueDetailsThresholdAA p),
    ("thresholdAAA" A..=) <$> Just (auditsLowTextContrastIssueDetailsThresholdAAA p),
    ("fontSize" A..=) <$> Just (auditsLowTextContrastIssueDetailsFontSize p),
    ("fontWeight" A..=) <$> Just (auditsLowTextContrastIssueDetailsFontWeight p)
    ]

-- | Type 'Audits.CorsIssueDetails'.
--   Details for a CORS related issue, e.g. a warning or error related to
--   CORS RFC1918 enforcement.
data AuditsCorsIssueDetails = AuditsCorsIssueDetails
  {
    auditsCorsIssueDetailsCorsErrorStatus :: DOMPageNetworkEmulationSecurity.NetworkCorsErrorStatus,
    auditsCorsIssueDetailsIsWarning :: Bool,
    auditsCorsIssueDetailsRequest :: AuditsAffectedRequest,
    auditsCorsIssueDetailsLocation :: Maybe AuditsSourceCodeLocation,
    auditsCorsIssueDetailsInitiatorOrigin :: Maybe String,
    auditsCorsIssueDetailsResourceIPAddressSpace :: Maybe DOMPageNetworkEmulationSecurity.NetworkIPAddressSpace,
    auditsCorsIssueDetailsClientSecurityState :: Maybe DOMPageNetworkEmulationSecurity.NetworkClientSecurityState
  }
  deriving (Eq, Show)
instance FromJSON AuditsCorsIssueDetails where
  parseJSON = A.withObject "AuditsCorsIssueDetails" $ \o -> AuditsCorsIssueDetails
    <$> o A..: "corsErrorStatus"
    <*> o A..: "isWarning"
    <*> o A..: "request"
    <*> o A..:? "location"
    <*> o A..:? "initiatorOrigin"
    <*> o A..:? "resourceIPAddressSpace"
    <*> o A..:? "clientSecurityState"
instance ToJSON AuditsCorsIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("corsErrorStatus" A..=) <$> Just (auditsCorsIssueDetailsCorsErrorStatus p),
    ("isWarning" A..=) <$> Just (auditsCorsIssueDetailsIsWarning p),
    ("request" A..=) <$> Just (auditsCorsIssueDetailsRequest p),
    ("location" A..=) <$> (auditsCorsIssueDetailsLocation p),
    ("initiatorOrigin" A..=) <$> (auditsCorsIssueDetailsInitiatorOrigin p),
    ("resourceIPAddressSpace" A..=) <$> (auditsCorsIssueDetailsResourceIPAddressSpace p),
    ("clientSecurityState" A..=) <$> (auditsCorsIssueDetailsClientSecurityState p)
    ]

-- | Type 'Audits.AttributionReportingIssueType'.
data AuditsAttributionReportingIssueType = AuditsAttributionReportingIssueTypePermissionPolicyDisabled | AuditsAttributionReportingIssueTypePermissionPolicyNotDelegated | AuditsAttributionReportingIssueTypeUntrustworthyReportingOrigin | AuditsAttributionReportingIssueTypeInsecureContext | AuditsAttributionReportingIssueTypeInvalidHeader | AuditsAttributionReportingIssueTypeInvalidRegisterTriggerHeader | AuditsAttributionReportingIssueTypeInvalidEligibleHeader | AuditsAttributionReportingIssueTypeTooManyConcurrentRequests | AuditsAttributionReportingIssueTypeSourceAndTriggerHeaders | AuditsAttributionReportingIssueTypeSourceIgnored | AuditsAttributionReportingIssueTypeTriggerIgnored
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsAttributionReportingIssueType where
  parseJSON = A.withText "AuditsAttributionReportingIssueType" $ \v -> case v of
    "PermissionPolicyDisabled" -> pure AuditsAttributionReportingIssueTypePermissionPolicyDisabled
    "PermissionPolicyNotDelegated" -> pure AuditsAttributionReportingIssueTypePermissionPolicyNotDelegated
    "UntrustworthyReportingOrigin" -> pure AuditsAttributionReportingIssueTypeUntrustworthyReportingOrigin
    "InsecureContext" -> pure AuditsAttributionReportingIssueTypeInsecureContext
    "InvalidHeader" -> pure AuditsAttributionReportingIssueTypeInvalidHeader
    "InvalidRegisterTriggerHeader" -> pure AuditsAttributionReportingIssueTypeInvalidRegisterTriggerHeader
    "InvalidEligibleHeader" -> pure AuditsAttributionReportingIssueTypeInvalidEligibleHeader
    "TooManyConcurrentRequests" -> pure AuditsAttributionReportingIssueTypeTooManyConcurrentRequests
    "SourceAndTriggerHeaders" -> pure AuditsAttributionReportingIssueTypeSourceAndTriggerHeaders
    "SourceIgnored" -> pure AuditsAttributionReportingIssueTypeSourceIgnored
    "TriggerIgnored" -> pure AuditsAttributionReportingIssueTypeTriggerIgnored
    "_" -> fail "failed to parse AuditsAttributionReportingIssueType"
instance ToJSON AuditsAttributionReportingIssueType where
  toJSON v = A.String $ case v of
    AuditsAttributionReportingIssueTypePermissionPolicyDisabled -> "PermissionPolicyDisabled"
    AuditsAttributionReportingIssueTypePermissionPolicyNotDelegated -> "PermissionPolicyNotDelegated"
    AuditsAttributionReportingIssueTypeUntrustworthyReportingOrigin -> "UntrustworthyReportingOrigin"
    AuditsAttributionReportingIssueTypeInsecureContext -> "InsecureContext"
    AuditsAttributionReportingIssueTypeInvalidHeader -> "InvalidHeader"
    AuditsAttributionReportingIssueTypeInvalidRegisterTriggerHeader -> "InvalidRegisterTriggerHeader"
    AuditsAttributionReportingIssueTypeInvalidEligibleHeader -> "InvalidEligibleHeader"
    AuditsAttributionReportingIssueTypeTooManyConcurrentRequests -> "TooManyConcurrentRequests"
    AuditsAttributionReportingIssueTypeSourceAndTriggerHeaders -> "SourceAndTriggerHeaders"
    AuditsAttributionReportingIssueTypeSourceIgnored -> "SourceIgnored"
    AuditsAttributionReportingIssueTypeTriggerIgnored -> "TriggerIgnored"

-- | Type 'Audits.AttributionReportingIssueDetails'.
--   Details for issues around "Attribution Reporting API" usage.
--   Explainer: https://github.com/WICG/attribution-reporting-api
data AuditsAttributionReportingIssueDetails = AuditsAttributionReportingIssueDetails
  {
    auditsAttributionReportingIssueDetailsViolationType :: AuditsAttributionReportingIssueType,
    auditsAttributionReportingIssueDetailsRequest :: Maybe AuditsAffectedRequest,
    auditsAttributionReportingIssueDetailsViolatingNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    auditsAttributionReportingIssueDetailsInvalidParameter :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON AuditsAttributionReportingIssueDetails where
  parseJSON = A.withObject "AuditsAttributionReportingIssueDetails" $ \o -> AuditsAttributionReportingIssueDetails
    <$> o A..: "violationType"
    <*> o A..:? "request"
    <*> o A..:? "violatingNodeId"
    <*> o A..:? "invalidParameter"
instance ToJSON AuditsAttributionReportingIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("violationType" A..=) <$> Just (auditsAttributionReportingIssueDetailsViolationType p),
    ("request" A..=) <$> (auditsAttributionReportingIssueDetailsRequest p),
    ("violatingNodeId" A..=) <$> (auditsAttributionReportingIssueDetailsViolatingNodeId p),
    ("invalidParameter" A..=) <$> (auditsAttributionReportingIssueDetailsInvalidParameter p)
    ]

-- | Type 'Audits.QuirksModeIssueDetails'.
--   Details for issues about documents in Quirks Mode
--   or Limited Quirks Mode that affects page layouting.
data AuditsQuirksModeIssueDetails = AuditsQuirksModeIssueDetails
  {
    -- | If false, it means the document's mode is "quirks"
    --   instead of "limited-quirks".
    auditsQuirksModeIssueDetailsIsLimitedQuirksMode :: Bool,
    auditsQuirksModeIssueDetailsDocumentNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    auditsQuirksModeIssueDetailsUrl :: String,
    auditsQuirksModeIssueDetailsFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    auditsQuirksModeIssueDetailsLoaderId :: DOMPageNetworkEmulationSecurity.NetworkLoaderId
  }
  deriving (Eq, Show)
instance FromJSON AuditsQuirksModeIssueDetails where
  parseJSON = A.withObject "AuditsQuirksModeIssueDetails" $ \o -> AuditsQuirksModeIssueDetails
    <$> o A..: "isLimitedQuirksMode"
    <*> o A..: "documentNodeId"
    <*> o A..: "url"
    <*> o A..: "frameId"
    <*> o A..: "loaderId"
instance ToJSON AuditsQuirksModeIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("isLimitedQuirksMode" A..=) <$> Just (auditsQuirksModeIssueDetailsIsLimitedQuirksMode p),
    ("documentNodeId" A..=) <$> Just (auditsQuirksModeIssueDetailsDocumentNodeId p),
    ("url" A..=) <$> Just (auditsQuirksModeIssueDetailsUrl p),
    ("frameId" A..=) <$> Just (auditsQuirksModeIssueDetailsFrameId p),
    ("loaderId" A..=) <$> Just (auditsQuirksModeIssueDetailsLoaderId p)
    ]

-- | Type 'Audits.NavigatorUserAgentIssueDetails'.
data AuditsNavigatorUserAgentIssueDetails = AuditsNavigatorUserAgentIssueDetails
  {
    auditsNavigatorUserAgentIssueDetailsUrl :: String,
    auditsNavigatorUserAgentIssueDetailsLocation :: Maybe AuditsSourceCodeLocation
  }
  deriving (Eq, Show)
instance FromJSON AuditsNavigatorUserAgentIssueDetails where
  parseJSON = A.withObject "AuditsNavigatorUserAgentIssueDetails" $ \o -> AuditsNavigatorUserAgentIssueDetails
    <$> o A..: "url"
    <*> o A..:? "location"
instance ToJSON AuditsNavigatorUserAgentIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (auditsNavigatorUserAgentIssueDetailsUrl p),
    ("location" A..=) <$> (auditsNavigatorUserAgentIssueDetailsLocation p)
    ]

-- | Type 'Audits.GenericIssueErrorType'.
data AuditsGenericIssueErrorType = AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsGenericIssueErrorType where
  parseJSON = A.withText "AuditsGenericIssueErrorType" $ \v -> case v of
    "CrossOriginPortalPostMessageError" -> pure AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError
    "_" -> fail "failed to parse AuditsGenericIssueErrorType"
instance ToJSON AuditsGenericIssueErrorType where
  toJSON v = A.String $ case v of
    AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError -> "CrossOriginPortalPostMessageError"

-- | Type 'Audits.GenericIssueDetails'.
--   Depending on the concrete errorType, different properties are set.
data AuditsGenericIssueDetails = AuditsGenericIssueDetails
  {
    -- | Issues with the same errorType are aggregated in the frontend.
    auditsGenericIssueDetailsErrorType :: AuditsGenericIssueErrorType,
    auditsGenericIssueDetailsFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON AuditsGenericIssueDetails where
  parseJSON = A.withObject "AuditsGenericIssueDetails" $ \o -> AuditsGenericIssueDetails
    <$> o A..: "errorType"
    <*> o A..:? "frameId"
instance ToJSON AuditsGenericIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("errorType" A..=) <$> Just (auditsGenericIssueDetailsErrorType p),
    ("frameId" A..=) <$> (auditsGenericIssueDetailsFrameId p)
    ]

-- | Type 'Audits.DeprecationIssueType'.
data AuditsDeprecationIssueType = AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard | AuditsDeprecationIssueTypeCanRequestURLHTTPContainingNewline | AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo | AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime | AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable | AuditsDeprecationIssueTypeCookieWithTruncatingChar | AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain | AuditsDeprecationIssueTypeCrossOriginWindowAlert | AuditsDeprecationIssueTypeCrossOriginWindowConfirm | AuditsDeprecationIssueTypeCSSSelectorInternalMediaControlsOverlayCastButton | AuditsDeprecationIssueTypeDeprecationExample | AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader | AuditsDeprecationIssueTypeEventPath | AuditsDeprecationIssueTypeExpectCTHeader | AuditsDeprecationIssueTypeGeolocationInsecureOrigin | AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved | AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin | AuditsDeprecationIssueTypeHostCandidateAttributeGetter | AuditsDeprecationIssueTypeIdentityInCanMakePaymentEvent | AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest | AuditsDeprecationIssueTypeLocalCSSFileExtensionRejected | AuditsDeprecationIssueTypeMediaSourceAbortRemove | AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered | AuditsDeprecationIssueTypeNoSysexWebMIDIWithoutPermission | AuditsDeprecationIssueTypeNotificationInsecureOrigin | AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe | AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite | AuditsDeprecationIssueTypeOpenWebDatabaseInsecureContext | AuditsDeprecationIssueTypeOverflowVisibleOnReplacedElement | AuditsDeprecationIssueTypePaymentInstruments | AuditsDeprecationIssueTypePaymentRequestCSPViolation | AuditsDeprecationIssueTypePersistentQuotaType | AuditsDeprecationIssueTypePictureSourceSrc | AuditsDeprecationIssueTypePrefixedCancelAnimationFrame | AuditsDeprecationIssueTypePrefixedRequestAnimationFrame | AuditsDeprecationIssueTypePrefixedStorageInfo | AuditsDeprecationIssueTypePrefixedVideoDisplayingFullscreen | AuditsDeprecationIssueTypePrefixedVideoEnterFullscreen | AuditsDeprecationIssueTypePrefixedVideoEnterFullScreen | AuditsDeprecationIssueTypePrefixedVideoExitFullscreen | AuditsDeprecationIssueTypePrefixedVideoExitFullScreen | AuditsDeprecationIssueTypePrefixedVideoSupportsFullscreen | AuditsDeprecationIssueTypeRangeExpand | AuditsDeprecationIssueTypeRequestedSubresourceWithEmbeddedCredentials | AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpFalse | AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpTrue | AuditsDeprecationIssueTypeRTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics | AuditsDeprecationIssueTypeRTCPeerConnectionSdpSemanticsPlanB | AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate | AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation | AuditsDeprecationIssueTypeTextToSpeech_DisallowedByAutoplay | AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation | AuditsDeprecationIssueTypeXHRJSONEncodingDetection | AuditsDeprecationIssueTypeXMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload | AuditsDeprecationIssueTypeXRSupportsSession
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsDeprecationIssueType where
  parseJSON = A.withText "AuditsDeprecationIssueType" $ \v -> case v of
    "AuthorizationCoveredByWildcard" -> pure AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard
    "CanRequestURLHTTPContainingNewline" -> pure AuditsDeprecationIssueTypeCanRequestURLHTTPContainingNewline
    "ChromeLoadTimesConnectionInfo" -> pure AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo
    "ChromeLoadTimesFirstPaintAfterLoadTime" -> pure AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime
    "ChromeLoadTimesWasAlternateProtocolAvailable" -> pure AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable
    "CookieWithTruncatingChar" -> pure AuditsDeprecationIssueTypeCookieWithTruncatingChar
    "CrossOriginAccessBasedOnDocumentDomain" -> pure AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain
    "CrossOriginWindowAlert" -> pure AuditsDeprecationIssueTypeCrossOriginWindowAlert
    "CrossOriginWindowConfirm" -> pure AuditsDeprecationIssueTypeCrossOriginWindowConfirm
    "CSSSelectorInternalMediaControlsOverlayCastButton" -> pure AuditsDeprecationIssueTypeCSSSelectorInternalMediaControlsOverlayCastButton
    "DeprecationExample" -> pure AuditsDeprecationIssueTypeDeprecationExample
    "DocumentDomainSettingWithoutOriginAgentClusterHeader" -> pure AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader
    "EventPath" -> pure AuditsDeprecationIssueTypeEventPath
    "ExpectCTHeader" -> pure AuditsDeprecationIssueTypeExpectCTHeader
    "GeolocationInsecureOrigin" -> pure AuditsDeprecationIssueTypeGeolocationInsecureOrigin
    "GeolocationInsecureOriginDeprecatedNotRemoved" -> pure AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved
    "GetUserMediaInsecureOrigin" -> pure AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin
    "HostCandidateAttributeGetter" -> pure AuditsDeprecationIssueTypeHostCandidateAttributeGetter
    "IdentityInCanMakePaymentEvent" -> pure AuditsDeprecationIssueTypeIdentityInCanMakePaymentEvent
    "InsecurePrivateNetworkSubresourceRequest" -> pure AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest
    "LocalCSSFileExtensionRejected" -> pure AuditsDeprecationIssueTypeLocalCSSFileExtensionRejected
    "MediaSourceAbortRemove" -> pure AuditsDeprecationIssueTypeMediaSourceAbortRemove
    "MediaSourceDurationTruncatingBuffered" -> pure AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered
    "NoSysexWebMIDIWithoutPermission" -> pure AuditsDeprecationIssueTypeNoSysexWebMIDIWithoutPermission
    "NotificationInsecureOrigin" -> pure AuditsDeprecationIssueTypeNotificationInsecureOrigin
    "NotificationPermissionRequestedIframe" -> pure AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe
    "ObsoleteWebRtcCipherSuite" -> pure AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite
    "OpenWebDatabaseInsecureContext" -> pure AuditsDeprecationIssueTypeOpenWebDatabaseInsecureContext
    "OverflowVisibleOnReplacedElement" -> pure AuditsDeprecationIssueTypeOverflowVisibleOnReplacedElement
    "PaymentInstruments" -> pure AuditsDeprecationIssueTypePaymentInstruments
    "PaymentRequestCSPViolation" -> pure AuditsDeprecationIssueTypePaymentRequestCSPViolation
    "PersistentQuotaType" -> pure AuditsDeprecationIssueTypePersistentQuotaType
    "PictureSourceSrc" -> pure AuditsDeprecationIssueTypePictureSourceSrc
    "PrefixedCancelAnimationFrame" -> pure AuditsDeprecationIssueTypePrefixedCancelAnimationFrame
    "PrefixedRequestAnimationFrame" -> pure AuditsDeprecationIssueTypePrefixedRequestAnimationFrame
    "PrefixedStorageInfo" -> pure AuditsDeprecationIssueTypePrefixedStorageInfo
    "PrefixedVideoDisplayingFullscreen" -> pure AuditsDeprecationIssueTypePrefixedVideoDisplayingFullscreen
    "PrefixedVideoEnterFullscreen" -> pure AuditsDeprecationIssueTypePrefixedVideoEnterFullscreen
    "PrefixedVideoEnterFullScreen" -> pure AuditsDeprecationIssueTypePrefixedVideoEnterFullScreen
    "PrefixedVideoExitFullscreen" -> pure AuditsDeprecationIssueTypePrefixedVideoExitFullscreen
    "PrefixedVideoExitFullScreen" -> pure AuditsDeprecationIssueTypePrefixedVideoExitFullScreen
    "PrefixedVideoSupportsFullscreen" -> pure AuditsDeprecationIssueTypePrefixedVideoSupportsFullscreen
    "RangeExpand" -> pure AuditsDeprecationIssueTypeRangeExpand
    "RequestedSubresourceWithEmbeddedCredentials" -> pure AuditsDeprecationIssueTypeRequestedSubresourceWithEmbeddedCredentials
    "RTCConstraintEnableDtlsSrtpFalse" -> pure AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpFalse
    "RTCConstraintEnableDtlsSrtpTrue" -> pure AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpTrue
    "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics" -> pure AuditsDeprecationIssueTypeRTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics
    "RTCPeerConnectionSdpSemanticsPlanB" -> pure AuditsDeprecationIssueTypeRTCPeerConnectionSdpSemanticsPlanB
    "RtcpMuxPolicyNegotiate" -> pure AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate
    "SharedArrayBufferConstructedWithoutIsolation" -> pure AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation
    "TextToSpeech_DisallowedByAutoplay" -> pure AuditsDeprecationIssueTypeTextToSpeech_DisallowedByAutoplay
    "V8SharedArrayBufferConstructedInExtensionWithoutIsolation" -> pure AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation
    "XHRJSONEncodingDetection" -> pure AuditsDeprecationIssueTypeXHRJSONEncodingDetection
    "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload" -> pure AuditsDeprecationIssueTypeXMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload
    "XRSupportsSession" -> pure AuditsDeprecationIssueTypeXRSupportsSession
    "_" -> fail "failed to parse AuditsDeprecationIssueType"
instance ToJSON AuditsDeprecationIssueType where
  toJSON v = A.String $ case v of
    AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard -> "AuthorizationCoveredByWildcard"
    AuditsDeprecationIssueTypeCanRequestURLHTTPContainingNewline -> "CanRequestURLHTTPContainingNewline"
    AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo -> "ChromeLoadTimesConnectionInfo"
    AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime -> "ChromeLoadTimesFirstPaintAfterLoadTime"
    AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable -> "ChromeLoadTimesWasAlternateProtocolAvailable"
    AuditsDeprecationIssueTypeCookieWithTruncatingChar -> "CookieWithTruncatingChar"
    AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain -> "CrossOriginAccessBasedOnDocumentDomain"
    AuditsDeprecationIssueTypeCrossOriginWindowAlert -> "CrossOriginWindowAlert"
    AuditsDeprecationIssueTypeCrossOriginWindowConfirm -> "CrossOriginWindowConfirm"
    AuditsDeprecationIssueTypeCSSSelectorInternalMediaControlsOverlayCastButton -> "CSSSelectorInternalMediaControlsOverlayCastButton"
    AuditsDeprecationIssueTypeDeprecationExample -> "DeprecationExample"
    AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader -> "DocumentDomainSettingWithoutOriginAgentClusterHeader"
    AuditsDeprecationIssueTypeEventPath -> "EventPath"
    AuditsDeprecationIssueTypeExpectCTHeader -> "ExpectCTHeader"
    AuditsDeprecationIssueTypeGeolocationInsecureOrigin -> "GeolocationInsecureOrigin"
    AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved -> "GeolocationInsecureOriginDeprecatedNotRemoved"
    AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin -> "GetUserMediaInsecureOrigin"
    AuditsDeprecationIssueTypeHostCandidateAttributeGetter -> "HostCandidateAttributeGetter"
    AuditsDeprecationIssueTypeIdentityInCanMakePaymentEvent -> "IdentityInCanMakePaymentEvent"
    AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest -> "InsecurePrivateNetworkSubresourceRequest"
    AuditsDeprecationIssueTypeLocalCSSFileExtensionRejected -> "LocalCSSFileExtensionRejected"
    AuditsDeprecationIssueTypeMediaSourceAbortRemove -> "MediaSourceAbortRemove"
    AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered -> "MediaSourceDurationTruncatingBuffered"
    AuditsDeprecationIssueTypeNoSysexWebMIDIWithoutPermission -> "NoSysexWebMIDIWithoutPermission"
    AuditsDeprecationIssueTypeNotificationInsecureOrigin -> "NotificationInsecureOrigin"
    AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe -> "NotificationPermissionRequestedIframe"
    AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite -> "ObsoleteWebRtcCipherSuite"
    AuditsDeprecationIssueTypeOpenWebDatabaseInsecureContext -> "OpenWebDatabaseInsecureContext"
    AuditsDeprecationIssueTypeOverflowVisibleOnReplacedElement -> "OverflowVisibleOnReplacedElement"
    AuditsDeprecationIssueTypePaymentInstruments -> "PaymentInstruments"
    AuditsDeprecationIssueTypePaymentRequestCSPViolation -> "PaymentRequestCSPViolation"
    AuditsDeprecationIssueTypePersistentQuotaType -> "PersistentQuotaType"
    AuditsDeprecationIssueTypePictureSourceSrc -> "PictureSourceSrc"
    AuditsDeprecationIssueTypePrefixedCancelAnimationFrame -> "PrefixedCancelAnimationFrame"
    AuditsDeprecationIssueTypePrefixedRequestAnimationFrame -> "PrefixedRequestAnimationFrame"
    AuditsDeprecationIssueTypePrefixedStorageInfo -> "PrefixedStorageInfo"
    AuditsDeprecationIssueTypePrefixedVideoDisplayingFullscreen -> "PrefixedVideoDisplayingFullscreen"
    AuditsDeprecationIssueTypePrefixedVideoEnterFullscreen -> "PrefixedVideoEnterFullscreen"
    AuditsDeprecationIssueTypePrefixedVideoEnterFullScreen -> "PrefixedVideoEnterFullScreen"
    AuditsDeprecationIssueTypePrefixedVideoExitFullscreen -> "PrefixedVideoExitFullscreen"
    AuditsDeprecationIssueTypePrefixedVideoExitFullScreen -> "PrefixedVideoExitFullScreen"
    AuditsDeprecationIssueTypePrefixedVideoSupportsFullscreen -> "PrefixedVideoSupportsFullscreen"
    AuditsDeprecationIssueTypeRangeExpand -> "RangeExpand"
    AuditsDeprecationIssueTypeRequestedSubresourceWithEmbeddedCredentials -> "RequestedSubresourceWithEmbeddedCredentials"
    AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpFalse -> "RTCConstraintEnableDtlsSrtpFalse"
    AuditsDeprecationIssueTypeRTCConstraintEnableDtlsSrtpTrue -> "RTCConstraintEnableDtlsSrtpTrue"
    AuditsDeprecationIssueTypeRTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics -> "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics"
    AuditsDeprecationIssueTypeRTCPeerConnectionSdpSemanticsPlanB -> "RTCPeerConnectionSdpSemanticsPlanB"
    AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate -> "RtcpMuxPolicyNegotiate"
    AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation -> "SharedArrayBufferConstructedWithoutIsolation"
    AuditsDeprecationIssueTypeTextToSpeech_DisallowedByAutoplay -> "TextToSpeech_DisallowedByAutoplay"
    AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation -> "V8SharedArrayBufferConstructedInExtensionWithoutIsolation"
    AuditsDeprecationIssueTypeXHRJSONEncodingDetection -> "XHRJSONEncodingDetection"
    AuditsDeprecationIssueTypeXMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload -> "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload"
    AuditsDeprecationIssueTypeXRSupportsSession -> "XRSupportsSession"

-- | Type 'Audits.DeprecationIssueDetails'.
--   This issue tracks information needed to print a deprecation message.
--   https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/frame/third_party/blink/renderer/core/frame/deprecation/README.md
data AuditsDeprecationIssueDetails = AuditsDeprecationIssueDetails
  {
    auditsDeprecationIssueDetailsAffectedFrame :: Maybe AuditsAffectedFrame,
    auditsDeprecationIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    auditsDeprecationIssueDetailsType :: AuditsDeprecationIssueType
  }
  deriving (Eq, Show)
instance FromJSON AuditsDeprecationIssueDetails where
  parseJSON = A.withObject "AuditsDeprecationIssueDetails" $ \o -> AuditsDeprecationIssueDetails
    <$> o A..:? "affectedFrame"
    <*> o A..: "sourceCodeLocation"
    <*> o A..: "type"
instance ToJSON AuditsDeprecationIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("affectedFrame" A..=) <$> (auditsDeprecationIssueDetailsAffectedFrame p),
    ("sourceCodeLocation" A..=) <$> Just (auditsDeprecationIssueDetailsSourceCodeLocation p),
    ("type" A..=) <$> Just (auditsDeprecationIssueDetailsType p)
    ]

-- | Type 'Audits.ClientHintIssueReason'.
data AuditsClientHintIssueReason = AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin | AuditsClientHintIssueReasonMetaTagModifiedHTML
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsClientHintIssueReason where
  parseJSON = A.withText "AuditsClientHintIssueReason" $ \v -> case v of
    "MetaTagAllowListInvalidOrigin" -> pure AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin
    "MetaTagModifiedHTML" -> pure AuditsClientHintIssueReasonMetaTagModifiedHTML
    "_" -> fail "failed to parse AuditsClientHintIssueReason"
instance ToJSON AuditsClientHintIssueReason where
  toJSON v = A.String $ case v of
    AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin -> "MetaTagAllowListInvalidOrigin"
    AuditsClientHintIssueReasonMetaTagModifiedHTML -> "MetaTagModifiedHTML"

-- | Type 'Audits.FederatedAuthRequestIssueDetails'.
data AuditsFederatedAuthRequestIssueDetails = AuditsFederatedAuthRequestIssueDetails
  {
    auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason :: AuditsFederatedAuthRequestIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsFederatedAuthRequestIssueDetails where
  parseJSON = A.withObject "AuditsFederatedAuthRequestIssueDetails" $ \o -> AuditsFederatedAuthRequestIssueDetails
    <$> o A..: "federatedAuthRequestIssueReason"
instance ToJSON AuditsFederatedAuthRequestIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("federatedAuthRequestIssueReason" A..=) <$> Just (auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason p)
    ]

-- | Type 'Audits.FederatedAuthRequestIssueReason'.
--   Represents the failure reason when a federated authentication reason fails.
--   Should be updated alongside RequestIdTokenStatus in
--   third_party/blink/public/mojom/devtools/inspector_issue.mojom to include
--   all cases except for success.
data AuditsFederatedAuthRequestIssueReason = AuditsFederatedAuthRequestIssueReasonShouldEmbargo | AuditsFederatedAuthRequestIssueReasonTooManyRequests | AuditsFederatedAuthRequestIssueReasonManifestListHttpNotFound | AuditsFederatedAuthRequestIssueReasonManifestListNoResponse | AuditsFederatedAuthRequestIssueReasonManifestListInvalidResponse | AuditsFederatedAuthRequestIssueReasonManifestNotInManifestList | AuditsFederatedAuthRequestIssueReasonManifestListTooBig | AuditsFederatedAuthRequestIssueReasonManifestHttpNotFound | AuditsFederatedAuthRequestIssueReasonManifestNoResponse | AuditsFederatedAuthRequestIssueReasonManifestInvalidResponse | AuditsFederatedAuthRequestIssueReasonClientMetadataHttpNotFound | AuditsFederatedAuthRequestIssueReasonClientMetadataNoResponse | AuditsFederatedAuthRequestIssueReasonClientMetadataInvalidResponse | AuditsFederatedAuthRequestIssueReasonDisabledInSettings | AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin | AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse | AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound | AuditsFederatedAuthRequestIssueReasonAccountsNoResponse | AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse | AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound | AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest | AuditsFederatedAuthRequestIssueReasonErrorIdToken | AuditsFederatedAuthRequestIssueReasonCanceled | AuditsFederatedAuthRequestIssueReasonRpPageNotVisible
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsFederatedAuthRequestIssueReason where
  parseJSON = A.withText "AuditsFederatedAuthRequestIssueReason" $ \v -> case v of
    "ShouldEmbargo" -> pure AuditsFederatedAuthRequestIssueReasonShouldEmbargo
    "TooManyRequests" -> pure AuditsFederatedAuthRequestIssueReasonTooManyRequests
    "ManifestListHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonManifestListHttpNotFound
    "ManifestListNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonManifestListNoResponse
    "ManifestListInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonManifestListInvalidResponse
    "ManifestNotInManifestList" -> pure AuditsFederatedAuthRequestIssueReasonManifestNotInManifestList
    "ManifestListTooBig" -> pure AuditsFederatedAuthRequestIssueReasonManifestListTooBig
    "ManifestHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonManifestHttpNotFound
    "ManifestNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonManifestNoResponse
    "ManifestInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonManifestInvalidResponse
    "ClientMetadataHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonClientMetadataHttpNotFound
    "ClientMetadataNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonClientMetadataNoResponse
    "ClientMetadataInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonClientMetadataInvalidResponse
    "DisabledInSettings" -> pure AuditsFederatedAuthRequestIssueReasonDisabledInSettings
    "ErrorFetchingSignin" -> pure AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin
    "InvalidSigninResponse" -> pure AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse
    "AccountsHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound
    "AccountsNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonAccountsNoResponse
    "AccountsInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse
    "IdTokenHttpNotFound" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound
    "IdTokenNoResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse
    "IdTokenInvalidResponse" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse
    "IdTokenInvalidRequest" -> pure AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest
    "ErrorIdToken" -> pure AuditsFederatedAuthRequestIssueReasonErrorIdToken
    "Canceled" -> pure AuditsFederatedAuthRequestIssueReasonCanceled
    "RpPageNotVisible" -> pure AuditsFederatedAuthRequestIssueReasonRpPageNotVisible
    "_" -> fail "failed to parse AuditsFederatedAuthRequestIssueReason"
instance ToJSON AuditsFederatedAuthRequestIssueReason where
  toJSON v = A.String $ case v of
    AuditsFederatedAuthRequestIssueReasonShouldEmbargo -> "ShouldEmbargo"
    AuditsFederatedAuthRequestIssueReasonTooManyRequests -> "TooManyRequests"
    AuditsFederatedAuthRequestIssueReasonManifestListHttpNotFound -> "ManifestListHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonManifestListNoResponse -> "ManifestListNoResponse"
    AuditsFederatedAuthRequestIssueReasonManifestListInvalidResponse -> "ManifestListInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonManifestNotInManifestList -> "ManifestNotInManifestList"
    AuditsFederatedAuthRequestIssueReasonManifestListTooBig -> "ManifestListTooBig"
    AuditsFederatedAuthRequestIssueReasonManifestHttpNotFound -> "ManifestHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonManifestNoResponse -> "ManifestNoResponse"
    AuditsFederatedAuthRequestIssueReasonManifestInvalidResponse -> "ManifestInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonClientMetadataHttpNotFound -> "ClientMetadataHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonClientMetadataNoResponse -> "ClientMetadataNoResponse"
    AuditsFederatedAuthRequestIssueReasonClientMetadataInvalidResponse -> "ClientMetadataInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonDisabledInSettings -> "DisabledInSettings"
    AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin -> "ErrorFetchingSignin"
    AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse -> "InvalidSigninResponse"
    AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound -> "AccountsHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonAccountsNoResponse -> "AccountsNoResponse"
    AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse -> "AccountsInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound -> "IdTokenHttpNotFound"
    AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse -> "IdTokenNoResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse -> "IdTokenInvalidResponse"
    AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest -> "IdTokenInvalidRequest"
    AuditsFederatedAuthRequestIssueReasonErrorIdToken -> "ErrorIdToken"
    AuditsFederatedAuthRequestIssueReasonCanceled -> "Canceled"
    AuditsFederatedAuthRequestIssueReasonRpPageNotVisible -> "RpPageNotVisible"

-- | Type 'Audits.ClientHintIssueDetails'.
--   This issue tracks client hints related issues. It's used to deprecate old
--   features, encourage the use of new ones, and provide general guidance.
data AuditsClientHintIssueDetails = AuditsClientHintIssueDetails
  {
    auditsClientHintIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
    auditsClientHintIssueDetailsClientHintIssueReason :: AuditsClientHintIssueReason
  }
  deriving (Eq, Show)
instance FromJSON AuditsClientHintIssueDetails where
  parseJSON = A.withObject "AuditsClientHintIssueDetails" $ \o -> AuditsClientHintIssueDetails
    <$> o A..: "sourceCodeLocation"
    <*> o A..: "clientHintIssueReason"
instance ToJSON AuditsClientHintIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("sourceCodeLocation" A..=) <$> Just (auditsClientHintIssueDetailsSourceCodeLocation p),
    ("clientHintIssueReason" A..=) <$> Just (auditsClientHintIssueDetailsClientHintIssueReason p)
    ]

-- | Type 'Audits.InspectorIssueCode'.
--   A unique identifier for the type of issue. Each type may use one of the
--   optional fields in InspectorIssueDetails to convey more specific
--   information about the kind of issue.
data AuditsInspectorIssueCode = AuditsInspectorIssueCodeCookieIssue | AuditsInspectorIssueCodeMixedContentIssue | AuditsInspectorIssueCodeBlockedByResponseIssue | AuditsInspectorIssueCodeHeavyAdIssue | AuditsInspectorIssueCodeContentSecurityPolicyIssue | AuditsInspectorIssueCodeSharedArrayBufferIssue | AuditsInspectorIssueCodeTrustedWebActivityIssue | AuditsInspectorIssueCodeLowTextContrastIssue | AuditsInspectorIssueCodeCorsIssue | AuditsInspectorIssueCodeAttributionReportingIssue | AuditsInspectorIssueCodeQuirksModeIssue | AuditsInspectorIssueCodeNavigatorUserAgentIssue | AuditsInspectorIssueCodeGenericIssue | AuditsInspectorIssueCodeDeprecationIssue | AuditsInspectorIssueCodeClientHintIssue | AuditsInspectorIssueCodeFederatedAuthRequestIssue
  deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsInspectorIssueCode where
  parseJSON = A.withText "AuditsInspectorIssueCode" $ \v -> case v of
    "CookieIssue" -> pure AuditsInspectorIssueCodeCookieIssue
    "MixedContentIssue" -> pure AuditsInspectorIssueCodeMixedContentIssue
    "BlockedByResponseIssue" -> pure AuditsInspectorIssueCodeBlockedByResponseIssue
    "HeavyAdIssue" -> pure AuditsInspectorIssueCodeHeavyAdIssue
    "ContentSecurityPolicyIssue" -> pure AuditsInspectorIssueCodeContentSecurityPolicyIssue
    "SharedArrayBufferIssue" -> pure AuditsInspectorIssueCodeSharedArrayBufferIssue
    "TrustedWebActivityIssue" -> pure AuditsInspectorIssueCodeTrustedWebActivityIssue
    "LowTextContrastIssue" -> pure AuditsInspectorIssueCodeLowTextContrastIssue
    "CorsIssue" -> pure AuditsInspectorIssueCodeCorsIssue
    "AttributionReportingIssue" -> pure AuditsInspectorIssueCodeAttributionReportingIssue
    "QuirksModeIssue" -> pure AuditsInspectorIssueCodeQuirksModeIssue
    "NavigatorUserAgentIssue" -> pure AuditsInspectorIssueCodeNavigatorUserAgentIssue
    "GenericIssue" -> pure AuditsInspectorIssueCodeGenericIssue
    "DeprecationIssue" -> pure AuditsInspectorIssueCodeDeprecationIssue
    "ClientHintIssue" -> pure AuditsInspectorIssueCodeClientHintIssue
    "FederatedAuthRequestIssue" -> pure AuditsInspectorIssueCodeFederatedAuthRequestIssue
    "_" -> fail "failed to parse AuditsInspectorIssueCode"
instance ToJSON AuditsInspectorIssueCode where
  toJSON v = A.String $ case v of
    AuditsInspectorIssueCodeCookieIssue -> "CookieIssue"
    AuditsInspectorIssueCodeMixedContentIssue -> "MixedContentIssue"
    AuditsInspectorIssueCodeBlockedByResponseIssue -> "BlockedByResponseIssue"
    AuditsInspectorIssueCodeHeavyAdIssue -> "HeavyAdIssue"
    AuditsInspectorIssueCodeContentSecurityPolicyIssue -> "ContentSecurityPolicyIssue"
    AuditsInspectorIssueCodeSharedArrayBufferIssue -> "SharedArrayBufferIssue"
    AuditsInspectorIssueCodeTrustedWebActivityIssue -> "TrustedWebActivityIssue"
    AuditsInspectorIssueCodeLowTextContrastIssue -> "LowTextContrastIssue"
    AuditsInspectorIssueCodeCorsIssue -> "CorsIssue"
    AuditsInspectorIssueCodeAttributionReportingIssue -> "AttributionReportingIssue"
    AuditsInspectorIssueCodeQuirksModeIssue -> "QuirksModeIssue"
    AuditsInspectorIssueCodeNavigatorUserAgentIssue -> "NavigatorUserAgentIssue"
    AuditsInspectorIssueCodeGenericIssue -> "GenericIssue"
    AuditsInspectorIssueCodeDeprecationIssue -> "DeprecationIssue"
    AuditsInspectorIssueCodeClientHintIssue -> "ClientHintIssue"
    AuditsInspectorIssueCodeFederatedAuthRequestIssue -> "FederatedAuthRequestIssue"

-- | Type 'Audits.InspectorIssueDetails'.
--   This struct holds a list of optional fields with additional information
--   specific to the kind of issue. When adding a new issue code, please also
--   add a new optional field to this type.
data AuditsInspectorIssueDetails = AuditsInspectorIssueDetails
  {
    auditsInspectorIssueDetailsCookieIssueDetails :: Maybe AuditsCookieIssueDetails,
    auditsInspectorIssueDetailsMixedContentIssueDetails :: Maybe AuditsMixedContentIssueDetails,
    auditsInspectorIssueDetailsBlockedByResponseIssueDetails :: Maybe AuditsBlockedByResponseIssueDetails,
    auditsInspectorIssueDetailsHeavyAdIssueDetails :: Maybe AuditsHeavyAdIssueDetails,
    auditsInspectorIssueDetailsContentSecurityPolicyIssueDetails :: Maybe AuditsContentSecurityPolicyIssueDetails,
    auditsInspectorIssueDetailsSharedArrayBufferIssueDetails :: Maybe AuditsSharedArrayBufferIssueDetails,
    auditsInspectorIssueDetailsTwaQualityEnforcementDetails :: Maybe AuditsTrustedWebActivityIssueDetails,
    auditsInspectorIssueDetailsLowTextContrastIssueDetails :: Maybe AuditsLowTextContrastIssueDetails,
    auditsInspectorIssueDetailsCorsIssueDetails :: Maybe AuditsCorsIssueDetails,
    auditsInspectorIssueDetailsAttributionReportingIssueDetails :: Maybe AuditsAttributionReportingIssueDetails,
    auditsInspectorIssueDetailsQuirksModeIssueDetails :: Maybe AuditsQuirksModeIssueDetails,
    auditsInspectorIssueDetailsNavigatorUserAgentIssueDetails :: Maybe AuditsNavigatorUserAgentIssueDetails,
    auditsInspectorIssueDetailsGenericIssueDetails :: Maybe AuditsGenericIssueDetails,
    auditsInspectorIssueDetailsDeprecationIssueDetails :: Maybe AuditsDeprecationIssueDetails,
    auditsInspectorIssueDetailsClientHintIssueDetails :: Maybe AuditsClientHintIssueDetails,
    auditsInspectorIssueDetailsFederatedAuthRequestIssueDetails :: Maybe AuditsFederatedAuthRequestIssueDetails
  }
  deriving (Eq, Show)
instance FromJSON AuditsInspectorIssueDetails where
  parseJSON = A.withObject "AuditsInspectorIssueDetails" $ \o -> AuditsInspectorIssueDetails
    <$> o A..:? "cookieIssueDetails"
    <*> o A..:? "mixedContentIssueDetails"
    <*> o A..:? "blockedByResponseIssueDetails"
    <*> o A..:? "heavyAdIssueDetails"
    <*> o A..:? "contentSecurityPolicyIssueDetails"
    <*> o A..:? "sharedArrayBufferIssueDetails"
    <*> o A..:? "twaQualityEnforcementDetails"
    <*> o A..:? "lowTextContrastIssueDetails"
    <*> o A..:? "corsIssueDetails"
    <*> o A..:? "attributionReportingIssueDetails"
    <*> o A..:? "quirksModeIssueDetails"
    <*> o A..:? "navigatorUserAgentIssueDetails"
    <*> o A..:? "genericIssueDetails"
    <*> o A..:? "deprecationIssueDetails"
    <*> o A..:? "clientHintIssueDetails"
    <*> o A..:? "federatedAuthRequestIssueDetails"
instance ToJSON AuditsInspectorIssueDetails where
  toJSON p = A.object $ catMaybes [
    ("cookieIssueDetails" A..=) <$> (auditsInspectorIssueDetailsCookieIssueDetails p),
    ("mixedContentIssueDetails" A..=) <$> (auditsInspectorIssueDetailsMixedContentIssueDetails p),
    ("blockedByResponseIssueDetails" A..=) <$> (auditsInspectorIssueDetailsBlockedByResponseIssueDetails p),
    ("heavyAdIssueDetails" A..=) <$> (auditsInspectorIssueDetailsHeavyAdIssueDetails p),
    ("contentSecurityPolicyIssueDetails" A..=) <$> (auditsInspectorIssueDetailsContentSecurityPolicyIssueDetails p),
    ("sharedArrayBufferIssueDetails" A..=) <$> (auditsInspectorIssueDetailsSharedArrayBufferIssueDetails p),
    ("twaQualityEnforcementDetails" A..=) <$> (auditsInspectorIssueDetailsTwaQualityEnforcementDetails p),
    ("lowTextContrastIssueDetails" A..=) <$> (auditsInspectorIssueDetailsLowTextContrastIssueDetails p),
    ("corsIssueDetails" A..=) <$> (auditsInspectorIssueDetailsCorsIssueDetails p),
    ("attributionReportingIssueDetails" A..=) <$> (auditsInspectorIssueDetailsAttributionReportingIssueDetails p),
    ("quirksModeIssueDetails" A..=) <$> (auditsInspectorIssueDetailsQuirksModeIssueDetails p),
    ("navigatorUserAgentIssueDetails" A..=) <$> (auditsInspectorIssueDetailsNavigatorUserAgentIssueDetails p),
    ("genericIssueDetails" A..=) <$> (auditsInspectorIssueDetailsGenericIssueDetails p),
    ("deprecationIssueDetails" A..=) <$> (auditsInspectorIssueDetailsDeprecationIssueDetails p),
    ("clientHintIssueDetails" A..=) <$> (auditsInspectorIssueDetailsClientHintIssueDetails p),
    ("federatedAuthRequestIssueDetails" A..=) <$> (auditsInspectorIssueDetailsFederatedAuthRequestIssueDetails p)
    ]

-- | Type 'Audits.IssueId'.
--   A unique id for a DevTools inspector issue. Allows other entities (e.g.
--   exceptions, CDP message, console messages, etc.) to reference an issue.
type AuditsIssueId = String

-- | Type 'Audits.InspectorIssue'.
--   An inspector issue reported from the back-end.
data AuditsInspectorIssue = AuditsInspectorIssue
  {
    auditsInspectorIssueCode :: AuditsInspectorIssueCode,
    auditsInspectorIssueDetails :: AuditsInspectorIssueDetails,
    -- | A unique id for this issue. May be omitted if no other entity (e.g.
    --   exception, CDP message, etc.) is referencing this issue.
    auditsInspectorIssueIssueId :: Maybe AuditsIssueId
  }
  deriving (Eq, Show)
instance FromJSON AuditsInspectorIssue where
  parseJSON = A.withObject "AuditsInspectorIssue" $ \o -> AuditsInspectorIssue
    <$> o A..: "code"
    <*> o A..: "details"
    <*> o A..:? "issueId"
instance ToJSON AuditsInspectorIssue where
  toJSON p = A.object $ catMaybes [
    ("code" A..=) <$> Just (auditsInspectorIssueCode p),
    ("details" A..=) <$> Just (auditsInspectorIssueDetails p),
    ("issueId" A..=) <$> (auditsInspectorIssueIssueId p)
    ]

-- | Type of the 'Audits.issueAdded' event.
data AuditsIssueAdded = AuditsIssueAdded
  {
    auditsIssueAddedIssue :: AuditsInspectorIssue
  }
  deriving (Eq, Show)
instance FromJSON AuditsIssueAdded where
  parseJSON = A.withObject "AuditsIssueAdded" $ \o -> AuditsIssueAdded
    <$> o A..: "issue"
instance Event AuditsIssueAdded where
  eventName _ = "Audits.issueAdded"

-- | Returns the response body and size if it were re-encoded with the specified settings. Only
--   applies to images.

-- | Parameters of the 'Audits.getEncodedResponse' command.
data PAuditsGetEncodedResponseEncoding = PAuditsGetEncodedResponseEncodingWebp | PAuditsGetEncodedResponseEncodingJpeg | PAuditsGetEncodedResponseEncodingPng
  deriving (Ord, Eq, Show, Read)
instance FromJSON PAuditsGetEncodedResponseEncoding where
  parseJSON = A.withText "PAuditsGetEncodedResponseEncoding" $ \v -> case v of
    "webp" -> pure PAuditsGetEncodedResponseEncodingWebp
    "jpeg" -> pure PAuditsGetEncodedResponseEncodingJpeg
    "png" -> pure PAuditsGetEncodedResponseEncodingPng
    "_" -> fail "failed to parse PAuditsGetEncodedResponseEncoding"
instance ToJSON PAuditsGetEncodedResponseEncoding where
  toJSON v = A.String $ case v of
    PAuditsGetEncodedResponseEncodingWebp -> "webp"
    PAuditsGetEncodedResponseEncodingJpeg -> "jpeg"
    PAuditsGetEncodedResponseEncodingPng -> "png"
data PAuditsGetEncodedResponse = PAuditsGetEncodedResponse
  {
    -- | Identifier of the network request to get content for.
    pAuditsGetEncodedResponseRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
    -- | The encoding to use.
    pAuditsGetEncodedResponseEncoding :: PAuditsGetEncodedResponseEncoding,
    -- | The quality of the encoding (0-1). (defaults to 1)
    pAuditsGetEncodedResponseQuality :: Maybe Double,
    -- | Whether to only return the size information (defaults to false).
    pAuditsGetEncodedResponseSizeOnly :: Maybe Bool
  }
  deriving (Eq, Show)
pAuditsGetEncodedResponse
  -- | Identifier of the network request to get content for.
  :: DOMPageNetworkEmulationSecurity.NetworkRequestId
  -- | The encoding to use.
  -> PAuditsGetEncodedResponseEncoding
  -> PAuditsGetEncodedResponse
pAuditsGetEncodedResponse
  arg_pAuditsGetEncodedResponseRequestId
  arg_pAuditsGetEncodedResponseEncoding
  = PAuditsGetEncodedResponse
    arg_pAuditsGetEncodedResponseRequestId
    arg_pAuditsGetEncodedResponseEncoding
    Nothing
    Nothing
instance ToJSON PAuditsGetEncodedResponse where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pAuditsGetEncodedResponseRequestId p),
    ("encoding" A..=) <$> Just (pAuditsGetEncodedResponseEncoding p),
    ("quality" A..=) <$> (pAuditsGetEncodedResponseQuality p),
    ("sizeOnly" A..=) <$> (pAuditsGetEncodedResponseSizeOnly p)
    ]
data AuditsGetEncodedResponse = AuditsGetEncodedResponse
  {
    -- | The encoded body as a base64 string. Omitted if sizeOnly is true. (Encoded as a base64 string when passed over JSON)
    auditsGetEncodedResponseBody :: Maybe String,
    -- | Size before re-encoding.
    auditsGetEncodedResponseOriginalSize :: Int,
    -- | Size after re-encoding.
    auditsGetEncodedResponseEncodedSize :: Int
  }
  deriving (Eq, Show)
instance FromJSON AuditsGetEncodedResponse where
  parseJSON = A.withObject "AuditsGetEncodedResponse" $ \o -> AuditsGetEncodedResponse
    <$> o A..:? "body"
    <*> o A..: "originalSize"
    <*> o A..: "encodedSize"
instance Command PAuditsGetEncodedResponse where
  type CommandResponse PAuditsGetEncodedResponse = AuditsGetEncodedResponse
  commandName _ = "Audits.getEncodedResponse"

-- | Disables issues domain, prevents further issues from being reported to the client.

-- | Parameters of the 'Audits.disable' command.
data PAuditsDisable = PAuditsDisable
  deriving (Eq, Show)
pAuditsDisable
  :: PAuditsDisable
pAuditsDisable
  = PAuditsDisable
instance ToJSON PAuditsDisable where
  toJSON _ = A.Null
instance Command PAuditsDisable where
  type CommandResponse PAuditsDisable = ()
  commandName _ = "Audits.disable"
  fromJSON = const . A.Success . const ()

-- | Enables issues domain, sends the issues collected so far to the client by means of the
--   `issueAdded` event.

-- | Parameters of the 'Audits.enable' command.
data PAuditsEnable = PAuditsEnable
  deriving (Eq, Show)
pAuditsEnable
  :: PAuditsEnable
pAuditsEnable
  = PAuditsEnable
instance ToJSON PAuditsEnable where
  toJSON _ = A.Null
instance Command PAuditsEnable where
  type CommandResponse PAuditsEnable = ()
  commandName _ = "Audits.enable"
  fromJSON = const . A.Success . const ()

-- | Runs the contrast check for the target page. Found issues are reported
--   using Audits.issueAdded event.

-- | Parameters of the 'Audits.checkContrast' command.
data PAuditsCheckContrast = PAuditsCheckContrast
  {
    -- | Whether to report WCAG AAA level issues. Default is false.
    pAuditsCheckContrastReportAAA :: Maybe Bool
  }
  deriving (Eq, Show)
pAuditsCheckContrast
  :: PAuditsCheckContrast
pAuditsCheckContrast
  = PAuditsCheckContrast
    Nothing
instance ToJSON PAuditsCheckContrast where
  toJSON p = A.object $ catMaybes [
    ("reportAAA" A..=) <$> (pAuditsCheckContrastReportAAA p)
    ]
instance Command PAuditsCheckContrast where
  type CommandResponse PAuditsCheckContrast = ()
  commandName _ = "Audits.checkContrast"
  fromJSON = const . A.Success . const ()

