{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Audits :
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
import Data.Text (Text(..))
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
import CDP.Domains.Runtime as Runtime


-- | Type 'Audits.AffectedCookie'.
--   Information about a cookie that is affected by an inspector issue.
data AuditsAffectedCookie = AuditsAffectedCookie {
  -- | The following three properties uniquely identify a cookie
  auditsAffectedCookieName :: Text,
  auditsAffectedCookiePath :: Text,
  auditsAffectedCookieDomain :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Audits.AffectedRequest'.
--   Information about a request that is affected by an inspector issue.
data AuditsAffectedRequest = AuditsAffectedRequest {
  -- | The unique request id.
  auditsAffectedRequestRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
  auditsAffectedRequestUrl :: Maybe Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Audits.AffectedFrame'.
--   Information about the frame affected by an inspector issue.
data AuditsAffectedFrame = AuditsAffectedFrame {
  auditsAffectedFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Audits.CookieExclusionReason'.
data AuditsCookieExclusionReason = AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax | AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure | AuditsCookieExclusionReasonExcludeSameSiteLax | AuditsCookieExclusionReasonExcludeSameSiteStrict | AuditsCookieExclusionReasonExcludeInvalidSameParty | AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieExclusionReason where
   parseJSON = A.withText  "AuditsCookieExclusionReason"  $ \v -> do
      case v of
         "ExcludeSameSiteUnspecifiedTreatedAsLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax
         "ExcludeSameSiteNoneInsecure" -> pure AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure
         "ExcludeSameSiteLax" -> pure AuditsCookieExclusionReasonExcludeSameSiteLax
         "ExcludeSameSiteStrict" -> pure AuditsCookieExclusionReasonExcludeSameSiteStrict
         "ExcludeInvalidSameParty" -> pure AuditsCookieExclusionReasonExcludeInvalidSameParty
         "ExcludeSamePartyCrossPartyContext" -> pure AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext
         _ -> fail "failed to parse AuditsCookieExclusionReason"

instance ToJSON AuditsCookieExclusionReason where
   toJSON v = A.String $
      case v of
         AuditsCookieExclusionReasonExcludeSameSiteUnspecifiedTreatedAsLax -> "ExcludeSameSiteUnspecifiedTreatedAsLax"
         AuditsCookieExclusionReasonExcludeSameSiteNoneInsecure -> "ExcludeSameSiteNoneInsecure"
         AuditsCookieExclusionReasonExcludeSameSiteLax -> "ExcludeSameSiteLax"
         AuditsCookieExclusionReasonExcludeSameSiteStrict -> "ExcludeSameSiteStrict"
         AuditsCookieExclusionReasonExcludeInvalidSameParty -> "ExcludeInvalidSameParty"
         AuditsCookieExclusionReasonExcludeSamePartyCrossPartyContext -> "ExcludeSamePartyCrossPartyContext"



-- | Type 'Audits.CookieWarningReason'.
data AuditsCookieWarningReason = AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext | AuditsCookieWarningReasonWarnSameSiteNoneInsecure | AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe | AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict | AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax | AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieWarningReason where
   parseJSON = A.withText  "AuditsCookieWarningReason"  $ \v -> do
      case v of
         "WarnSameSiteUnspecifiedCrossSiteContext" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext
         "WarnSameSiteNoneInsecure" -> pure AuditsCookieWarningReasonWarnSameSiteNoneInsecure
         "WarnSameSiteUnspecifiedLaxAllowUnsafe" -> pure AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe
         "WarnSameSiteStrictLaxDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict
         "WarnSameSiteStrictCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict
         "WarnSameSiteStrictCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax
         "WarnSameSiteLaxCrossDowngradeStrict" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict
         "WarnSameSiteLaxCrossDowngradeLax" -> pure AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax
         "WarnAttributeValueExceedsMaxSize" -> pure AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize
         _ -> fail "failed to parse AuditsCookieWarningReason"

instance ToJSON AuditsCookieWarningReason where
   toJSON v = A.String $
      case v of
         AuditsCookieWarningReasonWarnSameSiteUnspecifiedCrossSiteContext -> "WarnSameSiteUnspecifiedCrossSiteContext"
         AuditsCookieWarningReasonWarnSameSiteNoneInsecure -> "WarnSameSiteNoneInsecure"
         AuditsCookieWarningReasonWarnSameSiteUnspecifiedLaxAllowUnsafe -> "WarnSameSiteUnspecifiedLaxAllowUnsafe"
         AuditsCookieWarningReasonWarnSameSiteStrictLaxDowngradeStrict -> "WarnSameSiteStrictLaxDowngradeStrict"
         AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeStrict -> "WarnSameSiteStrictCrossDowngradeStrict"
         AuditsCookieWarningReasonWarnSameSiteStrictCrossDowngradeLax -> "WarnSameSiteStrictCrossDowngradeLax"
         AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeStrict -> "WarnSameSiteLaxCrossDowngradeStrict"
         AuditsCookieWarningReasonWarnSameSiteLaxCrossDowngradeLax -> "WarnSameSiteLaxCrossDowngradeLax"
         AuditsCookieWarningReasonWarnAttributeValueExceedsMaxSize -> "WarnAttributeValueExceedsMaxSize"



-- | Type 'Audits.CookieOperation'.
data AuditsCookieOperation = AuditsCookieOperationSetCookie | AuditsCookieOperationReadCookie
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsCookieOperation where
   parseJSON = A.withText  "AuditsCookieOperation"  $ \v -> do
      case v of
         "SetCookie" -> pure AuditsCookieOperationSetCookie
         "ReadCookie" -> pure AuditsCookieOperationReadCookie
         _ -> fail "failed to parse AuditsCookieOperation"

instance ToJSON AuditsCookieOperation where
   toJSON v = A.String $
      case v of
         AuditsCookieOperationSetCookie -> "SetCookie"
         AuditsCookieOperationReadCookie -> "ReadCookie"



-- | Type 'Audits.CookieIssueDetails'.
--   This information is currently necessary, as the front-end has a difficult
--   time finding a specific cookie. With this, we can convey specific error
--   information without the cookie.
data AuditsCookieIssueDetails = AuditsCookieIssueDetails {
  -- | If AffectedCookie is not set then rawCookieLine contains the raw
  --   Set-Cookie header string. This hints at a problem where the
  --   cookie line is syntactically or semantically malformed in a way
  --   that no valid cookie could be created.
  auditsCookieIssueDetailsCookie :: Maybe AuditsAffectedCookie,
  auditsCookieIssueDetailsRawCookieLine :: Maybe Text,
  auditsCookieIssueDetailsCookieWarningReasons :: [AuditsCookieWarningReason],
  auditsCookieIssueDetailsCookieExclusionReasons :: [AuditsCookieExclusionReason],
  -- | Optionally identifies the site-for-cookies and the cookie url, which
  --   may be used by the front-end as additional context.
  auditsCookieIssueDetailsOperation :: AuditsCookieOperation,
  auditsCookieIssueDetailsSiteForCookies :: Maybe Text,
  auditsCookieIssueDetailsCookieUrl :: Maybe Text,
  auditsCookieIssueDetailsRequest :: Maybe AuditsAffectedRequest
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsCookieIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AuditsCookieIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Audits.MixedContentResolutionStatus'.
data AuditsMixedContentResolutionStatus = AuditsMixedContentResolutionStatusMixedContentBlocked | AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded | AuditsMixedContentResolutionStatusMixedContentWarning
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResolutionStatus where
   parseJSON = A.withText  "AuditsMixedContentResolutionStatus"  $ \v -> do
      case v of
         "MixedContentBlocked" -> pure AuditsMixedContentResolutionStatusMixedContentBlocked
         "MixedContentAutomaticallyUpgraded" -> pure AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded
         "MixedContentWarning" -> pure AuditsMixedContentResolutionStatusMixedContentWarning
         _ -> fail "failed to parse AuditsMixedContentResolutionStatus"

instance ToJSON AuditsMixedContentResolutionStatus where
   toJSON v = A.String $
      case v of
         AuditsMixedContentResolutionStatusMixedContentBlocked -> "MixedContentBlocked"
         AuditsMixedContentResolutionStatusMixedContentAutomaticallyUpgraded -> "MixedContentAutomaticallyUpgraded"
         AuditsMixedContentResolutionStatusMixedContentWarning -> "MixedContentWarning"



-- | Type 'Audits.MixedContentResourceType'.
data AuditsMixedContentResourceType = AuditsMixedContentResourceTypeAttributionSrc | AuditsMixedContentResourceTypeAudio | AuditsMixedContentResourceTypeBeacon | AuditsMixedContentResourceTypeCspReport | AuditsMixedContentResourceTypeDownload | AuditsMixedContentResourceTypeEventSource | AuditsMixedContentResourceTypeFavicon | AuditsMixedContentResourceTypeFont | AuditsMixedContentResourceTypeForm | AuditsMixedContentResourceTypeFrame | AuditsMixedContentResourceTypeImage | AuditsMixedContentResourceTypeImport | AuditsMixedContentResourceTypeManifest | AuditsMixedContentResourceTypePing | AuditsMixedContentResourceTypePluginData | AuditsMixedContentResourceTypePluginResource | AuditsMixedContentResourceTypePrefetch | AuditsMixedContentResourceTypeResource | AuditsMixedContentResourceTypeScript | AuditsMixedContentResourceTypeServiceWorker | AuditsMixedContentResourceTypeSharedWorker | AuditsMixedContentResourceTypeStylesheet | AuditsMixedContentResourceTypeTrack | AuditsMixedContentResourceTypeVideo | AuditsMixedContentResourceTypeWorker | AuditsMixedContentResourceTypeXmlHttpRequest | AuditsMixedContentResourceTypeXslt
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsMixedContentResourceType where
   parseJSON = A.withText  "AuditsMixedContentResourceType"  $ \v -> do
      case v of
         "AttributionSrc" -> pure AuditsMixedContentResourceTypeAttributionSrc
         "Audio" -> pure AuditsMixedContentResourceTypeAudio
         "Beacon" -> pure AuditsMixedContentResourceTypeBeacon
         "CSPReport" -> pure AuditsMixedContentResourceTypeCspReport
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
         "XMLHttpRequest" -> pure AuditsMixedContentResourceTypeXmlHttpRequest
         "XSLT" -> pure AuditsMixedContentResourceTypeXslt
         _ -> fail "failed to parse AuditsMixedContentResourceType"

instance ToJSON AuditsMixedContentResourceType where
   toJSON v = A.String $
      case v of
         AuditsMixedContentResourceTypeAttributionSrc -> "AttributionSrc"
         AuditsMixedContentResourceTypeAudio -> "Audio"
         AuditsMixedContentResourceTypeBeacon -> "Beacon"
         AuditsMixedContentResourceTypeCspReport -> "CSPReport"
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
         AuditsMixedContentResourceTypeXmlHttpRequest -> "XMLHttpRequest"
         AuditsMixedContentResourceTypeXslt -> "XSLT"



-- | Type 'Audits.MixedContentIssueDetails'.
data AuditsMixedContentIssueDetails = AuditsMixedContentIssueDetails {
  -- | The type of resource causing the mixed content issue (css, js, iframe,
  --   form,...). Marked as optional because it is mapped to from
  --   blink::mojom::RequestContextType, which will be replaced
  --   by network::mojom::RequestDestination
  auditsMixedContentIssueDetailsResourceType :: Maybe AuditsMixedContentResourceType,
  -- | The way the mixed content issue is being resolved.
  auditsMixedContentIssueDetailsResolutionStatus :: AuditsMixedContentResolutionStatus,
  -- | The unsafe http url causing the mixed content issue.
  auditsMixedContentIssueDetailsInsecureUrl :: Text,
  -- | The url responsible for the call to an unsafe url.
  auditsMixedContentIssueDetailsMainResourceUrl :: Text,
  -- | The mixed content request.
  --   Does not always exist (e.g. for unsafe form submission urls).
  auditsMixedContentIssueDetailsRequest :: Maybe AuditsAffectedRequest,
  -- | Optional because not every mixed content issue is necessarily linked to a frame.
  auditsMixedContentIssueDetailsFrame :: Maybe AuditsAffectedFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsMixedContentIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  AuditsMixedContentIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Audits.BlockedByResponseReason'.
--   Enum indicating the reason a response has been blocked. These reasons are
--   refinements of the net error BLOCKED_BY_RESPONSE.
data AuditsBlockedByResponseReason = AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader | AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage | AuditsBlockedByResponseReasonCorpNotSameOrigin | AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | AuditsBlockedByResponseReasonCorpNotSameSite
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsBlockedByResponseReason where
   parseJSON = A.withText  "AuditsBlockedByResponseReason"  $ \v -> do
      case v of
         "CoepFrameResourceNeedsCoepHeader" -> pure AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader
         "CoopSandboxedIFrameCannotNavigateToCoopPage" -> pure AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage
         "CorpNotSameOrigin" -> pure AuditsBlockedByResponseReasonCorpNotSameOrigin
         "CorpNotSameOriginAfterDefaultedToSameOriginByCoep" -> pure AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
         "CorpNotSameSite" -> pure AuditsBlockedByResponseReasonCorpNotSameSite
         _ -> fail "failed to parse AuditsBlockedByResponseReason"

instance ToJSON AuditsBlockedByResponseReason where
   toJSON v = A.String $
      case v of
         AuditsBlockedByResponseReasonCoepFrameResourceNeedsCoepHeader -> "CoepFrameResourceNeedsCoepHeader"
         AuditsBlockedByResponseReasonCoopSandboxedIFrameCannotNavigateToCoopPage -> "CoopSandboxedIFrameCannotNavigateToCoopPage"
         AuditsBlockedByResponseReasonCorpNotSameOrigin -> "CorpNotSameOrigin"
         AuditsBlockedByResponseReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "CorpNotSameOriginAfterDefaultedToSameOriginByCoep"
         AuditsBlockedByResponseReasonCorpNotSameSite -> "CorpNotSameSite"



-- | Type 'Audits.BlockedByResponseIssueDetails'.
--   Details for a request that has been blocked with the BLOCKED_BY_RESPONSE
--   code. Currently only used for COEP/COOP, but may be extended to include
--   some CSP errors in the future.
data AuditsBlockedByResponseIssueDetails = AuditsBlockedByResponseIssueDetails {
  auditsBlockedByResponseIssueDetailsRequest :: AuditsAffectedRequest,
  auditsBlockedByResponseIssueDetailsParentFrame :: Maybe AuditsAffectedFrame,
  auditsBlockedByResponseIssueDetailsBlockedFrame :: Maybe AuditsAffectedFrame,
  auditsBlockedByResponseIssueDetailsReason :: AuditsBlockedByResponseReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsBlockedByResponseIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  AuditsBlockedByResponseIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Audits.HeavyAdResolutionStatus'.
data AuditsHeavyAdResolutionStatus = AuditsHeavyAdResolutionStatusHeavyAdBlocked | AuditsHeavyAdResolutionStatusHeavyAdWarning
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdResolutionStatus where
   parseJSON = A.withText  "AuditsHeavyAdResolutionStatus"  $ \v -> do
      case v of
         "HeavyAdBlocked" -> pure AuditsHeavyAdResolutionStatusHeavyAdBlocked
         "HeavyAdWarning" -> pure AuditsHeavyAdResolutionStatusHeavyAdWarning
         _ -> fail "failed to parse AuditsHeavyAdResolutionStatus"

instance ToJSON AuditsHeavyAdResolutionStatus where
   toJSON v = A.String $
      case v of
         AuditsHeavyAdResolutionStatusHeavyAdBlocked -> "HeavyAdBlocked"
         AuditsHeavyAdResolutionStatusHeavyAdWarning -> "HeavyAdWarning"



-- | Type 'Audits.HeavyAdReason'.
data AuditsHeavyAdReason = AuditsHeavyAdReasonNetworkTotalLimit | AuditsHeavyAdReasonCpuTotalLimit | AuditsHeavyAdReasonCpuPeakLimit
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsHeavyAdReason where
   parseJSON = A.withText  "AuditsHeavyAdReason"  $ \v -> do
      case v of
         "NetworkTotalLimit" -> pure AuditsHeavyAdReasonNetworkTotalLimit
         "CpuTotalLimit" -> pure AuditsHeavyAdReasonCpuTotalLimit
         "CpuPeakLimit" -> pure AuditsHeavyAdReasonCpuPeakLimit
         _ -> fail "failed to parse AuditsHeavyAdReason"

instance ToJSON AuditsHeavyAdReason where
   toJSON v = A.String $
      case v of
         AuditsHeavyAdReasonNetworkTotalLimit -> "NetworkTotalLimit"
         AuditsHeavyAdReasonCpuTotalLimit -> "CpuTotalLimit"
         AuditsHeavyAdReasonCpuPeakLimit -> "CpuPeakLimit"



-- | Type 'Audits.HeavyAdIssueDetails'.
data AuditsHeavyAdIssueDetails = AuditsHeavyAdIssueDetails {
  -- | The resolution status, either blocking the content or warning.
  auditsHeavyAdIssueDetailsResolution :: AuditsHeavyAdResolutionStatus,
  -- | The reason the ad was blocked, total network or cpu or peak cpu.
  auditsHeavyAdIssueDetailsReason :: AuditsHeavyAdReason,
  -- | The frame that was blocked.
  auditsHeavyAdIssueDetailsFrame :: AuditsAffectedFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsHeavyAdIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AuditsHeavyAdIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'Audits.ContentSecurityPolicyViolationType'.
data AuditsContentSecurityPolicyViolationType = AuditsContentSecurityPolicyViolationTypeKInlineViolation | AuditsContentSecurityPolicyViolationTypeKEvalViolation | AuditsContentSecurityPolicyViolationTypeKUrlViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation | AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation | AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsContentSecurityPolicyViolationType where
   parseJSON = A.withText  "AuditsContentSecurityPolicyViolationType"  $ \v -> do
      case v of
         "kInlineViolation" -> pure AuditsContentSecurityPolicyViolationTypeKInlineViolation
         "kEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKEvalViolation
         "kURLViolation" -> pure AuditsContentSecurityPolicyViolationTypeKUrlViolation
         "kTrustedTypesSinkViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation
         "kTrustedTypesPolicyViolation" -> pure AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation
         "kWasmEvalViolation" -> pure AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation
         _ -> fail "failed to parse AuditsContentSecurityPolicyViolationType"

instance ToJSON AuditsContentSecurityPolicyViolationType where
   toJSON v = A.String $
      case v of
         AuditsContentSecurityPolicyViolationTypeKInlineViolation -> "kInlineViolation"
         AuditsContentSecurityPolicyViolationTypeKEvalViolation -> "kEvalViolation"
         AuditsContentSecurityPolicyViolationTypeKUrlViolation -> "kURLViolation"
         AuditsContentSecurityPolicyViolationTypeKTrustedTypesSinkViolation -> "kTrustedTypesSinkViolation"
         AuditsContentSecurityPolicyViolationTypeKTrustedTypesPolicyViolation -> "kTrustedTypesPolicyViolation"
         AuditsContentSecurityPolicyViolationTypeKWasmEvalViolation -> "kWasmEvalViolation"



-- | Type 'Audits.SourceCodeLocation'.
data AuditsSourceCodeLocation = AuditsSourceCodeLocation {
  auditsSourceCodeLocationScriptId :: Maybe Runtime.RuntimeScriptId,
  auditsSourceCodeLocationUrl :: Text,
  auditsSourceCodeLocationLineNumber :: Int,
  auditsSourceCodeLocationColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsSourceCodeLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AuditsSourceCodeLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Audits.ContentSecurityPolicyIssueDetails'.
data AuditsContentSecurityPolicyIssueDetails = AuditsContentSecurityPolicyIssueDetails {
  -- | The url not included in allowed sources.
  auditsContentSecurityPolicyIssueDetailsBlockedUrl :: Maybe Text,
  -- | Specific directive that is violated, causing the CSP issue.
  auditsContentSecurityPolicyIssueDetailsViolatedDirective :: Text,
  auditsContentSecurityPolicyIssueDetailsIsReportOnly :: Bool,
  auditsContentSecurityPolicyIssueDetailsContentSecurityPolicyViolationType :: AuditsContentSecurityPolicyViolationType,
  auditsContentSecurityPolicyIssueDetailsFrameAncestor :: Maybe AuditsAffectedFrame,
  auditsContentSecurityPolicyIssueDetailsSourceCodeLocation :: Maybe AuditsSourceCodeLocation,
  auditsContentSecurityPolicyIssueDetailsViolatingNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsContentSecurityPolicyIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  AuditsContentSecurityPolicyIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }



-- | Type 'Audits.SharedArrayBufferIssueType'.
data AuditsSharedArrayBufferIssueType = AuditsSharedArrayBufferIssueTypeTransferIssue | AuditsSharedArrayBufferIssueTypeCreationIssue
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsSharedArrayBufferIssueType where
   parseJSON = A.withText  "AuditsSharedArrayBufferIssueType"  $ \v -> do
      case v of
         "TransferIssue" -> pure AuditsSharedArrayBufferIssueTypeTransferIssue
         "CreationIssue" -> pure AuditsSharedArrayBufferIssueTypeCreationIssue
         _ -> fail "failed to parse AuditsSharedArrayBufferIssueType"

instance ToJSON AuditsSharedArrayBufferIssueType where
   toJSON v = A.String $
      case v of
         AuditsSharedArrayBufferIssueTypeTransferIssue -> "TransferIssue"
         AuditsSharedArrayBufferIssueTypeCreationIssue -> "CreationIssue"



-- | Type 'Audits.SharedArrayBufferIssueDetails'.
--   Details for a issue arising from an SAB being instantiated in, or
--   transferred to a context that is not cross-origin isolated.
data AuditsSharedArrayBufferIssueDetails = AuditsSharedArrayBufferIssueDetails {
  auditsSharedArrayBufferIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
  auditsSharedArrayBufferIssueDetailsIsWarning :: Bool,
  auditsSharedArrayBufferIssueDetailsType :: AuditsSharedArrayBufferIssueType
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsSharedArrayBufferIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  AuditsSharedArrayBufferIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Audits.TwaQualityEnforcementViolationType'.
data AuditsTwaQualityEnforcementViolationType = AuditsTwaQualityEnforcementViolationTypeKHttpError | AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline | AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsTwaQualityEnforcementViolationType where
   parseJSON = A.withText  "AuditsTwaQualityEnforcementViolationType"  $ \v -> do
      case v of
         "kHttpError" -> pure AuditsTwaQualityEnforcementViolationTypeKHttpError
         "kUnavailableOffline" -> pure AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline
         "kDigitalAssetLinks" -> pure AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks
         _ -> fail "failed to parse AuditsTwaQualityEnforcementViolationType"

instance ToJSON AuditsTwaQualityEnforcementViolationType where
   toJSON v = A.String $
      case v of
         AuditsTwaQualityEnforcementViolationTypeKHttpError -> "kHttpError"
         AuditsTwaQualityEnforcementViolationTypeKUnavailableOffline -> "kUnavailableOffline"
         AuditsTwaQualityEnforcementViolationTypeKDigitalAssetLinks -> "kDigitalAssetLinks"



-- | Type 'Audits.TrustedWebActivityIssueDetails'.
data AuditsTrustedWebActivityIssueDetails = AuditsTrustedWebActivityIssueDetails {
  -- | The url that triggers the violation.
  auditsTrustedWebActivityIssueDetailsUrl :: Text,
  auditsTrustedWebActivityIssueDetailsViolationType :: AuditsTwaQualityEnforcementViolationType,
  auditsTrustedWebActivityIssueDetailsHttpStatusCode :: Maybe Int,
  -- | The package name of the Trusted Web Activity client app. This field is
  --   only used when violation type is kDigitalAssetLinks.
  auditsTrustedWebActivityIssueDetailsPackageName :: Maybe Text,
  -- | The signature of the Trusted Web Activity client app. This field is only
  --   used when violation type is kDigitalAssetLinks.
  auditsTrustedWebActivityIssueDetailsSignature :: Maybe Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsTrustedWebActivityIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  AuditsTrustedWebActivityIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Audits.LowTextContrastIssueDetails'.
data AuditsLowTextContrastIssueDetails = AuditsLowTextContrastIssueDetails {
  auditsLowTextContrastIssueDetailsViolatingNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  auditsLowTextContrastIssueDetailsViolatingNodeSelector :: Text,
  auditsLowTextContrastIssueDetailsContrastRatio :: Double,
  auditsLowTextContrastIssueDetailsThresholdAa :: Double,
  auditsLowTextContrastIssueDetailsThresholdAaa :: Double,
  auditsLowTextContrastIssueDetailsFontSize :: Text,
  auditsLowTextContrastIssueDetailsFontWeight :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsLowTextContrastIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  AuditsLowTextContrastIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Audits.CorsIssueDetails'.
--   Details for a CORS related issue, e.g. a warning or error related to
--   CORS RFC1918 enforcement.
data AuditsCorsIssueDetails = AuditsCorsIssueDetails {
  auditsCorsIssueDetailsCorsErrorStatus :: DOMPageNetworkEmulationSecurity.NetworkCorsErrorStatus,
  auditsCorsIssueDetailsIsWarning :: Bool,
  auditsCorsIssueDetailsRequest :: AuditsAffectedRequest,
  auditsCorsIssueDetailsLocation :: Maybe AuditsSourceCodeLocation,
  auditsCorsIssueDetailsInitiatorOrigin :: Maybe Text,
  auditsCorsIssueDetailsResourceIpAddressSpace :: Maybe DOMPageNetworkEmulationSecurity.NetworkIpAddressSpace,
  auditsCorsIssueDetailsClientSecurityState :: Maybe DOMPageNetworkEmulationSecurity.NetworkClientSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsCorsIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AuditsCorsIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Audits.AttributionReportingIssueType'.
data AuditsAttributionReportingIssueType = AuditsAttributionReportingIssueTypePermissionPolicyDisabled | AuditsAttributionReportingIssueTypeAttributionSourceUntrustworthyOrigin | AuditsAttributionReportingIssueTypeAttributionUntrustworthyOrigin | AuditsAttributionReportingIssueTypeInvalidHeader
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsAttributionReportingIssueType where
   parseJSON = A.withText  "AuditsAttributionReportingIssueType"  $ \v -> do
      case v of
         "PermissionPolicyDisabled" -> pure AuditsAttributionReportingIssueTypePermissionPolicyDisabled
         "AttributionSourceUntrustworthyOrigin" -> pure AuditsAttributionReportingIssueTypeAttributionSourceUntrustworthyOrigin
         "AttributionUntrustworthyOrigin" -> pure AuditsAttributionReportingIssueTypeAttributionUntrustworthyOrigin
         "InvalidHeader" -> pure AuditsAttributionReportingIssueTypeInvalidHeader
         _ -> fail "failed to parse AuditsAttributionReportingIssueType"

instance ToJSON AuditsAttributionReportingIssueType where
   toJSON v = A.String $
      case v of
         AuditsAttributionReportingIssueTypePermissionPolicyDisabled -> "PermissionPolicyDisabled"
         AuditsAttributionReportingIssueTypeAttributionSourceUntrustworthyOrigin -> "AttributionSourceUntrustworthyOrigin"
         AuditsAttributionReportingIssueTypeAttributionUntrustworthyOrigin -> "AttributionUntrustworthyOrigin"
         AuditsAttributionReportingIssueTypeInvalidHeader -> "InvalidHeader"



-- | Type 'Audits.AttributionReportingIssueDetails'.
--   Details for issues around "Attribution Reporting API" usage.
--   Explainer: https://github.com/WICG/conversion-measurement-api
data AuditsAttributionReportingIssueDetails = AuditsAttributionReportingIssueDetails {
  auditsAttributionReportingIssueDetailsViolationType :: AuditsAttributionReportingIssueType,
  auditsAttributionReportingIssueDetailsFrame :: Maybe AuditsAffectedFrame,
  auditsAttributionReportingIssueDetailsRequest :: Maybe AuditsAffectedRequest,
  auditsAttributionReportingIssueDetailsViolatingNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  auditsAttributionReportingIssueDetailsInvalidParameter :: Maybe Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAttributionReportingIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  AuditsAttributionReportingIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'Audits.QuirksModeIssueDetails'.
--   Details for issues about documents in Quirks Mode
--   or Limited Quirks Mode that affects page layouting.
data AuditsQuirksModeIssueDetails = AuditsQuirksModeIssueDetails {
  -- | If false, it means the document's mode is "quirks"
  --   instead of "limited-quirks".
  auditsQuirksModeIssueDetailsIsLimitedQuirksMode :: Bool,
  auditsQuirksModeIssueDetailsDocumentNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  auditsQuirksModeIssueDetailsUrl :: Text,
  auditsQuirksModeIssueDetailsFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
  auditsQuirksModeIssueDetailsLoaderId :: DOMPageNetworkEmulationSecurity.NetworkLoaderId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsQuirksModeIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  AuditsQuirksModeIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type 'Audits.NavigatorUserAgentIssueDetails'.
data AuditsNavigatorUserAgentIssueDetails = AuditsNavigatorUserAgentIssueDetails {
  auditsNavigatorUserAgentIssueDetailsUrl :: Text,
  auditsNavigatorUserAgentIssueDetailsLocation :: Maybe AuditsSourceCodeLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsNavigatorUserAgentIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  AuditsNavigatorUserAgentIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Audits.GenericIssueErrorType'.
data AuditsGenericIssueErrorType = AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsGenericIssueErrorType where
   parseJSON = A.withText  "AuditsGenericIssueErrorType"  $ \v -> do
      case v of
         "CrossOriginPortalPostMessageError" -> pure AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError
         _ -> fail "failed to parse AuditsGenericIssueErrorType"

instance ToJSON AuditsGenericIssueErrorType where
   toJSON v = A.String $
      case v of
         AuditsGenericIssueErrorTypeCrossOriginPortalPostMessageError -> "CrossOriginPortalPostMessageError"



-- | Type 'Audits.GenericIssueDetails'.
--   Depending on the concrete errorType, different properties are set.
data AuditsGenericIssueDetails = AuditsGenericIssueDetails {
  -- | Issues with the same errorType are aggregated in the frontend.
  auditsGenericIssueDetailsErrorType :: AuditsGenericIssueErrorType,
  auditsGenericIssueDetailsFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsGenericIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AuditsGenericIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'Audits.DeprecationIssueType'.
data AuditsDeprecationIssueType = AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard | AuditsDeprecationIssueTypeCanRequestUrlhttpContainingNewline | AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo | AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime | AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable | AuditsDeprecationIssueTypeCookieWithTruncatingChar | AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain | AuditsDeprecationIssueTypeCrossOriginWindowAlert | AuditsDeprecationIssueTypeCrossOriginWindowConfirm | AuditsDeprecationIssueTypeCssSelectorInternalMediaControlsOverlayCastButton | AuditsDeprecationIssueTypeDeprecationExample | AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader | AuditsDeprecationIssueTypeEventPath | AuditsDeprecationIssueTypeGeolocationInsecureOrigin | AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved | AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin | AuditsDeprecationIssueTypeHostCandidateAttributeGetter | AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest | AuditsDeprecationIssueTypeLegacyConstraintGoogIPv6 | AuditsDeprecationIssueTypeLocalCssFileExtensionRejected | AuditsDeprecationIssueTypeMediaSourceAbortRemove | AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered | AuditsDeprecationIssueTypeNoSysexWebMidiWithoutPermission | AuditsDeprecationIssueTypeNotificationInsecureOrigin | AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe | AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite | AuditsDeprecationIssueTypePaymentRequestBasicCard | AuditsDeprecationIssueTypePictureSourceSrc | AuditsDeprecationIssueTypePrefixedCancelAnimationFrame | AuditsDeprecationIssueTypePrefixedRequestAnimationFrame | AuditsDeprecationIssueTypePrefixedStorageInfo | AuditsDeprecationIssueTypePrefixedVideoDisplayingFullscreen | AuditsDeprecationIssueTypePrefixedVideoEnterFullscreen | AuditsDeprecationIssueTypePrefixedVideoEnterFullScreen | AuditsDeprecationIssueTypePrefixedVideoExitFullscreen | AuditsDeprecationIssueTypePrefixedVideoExitFullScreen | AuditsDeprecationIssueTypePrefixedVideoSupportsFullscreen | AuditsDeprecationIssueTypeRangeExpand | AuditsDeprecationIssueTypeRequestedSubresourceWithEmbeddedCredentials | AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpFalse | AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpTrue | AuditsDeprecationIssueTypeRtcPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics | AuditsDeprecationIssueTypeRtcPeerConnectionSdpSemanticsPlanB | AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate | AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation | AuditsDeprecationIssueTypeTextToSpeechDisallowedByAutoplay | AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation | AuditsDeprecationIssueTypeXhrjsonEncodingDetection | AuditsDeprecationIssueTypeXmlHttpRequestSynchronousInNonWorkerOutsideBeforeUnload | AuditsDeprecationIssueTypeXrSupportsSession
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsDeprecationIssueType where
   parseJSON = A.withText  "AuditsDeprecationIssueType"  $ \v -> do
      case v of
         "AuthorizationCoveredByWildcard" -> pure AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard
         "CanRequestURLHTTPContainingNewline" -> pure AuditsDeprecationIssueTypeCanRequestUrlhttpContainingNewline
         "ChromeLoadTimesConnectionInfo" -> pure AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo
         "ChromeLoadTimesFirstPaintAfterLoadTime" -> pure AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime
         "ChromeLoadTimesWasAlternateProtocolAvailable" -> pure AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable
         "CookieWithTruncatingChar" -> pure AuditsDeprecationIssueTypeCookieWithTruncatingChar
         "CrossOriginAccessBasedOnDocumentDomain" -> pure AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain
         "CrossOriginWindowAlert" -> pure AuditsDeprecationIssueTypeCrossOriginWindowAlert
         "CrossOriginWindowConfirm" -> pure AuditsDeprecationIssueTypeCrossOriginWindowConfirm
         "CSSSelectorInternalMediaControlsOverlayCastButton" -> pure AuditsDeprecationIssueTypeCssSelectorInternalMediaControlsOverlayCastButton
         "DeprecationExample" -> pure AuditsDeprecationIssueTypeDeprecationExample
         "DocumentDomainSettingWithoutOriginAgentClusterHeader" -> pure AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader
         "EventPath" -> pure AuditsDeprecationIssueTypeEventPath
         "GeolocationInsecureOrigin" -> pure AuditsDeprecationIssueTypeGeolocationInsecureOrigin
         "GeolocationInsecureOriginDeprecatedNotRemoved" -> pure AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved
         "GetUserMediaInsecureOrigin" -> pure AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin
         "HostCandidateAttributeGetter" -> pure AuditsDeprecationIssueTypeHostCandidateAttributeGetter
         "InsecurePrivateNetworkSubresourceRequest" -> pure AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest
         "LegacyConstraintGoogIPv6" -> pure AuditsDeprecationIssueTypeLegacyConstraintGoogIPv6
         "LocalCSSFileExtensionRejected" -> pure AuditsDeprecationIssueTypeLocalCssFileExtensionRejected
         "MediaSourceAbortRemove" -> pure AuditsDeprecationIssueTypeMediaSourceAbortRemove
         "MediaSourceDurationTruncatingBuffered" -> pure AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered
         "NoSysexWebMIDIWithoutPermission" -> pure AuditsDeprecationIssueTypeNoSysexWebMidiWithoutPermission
         "NotificationInsecureOrigin" -> pure AuditsDeprecationIssueTypeNotificationInsecureOrigin
         "NotificationPermissionRequestedIframe" -> pure AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe
         "ObsoleteWebRtcCipherSuite" -> pure AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite
         "PaymentRequestBasicCard" -> pure AuditsDeprecationIssueTypePaymentRequestBasicCard
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
         "RTCConstraintEnableDtlsSrtpFalse" -> pure AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpFalse
         "RTCConstraintEnableDtlsSrtpTrue" -> pure AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpTrue
         "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics" -> pure AuditsDeprecationIssueTypeRtcPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics
         "RTCPeerConnectionSdpSemanticsPlanB" -> pure AuditsDeprecationIssueTypeRtcPeerConnectionSdpSemanticsPlanB
         "RtcpMuxPolicyNegotiate" -> pure AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate
         "SharedArrayBufferConstructedWithoutIsolation" -> pure AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation
         "TextToSpeech_DisallowedByAutoplay" -> pure AuditsDeprecationIssueTypeTextToSpeechDisallowedByAutoplay
         "V8SharedArrayBufferConstructedInExtensionWithoutIsolation" -> pure AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation
         "XHRJSONEncodingDetection" -> pure AuditsDeprecationIssueTypeXhrjsonEncodingDetection
         "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload" -> pure AuditsDeprecationIssueTypeXmlHttpRequestSynchronousInNonWorkerOutsideBeforeUnload
         "XRSupportsSession" -> pure AuditsDeprecationIssueTypeXrSupportsSession
         _ -> fail "failed to parse AuditsDeprecationIssueType"

instance ToJSON AuditsDeprecationIssueType where
   toJSON v = A.String $
      case v of
         AuditsDeprecationIssueTypeAuthorizationCoveredByWildcard -> "AuthorizationCoveredByWildcard"
         AuditsDeprecationIssueTypeCanRequestUrlhttpContainingNewline -> "CanRequestURLHTTPContainingNewline"
         AuditsDeprecationIssueTypeChromeLoadTimesConnectionInfo -> "ChromeLoadTimesConnectionInfo"
         AuditsDeprecationIssueTypeChromeLoadTimesFirstPaintAfterLoadTime -> "ChromeLoadTimesFirstPaintAfterLoadTime"
         AuditsDeprecationIssueTypeChromeLoadTimesWasAlternateProtocolAvailable -> "ChromeLoadTimesWasAlternateProtocolAvailable"
         AuditsDeprecationIssueTypeCookieWithTruncatingChar -> "CookieWithTruncatingChar"
         AuditsDeprecationIssueTypeCrossOriginAccessBasedOnDocumentDomain -> "CrossOriginAccessBasedOnDocumentDomain"
         AuditsDeprecationIssueTypeCrossOriginWindowAlert -> "CrossOriginWindowAlert"
         AuditsDeprecationIssueTypeCrossOriginWindowConfirm -> "CrossOriginWindowConfirm"
         AuditsDeprecationIssueTypeCssSelectorInternalMediaControlsOverlayCastButton -> "CSSSelectorInternalMediaControlsOverlayCastButton"
         AuditsDeprecationIssueTypeDeprecationExample -> "DeprecationExample"
         AuditsDeprecationIssueTypeDocumentDomainSettingWithoutOriginAgentClusterHeader -> "DocumentDomainSettingWithoutOriginAgentClusterHeader"
         AuditsDeprecationIssueTypeEventPath -> "EventPath"
         AuditsDeprecationIssueTypeGeolocationInsecureOrigin -> "GeolocationInsecureOrigin"
         AuditsDeprecationIssueTypeGeolocationInsecureOriginDeprecatedNotRemoved -> "GeolocationInsecureOriginDeprecatedNotRemoved"
         AuditsDeprecationIssueTypeGetUserMediaInsecureOrigin -> "GetUserMediaInsecureOrigin"
         AuditsDeprecationIssueTypeHostCandidateAttributeGetter -> "HostCandidateAttributeGetter"
         AuditsDeprecationIssueTypeInsecurePrivateNetworkSubresourceRequest -> "InsecurePrivateNetworkSubresourceRequest"
         AuditsDeprecationIssueTypeLegacyConstraintGoogIPv6 -> "LegacyConstraintGoogIPv6"
         AuditsDeprecationIssueTypeLocalCssFileExtensionRejected -> "LocalCSSFileExtensionRejected"
         AuditsDeprecationIssueTypeMediaSourceAbortRemove -> "MediaSourceAbortRemove"
         AuditsDeprecationIssueTypeMediaSourceDurationTruncatingBuffered -> "MediaSourceDurationTruncatingBuffered"
         AuditsDeprecationIssueTypeNoSysexWebMidiWithoutPermission -> "NoSysexWebMIDIWithoutPermission"
         AuditsDeprecationIssueTypeNotificationInsecureOrigin -> "NotificationInsecureOrigin"
         AuditsDeprecationIssueTypeNotificationPermissionRequestedIframe -> "NotificationPermissionRequestedIframe"
         AuditsDeprecationIssueTypeObsoleteWebRtcCipherSuite -> "ObsoleteWebRtcCipherSuite"
         AuditsDeprecationIssueTypePaymentRequestBasicCard -> "PaymentRequestBasicCard"
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
         AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpFalse -> "RTCConstraintEnableDtlsSrtpFalse"
         AuditsDeprecationIssueTypeRtcConstraintEnableDtlsSrtpTrue -> "RTCConstraintEnableDtlsSrtpTrue"
         AuditsDeprecationIssueTypeRtcPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics -> "RTCPeerConnectionComplexPlanBSdpUsingDefaultSdpSemantics"
         AuditsDeprecationIssueTypeRtcPeerConnectionSdpSemanticsPlanB -> "RTCPeerConnectionSdpSemanticsPlanB"
         AuditsDeprecationIssueTypeRtcpMuxPolicyNegotiate -> "RtcpMuxPolicyNegotiate"
         AuditsDeprecationIssueTypeSharedArrayBufferConstructedWithoutIsolation -> "SharedArrayBufferConstructedWithoutIsolation"
         AuditsDeprecationIssueTypeTextToSpeechDisallowedByAutoplay -> "TextToSpeech_DisallowedByAutoplay"
         AuditsDeprecationIssueTypeV8SharedArrayBufferConstructedInExtensionWithoutIsolation -> "V8SharedArrayBufferConstructedInExtensionWithoutIsolation"
         AuditsDeprecationIssueTypeXhrjsonEncodingDetection -> "XHRJSONEncodingDetection"
         AuditsDeprecationIssueTypeXmlHttpRequestSynchronousInNonWorkerOutsideBeforeUnload -> "XMLHttpRequestSynchronousInNonWorkerOutsideBeforeUnload"
         AuditsDeprecationIssueTypeXrSupportsSession -> "XRSupportsSession"



-- | Type 'Audits.DeprecationIssueDetails'.
--   This issue tracks information needed to print a deprecation message.
--   https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/frame/third_party/blink/renderer/core/frame/deprecation/README.md
data AuditsDeprecationIssueDetails = AuditsDeprecationIssueDetails {
  auditsDeprecationIssueDetailsAffectedFrame :: Maybe AuditsAffectedFrame,
  auditsDeprecationIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
  auditsDeprecationIssueDetailsType :: AuditsDeprecationIssueType
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsDeprecationIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  AuditsDeprecationIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type 'Audits.ClientHintIssueReason'.
data AuditsClientHintIssueReason = AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin | AuditsClientHintIssueReasonMetaTagModifiedHtml
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsClientHintIssueReason where
   parseJSON = A.withText  "AuditsClientHintIssueReason"  $ \v -> do
      case v of
         "MetaTagAllowListInvalidOrigin" -> pure AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin
         "MetaTagModifiedHTML" -> pure AuditsClientHintIssueReasonMetaTagModifiedHtml
         _ -> fail "failed to parse AuditsClientHintIssueReason"

instance ToJSON AuditsClientHintIssueReason where
   toJSON v = A.String $
      case v of
         AuditsClientHintIssueReasonMetaTagAllowListInvalidOrigin -> "MetaTagAllowListInvalidOrigin"
         AuditsClientHintIssueReasonMetaTagModifiedHtml -> "MetaTagModifiedHTML"



-- | Type 'Audits.FederatedAuthRequestIssueDetails'.
data AuditsFederatedAuthRequestIssueDetails = AuditsFederatedAuthRequestIssueDetails {
  auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason :: AuditsFederatedAuthRequestIssueReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsFederatedAuthRequestIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  AuditsFederatedAuthRequestIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'Audits.FederatedAuthRequestIssueReason'.
--   Represents the failure reason when a federated authentication reason fails.
--   Should be updated alongside RequestIdTokenStatus in
--   third_party/blink/public/mojom/devtools/inspector_issue.mojom to include
--   all cases except for success.
data AuditsFederatedAuthRequestIssueReason = AuditsFederatedAuthRequestIssueReasonApprovalDeclined | AuditsFederatedAuthRequestIssueReasonTooManyRequests | AuditsFederatedAuthRequestIssueReasonManifestListHttpNotFound | AuditsFederatedAuthRequestIssueReasonManifestListNoResponse | AuditsFederatedAuthRequestIssueReasonManifestListInvalidResponse | AuditsFederatedAuthRequestIssueReasonManifestNotInManifestList | AuditsFederatedAuthRequestIssueReasonManifestListTooBig | AuditsFederatedAuthRequestIssueReasonManifestHttpNotFound | AuditsFederatedAuthRequestIssueReasonManifestNoResponse | AuditsFederatedAuthRequestIssueReasonManifestInvalidResponse | AuditsFederatedAuthRequestIssueReasonClientMetadataHttpNotFound | AuditsFederatedAuthRequestIssueReasonClientMetadataNoResponse | AuditsFederatedAuthRequestIssueReasonClientMetadataInvalidResponse | AuditsFederatedAuthRequestIssueReasonClientMetadataMissingPrivacyPolicyUrl | AuditsFederatedAuthRequestIssueReasonDisabledInSettings | AuditsFederatedAuthRequestIssueReasonErrorFetchingSignin | AuditsFederatedAuthRequestIssueReasonInvalidSigninResponse | AuditsFederatedAuthRequestIssueReasonAccountsHttpNotFound | AuditsFederatedAuthRequestIssueReasonAccountsNoResponse | AuditsFederatedAuthRequestIssueReasonAccountsInvalidResponse | AuditsFederatedAuthRequestIssueReasonIdTokenHttpNotFound | AuditsFederatedAuthRequestIssueReasonIdTokenNoResponse | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidResponse | AuditsFederatedAuthRequestIssueReasonIdTokenInvalidRequest | AuditsFederatedAuthRequestIssueReasonErrorIdToken | AuditsFederatedAuthRequestIssueReasonCanceled
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsFederatedAuthRequestIssueReason where
   parseJSON = A.withText  "AuditsFederatedAuthRequestIssueReason"  $ \v -> do
      case v of
         "ApprovalDeclined" -> pure AuditsFederatedAuthRequestIssueReasonApprovalDeclined
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
         "ClientMetadataMissingPrivacyPolicyUrl" -> pure AuditsFederatedAuthRequestIssueReasonClientMetadataMissingPrivacyPolicyUrl
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
         _ -> fail "failed to parse AuditsFederatedAuthRequestIssueReason"

instance ToJSON AuditsFederatedAuthRequestIssueReason where
   toJSON v = A.String $
      case v of
         AuditsFederatedAuthRequestIssueReasonApprovalDeclined -> "ApprovalDeclined"
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
         AuditsFederatedAuthRequestIssueReasonClientMetadataMissingPrivacyPolicyUrl -> "ClientMetadataMissingPrivacyPolicyUrl"
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



-- | Type 'Audits.ClientHintIssueDetails'.
--   This issue tracks client hints related issues. It's used to deprecate old
--   features, encourage the use of new ones, and provide general guidance.
data AuditsClientHintIssueDetails = AuditsClientHintIssueDetails {
  auditsClientHintIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
  auditsClientHintIssueDetailsClientHintIssueReason :: AuditsClientHintIssueReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsClientHintIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  AuditsClientHintIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type 'Audits.InspectorIssueCode'.
--   A unique identifier for the type of issue. Each type may use one of the
--   optional fields in InspectorIssueDetails to convey more specific
--   information about the kind of issue.
data AuditsInspectorIssueCode = AuditsInspectorIssueCodeCookieIssue | AuditsInspectorIssueCodeMixedContentIssue | AuditsInspectorIssueCodeBlockedByResponseIssue | AuditsInspectorIssueCodeHeavyAdIssue | AuditsInspectorIssueCodeContentSecurityPolicyIssue | AuditsInspectorIssueCodeSharedArrayBufferIssue | AuditsInspectorIssueCodeTrustedWebActivityIssue | AuditsInspectorIssueCodeLowTextContrastIssue | AuditsInspectorIssueCodeCorsIssue | AuditsInspectorIssueCodeAttributionReportingIssue | AuditsInspectorIssueCodeQuirksModeIssue | AuditsInspectorIssueCodeNavigatorUserAgentIssue | AuditsInspectorIssueCodeGenericIssue | AuditsInspectorIssueCodeDeprecationIssue | AuditsInspectorIssueCodeClientHintIssue | AuditsInspectorIssueCodeFederatedAuthRequestIssue
   deriving (Ord, Eq, Show, Read)
instance FromJSON AuditsInspectorIssueCode where
   parseJSON = A.withText  "AuditsInspectorIssueCode"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse AuditsInspectorIssueCode"

instance ToJSON AuditsInspectorIssueCode where
   toJSON v = A.String $
      case v of
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
data AuditsInspectorIssueDetails = AuditsInspectorIssueDetails {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsInspectorIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  AuditsInspectorIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'Audits.IssueId'.
--   A unique id for a DevTools inspector issue. Allows other entities (e.g.
--   exceptions, CDP message, console messages, etc.) to reference an issue.
type AuditsIssueId = Text

-- | Type 'Audits.InspectorIssue'.
--   An inspector issue reported from the back-end.
data AuditsInspectorIssue = AuditsInspectorIssue {
  auditsInspectorIssueCode :: AuditsInspectorIssueCode,
  auditsInspectorIssueDetails :: AuditsInspectorIssueDetails,
  -- | A unique id for this issue. May be omitted if no other entity (e.g.
  --   exception, CDP message, etc.) is referencing this issue.
  auditsInspectorIssueIssueId :: Maybe AuditsIssueId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsInspectorIssue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AuditsInspectorIssue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Type of the 'Audits.issueAdded' event.
data AuditsIssueAdded = AuditsIssueAdded {
  auditsIssueAddedIssue :: AuditsInspectorIssue
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsIssueAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  AuditsIssueAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





-- | Parameters of the 'auditsGetEncodedResponse' command.
data PAuditsGetEncodedResponseEncoding = PAuditsGetEncodedResponseEncodingWebp | PAuditsGetEncodedResponseEncodingJpeg | PAuditsGetEncodedResponseEncodingPng
   deriving (Ord, Eq, Show, Read)
instance FromJSON PAuditsGetEncodedResponseEncoding where
   parseJSON = A.withText  "PAuditsGetEncodedResponseEncoding"  $ \v -> do
      case v of
         "webp" -> pure PAuditsGetEncodedResponseEncodingWebp
         "jpeg" -> pure PAuditsGetEncodedResponseEncodingJpeg
         "png" -> pure PAuditsGetEncodedResponseEncodingPng
         _ -> fail "failed to parse PAuditsGetEncodedResponseEncoding"

instance ToJSON PAuditsGetEncodedResponseEncoding where
   toJSON v = A.String $
      case v of
         PAuditsGetEncodedResponseEncodingWebp -> "webp"
         PAuditsGetEncodedResponseEncodingJpeg -> "jpeg"
         PAuditsGetEncodedResponseEncodingPng -> "png"



data PAuditsGetEncodedResponse = PAuditsGetEncodedResponse {
  -- | Identifier of the network request to get content for.
  pAuditsGetEncodedResponseRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
  -- | The encoding to use.
  pAuditsGetEncodedResponseEncoding :: PAuditsGetEncodedResponseEncoding,
  -- | The quality of the encoding (0-1). (defaults to 1)
  pAuditsGetEncodedResponseQuality :: Maybe Double,
  -- | Whether to only return the size information (defaults to false).
  pAuditsGetEncodedResponseSizeOnly :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAuditsGetEncodedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAuditsGetEncodedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Audits.getEncodedResponse' command.
--   Returns the response body and size if it were re-encoded with the specified settings. Only
--   applies to images.
--   Parameters: 'PAuditsGetEncodedResponse'
--   Returns: 'AuditsGetEncodedResponse'
auditsGetEncodedResponse :: Handle ev -> PAuditsGetEncodedResponse -> IO AuditsGetEncodedResponse
auditsGetEncodedResponse handle params = sendReceiveCommandResult handle "Audits.getEncodedResponse" (Just params)

-- | Return type of the 'auditsGetEncodedResponse' command.
data AuditsGetEncodedResponse = AuditsGetEncodedResponse {
  -- | The encoded body as a base64 string. Omitted if sizeOnly is true. (Encoded as a base64 string when passed over JSON)
  auditsGetEncodedResponseBody :: Maybe Text,
  -- | Size before re-encoding.
  auditsGetEncodedResponseOriginalSize :: Int,
  -- | Size after re-encoding.
  auditsGetEncodedResponseEncodedSize :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AuditsGetEncodedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AuditsGetEncodedResponse where
   commandName _ = "Audits.getEncodedResponse"



-- | Function for the 'Audits.disable' command.
--   Disables issues domain, prevents further issues from being reported to the client.
auditsDisable :: Handle ev -> IO ()
auditsDisable handle = sendReceiveCommand handle "Audits.disable" (Nothing :: Maybe ())


-- | Function for the 'Audits.enable' command.
--   Enables issues domain, sends the issues collected so far to the client by means of the
--   `issueAdded` event.
auditsEnable :: Handle ev -> IO ()
auditsEnable handle = sendReceiveCommand handle "Audits.enable" (Nothing :: Maybe ())


-- | Parameters of the 'auditsCheckContrast' command.
data PAuditsCheckContrast = PAuditsCheckContrast {
  -- | Whether to report WCAG AAA level issues. Default is false.
  pAuditsCheckContrastReportAaa :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAuditsCheckContrast  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PAuditsCheckContrast where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Audits.checkContrast' command.
--   Runs the contrast check for the target page. Found issues are reported
--   using Audits.issueAdded event.
--   Parameters: 'PAuditsCheckContrast'
auditsCheckContrast :: Handle ev -> PAuditsCheckContrast -> IO ()
auditsCheckContrast handle params = sendReceiveCommand handle "Audits.checkContrast" (Just params)



