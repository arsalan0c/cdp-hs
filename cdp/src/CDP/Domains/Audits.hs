{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



data AuditsAffectedCookie = AuditsAffectedCookie {
   auditsAffectedCookieName :: String,
   auditsAffectedCookiePath :: String,
   auditsAffectedCookieDomain :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data AuditsAffectedRequest = AuditsAffectedRequest {
   auditsAffectedRequestRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
   auditsAffectedRequestUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data AuditsAffectedFrame = AuditsAffectedFrame {
   auditsAffectedFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAffectedFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  AuditsAffectedFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


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



data AuditsCookieIssueDetails = AuditsCookieIssueDetails {
   auditsCookieIssueDetailsCookie :: Maybe AuditsAffectedCookie,
   auditsCookieIssueDetailsRawCookieLine :: Maybe String,
   auditsCookieIssueDetailsCookieWarningReasons :: [AuditsCookieWarningReason],
   auditsCookieIssueDetailsCookieExclusionReasons :: [AuditsCookieExclusionReason],
   auditsCookieIssueDetailsOperation :: AuditsCookieOperation,
   auditsCookieIssueDetailsSiteForCookies :: Maybe String,
   auditsCookieIssueDetailsCookieUrl :: Maybe String,
   auditsCookieIssueDetailsRequest :: Maybe AuditsAffectedRequest
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsCookieIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AuditsCookieIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


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



data AuditsMixedContentIssueDetails = AuditsMixedContentIssueDetails {
   auditsMixedContentIssueDetailsResourceType :: Maybe AuditsMixedContentResourceType,
   auditsMixedContentIssueDetailsResolutionStatus :: AuditsMixedContentResolutionStatus,
   auditsMixedContentIssueDetailsInsecureUrl :: String,
   auditsMixedContentIssueDetailsMainResourceUrl :: String,
   auditsMixedContentIssueDetailsRequest :: Maybe AuditsAffectedRequest,
   auditsMixedContentIssueDetailsFrame :: Maybe AuditsAffectedFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsMixedContentIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  AuditsMixedContentIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


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



data AuditsHeavyAdIssueDetails = AuditsHeavyAdIssueDetails {
   auditsHeavyAdIssueDetailsResolution :: AuditsHeavyAdResolutionStatus,
   auditsHeavyAdIssueDetailsReason :: AuditsHeavyAdReason,
   auditsHeavyAdIssueDetailsFrame :: AuditsAffectedFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsHeavyAdIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AuditsHeavyAdIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


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



data AuditsSourceCodeLocation = AuditsSourceCodeLocation {
   auditsSourceCodeLocationScriptId :: Maybe Runtime.RuntimeScriptId,
   auditsSourceCodeLocationUrl :: String,
   auditsSourceCodeLocationLineNumber :: Int,
   auditsSourceCodeLocationColumnNumber :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsSourceCodeLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  AuditsSourceCodeLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data AuditsContentSecurityPolicyIssueDetails = AuditsContentSecurityPolicyIssueDetails {
   auditsContentSecurityPolicyIssueDetailsBlockedUrl :: Maybe String,
   auditsContentSecurityPolicyIssueDetailsViolatedDirective :: String,
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



data AuditsSharedArrayBufferIssueDetails = AuditsSharedArrayBufferIssueDetails {
   auditsSharedArrayBufferIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
   auditsSharedArrayBufferIssueDetailsIsWarning :: Bool,
   auditsSharedArrayBufferIssueDetailsType :: AuditsSharedArrayBufferIssueType
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsSharedArrayBufferIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  AuditsSharedArrayBufferIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


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



data AuditsTrustedWebActivityIssueDetails = AuditsTrustedWebActivityIssueDetails {
   auditsTrustedWebActivityIssueDetailsUrl :: String,
   auditsTrustedWebActivityIssueDetailsViolationType :: AuditsTwaQualityEnforcementViolationType,
   auditsTrustedWebActivityIssueDetailsHttpStatusCode :: Maybe Int,
   auditsTrustedWebActivityIssueDetailsPackageName :: Maybe String,
   auditsTrustedWebActivityIssueDetailsSignature :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsTrustedWebActivityIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  AuditsTrustedWebActivityIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



data AuditsLowTextContrastIssueDetails = AuditsLowTextContrastIssueDetails {
   auditsLowTextContrastIssueDetailsViolatingNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   auditsLowTextContrastIssueDetailsViolatingNodeSelector :: String,
   auditsLowTextContrastIssueDetailsContrastRatio :: Double,
   auditsLowTextContrastIssueDetailsThresholdAa :: Double,
   auditsLowTextContrastIssueDetailsThresholdAaa :: Double,
   auditsLowTextContrastIssueDetailsFontSize :: String,
   auditsLowTextContrastIssueDetailsFontWeight :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsLowTextContrastIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  AuditsLowTextContrastIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data AuditsCorsIssueDetails = AuditsCorsIssueDetails {
   auditsCorsIssueDetailsCorsErrorStatus :: DOMPageNetworkEmulationSecurity.NetworkCorsErrorStatus,
   auditsCorsIssueDetailsIsWarning :: Bool,
   auditsCorsIssueDetailsRequest :: AuditsAffectedRequest,
   auditsCorsIssueDetailsLocation :: Maybe AuditsSourceCodeLocation,
   auditsCorsIssueDetailsInitiatorOrigin :: Maybe String,
   auditsCorsIssueDetailsResourceIpAddressSpace :: Maybe DOMPageNetworkEmulationSecurity.NetworkIpAddressSpace,
   auditsCorsIssueDetailsClientSecurityState :: Maybe DOMPageNetworkEmulationSecurity.NetworkClientSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsCorsIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  AuditsCorsIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


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



data AuditsAttributionReportingIssueDetails = AuditsAttributionReportingIssueDetails {
   auditsAttributionReportingIssueDetailsViolationType :: AuditsAttributionReportingIssueType,
   auditsAttributionReportingIssueDetailsFrame :: Maybe AuditsAffectedFrame,
   auditsAttributionReportingIssueDetailsRequest :: Maybe AuditsAffectedRequest,
   auditsAttributionReportingIssueDetailsViolatingNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   auditsAttributionReportingIssueDetailsInvalidParameter :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsAttributionReportingIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  AuditsAttributionReportingIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



data AuditsQuirksModeIssueDetails = AuditsQuirksModeIssueDetails {
   auditsQuirksModeIssueDetailsIsLimitedQuirksMode :: Bool,
   auditsQuirksModeIssueDetailsDocumentNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   auditsQuirksModeIssueDetailsUrl :: String,
   auditsQuirksModeIssueDetailsFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   auditsQuirksModeIssueDetailsLoaderId :: DOMPageNetworkEmulationSecurity.NetworkLoaderId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsQuirksModeIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  AuditsQuirksModeIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data AuditsNavigatorUserAgentIssueDetails = AuditsNavigatorUserAgentIssueDetails {
   auditsNavigatorUserAgentIssueDetailsUrl :: String,
   auditsNavigatorUserAgentIssueDetailsLocation :: Maybe AuditsSourceCodeLocation
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsNavigatorUserAgentIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  AuditsNavigatorUserAgentIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


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



data AuditsGenericIssueDetails = AuditsGenericIssueDetails {
   auditsGenericIssueDetailsErrorType :: AuditsGenericIssueErrorType,
   auditsGenericIssueDetailsFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsGenericIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AuditsGenericIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


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



data AuditsDeprecationIssueDetails = AuditsDeprecationIssueDetails {
   auditsDeprecationIssueDetailsAffectedFrame :: Maybe AuditsAffectedFrame,
   auditsDeprecationIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
   auditsDeprecationIssueDetailsType :: AuditsDeprecationIssueType
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsDeprecationIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  AuditsDeprecationIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


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



data AuditsFederatedAuthRequestIssueDetails = AuditsFederatedAuthRequestIssueDetails {
   auditsFederatedAuthRequestIssueDetailsFederatedAuthRequestIssueReason :: AuditsFederatedAuthRequestIssueReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsFederatedAuthRequestIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  AuditsFederatedAuthRequestIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


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



data AuditsClientHintIssueDetails = AuditsClientHintIssueDetails {
   auditsClientHintIssueDetailsSourceCodeLocation :: AuditsSourceCodeLocation,
   auditsClientHintIssueDetailsClientHintIssueReason :: AuditsClientHintIssueReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsClientHintIssueDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  AuditsClientHintIssueDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


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


type AuditsIssueId = String

data AuditsInspectorIssue = AuditsInspectorIssue {
   auditsInspectorIssueCode :: AuditsInspectorIssueCode,
   auditsInspectorIssueDetails :: AuditsInspectorIssueDetails,
   auditsInspectorIssueIssueId :: Maybe AuditsIssueId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsInspectorIssue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AuditsInspectorIssue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





data AuditsIssueAdded = AuditsIssueAdded {
   auditsIssueAddedIssue :: AuditsInspectorIssue
} deriving (Generic, Eq, Show, Read)
instance ToJSON AuditsIssueAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  AuditsIssueAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }




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
   pAuditsGetEncodedResponseRequestId :: DOMPageNetworkEmulationSecurity.NetworkRequestId,
   pAuditsGetEncodedResponseEncoding :: PAuditsGetEncodedResponseEncoding,
   pAuditsGetEncodedResponseQuality :: Maybe Double,
   pAuditsGetEncodedResponseSizeOnly :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAuditsGetEncodedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAuditsGetEncodedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


auditsGetEncodedResponse :: Handle ev -> PAuditsGetEncodedResponse -> IO (Either Error AuditsGetEncodedResponse)
auditsGetEncodedResponse handle params = sendReceiveCommandResult handle "Audits.getEncodedResponse" (Just params)

data AuditsGetEncodedResponse = AuditsGetEncodedResponse {
   auditsGetEncodedResponseBody :: Maybe String,
   auditsGetEncodedResponseOriginalSize :: Int,
   auditsGetEncodedResponseEncodedSize :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AuditsGetEncodedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AuditsGetEncodedResponse where
   commandName _ = "Audits.getEncodedResponse"



auditsDisable :: Handle ev -> IO (Maybe Error)
auditsDisable handle = sendReceiveCommand handle "Audits.disable" (Nothing :: Maybe ())


auditsEnable :: Handle ev -> IO (Maybe Error)
auditsEnable handle = sendReceiveCommand handle "Audits.enable" (Nothing :: Maybe ())



data PAuditsCheckContrast = PAuditsCheckContrast {
   pAuditsCheckContrastReportAaa :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAuditsCheckContrast  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PAuditsCheckContrast where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


auditsCheckContrast :: Handle ev -> PAuditsCheckContrast -> IO (Maybe Error)
auditsCheckContrast handle params = sendReceiveCommand handle "Audits.checkContrast" (Just params)



