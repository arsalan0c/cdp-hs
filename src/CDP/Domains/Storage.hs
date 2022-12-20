{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Storage

-}


module CDP.Domains.Storage (module CDP.Domains.Storage) where

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


import CDP.Domains.BrowserTarget as BrowserTarget
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'Storage.SerializedStorageKey'.
type StorageSerializedStorageKey = T.Text

-- | Type 'Storage.StorageType'.
--   Enum of possible storage types.
data StorageStorageType = StorageStorageTypeAppcache | StorageStorageTypeCookies | StorageStorageTypeFile_systems | StorageStorageTypeIndexeddb | StorageStorageTypeLocal_storage | StorageStorageTypeShader_cache | StorageStorageTypeWebsql | StorageStorageTypeService_workers | StorageStorageTypeCache_storage | StorageStorageTypeInterest_groups | StorageStorageTypeAll | StorageStorageTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageStorageType where
  parseJSON = A.withText "StorageStorageType" $ \v -> case v of
    "appcache" -> pure StorageStorageTypeAppcache
    "cookies" -> pure StorageStorageTypeCookies
    "file_systems" -> pure StorageStorageTypeFile_systems
    "indexeddb" -> pure StorageStorageTypeIndexeddb
    "local_storage" -> pure StorageStorageTypeLocal_storage
    "shader_cache" -> pure StorageStorageTypeShader_cache
    "websql" -> pure StorageStorageTypeWebsql
    "service_workers" -> pure StorageStorageTypeService_workers
    "cache_storage" -> pure StorageStorageTypeCache_storage
    "interest_groups" -> pure StorageStorageTypeInterest_groups
    "all" -> pure StorageStorageTypeAll
    "other" -> pure StorageStorageTypeOther
    "_" -> fail "failed to parse StorageStorageType"
instance ToJSON StorageStorageType where
  toJSON v = A.String $ case v of
    StorageStorageTypeAppcache -> "appcache"
    StorageStorageTypeCookies -> "cookies"
    StorageStorageTypeFile_systems -> "file_systems"
    StorageStorageTypeIndexeddb -> "indexeddb"
    StorageStorageTypeLocal_storage -> "local_storage"
    StorageStorageTypeShader_cache -> "shader_cache"
    StorageStorageTypeWebsql -> "websql"
    StorageStorageTypeService_workers -> "service_workers"
    StorageStorageTypeCache_storage -> "cache_storage"
    StorageStorageTypeInterest_groups -> "interest_groups"
    StorageStorageTypeAll -> "all"
    StorageStorageTypeOther -> "other"

-- | Type 'Storage.UsageForType'.
--   Usage for a storage type.
data StorageUsageForType = StorageUsageForType
  {
    -- | Name of storage type.
    storageUsageForTypeStorageType :: StorageStorageType,
    -- | Storage usage (bytes).
    storageUsageForTypeUsage :: Double
  }
  deriving (Eq, Show)
instance FromJSON StorageUsageForType where
  parseJSON = A.withObject "StorageUsageForType" $ \o -> StorageUsageForType
    <$> o A..: "storageType"
    <*> o A..: "usage"
instance ToJSON StorageUsageForType where
  toJSON p = A.object $ catMaybes [
    ("storageType" A..=) <$> Just (storageUsageForTypeStorageType p),
    ("usage" A..=) <$> Just (storageUsageForTypeUsage p)
    ]

-- | Type 'Storage.TrustTokens'.
--   Pair of issuer origin and number of available (signed, but not used) Trust
--   Tokens from that issuer.
data StorageTrustTokens = StorageTrustTokens
  {
    storageTrustTokensIssuerOrigin :: T.Text,
    storageTrustTokensCount :: Double
  }
  deriving (Eq, Show)
instance FromJSON StorageTrustTokens where
  parseJSON = A.withObject "StorageTrustTokens" $ \o -> StorageTrustTokens
    <$> o A..: "issuerOrigin"
    <*> o A..: "count"
instance ToJSON StorageTrustTokens where
  toJSON p = A.object $ catMaybes [
    ("issuerOrigin" A..=) <$> Just (storageTrustTokensIssuerOrigin p),
    ("count" A..=) <$> Just (storageTrustTokensCount p)
    ]

-- | Type 'Storage.InterestGroupAccessType'.
--   Enum of interest group access types.
data StorageInterestGroupAccessType = StorageInterestGroupAccessTypeJoin | StorageInterestGroupAccessTypeLeave | StorageInterestGroupAccessTypeUpdate | StorageInterestGroupAccessTypeBid | StorageInterestGroupAccessTypeWin
  deriving (Ord, Eq, Show, Read)
instance FromJSON StorageInterestGroupAccessType where
  parseJSON = A.withText "StorageInterestGroupAccessType" $ \v -> case v of
    "join" -> pure StorageInterestGroupAccessTypeJoin
    "leave" -> pure StorageInterestGroupAccessTypeLeave
    "update" -> pure StorageInterestGroupAccessTypeUpdate
    "bid" -> pure StorageInterestGroupAccessTypeBid
    "win" -> pure StorageInterestGroupAccessTypeWin
    "_" -> fail "failed to parse StorageInterestGroupAccessType"
instance ToJSON StorageInterestGroupAccessType where
  toJSON v = A.String $ case v of
    StorageInterestGroupAccessTypeJoin -> "join"
    StorageInterestGroupAccessTypeLeave -> "leave"
    StorageInterestGroupAccessTypeUpdate -> "update"
    StorageInterestGroupAccessTypeBid -> "bid"
    StorageInterestGroupAccessTypeWin -> "win"

-- | Type 'Storage.InterestGroupAd'.
--   Ad advertising element inside an interest group.
data StorageInterestGroupAd = StorageInterestGroupAd
  {
    storageInterestGroupAdRenderUrl :: T.Text,
    storageInterestGroupAdMetadata :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageInterestGroupAd where
  parseJSON = A.withObject "StorageInterestGroupAd" $ \o -> StorageInterestGroupAd
    <$> o A..: "renderUrl"
    <*> o A..:? "metadata"
instance ToJSON StorageInterestGroupAd where
  toJSON p = A.object $ catMaybes [
    ("renderUrl" A..=) <$> Just (storageInterestGroupAdRenderUrl p),
    ("metadata" A..=) <$> (storageInterestGroupAdMetadata p)
    ]

-- | Type 'Storage.InterestGroupDetails'.
--   The full details of an interest group.
data StorageInterestGroupDetails = StorageInterestGroupDetails
  {
    storageInterestGroupDetailsOwnerOrigin :: T.Text,
    storageInterestGroupDetailsName :: T.Text,
    storageInterestGroupDetailsExpirationTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    storageInterestGroupDetailsJoiningOrigin :: T.Text,
    storageInterestGroupDetailsBiddingUrl :: Maybe T.Text,
    storageInterestGroupDetailsBiddingWasmHelperUrl :: Maybe T.Text,
    storageInterestGroupDetailsUpdateUrl :: Maybe T.Text,
    storageInterestGroupDetailsTrustedBiddingSignalsUrl :: Maybe T.Text,
    storageInterestGroupDetailsTrustedBiddingSignalsKeys :: [T.Text],
    storageInterestGroupDetailsUserBiddingSignals :: Maybe T.Text,
    storageInterestGroupDetailsAds :: [StorageInterestGroupAd],
    storageInterestGroupDetailsAdComponents :: [StorageInterestGroupAd]
  }
  deriving (Eq, Show)
instance FromJSON StorageInterestGroupDetails where
  parseJSON = A.withObject "StorageInterestGroupDetails" $ \o -> StorageInterestGroupDetails
    <$> o A..: "ownerOrigin"
    <*> o A..: "name"
    <*> o A..: "expirationTime"
    <*> o A..: "joiningOrigin"
    <*> o A..:? "biddingUrl"
    <*> o A..:? "biddingWasmHelperUrl"
    <*> o A..:? "updateUrl"
    <*> o A..:? "trustedBiddingSignalsUrl"
    <*> o A..: "trustedBiddingSignalsKeys"
    <*> o A..:? "userBiddingSignals"
    <*> o A..: "ads"
    <*> o A..: "adComponents"
instance ToJSON StorageInterestGroupDetails where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (storageInterestGroupDetailsOwnerOrigin p),
    ("name" A..=) <$> Just (storageInterestGroupDetailsName p),
    ("expirationTime" A..=) <$> Just (storageInterestGroupDetailsExpirationTime p),
    ("joiningOrigin" A..=) <$> Just (storageInterestGroupDetailsJoiningOrigin p),
    ("biddingUrl" A..=) <$> (storageInterestGroupDetailsBiddingUrl p),
    ("biddingWasmHelperUrl" A..=) <$> (storageInterestGroupDetailsBiddingWasmHelperUrl p),
    ("updateUrl" A..=) <$> (storageInterestGroupDetailsUpdateUrl p),
    ("trustedBiddingSignalsUrl" A..=) <$> (storageInterestGroupDetailsTrustedBiddingSignalsUrl p),
    ("trustedBiddingSignalsKeys" A..=) <$> Just (storageInterestGroupDetailsTrustedBiddingSignalsKeys p),
    ("userBiddingSignals" A..=) <$> (storageInterestGroupDetailsUserBiddingSignals p),
    ("ads" A..=) <$> Just (storageInterestGroupDetailsAds p),
    ("adComponents" A..=) <$> Just (storageInterestGroupDetailsAdComponents p)
    ]

-- | Type of the 'Storage.cacheStorageContentUpdated' event.
data StorageCacheStorageContentUpdated = StorageCacheStorageContentUpdated
  {
    -- | Origin to update.
    storageCacheStorageContentUpdatedOrigin :: T.Text,
    -- | Name of cache in origin.
    storageCacheStorageContentUpdatedCacheName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageCacheStorageContentUpdated where
  parseJSON = A.withObject "StorageCacheStorageContentUpdated" $ \o -> StorageCacheStorageContentUpdated
    <$> o A..: "origin"
    <*> o A..: "cacheName"
instance Event StorageCacheStorageContentUpdated where
  eventName _ = "Storage.cacheStorageContentUpdated"

-- | Type of the 'Storage.cacheStorageListUpdated' event.
data StorageCacheStorageListUpdated = StorageCacheStorageListUpdated
  {
    -- | Origin to update.
    storageCacheStorageListUpdatedOrigin :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageCacheStorageListUpdated where
  parseJSON = A.withObject "StorageCacheStorageListUpdated" $ \o -> StorageCacheStorageListUpdated
    <$> o A..: "origin"
instance Event StorageCacheStorageListUpdated where
  eventName _ = "Storage.cacheStorageListUpdated"

-- | Type of the 'Storage.indexedDBContentUpdated' event.
data StorageIndexedDBContentUpdated = StorageIndexedDBContentUpdated
  {
    -- | Origin to update.
    storageIndexedDBContentUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageIndexedDBContentUpdatedStorageKey :: T.Text,
    -- | Database to update.
    storageIndexedDBContentUpdatedDatabaseName :: T.Text,
    -- | ObjectStore to update.
    storageIndexedDBContentUpdatedObjectStoreName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageIndexedDBContentUpdated where
  parseJSON = A.withObject "StorageIndexedDBContentUpdated" $ \o -> StorageIndexedDBContentUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
    <*> o A..: "databaseName"
    <*> o A..: "objectStoreName"
instance Event StorageIndexedDBContentUpdated where
  eventName _ = "Storage.indexedDBContentUpdated"

-- | Type of the 'Storage.indexedDBListUpdated' event.
data StorageIndexedDBListUpdated = StorageIndexedDBListUpdated
  {
    -- | Origin to update.
    storageIndexedDBListUpdatedOrigin :: T.Text,
    -- | Storage key to update.
    storageIndexedDBListUpdatedStorageKey :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageIndexedDBListUpdated where
  parseJSON = A.withObject "StorageIndexedDBListUpdated" $ \o -> StorageIndexedDBListUpdated
    <$> o A..: "origin"
    <*> o A..: "storageKey"
instance Event StorageIndexedDBListUpdated where
  eventName _ = "Storage.indexedDBListUpdated"

-- | Type of the 'Storage.interestGroupAccessed' event.
data StorageInterestGroupAccessed = StorageInterestGroupAccessed
  {
    storageInterestGroupAccessedAccessTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
    storageInterestGroupAccessedType :: StorageInterestGroupAccessType,
    storageInterestGroupAccessedOwnerOrigin :: T.Text,
    storageInterestGroupAccessedName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON StorageInterestGroupAccessed where
  parseJSON = A.withObject "StorageInterestGroupAccessed" $ \o -> StorageInterestGroupAccessed
    <$> o A..: "accessTime"
    <*> o A..: "type"
    <*> o A..: "ownerOrigin"
    <*> o A..: "name"
instance Event StorageInterestGroupAccessed where
  eventName _ = "Storage.interestGroupAccessed"

-- | Returns a storage key given a frame id.

-- | Parameters of the 'Storage.getStorageKeyForFrame' command.
data PStorageGetStorageKeyForFrame = PStorageGetStorageKeyForFrame
  {
    pStorageGetStorageKeyForFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
pStorageGetStorageKeyForFrame
  :: DOMPageNetworkEmulationSecurity.PageFrameId
  -> PStorageGetStorageKeyForFrame
pStorageGetStorageKeyForFrame
  arg_pStorageGetStorageKeyForFrameFrameId
  = PStorageGetStorageKeyForFrame
    arg_pStorageGetStorageKeyForFrameFrameId
instance ToJSON PStorageGetStorageKeyForFrame where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pStorageGetStorageKeyForFrameFrameId p)
    ]
data StorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame
  {
    storageGetStorageKeyForFrameStorageKey :: StorageSerializedStorageKey
  }
  deriving (Eq, Show)
instance FromJSON StorageGetStorageKeyForFrame where
  parseJSON = A.withObject "StorageGetStorageKeyForFrame" $ \o -> StorageGetStorageKeyForFrame
    <$> o A..: "storageKey"
instance Command PStorageGetStorageKeyForFrame where
  type CommandResponse PStorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame
  commandName _ = "Storage.getStorageKeyForFrame"

-- | Clears storage for origin.

-- | Parameters of the 'Storage.clearDataForOrigin' command.
data PStorageClearDataForOrigin = PStorageClearDataForOrigin
  {
    -- | Security origin.
    pStorageClearDataForOriginOrigin :: T.Text,
    -- | Comma separated list of StorageType to clear.
    pStorageClearDataForOriginStorageTypes :: T.Text
  }
  deriving (Eq, Show)
pStorageClearDataForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  {-
  -- | Comma separated list of StorageType to clear.
  -}
  -> T.Text
  -> PStorageClearDataForOrigin
pStorageClearDataForOrigin
  arg_pStorageClearDataForOriginOrigin
  arg_pStorageClearDataForOriginStorageTypes
  = PStorageClearDataForOrigin
    arg_pStorageClearDataForOriginOrigin
    arg_pStorageClearDataForOriginStorageTypes
instance ToJSON PStorageClearDataForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageClearDataForOriginOrigin p),
    ("storageTypes" A..=) <$> Just (pStorageClearDataForOriginStorageTypes p)
    ]
instance Command PStorageClearDataForOrigin where
  type CommandResponse PStorageClearDataForOrigin = ()
  commandName _ = "Storage.clearDataForOrigin"
  fromJSON = const . A.Success . const ()

-- | Clears storage for storage key.

-- | Parameters of the 'Storage.clearDataForStorageKey' command.
data PStorageClearDataForStorageKey = PStorageClearDataForStorageKey
  {
    -- | Storage key.
    pStorageClearDataForStorageKeyStorageKey :: T.Text,
    -- | Comma separated list of StorageType to clear.
    pStorageClearDataForStorageKeyStorageTypes :: T.Text
  }
  deriving (Eq, Show)
pStorageClearDataForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  {-
  -- | Comma separated list of StorageType to clear.
  -}
  -> T.Text
  -> PStorageClearDataForStorageKey
pStorageClearDataForStorageKey
  arg_pStorageClearDataForStorageKeyStorageKey
  arg_pStorageClearDataForStorageKeyStorageTypes
  = PStorageClearDataForStorageKey
    arg_pStorageClearDataForStorageKeyStorageKey
    arg_pStorageClearDataForStorageKeyStorageTypes
instance ToJSON PStorageClearDataForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageClearDataForStorageKeyStorageKey p),
    ("storageTypes" A..=) <$> Just (pStorageClearDataForStorageKeyStorageTypes p)
    ]
instance Command PStorageClearDataForStorageKey where
  type CommandResponse PStorageClearDataForStorageKey = ()
  commandName _ = "Storage.clearDataForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Returns all browser cookies.

-- | Parameters of the 'Storage.getCookies' command.
data PStorageGetCookies = PStorageGetCookies
  {
    -- | Browser context to use when called on the browser endpoint.
    pStorageGetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageGetCookies
  :: PStorageGetCookies
pStorageGetCookies
  = PStorageGetCookies
    Nothing
instance ToJSON PStorageGetCookies where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> (pStorageGetCookiesBrowserContextId p)
    ]
data StorageGetCookies = StorageGetCookies
  {
    -- | Array of cookie objects.
    storageGetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookie]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetCookies where
  parseJSON = A.withObject "StorageGetCookies" $ \o -> StorageGetCookies
    <$> o A..: "cookies"
instance Command PStorageGetCookies where
  type CommandResponse PStorageGetCookies = StorageGetCookies
  commandName _ = "Storage.getCookies"

-- | Sets given cookies.

-- | Parameters of the 'Storage.setCookies' command.
data PStorageSetCookies = PStorageSetCookies
  {
    -- | Cookies to be set.
    pStorageSetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookieParam],
    -- | Browser context to use when called on the browser endpoint.
    pStorageSetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageSetCookies
  {-
  -- | Cookies to be set.
  -}
  :: [DOMPageNetworkEmulationSecurity.NetworkCookieParam]
  -> PStorageSetCookies
pStorageSetCookies
  arg_pStorageSetCookiesCookies
  = PStorageSetCookies
    arg_pStorageSetCookiesCookies
    Nothing
instance ToJSON PStorageSetCookies where
  toJSON p = A.object $ catMaybes [
    ("cookies" A..=) <$> Just (pStorageSetCookiesCookies p),
    ("browserContextId" A..=) <$> (pStorageSetCookiesBrowserContextId p)
    ]
instance Command PStorageSetCookies where
  type CommandResponse PStorageSetCookies = ()
  commandName _ = "Storage.setCookies"
  fromJSON = const . A.Success . const ()

-- | Clears cookies.

-- | Parameters of the 'Storage.clearCookies' command.
data PStorageClearCookies = PStorageClearCookies
  {
    -- | Browser context to use when called on the browser endpoint.
    pStorageClearCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
  }
  deriving (Eq, Show)
pStorageClearCookies
  :: PStorageClearCookies
pStorageClearCookies
  = PStorageClearCookies
    Nothing
instance ToJSON PStorageClearCookies where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> (pStorageClearCookiesBrowserContextId p)
    ]
instance Command PStorageClearCookies where
  type CommandResponse PStorageClearCookies = ()
  commandName _ = "Storage.clearCookies"
  fromJSON = const . A.Success . const ()

-- | Returns usage and quota in bytes.

-- | Parameters of the 'Storage.getUsageAndQuota' command.
data PStorageGetUsageAndQuota = PStorageGetUsageAndQuota
  {
    -- | Security origin.
    pStorageGetUsageAndQuotaOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageGetUsageAndQuota
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageGetUsageAndQuota
pStorageGetUsageAndQuota
  arg_pStorageGetUsageAndQuotaOrigin
  = PStorageGetUsageAndQuota
    arg_pStorageGetUsageAndQuotaOrigin
instance ToJSON PStorageGetUsageAndQuota where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageGetUsageAndQuotaOrigin p)
    ]
data StorageGetUsageAndQuota = StorageGetUsageAndQuota
  {
    -- | Storage usage (bytes).
    storageGetUsageAndQuotaUsage :: Double,
    -- | Storage quota (bytes).
    storageGetUsageAndQuotaQuota :: Double,
    -- | Whether or not the origin has an active storage quota override
    storageGetUsageAndQuotaOverrideActive :: Bool,
    -- | Storage usage per type (bytes).
    storageGetUsageAndQuotaUsageBreakdown :: [StorageUsageForType]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetUsageAndQuota where
  parseJSON = A.withObject "StorageGetUsageAndQuota" $ \o -> StorageGetUsageAndQuota
    <$> o A..: "usage"
    <*> o A..: "quota"
    <*> o A..: "overrideActive"
    <*> o A..: "usageBreakdown"
instance Command PStorageGetUsageAndQuota where
  type CommandResponse PStorageGetUsageAndQuota = StorageGetUsageAndQuota
  commandName _ = "Storage.getUsageAndQuota"

-- | Override quota for the specified origin

-- | Parameters of the 'Storage.overrideQuotaForOrigin' command.
data PStorageOverrideQuotaForOrigin = PStorageOverrideQuotaForOrigin
  {
    -- | Security origin.
    pStorageOverrideQuotaForOriginOrigin :: T.Text,
    -- | The quota size (in bytes) to override the original quota with.
    --   If this is called multiple times, the overridden quota will be equal to
    --   the quotaSize provided in the final call. If this is called without
    --   specifying a quotaSize, the quota will be reset to the default value for
    --   the specified origin. If this is called multiple times with different
    --   origins, the override will be maintained for each origin until it is
    --   disabled (called without a quotaSize).
    pStorageOverrideQuotaForOriginQuotaSize :: Maybe Double
  }
  deriving (Eq, Show)
pStorageOverrideQuotaForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageOverrideQuotaForOrigin
pStorageOverrideQuotaForOrigin
  arg_pStorageOverrideQuotaForOriginOrigin
  = PStorageOverrideQuotaForOrigin
    arg_pStorageOverrideQuotaForOriginOrigin
    Nothing
instance ToJSON PStorageOverrideQuotaForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageOverrideQuotaForOriginOrigin p),
    ("quotaSize" A..=) <$> (pStorageOverrideQuotaForOriginQuotaSize p)
    ]
instance Command PStorageOverrideQuotaForOrigin where
  type CommandResponse PStorageOverrideQuotaForOrigin = ()
  commandName _ = "Storage.overrideQuotaForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers origin to be notified when an update occurs to its cache storage list.

-- | Parameters of the 'Storage.trackCacheStorageForOrigin' command.
data PStorageTrackCacheStorageForOrigin = PStorageTrackCacheStorageForOrigin
  {
    -- | Security origin.
    pStorageTrackCacheStorageForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackCacheStorageForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageTrackCacheStorageForOrigin
pStorageTrackCacheStorageForOrigin
  arg_pStorageTrackCacheStorageForOriginOrigin
  = PStorageTrackCacheStorageForOrigin
    arg_pStorageTrackCacheStorageForOriginOrigin
instance ToJSON PStorageTrackCacheStorageForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageTrackCacheStorageForOriginOrigin p)
    ]
instance Command PStorageTrackCacheStorageForOrigin where
  type CommandResponse PStorageTrackCacheStorageForOrigin = ()
  commandName _ = "Storage.trackCacheStorageForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers origin to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForOrigin' command.
data PStorageTrackIndexedDBForOrigin = PStorageTrackIndexedDBForOrigin
  {
    -- | Security origin.
    pStorageTrackIndexedDBForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackIndexedDBForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageTrackIndexedDBForOrigin
pStorageTrackIndexedDBForOrigin
  arg_pStorageTrackIndexedDBForOriginOrigin
  = PStorageTrackIndexedDBForOrigin
    arg_pStorageTrackIndexedDBForOriginOrigin
instance ToJSON PStorageTrackIndexedDBForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageTrackIndexedDBForOriginOrigin p)
    ]
instance Command PStorageTrackIndexedDBForOrigin where
  type CommandResponse PStorageTrackIndexedDBForOrigin = ()
  commandName _ = "Storage.trackIndexedDBForOrigin"
  fromJSON = const . A.Success . const ()

-- | Registers storage key to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForStorageKey' command.
data PStorageTrackIndexedDBForStorageKey = PStorageTrackIndexedDBForStorageKey
  {
    -- | Storage key.
    pStorageTrackIndexedDBForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageTrackIndexedDBForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageTrackIndexedDBForStorageKey
pStorageTrackIndexedDBForStorageKey
  arg_pStorageTrackIndexedDBForStorageKeyStorageKey
  = PStorageTrackIndexedDBForStorageKey
    arg_pStorageTrackIndexedDBForStorageKeyStorageKey
instance ToJSON PStorageTrackIndexedDBForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageTrackIndexedDBForStorageKeyStorageKey p)
    ]
instance Command PStorageTrackIndexedDBForStorageKey where
  type CommandResponse PStorageTrackIndexedDBForStorageKey = ()
  commandName _ = "Storage.trackIndexedDBForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Unregisters origin from receiving notifications for cache storage.

-- | Parameters of the 'Storage.untrackCacheStorageForOrigin' command.
data PStorageUntrackCacheStorageForOrigin = PStorageUntrackCacheStorageForOrigin
  {
    -- | Security origin.
    pStorageUntrackCacheStorageForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackCacheStorageForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageUntrackCacheStorageForOrigin
pStorageUntrackCacheStorageForOrigin
  arg_pStorageUntrackCacheStorageForOriginOrigin
  = PStorageUntrackCacheStorageForOrigin
    arg_pStorageUntrackCacheStorageForOriginOrigin
instance ToJSON PStorageUntrackCacheStorageForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageUntrackCacheStorageForOriginOrigin p)
    ]
instance Command PStorageUntrackCacheStorageForOrigin where
  type CommandResponse PStorageUntrackCacheStorageForOrigin = ()
  commandName _ = "Storage.untrackCacheStorageForOrigin"
  fromJSON = const . A.Success . const ()

-- | Unregisters origin from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForOrigin' command.
data PStorageUntrackIndexedDBForOrigin = PStorageUntrackIndexedDBForOrigin
  {
    -- | Security origin.
    pStorageUntrackIndexedDBForOriginOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackIndexedDBForOrigin
  {-
  -- | Security origin.
  -}
  :: T.Text
  -> PStorageUntrackIndexedDBForOrigin
pStorageUntrackIndexedDBForOrigin
  arg_pStorageUntrackIndexedDBForOriginOrigin
  = PStorageUntrackIndexedDBForOrigin
    arg_pStorageUntrackIndexedDBForOriginOrigin
instance ToJSON PStorageUntrackIndexedDBForOrigin where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pStorageUntrackIndexedDBForOriginOrigin p)
    ]
instance Command PStorageUntrackIndexedDBForOrigin where
  type CommandResponse PStorageUntrackIndexedDBForOrigin = ()
  commandName _ = "Storage.untrackIndexedDBForOrigin"
  fromJSON = const . A.Success . const ()

-- | Unregisters storage key from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForStorageKey' command.
data PStorageUntrackIndexedDBForStorageKey = PStorageUntrackIndexedDBForStorageKey
  {
    -- | Storage key.
    pStorageUntrackIndexedDBForStorageKeyStorageKey :: T.Text
  }
  deriving (Eq, Show)
pStorageUntrackIndexedDBForStorageKey
  {-
  -- | Storage key.
  -}
  :: T.Text
  -> PStorageUntrackIndexedDBForStorageKey
pStorageUntrackIndexedDBForStorageKey
  arg_pStorageUntrackIndexedDBForStorageKeyStorageKey
  = PStorageUntrackIndexedDBForStorageKey
    arg_pStorageUntrackIndexedDBForStorageKeyStorageKey
instance ToJSON PStorageUntrackIndexedDBForStorageKey where
  toJSON p = A.object $ catMaybes [
    ("storageKey" A..=) <$> Just (pStorageUntrackIndexedDBForStorageKeyStorageKey p)
    ]
instance Command PStorageUntrackIndexedDBForStorageKey where
  type CommandResponse PStorageUntrackIndexedDBForStorageKey = ()
  commandName _ = "Storage.untrackIndexedDBForStorageKey"
  fromJSON = const . A.Success . const ()

-- | Returns the number of stored Trust Tokens per issuer for the
--   current browsing context.

-- | Parameters of the 'Storage.getTrustTokens' command.
data PStorageGetTrustTokens = PStorageGetTrustTokens
  deriving (Eq, Show)
pStorageGetTrustTokens
  :: PStorageGetTrustTokens
pStorageGetTrustTokens
  = PStorageGetTrustTokens
instance ToJSON PStorageGetTrustTokens where
  toJSON _ = A.Null
data StorageGetTrustTokens = StorageGetTrustTokens
  {
    storageGetTrustTokensTokens :: [StorageTrustTokens]
  }
  deriving (Eq, Show)
instance FromJSON StorageGetTrustTokens where
  parseJSON = A.withObject "StorageGetTrustTokens" $ \o -> StorageGetTrustTokens
    <$> o A..: "tokens"
instance Command PStorageGetTrustTokens where
  type CommandResponse PStorageGetTrustTokens = StorageGetTrustTokens
  commandName _ = "Storage.getTrustTokens"

-- | Removes all Trust Tokens issued by the provided issuerOrigin.
--   Leaves other stored data, including the issuer's Redemption Records, intact.

-- | Parameters of the 'Storage.clearTrustTokens' command.
data PStorageClearTrustTokens = PStorageClearTrustTokens
  {
    pStorageClearTrustTokensIssuerOrigin :: T.Text
  }
  deriving (Eq, Show)
pStorageClearTrustTokens
  :: T.Text
  -> PStorageClearTrustTokens
pStorageClearTrustTokens
  arg_pStorageClearTrustTokensIssuerOrigin
  = PStorageClearTrustTokens
    arg_pStorageClearTrustTokensIssuerOrigin
instance ToJSON PStorageClearTrustTokens where
  toJSON p = A.object $ catMaybes [
    ("issuerOrigin" A..=) <$> Just (pStorageClearTrustTokensIssuerOrigin p)
    ]
data StorageClearTrustTokens = StorageClearTrustTokens
  {
    -- | True if any tokens were deleted, false otherwise.
    storageClearTrustTokensDidDeleteTokens :: Bool
  }
  deriving (Eq, Show)
instance FromJSON StorageClearTrustTokens where
  parseJSON = A.withObject "StorageClearTrustTokens" $ \o -> StorageClearTrustTokens
    <$> o A..: "didDeleteTokens"
instance Command PStorageClearTrustTokens where
  type CommandResponse PStorageClearTrustTokens = StorageClearTrustTokens
  commandName _ = "Storage.clearTrustTokens"

-- | Gets details for a named interest group.

-- | Parameters of the 'Storage.getInterestGroupDetails' command.
data PStorageGetInterestGroupDetails = PStorageGetInterestGroupDetails
  {
    pStorageGetInterestGroupDetailsOwnerOrigin :: T.Text,
    pStorageGetInterestGroupDetailsName :: T.Text
  }
  deriving (Eq, Show)
pStorageGetInterestGroupDetails
  :: T.Text
  -> T.Text
  -> PStorageGetInterestGroupDetails
pStorageGetInterestGroupDetails
  arg_pStorageGetInterestGroupDetailsOwnerOrigin
  arg_pStorageGetInterestGroupDetailsName
  = PStorageGetInterestGroupDetails
    arg_pStorageGetInterestGroupDetailsOwnerOrigin
    arg_pStorageGetInterestGroupDetailsName
instance ToJSON PStorageGetInterestGroupDetails where
  toJSON p = A.object $ catMaybes [
    ("ownerOrigin" A..=) <$> Just (pStorageGetInterestGroupDetailsOwnerOrigin p),
    ("name" A..=) <$> Just (pStorageGetInterestGroupDetailsName p)
    ]
data StorageGetInterestGroupDetails = StorageGetInterestGroupDetails
  {
    storageGetInterestGroupDetailsDetails :: StorageInterestGroupDetails
  }
  deriving (Eq, Show)
instance FromJSON StorageGetInterestGroupDetails where
  parseJSON = A.withObject "StorageGetInterestGroupDetails" $ \o -> StorageGetInterestGroupDetails
    <$> o A..: "details"
instance Command PStorageGetInterestGroupDetails where
  type CommandResponse PStorageGetInterestGroupDetails = StorageGetInterestGroupDetails
  commandName _ = "Storage.getInterestGroupDetails"

-- | Enables/Disables issuing of interestGroupAccessed events.

-- | Parameters of the 'Storage.setInterestGroupTracking' command.
data PStorageSetInterestGroupTracking = PStorageSetInterestGroupTracking
  {
    pStorageSetInterestGroupTrackingEnable :: Bool
  }
  deriving (Eq, Show)
pStorageSetInterestGroupTracking
  :: Bool
  -> PStorageSetInterestGroupTracking
pStorageSetInterestGroupTracking
  arg_pStorageSetInterestGroupTrackingEnable
  = PStorageSetInterestGroupTracking
    arg_pStorageSetInterestGroupTrackingEnable
instance ToJSON PStorageSetInterestGroupTracking where
  toJSON p = A.object $ catMaybes [
    ("enable" A..=) <$> Just (pStorageSetInterestGroupTrackingEnable p)
    ]
instance Command PStorageSetInterestGroupTracking where
  type CommandResponse PStorageSetInterestGroupTracking = ()
  commandName _ = "Storage.setInterestGroupTracking"
  fromJSON = const . A.Success . const ()

