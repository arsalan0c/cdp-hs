{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Storage 
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
type StorageSerializedStorageKey = String

-- | Type 'Storage.StorageType'.
--   Enum of possible storage types.
data StorageStorageType = StorageStorageTypeAppcache | StorageStorageTypeCookies | StorageStorageTypeFile_systems | StorageStorageTypeIndexeddb | StorageStorageTypeLocal_storage | StorageStorageTypeShader_cache | StorageStorageTypeWebsql | StorageStorageTypeService_workers | StorageStorageTypeCache_storage | StorageStorageTypeInterest_groups | StorageStorageTypeAll | StorageStorageTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON StorageStorageType where
   parseJSON = A.withText  "StorageStorageType"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse StorageStorageType"

instance ToJSON StorageStorageType where
   toJSON v = A.String $
      case v of
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
data StorageUsageForType = StorageUsageForType {
  -- | Name of storage type.
  storageUsageForTypeStorageType :: StorageStorageType,
  -- | Storage usage (bytes).
  storageUsageForTypeUsage :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageUsageForType  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  StorageUsageForType where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Storage.TrustTokens'.
--   Pair of issuer origin and number of available (signed, but not used) Trust
--   Tokens from that issuer.
data StorageTrustTokens = StorageTrustTokens {
  storageTrustTokensIssuerOrigin :: String,
  storageTrustTokensCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  StorageTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Storage.InterestGroupAccessType'.
--   Enum of interest group access types.
data StorageInterestGroupAccessType = StorageInterestGroupAccessTypeJoin | StorageInterestGroupAccessTypeLeave | StorageInterestGroupAccessTypeUpdate | StorageInterestGroupAccessTypeBid | StorageInterestGroupAccessTypeWin
   deriving (Ord, Eq, Show, Read)
instance FromJSON StorageInterestGroupAccessType where
   parseJSON = A.withText  "StorageInterestGroupAccessType"  $ \v -> do
      case v of
         "join" -> pure StorageInterestGroupAccessTypeJoin
         "leave" -> pure StorageInterestGroupAccessTypeLeave
         "update" -> pure StorageInterestGroupAccessTypeUpdate
         "bid" -> pure StorageInterestGroupAccessTypeBid
         "win" -> pure StorageInterestGroupAccessTypeWin
         _ -> fail "failed to parse StorageInterestGroupAccessType"

instance ToJSON StorageInterestGroupAccessType where
   toJSON v = A.String $
      case v of
         StorageInterestGroupAccessTypeJoin -> "join"
         StorageInterestGroupAccessTypeLeave -> "leave"
         StorageInterestGroupAccessTypeUpdate -> "update"
         StorageInterestGroupAccessTypeBid -> "bid"
         StorageInterestGroupAccessTypeWin -> "win"



-- | Type 'Storage.InterestGroupAd'.
--   Ad advertising element inside an interest group.
data StorageInterestGroupAd = StorageInterestGroupAd {
  storageInterestGroupAdRenderUrl :: String,
  storageInterestGroupAdMetadata :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageInterestGroupAd  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  StorageInterestGroupAd where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Storage.InterestGroupDetails'.
--   The full details of an interest group.
data StorageInterestGroupDetails = StorageInterestGroupDetails {
  storageInterestGroupDetailsOwnerOrigin :: String,
  storageInterestGroupDetailsName :: String,
  storageInterestGroupDetailsExpirationTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  storageInterestGroupDetailsJoiningOrigin :: String,
  storageInterestGroupDetailsBiddingUrl :: Maybe String,
  storageInterestGroupDetailsBiddingWasmHelperUrl :: Maybe String,
  storageInterestGroupDetailsUpdateUrl :: Maybe String,
  storageInterestGroupDetailsTrustedBiddingSignalsUrl :: Maybe String,
  storageInterestGroupDetailsTrustedBiddingSignalsKeys :: [String],
  storageInterestGroupDetailsUserBiddingSignals :: Maybe String,
  storageInterestGroupDetailsAds :: [StorageInterestGroupAd],
  storageInterestGroupDetailsAdComponents :: [StorageInterestGroupAd]
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageInterestGroupDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  StorageInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Type of the 'Storage.cacheStorageContentUpdated' event.
data StorageCacheStorageContentUpdated = StorageCacheStorageContentUpdated {
  -- | Origin to update.
  storageCacheStorageContentUpdatedOrigin :: String,
  -- | Name of cache in origin.
  storageCacheStorageContentUpdatedCacheName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageCacheStorageContentUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  StorageCacheStorageContentUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Event StorageCacheStorageContentUpdated where
    eventName _ = "Storage.cacheStorageContentUpdated"

-- | Type of the 'Storage.cacheStorageListUpdated' event.
data StorageCacheStorageListUpdated = StorageCacheStorageListUpdated {
  -- | Origin to update.
  storageCacheStorageListUpdatedOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageCacheStorageListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageCacheStorageListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Event StorageCacheStorageListUpdated where
    eventName _ = "Storage.cacheStorageListUpdated"

-- | Type of the 'Storage.indexedDBContentUpdated' event.
data StorageIndexedDBContentUpdated = StorageIndexedDBContentUpdated {
  -- | Origin to update.
  storageIndexedDBContentUpdatedOrigin :: String,
  -- | Storage key to update.
  storageIndexedDBContentUpdatedStorageKey :: String,
  -- | Database to update.
  storageIndexedDBContentUpdatedDatabaseName :: String,
  -- | ObjectStore to update.
  storageIndexedDBContentUpdatedObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDBContentUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDBContentUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Event StorageIndexedDBContentUpdated where
    eventName _ = "Storage.indexedDBContentUpdated"

-- | Type of the 'Storage.indexedDBListUpdated' event.
data StorageIndexedDBListUpdated = StorageIndexedDBListUpdated {
  -- | Origin to update.
  storageIndexedDBListUpdatedOrigin :: String,
  -- | Storage key to update.
  storageIndexedDBListUpdatedStorageKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDBListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDBListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Event StorageIndexedDBListUpdated where
    eventName _ = "Storage.indexedDBListUpdated"

-- | Type of the 'Storage.interestGroupAccessed' event.
data StorageInterestGroupAccessed = StorageInterestGroupAccessed {
  storageInterestGroupAccessedAccessTime :: DOMPageNetworkEmulationSecurity.NetworkTimeSinceEpoch,
  storageInterestGroupAccessedType :: StorageInterestGroupAccessType,
  storageInterestGroupAccessedOwnerOrigin :: String,
  storageInterestGroupAccessedName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageInterestGroupAccessed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  StorageInterestGroupAccessed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Event StorageInterestGroupAccessed where
    eventName _ = "Storage.interestGroupAccessed"



-- | Storage.getStorageKeyForFrame
--   Returns a storage key given a frame id.

-- | Parameters of the 'Storage.getStorageKeyForFrame' command.
data PStorageGetStorageKeyForFrame = PStorageGetStorageKeyForFrame {
  pStorageGetStorageKeyForFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetStorageKeyForFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PStorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Return type of the 'Storage.getStorageKeyForFrame' command.
data StorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame {
  storageGetStorageKeyForFrameStorageKey :: StorageSerializedStorageKey
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command PStorageGetStorageKeyForFrame where
   type CommandResponse PStorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame
   commandName _ = "Storage.getStorageKeyForFrame"



-- | Storage.clearDataForOrigin
--   Clears storage for origin.

-- | Parameters of the 'Storage.clearDataForOrigin' command.
data PStorageClearDataForOrigin = PStorageClearDataForOrigin {
  -- | Security origin.
  pStorageClearDataForOriginOrigin :: String,
  -- | Comma separated list of StorageType to clear.
  pStorageClearDataForOriginStorageTypes :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearDataForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PStorageClearDataForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PStorageClearDataForOrigin where
   type CommandResponse PStorageClearDataForOrigin = ()
   commandName _ = "Storage.clearDataForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.clearDataForStorageKey
--   Clears storage for storage key.

-- | Parameters of the 'Storage.clearDataForStorageKey' command.
data PStorageClearDataForStorageKey = PStorageClearDataForStorageKey {
  -- | Storage key.
  pStorageClearDataForStorageKeyStorageKey :: String,
  -- | Comma separated list of StorageType to clear.
  pStorageClearDataForStorageKeyStorageTypes :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearDataForStorageKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PStorageClearDataForStorageKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PStorageClearDataForStorageKey where
   type CommandResponse PStorageClearDataForStorageKey = ()
   commandName _ = "Storage.clearDataForStorageKey"
   fromJSON = const . A.Success . const ()


-- | Storage.getCookies
--   Returns all browser cookies.

-- | Parameters of the 'Storage.getCookies' command.
data PStorageGetCookies = PStorageGetCookies {
  -- | Browser context to use when called on the browser endpoint.
  pStorageGetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Return type of the 'Storage.getCookies' command.
data StorageGetCookies = StorageGetCookies {
  -- | Array of cookie objects.
  storageGetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command PStorageGetCookies where
   type CommandResponse PStorageGetCookies = StorageGetCookies
   commandName _ = "Storage.getCookies"



-- | Storage.setCookies
--   Sets given cookies.

-- | Parameters of the 'Storage.setCookies' command.
data PStorageSetCookies = PStorageSetCookies {
  -- | Cookies to be set.
  pStorageSetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookieParam],
  -- | Browser context to use when called on the browser endpoint.
  pStorageSetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Command PStorageSetCookies where
   type CommandResponse PStorageSetCookies = ()
   commandName _ = "Storage.setCookies"
   fromJSON = const . A.Success . const ()


-- | Storage.clearCookies
--   Clears cookies.

-- | Parameters of the 'Storage.clearCookies' command.
data PStorageClearCookies = PStorageClearCookies {
  -- | Browser context to use when called on the browser endpoint.
  pStorageClearCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextID
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PStorageClearCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command PStorageClearCookies where
   type CommandResponse PStorageClearCookies = ()
   commandName _ = "Storage.clearCookies"
   fromJSON = const . A.Success . const ()


-- | Storage.getUsageAndQuota
--   Returns usage and quota in bytes.

-- | Parameters of the 'Storage.getUsageAndQuota' command.
data PStorageGetUsageAndQuota = PStorageGetUsageAndQuota {
  -- | Security origin.
  pStorageGetUsageAndQuotaOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetUsageAndQuota  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageGetUsageAndQuota where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Storage.getUsageAndQuota' command.
data StorageGetUsageAndQuota = StorageGetUsageAndQuota {
  -- | Storage usage (bytes).
  storageGetUsageAndQuotaUsage :: Double,
  -- | Storage quota (bytes).
  storageGetUsageAndQuotaQuota :: Double,
  -- | Whether or not the origin has an active storage quota override
  storageGetUsageAndQuotaOverrideActive :: Bool,
  -- | Storage usage per type (bytes).
  storageGetUsageAndQuotaUsageBreakdown :: [StorageUsageForType]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetUsageAndQuota where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PStorageGetUsageAndQuota where
   type CommandResponse PStorageGetUsageAndQuota = StorageGetUsageAndQuota
   commandName _ = "Storage.getUsageAndQuota"



-- | Storage.overrideQuotaForOrigin
--   Override quota for the specified origin

-- | Parameters of the 'Storage.overrideQuotaForOrigin' command.
data PStorageOverrideQuotaForOrigin = PStorageOverrideQuotaForOrigin {
  -- | Security origin.
  pStorageOverrideQuotaForOriginOrigin :: String,
  -- | The quota size (in bytes) to override the original quota with.
  --   If this is called multiple times, the overridden quota will be equal to
  --   the quotaSize provided in the final call. If this is called without
  --   specifying a quotaSize, the quota will be reset to the default value for
  --   the specified origin. If this is called multiple times with different
  --   origins, the override will be maintained for each origin until it is
  --   disabled (called without a quotaSize).
  pStorageOverrideQuotaForOriginQuotaSize :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageOverrideQuotaForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PStorageOverrideQuotaForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PStorageOverrideQuotaForOrigin where
   type CommandResponse PStorageOverrideQuotaForOrigin = ()
   commandName _ = "Storage.overrideQuotaForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.trackCacheStorageForOrigin
--   Registers origin to be notified when an update occurs to its cache storage list.

-- | Parameters of the 'Storage.trackCacheStorageForOrigin' command.
data PStorageTrackCacheStorageForOrigin = PStorageTrackCacheStorageForOrigin {
  -- | Security origin.
  pStorageTrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Command PStorageTrackCacheStorageForOrigin where
   type CommandResponse PStorageTrackCacheStorageForOrigin = ()
   commandName _ = "Storage.trackCacheStorageForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.trackIndexedDBForOrigin
--   Registers origin to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForOrigin' command.
data PStorageTrackIndexedDBForOrigin = PStorageTrackIndexedDBForOrigin {
  -- | Security origin.
  pStorageTrackIndexedDBForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackIndexedDBForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackIndexedDBForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PStorageTrackIndexedDBForOrigin where
   type CommandResponse PStorageTrackIndexedDBForOrigin = ()
   commandName _ = "Storage.trackIndexedDBForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.trackIndexedDBForStorageKey
--   Registers storage key to be notified when an update occurs to its IndexedDB.

-- | Parameters of the 'Storage.trackIndexedDBForStorageKey' command.
data PStorageTrackIndexedDBForStorageKey = PStorageTrackIndexedDBForStorageKey {
  -- | Storage key.
  pStorageTrackIndexedDBForStorageKeyStorageKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackIndexedDBForStorageKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackIndexedDBForStorageKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PStorageTrackIndexedDBForStorageKey where
   type CommandResponse PStorageTrackIndexedDBForStorageKey = ()
   commandName _ = "Storage.trackIndexedDBForStorageKey"
   fromJSON = const . A.Success . const ()


-- | Storage.untrackCacheStorageForOrigin
--   Unregisters origin from receiving notifications for cache storage.

-- | Parameters of the 'Storage.untrackCacheStorageForOrigin' command.
data PStorageUntrackCacheStorageForOrigin = PStorageUntrackCacheStorageForOrigin {
  -- | Security origin.
  pStorageUntrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Command PStorageUntrackCacheStorageForOrigin where
   type CommandResponse PStorageUntrackCacheStorageForOrigin = ()
   commandName _ = "Storage.untrackCacheStorageForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.untrackIndexedDBForOrigin
--   Unregisters origin from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForOrigin' command.
data PStorageUntrackIndexedDBForOrigin = PStorageUntrackIndexedDBForOrigin {
  -- | Security origin.
  pStorageUntrackIndexedDBForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackIndexedDBForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackIndexedDBForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Command PStorageUntrackIndexedDBForOrigin where
   type CommandResponse PStorageUntrackIndexedDBForOrigin = ()
   commandName _ = "Storage.untrackIndexedDBForOrigin"
   fromJSON = const . A.Success . const ()


-- | Storage.untrackIndexedDBForStorageKey
--   Unregisters storage key from receiving notifications for IndexedDB.

-- | Parameters of the 'Storage.untrackIndexedDBForStorageKey' command.
data PStorageUntrackIndexedDBForStorageKey = PStorageUntrackIndexedDBForStorageKey {
  -- | Storage key.
  pStorageUntrackIndexedDBForStorageKeyStorageKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackIndexedDBForStorageKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackIndexedDBForStorageKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


instance Command PStorageUntrackIndexedDBForStorageKey where
   type CommandResponse PStorageUntrackIndexedDBForStorageKey = ()
   commandName _ = "Storage.untrackIndexedDBForStorageKey"
   fromJSON = const . A.Success . const ()


-- | Storage.getTrustTokens
--   Returns the number of stored Trust Tokens per issuer for the
--   current browsing context.

-- | Parameters of the 'Storage.getTrustTokens' command.
data PStorageGetTrustTokens = PStorageGetTrustTokens
instance ToJSON PStorageGetTrustTokens where toJSON _ = A.Null

-- | Return type of the 'Storage.getTrustTokens' command.
data StorageGetTrustTokens = StorageGetTrustTokens {
  storageGetTrustTokensTokens :: [StorageTrustTokens]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PStorageGetTrustTokens where
   type CommandResponse PStorageGetTrustTokens = StorageGetTrustTokens
   commandName _ = "Storage.getTrustTokens"



-- | Storage.clearTrustTokens
--   Removes all Trust Tokens issued by the provided issuerOrigin.
--   Leaves other stored data, including the issuer's Redemption Records, intact.

-- | Parameters of the 'Storage.clearTrustTokens' command.
data PStorageClearTrustTokens = PStorageClearTrustTokens {
  pStorageClearTrustTokensIssuerOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Storage.clearTrustTokens' command.
data StorageClearTrustTokens = StorageClearTrustTokens {
  -- | True if any tokens were deleted, false otherwise.
  storageClearTrustTokensDidDeleteTokens :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PStorageClearTrustTokens where
   type CommandResponse PStorageClearTrustTokens = StorageClearTrustTokens
   commandName _ = "Storage.clearTrustTokens"



-- | Storage.getInterestGroupDetails
--   Gets details for a named interest group.

-- | Parameters of the 'Storage.getInterestGroupDetails' command.
data PStorageGetInterestGroupDetails = PStorageGetInterestGroupDetails {
  pStorageGetInterestGroupDetailsOwnerOrigin :: String,
  pStorageGetInterestGroupDetailsName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetInterestGroupDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Return type of the 'Storage.getInterestGroupDetails' command.
data StorageGetInterestGroupDetails = StorageGetInterestGroupDetails {
  storageGetInterestGroupDetailsDetails :: StorageInterestGroupDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command PStorageGetInterestGroupDetails where
   type CommandResponse PStorageGetInterestGroupDetails = StorageGetInterestGroupDetails
   commandName _ = "Storage.getInterestGroupDetails"



-- | Storage.setInterestGroupTracking
--   Enables/Disables issuing of interestGroupAccessed events.

-- | Parameters of the 'Storage.setInterestGroupTracking' command.
data PStorageSetInterestGroupTracking = PStorageSetInterestGroupTracking {
  pStorageSetInterestGroupTrackingEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetInterestGroupTracking  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PStorageSetInterestGroupTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Command PStorageSetInterestGroupTracking where
   type CommandResponse PStorageSetInterestGroupTracking = ()
   commandName _ = "Storage.setInterestGroupTracking"
   fromJSON = const . A.Success . const ()



