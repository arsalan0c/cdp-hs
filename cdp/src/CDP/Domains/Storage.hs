{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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

import CDP.Domains.BrowserTarget as BrowserTarget
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'Storage.SerializedStorageKey' .
type StorageSerializedStorageKey = String

-- | Type 'Storage.StorageType' .Enum of possible storage types.
data StorageStorageType = StorageStorageTypeAppcache | StorageStorageTypeCookies | StorageStorageTypeFileSystems | StorageStorageTypeIndexeddb | StorageStorageTypeLocalStorage | StorageStorageTypeShaderCache | StorageStorageTypeWebsql | StorageStorageTypeServiceWorkers | StorageStorageTypeCacheStorage | StorageStorageTypeInterestGroups | StorageStorageTypeAll | StorageStorageTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON StorageStorageType where
   parseJSON = A.withText  "StorageStorageType"  $ \v -> do
      case v of
         "appcache" -> pure StorageStorageTypeAppcache
         "cookies" -> pure StorageStorageTypeCookies
         "file_systems" -> pure StorageStorageTypeFileSystems
         "indexeddb" -> pure StorageStorageTypeIndexeddb
         "local_storage" -> pure StorageStorageTypeLocalStorage
         "shader_cache" -> pure StorageStorageTypeShaderCache
         "websql" -> pure StorageStorageTypeWebsql
         "service_workers" -> pure StorageStorageTypeServiceWorkers
         "cache_storage" -> pure StorageStorageTypeCacheStorage
         "interest_groups" -> pure StorageStorageTypeInterestGroups
         "all" -> pure StorageStorageTypeAll
         "other" -> pure StorageStorageTypeOther
         _ -> fail "failed to parse StorageStorageType"

instance ToJSON StorageStorageType where
   toJSON v = A.String $
      case v of
         StorageStorageTypeAppcache -> "appcache"
         StorageStorageTypeCookies -> "cookies"
         StorageStorageTypeFileSystems -> "file_systems"
         StorageStorageTypeIndexeddb -> "indexeddb"
         StorageStorageTypeLocalStorage -> "local_storage"
         StorageStorageTypeShaderCache -> "shader_cache"
         StorageStorageTypeWebsql -> "websql"
         StorageStorageTypeServiceWorkers -> "service_workers"
         StorageStorageTypeCacheStorage -> "cache_storage"
         StorageStorageTypeInterestGroups -> "interest_groups"
         StorageStorageTypeAll -> "all"
         StorageStorageTypeOther -> "other"



-- | Type 'Storage.UsageForType' .Usage for a storage type.
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



-- | Type 'Storage.TrustTokens' .Pair of issuer origin and number of available (signed, but not used) Trust
--   Tokens from that issuer.
data StorageTrustTokens = StorageTrustTokens {
  storageTrustTokensIssuerOrigin :: String,
  storageTrustTokensCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  StorageTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Storage.InterestGroupAccessType' .Enum of interest group access types.
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



-- | Type 'Storage.InterestGroupAd' .Ad advertising element inside an interest group.
data StorageInterestGroupAd = StorageInterestGroupAd {
  storageInterestGroupAdRenderUrl :: String,
  storageInterestGroupAdMetadata :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageInterestGroupAd  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  StorageInterestGroupAd where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Storage.InterestGroupDetails' .The full details of an interest group.
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



-- | Type of the 'Storage.cacheStorageListUpdated' event.
data StorageCacheStorageListUpdated = StorageCacheStorageListUpdated {
  -- | Origin to update.
  storageCacheStorageListUpdatedOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageCacheStorageListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageCacheStorageListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Storage.indexedDBContentUpdated' event.
data StorageIndexedDbContentUpdated = StorageIndexedDbContentUpdated {
  -- | Origin to update.
  storageIndexedDbContentUpdatedOrigin :: String,
  -- | Database to update.
  storageIndexedDbContentUpdatedDatabaseName :: String,
  -- | ObjectStore to update.
  storageIndexedDbContentUpdatedObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDbContentUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDbContentUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Storage.indexedDBListUpdated' event.
data StorageIndexedDbListUpdated = StorageIndexedDbListUpdated {
  -- | Origin to update.
  storageIndexedDbListUpdatedOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDbListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDbListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



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





-- | Parameters of the 'storageGetStorageKeyForFrame' command.
data PStorageGetStorageKeyForFrame = PStorageGetStorageKeyForFrame {
  pStorageGetStorageKeyForFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetStorageKeyForFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PStorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Storage.getStorageKeyForFrame' command.
--   Returns a storage key given a frame id.
--   Parameters: 'PStorageGetStorageKeyForFrame'
--   Returns: 'StorageGetStorageKeyForFrame'
storageGetStorageKeyForFrame :: Handle ev -> PStorageGetStorageKeyForFrame -> IO StorageGetStorageKeyForFrame
storageGetStorageKeyForFrame handle params = sendReceiveCommandResult handle "Storage.getStorageKeyForFrame" (Just params)

-- | Return type of the 'storageGetStorageKeyForFrame' command.
data StorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame {
  storageGetStorageKeyForFrameStorageKey :: StorageSerializedStorageKey
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command StorageGetStorageKeyForFrame where
   commandName _ = "Storage.getStorageKeyForFrame"



-- | Parameters of the 'storageClearDataForOrigin' command.
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


-- | Function for the 'Storage.clearDataForOrigin' command.
--   Clears storage for origin.
--   Parameters: 'PStorageClearDataForOrigin'
storageClearDataForOrigin :: Handle ev -> PStorageClearDataForOrigin -> IO ()
storageClearDataForOrigin handle params = sendReceiveCommand handle "Storage.clearDataForOrigin" (Just params)


-- | Parameters of the 'storageGetCookies' command.
data PStorageGetCookies = PStorageGetCookies {
  -- | Browser context to use when called on the browser endpoint.
  pStorageGetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'Storage.getCookies' command.
--   Returns all browser cookies.
--   Parameters: 'PStorageGetCookies'
--   Returns: 'StorageGetCookies'
storageGetCookies :: Handle ev -> PStorageGetCookies -> IO StorageGetCookies
storageGetCookies handle params = sendReceiveCommandResult handle "Storage.getCookies" (Just params)

-- | Return type of the 'storageGetCookies' command.
data StorageGetCookies = StorageGetCookies {
  -- | Array of cookie objects.
  storageGetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command StorageGetCookies where
   commandName _ = "Storage.getCookies"



-- | Parameters of the 'storageSetCookies' command.
data PStorageSetCookies = PStorageSetCookies {
  -- | Cookies to be set.
  pStorageSetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookieParam],
  -- | Browser context to use when called on the browser endpoint.
  pStorageSetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'Storage.setCookies' command.
--   Sets given cookies.
--   Parameters: 'PStorageSetCookies'
storageSetCookies :: Handle ev -> PStorageSetCookies -> IO ()
storageSetCookies handle params = sendReceiveCommand handle "Storage.setCookies" (Just params)


-- | Parameters of the 'storageClearCookies' command.
data PStorageClearCookies = PStorageClearCookies {
  -- | Browser context to use when called on the browser endpoint.
  pStorageClearCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PStorageClearCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Storage.clearCookies' command.
--   Clears cookies.
--   Parameters: 'PStorageClearCookies'
storageClearCookies :: Handle ev -> PStorageClearCookies -> IO ()
storageClearCookies handle params = sendReceiveCommand handle "Storage.clearCookies" (Just params)


-- | Parameters of the 'storageGetUsageAndQuota' command.
data PStorageGetUsageAndQuota = PStorageGetUsageAndQuota {
  -- | Security origin.
  pStorageGetUsageAndQuotaOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetUsageAndQuota  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageGetUsageAndQuota where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Storage.getUsageAndQuota' command.
--   Returns usage and quota in bytes.
--   Parameters: 'PStorageGetUsageAndQuota'
--   Returns: 'StorageGetUsageAndQuota'
storageGetUsageAndQuota :: Handle ev -> PStorageGetUsageAndQuota -> IO StorageGetUsageAndQuota
storageGetUsageAndQuota handle params = sendReceiveCommandResult handle "Storage.getUsageAndQuota" (Just params)

-- | Return type of the 'storageGetUsageAndQuota' command.
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

instance Command StorageGetUsageAndQuota where
   commandName _ = "Storage.getUsageAndQuota"



-- | Parameters of the 'storageOverrideQuotaForOrigin' command.
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


-- | Function for the 'Storage.overrideQuotaForOrigin' command.
--   Override quota for the specified origin
--   Parameters: 'PStorageOverrideQuotaForOrigin'
storageOverrideQuotaForOrigin :: Handle ev -> PStorageOverrideQuotaForOrigin -> IO ()
storageOverrideQuotaForOrigin handle params = sendReceiveCommand handle "Storage.overrideQuotaForOrigin" (Just params)


-- | Parameters of the 'storageTrackCacheStorageForOrigin' command.
data PStorageTrackCacheStorageForOrigin = PStorageTrackCacheStorageForOrigin {
  -- | Security origin.
  pStorageTrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Storage.trackCacheStorageForOrigin' command.
--   Registers origin to be notified when an update occurs to its cache storage list.
--   Parameters: 'PStorageTrackCacheStorageForOrigin'
storageTrackCacheStorageForOrigin :: Handle ev -> PStorageTrackCacheStorageForOrigin -> IO ()
storageTrackCacheStorageForOrigin handle params = sendReceiveCommand handle "Storage.trackCacheStorageForOrigin" (Just params)


-- | Parameters of the 'storageTrackIndexedDbForOrigin' command.
data PStorageTrackIndexedDbForOrigin = PStorageTrackIndexedDbForOrigin {
  -- | Security origin.
  pStorageTrackIndexedDbForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackIndexedDbForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackIndexedDbForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'Storage.trackIndexedDBForOrigin' command.
--   Registers origin to be notified when an update occurs to its IndexedDB.
--   Parameters: 'PStorageTrackIndexedDbForOrigin'
storageTrackIndexedDbForOrigin :: Handle ev -> PStorageTrackIndexedDbForOrigin -> IO ()
storageTrackIndexedDbForOrigin handle params = sendReceiveCommand handle "Storage.trackIndexedDBForOrigin" (Just params)


-- | Parameters of the 'storageUntrackCacheStorageForOrigin' command.
data PStorageUntrackCacheStorageForOrigin = PStorageUntrackCacheStorageForOrigin {
  -- | Security origin.
  pStorageUntrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'Storage.untrackCacheStorageForOrigin' command.
--   Unregisters origin from receiving notifications for cache storage.
--   Parameters: 'PStorageUntrackCacheStorageForOrigin'
storageUntrackCacheStorageForOrigin :: Handle ev -> PStorageUntrackCacheStorageForOrigin -> IO ()
storageUntrackCacheStorageForOrigin handle params = sendReceiveCommand handle "Storage.untrackCacheStorageForOrigin" (Just params)


-- | Parameters of the 'storageUntrackIndexedDbForOrigin' command.
data PStorageUntrackIndexedDbForOrigin = PStorageUntrackIndexedDbForOrigin {
  -- | Security origin.
  pStorageUntrackIndexedDbForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackIndexedDbForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackIndexedDbForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Storage.untrackIndexedDBForOrigin' command.
--   Unregisters origin from receiving notifications for IndexedDB.
--   Parameters: 'PStorageUntrackIndexedDbForOrigin'
storageUntrackIndexedDbForOrigin :: Handle ev -> PStorageUntrackIndexedDbForOrigin -> IO ()
storageUntrackIndexedDbForOrigin handle params = sendReceiveCommand handle "Storage.untrackIndexedDBForOrigin" (Just params)


-- | Function for the 'Storage.getTrustTokens' command.
--   Returns the number of stored Trust Tokens per issuer for the
--   current browsing context.
--   Returns: 'StorageGetTrustTokens'
storageGetTrustTokens :: Handle ev -> IO StorageGetTrustTokens
storageGetTrustTokens handle = sendReceiveCommandResult handle "Storage.getTrustTokens" (Nothing :: Maybe ())

-- | Return type of the 'storageGetTrustTokens' command.
data StorageGetTrustTokens = StorageGetTrustTokens {
  storageGetTrustTokensTokens :: [StorageTrustTokens]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command StorageGetTrustTokens where
   commandName _ = "Storage.getTrustTokens"



-- | Parameters of the 'storageClearTrustTokens' command.
data PStorageClearTrustTokens = PStorageClearTrustTokens {
  pStorageClearTrustTokensIssuerOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Storage.clearTrustTokens' command.
--   Removes all Trust Tokens issued by the provided issuerOrigin.
--   Leaves other stored data, including the issuer's Redemption Records, intact.
--   Parameters: 'PStorageClearTrustTokens'
--   Returns: 'StorageClearTrustTokens'
storageClearTrustTokens :: Handle ev -> PStorageClearTrustTokens -> IO StorageClearTrustTokens
storageClearTrustTokens handle params = sendReceiveCommandResult handle "Storage.clearTrustTokens" (Just params)

-- | Return type of the 'storageClearTrustTokens' command.
data StorageClearTrustTokens = StorageClearTrustTokens {
  -- | True if any tokens were deleted, false otherwise.
  storageClearTrustTokensDidDeleteTokens :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command StorageClearTrustTokens where
   commandName _ = "Storage.clearTrustTokens"



-- | Parameters of the 'storageGetInterestGroupDetails' command.
data PStorageGetInterestGroupDetails = PStorageGetInterestGroupDetails {
  pStorageGetInterestGroupDetailsOwnerOrigin :: String,
  pStorageGetInterestGroupDetailsName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetInterestGroupDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'Storage.getInterestGroupDetails' command.
--   Gets details for a named interest group.
--   Parameters: 'PStorageGetInterestGroupDetails'
--   Returns: 'StorageGetInterestGroupDetails'
storageGetInterestGroupDetails :: Handle ev -> PStorageGetInterestGroupDetails -> IO StorageGetInterestGroupDetails
storageGetInterestGroupDetails handle params = sendReceiveCommandResult handle "Storage.getInterestGroupDetails" (Just params)

-- | Return type of the 'storageGetInterestGroupDetails' command.
data StorageGetInterestGroupDetails = StorageGetInterestGroupDetails {
  storageGetInterestGroupDetailsDetails :: StorageInterestGroupDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command StorageGetInterestGroupDetails where
   commandName _ = "Storage.getInterestGroupDetails"



-- | Parameters of the 'storageSetInterestGroupTracking' command.
data PStorageSetInterestGroupTracking = PStorageSetInterestGroupTracking {
  pStorageSetInterestGroupTrackingEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetInterestGroupTracking  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PStorageSetInterestGroupTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'Storage.setInterestGroupTracking' command.
--   Enables/Disables issuing of interestGroupAccessed events.
--   Parameters: 'PStorageSetInterestGroupTracking'
storageSetInterestGroupTracking :: Handle ev -> PStorageSetInterestGroupTracking -> IO ()
storageSetInterestGroupTracking handle params = sendReceiveCommand handle "Storage.setInterestGroupTracking" (Just params)



