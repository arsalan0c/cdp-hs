{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type StorageSerializedStorageKey = String
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



data StorageUsageForType = StorageUsageForType {
   storageUsageForTypeStorageType :: StorageStorageType,
   storageUsageForTypeUsage :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageUsageForType  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  StorageUsageForType where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data StorageTrustTokens = StorageTrustTokens {
   storageTrustTokensIssuerOrigin :: String,
   storageTrustTokensCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  StorageTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


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



data StorageInterestGroupAd = StorageInterestGroupAd {
   storageInterestGroupAdRenderUrl :: String,
   storageInterestGroupAdMetadata :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageInterestGroupAd  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  StorageInterestGroupAd where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



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





data StorageCacheStorageContentUpdated = StorageCacheStorageContentUpdated {
   storageCacheStorageContentUpdatedOrigin :: String,
   storageCacheStorageContentUpdatedCacheName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageCacheStorageContentUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  StorageCacheStorageContentUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data StorageCacheStorageListUpdated = StorageCacheStorageListUpdated {
   storageCacheStorageListUpdatedOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageCacheStorageListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageCacheStorageListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data StorageIndexedDbContentUpdated = StorageIndexedDbContentUpdated {
   storageIndexedDbContentUpdatedOrigin :: String,
   storageIndexedDbContentUpdatedDatabaseName :: String,
   storageIndexedDbContentUpdatedObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDbContentUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDbContentUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data StorageIndexedDbListUpdated = StorageIndexedDbListUpdated {
   storageIndexedDbListUpdatedOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON StorageIndexedDbListUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  StorageIndexedDbListUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



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





data PStorageGetStorageKeyForFrame = PStorageGetStorageKeyForFrame {
   pStorageGetStorageKeyForFrameFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetStorageKeyForFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PStorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


storageGetStorageKeyForFrame :: Handle ev -> PStorageGetStorageKeyForFrame -> IO (Either Error StorageGetStorageKeyForFrame)
storageGetStorageKeyForFrame handle params = sendReceiveCommandResult handle "Storage.getStorageKeyForFrame" (Just params)

data StorageGetStorageKeyForFrame = StorageGetStorageKeyForFrame {
   storageGetStorageKeyForFrameStorageKey :: StorageSerializedStorageKey
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetStorageKeyForFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command StorageGetStorageKeyForFrame where
   commandName _ = "Storage.getStorageKeyForFrame"




data PStorageClearDataForOrigin = PStorageClearDataForOrigin {
   pStorageClearDataForOriginOrigin :: String,
   pStorageClearDataForOriginStorageTypes :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearDataForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PStorageClearDataForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


storageClearDataForOrigin :: Handle ev -> PStorageClearDataForOrigin -> IO (Maybe Error)
storageClearDataForOrigin handle params = sendReceiveCommand handle "Storage.clearDataForOrigin" (Just params)



data PStorageGetCookies = PStorageGetCookies {
   pStorageGetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


storageGetCookies :: Handle ev -> PStorageGetCookies -> IO (Either Error StorageGetCookies)
storageGetCookies handle params = sendReceiveCommandResult handle "Storage.getCookies" (Just params)

data StorageGetCookies = StorageGetCookies {
   storageGetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command StorageGetCookies where
   commandName _ = "Storage.getCookies"




data PStorageSetCookies = PStorageSetCookies {
   pStorageSetCookiesCookies :: [DOMPageNetworkEmulationSecurity.NetworkCookieParam],
   pStorageSetCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PStorageSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


storageSetCookies :: Handle ev -> PStorageSetCookies -> IO (Maybe Error)
storageSetCookies handle params = sendReceiveCommand handle "Storage.setCookies" (Just params)



data PStorageClearCookies = PStorageClearCookies {
   pStorageClearCookiesBrowserContextId :: Maybe BrowserTarget.BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PStorageClearCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


storageClearCookies :: Handle ev -> PStorageClearCookies -> IO (Maybe Error)
storageClearCookies handle params = sendReceiveCommand handle "Storage.clearCookies" (Just params)



data PStorageGetUsageAndQuota = PStorageGetUsageAndQuota {
   pStorageGetUsageAndQuotaOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetUsageAndQuota  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageGetUsageAndQuota where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


storageGetUsageAndQuota :: Handle ev -> PStorageGetUsageAndQuota -> IO (Either Error StorageGetUsageAndQuota)
storageGetUsageAndQuota handle params = sendReceiveCommandResult handle "Storage.getUsageAndQuota" (Just params)

data StorageGetUsageAndQuota = StorageGetUsageAndQuota {
   storageGetUsageAndQuotaUsage :: Double,
   storageGetUsageAndQuotaQuota :: Double,
   storageGetUsageAndQuotaOverrideActive :: Bool,
   storageGetUsageAndQuotaUsageBreakdown :: [StorageUsageForType]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetUsageAndQuota where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command StorageGetUsageAndQuota where
   commandName _ = "Storage.getUsageAndQuota"




data PStorageOverrideQuotaForOrigin = PStorageOverrideQuotaForOrigin {
   pStorageOverrideQuotaForOriginOrigin :: String,
   pStorageOverrideQuotaForOriginQuotaSize :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageOverrideQuotaForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PStorageOverrideQuotaForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


storageOverrideQuotaForOrigin :: Handle ev -> PStorageOverrideQuotaForOrigin -> IO (Maybe Error)
storageOverrideQuotaForOrigin handle params = sendReceiveCommand handle "Storage.overrideQuotaForOrigin" (Just params)



data PStorageTrackCacheStorageForOrigin = PStorageTrackCacheStorageForOrigin {
   pStorageTrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


storageTrackCacheStorageForOrigin :: Handle ev -> PStorageTrackCacheStorageForOrigin -> IO (Maybe Error)
storageTrackCacheStorageForOrigin handle params = sendReceiveCommand handle "Storage.trackCacheStorageForOrigin" (Just params)



data PStorageTrackIndexedDbForOrigin = PStorageTrackIndexedDbForOrigin {
   pStorageTrackIndexedDbForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageTrackIndexedDbForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageTrackIndexedDbForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


storageTrackIndexedDbForOrigin :: Handle ev -> PStorageTrackIndexedDbForOrigin -> IO (Maybe Error)
storageTrackIndexedDbForOrigin handle params = sendReceiveCommand handle "Storage.trackIndexedDBForOrigin" (Just params)



data PStorageUntrackCacheStorageForOrigin = PStorageUntrackCacheStorageForOrigin {
   pStorageUntrackCacheStorageForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackCacheStorageForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackCacheStorageForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


storageUntrackCacheStorageForOrigin :: Handle ev -> PStorageUntrackCacheStorageForOrigin -> IO (Maybe Error)
storageUntrackCacheStorageForOrigin handle params = sendReceiveCommand handle "Storage.untrackCacheStorageForOrigin" (Just params)



data PStorageUntrackIndexedDbForOrigin = PStorageUntrackIndexedDbForOrigin {
   pStorageUntrackIndexedDbForOriginOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageUntrackIndexedDbForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PStorageUntrackIndexedDbForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


storageUntrackIndexedDbForOrigin :: Handle ev -> PStorageUntrackIndexedDbForOrigin -> IO (Maybe Error)
storageUntrackIndexedDbForOrigin handle params = sendReceiveCommand handle "Storage.untrackIndexedDBForOrigin" (Just params)


storageGetTrustTokens :: Handle ev -> IO (Either Error StorageGetTrustTokens)
storageGetTrustTokens handle = sendReceiveCommandResult handle "Storage.getTrustTokens" (Nothing :: Maybe ())

data StorageGetTrustTokens = StorageGetTrustTokens {
   storageGetTrustTokensTokens :: [StorageTrustTokens]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command StorageGetTrustTokens where
   commandName _ = "Storage.getTrustTokens"




data PStorageClearTrustTokens = PStorageClearTrustTokens {
   pStorageClearTrustTokensIssuerOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageClearTrustTokens  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PStorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


storageClearTrustTokens :: Handle ev -> PStorageClearTrustTokens -> IO (Either Error StorageClearTrustTokens)
storageClearTrustTokens handle params = sendReceiveCommandResult handle "Storage.clearTrustTokens" (Just params)

data StorageClearTrustTokens = StorageClearTrustTokens {
   storageClearTrustTokensDidDeleteTokens :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageClearTrustTokens where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command StorageClearTrustTokens where
   commandName _ = "Storage.clearTrustTokens"




data PStorageGetInterestGroupDetails = PStorageGetInterestGroupDetails {
   pStorageGetInterestGroupDetailsOwnerOrigin :: String,
   pStorageGetInterestGroupDetailsName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageGetInterestGroupDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PStorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


storageGetInterestGroupDetails :: Handle ev -> PStorageGetInterestGroupDetails -> IO (Either Error StorageGetInterestGroupDetails)
storageGetInterestGroupDetails handle params = sendReceiveCommandResult handle "Storage.getInterestGroupDetails" (Just params)

data StorageGetInterestGroupDetails = StorageGetInterestGroupDetails {
   storageGetInterestGroupDetailsDetails :: StorageInterestGroupDetails
} deriving (Generic, Eq, Show, Read)

instance FromJSON  StorageGetInterestGroupDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command StorageGetInterestGroupDetails where
   commandName _ = "Storage.getInterestGroupDetails"




data PStorageSetInterestGroupTracking = PStorageSetInterestGroupTracking {
   pStorageSetInterestGroupTrackingEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PStorageSetInterestGroupTracking  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PStorageSetInterestGroupTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


storageSetInterestGroupTracking :: Handle ev -> PStorageSetInterestGroupTracking -> IO (Maybe Error)
storageSetInterestGroupTracking handle params = sendReceiveCommand handle "Storage.setInterestGroupTracking" (Just params)



