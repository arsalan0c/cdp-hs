{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= CacheStorage

-}


module CDP.Domains.CacheStorage (module CDP.Domains.CacheStorage) where

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




-- | Type 'CacheStorage.CacheId'.
--   Unique identifier of the Cache object.
type CacheStorageCacheId = String

-- | Type 'CacheStorage.CachedResponseType'.
--   type of HTTP response cached
data CacheStorageCachedResponseType = CacheStorageCachedResponseTypeBasic | CacheStorageCachedResponseTypeCors | CacheStorageCachedResponseTypeDefault | CacheStorageCachedResponseTypeError | CacheStorageCachedResponseTypeOpaqueResponse | CacheStorageCachedResponseTypeOpaqueRedirect
  deriving (Ord, Eq, Show, Read)
instance FromJSON CacheStorageCachedResponseType where
  parseJSON = A.withText "CacheStorageCachedResponseType" $ \v -> case v of
    "basic" -> pure CacheStorageCachedResponseTypeBasic
    "cors" -> pure CacheStorageCachedResponseTypeCors
    "default" -> pure CacheStorageCachedResponseTypeDefault
    "error" -> pure CacheStorageCachedResponseTypeError
    "opaqueResponse" -> pure CacheStorageCachedResponseTypeOpaqueResponse
    "opaqueRedirect" -> pure CacheStorageCachedResponseTypeOpaqueRedirect
    "_" -> fail "failed to parse CacheStorageCachedResponseType"
instance ToJSON CacheStorageCachedResponseType where
  toJSON v = A.String $ case v of
    CacheStorageCachedResponseTypeBasic -> "basic"
    CacheStorageCachedResponseTypeCors -> "cors"
    CacheStorageCachedResponseTypeDefault -> "default"
    CacheStorageCachedResponseTypeError -> "error"
    CacheStorageCachedResponseTypeOpaqueResponse -> "opaqueResponse"
    CacheStorageCachedResponseTypeOpaqueRedirect -> "opaqueRedirect"

-- | Type 'CacheStorage.DataEntry'.
--   Data entry.
data CacheStorageDataEntry = CacheStorageDataEntry
  {
    -- | Request URL.
    cacheStorageDataEntryRequestURL :: String,
    -- | Request method.
    cacheStorageDataEntryRequestMethod :: String,
    -- | Request headers
    cacheStorageDataEntryRequestHeaders :: [CacheStorageHeader],
    -- | Number of seconds since epoch.
    cacheStorageDataEntryResponseTime :: Double,
    -- | HTTP response status code.
    cacheStorageDataEntryResponseStatus :: Int,
    -- | HTTP response status text.
    cacheStorageDataEntryResponseStatusText :: String,
    -- | HTTP response type
    cacheStorageDataEntryResponseType :: CacheStorageCachedResponseType,
    -- | Response headers
    cacheStorageDataEntryResponseHeaders :: [CacheStorageHeader]
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageDataEntry where
  parseJSON = A.withObject "CacheStorageDataEntry" $ \o -> CacheStorageDataEntry
    <$> o A..: "requestURL"
    <*> o A..: "requestMethod"
    <*> o A..: "requestHeaders"
    <*> o A..: "responseTime"
    <*> o A..: "responseStatus"
    <*> o A..: "responseStatusText"
    <*> o A..: "responseType"
    <*> o A..: "responseHeaders"
instance ToJSON CacheStorageDataEntry where
  toJSON p = A.object $ catMaybes [
    ("requestURL" A..=) <$> Just (cacheStorageDataEntryRequestURL p),
    ("requestMethod" A..=) <$> Just (cacheStorageDataEntryRequestMethod p),
    ("requestHeaders" A..=) <$> Just (cacheStorageDataEntryRequestHeaders p),
    ("responseTime" A..=) <$> Just (cacheStorageDataEntryResponseTime p),
    ("responseStatus" A..=) <$> Just (cacheStorageDataEntryResponseStatus p),
    ("responseStatusText" A..=) <$> Just (cacheStorageDataEntryResponseStatusText p),
    ("responseType" A..=) <$> Just (cacheStorageDataEntryResponseType p),
    ("responseHeaders" A..=) <$> Just (cacheStorageDataEntryResponseHeaders p)
    ]

-- | Type 'CacheStorage.Cache'.
--   Cache identifier.
data CacheStorageCache = CacheStorageCache
  {
    -- | An opaque unique id of the cache.
    cacheStorageCacheCacheId :: CacheStorageCacheId,
    -- | Security origin of the cache.
    cacheStorageCacheSecurityOrigin :: String,
    -- | The name of the cache.
    cacheStorageCacheCacheName :: String
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageCache where
  parseJSON = A.withObject "CacheStorageCache" $ \o -> CacheStorageCache
    <$> o A..: "cacheId"
    <*> o A..: "securityOrigin"
    <*> o A..: "cacheName"
instance ToJSON CacheStorageCache where
  toJSON p = A.object $ catMaybes [
    ("cacheId" A..=) <$> Just (cacheStorageCacheCacheId p),
    ("securityOrigin" A..=) <$> Just (cacheStorageCacheSecurityOrigin p),
    ("cacheName" A..=) <$> Just (cacheStorageCacheCacheName p)
    ]

-- | Type 'CacheStorage.Header'.
data CacheStorageHeader = CacheStorageHeader
  {
    cacheStorageHeaderName :: String,
    cacheStorageHeaderValue :: String
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageHeader where
  parseJSON = A.withObject "CacheStorageHeader" $ \o -> CacheStorageHeader
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON CacheStorageHeader where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cacheStorageHeaderName p),
    ("value" A..=) <$> Just (cacheStorageHeaderValue p)
    ]

-- | Type 'CacheStorage.CachedResponse'.
--   Cached response
data CacheStorageCachedResponse = CacheStorageCachedResponse
  {
    -- | Entry content, base64-encoded. (Encoded as a base64 string when passed over JSON)
    cacheStorageCachedResponseBody :: String
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageCachedResponse where
  parseJSON = A.withObject "CacheStorageCachedResponse" $ \o -> CacheStorageCachedResponse
    <$> o A..: "body"
instance ToJSON CacheStorageCachedResponse where
  toJSON p = A.object $ catMaybes [
    ("body" A..=) <$> Just (cacheStorageCachedResponseBody p)
    ]

-- | Deletes a cache.

-- | Parameters of the 'CacheStorage.deleteCache' command.
data PCacheStorageDeleteCache = PCacheStorageDeleteCache
  {
    -- | Id of cache for deletion.
    pCacheStorageDeleteCacheCacheId :: CacheStorageCacheId
  }
  deriving (Eq, Show)
pCacheStorageDeleteCache
  -- | Id of cache for deletion.
  :: CacheStorageCacheId
  -> PCacheStorageDeleteCache
pCacheStorageDeleteCache
  arg_pCacheStorageDeleteCacheCacheId
  = PCacheStorageDeleteCache
    arg_pCacheStorageDeleteCacheCacheId
instance ToJSON PCacheStorageDeleteCache where
  toJSON p = A.object $ catMaybes [
    ("cacheId" A..=) <$> Just (pCacheStorageDeleteCacheCacheId p)
    ]
instance Command PCacheStorageDeleteCache where
  type CommandResponse PCacheStorageDeleteCache = ()
  commandName _ = "CacheStorage.deleteCache"
  fromJSON = const . A.Success . const ()

-- | Deletes a cache entry.

-- | Parameters of the 'CacheStorage.deleteEntry' command.
data PCacheStorageDeleteEntry = PCacheStorageDeleteEntry
  {
    -- | Id of cache where the entry will be deleted.
    pCacheStorageDeleteEntryCacheId :: CacheStorageCacheId,
    -- | URL spec of the request.
    pCacheStorageDeleteEntryRequest :: String
  }
  deriving (Eq, Show)
pCacheStorageDeleteEntry
  -- | Id of cache where the entry will be deleted.
  :: CacheStorageCacheId
  -- | URL spec of the request.
  -> String
  -> PCacheStorageDeleteEntry
pCacheStorageDeleteEntry
  arg_pCacheStorageDeleteEntryCacheId
  arg_pCacheStorageDeleteEntryRequest
  = PCacheStorageDeleteEntry
    arg_pCacheStorageDeleteEntryCacheId
    arg_pCacheStorageDeleteEntryRequest
instance ToJSON PCacheStorageDeleteEntry where
  toJSON p = A.object $ catMaybes [
    ("cacheId" A..=) <$> Just (pCacheStorageDeleteEntryCacheId p),
    ("request" A..=) <$> Just (pCacheStorageDeleteEntryRequest p)
    ]
instance Command PCacheStorageDeleteEntry where
  type CommandResponse PCacheStorageDeleteEntry = ()
  commandName _ = "CacheStorage.deleteEntry"
  fromJSON = const . A.Success . const ()

-- | Requests cache names.

-- | Parameters of the 'CacheStorage.requestCacheNames' command.
data PCacheStorageRequestCacheNames = PCacheStorageRequestCacheNames
  {
    -- | Security origin.
    pCacheStorageRequestCacheNamesSecurityOrigin :: String
  }
  deriving (Eq, Show)
pCacheStorageRequestCacheNames
  -- | Security origin.
  :: String
  -> PCacheStorageRequestCacheNames
pCacheStorageRequestCacheNames
  arg_pCacheStorageRequestCacheNamesSecurityOrigin
  = PCacheStorageRequestCacheNames
    arg_pCacheStorageRequestCacheNamesSecurityOrigin
instance ToJSON PCacheStorageRequestCacheNames where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> Just (pCacheStorageRequestCacheNamesSecurityOrigin p)
    ]
data CacheStorageRequestCacheNames = CacheStorageRequestCacheNames
  {
    -- | Caches for the security origin.
    cacheStorageRequestCacheNamesCaches :: [CacheStorageCache]
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageRequestCacheNames where
  parseJSON = A.withObject "CacheStorageRequestCacheNames" $ \o -> CacheStorageRequestCacheNames
    <$> o A..: "caches"
instance Command PCacheStorageRequestCacheNames where
  type CommandResponse PCacheStorageRequestCacheNames = CacheStorageRequestCacheNames
  commandName _ = "CacheStorage.requestCacheNames"

-- | Fetches cache entry.

-- | Parameters of the 'CacheStorage.requestCachedResponse' command.
data PCacheStorageRequestCachedResponse = PCacheStorageRequestCachedResponse
  {
    -- | Id of cache that contains the entry.
    pCacheStorageRequestCachedResponseCacheId :: CacheStorageCacheId,
    -- | URL spec of the request.
    pCacheStorageRequestCachedResponseRequestURL :: String,
    -- | headers of the request.
    pCacheStorageRequestCachedResponseRequestHeaders :: [CacheStorageHeader]
  }
  deriving (Eq, Show)
pCacheStorageRequestCachedResponse
  -- | Id of cache that contains the entry.
  :: CacheStorageCacheId
  -- | URL spec of the request.
  -> String
  -- | headers of the request.
  -> [CacheStorageHeader]
  -> PCacheStorageRequestCachedResponse
pCacheStorageRequestCachedResponse
  arg_pCacheStorageRequestCachedResponseCacheId
  arg_pCacheStorageRequestCachedResponseRequestURL
  arg_pCacheStorageRequestCachedResponseRequestHeaders
  = PCacheStorageRequestCachedResponse
    arg_pCacheStorageRequestCachedResponseCacheId
    arg_pCacheStorageRequestCachedResponseRequestURL
    arg_pCacheStorageRequestCachedResponseRequestHeaders
instance ToJSON PCacheStorageRequestCachedResponse where
  toJSON p = A.object $ catMaybes [
    ("cacheId" A..=) <$> Just (pCacheStorageRequestCachedResponseCacheId p),
    ("requestURL" A..=) <$> Just (pCacheStorageRequestCachedResponseRequestURL p),
    ("requestHeaders" A..=) <$> Just (pCacheStorageRequestCachedResponseRequestHeaders p)
    ]
data CacheStorageRequestCachedResponse = CacheStorageRequestCachedResponse
  {
    -- | Response read from the cache.
    cacheStorageRequestCachedResponseResponse :: CacheStorageCachedResponse
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageRequestCachedResponse where
  parseJSON = A.withObject "CacheStorageRequestCachedResponse" $ \o -> CacheStorageRequestCachedResponse
    <$> o A..: "response"
instance Command PCacheStorageRequestCachedResponse where
  type CommandResponse PCacheStorageRequestCachedResponse = CacheStorageRequestCachedResponse
  commandName _ = "CacheStorage.requestCachedResponse"

-- | Requests data from cache.

-- | Parameters of the 'CacheStorage.requestEntries' command.
data PCacheStorageRequestEntries = PCacheStorageRequestEntries
  {
    -- | ID of cache to get entries from.
    pCacheStorageRequestEntriesCacheId :: CacheStorageCacheId,
    -- | Number of records to skip.
    pCacheStorageRequestEntriesSkipCount :: Maybe Int,
    -- | Number of records to fetch.
    pCacheStorageRequestEntriesPageSize :: Maybe Int,
    -- | If present, only return the entries containing this substring in the path
    pCacheStorageRequestEntriesPathFilter :: Maybe String
  }
  deriving (Eq, Show)
pCacheStorageRequestEntries
  -- | ID of cache to get entries from.
  :: CacheStorageCacheId
  -> PCacheStorageRequestEntries
pCacheStorageRequestEntries
  arg_pCacheStorageRequestEntriesCacheId
  = PCacheStorageRequestEntries
    arg_pCacheStorageRequestEntriesCacheId
    Nothing
    Nothing
    Nothing
instance ToJSON PCacheStorageRequestEntries where
  toJSON p = A.object $ catMaybes [
    ("cacheId" A..=) <$> Just (pCacheStorageRequestEntriesCacheId p),
    ("skipCount" A..=) <$> (pCacheStorageRequestEntriesSkipCount p),
    ("pageSize" A..=) <$> (pCacheStorageRequestEntriesPageSize p),
    ("pathFilter" A..=) <$> (pCacheStorageRequestEntriesPathFilter p)
    ]
data CacheStorageRequestEntries = CacheStorageRequestEntries
  {
    -- | Array of object store data entries.
    cacheStorageRequestEntriesCacheDataEntries :: [CacheStorageDataEntry],
    -- | Count of returned entries from this storage. If pathFilter is empty, it
    --   is the count of all entries from this storage.
    cacheStorageRequestEntriesReturnCount :: Double
  }
  deriving (Eq, Show)
instance FromJSON CacheStorageRequestEntries where
  parseJSON = A.withObject "CacheStorageRequestEntries" $ \o -> CacheStorageRequestEntries
    <$> o A..: "cacheDataEntries"
    <*> o A..: "returnCount"
instance Command PCacheStorageRequestEntries where
  type CommandResponse PCacheStorageRequestEntries = CacheStorageRequestEntries
  commandName _ = "CacheStorage.requestEntries"

