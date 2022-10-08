{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  CacheStorage 
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




-- | Type 'CacheStorage.CacheId'.
--   Unique identifier of the Cache object.
type CacheStorageCacheId = String

-- | Type 'CacheStorage.CachedResponseType'.
--   type of HTTP response cached
data CacheStorageCachedResponseType = CacheStorageCachedResponseTypeBasic | CacheStorageCachedResponseTypeCors | CacheStorageCachedResponseTypeDefault | CacheStorageCachedResponseTypeError | CacheStorageCachedResponseTypeOpaqueResponse | CacheStorageCachedResponseTypeOpaqueRedirect
   deriving (Ord, Eq, Show, Read)
instance FromJSON CacheStorageCachedResponseType where
   parseJSON = A.withText  "CacheStorageCachedResponseType"  $ \v -> do
      case v of
         "basic" -> pure CacheStorageCachedResponseTypeBasic
         "cors" -> pure CacheStorageCachedResponseTypeCors
         "default" -> pure CacheStorageCachedResponseTypeDefault
         "error" -> pure CacheStorageCachedResponseTypeError
         "opaqueResponse" -> pure CacheStorageCachedResponseTypeOpaqueResponse
         "opaqueRedirect" -> pure CacheStorageCachedResponseTypeOpaqueRedirect
         _ -> fail "failed to parse CacheStorageCachedResponseType"

instance ToJSON CacheStorageCachedResponseType where
   toJSON v = A.String $
      case v of
         CacheStorageCachedResponseTypeBasic -> "basic"
         CacheStorageCachedResponseTypeCors -> "cors"
         CacheStorageCachedResponseTypeDefault -> "default"
         CacheStorageCachedResponseTypeError -> "error"
         CacheStorageCachedResponseTypeOpaqueResponse -> "opaqueResponse"
         CacheStorageCachedResponseTypeOpaqueRedirect -> "opaqueRedirect"



-- | Type 'CacheStorage.DataEntry'.
--   Data entry.
data CacheStorageDataEntry = CacheStorageDataEntry {
  -- | Request URL.
  cacheStorageDataEntryRequestUrl :: String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  CacheStorageDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'CacheStorage.Cache'.
--   Cache identifier.
data CacheStorageCache = CacheStorageCache {
  -- | An opaque unique id of the cache.
  cacheStorageCacheCacheId :: CacheStorageCacheId,
  -- | Security origin of the cache.
  cacheStorageCacheSecurityOrigin :: String,
  -- | The name of the cache.
  cacheStorageCacheCacheName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  CacheStorageCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'CacheStorage.Header'.
data CacheStorageHeader = CacheStorageHeader {
  cacheStorageHeaderName :: String,
  cacheStorageHeaderValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CacheStorageHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'CacheStorage.CachedResponse'.
--   Cached response
data CacheStorageCachedResponse = CacheStorageCachedResponse {
  -- | Entry content, base64-encoded. (Encoded as a base64 string when passed over JSON)
  cacheStorageCachedResponseBody :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageCachedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  CacheStorageCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }







-- | Parameters of the 'cacheStorageDeleteCache' command.
data PCacheStorageDeleteCache = PCacheStorageDeleteCache {
  -- | Id of cache for deletion.
  pCacheStorageDeleteCacheCacheId :: CacheStorageCacheId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageDeleteCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageDeleteCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'CacheStorage.deleteCache' command.
--   Deletes a cache.
--   Parameters: 'PCacheStorageDeleteCache'
cacheStorageDeleteCache :: Handle ev -> PCacheStorageDeleteCache -> IO ()
cacheStorageDeleteCache handle params = sendReceiveCommand handle "CacheStorage.deleteCache" (Just params)


-- | Parameters of the 'cacheStorageDeleteEntry' command.
data PCacheStorageDeleteEntry = PCacheStorageDeleteEntry {
  -- | Id of cache where the entry will be deleted.
  pCacheStorageDeleteEntryCacheId :: CacheStorageCacheId,
  -- | URL spec of the request.
  pCacheStorageDeleteEntryRequest :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageDeleteEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageDeleteEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'CacheStorage.deleteEntry' command.
--   Deletes a cache entry.
--   Parameters: 'PCacheStorageDeleteEntry'
cacheStorageDeleteEntry :: Handle ev -> PCacheStorageDeleteEntry -> IO ()
cacheStorageDeleteEntry handle params = sendReceiveCommand handle "CacheStorage.deleteEntry" (Just params)


-- | Parameters of the 'cacheStorageRequestCacheNames' command.
data PCacheStorageRequestCacheNames = PCacheStorageRequestCacheNames {
  -- | Security origin.
  pCacheStorageRequestCacheNamesSecurityOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestCacheNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestCacheNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'CacheStorage.requestCacheNames' command.
--   Requests cache names.
--   Parameters: 'PCacheStorageRequestCacheNames'
--   Returns: 'CacheStorageRequestCacheNames'
cacheStorageRequestCacheNames :: Handle ev -> PCacheStorageRequestCacheNames -> IO CacheStorageRequestCacheNames
cacheStorageRequestCacheNames handle params = sendReceiveCommandResult handle "CacheStorage.requestCacheNames" (Just params)

-- | Return type of the 'cacheStorageRequestCacheNames' command.
data CacheStorageRequestCacheNames = CacheStorageRequestCacheNames {
  -- | Caches for the security origin.
  cacheStorageRequestCacheNamesCaches :: [CacheStorageCache]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestCacheNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command CacheStorageRequestCacheNames where
   commandName _ = "CacheStorage.requestCacheNames"



-- | Parameters of the 'cacheStorageRequestCachedResponse' command.
data PCacheStorageRequestCachedResponse = PCacheStorageRequestCachedResponse {
  -- | Id of cache that contains the entry.
  pCacheStorageRequestCachedResponseCacheId :: CacheStorageCacheId,
  -- | URL spec of the request.
  pCacheStorageRequestCachedResponseRequestUrl :: String,
  -- | headers of the request.
  pCacheStorageRequestCachedResponseRequestHeaders :: [CacheStorageHeader]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestCachedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'CacheStorage.requestCachedResponse' command.
--   Fetches cache entry.
--   Parameters: 'PCacheStorageRequestCachedResponse'
--   Returns: 'CacheStorageRequestCachedResponse'
cacheStorageRequestCachedResponse :: Handle ev -> PCacheStorageRequestCachedResponse -> IO CacheStorageRequestCachedResponse
cacheStorageRequestCachedResponse handle params = sendReceiveCommandResult handle "CacheStorage.requestCachedResponse" (Just params)

-- | Return type of the 'cacheStorageRequestCachedResponse' command.
data CacheStorageRequestCachedResponse = CacheStorageRequestCachedResponse {
  -- | Response read from the cache.
  cacheStorageRequestCachedResponseResponse :: CacheStorageCachedResponse
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command CacheStorageRequestCachedResponse where
   commandName _ = "CacheStorage.requestCachedResponse"



-- | Parameters of the 'cacheStorageRequestEntries' command.
data PCacheStorageRequestEntries = PCacheStorageRequestEntries {
  -- | ID of cache to get entries from.
  pCacheStorageRequestEntriesCacheId :: CacheStorageCacheId,
  -- | Number of records to skip.
  pCacheStorageRequestEntriesSkipCount :: Maybe Int,
  -- | Number of records to fetch.
  pCacheStorageRequestEntriesPageSize :: Maybe Int,
  -- | If present, only return the entries containing this substring in the path
  pCacheStorageRequestEntriesPathFilter :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'CacheStorage.requestEntries' command.
--   Requests data from cache.
--   Parameters: 'PCacheStorageRequestEntries'
--   Returns: 'CacheStorageRequestEntries'
cacheStorageRequestEntries :: Handle ev -> PCacheStorageRequestEntries -> IO CacheStorageRequestEntries
cacheStorageRequestEntries handle params = sendReceiveCommandResult handle "CacheStorage.requestEntries" (Just params)

-- | Return type of the 'cacheStorageRequestEntries' command.
data CacheStorageRequestEntries = CacheStorageRequestEntries {
  -- | Array of object store data entries.
  cacheStorageRequestEntriesCacheDataEntries :: [CacheStorageDataEntry],
  -- | Count of returned entries from this storage. If pathFilter is empty, it
  --   is the count of all entries from this storage.
  cacheStorageRequestEntriesReturnCount :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CacheStorageRequestEntries where
   commandName _ = "CacheStorage.requestEntries"




