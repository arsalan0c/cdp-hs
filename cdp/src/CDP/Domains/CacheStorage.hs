{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



type CacheStorageCacheId = String
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



data CacheStorageDataEntry = CacheStorageDataEntry {
   cacheStorageDataEntryRequestUrl :: String,
   cacheStorageDataEntryRequestMethod :: String,
   cacheStorageDataEntryRequestHeaders :: [CacheStorageHeader],
   cacheStorageDataEntryResponseTime :: Double,
   cacheStorageDataEntryResponseStatus :: Int,
   cacheStorageDataEntryResponseStatusText :: String,
   cacheStorageDataEntryResponseType :: CacheStorageCachedResponseType,
   cacheStorageDataEntryResponseHeaders :: [CacheStorageHeader]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  CacheStorageDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data CacheStorageCache = CacheStorageCache {
   cacheStorageCacheCacheId :: CacheStorageCacheId,
   cacheStorageCacheSecurityOrigin :: String,
   cacheStorageCacheCacheName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  CacheStorageCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data CacheStorageHeader = CacheStorageHeader {
   cacheStorageHeaderName :: String,
   cacheStorageHeaderValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CacheStorageHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data CacheStorageCachedResponse = CacheStorageCachedResponse {
   cacheStorageCachedResponseBody :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CacheStorageCachedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  CacheStorageCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }







data PCacheStorageDeleteCache = PCacheStorageDeleteCache {
   pCacheStorageDeleteCacheCacheId :: CacheStorageCacheId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageDeleteCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageDeleteCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


cacheStorageDeleteCache :: Handle ev -> PCacheStorageDeleteCache -> IO (Maybe Error)
cacheStorageDeleteCache handle params = sendReceiveCommand handle "CacheStorage.deleteCache" (Just params)



data PCacheStorageDeleteEntry = PCacheStorageDeleteEntry {
   pCacheStorageDeleteEntryCacheId :: CacheStorageCacheId,
   pCacheStorageDeleteEntryRequest :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageDeleteEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageDeleteEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


cacheStorageDeleteEntry :: Handle ev -> PCacheStorageDeleteEntry -> IO (Maybe Error)
cacheStorageDeleteEntry handle params = sendReceiveCommand handle "CacheStorage.deleteEntry" (Just params)



data PCacheStorageRequestCacheNames = PCacheStorageRequestCacheNames {
   pCacheStorageRequestCacheNamesSecurityOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestCacheNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestCacheNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


cacheStorageRequestCacheNames :: Handle ev -> PCacheStorageRequestCacheNames -> IO (Either Error CacheStorageRequestCacheNames)
cacheStorageRequestCacheNames handle params = sendReceiveCommandResult handle "CacheStorage.requestCacheNames" (Just params)

data CacheStorageRequestCacheNames = CacheStorageRequestCacheNames {
   cacheStorageRequestCacheNamesCaches :: [CacheStorageCache]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestCacheNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command CacheStorageRequestCacheNames where
   commandName _ = "CacheStorage.requestCacheNames"




data PCacheStorageRequestCachedResponse = PCacheStorageRequestCachedResponse {
   pCacheStorageRequestCachedResponseCacheId :: CacheStorageCacheId,
   pCacheStorageRequestCachedResponseRequestUrl :: String,
   pCacheStorageRequestCachedResponseRequestHeaders :: [CacheStorageHeader]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestCachedResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


cacheStorageRequestCachedResponse :: Handle ev -> PCacheStorageRequestCachedResponse -> IO (Either Error CacheStorageRequestCachedResponse)
cacheStorageRequestCachedResponse handle params = sendReceiveCommandResult handle "CacheStorage.requestCachedResponse" (Just params)

data CacheStorageRequestCachedResponse = CacheStorageRequestCachedResponse {
   cacheStorageRequestCachedResponseResponse :: CacheStorageCachedResponse
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestCachedResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command CacheStorageRequestCachedResponse where
   commandName _ = "CacheStorage.requestCachedResponse"




data PCacheStorageRequestEntries = PCacheStorageRequestEntries {
   pCacheStorageRequestEntriesCacheId :: CacheStorageCacheId,
   pCacheStorageRequestEntriesSkipCount :: Maybe Int,
   pCacheStorageRequestEntriesPageSize :: Maybe Int,
   pCacheStorageRequestEntriesPathFilter :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCacheStorageRequestEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCacheStorageRequestEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


cacheStorageRequestEntries :: Handle ev -> PCacheStorageRequestEntries -> IO (Either Error CacheStorageRequestEntries)
cacheStorageRequestEntries handle params = sendReceiveCommandResult handle "CacheStorage.requestEntries" (Just params)

data CacheStorageRequestEntries = CacheStorageRequestEntries {
   cacheStorageRequestEntriesCacheDataEntries :: [CacheStorageDataEntry],
   cacheStorageRequestEntriesReturnCount :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CacheStorageRequestEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CacheStorageRequestEntries where
   commandName _ = "CacheStorage.requestEntries"




