{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.IndexedDB (module CDP.Domains.IndexedDB) where

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

import CDP.Domains.Runtime as Runtime



data IndexedDbDatabaseWithObjectStores = IndexedDbDatabaseWithObjectStores {
   indexedDbDatabaseWithObjectStoresName :: String,
   indexedDbDatabaseWithObjectStoresVersion :: Double,
   indexedDbDatabaseWithObjectStoresObjectStores :: [IndexedDbObjectStore]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDatabaseWithObjectStores  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDatabaseWithObjectStores where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data IndexedDbObjectStore = IndexedDbObjectStore {
   indexedDbObjectStoreName :: String,
   indexedDbObjectStoreKeyPath :: IndexedDbKeyPath,
   indexedDbObjectStoreAutoIncrement :: Bool,
   indexedDbObjectStoreIndexes :: [IndexedDbObjectStoreIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data IndexedDbObjectStoreIndex = IndexedDbObjectStoreIndex {
   indexedDbObjectStoreIndexName :: String,
   indexedDbObjectStoreIndexKeyPath :: IndexedDbKeyPath,
   indexedDbObjectStoreIndexUnique :: Bool,
   indexedDbObjectStoreIndexMultiEntry :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStoreIndex  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStoreIndex where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


data IndexedDbKeyType = IndexedDbKeyTypeNumber | IndexedDbKeyTypeString | IndexedDbKeyTypeDate | IndexedDbKeyTypeArray
   deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDbKeyType where
   parseJSON = A.withText  "IndexedDbKeyType"  $ \v -> do
      case v of
         "number" -> pure IndexedDbKeyTypeNumber
         "string" -> pure IndexedDbKeyTypeString
         "date" -> pure IndexedDbKeyTypeDate
         "array" -> pure IndexedDbKeyTypeArray
         _ -> fail "failed to parse IndexedDbKeyType"

instance ToJSON IndexedDbKeyType where
   toJSON v = A.String $
      case v of
         IndexedDbKeyTypeNumber -> "number"
         IndexedDbKeyTypeString -> "string"
         IndexedDbKeyTypeDate -> "date"
         IndexedDbKeyTypeArray -> "array"



data IndexedDbKey = IndexedDbKey {
   indexedDbKeyType :: IndexedDbKeyType,
   indexedDbKeyNumber :: Maybe Double,
   indexedDbKeyString :: Maybe String,
   indexedDbKeyDate :: Maybe Double,
   indexedDbKeyArray :: Maybe [IndexedDbKey]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



data IndexedDbKeyRange = IndexedDbKeyRange {
   indexedDbKeyRangeLower :: Maybe IndexedDbKey,
   indexedDbKeyRangeUpper :: Maybe IndexedDbKey,
   indexedDbKeyRangeLowerOpen :: Bool,
   indexedDbKeyRangeUpperOpen :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data IndexedDbDataEntry = IndexedDbDataEntry {
   indexedDbDataEntryKey :: Runtime.RuntimeRemoteObject,
   indexedDbDataEntryPrimaryKey :: Runtime.RuntimeRemoteObject,
   indexedDbDataEntryValue :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


data IndexedDbKeyPathType = IndexedDbKeyPathTypeNull | IndexedDbKeyPathTypeString | IndexedDbKeyPathTypeArray
   deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDbKeyPathType where
   parseJSON = A.withText  "IndexedDbKeyPathType"  $ \v -> do
      case v of
         "null" -> pure IndexedDbKeyPathTypeNull
         "string" -> pure IndexedDbKeyPathTypeString
         "array" -> pure IndexedDbKeyPathTypeArray
         _ -> fail "failed to parse IndexedDbKeyPathType"

instance ToJSON IndexedDbKeyPathType where
   toJSON v = A.String $
      case v of
         IndexedDbKeyPathTypeNull -> "null"
         IndexedDbKeyPathTypeString -> "string"
         IndexedDbKeyPathTypeArray -> "array"



data IndexedDbKeyPath = IndexedDbKeyPath {
   indexedDbKeyPathType :: IndexedDbKeyPathType,
   indexedDbKeyPathString :: Maybe String,
   indexedDbKeyPathArray :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyPath  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyPath where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }







data PIndexedDbClearObjectStore = PIndexedDbClearObjectStore {
   pIndexedDbClearObjectStoreSecurityOrigin :: String,
   pIndexedDbClearObjectStoreDatabaseName :: String,
   pIndexedDbClearObjectStoreObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbClearObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbClearObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


indexedDbClearObjectStore :: Handle ev -> PIndexedDbClearObjectStore -> IO (Maybe Error)
indexedDbClearObjectStore handle params = sendReceiveCommand handle "IndexedDB.clearObjectStore" (Just params)



data PIndexedDbDeleteDatabase = PIndexedDbDeleteDatabase {
   pIndexedDbDeleteDatabaseSecurityOrigin :: String,
   pIndexedDbDeleteDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


indexedDbDeleteDatabase :: Handle ev -> PIndexedDbDeleteDatabase -> IO (Maybe Error)
indexedDbDeleteDatabase handle params = sendReceiveCommand handle "IndexedDB.deleteDatabase" (Just params)



data PIndexedDbDeleteObjectStoreEntries = PIndexedDbDeleteObjectStoreEntries {
   pIndexedDbDeleteObjectStoreEntriesSecurityOrigin :: String,
   pIndexedDbDeleteObjectStoreEntriesDatabaseName :: String,
   pIndexedDbDeleteObjectStoreEntriesObjectStoreName :: String,
   pIndexedDbDeleteObjectStoreEntriesKeyRange :: IndexedDbKeyRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteObjectStoreEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteObjectStoreEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


indexedDbDeleteObjectStoreEntries :: Handle ev -> PIndexedDbDeleteObjectStoreEntries -> IO (Maybe Error)
indexedDbDeleteObjectStoreEntries handle params = sendReceiveCommand handle "IndexedDB.deleteObjectStoreEntries" (Just params)


indexedDbDisable :: Handle ev -> IO (Maybe Error)
indexedDbDisable handle = sendReceiveCommand handle "IndexedDB.disable" (Nothing :: Maybe ())


indexedDbEnable :: Handle ev -> IO (Maybe Error)
indexedDbEnable handle = sendReceiveCommand handle "IndexedDB.enable" (Nothing :: Maybe ())



data PIndexedDbRequestData = PIndexedDbRequestData {
   pIndexedDbRequestDataSecurityOrigin :: String,
   pIndexedDbRequestDataDatabaseName :: String,
   pIndexedDbRequestDataObjectStoreName :: String,
   pIndexedDbRequestDataIndexName :: String,
   pIndexedDbRequestDataSkipCount :: Int,
   pIndexedDbRequestDataPageSize :: Int,
   pIndexedDbRequestDataKeyRange :: Maybe IndexedDbKeyRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


indexedDbRequestData :: Handle ev -> PIndexedDbRequestData -> IO (Either Error IndexedDbRequestData)
indexedDbRequestData handle params = sendReceiveCommandResult handle "IndexedDB.requestData" (Just params)

data IndexedDbRequestData = IndexedDbRequestData {
   indexedDbRequestDataObjectStoreDataEntries :: [IndexedDbDataEntry],
   indexedDbRequestDataHasMore :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbRequestData where
   commandName _ = "IndexedDB.requestData"




data PIndexedDbGetMetadata = PIndexedDbGetMetadata {
   pIndexedDbGetMetadataSecurityOrigin :: String,
   pIndexedDbGetMetadataDatabaseName :: String,
   pIndexedDbGetMetadataObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbGetMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


indexedDbGetMetadata :: Handle ev -> PIndexedDbGetMetadata -> IO (Either Error IndexedDbGetMetadata)
indexedDbGetMetadata handle params = sendReceiveCommandResult handle "IndexedDB.getMetadata" (Just params)

data IndexedDbGetMetadata = IndexedDbGetMetadata {
   indexedDbGetMetadataEntriesCount :: Double,
   indexedDbGetMetadataKeyGeneratorValue :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbGetMetadata where
   commandName _ = "IndexedDB.getMetadata"




data PIndexedDbRequestDatabase = PIndexedDbRequestDatabase {
   pIndexedDbRequestDatabaseSecurityOrigin :: String,
   pIndexedDbRequestDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


indexedDbRequestDatabase :: Handle ev -> PIndexedDbRequestDatabase -> IO (Either Error IndexedDbRequestDatabase)
indexedDbRequestDatabase handle params = sendReceiveCommandResult handle "IndexedDB.requestDatabase" (Just params)

data IndexedDbRequestDatabase = IndexedDbRequestDatabase {
   indexedDbRequestDatabaseDatabaseWithObjectStores :: IndexedDbDatabaseWithObjectStores
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command IndexedDbRequestDatabase where
   commandName _ = "IndexedDB.requestDatabase"




data PIndexedDbRequestDatabaseNames = PIndexedDbRequestDatabaseNames {
   pIndexedDbRequestDatabaseNamesSecurityOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabaseNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


indexedDbRequestDatabaseNames :: Handle ev -> PIndexedDbRequestDatabaseNames -> IO (Either Error IndexedDbRequestDatabaseNames)
indexedDbRequestDatabaseNames handle params = sendReceiveCommandResult handle "IndexedDB.requestDatabaseNames" (Just params)

data IndexedDbRequestDatabaseNames = IndexedDbRequestDatabaseNames {
   indexedDbRequestDatabaseNamesDatabaseNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command IndexedDbRequestDatabaseNames where
   commandName _ = "IndexedDB.requestDatabaseNames"




