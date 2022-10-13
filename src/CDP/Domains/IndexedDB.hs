{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= IndexedDB

-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.Runtime as Runtime


-- | Type 'IndexedDB.DatabaseWithObjectStores'.
--   Database with an array of object stores.
data IndexedDBDatabaseWithObjectStores = IndexedDBDatabaseWithObjectStores
  {
    -- | Database name.
    indexedDBDatabaseWithObjectStoresName :: String,
    -- | Database version (type is not 'integer', as the standard
    --   requires the version number to be 'unsigned long long')
    indexedDBDatabaseWithObjectStoresVersion :: Double,
    -- | Object stores in this database.
    indexedDBDatabaseWithObjectStoresObjectStores :: [IndexedDBObjectStore]
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBDatabaseWithObjectStores where
  parseJSON = A.withObject "IndexedDBDatabaseWithObjectStores" $ \o -> IndexedDBDatabaseWithObjectStores
    <$> o A..: "name"
    <*> o A..: "version"
    <*> o A..: "objectStores"
instance ToJSON IndexedDBDatabaseWithObjectStores where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (indexedDBDatabaseWithObjectStoresName p),
    ("version" A..=) <$> Just (indexedDBDatabaseWithObjectStoresVersion p),
    ("objectStores" A..=) <$> Just (indexedDBDatabaseWithObjectStoresObjectStores p)
    ]

-- | Type 'IndexedDB.ObjectStore'.
--   Object store.
data IndexedDBObjectStore = IndexedDBObjectStore
  {
    -- | Object store name.
    indexedDBObjectStoreName :: String,
    -- | Object store key path.
    indexedDBObjectStoreKeyPath :: IndexedDBKeyPath,
    -- | If true, object store has auto increment flag set.
    indexedDBObjectStoreAutoIncrement :: Bool,
    -- | Indexes in this object store.
    indexedDBObjectStoreIndexes :: [IndexedDBObjectStoreIndex]
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBObjectStore where
  parseJSON = A.withObject "IndexedDBObjectStore" $ \o -> IndexedDBObjectStore
    <$> o A..: "name"
    <*> o A..: "keyPath"
    <*> o A..: "autoIncrement"
    <*> o A..: "indexes"
instance ToJSON IndexedDBObjectStore where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (indexedDBObjectStoreName p),
    ("keyPath" A..=) <$> Just (indexedDBObjectStoreKeyPath p),
    ("autoIncrement" A..=) <$> Just (indexedDBObjectStoreAutoIncrement p),
    ("indexes" A..=) <$> Just (indexedDBObjectStoreIndexes p)
    ]

-- | Type 'IndexedDB.ObjectStoreIndex'.
--   Object store index.
data IndexedDBObjectStoreIndex = IndexedDBObjectStoreIndex
  {
    -- | Index name.
    indexedDBObjectStoreIndexName :: String,
    -- | Index key path.
    indexedDBObjectStoreIndexKeyPath :: IndexedDBKeyPath,
    -- | If true, index is unique.
    indexedDBObjectStoreIndexUnique :: Bool,
    -- | If true, index allows multiple entries for a key.
    indexedDBObjectStoreIndexMultiEntry :: Bool
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBObjectStoreIndex where
  parseJSON = A.withObject "IndexedDBObjectStoreIndex" $ \o -> IndexedDBObjectStoreIndex
    <$> o A..: "name"
    <*> o A..: "keyPath"
    <*> o A..: "unique"
    <*> o A..: "multiEntry"
instance ToJSON IndexedDBObjectStoreIndex where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (indexedDBObjectStoreIndexName p),
    ("keyPath" A..=) <$> Just (indexedDBObjectStoreIndexKeyPath p),
    ("unique" A..=) <$> Just (indexedDBObjectStoreIndexUnique p),
    ("multiEntry" A..=) <$> Just (indexedDBObjectStoreIndexMultiEntry p)
    ]

-- | Type 'IndexedDB.Key'.
--   Key.
data IndexedDBKeyType = IndexedDBKeyTypeNumber | IndexedDBKeyTypeString | IndexedDBKeyTypeDate | IndexedDBKeyTypeArray
  deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDBKeyType where
  parseJSON = A.withText "IndexedDBKeyType" $ \v -> case v of
    "number" -> pure IndexedDBKeyTypeNumber
    "string" -> pure IndexedDBKeyTypeString
    "date" -> pure IndexedDBKeyTypeDate
    "array" -> pure IndexedDBKeyTypeArray
    "_" -> fail "failed to parse IndexedDBKeyType"
instance ToJSON IndexedDBKeyType where
  toJSON v = A.String $ case v of
    IndexedDBKeyTypeNumber -> "number"
    IndexedDBKeyTypeString -> "string"
    IndexedDBKeyTypeDate -> "date"
    IndexedDBKeyTypeArray -> "array"
data IndexedDBKey = IndexedDBKey
  {
    -- | Key type.
    indexedDBKeyType :: IndexedDBKeyType,
    -- | Number value.
    indexedDBKeyNumber :: Maybe Double,
    -- | String value.
    indexedDBKeyString :: Maybe String,
    -- | Date value.
    indexedDBKeyDate :: Maybe Double,
    -- | Array value.
    indexedDBKeyArray :: Maybe [IndexedDBKey]
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBKey where
  parseJSON = A.withObject "IndexedDBKey" $ \o -> IndexedDBKey
    <$> o A..: "type"
    <*> o A..:? "number"
    <*> o A..:? "string"
    <*> o A..:? "date"
    <*> o A..:? "array"
instance ToJSON IndexedDBKey where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (indexedDBKeyType p),
    ("number" A..=) <$> (indexedDBKeyNumber p),
    ("string" A..=) <$> (indexedDBKeyString p),
    ("date" A..=) <$> (indexedDBKeyDate p),
    ("array" A..=) <$> (indexedDBKeyArray p)
    ]

-- | Type 'IndexedDB.KeyRange'.
--   Key range.
data IndexedDBKeyRange = IndexedDBKeyRange
  {
    -- | Lower bound.
    indexedDBKeyRangeLower :: Maybe IndexedDBKey,
    -- | Upper bound.
    indexedDBKeyRangeUpper :: Maybe IndexedDBKey,
    -- | If true lower bound is open.
    indexedDBKeyRangeLowerOpen :: Bool,
    -- | If true upper bound is open.
    indexedDBKeyRangeUpperOpen :: Bool
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBKeyRange where
  parseJSON = A.withObject "IndexedDBKeyRange" $ \o -> IndexedDBKeyRange
    <$> o A..:? "lower"
    <*> o A..:? "upper"
    <*> o A..: "lowerOpen"
    <*> o A..: "upperOpen"
instance ToJSON IndexedDBKeyRange where
  toJSON p = A.object $ catMaybes [
    ("lower" A..=) <$> (indexedDBKeyRangeLower p),
    ("upper" A..=) <$> (indexedDBKeyRangeUpper p),
    ("lowerOpen" A..=) <$> Just (indexedDBKeyRangeLowerOpen p),
    ("upperOpen" A..=) <$> Just (indexedDBKeyRangeUpperOpen p)
    ]

-- | Type 'IndexedDB.DataEntry'.
--   Data entry.
data IndexedDBDataEntry = IndexedDBDataEntry
  {
    -- | Key object.
    indexedDBDataEntryKey :: Runtime.RuntimeRemoteObject,
    -- | Primary key object.
    indexedDBDataEntryPrimaryKey :: Runtime.RuntimeRemoteObject,
    -- | Value object.
    indexedDBDataEntryValue :: Runtime.RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBDataEntry where
  parseJSON = A.withObject "IndexedDBDataEntry" $ \o -> IndexedDBDataEntry
    <$> o A..: "key"
    <*> o A..: "primaryKey"
    <*> o A..: "value"
instance ToJSON IndexedDBDataEntry where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> Just (indexedDBDataEntryKey p),
    ("primaryKey" A..=) <$> Just (indexedDBDataEntryPrimaryKey p),
    ("value" A..=) <$> Just (indexedDBDataEntryValue p)
    ]

-- | Type 'IndexedDB.KeyPath'.
--   Key path.
data IndexedDBKeyPathType = IndexedDBKeyPathTypeNull | IndexedDBKeyPathTypeString | IndexedDBKeyPathTypeArray
  deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDBKeyPathType where
  parseJSON = A.withText "IndexedDBKeyPathType" $ \v -> case v of
    "null" -> pure IndexedDBKeyPathTypeNull
    "string" -> pure IndexedDBKeyPathTypeString
    "array" -> pure IndexedDBKeyPathTypeArray
    "_" -> fail "failed to parse IndexedDBKeyPathType"
instance ToJSON IndexedDBKeyPathType where
  toJSON v = A.String $ case v of
    IndexedDBKeyPathTypeNull -> "null"
    IndexedDBKeyPathTypeString -> "string"
    IndexedDBKeyPathTypeArray -> "array"
data IndexedDBKeyPath = IndexedDBKeyPath
  {
    -- | Key path type.
    indexedDBKeyPathType :: IndexedDBKeyPathType,
    -- | String value.
    indexedDBKeyPathString :: Maybe String,
    -- | Array value.
    indexedDBKeyPathArray :: Maybe [String]
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBKeyPath where
  parseJSON = A.withObject "IndexedDBKeyPath" $ \o -> IndexedDBKeyPath
    <$> o A..: "type"
    <*> o A..:? "string"
    <*> o A..:? "array"
instance ToJSON IndexedDBKeyPath where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (indexedDBKeyPathType p),
    ("string" A..=) <$> (indexedDBKeyPathString p),
    ("array" A..=) <$> (indexedDBKeyPathArray p)
    ]

-- | Clears all entries from an object store.

-- | Parameters of the 'IndexedDB.clearObjectStore' command.
data PIndexedDBClearObjectStore = PIndexedDBClearObjectStore
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBClearObjectStoreSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBClearObjectStoreStorageKey :: Maybe String,
    -- | Database name.
    pIndexedDBClearObjectStoreDatabaseName :: String,
    -- | Object store name.
    pIndexedDBClearObjectStoreObjectStoreName :: String
  }
  deriving (Eq, Show)
pIndexedDBClearObjectStore
  -- | Database name.
  :: String
  -- | Object store name.
  -> String
  -> PIndexedDBClearObjectStore
pIndexedDBClearObjectStore
  arg_pIndexedDBClearObjectStoreDatabaseName
  arg_pIndexedDBClearObjectStoreObjectStoreName
  = PIndexedDBClearObjectStore
    Nothing
    Nothing
    arg_pIndexedDBClearObjectStoreDatabaseName
    arg_pIndexedDBClearObjectStoreObjectStoreName
instance ToJSON PIndexedDBClearObjectStore where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBClearObjectStoreSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBClearObjectStoreStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBClearObjectStoreDatabaseName p),
    ("objectStoreName" A..=) <$> Just (pIndexedDBClearObjectStoreObjectStoreName p)
    ]
instance Command PIndexedDBClearObjectStore where
  type CommandResponse PIndexedDBClearObjectStore = ()
  commandName _ = "IndexedDB.clearObjectStore"
  fromJSON = const . A.Success . const ()

-- | Deletes a database.

-- | Parameters of the 'IndexedDB.deleteDatabase' command.
data PIndexedDBDeleteDatabase = PIndexedDBDeleteDatabase
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBDeleteDatabaseSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBDeleteDatabaseStorageKey :: Maybe String,
    -- | Database name.
    pIndexedDBDeleteDatabaseDatabaseName :: String
  }
  deriving (Eq, Show)
pIndexedDBDeleteDatabase
  -- | Database name.
  :: String
  -> PIndexedDBDeleteDatabase
pIndexedDBDeleteDatabase
  arg_pIndexedDBDeleteDatabaseDatabaseName
  = PIndexedDBDeleteDatabase
    Nothing
    Nothing
    arg_pIndexedDBDeleteDatabaseDatabaseName
instance ToJSON PIndexedDBDeleteDatabase where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBDeleteDatabaseSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBDeleteDatabaseStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBDeleteDatabaseDatabaseName p)
    ]
instance Command PIndexedDBDeleteDatabase where
  type CommandResponse PIndexedDBDeleteDatabase = ()
  commandName _ = "IndexedDB.deleteDatabase"
  fromJSON = const . A.Success . const ()

-- | Delete a range of entries from an object store

-- | Parameters of the 'IndexedDB.deleteObjectStoreEntries' command.
data PIndexedDBDeleteObjectStoreEntries = PIndexedDBDeleteObjectStoreEntries
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBDeleteObjectStoreEntriesSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBDeleteObjectStoreEntriesStorageKey :: Maybe String,
    pIndexedDBDeleteObjectStoreEntriesDatabaseName :: String,
    pIndexedDBDeleteObjectStoreEntriesObjectStoreName :: String,
    -- | Range of entry keys to delete
    pIndexedDBDeleteObjectStoreEntriesKeyRange :: IndexedDBKeyRange
  }
  deriving (Eq, Show)
pIndexedDBDeleteObjectStoreEntries
  :: String
  -> String
  -- | Range of entry keys to delete
  -> IndexedDBKeyRange
  -> PIndexedDBDeleteObjectStoreEntries
pIndexedDBDeleteObjectStoreEntries
  arg_pIndexedDBDeleteObjectStoreEntriesDatabaseName
  arg_pIndexedDBDeleteObjectStoreEntriesObjectStoreName
  arg_pIndexedDBDeleteObjectStoreEntriesKeyRange
  = PIndexedDBDeleteObjectStoreEntries
    Nothing
    Nothing
    arg_pIndexedDBDeleteObjectStoreEntriesDatabaseName
    arg_pIndexedDBDeleteObjectStoreEntriesObjectStoreName
    arg_pIndexedDBDeleteObjectStoreEntriesKeyRange
instance ToJSON PIndexedDBDeleteObjectStoreEntries where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBDeleteObjectStoreEntriesSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBDeleteObjectStoreEntriesStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBDeleteObjectStoreEntriesDatabaseName p),
    ("objectStoreName" A..=) <$> Just (pIndexedDBDeleteObjectStoreEntriesObjectStoreName p),
    ("keyRange" A..=) <$> Just (pIndexedDBDeleteObjectStoreEntriesKeyRange p)
    ]
instance Command PIndexedDBDeleteObjectStoreEntries where
  type CommandResponse PIndexedDBDeleteObjectStoreEntries = ()
  commandName _ = "IndexedDB.deleteObjectStoreEntries"
  fromJSON = const . A.Success . const ()

-- | Disables events from backend.

-- | Parameters of the 'IndexedDB.disable' command.
data PIndexedDBDisable = PIndexedDBDisable
  deriving (Eq, Show)
pIndexedDBDisable
  :: PIndexedDBDisable
pIndexedDBDisable
  = PIndexedDBDisable
instance ToJSON PIndexedDBDisable where
  toJSON _ = A.Null
instance Command PIndexedDBDisable where
  type CommandResponse PIndexedDBDisable = ()
  commandName _ = "IndexedDB.disable"
  fromJSON = const . A.Success . const ()

-- | Enables events from backend.

-- | Parameters of the 'IndexedDB.enable' command.
data PIndexedDBEnable = PIndexedDBEnable
  deriving (Eq, Show)
pIndexedDBEnable
  :: PIndexedDBEnable
pIndexedDBEnable
  = PIndexedDBEnable
instance ToJSON PIndexedDBEnable where
  toJSON _ = A.Null
instance Command PIndexedDBEnable where
  type CommandResponse PIndexedDBEnable = ()
  commandName _ = "IndexedDB.enable"
  fromJSON = const . A.Success . const ()

-- | Requests data from object store or index.

-- | Parameters of the 'IndexedDB.requestData' command.
data PIndexedDBRequestData = PIndexedDBRequestData
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBRequestDataSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBRequestDataStorageKey :: Maybe String,
    -- | Database name.
    pIndexedDBRequestDataDatabaseName :: String,
    -- | Object store name.
    pIndexedDBRequestDataObjectStoreName :: String,
    -- | Index name, empty string for object store data requests.
    pIndexedDBRequestDataIndexName :: String,
    -- | Number of records to skip.
    pIndexedDBRequestDataSkipCount :: Int,
    -- | Number of records to fetch.
    pIndexedDBRequestDataPageSize :: Int,
    -- | Key range.
    pIndexedDBRequestDataKeyRange :: Maybe IndexedDBKeyRange
  }
  deriving (Eq, Show)
pIndexedDBRequestData
  -- | Database name.
  :: String
  -- | Object store name.
  -> String
  -- | Index name, empty string for object store data requests.
  -> String
  -- | Number of records to skip.
  -> Int
  -- | Number of records to fetch.
  -> Int
  -> PIndexedDBRequestData
pIndexedDBRequestData
  arg_pIndexedDBRequestDataDatabaseName
  arg_pIndexedDBRequestDataObjectStoreName
  arg_pIndexedDBRequestDataIndexName
  arg_pIndexedDBRequestDataSkipCount
  arg_pIndexedDBRequestDataPageSize
  = PIndexedDBRequestData
    Nothing
    Nothing
    arg_pIndexedDBRequestDataDatabaseName
    arg_pIndexedDBRequestDataObjectStoreName
    arg_pIndexedDBRequestDataIndexName
    arg_pIndexedDBRequestDataSkipCount
    arg_pIndexedDBRequestDataPageSize
    Nothing
instance ToJSON PIndexedDBRequestData where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBRequestDataSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBRequestDataStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBRequestDataDatabaseName p),
    ("objectStoreName" A..=) <$> Just (pIndexedDBRequestDataObjectStoreName p),
    ("indexName" A..=) <$> Just (pIndexedDBRequestDataIndexName p),
    ("skipCount" A..=) <$> Just (pIndexedDBRequestDataSkipCount p),
    ("pageSize" A..=) <$> Just (pIndexedDBRequestDataPageSize p),
    ("keyRange" A..=) <$> (pIndexedDBRequestDataKeyRange p)
    ]
data IndexedDBRequestData = IndexedDBRequestData
  {
    -- | Array of object store data entries.
    indexedDBRequestDataObjectStoreDataEntries :: [IndexedDBDataEntry],
    -- | If true, there are more entries to fetch in the given range.
    indexedDBRequestDataHasMore :: Bool
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBRequestData where
  parseJSON = A.withObject "IndexedDBRequestData" $ \o -> IndexedDBRequestData
    <$> o A..: "objectStoreDataEntries"
    <*> o A..: "hasMore"
instance Command PIndexedDBRequestData where
  type CommandResponse PIndexedDBRequestData = IndexedDBRequestData
  commandName _ = "IndexedDB.requestData"

-- | Gets metadata of an object store

-- | Parameters of the 'IndexedDB.getMetadata' command.
data PIndexedDBGetMetadata = PIndexedDBGetMetadata
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBGetMetadataSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBGetMetadataStorageKey :: Maybe String,
    -- | Database name.
    pIndexedDBGetMetadataDatabaseName :: String,
    -- | Object store name.
    pIndexedDBGetMetadataObjectStoreName :: String
  }
  deriving (Eq, Show)
pIndexedDBGetMetadata
  -- | Database name.
  :: String
  -- | Object store name.
  -> String
  -> PIndexedDBGetMetadata
pIndexedDBGetMetadata
  arg_pIndexedDBGetMetadataDatabaseName
  arg_pIndexedDBGetMetadataObjectStoreName
  = PIndexedDBGetMetadata
    Nothing
    Nothing
    arg_pIndexedDBGetMetadataDatabaseName
    arg_pIndexedDBGetMetadataObjectStoreName
instance ToJSON PIndexedDBGetMetadata where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBGetMetadataSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBGetMetadataStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBGetMetadataDatabaseName p),
    ("objectStoreName" A..=) <$> Just (pIndexedDBGetMetadataObjectStoreName p)
    ]
data IndexedDBGetMetadata = IndexedDBGetMetadata
  {
    -- | the entries count
    indexedDBGetMetadataEntriesCount :: Double,
    -- | the current value of key generator, to become the next inserted
    --   key into the object store. Valid if objectStore.autoIncrement
    --   is true.
    indexedDBGetMetadataKeyGeneratorValue :: Double
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBGetMetadata where
  parseJSON = A.withObject "IndexedDBGetMetadata" $ \o -> IndexedDBGetMetadata
    <$> o A..: "entriesCount"
    <*> o A..: "keyGeneratorValue"
instance Command PIndexedDBGetMetadata where
  type CommandResponse PIndexedDBGetMetadata = IndexedDBGetMetadata
  commandName _ = "IndexedDB.getMetadata"

-- | Requests database with given name in given frame.

-- | Parameters of the 'IndexedDB.requestDatabase' command.
data PIndexedDBRequestDatabase = PIndexedDBRequestDatabase
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBRequestDatabaseSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBRequestDatabaseStorageKey :: Maybe String,
    -- | Database name.
    pIndexedDBRequestDatabaseDatabaseName :: String
  }
  deriving (Eq, Show)
pIndexedDBRequestDatabase
  -- | Database name.
  :: String
  -> PIndexedDBRequestDatabase
pIndexedDBRequestDatabase
  arg_pIndexedDBRequestDatabaseDatabaseName
  = PIndexedDBRequestDatabase
    Nothing
    Nothing
    arg_pIndexedDBRequestDatabaseDatabaseName
instance ToJSON PIndexedDBRequestDatabase where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBRequestDatabaseSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBRequestDatabaseStorageKey p),
    ("databaseName" A..=) <$> Just (pIndexedDBRequestDatabaseDatabaseName p)
    ]
data IndexedDBRequestDatabase = IndexedDBRequestDatabase
  {
    -- | Database with an array of object stores.
    indexedDBRequestDatabaseDatabaseWithObjectStores :: IndexedDBDatabaseWithObjectStores
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBRequestDatabase where
  parseJSON = A.withObject "IndexedDBRequestDatabase" $ \o -> IndexedDBRequestDatabase
    <$> o A..: "databaseWithObjectStores"
instance Command PIndexedDBRequestDatabase where
  type CommandResponse PIndexedDBRequestDatabase = IndexedDBRequestDatabase
  commandName _ = "IndexedDB.requestDatabase"

-- | Requests database names for given security origin.

-- | Parameters of the 'IndexedDB.requestDatabaseNames' command.
data PIndexedDBRequestDatabaseNames = PIndexedDBRequestDatabaseNames
  {
    -- | At least and at most one of securityOrigin, storageKey must be specified.
    --   Security origin.
    pIndexedDBRequestDatabaseNamesSecurityOrigin :: Maybe String,
    -- | Storage key.
    pIndexedDBRequestDatabaseNamesStorageKey :: Maybe String
  }
  deriving (Eq, Show)
pIndexedDBRequestDatabaseNames
  :: PIndexedDBRequestDatabaseNames
pIndexedDBRequestDatabaseNames
  = PIndexedDBRequestDatabaseNames
    Nothing
    Nothing
instance ToJSON PIndexedDBRequestDatabaseNames where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (pIndexedDBRequestDatabaseNamesSecurityOrigin p),
    ("storageKey" A..=) <$> (pIndexedDBRequestDatabaseNamesStorageKey p)
    ]
data IndexedDBRequestDatabaseNames = IndexedDBRequestDatabaseNames
  {
    -- | Database names for origin.
    indexedDBRequestDatabaseNamesDatabaseNames :: [String]
  }
  deriving (Eq, Show)
instance FromJSON IndexedDBRequestDatabaseNames where
  parseJSON = A.withObject "IndexedDBRequestDatabaseNames" $ \o -> IndexedDBRequestDatabaseNames
    <$> o A..: "databaseNames"
instance Command PIndexedDBRequestDatabaseNames where
  type CommandResponse PIndexedDBRequestDatabaseNames = IndexedDBRequestDatabaseNames
  commandName _ = "IndexedDB.requestDatabaseNames"

