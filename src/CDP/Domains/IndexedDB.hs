{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  IndexedDB 
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
data IndexedDBDatabaseWithObjectStores = IndexedDBDatabaseWithObjectStores {
  -- | Database name.
  indexedDBDatabaseWithObjectStoresName :: String,
  -- | Database version (type is not 'integer', as the standard
  --   requires the version number to be 'unsigned long long')
  indexedDBDatabaseWithObjectStoresVersion :: Double,
  -- | Object stores in this database.
  indexedDBDatabaseWithObjectStoresObjectStores :: [IndexedDBObjectStore]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBDatabaseWithObjectStores  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  IndexedDBDatabaseWithObjectStores where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'IndexedDB.ObjectStore'.
--   Object store.
data IndexedDBObjectStore = IndexedDBObjectStore {
  -- | Object store name.
  indexedDBObjectStoreName :: String,
  -- | Object store key path.
  indexedDBObjectStoreKeyPath :: IndexedDBKeyPath,
  -- | If true, object store has auto increment flag set.
  indexedDBObjectStoreAutoIncrement :: Bool,
  -- | Indexes in this object store.
  indexedDBObjectStoreIndexes :: [IndexedDBObjectStoreIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  IndexedDBObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'IndexedDB.ObjectStoreIndex'.
--   Object store index.
data IndexedDBObjectStoreIndex = IndexedDBObjectStoreIndex {
  -- | Index name.
  indexedDBObjectStoreIndexName :: String,
  -- | Index key path.
  indexedDBObjectStoreIndexKeyPath :: IndexedDBKeyPath,
  -- | If true, index is unique.
  indexedDBObjectStoreIndexUnique :: Bool,
  -- | If true, index allows multiple entries for a key.
  indexedDBObjectStoreIndexMultiEntry :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBObjectStoreIndex  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  IndexedDBObjectStoreIndex where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'IndexedDB.Key'.
--   Key.
data IndexedDBKeyType = IndexedDBKeyTypeNumber | IndexedDBKeyTypeString | IndexedDBKeyTypeDate | IndexedDBKeyTypeArray
   deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDBKeyType where
   parseJSON = A.withText  "IndexedDBKeyType"  $ \v -> do
      case v of
         "number" -> pure IndexedDBKeyTypeNumber
         "string" -> pure IndexedDBKeyTypeString
         "date" -> pure IndexedDBKeyTypeDate
         "array" -> pure IndexedDBKeyTypeArray
         _ -> fail "failed to parse IndexedDBKeyType"

instance ToJSON IndexedDBKeyType where
   toJSON v = A.String $
      case v of
         IndexedDBKeyTypeNumber -> "number"
         IndexedDBKeyTypeString -> "string"
         IndexedDBKeyTypeDate -> "date"
         IndexedDBKeyTypeArray -> "array"



data IndexedDBKey = IndexedDBKey {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  IndexedDBKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Type 'IndexedDB.KeyRange'.
--   Key range.
data IndexedDBKeyRange = IndexedDBKeyRange {
  -- | Lower bound.
  indexedDBKeyRangeLower :: Maybe IndexedDBKey,
  -- | Upper bound.
  indexedDBKeyRangeUpper :: Maybe IndexedDBKey,
  -- | If true lower bound is open.
  indexedDBKeyRangeLowerOpen :: Bool,
  -- | If true upper bound is open.
  indexedDBKeyRangeUpperOpen :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBKeyRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  IndexedDBKeyRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'IndexedDB.DataEntry'.
--   Data entry.
data IndexedDBDataEntry = IndexedDBDataEntry {
  -- | Key object.
  indexedDBDataEntryKey :: Runtime.RuntimeRemoteObject,
  -- | Primary key object.
  indexedDBDataEntryPrimaryKey :: Runtime.RuntimeRemoteObject,
  -- | Value object.
  indexedDBDataEntryValue :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  IndexedDBDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'IndexedDB.KeyPath'.
--   Key path.
data IndexedDBKeyPathType = IndexedDBKeyPathTypeNull | IndexedDBKeyPathTypeString | IndexedDBKeyPathTypeArray
   deriving (Ord, Eq, Show, Read)
instance FromJSON IndexedDBKeyPathType where
   parseJSON = A.withText  "IndexedDBKeyPathType"  $ \v -> do
      case v of
         "null" -> pure IndexedDBKeyPathTypeNull
         "string" -> pure IndexedDBKeyPathTypeString
         "array" -> pure IndexedDBKeyPathTypeArray
         _ -> fail "failed to parse IndexedDBKeyPathType"

instance ToJSON IndexedDBKeyPathType where
   toJSON v = A.String $
      case v of
         IndexedDBKeyPathTypeNull -> "null"
         IndexedDBKeyPathTypeString -> "string"
         IndexedDBKeyPathTypeArray -> "array"



data IndexedDBKeyPath = IndexedDBKeyPath {
  -- | Key path type.
  indexedDBKeyPathType :: IndexedDBKeyPathType,
  -- | String value.
  indexedDBKeyPathString :: Maybe String,
  -- | Array value.
  indexedDBKeyPathArray :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDBKeyPath  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  IndexedDBKeyPath where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }







-- | IndexedDB.clearObjectStore
--   Clears all entries from an object store.

-- | Parameters of the 'IndexedDB.clearObjectStore' command.
data PIndexedDBClearObjectStore = PIndexedDBClearObjectStore {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBClearObjectStoreSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBClearObjectStoreStorageKey :: Maybe String,
  -- | Database name.
  pIndexedDBClearObjectStoreDatabaseName :: String,
  -- | Object store name.
  pIndexedDBClearObjectStoreObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBClearObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBClearObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PIndexedDBClearObjectStore where
   type CommandResponse PIndexedDBClearObjectStore = ()
   commandName _ = "IndexedDB.clearObjectStore"
   fromJSON = const . A.Success . const ()


-- | IndexedDB.deleteDatabase
--   Deletes a database.

-- | Parameters of the 'IndexedDB.deleteDatabase' command.
data PIndexedDBDeleteDatabase = PIndexedDBDeleteDatabase {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBDeleteDatabaseSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBDeleteDatabaseStorageKey :: Maybe String,
  -- | Database name.
  pIndexedDBDeleteDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBDeleteDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBDeleteDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PIndexedDBDeleteDatabase where
   type CommandResponse PIndexedDBDeleteDatabase = ()
   commandName _ = "IndexedDB.deleteDatabase"
   fromJSON = const . A.Success . const ()


-- | IndexedDB.deleteObjectStoreEntries
--   Delete a range of entries from an object store

-- | Parameters of the 'IndexedDB.deleteObjectStoreEntries' command.
data PIndexedDBDeleteObjectStoreEntries = PIndexedDBDeleteObjectStoreEntries {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBDeleteObjectStoreEntriesSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBDeleteObjectStoreEntriesStorageKey :: Maybe String,
  pIndexedDBDeleteObjectStoreEntriesDatabaseName :: String,
  pIndexedDBDeleteObjectStoreEntriesObjectStoreName :: String,
  -- | Range of entry keys to delete
  pIndexedDBDeleteObjectStoreEntriesKeyRange :: IndexedDBKeyRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBDeleteObjectStoreEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBDeleteObjectStoreEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Command PIndexedDBDeleteObjectStoreEntries where
   type CommandResponse PIndexedDBDeleteObjectStoreEntries = ()
   commandName _ = "IndexedDB.deleteObjectStoreEntries"
   fromJSON = const . A.Success . const ()


-- | IndexedDB.disable
--   Disables events from backend.

-- | Parameters of the 'IndexedDB.disable' command.
data PIndexedDBDisable = PIndexedDBDisable
instance ToJSON PIndexedDBDisable where toJSON _ = A.Null

instance Command PIndexedDBDisable where
   type CommandResponse PIndexedDBDisable = ()
   commandName _ = "IndexedDB.disable"
   fromJSON = const . A.Success . const ()


-- | IndexedDB.enable
--   Enables events from backend.

-- | Parameters of the 'IndexedDB.enable' command.
data PIndexedDBEnable = PIndexedDBEnable
instance ToJSON PIndexedDBEnable where toJSON _ = A.Null

instance Command PIndexedDBEnable where
   type CommandResponse PIndexedDBEnable = ()
   commandName _ = "IndexedDB.enable"
   fromJSON = const . A.Success . const ()


-- | IndexedDB.requestData
--   Requests data from object store or index.

-- | Parameters of the 'IndexedDB.requestData' command.
data PIndexedDBRequestData = PIndexedDBRequestData {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBRequestData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Return type of the 'IndexedDB.requestData' command.
data IndexedDBRequestData = IndexedDBRequestData {
  -- | Array of object store data entries.
  indexedDBRequestDataObjectStoreDataEntries :: [IndexedDBDataEntry],
  -- | If true, there are more entries to fetch in the given range.
  indexedDBRequestDataHasMore :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDBRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PIndexedDBRequestData where
   type CommandResponse PIndexedDBRequestData = IndexedDBRequestData
   commandName _ = "IndexedDB.requestData"



-- | IndexedDB.getMetadata
--   Gets metadata of an object store

-- | Parameters of the 'IndexedDB.getMetadata' command.
data PIndexedDBGetMetadata = PIndexedDBGetMetadata {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBGetMetadataSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBGetMetadataStorageKey :: Maybe String,
  -- | Database name.
  pIndexedDBGetMetadataDatabaseName :: String,
  -- | Object store name.
  pIndexedDBGetMetadataObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBGetMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Return type of the 'IndexedDB.getMetadata' command.
data IndexedDBGetMetadata = IndexedDBGetMetadata {
  -- | the entries count
  indexedDBGetMetadataEntriesCount :: Double,
  -- | the current value of key generator, to become the next inserted
  --   key into the object store. Valid if objectStore.autoIncrement
  --   is true.
  indexedDBGetMetadataKeyGeneratorValue :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDBGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PIndexedDBGetMetadata where
   type CommandResponse PIndexedDBGetMetadata = IndexedDBGetMetadata
   commandName _ = "IndexedDB.getMetadata"



-- | IndexedDB.requestDatabase
--   Requests database with given name in given frame.

-- | Parameters of the 'IndexedDB.requestDatabase' command.
data PIndexedDBRequestDatabase = PIndexedDBRequestDatabase {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBRequestDatabaseSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBRequestDatabaseStorageKey :: Maybe String,
  -- | Database name.
  pIndexedDBRequestDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBRequestDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Return type of the 'IndexedDB.requestDatabase' command.
data IndexedDBRequestDatabase = IndexedDBRequestDatabase {
  -- | Database with an array of object stores.
  indexedDBRequestDatabaseDatabaseWithObjectStores :: IndexedDBDatabaseWithObjectStores
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDBRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PIndexedDBRequestDatabase where
   type CommandResponse PIndexedDBRequestDatabase = IndexedDBRequestDatabase
   commandName _ = "IndexedDB.requestDatabase"



-- | IndexedDB.requestDatabaseNames
--   Requests database names for given security origin.

-- | Parameters of the 'IndexedDB.requestDatabaseNames' command.
data PIndexedDBRequestDatabaseNames = PIndexedDBRequestDatabaseNames {
  -- | At least and at most one of securityOrigin, storageKey must be specified.
  --   Security origin.
  pIndexedDBRequestDatabaseNamesSecurityOrigin :: Maybe String,
  -- | Storage key.
  pIndexedDBRequestDatabaseNamesStorageKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDBRequestDatabaseNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PIndexedDBRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Return type of the 'IndexedDB.requestDatabaseNames' command.
data IndexedDBRequestDatabaseNames = IndexedDBRequestDatabaseNames {
  -- | Database names for origin.
  indexedDBRequestDatabaseNamesDatabaseNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDBRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PIndexedDBRequestDatabaseNames where
   type CommandResponse PIndexedDBRequestDatabaseNames = IndexedDBRequestDatabaseNames
   commandName _ = "IndexedDB.requestDatabaseNames"




