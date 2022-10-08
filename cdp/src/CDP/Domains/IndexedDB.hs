{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


-- | Type 'IndexedDB.DatabaseWithObjectStores'.
--   Database with an array of object stores.
data IndexedDbDatabaseWithObjectStores = IndexedDbDatabaseWithObjectStores {
  -- | Database name.
  indexedDbDatabaseWithObjectStoresName :: String,
  -- | Database version (type is not 'integer', as the standard
  --   requires the version number to be 'unsigned long long')
  indexedDbDatabaseWithObjectStoresVersion :: Double,
  -- | Object stores in this database.
  indexedDbDatabaseWithObjectStoresObjectStores :: [IndexedDbObjectStore]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDatabaseWithObjectStores  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDatabaseWithObjectStores where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'IndexedDB.ObjectStore'.
--   Object store.
data IndexedDbObjectStore = IndexedDbObjectStore {
  -- | Object store name.
  indexedDbObjectStoreName :: String,
  -- | Object store key path.
  indexedDbObjectStoreKeyPath :: IndexedDbKeyPath,
  -- | If true, object store has auto increment flag set.
  indexedDbObjectStoreAutoIncrement :: Bool,
  -- | Indexes in this object store.
  indexedDbObjectStoreIndexes :: [IndexedDbObjectStoreIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'IndexedDB.ObjectStoreIndex'.
--   Object store index.
data IndexedDbObjectStoreIndex = IndexedDbObjectStoreIndex {
  -- | Index name.
  indexedDbObjectStoreIndexName :: String,
  -- | Index key path.
  indexedDbObjectStoreIndexKeyPath :: IndexedDbKeyPath,
  -- | If true, index is unique.
  indexedDbObjectStoreIndexUnique :: Bool,
  -- | If true, index allows multiple entries for a key.
  indexedDbObjectStoreIndexMultiEntry :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStoreIndex  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStoreIndex where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'IndexedDB.Key'.
--   Key.
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
  -- | Key type.
  indexedDbKeyType :: IndexedDbKeyType,
  -- | Number value.
  indexedDbKeyNumber :: Maybe Double,
  -- | String value.
  indexedDbKeyString :: Maybe String,
  -- | Date value.
  indexedDbKeyDate :: Maybe Double,
  -- | Array value.
  indexedDbKeyArray :: Maybe [IndexedDbKey]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Type 'IndexedDB.KeyRange'.
--   Key range.
data IndexedDbKeyRange = IndexedDbKeyRange {
  -- | Lower bound.
  indexedDbKeyRangeLower :: Maybe IndexedDbKey,
  -- | Upper bound.
  indexedDbKeyRangeUpper :: Maybe IndexedDbKey,
  -- | If true lower bound is open.
  indexedDbKeyRangeLowerOpen :: Bool,
  -- | If true upper bound is open.
  indexedDbKeyRangeUpperOpen :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'IndexedDB.DataEntry'.
--   Data entry.
data IndexedDbDataEntry = IndexedDbDataEntry {
  -- | Key object.
  indexedDbDataEntryKey :: Runtime.RuntimeRemoteObject,
  -- | Primary key object.
  indexedDbDataEntryPrimaryKey :: Runtime.RuntimeRemoteObject,
  -- | Value object.
  indexedDbDataEntryValue :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'IndexedDB.KeyPath'.
--   Key path.
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
  -- | Key path type.
  indexedDbKeyPathType :: IndexedDbKeyPathType,
  -- | String value.
  indexedDbKeyPathString :: Maybe String,
  -- | Array value.
  indexedDbKeyPathArray :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyPath  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyPath where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }







-- | Parameters of the 'indexedDbClearObjectStore' command.
data PIndexedDbClearObjectStore = PIndexedDbClearObjectStore {
  -- | Security origin.
  pIndexedDbClearObjectStoreSecurityOrigin :: String,
  -- | Database name.
  pIndexedDbClearObjectStoreDatabaseName :: String,
  -- | Object store name.
  pIndexedDbClearObjectStoreObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbClearObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbClearObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'IndexedDB.clearObjectStore' command.
--   Clears all entries from an object store.
--   Parameters: 'PIndexedDbClearObjectStore'
indexedDbClearObjectStore :: Handle ev -> Maybe String -> PIndexedDbClearObjectStore -> IO ()
indexedDbClearObjectStore handle sessionId params = sendReceiveCommand handle sessionId "IndexedDB.clearObjectStore" (Just params )


-- | Parameters of the 'indexedDbDeleteDatabase' command.
data PIndexedDbDeleteDatabase = PIndexedDbDeleteDatabase {
  -- | Security origin.
  pIndexedDbDeleteDatabaseSecurityOrigin :: String,
  -- | Database name.
  pIndexedDbDeleteDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'IndexedDB.deleteDatabase' command.
--   Deletes a database.
--   Parameters: 'PIndexedDbDeleteDatabase'
indexedDbDeleteDatabase :: Handle ev -> Maybe String -> PIndexedDbDeleteDatabase -> IO ()
indexedDbDeleteDatabase handle sessionId params = sendReceiveCommand handle sessionId "IndexedDB.deleteDatabase" (Just params )


-- | Parameters of the 'indexedDbDeleteObjectStoreEntries' command.
data PIndexedDbDeleteObjectStoreEntries = PIndexedDbDeleteObjectStoreEntries {
  pIndexedDbDeleteObjectStoreEntriesSecurityOrigin :: String,
  pIndexedDbDeleteObjectStoreEntriesDatabaseName :: String,
  pIndexedDbDeleteObjectStoreEntriesObjectStoreName :: String,
  -- | Range of entry keys to delete
  pIndexedDbDeleteObjectStoreEntriesKeyRange :: IndexedDbKeyRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteObjectStoreEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteObjectStoreEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'IndexedDB.deleteObjectStoreEntries' command.
--   Delete a range of entries from an object store
--   Parameters: 'PIndexedDbDeleteObjectStoreEntries'
indexedDbDeleteObjectStoreEntries :: Handle ev -> Maybe String -> PIndexedDbDeleteObjectStoreEntries -> IO ()
indexedDbDeleteObjectStoreEntries handle sessionId params = sendReceiveCommand handle sessionId "IndexedDB.deleteObjectStoreEntries" (Just params )


-- | Function for the 'IndexedDB.disable' command.
--   Disables events from backend.
indexedDbDisable :: Handle ev -> Maybe String -> IO ()
indexedDbDisable handle sessionId = sendReceiveCommand handle sessionId "IndexedDB.disable" (Nothing :: Maybe ())


-- | Function for the 'IndexedDB.enable' command.
--   Enables events from backend.
indexedDbEnable :: Handle ev -> Maybe String -> IO ()
indexedDbEnable handle sessionId = sendReceiveCommand handle sessionId "IndexedDB.enable" (Nothing :: Maybe ())


-- | Parameters of the 'indexedDbRequestData' command.
data PIndexedDbRequestData = PIndexedDbRequestData {
  -- | Security origin.
  pIndexedDbRequestDataSecurityOrigin :: String,
  -- | Database name.
  pIndexedDbRequestDataDatabaseName :: String,
  -- | Object store name.
  pIndexedDbRequestDataObjectStoreName :: String,
  -- | Index name, empty string for object store data requests.
  pIndexedDbRequestDataIndexName :: String,
  -- | Number of records to skip.
  pIndexedDbRequestDataSkipCount :: Int,
  -- | Number of records to fetch.
  pIndexedDbRequestDataPageSize :: Int,
  -- | Key range.
  pIndexedDbRequestDataKeyRange :: Maybe IndexedDbKeyRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'IndexedDB.requestData' command.
--   Requests data from object store or index.
--   Parameters: 'PIndexedDbRequestData'
--   Returns: 'IndexedDbRequestData'
indexedDbRequestData :: Handle ev -> Maybe String -> PIndexedDbRequestData -> IO IndexedDbRequestData
indexedDbRequestData handle sessionId params = sendReceiveCommandResult handle sessionId "IndexedDB.requestData" (Just params )

-- | Return type of the 'indexedDbRequestData' command.
data IndexedDbRequestData = IndexedDbRequestData {
  -- | Array of object store data entries.
  indexedDbRequestDataObjectStoreDataEntries :: [IndexedDbDataEntry],
  -- | If true, there are more entries to fetch in the given range.
  indexedDbRequestDataHasMore :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbRequestData where
   commandName _ = "IndexedDB.requestData"



-- | Parameters of the 'indexedDbGetMetadata' command.
data PIndexedDbGetMetadata = PIndexedDbGetMetadata {
  -- | Security origin.
  pIndexedDbGetMetadataSecurityOrigin :: String,
  -- | Database name.
  pIndexedDbGetMetadataDatabaseName :: String,
  -- | Object store name.
  pIndexedDbGetMetadataObjectStoreName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbGetMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'IndexedDB.getMetadata' command.
--   Gets metadata of an object store
--   Parameters: 'PIndexedDbGetMetadata'
--   Returns: 'IndexedDbGetMetadata'
indexedDbGetMetadata :: Handle ev -> Maybe String -> PIndexedDbGetMetadata -> IO IndexedDbGetMetadata
indexedDbGetMetadata handle sessionId params = sendReceiveCommandResult handle sessionId "IndexedDB.getMetadata" (Just params )

-- | Return type of the 'indexedDbGetMetadata' command.
data IndexedDbGetMetadata = IndexedDbGetMetadata {
  -- | the entries count
  indexedDbGetMetadataEntriesCount :: Double,
  -- | the current value of key generator, to become the next inserted
  --   key into the object store. Valid if objectStore.autoIncrement
  --   is true.
  indexedDbGetMetadataKeyGeneratorValue :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbGetMetadata where
   commandName _ = "IndexedDB.getMetadata"



-- | Parameters of the 'indexedDbRequestDatabase' command.
data PIndexedDbRequestDatabase = PIndexedDbRequestDatabase {
  -- | Security origin.
  pIndexedDbRequestDatabaseSecurityOrigin :: String,
  -- | Database name.
  pIndexedDbRequestDatabaseDatabaseName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'IndexedDB.requestDatabase' command.
--   Requests database with given name in given frame.
--   Parameters: 'PIndexedDbRequestDatabase'
--   Returns: 'IndexedDbRequestDatabase'
indexedDbRequestDatabase :: Handle ev -> Maybe String -> PIndexedDbRequestDatabase -> IO IndexedDbRequestDatabase
indexedDbRequestDatabase handle sessionId params = sendReceiveCommandResult handle sessionId "IndexedDB.requestDatabase" (Just params )

-- | Return type of the 'indexedDbRequestDatabase' command.
data IndexedDbRequestDatabase = IndexedDbRequestDatabase {
  -- | Database with an array of object stores.
  indexedDbRequestDatabaseDatabaseWithObjectStores :: IndexedDbDatabaseWithObjectStores
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command IndexedDbRequestDatabase where
   commandName _ = "IndexedDB.requestDatabase"



-- | Parameters of the 'indexedDbRequestDatabaseNames' command.
data PIndexedDbRequestDatabaseNames = PIndexedDbRequestDatabaseNames {
  -- | Security origin.
  pIndexedDbRequestDatabaseNamesSecurityOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabaseNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'IndexedDB.requestDatabaseNames' command.
--   Requests database names for given security origin.
--   Parameters: 'PIndexedDbRequestDatabaseNames'
--   Returns: 'IndexedDbRequestDatabaseNames'
indexedDbRequestDatabaseNames :: Handle ev -> Maybe String -> PIndexedDbRequestDatabaseNames -> IO IndexedDbRequestDatabaseNames
indexedDbRequestDatabaseNames handle sessionId params = sendReceiveCommandResult handle sessionId "IndexedDB.requestDatabaseNames" (Just params )

-- | Return type of the 'indexedDbRequestDatabaseNames' command.
data IndexedDbRequestDatabaseNames = IndexedDbRequestDatabaseNames {
  -- | Database names for origin.
  indexedDbRequestDatabaseNamesDatabaseNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command IndexedDbRequestDatabaseNames where
   commandName _ = "IndexedDB.requestDatabaseNames"




