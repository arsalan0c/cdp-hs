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


-- | Database with an array of object stores.
data IndexedDbDatabaseWithObjectStores = IndexedDbDatabaseWithObjectStores {
   indexedDbDatabaseWithObjectStoresName :: IndexedDbDatabaseWithObjectStoresName, -- ^ Database name.
   indexedDbDatabaseWithObjectStoresVersion :: IndexedDbDatabaseWithObjectStoresVersion, -- ^ Database version (type is not 'integer', as the standard
requires the version number to be 'unsigned long long')
   indexedDbDatabaseWithObjectStoresObjectStores :: IndexedDbDatabaseWithObjectStoresObjectStores -- ^ Object stores in this database.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDatabaseWithObjectStores  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDatabaseWithObjectStores where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Object store.
data IndexedDbObjectStore = IndexedDbObjectStore {
   indexedDbObjectStoreName :: IndexedDbObjectStoreName, -- ^ Object store name.
   indexedDbObjectStoreKeyPath :: IndexedDbObjectStoreKeyPath, -- ^ Object store key path.
   indexedDbObjectStoreAutoIncrement :: IndexedDbObjectStoreAutoIncrement, -- ^ If true, object store has auto increment flag set.
   indexedDbObjectStoreIndexes :: IndexedDbObjectStoreIndexes -- ^ Indexes in this object store.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Object store index.
data IndexedDbObjectStoreIndex = IndexedDbObjectStoreIndex {
   indexedDbObjectStoreIndexName :: IndexedDbObjectStoreIndexName, -- ^ Index name.
   indexedDbObjectStoreIndexKeyPath :: IndexedDbObjectStoreIndexKeyPath, -- ^ Index key path.
   indexedDbObjectStoreIndexUnique :: IndexedDbObjectStoreIndexUnique, -- ^ If true, index is unique.
   indexedDbObjectStoreIndexMultiEntry :: IndexedDbObjectStoreIndexMultiEntry -- ^ If true, index allows multiple entries for a key.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbObjectStoreIndex  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  IndexedDbObjectStoreIndex where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Key.
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
   indexedDbKeyType :: IndexedDbKeyType, -- ^ Key type.
   indexedDbKeyNumber :: IndexedDbKeyNumber, -- ^ Number value.
   indexedDbKeyString :: IndexedDbKeyString, -- ^ String value.
   indexedDbKeyDate :: IndexedDbKeyDate, -- ^ Date value.
   indexedDbKeyArray :: IndexedDbKeyArray -- ^ Array value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Key range.
data IndexedDbKeyRange = IndexedDbKeyRange {
   indexedDbKeyRangeLower :: IndexedDbKeyRangeLower, -- ^ Lower bound.
   indexedDbKeyRangeUpper :: IndexedDbKeyRangeUpper, -- ^ Upper bound.
   indexedDbKeyRangeLowerOpen :: IndexedDbKeyRangeLowerOpen, -- ^ If true lower bound is open.
   indexedDbKeyRangeUpperOpen :: IndexedDbKeyRangeUpperOpen -- ^ If true upper bound is open.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Data entry.
data IndexedDbDataEntry = IndexedDbDataEntry {
   indexedDbDataEntryKey :: IndexedDbDataEntryKey, -- ^ Key object.
   indexedDbDataEntryPrimaryKey :: IndexedDbDataEntryPrimaryKey, -- ^ Primary key object.
   indexedDbDataEntryValue :: IndexedDbDataEntryValue -- ^ Value object.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  IndexedDbDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Key path.
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
   indexedDbKeyPathType :: IndexedDbKeyPathType, -- ^ Key path type.
   indexedDbKeyPathString :: IndexedDbKeyPathString, -- ^ String value.
   indexedDbKeyPathArray :: IndexedDbKeyPathArray -- ^ Array value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON IndexedDbKeyPath  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  IndexedDbKeyPath where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }







-- | Parameters of the 'indexedDbClearObjectStore' command.
data PIndexedDbClearObjectStore = PIndexedDbClearObjectStore {
   pIndexedDbClearObjectStoreSecurityOrigin :: PIndexedDbClearObjectStoreSecurityOrigin, -- ^ Security origin.
   pIndexedDbClearObjectStoreDatabaseName :: PIndexedDbClearObjectStoreDatabaseName, -- ^ Database name.
   pIndexedDbClearObjectStoreObjectStoreName :: PIndexedDbClearObjectStoreObjectStoreName -- ^ Object store name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbClearObjectStore  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbClearObjectStore where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'IndexedDB.clearObjectStore'.
-- Clears all entries from an object store.
-- Parameters: 'PIndexedDbClearObjectStore'
indexedDbClearObjectStore :: Handle ev -> PIndexedDbClearObjectStore -> IO (Maybe Error)
indexedDbClearObjectStore handle params = sendReceiveCommand handle "IndexedDB.clearObjectStore" (Just params)


-- | Parameters of the 'indexedDbDeleteDatabase' command.
data PIndexedDbDeleteDatabase = PIndexedDbDeleteDatabase {
   pIndexedDbDeleteDatabaseSecurityOrigin :: PIndexedDbDeleteDatabaseSecurityOrigin, -- ^ Security origin.
   pIndexedDbDeleteDatabaseDatabaseName :: PIndexedDbDeleteDatabaseDatabaseName -- ^ Database name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'IndexedDB.deleteDatabase'.
-- Deletes a database.
-- Parameters: 'PIndexedDbDeleteDatabase'
indexedDbDeleteDatabase :: Handle ev -> PIndexedDbDeleteDatabase -> IO (Maybe Error)
indexedDbDeleteDatabase handle params = sendReceiveCommand handle "IndexedDB.deleteDatabase" (Just params)


-- | Parameters of the 'indexedDbDeleteObjectStoreEntries' command.
data PIndexedDbDeleteObjectStoreEntries = PIndexedDbDeleteObjectStoreEntries {



   pIndexedDbDeleteObjectStoreEntriesKeyRange :: PIndexedDbDeleteObjectStoreEntriesKeyRange -- ^ Range of entry keys to delete
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbDeleteObjectStoreEntries  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbDeleteObjectStoreEntries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'IndexedDB.deleteObjectStoreEntries'.
-- Delete a range of entries from an object store
-- Parameters: 'PIndexedDbDeleteObjectStoreEntries'
indexedDbDeleteObjectStoreEntries :: Handle ev -> PIndexedDbDeleteObjectStoreEntries -> IO (Maybe Error)
indexedDbDeleteObjectStoreEntries handle params = sendReceiveCommand handle "IndexedDB.deleteObjectStoreEntries" (Just params)


-- | Function for the command 'IndexedDB.disable'.
-- Disables events from backend.
indexedDbDisable :: Handle ev -> IO (Maybe Error)
indexedDbDisable handle = sendReceiveCommand handle "IndexedDB.disable" (Nothing :: Maybe ())


-- | Function for the command 'IndexedDB.enable'.
-- Enables events from backend.
indexedDbEnable :: Handle ev -> IO (Maybe Error)
indexedDbEnable handle = sendReceiveCommand handle "IndexedDB.enable" (Nothing :: Maybe ())


-- | Parameters of the 'indexedDbRequestData' command.
data PIndexedDbRequestData = PIndexedDbRequestData {
   pIndexedDbRequestDataSecurityOrigin :: PIndexedDbRequestDataSecurityOrigin, -- ^ Security origin.
   pIndexedDbRequestDataDatabaseName :: PIndexedDbRequestDataDatabaseName, -- ^ Database name.
   pIndexedDbRequestDataObjectStoreName :: PIndexedDbRequestDataObjectStoreName, -- ^ Object store name.
   pIndexedDbRequestDataIndexName :: PIndexedDbRequestDataIndexName, -- ^ Index name, empty string for object store data requests.
   pIndexedDbRequestDataSkipCount :: PIndexedDbRequestDataSkipCount, -- ^ Number of records to skip.
   pIndexedDbRequestDataPageSize :: PIndexedDbRequestDataPageSize, -- ^ Number of records to fetch.
   pIndexedDbRequestDataKeyRange :: PIndexedDbRequestDataKeyRange -- ^ Key range.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'IndexedDB.requestData'.
-- Requests data from object store or index.
-- Parameters: 'PIndexedDbRequestData'
-- Returns: 'IndexedDbRequestData'
indexedDbRequestData :: Handle ev -> PIndexedDbRequestData -> IO (Either Error IndexedDbRequestData)
indexedDbRequestData handle params = sendReceiveCommandResult handle "IndexedDB.requestData" (Just params)

-- | Return type of the 'indexedDbRequestData' command.
data IndexedDbRequestData = IndexedDbRequestData {
   indexedDbRequestDataObjectStoreDataEntries :: [IndexedDbDataEntry], -- ^ Array of object store data entries.
   indexedDbRequestDataHasMore :: Bool -- ^ If true, there are more entries to fetch in the given range.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbRequestData where
   commandName _ = "IndexedDB.requestData"



-- | Parameters of the 'indexedDbGetMetadata' command.
data PIndexedDbGetMetadata = PIndexedDbGetMetadata {
   pIndexedDbGetMetadataSecurityOrigin :: PIndexedDbGetMetadataSecurityOrigin, -- ^ Security origin.
   pIndexedDbGetMetadataDatabaseName :: PIndexedDbGetMetadataDatabaseName, -- ^ Database name.
   pIndexedDbGetMetadataObjectStoreName :: PIndexedDbGetMetadataObjectStoreName -- ^ Object store name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbGetMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'IndexedDB.getMetadata'.
-- Gets metadata of an object store
-- Parameters: 'PIndexedDbGetMetadata'
-- Returns: 'IndexedDbGetMetadata'
indexedDbGetMetadata :: Handle ev -> PIndexedDbGetMetadata -> IO (Either Error IndexedDbGetMetadata)
indexedDbGetMetadata handle params = sendReceiveCommandResult handle "IndexedDB.getMetadata" (Just params)

-- | Return type of the 'indexedDbGetMetadata' command.
data IndexedDbGetMetadata = IndexedDbGetMetadata {
   indexedDbGetMetadataEntriesCount :: Double, -- ^ the entries count
   indexedDbGetMetadataKeyGeneratorValue :: Double -- ^ the current value of key generator, to become the next inserted
key into the object store. Valid if objectStore.autoIncrement
is true.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbGetMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command IndexedDbGetMetadata where
   commandName _ = "IndexedDB.getMetadata"



-- | Parameters of the 'indexedDbRequestDatabase' command.
data PIndexedDbRequestDatabase = PIndexedDbRequestDatabase {
   pIndexedDbRequestDatabaseSecurityOrigin :: PIndexedDbRequestDatabaseSecurityOrigin, -- ^ Security origin.
   pIndexedDbRequestDatabaseDatabaseName :: PIndexedDbRequestDatabaseDatabaseName -- ^ Database name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'IndexedDB.requestDatabase'.
-- Requests database with given name in given frame.
-- Parameters: 'PIndexedDbRequestDatabase'
-- Returns: 'IndexedDbRequestDatabase'
indexedDbRequestDatabase :: Handle ev -> PIndexedDbRequestDatabase -> IO (Either Error IndexedDbRequestDatabase)
indexedDbRequestDatabase handle params = sendReceiveCommandResult handle "IndexedDB.requestDatabase" (Just params)

-- | Return type of the 'indexedDbRequestDatabase' command.
data IndexedDbRequestDatabase = IndexedDbRequestDatabase {
   indexedDbRequestDatabaseDatabaseWithObjectStores :: IndexedDbDatabaseWithObjectStores -- ^ Database with an array of object stores.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command IndexedDbRequestDatabase where
   commandName _ = "IndexedDB.requestDatabase"



-- | Parameters of the 'indexedDbRequestDatabaseNames' command.
data PIndexedDbRequestDatabaseNames = PIndexedDbRequestDatabaseNames {
   pIndexedDbRequestDatabaseNamesSecurityOrigin :: PIndexedDbRequestDatabaseNamesSecurityOrigin -- ^ Security origin.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PIndexedDbRequestDatabaseNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PIndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'IndexedDB.requestDatabaseNames'.
-- Requests database names for given security origin.
-- Parameters: 'PIndexedDbRequestDatabaseNames'
-- Returns: 'IndexedDbRequestDatabaseNames'
indexedDbRequestDatabaseNames :: Handle ev -> PIndexedDbRequestDatabaseNames -> IO (Either Error IndexedDbRequestDatabaseNames)
indexedDbRequestDatabaseNames handle params = sendReceiveCommandResult handle "IndexedDB.requestDatabaseNames" (Just params)

-- | Return type of the 'indexedDbRequestDatabaseNames' command.
data IndexedDbRequestDatabaseNames = IndexedDbRequestDatabaseNames {
   indexedDbRequestDatabaseNamesDatabaseNames :: [String] -- ^ Database names for origin.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  IndexedDbRequestDatabaseNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command IndexedDbRequestDatabaseNames where
   commandName _ = "IndexedDB.requestDatabaseNames"




