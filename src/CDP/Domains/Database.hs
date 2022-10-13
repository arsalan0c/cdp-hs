{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  Database 
-}


module CDP.Domains.Database (module CDP.Domains.Database) where

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




-- | Type 'Database.DatabaseId'.
--   Unique identifier of Database object.
type DatabaseDatabaseId = String

-- | Type 'Database.Database'.
--   Database object.
data DatabaseDatabase = DatabaseDatabase {
  -- | Database ID.
  databaseDatabaseId :: DatabaseDatabaseId,
  -- | Database domain.
  databaseDatabaseDomain :: String,
  -- | Database name.
  databaseDatabaseName :: String,
  -- | Database version.
  databaseDatabaseVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DatabaseDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Database.Error'.
--   Database error.
data DatabaseError = DatabaseError {
  -- | Error message.
  databaseErrorMessage :: String,
  -- | Error code.
  databaseErrorCode :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DatabaseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }





-- | Type of the 'Database.addDatabase' event.
data DatabaseAddDatabase = DatabaseAddDatabase {
  databaseAddDatabaseDatabase :: DatabaseDatabase
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseAddDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DatabaseAddDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event DatabaseAddDatabase where
    eventName _ = "Database.addDatabase"



-- | Database.disable
--   Disables database tracking, prevents database events from being sent to the client.

-- | Parameters of the 'Database.disable' command.
data PDatabaseDisable = PDatabaseDisable
instance ToJSON PDatabaseDisable where toJSON _ = A.Null

instance Command PDatabaseDisable where
   type CommandResponse PDatabaseDisable = ()
   commandName _ = "Database.disable"
   fromJSON = const . A.Success . const ()


-- | Database.enable
--   Enables database tracking, database events will now be delivered to the client.

-- | Parameters of the 'Database.enable' command.
data PDatabaseEnable = PDatabaseEnable
instance ToJSON PDatabaseEnable where toJSON _ = A.Null

instance Command PDatabaseEnable where
   type CommandResponse PDatabaseEnable = ()
   commandName _ = "Database.enable"
   fromJSON = const . A.Success . const ()


-- | Database.executeSQL

-- | Parameters of the 'Database.executeSQL' command.
data PDatabaseExecuteSQL = PDatabaseExecuteSQL {
  pDatabaseExecuteSQLDatabaseId :: DatabaseDatabaseId,
  pDatabaseExecuteSQLQuery :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseExecuteSQL  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDatabaseExecuteSQL where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Return type of the 'Database.executeSQL' command.
data DatabaseExecuteSQL = DatabaseExecuteSQL {
  databaseExecuteSQLColumnNames :: Maybe [String],
  databaseExecuteSQLValues :: Maybe [Int],
  databaseExecuteSQLSqlError :: Maybe DatabaseError
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseExecuteSQL where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PDatabaseExecuteSQL where
   type CommandResponse PDatabaseExecuteSQL = DatabaseExecuteSQL
   commandName _ = "Database.executeSQL"



-- | Database.getDatabaseTableNames

-- | Parameters of the 'Database.getDatabaseTableNames' command.
data PDatabaseGetDatabaseTableNames = PDatabaseGetDatabaseTableNames {
  pDatabaseGetDatabaseTableNamesDatabaseId :: DatabaseDatabaseId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseGetDatabaseTableNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PDatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Return type of the 'Database.getDatabaseTableNames' command.
data DatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames {
  databaseGetDatabaseTableNamesTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PDatabaseGetDatabaseTableNames where
   type CommandResponse PDatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames
   commandName _ = "Database.getDatabaseTableNames"




