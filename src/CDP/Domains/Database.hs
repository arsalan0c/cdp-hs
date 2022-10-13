{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Database

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
data DatabaseDatabase = DatabaseDatabase
  {
    -- | Database ID.
    databaseDatabaseId :: DatabaseDatabaseId,
    -- | Database domain.
    databaseDatabaseDomain :: String,
    -- | Database name.
    databaseDatabaseName :: String,
    -- | Database version.
    databaseDatabaseVersion :: String
  }
  deriving (Eq, Show)
instance FromJSON DatabaseDatabase where
  parseJSON = A.withObject "DatabaseDatabase" $ \o -> DatabaseDatabase
    <$> o A..: "id"
    <*> o A..: "domain"
    <*> o A..: "name"
    <*> o A..: "version"
instance ToJSON DatabaseDatabase where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (databaseDatabaseId p),
    ("domain" A..=) <$> Just (databaseDatabaseDomain p),
    ("name" A..=) <$> Just (databaseDatabaseName p),
    ("version" A..=) <$> Just (databaseDatabaseVersion p)
    ]

-- | Type 'Database.Error'.
--   Database error.
data DatabaseError = DatabaseError
  {
    -- | Error message.
    databaseErrorMessage :: String,
    -- | Error code.
    databaseErrorCode :: Int
  }
  deriving (Eq, Show)
instance FromJSON DatabaseError where
  parseJSON = A.withObject "DatabaseError" $ \o -> DatabaseError
    <$> o A..: "message"
    <*> o A..: "code"
instance ToJSON DatabaseError where
  toJSON p = A.object $ catMaybes [
    ("message" A..=) <$> Just (databaseErrorMessage p),
    ("code" A..=) <$> Just (databaseErrorCode p)
    ]

-- | Type of the 'Database.addDatabase' event.
data DatabaseAddDatabase = DatabaseAddDatabase
  {
    databaseAddDatabaseDatabase :: DatabaseDatabase
  }
  deriving (Eq, Show)
instance FromJSON DatabaseAddDatabase where
  parseJSON = A.withObject "DatabaseAddDatabase" $ \o -> DatabaseAddDatabase
    <$> o A..: "database"
instance Event DatabaseAddDatabase where
  eventName _ = "Database.addDatabase"

-- | Disables database tracking, prevents database events from being sent to the client.

-- | Parameters of the 'Database.disable' command.
data PDatabaseDisable = PDatabaseDisable
  deriving (Eq, Show)
pDatabaseDisable
  :: PDatabaseDisable
pDatabaseDisable
  = PDatabaseDisable
instance ToJSON PDatabaseDisable where
  toJSON _ = A.Null
instance Command PDatabaseDisable where
  type CommandResponse PDatabaseDisable = ()
  commandName _ = "Database.disable"
  fromJSON = const . A.Success . const ()

-- | Enables database tracking, database events will now be delivered to the client.

-- | Parameters of the 'Database.enable' command.
data PDatabaseEnable = PDatabaseEnable
  deriving (Eq, Show)
pDatabaseEnable
  :: PDatabaseEnable
pDatabaseEnable
  = PDatabaseEnable
instance ToJSON PDatabaseEnable where
  toJSON _ = A.Null
instance Command PDatabaseEnable where
  type CommandResponse PDatabaseEnable = ()
  commandName _ = "Database.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Database.executeSQL' command.
data PDatabaseExecuteSQL = PDatabaseExecuteSQL
  {
    pDatabaseExecuteSQLDatabaseId :: DatabaseDatabaseId,
    pDatabaseExecuteSQLQuery :: String
  }
  deriving (Eq, Show)
pDatabaseExecuteSQL
  :: DatabaseDatabaseId
  -> String
  -> PDatabaseExecuteSQL
pDatabaseExecuteSQL
  arg_pDatabaseExecuteSQLDatabaseId
  arg_pDatabaseExecuteSQLQuery
  = PDatabaseExecuteSQL
    arg_pDatabaseExecuteSQLDatabaseId
    arg_pDatabaseExecuteSQLQuery
instance ToJSON PDatabaseExecuteSQL where
  toJSON p = A.object $ catMaybes [
    ("databaseId" A..=) <$> Just (pDatabaseExecuteSQLDatabaseId p),
    ("query" A..=) <$> Just (pDatabaseExecuteSQLQuery p)
    ]
data DatabaseExecuteSQL = DatabaseExecuteSQL
  {
    databaseExecuteSQLColumnNames :: Maybe [String],
    databaseExecuteSQLValues :: Maybe [Int],
    databaseExecuteSQLSqlError :: Maybe DatabaseError
  }
  deriving (Eq, Show)
instance FromJSON DatabaseExecuteSQL where
  parseJSON = A.withObject "DatabaseExecuteSQL" $ \o -> DatabaseExecuteSQL
    <$> o A..:? "columnNames"
    <*> o A..:? "values"
    <*> o A..:? "sqlError"
instance Command PDatabaseExecuteSQL where
  type CommandResponse PDatabaseExecuteSQL = DatabaseExecuteSQL
  commandName _ = "Database.executeSQL"


-- | Parameters of the 'Database.getDatabaseTableNames' command.
data PDatabaseGetDatabaseTableNames = PDatabaseGetDatabaseTableNames
  {
    pDatabaseGetDatabaseTableNamesDatabaseId :: DatabaseDatabaseId
  }
  deriving (Eq, Show)
pDatabaseGetDatabaseTableNames
  :: DatabaseDatabaseId
  -> PDatabaseGetDatabaseTableNames
pDatabaseGetDatabaseTableNames
  arg_pDatabaseGetDatabaseTableNamesDatabaseId
  = PDatabaseGetDatabaseTableNames
    arg_pDatabaseGetDatabaseTableNamesDatabaseId
instance ToJSON PDatabaseGetDatabaseTableNames where
  toJSON p = A.object $ catMaybes [
    ("databaseId" A..=) <$> Just (pDatabaseGetDatabaseTableNamesDatabaseId p)
    ]
data DatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames
  {
    databaseGetDatabaseTableNamesTableNames :: [String]
  }
  deriving (Eq, Show)
instance FromJSON DatabaseGetDatabaseTableNames where
  parseJSON = A.withObject "DatabaseGetDatabaseTableNames" $ \o -> DatabaseGetDatabaseTableNames
    <$> o A..: "tableNames"
instance Command PDatabaseGetDatabaseTableNames where
  type CommandResponse PDatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames
  commandName _ = "Database.getDatabaseTableNames"

