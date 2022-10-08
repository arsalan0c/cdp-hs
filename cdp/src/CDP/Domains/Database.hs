{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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





-- | Function for the 'Database.disable' command.
--   Disables database tracking, prevents database events from being sent to the client.
databaseDisable :: Handle ev -> Maybe String -> IO ()
databaseDisable handle sessionId = sendReceiveCommand handle sessionId "Database.disable" (Nothing :: Maybe ())


-- | Function for the 'Database.enable' command.
--   Enables database tracking, database events will now be delivered to the client.
databaseEnable :: Handle ev -> Maybe String -> IO ()
databaseEnable handle sessionId = sendReceiveCommand handle sessionId "Database.enable" (Nothing :: Maybe ())


-- | Parameters of the 'databaseExecuteSql' command.
data PDatabaseExecuteSql = PDatabaseExecuteSql {
  pDatabaseExecuteSqlDatabaseId :: DatabaseDatabaseId,
  pDatabaseExecuteSqlQuery :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseExecuteSql  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDatabaseExecuteSql where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'Database.executeSQL' command.
--   
--   Parameters: 'PDatabaseExecuteSql'
--   Returns: 'DatabaseExecuteSql'
databaseExecuteSql :: Handle ev -> Maybe String -> PDatabaseExecuteSql -> IO DatabaseExecuteSql
databaseExecuteSql handle sessionId params = sendReceiveCommandResult handle sessionId "Database.executeSQL" (Just params )

-- | Return type of the 'databaseExecuteSql' command.
data DatabaseExecuteSql = DatabaseExecuteSql {
  databaseExecuteSqlColumnNames :: Maybe [String],
  databaseExecuteSqlValues :: Maybe [Int],
  databaseExecuteSqlSqlError :: Maybe DatabaseError
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseExecuteSql where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command DatabaseExecuteSql where
   commandName _ = "Database.executeSQL"



-- | Parameters of the 'databaseGetDatabaseTableNames' command.
data PDatabaseGetDatabaseTableNames = PDatabaseGetDatabaseTableNames {
  pDatabaseGetDatabaseTableNamesDatabaseId :: DatabaseDatabaseId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseGetDatabaseTableNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PDatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Database.getDatabaseTableNames' command.
--   
--   Parameters: 'PDatabaseGetDatabaseTableNames'
--   Returns: 'DatabaseGetDatabaseTableNames'
databaseGetDatabaseTableNames :: Handle ev -> Maybe String -> PDatabaseGetDatabaseTableNames -> IO DatabaseGetDatabaseTableNames
databaseGetDatabaseTableNames handle sessionId params = sendReceiveCommandResult handle sessionId "Database.getDatabaseTableNames" (Just params )

-- | Return type of the 'databaseGetDatabaseTableNames' command.
data DatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames {
  databaseGetDatabaseTableNamesTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command DatabaseGetDatabaseTableNames where
   commandName _ = "Database.getDatabaseTableNames"




