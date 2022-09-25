{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



type DatabaseDatabaseId = String

data DatabaseDatabase = DatabaseDatabase {
   databaseDatabaseId :: DatabaseDatabaseId,
   databaseDatabaseDomain :: String,
   databaseDatabaseName :: String,
   databaseDatabaseVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DatabaseDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data DatabaseError = DatabaseError {
   databaseErrorMessage :: String,
   databaseErrorCode :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  DatabaseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }





data DatabaseAddDatabase = DatabaseAddDatabase {
   databaseAddDatabaseDatabase :: DatabaseDatabase
} deriving (Generic, Eq, Show, Read)
instance ToJSON DatabaseAddDatabase  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DatabaseAddDatabase where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }




databaseDisable :: Handle ev -> IO (Maybe Error)
databaseDisable handle = sendReceiveCommand handle "Database.disable" (Nothing :: Maybe ())


databaseEnable :: Handle ev -> IO (Maybe Error)
databaseEnable handle = sendReceiveCommand handle "Database.enable" (Nothing :: Maybe ())



data PDatabaseExecuteSql = PDatabaseExecuteSql {
   pDatabaseExecuteSqlDatabaseId :: DatabaseDatabaseId,
   pDatabaseExecuteSqlQuery :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseExecuteSql  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDatabaseExecuteSql where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


databaseExecuteSql :: Handle ev -> PDatabaseExecuteSql -> IO (Either Error DatabaseExecuteSql)
databaseExecuteSql handle params = sendReceiveCommandResult handle "Database.executeSQL" (Just params)

data DatabaseExecuteSql = DatabaseExecuteSql {
   databaseExecuteSqlColumnNames :: Maybe [String],
   databaseExecuteSqlValues :: Maybe [Int],
   databaseExecuteSqlSqlError :: Maybe DatabaseError
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseExecuteSql where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command DatabaseExecuteSql where
   commandName _ = "Database.executeSQL"




data PDatabaseGetDatabaseTableNames = PDatabaseGetDatabaseTableNames {
   pDatabaseGetDatabaseTableNamesDatabaseId :: DatabaseDatabaseId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDatabaseGetDatabaseTableNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PDatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


databaseGetDatabaseTableNames :: Handle ev -> PDatabaseGetDatabaseTableNames -> IO (Either Error DatabaseGetDatabaseTableNames)
databaseGetDatabaseTableNames handle params = sendReceiveCommandResult handle "Database.getDatabaseTableNames" (Just params)

data DatabaseGetDatabaseTableNames = DatabaseGetDatabaseTableNames {
   databaseGetDatabaseTableNamesTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DatabaseGetDatabaseTableNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command DatabaseGetDatabaseTableNames where
   commandName _ = "Database.getDatabaseTableNames"




