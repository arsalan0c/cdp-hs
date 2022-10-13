{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module CDP.Definition where
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),pairs,(.:), (.:?), (.=), (.!=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data Version = Version { 
    versionMinor :: Text,
    versionMajor :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Version where
  parseJSON (Object v) = Version <$> v .:  "minor" <*> v .:  "major"
  parseJSON _          = mzero


data Items = Items { 
    itemsType :: Maybe Text,
    itemsRef :: Maybe Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Items where
  parseJSON (Object v) = Items <$> v .:? "type" <*> v .:? "$ref"
  parseJSON _          = mzero


data Property = Property {
    propertyItems :: Maybe Items,
    propertyExperimental :: Bool,
    propertyName :: Text,
    propertyType :: Maybe Text,
    propertyEnum :: Maybe [Text],
    propertyOptional :: Bool,
    propertyRef :: Maybe Text,
    propertyDescription :: Maybe Text,
    propertyDeprecated :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Property where
  parseJSON (Object v) = Property
    <$> v .:? "items"
    <*> v .:? "experimental" .!= False
    <*> v .:  "name"
    <*> v .:? "type"
    <*> v .:? "enum"
    <*> v .:? "optional" .!= False
    <*> v .:? "$ref"
    <*> v .:? "description"
    <*> v .:? "deprecated" .!= False
  parseJSON _          = mzero


data Command = Command {
    commandExperimental :: Bool,
    commandName :: Text,
    commandReturns :: [Property],
    commandParameters :: [Property],
    commandRedirect :: Maybe Text,
    commandDescription :: Maybe Text,
    commandDeprecated ::  Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Command where
  parseJSON (Object v) = Command
    <$> v .:? "experimental" .!= False
    <*> v .:  "name"
    <*> v .:? "returns" .!= []
    <*> v .:? "parameters" .!= []
    <*> v .:? "redirect"
    <*> v .:? "description"
    <*> v .:? "deprecated" .!= False
  parseJSON _          = mzero


data Type = Type {
    typeItems :: Maybe Items,
    typeExperimental :: Bool,
    typeId :: Text,
    typeType :: Text,
    typeEnum :: Maybe [Text],
    typeProperties :: Maybe [Property],
    typeDescription :: Maybe Text,
    typeDeprecated :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Type where
  parseJSON (Object v) = Type
    <$> v .:? "items"
    <*> v .:? "experimental" .!= False
    <*> v .:  "id"
    <*> v .:  "type"
    <*> v .:? "enum"
    <*> v .:? "properties"
    <*> v .:? "description"
    <*> v .:? "deprecated" .!= False
  parseJSON _          = mzero


data Event = Event {
    eventExperimental :: Bool,
    eventName :: Text,
    eventParameters :: [Property],
    eventDescription :: Maybe Text,
    eventDeprecated :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Event where
  parseJSON (Object v) = Event
    <$> v .:? "experimental" .!= False
    <*> v .:  "name"
    <*> v .:? "parameters" .!= []
    <*> v .:? "description"
    <*> v .:? "deprecated" .!= False
  parseJSON _          = mzero


data Domain = Domain {
    domainCommands :: [Command],
    domainDomain :: Text,
    domainDependencies :: [Text],
    domainExperimental :: Bool,
    domainTypes :: [Type],
    domainEvents :: [Event],
    domainDescription :: Maybe Text,
    domainDeprecated :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Domain where
  parseJSON (Object v) = Domain
    <$> v .:  "commands"
    <*> v .:  "domain"
    <*> v .:? "dependencies" .!= []
    <*> v .:? "experimental" .!= False
    <*> v .:? "types" .!= []
    <*> v .:? "events" .!= []
    <*> v .:? "description"
    <*> v .:? "deprecated" .!= False
  parseJSON _          = mzero


data TopLevel = TopLevel { 
    topLevelVersion :: Version,
    topLevelDomains :: [Domain]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "version" <*> v .:  "domains"
  parseJSON _          = mzero




-- | Use parser to get TopLevel object
parse :: FilePath -> IO TopLevel
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left errTop -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err -> "Invalid JSON file: " ++ filename ++ "\n   " ++ err
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ errTop
      Right r     -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure
