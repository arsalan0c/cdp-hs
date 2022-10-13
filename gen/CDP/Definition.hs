-- parametersElt=propertiesElt

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
-- | DO NOT EDIT THIS FILE MANUALLY!
--   It was automatically generated by `json-autotype`.
module CDP.Definition where
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),pairs,(.:), (.:?), (.=), object)
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


data ReturnsElt = ReturnsElt { 
    returnsEltItems :: Maybe Items,
    returnsEltExperimental :: Maybe Bool,
    returnsEltName :: Text,
    returnsEltType :: Maybe Text,
    returnsEltOptional :: Maybe Bool,
    returnsEltRef :: Maybe Text,
    returnsEltDescription :: Maybe Text,
    returnsEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ReturnsElt where
  parseJSON (Object v) = ReturnsElt <$> v .:? "items" <*> v .:? "experimental" <*> v .:  "name" <*> v .:? "type" <*> v .:? "optional" <*> v .:? "$ref" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data ParametersElt = ParametersElt { 
    parametersEltItems :: Maybe Items,
    parametersEltExperimental :: Maybe Bool,
    parametersEltName :: Text,
    parametersEltType :: Maybe Text,
    parametersEltEnum :: (Maybe ([Text])),
    parametersEltOptional :: Maybe Bool,
    parametersEltRef :: Maybe Text,
    parametersEltDescription :: Maybe Text,
    parametersEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ParametersElt where
  parseJSON (Object v) = ParametersElt <$> v .:? "items" <*> v .:? "experimental" <*> v .:  "name" <*> v .:? "type" <*> v .:? "enum" <*> v .:? "optional" <*> v .:? "$ref" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data CommandsElt = CommandsElt { 
    commandsEltExperimental :: Maybe Bool,
    commandsEltName :: Text,
    commandsEltReturns :: (Maybe ([ReturnsElt])),
    commandsEltParameters :: (Maybe ([ParametersElt])),
    commandsEltRedirect :: Maybe Text,
    commandsEltDescription :: Maybe Text,
    commandsEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON CommandsElt where
  parseJSON (Object v) = CommandsElt <$> v .:? "experimental" <*> v .:  "name" <*> v .:? "returns" <*> v .:? "parameters" <*> v .:? "redirect" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data TypesElt = TypesElt { 
    typesEltItems :: Maybe Items,
    typesEltExperimental :: Maybe Bool,
    typesEltId :: Text,
    typesEltType :: Text,
    typesEltEnum :: (Maybe ([Text])),
    typesEltProperties :: (Maybe ([ParametersElt])),
    typesEltDescription :: Maybe Text,
    typesEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TypesElt where
  parseJSON (Object v) = TypesElt <$> v .:? "items" <*> v .:? "experimental" <*> v .:  "id" <*> v .:  "type" <*> v .:? "enum" <*> v .:? "properties" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data EventsElt = EventsElt { 
    eventsEltExperimental :: Maybe Bool,
    eventsEltName :: Text,
    eventsEltParameters :: (Maybe ([ParametersElt])),
    eventsEltDescription :: Maybe Text,
    eventsEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON EventsElt where
  parseJSON (Object v) = EventsElt <$> v .:? "experimental" <*> v .:  "name" <*> v .:? "parameters" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data DomainsElt = DomainsElt { 
    domainsEltCommands :: [CommandsElt],
    domainsEltDomain :: Text,
    domainsEltDependencies :: (Maybe ([Text])),
    domainsEltExperimental :: Maybe Bool,
    domainsEltTypes :: (Maybe ([TypesElt])),
    domainsEltEvents :: (Maybe ([EventsElt])),
    domainsEltDescription :: Maybe Text,
    domainsEltDeprecated :: Maybe Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON DomainsElt where
  parseJSON (Object v) = DomainsElt <$> v .:  "commands" <*> v .:  "domain" <*> v .:? "dependencies" <*> v .:? "experimental" <*> v .:? "types" <*> v .:? "events" <*> v .:? "description" <*> v .:? "deprecated"
  parseJSON _          = mzero


data TopLevel = TopLevel { 
    topLevelVersion :: Version,
    topLevelDomains :: [DomainsElt]
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
