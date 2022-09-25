{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.DOMStorage (module CDP.Domains.DOMStorage) where

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



type DomStorageSerializedStorageKey = String

data DomStorageStorageId = DomStorageStorageId {
   domStorageStorageIdSecurityOrigin :: Maybe String,
   domStorageStorageIdStorageKey :: Maybe DomStorageSerializedStorageKey,
   domStorageStorageIdIsLocalStorage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageStorageId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomStorageStorageId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


type DomStorageItem = [String]



data DomStorageDomStorageItemAdded = DomStorageDomStorageItemAdded {
   domStorageDomStorageItemAddedStorageId :: DomStorageStorageId,
   domStorageDomStorageItemAddedKey :: String,
   domStorageDomStorageItemAddedNewValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data DomStorageDomStorageItemRemoved = DomStorageDomStorageItemRemoved {
   domStorageDomStorageItemRemovedStorageId :: DomStorageStorageId,
   domStorageDomStorageItemRemovedKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data DomStorageDomStorageItemUpdated = DomStorageDomStorageItemUpdated {
   domStorageDomStorageItemUpdatedStorageId :: DomStorageStorageId,
   domStorageDomStorageItemUpdatedKey :: String,
   domStorageDomStorageItemUpdatedOldValue :: String,
   domStorageDomStorageItemUpdatedNewValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data DomStorageDomStorageItemsCleared = DomStorageDomStorageItemsCleared {
   domStorageDomStorageItemsClearedStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemsCleared  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemsCleared where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }





data PDomStorageClear = PDomStorageClear {
   pDomStorageClearStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageClear  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomStorageClear where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domStorageClear :: Handle ev -> PDomStorageClear -> IO (Maybe Error)
domStorageClear handle params = sendReceiveCommand handle "DOMStorage.clear" (Just params)


domStorageDisable :: Handle ev -> IO (Maybe Error)
domStorageDisable handle = sendReceiveCommand handle "DOMStorage.disable" (Nothing :: Maybe ())


domStorageEnable :: Handle ev -> IO (Maybe Error)
domStorageEnable handle = sendReceiveCommand handle "DOMStorage.enable" (Nothing :: Maybe ())



data PDomStorageGetDomStorageItems = PDomStorageGetDomStorageItems {
   pDomStorageGetDomStorageItemsStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageGetDomStorageItems  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


domStorageGetDomStorageItems :: Handle ev -> PDomStorageGetDomStorageItems -> IO (Either Error DomStorageGetDomStorageItems)
domStorageGetDomStorageItems handle params = sendReceiveCommandResult handle "DOMStorage.getDOMStorageItems" (Just params)

data DomStorageGetDomStorageItems = DomStorageGetDomStorageItems {
   domStorageGetDomStorageItemsEntries :: [DomStorageItem]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomStorageGetDomStorageItems where
   commandName _ = "DOMStorage.getDOMStorageItems"




data PDomStorageRemoveDomStorageItem = PDomStorageRemoveDomStorageItem {
   pDomStorageRemoveDomStorageItemStorageId :: DomStorageStorageId,
   pDomStorageRemoveDomStorageItemKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageRemoveDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomStorageRemoveDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


domStorageRemoveDomStorageItem :: Handle ev -> PDomStorageRemoveDomStorageItem -> IO (Maybe Error)
domStorageRemoveDomStorageItem handle params = sendReceiveCommand handle "DOMStorage.removeDOMStorageItem" (Just params)



data PDomStorageSetDomStorageItem = PDomStorageSetDomStorageItem {
   pDomStorageSetDomStorageItemStorageId :: DomStorageStorageId,
   pDomStorageSetDomStorageItemKey :: String,
   pDomStorageSetDomStorageItemValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageSetDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomStorageSetDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domStorageSetDomStorageItem :: Handle ev -> PDomStorageSetDomStorageItem -> IO (Maybe Error)
domStorageSetDomStorageItem handle params = sendReceiveCommand handle "DOMStorage.setDOMStorageItem" (Just params)



