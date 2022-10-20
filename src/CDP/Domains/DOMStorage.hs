{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DOMStorage

Query and modify DOM storage.
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'DOMStorage.SerializedStorageKey'.
type DOMStorageSerializedStorageKey = T.Text

-- | Type 'DOMStorage.StorageId'.
--   DOM Storage identifier.
data DOMStorageStorageId = DOMStorageStorageId
  {
    -- | Security origin for the storage.
    dOMStorageStorageIdSecurityOrigin :: Maybe T.Text,
    -- | Represents a key by which DOM Storage keys its CachedStorageAreas
    dOMStorageStorageIdStorageKey :: Maybe DOMStorageSerializedStorageKey,
    -- | Whether the storage is local storage (not session storage).
    dOMStorageStorageIdIsLocalStorage :: Bool
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageStorageId where
  parseJSON = A.withObject "DOMStorageStorageId" $ \o -> DOMStorageStorageId
    <$> o A..:? "securityOrigin"
    <*> o A..:? "storageKey"
    <*> o A..: "isLocalStorage"
instance ToJSON DOMStorageStorageId where
  toJSON p = A.object $ catMaybes [
    ("securityOrigin" A..=) <$> (dOMStorageStorageIdSecurityOrigin p),
    ("storageKey" A..=) <$> (dOMStorageStorageIdStorageKey p),
    ("isLocalStorage" A..=) <$> Just (dOMStorageStorageIdIsLocalStorage p)
    ]

-- | Type 'DOMStorage.Item'.
--   DOM Storage item.
type DOMStorageItem = [T.Text]

-- | Type of the 'DOMStorage.domStorageItemAdded' event.
data DOMStorageDomStorageItemAdded = DOMStorageDomStorageItemAdded
  {
    dOMStorageDomStorageItemAddedStorageId :: DOMStorageStorageId,
    dOMStorageDomStorageItemAddedKey :: T.Text,
    dOMStorageDomStorageItemAddedNewValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageDomStorageItemAdded where
  parseJSON = A.withObject "DOMStorageDomStorageItemAdded" $ \o -> DOMStorageDomStorageItemAdded
    <$> o A..: "storageId"
    <*> o A..: "key"
    <*> o A..: "newValue"
instance Event DOMStorageDomStorageItemAdded where
  eventName _ = "DOMStorage.domStorageItemAdded"

-- | Type of the 'DOMStorage.domStorageItemRemoved' event.
data DOMStorageDomStorageItemRemoved = DOMStorageDomStorageItemRemoved
  {
    dOMStorageDomStorageItemRemovedStorageId :: DOMStorageStorageId,
    dOMStorageDomStorageItemRemovedKey :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageDomStorageItemRemoved where
  parseJSON = A.withObject "DOMStorageDomStorageItemRemoved" $ \o -> DOMStorageDomStorageItemRemoved
    <$> o A..: "storageId"
    <*> o A..: "key"
instance Event DOMStorageDomStorageItemRemoved where
  eventName _ = "DOMStorage.domStorageItemRemoved"

-- | Type of the 'DOMStorage.domStorageItemUpdated' event.
data DOMStorageDomStorageItemUpdated = DOMStorageDomStorageItemUpdated
  {
    dOMStorageDomStorageItemUpdatedStorageId :: DOMStorageStorageId,
    dOMStorageDomStorageItemUpdatedKey :: T.Text,
    dOMStorageDomStorageItemUpdatedOldValue :: T.Text,
    dOMStorageDomStorageItemUpdatedNewValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageDomStorageItemUpdated where
  parseJSON = A.withObject "DOMStorageDomStorageItemUpdated" $ \o -> DOMStorageDomStorageItemUpdated
    <$> o A..: "storageId"
    <*> o A..: "key"
    <*> o A..: "oldValue"
    <*> o A..: "newValue"
instance Event DOMStorageDomStorageItemUpdated where
  eventName _ = "DOMStorage.domStorageItemUpdated"

-- | Type of the 'DOMStorage.domStorageItemsCleared' event.
data DOMStorageDomStorageItemsCleared = DOMStorageDomStorageItemsCleared
  {
    dOMStorageDomStorageItemsClearedStorageId :: DOMStorageStorageId
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageDomStorageItemsCleared where
  parseJSON = A.withObject "DOMStorageDomStorageItemsCleared" $ \o -> DOMStorageDomStorageItemsCleared
    <$> o A..: "storageId"
instance Event DOMStorageDomStorageItemsCleared where
  eventName _ = "DOMStorage.domStorageItemsCleared"


-- | Parameters of the 'DOMStorage.clear' command.
data PDOMStorageClear = PDOMStorageClear
  {
    pDOMStorageClearStorageId :: DOMStorageStorageId
  }
  deriving (Eq, Show)
pDOMStorageClear
  :: DOMStorageStorageId
  -> PDOMStorageClear
pDOMStorageClear
  arg_pDOMStorageClearStorageId
  = PDOMStorageClear
    arg_pDOMStorageClearStorageId
instance ToJSON PDOMStorageClear where
  toJSON p = A.object $ catMaybes [
    ("storageId" A..=) <$> Just (pDOMStorageClearStorageId p)
    ]
instance Command PDOMStorageClear where
  type CommandResponse PDOMStorageClear = ()
  commandName _ = "DOMStorage.clear"
  fromJSON = const . A.Success . const ()

-- | Disables storage tracking, prevents storage events from being sent to the client.

-- | Parameters of the 'DOMStorage.disable' command.
data PDOMStorageDisable = PDOMStorageDisable
  deriving (Eq, Show)
pDOMStorageDisable
  :: PDOMStorageDisable
pDOMStorageDisable
  = PDOMStorageDisable
instance ToJSON PDOMStorageDisable where
  toJSON _ = A.Null
instance Command PDOMStorageDisable where
  type CommandResponse PDOMStorageDisable = ()
  commandName _ = "DOMStorage.disable"
  fromJSON = const . A.Success . const ()

-- | Enables storage tracking, storage events will now be delivered to the client.

-- | Parameters of the 'DOMStorage.enable' command.
data PDOMStorageEnable = PDOMStorageEnable
  deriving (Eq, Show)
pDOMStorageEnable
  :: PDOMStorageEnable
pDOMStorageEnable
  = PDOMStorageEnable
instance ToJSON PDOMStorageEnable where
  toJSON _ = A.Null
instance Command PDOMStorageEnable where
  type CommandResponse PDOMStorageEnable = ()
  commandName _ = "DOMStorage.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'DOMStorage.getDOMStorageItems' command.
data PDOMStorageGetDOMStorageItems = PDOMStorageGetDOMStorageItems
  {
    pDOMStorageGetDOMStorageItemsStorageId :: DOMStorageStorageId
  }
  deriving (Eq, Show)
pDOMStorageGetDOMStorageItems
  :: DOMStorageStorageId
  -> PDOMStorageGetDOMStorageItems
pDOMStorageGetDOMStorageItems
  arg_pDOMStorageGetDOMStorageItemsStorageId
  = PDOMStorageGetDOMStorageItems
    arg_pDOMStorageGetDOMStorageItemsStorageId
instance ToJSON PDOMStorageGetDOMStorageItems where
  toJSON p = A.object $ catMaybes [
    ("storageId" A..=) <$> Just (pDOMStorageGetDOMStorageItemsStorageId p)
    ]
data DOMStorageGetDOMStorageItems = DOMStorageGetDOMStorageItems
  {
    dOMStorageGetDOMStorageItemsEntries :: [DOMStorageItem]
  }
  deriving (Eq, Show)
instance FromJSON DOMStorageGetDOMStorageItems where
  parseJSON = A.withObject "DOMStorageGetDOMStorageItems" $ \o -> DOMStorageGetDOMStorageItems
    <$> o A..: "entries"
instance Command PDOMStorageGetDOMStorageItems where
  type CommandResponse PDOMStorageGetDOMStorageItems = DOMStorageGetDOMStorageItems
  commandName _ = "DOMStorage.getDOMStorageItems"


-- | Parameters of the 'DOMStorage.removeDOMStorageItem' command.
data PDOMStorageRemoveDOMStorageItem = PDOMStorageRemoveDOMStorageItem
  {
    pDOMStorageRemoveDOMStorageItemStorageId :: DOMStorageStorageId,
    pDOMStorageRemoveDOMStorageItemKey :: T.Text
  }
  deriving (Eq, Show)
pDOMStorageRemoveDOMStorageItem
  :: DOMStorageStorageId
  -> T.Text
  -> PDOMStorageRemoveDOMStorageItem
pDOMStorageRemoveDOMStorageItem
  arg_pDOMStorageRemoveDOMStorageItemStorageId
  arg_pDOMStorageRemoveDOMStorageItemKey
  = PDOMStorageRemoveDOMStorageItem
    arg_pDOMStorageRemoveDOMStorageItemStorageId
    arg_pDOMStorageRemoveDOMStorageItemKey
instance ToJSON PDOMStorageRemoveDOMStorageItem where
  toJSON p = A.object $ catMaybes [
    ("storageId" A..=) <$> Just (pDOMStorageRemoveDOMStorageItemStorageId p),
    ("key" A..=) <$> Just (pDOMStorageRemoveDOMStorageItemKey p)
    ]
instance Command PDOMStorageRemoveDOMStorageItem where
  type CommandResponse PDOMStorageRemoveDOMStorageItem = ()
  commandName _ = "DOMStorage.removeDOMStorageItem"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'DOMStorage.setDOMStorageItem' command.
data PDOMStorageSetDOMStorageItem = PDOMStorageSetDOMStorageItem
  {
    pDOMStorageSetDOMStorageItemStorageId :: DOMStorageStorageId,
    pDOMStorageSetDOMStorageItemKey :: T.Text,
    pDOMStorageSetDOMStorageItemValue :: T.Text
  }
  deriving (Eq, Show)
pDOMStorageSetDOMStorageItem
  :: DOMStorageStorageId
  -> T.Text
  -> T.Text
  -> PDOMStorageSetDOMStorageItem
pDOMStorageSetDOMStorageItem
  arg_pDOMStorageSetDOMStorageItemStorageId
  arg_pDOMStorageSetDOMStorageItemKey
  arg_pDOMStorageSetDOMStorageItemValue
  = PDOMStorageSetDOMStorageItem
    arg_pDOMStorageSetDOMStorageItemStorageId
    arg_pDOMStorageSetDOMStorageItemKey
    arg_pDOMStorageSetDOMStorageItemValue
instance ToJSON PDOMStorageSetDOMStorageItem where
  toJSON p = A.object $ catMaybes [
    ("storageId" A..=) <$> Just (pDOMStorageSetDOMStorageItemStorageId p),
    ("key" A..=) <$> Just (pDOMStorageSetDOMStorageItemKey p),
    ("value" A..=) <$> Just (pDOMStorageSetDOMStorageItemValue p)
    ]
instance Command PDOMStorageSetDOMStorageItem where
  type CommandResponse PDOMStorageSetDOMStorageItem = ()
  commandName _ = "DOMStorage.setDOMStorageItem"
  fromJSON = const . A.Success . const ()

