{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  DOMStorage :
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
type DOMStorageSerializedStorageKey = String

-- | Type 'DOMStorage.StorageId'.
--   DOM Storage identifier.
data DOMStorageStorageId = DOMStorageStorageId {
  -- | Security origin for the storage.
  dOMStorageStorageIdSecurityOrigin :: Maybe String,
  -- | Represents a key by which DOM Storage keys its CachedStorageAreas
  dOMStorageStorageIdStorageKey :: Maybe DOMStorageSerializedStorageKey,
  -- | Whether the storage is local storage (not session storage).
  dOMStorageStorageIdIsLocalStorage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMStorageStorageId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMStorageStorageId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'DOMStorage.Item'.
--   DOM Storage item.
type DOMStorageItem = [String]



-- | Type of the 'DOMStorage.domStorageItemAdded' event.
data DOMStorageDomStorageItemAdded = DOMStorageDomStorageItemAdded {
  dOMStorageDomStorageItemAddedStorageId :: DOMStorageStorageId,
  dOMStorageDomStorageItemAddedKey :: String,
  dOMStorageDomStorageItemAddedNewValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMStorageDomStorageItemAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DOMStorageDomStorageItemAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Event DOMStorageDomStorageItemAdded where
    eventName _ = "DOMStorage.domStorageItemAdded"

-- | Type of the 'DOMStorage.domStorageItemRemoved' event.
data DOMStorageDomStorageItemRemoved = DOMStorageDomStorageItemRemoved {
  dOMStorageDomStorageItemRemovedStorageId :: DOMStorageStorageId,
  dOMStorageDomStorageItemRemovedKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMStorageDomStorageItemRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DOMStorageDomStorageItemRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Event DOMStorageDomStorageItemRemoved where
    eventName _ = "DOMStorage.domStorageItemRemoved"

-- | Type of the 'DOMStorage.domStorageItemUpdated' event.
data DOMStorageDomStorageItemUpdated = DOMStorageDomStorageItemUpdated {
  dOMStorageDomStorageItemUpdatedStorageId :: DOMStorageStorageId,
  dOMStorageDomStorageItemUpdatedKey :: String,
  dOMStorageDomStorageItemUpdatedOldValue :: String,
  dOMStorageDomStorageItemUpdatedNewValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMStorageDomStorageItemUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DOMStorageDomStorageItemUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Event DOMStorageDomStorageItemUpdated where
    eventName _ = "DOMStorage.domStorageItemUpdated"

-- | Type of the 'DOMStorage.domStorageItemsCleared' event.
data DOMStorageDomStorageItemsCleared = DOMStorageDomStorageItemsCleared {
  dOMStorageDomStorageItemsClearedStorageId :: DOMStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMStorageDomStorageItemsCleared  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  DOMStorageDomStorageItemsCleared where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event DOMStorageDomStorageItemsCleared where
    eventName _ = "DOMStorage.domStorageItemsCleared"



-- | DOMStorage.clear

-- | Parameters of the 'DOMStorage.clear' command.
data PDOMStorageClear = PDOMStorageClear {
  pDOMStorageClearStorageId :: DOMStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageClear  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageClear where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command PDOMStorageClear where
   type CommandResponse PDOMStorageClear = ()
   commandName _ = "DOMStorage.clear"
   fromJSON = const . A.Success . const ()


-- | DOMStorage.disable
--   Disables storage tracking, prevents storage events from being sent to the client.

-- | Parameters of the 'DOMStorage.disable' command.
data PDOMStorageDisable = PDOMStorageDisable
instance ToJSON PDOMStorageDisable where toJSON _ = A.Null

instance Command PDOMStorageDisable where
   type CommandResponse PDOMStorageDisable = ()
   commandName _ = "DOMStorage.disable"
   fromJSON = const . A.Success . const ()


-- | DOMStorage.enable
--   Enables storage tracking, storage events will now be delivered to the client.

-- | Parameters of the 'DOMStorage.enable' command.
data PDOMStorageEnable = PDOMStorageEnable
instance ToJSON PDOMStorageEnable where toJSON _ = A.Null

instance Command PDOMStorageEnable where
   type CommandResponse PDOMStorageEnable = ()
   commandName _ = "DOMStorage.enable"
   fromJSON = const . A.Success . const ()


-- | DOMStorage.getDOMStorageItems

-- | Parameters of the 'DOMStorage.getDOMStorageItems' command.
data PDOMStorageGetDOMStorageItems = PDOMStorageGetDOMStorageItems {
  pDOMStorageGetDOMStorageItemsStorageId :: DOMStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageGetDOMStorageItems  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageGetDOMStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Return type of the 'DOMStorage.getDOMStorageItems' command.
data DOMStorageGetDOMStorageItems = DOMStorageGetDOMStorageItems {
  dOMStorageGetDOMStorageItemsEntries :: [DOMStorageItem]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMStorageGetDOMStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command PDOMStorageGetDOMStorageItems where
   type CommandResponse PDOMStorageGetDOMStorageItems = DOMStorageGetDOMStorageItems
   commandName _ = "DOMStorage.getDOMStorageItems"



-- | DOMStorage.removeDOMStorageItem

-- | Parameters of the 'DOMStorage.removeDOMStorageItem' command.
data PDOMStorageRemoveDOMStorageItem = PDOMStorageRemoveDOMStorageItem {
  pDOMStorageRemoveDOMStorageItemStorageId :: DOMStorageStorageId,
  pDOMStorageRemoveDOMStorageItemKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageRemoveDOMStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageRemoveDOMStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PDOMStorageRemoveDOMStorageItem where
   type CommandResponse PDOMStorageRemoveDOMStorageItem = ()
   commandName _ = "DOMStorage.removeDOMStorageItem"
   fromJSON = const . A.Success . const ()


-- | DOMStorage.setDOMStorageItem

-- | Parameters of the 'DOMStorage.setDOMStorageItem' command.
data PDOMStorageSetDOMStorageItem = PDOMStorageSetDOMStorageItem {
  pDOMStorageSetDOMStorageItemStorageId :: DOMStorageStorageId,
  pDOMStorageSetDOMStorageItemKey :: String,
  pDOMStorageSetDOMStorageItemValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageSetDOMStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageSetDOMStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PDOMStorageSetDOMStorageItem where
   type CommandResponse PDOMStorageSetDOMStorageItem = ()
   commandName _ = "DOMStorage.setDOMStorageItem"
   fromJSON = const . A.Success . const ()



