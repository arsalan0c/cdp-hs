{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


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

import CDP.Internal.Runtime
import CDP.Handle




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



-- | Parameters of the 'dOMStorageClear' command.
data PDOMStorageClear = PDOMStorageClear {
  pDOMStorageClearStorageId :: DOMStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageClear  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageClear where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOMStorage.clear' command.
--   
--   Parameters: 'PDOMStorageClear'
dOMStorageClear :: Handle -> PDOMStorageClear -> IO ()
dOMStorageClear handle params = sendReceiveCommand handle "DOMStorage.clear" (Just params)


-- | Function for the 'DOMStorage.disable' command.
--   Disables storage tracking, prevents storage events from being sent to the client.
dOMStorageDisable :: Handle -> IO ()
dOMStorageDisable handle = sendReceiveCommand handle "DOMStorage.disable" (Nothing :: Maybe ())


-- | Function for the 'DOMStorage.enable' command.
--   Enables storage tracking, storage events will now be delivered to the client.
dOMStorageEnable :: Handle -> IO ()
dOMStorageEnable handle = sendReceiveCommand handle "DOMStorage.enable" (Nothing :: Maybe ())


-- | Parameters of the 'dOMStorageGetDOMStorageItems' command.
data PDOMStorageGetDOMStorageItems = PDOMStorageGetDOMStorageItems {
  pDOMStorageGetDOMStorageItemsStorageId :: DOMStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageGetDOMStorageItems  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageGetDOMStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'DOMStorage.getDOMStorageItems' command.
--   
--   Parameters: 'PDOMStorageGetDOMStorageItems'
--   Returns: 'DOMStorageGetDOMStorageItems'
dOMStorageGetDOMStorageItems :: Handle -> PDOMStorageGetDOMStorageItems -> IO DOMStorageGetDOMStorageItems
dOMStorageGetDOMStorageItems handle params = sendReceiveCommandResult handle "DOMStorage.getDOMStorageItems" (Just params)

-- | Return type of the 'dOMStorageGetDOMStorageItems' command.
data DOMStorageGetDOMStorageItems = DOMStorageGetDOMStorageItems {
  dOMStorageGetDOMStorageItemsEntries :: [DOMStorageItem]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMStorageGetDOMStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DOMStorageGetDOMStorageItems where
   commandName _ = "DOMStorage.getDOMStorageItems"



-- | Parameters of the 'dOMStorageRemoveDOMStorageItem' command.
data PDOMStorageRemoveDOMStorageItem = PDOMStorageRemoveDOMStorageItem {
  pDOMStorageRemoveDOMStorageItemStorageId :: DOMStorageStorageId,
  pDOMStorageRemoveDOMStorageItemKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageRemoveDOMStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageRemoveDOMStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'DOMStorage.removeDOMStorageItem' command.
--   
--   Parameters: 'PDOMStorageRemoveDOMStorageItem'
dOMStorageRemoveDOMStorageItem :: Handle -> PDOMStorageRemoveDOMStorageItem -> IO ()
dOMStorageRemoveDOMStorageItem handle params = sendReceiveCommand handle "DOMStorage.removeDOMStorageItem" (Just params)


-- | Parameters of the 'dOMStorageSetDOMStorageItem' command.
data PDOMStorageSetDOMStorageItem = PDOMStorageSetDOMStorageItem {
  pDOMStorageSetDOMStorageItemStorageId :: DOMStorageStorageId,
  pDOMStorageSetDOMStorageItemKey :: String,
  pDOMStorageSetDOMStorageItemValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMStorageSetDOMStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMStorageSetDOMStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOMStorage.setDOMStorageItem' command.
--   
--   Parameters: 'PDOMStorageSetDOMStorageItem'
dOMStorageSetDOMStorageItem :: Handle -> PDOMStorageSetDOMStorageItem -> IO ()
dOMStorageSetDOMStorageItem handle params = sendReceiveCommand handle "DOMStorage.setDOMStorageItem" (Just params)



