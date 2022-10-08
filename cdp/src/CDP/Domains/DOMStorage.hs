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



-- | Type 'DOMStorage.SerializedStorageKey'.
type DomStorageSerializedStorageKey = String

-- | Type 'DOMStorage.StorageId'.
--   DOM Storage identifier.
data DomStorageStorageId = DomStorageStorageId {
  -- | Security origin for the storage.
  domStorageStorageIdSecurityOrigin :: Maybe String,
  -- | Represents a key by which DOM Storage keys its CachedStorageAreas
  domStorageStorageIdStorageKey :: Maybe DomStorageSerializedStorageKey,
  -- | Whether the storage is local storage (not session storage).
  domStorageStorageIdIsLocalStorage :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageStorageId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomStorageStorageId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'DOMStorage.Item'.
--   DOM Storage item.
type DomStorageItem = [String]



-- | Type of the 'DOMStorage.domStorageItemAdded' event.
data DomStorageDomStorageItemAdded = DomStorageDomStorageItemAdded {
  domStorageDomStorageItemAddedStorageId :: DomStorageStorageId,
  domStorageDomStorageItemAddedKey :: String,
  domStorageDomStorageItemAddedNewValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'DOMStorage.domStorageItemRemoved' event.
data DomStorageDomStorageItemRemoved = DomStorageDomStorageItemRemoved {
  domStorageDomStorageItemRemovedStorageId :: DomStorageStorageId,
  domStorageDomStorageItemRemovedKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'DOMStorage.domStorageItemUpdated' event.
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



-- | Type of the 'DOMStorage.domStorageItemsCleared' event.
data DomStorageDomStorageItemsCleared = DomStorageDomStorageItemsCleared {
  domStorageDomStorageItemsClearedStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemsCleared  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemsCleared where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }





-- | Parameters of the 'domStorageClear' command.
data PDomStorageClear = PDomStorageClear {
  pDomStorageClearStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageClear  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomStorageClear where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOMStorage.clear' command.
--   
--   Parameters: 'PDomStorageClear'
domStorageClear :: Handle ev -> Maybe String -> PDomStorageClear -> IO ()
domStorageClear handle sessionId params = sendReceiveCommand handle sessionId "DOMStorage.clear" (Just params )


-- | Function for the 'DOMStorage.disable' command.
--   Disables storage tracking, prevents storage events from being sent to the client.
domStorageDisable :: Handle ev -> Maybe String -> IO ()
domStorageDisable handle sessionId = sendReceiveCommand handle sessionId "DOMStorage.disable" (Nothing :: Maybe ())


-- | Function for the 'DOMStorage.enable' command.
--   Enables storage tracking, storage events will now be delivered to the client.
domStorageEnable :: Handle ev -> Maybe String -> IO ()
domStorageEnable handle sessionId = sendReceiveCommand handle sessionId "DOMStorage.enable" (Nothing :: Maybe ())


-- | Parameters of the 'domStorageGetDomStorageItems' command.
data PDomStorageGetDomStorageItems = PDomStorageGetDomStorageItems {
  pDomStorageGetDomStorageItemsStorageId :: DomStorageStorageId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageGetDomStorageItems  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'DOMStorage.getDOMStorageItems' command.
--   
--   Parameters: 'PDomStorageGetDomStorageItems'
--   Returns: 'DomStorageGetDomStorageItems'
domStorageGetDomStorageItems :: Handle ev -> Maybe String -> PDomStorageGetDomStorageItems -> IO DomStorageGetDomStorageItems
domStorageGetDomStorageItems handle sessionId params = sendReceiveCommandResult handle sessionId "DOMStorage.getDOMStorageItems" (Just params )

-- | Return type of the 'domStorageGetDomStorageItems' command.
data DomStorageGetDomStorageItems = DomStorageGetDomStorageItems {
  domStorageGetDomStorageItemsEntries :: [DomStorageItem]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomStorageGetDomStorageItems where
   commandName _ = "DOMStorage.getDOMStorageItems"



-- | Parameters of the 'domStorageRemoveDomStorageItem' command.
data PDomStorageRemoveDomStorageItem = PDomStorageRemoveDomStorageItem {
  pDomStorageRemoveDomStorageItemStorageId :: DomStorageStorageId,
  pDomStorageRemoveDomStorageItemKey :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageRemoveDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomStorageRemoveDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'DOMStorage.removeDOMStorageItem' command.
--   
--   Parameters: 'PDomStorageRemoveDomStorageItem'
domStorageRemoveDomStorageItem :: Handle ev -> Maybe String -> PDomStorageRemoveDomStorageItem -> IO ()
domStorageRemoveDomStorageItem handle sessionId params = sendReceiveCommand handle sessionId "DOMStorage.removeDOMStorageItem" (Just params )


-- | Parameters of the 'domStorageSetDomStorageItem' command.
data PDomStorageSetDomStorageItem = PDomStorageSetDomStorageItem {
  pDomStorageSetDomStorageItemStorageId :: DomStorageStorageId,
  pDomStorageSetDomStorageItemKey :: String,
  pDomStorageSetDomStorageItemValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageSetDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomStorageSetDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOMStorage.setDOMStorageItem' command.
--   
--   Parameters: 'PDomStorageSetDomStorageItem'
domStorageSetDomStorageItem :: Handle ev -> Maybe String -> PDomStorageSetDomStorageItem -> IO ()
domStorageSetDomStorageItem handle sessionId params = sendReceiveCommand handle sessionId "DOMStorage.setDOMStorageItem" (Just params )



