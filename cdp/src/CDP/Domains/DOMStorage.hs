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



-- | Type 'DOMStorage.SerializedStorageKey' .
type DomStorageSerializedStorageKey = String

-- | DOM Storage identifier.
data DomStorageStorageId = DomStorageStorageId {
   domStorageStorageIdSecurityOrigin :: DomStorageStorageIdSecurityOrigin, -- ^ Security origin for the storage.
   domStorageStorageIdStorageKey :: DomStorageStorageIdStorageKey, -- ^ Represents a key by which DOM Storage keys its CachedStorageAreas
   domStorageStorageIdIsLocalStorage :: DomStorageStorageIdIsLocalStorage -- ^ Whether the storage is local storage (not session storage).
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageStorageId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomStorageStorageId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | DOM Storage item.
type DomStorageItem = [String]



-- | Type of the 'DOMStorage.domStorageItemAdded' event.
data DomStorageDomStorageItemAdded = DomStorageDomStorageItemAdded {



} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'DOMStorage.domStorageItemRemoved' event.
data DomStorageDomStorageItemRemoved = DomStorageDomStorageItemRemoved {


} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'DOMStorage.domStorageItemUpdated' event.
data DomStorageDomStorageItemUpdated = DomStorageDomStorageItemUpdated {




} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'DOMStorage.domStorageItemsCleared' event.
data DomStorageDomStorageItemsCleared = DomStorageDomStorageItemsCleared {
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomStorageDomStorageItemsCleared  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  DomStorageDomStorageItemsCleared where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }





-- | Parameters of the 'domStorageClear' command.
data PDomStorageClear = PDomStorageClear {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageClear  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomStorageClear where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'DOMStorage.clear'.
-- Parameters: 'PDomStorageClear'
domStorageClear :: Handle ev -> PDomStorageClear -> IO (Maybe Error)
domStorageClear handle params = sendReceiveCommand handle "DOMStorage.clear" (Just params)


-- | Function for the command 'DOMStorage.disable'.
-- Disables storage tracking, prevents storage events from being sent to the client.
domStorageDisable :: Handle ev -> IO (Maybe Error)
domStorageDisable handle = sendReceiveCommand handle "DOMStorage.disable" (Nothing :: Maybe ())


-- | Function for the command 'DOMStorage.enable'.
-- Enables storage tracking, storage events will now be delivered to the client.
domStorageEnable :: Handle ev -> IO (Maybe Error)
domStorageEnable handle = sendReceiveCommand handle "DOMStorage.enable" (Nothing :: Maybe ())


-- | Parameters of the 'domStorageGetDomStorageItems' command.
data PDomStorageGetDomStorageItems = PDomStorageGetDomStorageItems {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageGetDomStorageItems  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'DOMStorage.getDOMStorageItems'.
-- Parameters: 'PDomStorageGetDomStorageItems'
-- Returns: 'DomStorageGetDomStorageItems'
domStorageGetDomStorageItems :: Handle ev -> PDomStorageGetDomStorageItems -> IO (Either Error DomStorageGetDomStorageItems)
domStorageGetDomStorageItems handle params = sendReceiveCommandResult handle "DOMStorage.getDOMStorageItems" (Just params)

-- | Return type of the 'domStorageGetDomStorageItems' command.
data DomStorageGetDomStorageItems = DomStorageGetDomStorageItems {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomStorageGetDomStorageItems where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomStorageGetDomStorageItems where
   commandName _ = "DOMStorage.getDOMStorageItems"



-- | Parameters of the 'domStorageRemoveDomStorageItem' command.
data PDomStorageRemoveDomStorageItem = PDomStorageRemoveDomStorageItem {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageRemoveDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PDomStorageRemoveDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'DOMStorage.removeDOMStorageItem'.
-- Parameters: 'PDomStorageRemoveDomStorageItem'
domStorageRemoveDomStorageItem :: Handle ev -> PDomStorageRemoveDomStorageItem -> IO (Maybe Error)
domStorageRemoveDomStorageItem handle params = sendReceiveCommand handle "DOMStorage.removeDOMStorageItem" (Just params)


-- | Parameters of the 'domStorageSetDomStorageItem' command.
data PDomStorageSetDomStorageItem = PDomStorageSetDomStorageItem {



} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomStorageSetDomStorageItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomStorageSetDomStorageItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'DOMStorage.setDOMStorageItem'.
-- Parameters: 'PDomStorageSetDomStorageItem'
domStorageSetDomStorageItem :: Handle ev -> PDomStorageSetDomStorageItem -> IO (Maybe Error)
domStorageSetDomStorageItem handle params = sendReceiveCommand handle "DOMStorage.setDOMStorageItem" (Just params)



