{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.DOMSnapshot (module CDP.Domains.DOMSnapshot) where

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

import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity



data DomSnapshotDomNode = DomSnapshotDomNode {
   domSnapshotDomNodeNodeType :: Int,
   domSnapshotDomNodeNodeName :: String,
   domSnapshotDomNodeNodeValue :: String,
   domSnapshotDomNodeTextValue :: Maybe String,
   domSnapshotDomNodeInputValue :: Maybe String,
   domSnapshotDomNodeInputChecked :: Maybe Bool,
   domSnapshotDomNodeOptionSelected :: Maybe Bool,
   domSnapshotDomNodeBackendNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   domSnapshotDomNodeChildNodeIndexes :: Maybe [Int],
   domSnapshotDomNodeAttributes :: Maybe [DomSnapshotNameValue],
   domSnapshotDomNodePseudoElementIndexes :: Maybe [Int],
   domSnapshotDomNodeLayoutNodeIndex :: Maybe Int,
   domSnapshotDomNodeDocumentUrl :: Maybe String,
   domSnapshotDomNodeBaseUrl :: Maybe String,
   domSnapshotDomNodeContentLanguage :: Maybe String,
   domSnapshotDomNodeDocumentEncoding :: Maybe String,
   domSnapshotDomNodePublicId :: Maybe String,
   domSnapshotDomNodeSystemId :: Maybe String,
   domSnapshotDomNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
   domSnapshotDomNodeContentDocumentIndex :: Maybe Int,
   domSnapshotDomNodePseudoType :: Maybe DOMPageNetworkEmulationSecurity.DomPseudoType,
   domSnapshotDomNodeShadowRootType :: Maybe DOMPageNetworkEmulationSecurity.DomShadowRootType,
   domSnapshotDomNodeIsClickable :: Maybe Bool,
   domSnapshotDomNodeEventListeners :: Maybe [DOMDebugger.DomDebuggerEventListener],
   domSnapshotDomNodeCurrentSourceUrl :: Maybe String,
   domSnapshotDomNodeOriginUrl :: Maybe String,
   domSnapshotDomNodeScrollOffsetX :: Maybe Double,
   domSnapshotDomNodeScrollOffsetY :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data DomSnapshotInlineTextBox = DomSnapshotInlineTextBox {
   domSnapshotInlineTextBoxBoundingBox :: DOMPageNetworkEmulationSecurity.DomRect,
   domSnapshotInlineTextBoxStartCharacterIndex :: Int,
   domSnapshotInlineTextBoxNumCharacters :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotInlineTextBox  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotInlineTextBox where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data DomSnapshotLayoutTreeNode = DomSnapshotLayoutTreeNode {
   domSnapshotLayoutTreeNodeDomNodeIndex :: Int,
   domSnapshotLayoutTreeNodeBoundingBox :: DOMPageNetworkEmulationSecurity.DomRect,
   domSnapshotLayoutTreeNodeLayoutText :: Maybe String,
   domSnapshotLayoutTreeNodeInlineTextNodes :: Maybe [DomSnapshotInlineTextBox],
   domSnapshotLayoutTreeNodeStyleIndex :: Maybe Int,
   domSnapshotLayoutTreeNodePaintOrder :: Maybe Int,
   domSnapshotLayoutTreeNodeIsStackingContext :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data DomSnapshotComputedStyle = DomSnapshotComputedStyle {
   domSnapshotComputedStyleProperties :: [DomSnapshotNameValue]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotComputedStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotComputedStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data DomSnapshotNameValue = DomSnapshotNameValue {
   domSnapshotNameValueName :: String,
   domSnapshotNameValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotNameValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotNameValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


type DomSnapshotStringIndex = Int
type DomSnapshotArrayOfStrings = [DomSnapshotStringIndex]

data DomSnapshotRareStringData = DomSnapshotRareStringData {
   domSnapshotRareStringDataIndex :: [Int],
   domSnapshotRareStringDataValue :: [DomSnapshotStringIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareStringData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareStringData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data DomSnapshotRareBooleanData = DomSnapshotRareBooleanData {
   domSnapshotRareBooleanDataIndex :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareBooleanData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareBooleanData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data DomSnapshotRareIntegerData = DomSnapshotRareIntegerData {
   domSnapshotRareIntegerDataIndex :: [Int],
   domSnapshotRareIntegerDataValue :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareIntegerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareIntegerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


type DomSnapshotRectangle = [Double]

data DomSnapshotDocumentSnapshot = DomSnapshotDocumentSnapshot {
   domSnapshotDocumentSnapshotDocumentUrl :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotTitle :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotBaseUrl :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotContentLanguage :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotEncodingName :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotPublicId :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotSystemId :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotFrameId :: DomSnapshotStringIndex,
   domSnapshotDocumentSnapshotNodes :: DomSnapshotNodeTreeSnapshot,
   domSnapshotDocumentSnapshotLayout :: DomSnapshotLayoutTreeSnapshot,
   domSnapshotDocumentSnapshotTextBoxes :: DomSnapshotTextBoxSnapshot,
   domSnapshotDocumentSnapshotScrollOffsetX :: Maybe Double,
   domSnapshotDocumentSnapshotScrollOffsetY :: Maybe Double,
   domSnapshotDocumentSnapshotContentWidth :: Maybe Double,
   domSnapshotDocumentSnapshotContentHeight :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDocumentSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDocumentSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data DomSnapshotNodeTreeSnapshot = DomSnapshotNodeTreeSnapshot {
   domSnapshotNodeTreeSnapshotParentIndex :: Maybe [Int],
   domSnapshotNodeTreeSnapshotNodeType :: Maybe [Int],
   domSnapshotNodeTreeSnapshotShadowRootType :: Maybe DomSnapshotRareStringData,
   domSnapshotNodeTreeSnapshotNodeName :: Maybe [DomSnapshotStringIndex],
   domSnapshotNodeTreeSnapshotNodeValue :: Maybe [DomSnapshotStringIndex],
   domSnapshotNodeTreeSnapshotBackendNodeId :: Maybe [DOMPageNetworkEmulationSecurity.DomBackendNodeId],
   domSnapshotNodeTreeSnapshotAttributes :: Maybe [DomSnapshotArrayOfStrings],
   domSnapshotNodeTreeSnapshotTextValue :: Maybe DomSnapshotRareStringData,
   domSnapshotNodeTreeSnapshotInputValue :: Maybe DomSnapshotRareStringData,
   domSnapshotNodeTreeSnapshotInputChecked :: Maybe DomSnapshotRareBooleanData,
   domSnapshotNodeTreeSnapshotOptionSelected :: Maybe DomSnapshotRareBooleanData,
   domSnapshotNodeTreeSnapshotContentDocumentIndex :: Maybe DomSnapshotRareIntegerData,
   domSnapshotNodeTreeSnapshotPseudoType :: Maybe DomSnapshotRareStringData,
   domSnapshotNodeTreeSnapshotIsClickable :: Maybe DomSnapshotRareBooleanData,
   domSnapshotNodeTreeSnapshotCurrentSourceUrl :: Maybe DomSnapshotRareStringData,
   domSnapshotNodeTreeSnapshotOriginUrl :: Maybe DomSnapshotRareStringData
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotNodeTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotNodeTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data DomSnapshotLayoutTreeSnapshot = DomSnapshotLayoutTreeSnapshot {
   domSnapshotLayoutTreeSnapshotNodeIndex :: [Int],
   domSnapshotLayoutTreeSnapshotStyles :: [DomSnapshotArrayOfStrings],
   domSnapshotLayoutTreeSnapshotBounds :: [DomSnapshotRectangle],
   domSnapshotLayoutTreeSnapshotText :: [DomSnapshotStringIndex],
   domSnapshotLayoutTreeSnapshotStackingContexts :: DomSnapshotRareBooleanData,
   domSnapshotLayoutTreeSnapshotPaintOrders :: Maybe [Int],
   domSnapshotLayoutTreeSnapshotOffsetRects :: Maybe [DomSnapshotRectangle],
   domSnapshotLayoutTreeSnapshotScrollRects :: Maybe [DomSnapshotRectangle],
   domSnapshotLayoutTreeSnapshotClientRects :: Maybe [DomSnapshotRectangle],
   domSnapshotLayoutTreeSnapshotBlendedBackgroundColors :: Maybe [DomSnapshotStringIndex],
   domSnapshotLayoutTreeSnapshotTextColorOpacities :: Maybe [Double]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data DomSnapshotTextBoxSnapshot = DomSnapshotTextBoxSnapshot {
   domSnapshotTextBoxSnapshotLayoutIndex :: [Int],
   domSnapshotTextBoxSnapshotBounds :: [DomSnapshotRectangle],
   domSnapshotTextBoxSnapshotStart :: [Int],
   domSnapshotTextBoxSnapshotLength :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotTextBoxSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotTextBoxSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }






domSnapshotDisable :: Handle ev -> IO (Maybe Error)
domSnapshotDisable handle = sendReceiveCommand handle "DOMSnapshot.disable" (Nothing :: Maybe ())


domSnapshotEnable :: Handle ev -> IO (Maybe Error)
domSnapshotEnable handle = sendReceiveCommand handle "DOMSnapshot.enable" (Nothing :: Maybe ())



data PDomSnapshotCaptureSnapshot = PDomSnapshotCaptureSnapshot {
   pDomSnapshotCaptureSnapshotComputedStyles :: [String],
   pDomSnapshotCaptureSnapshotIncludePaintOrder :: Maybe Bool,
   pDomSnapshotCaptureSnapshotIncludeDomRects :: Maybe Bool,
   pDomSnapshotCaptureSnapshotIncludeBlendedBackgroundColors :: Maybe Bool,
   pDomSnapshotCaptureSnapshotIncludeTextColorOpacities :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSnapshotCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


domSnapshotCaptureSnapshot :: Handle ev -> PDomSnapshotCaptureSnapshot -> IO (Either Error DomSnapshotCaptureSnapshot)
domSnapshotCaptureSnapshot handle params = sendReceiveCommandResult handle "DOMSnapshot.captureSnapshot" (Just params)

data DomSnapshotCaptureSnapshot = DomSnapshotCaptureSnapshot {
   domSnapshotCaptureSnapshotDocuments :: [DomSnapshotDocumentSnapshot],
   domSnapshotCaptureSnapshotStrings :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DomSnapshotCaptureSnapshot where
   commandName _ = "DOMSnapshot.captureSnapshot"




