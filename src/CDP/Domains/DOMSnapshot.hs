{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DOMSnapshot

This domain facilitates obtaining document snapshots with DOM, layout, and style information.
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'DOMSnapshot.DOMNode'.
--   A Node in the DOM tree.
data DOMSnapshotDOMNode = DOMSnapshotDOMNode
  {
    -- | `Node`'s nodeType.
    dOMSnapshotDOMNodeNodeType :: Int,
    -- | `Node`'s nodeName.
    dOMSnapshotDOMNodeNodeName :: T.Text,
    -- | `Node`'s nodeValue.
    dOMSnapshotDOMNodeNodeValue :: T.Text,
    -- | Only set for textarea elements, contains the text value.
    dOMSnapshotDOMNodeTextValue :: Maybe T.Text,
    -- | Only set for input elements, contains the input's associated text value.
    dOMSnapshotDOMNodeInputValue :: Maybe T.Text,
    -- | Only set for radio and checkbox input elements, indicates if the element has been checked
    dOMSnapshotDOMNodeInputChecked :: Maybe Bool,
    -- | Only set for option elements, indicates if the element has been selected
    dOMSnapshotDOMNodeOptionSelected :: Maybe Bool,
    -- | `Node`'s id, corresponds to DOM.Node.backendNodeId.
    dOMSnapshotDOMNodeBackendNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | The indexes of the node's child nodes in the `domNodes` array returned by `getSnapshot`, if
    --   any.
    dOMSnapshotDOMNodeChildNodeIndexes :: Maybe [Int],
    -- | Attributes of an `Element` node.
    dOMSnapshotDOMNodeAttributes :: Maybe [DOMSnapshotNameValue],
    -- | Indexes of pseudo elements associated with this node in the `domNodes` array returned by
    --   `getSnapshot`, if any.
    dOMSnapshotDOMNodePseudoElementIndexes :: Maybe [Int],
    -- | The index of the node's related layout tree node in the `layoutTreeNodes` array returned by
    --   `getSnapshot`, if any.
    dOMSnapshotDOMNodeLayoutNodeIndex :: Maybe Int,
    -- | Document URL that `Document` or `FrameOwner` node points to.
    dOMSnapshotDOMNodeDocumentURL :: Maybe T.Text,
    -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
    dOMSnapshotDOMNodeBaseURL :: Maybe T.Text,
    -- | Only set for documents, contains the document's content language.
    dOMSnapshotDOMNodeContentLanguage :: Maybe T.Text,
    -- | Only set for documents, contains the document's character set encoding.
    dOMSnapshotDOMNodeDocumentEncoding :: Maybe T.Text,
    -- | `DocumentType` node's publicId.
    dOMSnapshotDOMNodePublicId :: Maybe T.Text,
    -- | `DocumentType` node's systemId.
    dOMSnapshotDOMNodeSystemId :: Maybe T.Text,
    -- | Frame ID for frame owner elements and also for the document node.
    dOMSnapshotDOMNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | The index of a frame owner element's content document in the `domNodes` array returned by
    --   `getSnapshot`, if any.
    dOMSnapshotDOMNodeContentDocumentIndex :: Maybe Int,
    -- | Type of a pseudo element node.
    dOMSnapshotDOMNodePseudoType :: Maybe DOMPageNetworkEmulationSecurity.DOMPseudoType,
    -- | Shadow root type.
    dOMSnapshotDOMNodeShadowRootType :: Maybe DOMPageNetworkEmulationSecurity.DOMShadowRootType,
    -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
    --   event listeners attached via JavaScript as well as anchor tags that naturally navigate when
    --   clicked.
    dOMSnapshotDOMNodeIsClickable :: Maybe Bool,
    -- | Details of the node's event listeners, if any.
    dOMSnapshotDOMNodeEventListeners :: Maybe [DOMDebugger.DOMDebuggerEventListener],
    -- | The selected url for nodes with a srcset attribute.
    dOMSnapshotDOMNodeCurrentSourceURL :: Maybe T.Text,
    -- | The url of the script (if any) that generates this node.
    dOMSnapshotDOMNodeOriginURL :: Maybe T.Text,
    -- | Scroll offsets, set when this node is a Document.
    dOMSnapshotDOMNodeScrollOffsetX :: Maybe Double,
    dOMSnapshotDOMNodeScrollOffsetY :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotDOMNode where
  parseJSON = A.withObject "DOMSnapshotDOMNode" $ \o -> DOMSnapshotDOMNode
    <$> o A..: "nodeType"
    <*> o A..: "nodeName"
    <*> o A..: "nodeValue"
    <*> o A..:? "textValue"
    <*> o A..:? "inputValue"
    <*> o A..:? "inputChecked"
    <*> o A..:? "optionSelected"
    <*> o A..: "backendNodeId"
    <*> o A..:? "childNodeIndexes"
    <*> o A..:? "attributes"
    <*> o A..:? "pseudoElementIndexes"
    <*> o A..:? "layoutNodeIndex"
    <*> o A..:? "documentURL"
    <*> o A..:? "baseURL"
    <*> o A..:? "contentLanguage"
    <*> o A..:? "documentEncoding"
    <*> o A..:? "publicId"
    <*> o A..:? "systemId"
    <*> o A..:? "frameId"
    <*> o A..:? "contentDocumentIndex"
    <*> o A..:? "pseudoType"
    <*> o A..:? "shadowRootType"
    <*> o A..:? "isClickable"
    <*> o A..:? "eventListeners"
    <*> o A..:? "currentSourceURL"
    <*> o A..:? "originURL"
    <*> o A..:? "scrollOffsetX"
    <*> o A..:? "scrollOffsetY"
instance ToJSON DOMSnapshotDOMNode where
  toJSON p = A.object $ catMaybes [
    ("nodeType" A..=) <$> Just (dOMSnapshotDOMNodeNodeType p),
    ("nodeName" A..=) <$> Just (dOMSnapshotDOMNodeNodeName p),
    ("nodeValue" A..=) <$> Just (dOMSnapshotDOMNodeNodeValue p),
    ("textValue" A..=) <$> (dOMSnapshotDOMNodeTextValue p),
    ("inputValue" A..=) <$> (dOMSnapshotDOMNodeInputValue p),
    ("inputChecked" A..=) <$> (dOMSnapshotDOMNodeInputChecked p),
    ("optionSelected" A..=) <$> (dOMSnapshotDOMNodeOptionSelected p),
    ("backendNodeId" A..=) <$> Just (dOMSnapshotDOMNodeBackendNodeId p),
    ("childNodeIndexes" A..=) <$> (dOMSnapshotDOMNodeChildNodeIndexes p),
    ("attributes" A..=) <$> (dOMSnapshotDOMNodeAttributes p),
    ("pseudoElementIndexes" A..=) <$> (dOMSnapshotDOMNodePseudoElementIndexes p),
    ("layoutNodeIndex" A..=) <$> (dOMSnapshotDOMNodeLayoutNodeIndex p),
    ("documentURL" A..=) <$> (dOMSnapshotDOMNodeDocumentURL p),
    ("baseURL" A..=) <$> (dOMSnapshotDOMNodeBaseURL p),
    ("contentLanguage" A..=) <$> (dOMSnapshotDOMNodeContentLanguage p),
    ("documentEncoding" A..=) <$> (dOMSnapshotDOMNodeDocumentEncoding p),
    ("publicId" A..=) <$> (dOMSnapshotDOMNodePublicId p),
    ("systemId" A..=) <$> (dOMSnapshotDOMNodeSystemId p),
    ("frameId" A..=) <$> (dOMSnapshotDOMNodeFrameId p),
    ("contentDocumentIndex" A..=) <$> (dOMSnapshotDOMNodeContentDocumentIndex p),
    ("pseudoType" A..=) <$> (dOMSnapshotDOMNodePseudoType p),
    ("shadowRootType" A..=) <$> (dOMSnapshotDOMNodeShadowRootType p),
    ("isClickable" A..=) <$> (dOMSnapshotDOMNodeIsClickable p),
    ("eventListeners" A..=) <$> (dOMSnapshotDOMNodeEventListeners p),
    ("currentSourceURL" A..=) <$> (dOMSnapshotDOMNodeCurrentSourceURL p),
    ("originURL" A..=) <$> (dOMSnapshotDOMNodeOriginURL p),
    ("scrollOffsetX" A..=) <$> (dOMSnapshotDOMNodeScrollOffsetX p),
    ("scrollOffsetY" A..=) <$> (dOMSnapshotDOMNodeScrollOffsetY p)
    ]

-- | Type 'DOMSnapshot.InlineTextBox'.
--   Details of post layout rendered text positions. The exact layout should not be regarded as
--   stable and may change between versions.
data DOMSnapshotInlineTextBox = DOMSnapshotInlineTextBox
  {
    -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
    dOMSnapshotInlineTextBoxBoundingBox :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | The starting index in characters, for this post layout textbox substring. Characters that
    --   would be represented as a surrogate pair in UTF-16 have length 2.
    dOMSnapshotInlineTextBoxStartCharacterIndex :: Int,
    -- | The number of characters in this post layout textbox substring. Characters that would be
    --   represented as a surrogate pair in UTF-16 have length 2.
    dOMSnapshotInlineTextBoxNumCharacters :: Int
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotInlineTextBox where
  parseJSON = A.withObject "DOMSnapshotInlineTextBox" $ \o -> DOMSnapshotInlineTextBox
    <$> o A..: "boundingBox"
    <*> o A..: "startCharacterIndex"
    <*> o A..: "numCharacters"
instance ToJSON DOMSnapshotInlineTextBox where
  toJSON p = A.object $ catMaybes [
    ("boundingBox" A..=) <$> Just (dOMSnapshotInlineTextBoxBoundingBox p),
    ("startCharacterIndex" A..=) <$> Just (dOMSnapshotInlineTextBoxStartCharacterIndex p),
    ("numCharacters" A..=) <$> Just (dOMSnapshotInlineTextBoxNumCharacters p)
    ]

-- | Type 'DOMSnapshot.LayoutTreeNode'.
--   Details of an element in the DOM tree with a LayoutObject.
data DOMSnapshotLayoutTreeNode = DOMSnapshotLayoutTreeNode
  {
    -- | The index of the related DOM node in the `domNodes` array returned by `getSnapshot`.
    dOMSnapshotLayoutTreeNodeDomNodeIndex :: Int,
    -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
    dOMSnapshotLayoutTreeNodeBoundingBox :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | Contents of the LayoutText, if any.
    dOMSnapshotLayoutTreeNodeLayoutText :: Maybe T.Text,
    -- | The post-layout inline text nodes, if any.
    dOMSnapshotLayoutTreeNodeInlineTextNodes :: Maybe [DOMSnapshotInlineTextBox],
    -- | Index into the `computedStyles` array returned by `getSnapshot`.
    dOMSnapshotLayoutTreeNodeStyleIndex :: Maybe Int,
    -- | Global paint order index, which is determined by the stacking order of the nodes. Nodes
    --   that are painted together will have the same index. Only provided if includePaintOrder in
    --   getSnapshot was true.
    dOMSnapshotLayoutTreeNodePaintOrder :: Maybe Int,
    -- | Set to true to indicate the element begins a new stacking context.
    dOMSnapshotLayoutTreeNodeIsStackingContext :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotLayoutTreeNode where
  parseJSON = A.withObject "DOMSnapshotLayoutTreeNode" $ \o -> DOMSnapshotLayoutTreeNode
    <$> o A..: "domNodeIndex"
    <*> o A..: "boundingBox"
    <*> o A..:? "layoutText"
    <*> o A..:? "inlineTextNodes"
    <*> o A..:? "styleIndex"
    <*> o A..:? "paintOrder"
    <*> o A..:? "isStackingContext"
instance ToJSON DOMSnapshotLayoutTreeNode where
  toJSON p = A.object $ catMaybes [
    ("domNodeIndex" A..=) <$> Just (dOMSnapshotLayoutTreeNodeDomNodeIndex p),
    ("boundingBox" A..=) <$> Just (dOMSnapshotLayoutTreeNodeBoundingBox p),
    ("layoutText" A..=) <$> (dOMSnapshotLayoutTreeNodeLayoutText p),
    ("inlineTextNodes" A..=) <$> (dOMSnapshotLayoutTreeNodeInlineTextNodes p),
    ("styleIndex" A..=) <$> (dOMSnapshotLayoutTreeNodeStyleIndex p),
    ("paintOrder" A..=) <$> (dOMSnapshotLayoutTreeNodePaintOrder p),
    ("isStackingContext" A..=) <$> (dOMSnapshotLayoutTreeNodeIsStackingContext p)
    ]

-- | Type 'DOMSnapshot.ComputedStyle'.
--   A subset of the full ComputedStyle as defined by the request whitelist.
data DOMSnapshotComputedStyle = DOMSnapshotComputedStyle
  {
    -- | Name/value pairs of computed style properties.
    dOMSnapshotComputedStyleProperties :: [DOMSnapshotNameValue]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotComputedStyle where
  parseJSON = A.withObject "DOMSnapshotComputedStyle" $ \o -> DOMSnapshotComputedStyle
    <$> o A..: "properties"
instance ToJSON DOMSnapshotComputedStyle where
  toJSON p = A.object $ catMaybes [
    ("properties" A..=) <$> Just (dOMSnapshotComputedStyleProperties p)
    ]

-- | Type 'DOMSnapshot.NameValue'.
--   A name/value pair.
data DOMSnapshotNameValue = DOMSnapshotNameValue
  {
    -- | Attribute/property name.
    dOMSnapshotNameValueName :: T.Text,
    -- | Attribute/property value.
    dOMSnapshotNameValueValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotNameValue where
  parseJSON = A.withObject "DOMSnapshotNameValue" $ \o -> DOMSnapshotNameValue
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON DOMSnapshotNameValue where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (dOMSnapshotNameValueName p),
    ("value" A..=) <$> Just (dOMSnapshotNameValueValue p)
    ]

-- | Type 'DOMSnapshot.StringIndex'.
--   Index of the string in the strings table.
type DOMSnapshotStringIndex = Int

-- | Type 'DOMSnapshot.ArrayOfStrings'.
--   Index of the string in the strings table.
type DOMSnapshotArrayOfStrings = [DOMSnapshotStringIndex]

-- | Type 'DOMSnapshot.RareStringData'.
--   Data that is only present on rare nodes.
data DOMSnapshotRareStringData = DOMSnapshotRareStringData
  {
    dOMSnapshotRareStringDataIndex :: [Int],
    dOMSnapshotRareStringDataValue :: [DOMSnapshotStringIndex]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotRareStringData where
  parseJSON = A.withObject "DOMSnapshotRareStringData" $ \o -> DOMSnapshotRareStringData
    <$> o A..: "index"
    <*> o A..: "value"
instance ToJSON DOMSnapshotRareStringData where
  toJSON p = A.object $ catMaybes [
    ("index" A..=) <$> Just (dOMSnapshotRareStringDataIndex p),
    ("value" A..=) <$> Just (dOMSnapshotRareStringDataValue p)
    ]

-- | Type 'DOMSnapshot.RareBooleanData'.
data DOMSnapshotRareBooleanData = DOMSnapshotRareBooleanData
  {
    dOMSnapshotRareBooleanDataIndex :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotRareBooleanData where
  parseJSON = A.withObject "DOMSnapshotRareBooleanData" $ \o -> DOMSnapshotRareBooleanData
    <$> o A..: "index"
instance ToJSON DOMSnapshotRareBooleanData where
  toJSON p = A.object $ catMaybes [
    ("index" A..=) <$> Just (dOMSnapshotRareBooleanDataIndex p)
    ]

-- | Type 'DOMSnapshot.RareIntegerData'.
data DOMSnapshotRareIntegerData = DOMSnapshotRareIntegerData
  {
    dOMSnapshotRareIntegerDataIndex :: [Int],
    dOMSnapshotRareIntegerDataValue :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotRareIntegerData where
  parseJSON = A.withObject "DOMSnapshotRareIntegerData" $ \o -> DOMSnapshotRareIntegerData
    <$> o A..: "index"
    <*> o A..: "value"
instance ToJSON DOMSnapshotRareIntegerData where
  toJSON p = A.object $ catMaybes [
    ("index" A..=) <$> Just (dOMSnapshotRareIntegerDataIndex p),
    ("value" A..=) <$> Just (dOMSnapshotRareIntegerDataValue p)
    ]

-- | Type 'DOMSnapshot.Rectangle'.
type DOMSnapshotRectangle = [Double]

-- | Type 'DOMSnapshot.DocumentSnapshot'.
--   Document snapshot.
data DOMSnapshotDocumentSnapshot = DOMSnapshotDocumentSnapshot
  {
    -- | Document URL that `Document` or `FrameOwner` node points to.
    dOMSnapshotDocumentSnapshotDocumentURL :: DOMSnapshotStringIndex,
    -- | Document title.
    dOMSnapshotDocumentSnapshotTitle :: DOMSnapshotStringIndex,
    -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
    dOMSnapshotDocumentSnapshotBaseURL :: DOMSnapshotStringIndex,
    -- | Contains the document's content language.
    dOMSnapshotDocumentSnapshotContentLanguage :: DOMSnapshotStringIndex,
    -- | Contains the document's character set encoding.
    dOMSnapshotDocumentSnapshotEncodingName :: DOMSnapshotStringIndex,
    -- | `DocumentType` node's publicId.
    dOMSnapshotDocumentSnapshotPublicId :: DOMSnapshotStringIndex,
    -- | `DocumentType` node's systemId.
    dOMSnapshotDocumentSnapshotSystemId :: DOMSnapshotStringIndex,
    -- | Frame ID for frame owner elements and also for the document node.
    dOMSnapshotDocumentSnapshotFrameId :: DOMSnapshotStringIndex,
    -- | A table with dom nodes.
    dOMSnapshotDocumentSnapshotNodes :: DOMSnapshotNodeTreeSnapshot,
    -- | The nodes in the layout tree.
    dOMSnapshotDocumentSnapshotLayout :: DOMSnapshotLayoutTreeSnapshot,
    -- | The post-layout inline text nodes.
    dOMSnapshotDocumentSnapshotTextBoxes :: DOMSnapshotTextBoxSnapshot,
    -- | Horizontal scroll offset.
    dOMSnapshotDocumentSnapshotScrollOffsetX :: Maybe Double,
    -- | Vertical scroll offset.
    dOMSnapshotDocumentSnapshotScrollOffsetY :: Maybe Double,
    -- | Document content width.
    dOMSnapshotDocumentSnapshotContentWidth :: Maybe Double,
    -- | Document content height.
    dOMSnapshotDocumentSnapshotContentHeight :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotDocumentSnapshot where
  parseJSON = A.withObject "DOMSnapshotDocumentSnapshot" $ \o -> DOMSnapshotDocumentSnapshot
    <$> o A..: "documentURL"
    <*> o A..: "title"
    <*> o A..: "baseURL"
    <*> o A..: "contentLanguage"
    <*> o A..: "encodingName"
    <*> o A..: "publicId"
    <*> o A..: "systemId"
    <*> o A..: "frameId"
    <*> o A..: "nodes"
    <*> o A..: "layout"
    <*> o A..: "textBoxes"
    <*> o A..:? "scrollOffsetX"
    <*> o A..:? "scrollOffsetY"
    <*> o A..:? "contentWidth"
    <*> o A..:? "contentHeight"
instance ToJSON DOMSnapshotDocumentSnapshot where
  toJSON p = A.object $ catMaybes [
    ("documentURL" A..=) <$> Just (dOMSnapshotDocumentSnapshotDocumentURL p),
    ("title" A..=) <$> Just (dOMSnapshotDocumentSnapshotTitle p),
    ("baseURL" A..=) <$> Just (dOMSnapshotDocumentSnapshotBaseURL p),
    ("contentLanguage" A..=) <$> Just (dOMSnapshotDocumentSnapshotContentLanguage p),
    ("encodingName" A..=) <$> Just (dOMSnapshotDocumentSnapshotEncodingName p),
    ("publicId" A..=) <$> Just (dOMSnapshotDocumentSnapshotPublicId p),
    ("systemId" A..=) <$> Just (dOMSnapshotDocumentSnapshotSystemId p),
    ("frameId" A..=) <$> Just (dOMSnapshotDocumentSnapshotFrameId p),
    ("nodes" A..=) <$> Just (dOMSnapshotDocumentSnapshotNodes p),
    ("layout" A..=) <$> Just (dOMSnapshotDocumentSnapshotLayout p),
    ("textBoxes" A..=) <$> Just (dOMSnapshotDocumentSnapshotTextBoxes p),
    ("scrollOffsetX" A..=) <$> (dOMSnapshotDocumentSnapshotScrollOffsetX p),
    ("scrollOffsetY" A..=) <$> (dOMSnapshotDocumentSnapshotScrollOffsetY p),
    ("contentWidth" A..=) <$> (dOMSnapshotDocumentSnapshotContentWidth p),
    ("contentHeight" A..=) <$> (dOMSnapshotDocumentSnapshotContentHeight p)
    ]

-- | Type 'DOMSnapshot.NodeTreeSnapshot'.
--   Table containing nodes.
data DOMSnapshotNodeTreeSnapshot = DOMSnapshotNodeTreeSnapshot
  {
    -- | Parent node index.
    dOMSnapshotNodeTreeSnapshotParentIndex :: Maybe [Int],
    -- | `Node`'s nodeType.
    dOMSnapshotNodeTreeSnapshotNodeType :: Maybe [Int],
    -- | Type of the shadow root the `Node` is in. String values are equal to the `ShadowRootType` enum.
    dOMSnapshotNodeTreeSnapshotShadowRootType :: Maybe DOMSnapshotRareStringData,
    -- | `Node`'s nodeName.
    dOMSnapshotNodeTreeSnapshotNodeName :: Maybe [DOMSnapshotStringIndex],
    -- | `Node`'s nodeValue.
    dOMSnapshotNodeTreeSnapshotNodeValue :: Maybe [DOMSnapshotStringIndex],
    -- | `Node`'s id, corresponds to DOM.Node.backendNodeId.
    dOMSnapshotNodeTreeSnapshotBackendNodeId :: Maybe [DOMPageNetworkEmulationSecurity.DOMBackendNodeId],
    -- | Attributes of an `Element` node. Flatten name, value pairs.
    dOMSnapshotNodeTreeSnapshotAttributes :: Maybe [DOMSnapshotArrayOfStrings],
    -- | Only set for textarea elements, contains the text value.
    dOMSnapshotNodeTreeSnapshotTextValue :: Maybe DOMSnapshotRareStringData,
    -- | Only set for input elements, contains the input's associated text value.
    dOMSnapshotNodeTreeSnapshotInputValue :: Maybe DOMSnapshotRareStringData,
    -- | Only set for radio and checkbox input elements, indicates if the element has been checked
    dOMSnapshotNodeTreeSnapshotInputChecked :: Maybe DOMSnapshotRareBooleanData,
    -- | Only set for option elements, indicates if the element has been selected
    dOMSnapshotNodeTreeSnapshotOptionSelected :: Maybe DOMSnapshotRareBooleanData,
    -- | The index of the document in the list of the snapshot documents.
    dOMSnapshotNodeTreeSnapshotContentDocumentIndex :: Maybe DOMSnapshotRareIntegerData,
    -- | Type of a pseudo element node.
    dOMSnapshotNodeTreeSnapshotPseudoType :: Maybe DOMSnapshotRareStringData,
    -- | Pseudo element identifier for this node. Only present if there is a
    --   valid pseudoType.
    dOMSnapshotNodeTreeSnapshotPseudoIdentifier :: Maybe DOMSnapshotRareStringData,
    -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
    --   event listeners attached via JavaScript as well as anchor tags that naturally navigate when
    --   clicked.
    dOMSnapshotNodeTreeSnapshotIsClickable :: Maybe DOMSnapshotRareBooleanData,
    -- | The selected url for nodes with a srcset attribute.
    dOMSnapshotNodeTreeSnapshotCurrentSourceURL :: Maybe DOMSnapshotRareStringData,
    -- | The url of the script (if any) that generates this node.
    dOMSnapshotNodeTreeSnapshotOriginURL :: Maybe DOMSnapshotRareStringData
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotNodeTreeSnapshot where
  parseJSON = A.withObject "DOMSnapshotNodeTreeSnapshot" $ \o -> DOMSnapshotNodeTreeSnapshot
    <$> o A..:? "parentIndex"
    <*> o A..:? "nodeType"
    <*> o A..:? "shadowRootType"
    <*> o A..:? "nodeName"
    <*> o A..:? "nodeValue"
    <*> o A..:? "backendNodeId"
    <*> o A..:? "attributes"
    <*> o A..:? "textValue"
    <*> o A..:? "inputValue"
    <*> o A..:? "inputChecked"
    <*> o A..:? "optionSelected"
    <*> o A..:? "contentDocumentIndex"
    <*> o A..:? "pseudoType"
    <*> o A..:? "pseudoIdentifier"
    <*> o A..:? "isClickable"
    <*> o A..:? "currentSourceURL"
    <*> o A..:? "originURL"
instance ToJSON DOMSnapshotNodeTreeSnapshot where
  toJSON p = A.object $ catMaybes [
    ("parentIndex" A..=) <$> (dOMSnapshotNodeTreeSnapshotParentIndex p),
    ("nodeType" A..=) <$> (dOMSnapshotNodeTreeSnapshotNodeType p),
    ("shadowRootType" A..=) <$> (dOMSnapshotNodeTreeSnapshotShadowRootType p),
    ("nodeName" A..=) <$> (dOMSnapshotNodeTreeSnapshotNodeName p),
    ("nodeValue" A..=) <$> (dOMSnapshotNodeTreeSnapshotNodeValue p),
    ("backendNodeId" A..=) <$> (dOMSnapshotNodeTreeSnapshotBackendNodeId p),
    ("attributes" A..=) <$> (dOMSnapshotNodeTreeSnapshotAttributes p),
    ("textValue" A..=) <$> (dOMSnapshotNodeTreeSnapshotTextValue p),
    ("inputValue" A..=) <$> (dOMSnapshotNodeTreeSnapshotInputValue p),
    ("inputChecked" A..=) <$> (dOMSnapshotNodeTreeSnapshotInputChecked p),
    ("optionSelected" A..=) <$> (dOMSnapshotNodeTreeSnapshotOptionSelected p),
    ("contentDocumentIndex" A..=) <$> (dOMSnapshotNodeTreeSnapshotContentDocumentIndex p),
    ("pseudoType" A..=) <$> (dOMSnapshotNodeTreeSnapshotPseudoType p),
    ("pseudoIdentifier" A..=) <$> (dOMSnapshotNodeTreeSnapshotPseudoIdentifier p),
    ("isClickable" A..=) <$> (dOMSnapshotNodeTreeSnapshotIsClickable p),
    ("currentSourceURL" A..=) <$> (dOMSnapshotNodeTreeSnapshotCurrentSourceURL p),
    ("originURL" A..=) <$> (dOMSnapshotNodeTreeSnapshotOriginURL p)
    ]

-- | Type 'DOMSnapshot.LayoutTreeSnapshot'.
--   Table of details of an element in the DOM tree with a LayoutObject.
data DOMSnapshotLayoutTreeSnapshot = DOMSnapshotLayoutTreeSnapshot
  {
    -- | Index of the corresponding node in the `NodeTreeSnapshot` array returned by `captureSnapshot`.
    dOMSnapshotLayoutTreeSnapshotNodeIndex :: [Int],
    -- | Array of indexes specifying computed style strings, filtered according to the `computedStyles` parameter passed to `captureSnapshot`.
    dOMSnapshotLayoutTreeSnapshotStyles :: [DOMSnapshotArrayOfStrings],
    -- | The absolute position bounding box.
    dOMSnapshotLayoutTreeSnapshotBounds :: [DOMSnapshotRectangle],
    -- | Contents of the LayoutText, if any.
    dOMSnapshotLayoutTreeSnapshotText :: [DOMSnapshotStringIndex],
    -- | Stacking context information.
    dOMSnapshotLayoutTreeSnapshotStackingContexts :: DOMSnapshotRareBooleanData,
    -- | Global paint order index, which is determined by the stacking order of the nodes. Nodes
    --   that are painted together will have the same index. Only provided if includePaintOrder in
    --   captureSnapshot was true.
    dOMSnapshotLayoutTreeSnapshotPaintOrders :: Maybe [Int],
    -- | The offset rect of nodes. Only available when includeDOMRects is set to true
    dOMSnapshotLayoutTreeSnapshotOffsetRects :: Maybe [DOMSnapshotRectangle],
    -- | The scroll rect of nodes. Only available when includeDOMRects is set to true
    dOMSnapshotLayoutTreeSnapshotScrollRects :: Maybe [DOMSnapshotRectangle],
    -- | The client rect of nodes. Only available when includeDOMRects is set to true
    dOMSnapshotLayoutTreeSnapshotClientRects :: Maybe [DOMSnapshotRectangle],
    -- | The list of background colors that are blended with colors of overlapping elements.
    dOMSnapshotLayoutTreeSnapshotBlendedBackgroundColors :: Maybe [DOMSnapshotStringIndex],
    -- | The list of computed text opacities.
    dOMSnapshotLayoutTreeSnapshotTextColorOpacities :: Maybe [Double]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotLayoutTreeSnapshot where
  parseJSON = A.withObject "DOMSnapshotLayoutTreeSnapshot" $ \o -> DOMSnapshotLayoutTreeSnapshot
    <$> o A..: "nodeIndex"
    <*> o A..: "styles"
    <*> o A..: "bounds"
    <*> o A..: "text"
    <*> o A..: "stackingContexts"
    <*> o A..:? "paintOrders"
    <*> o A..:? "offsetRects"
    <*> o A..:? "scrollRects"
    <*> o A..:? "clientRects"
    <*> o A..:? "blendedBackgroundColors"
    <*> o A..:? "textColorOpacities"
instance ToJSON DOMSnapshotLayoutTreeSnapshot where
  toJSON p = A.object $ catMaybes [
    ("nodeIndex" A..=) <$> Just (dOMSnapshotLayoutTreeSnapshotNodeIndex p),
    ("styles" A..=) <$> Just (dOMSnapshotLayoutTreeSnapshotStyles p),
    ("bounds" A..=) <$> Just (dOMSnapshotLayoutTreeSnapshotBounds p),
    ("text" A..=) <$> Just (dOMSnapshotLayoutTreeSnapshotText p),
    ("stackingContexts" A..=) <$> Just (dOMSnapshotLayoutTreeSnapshotStackingContexts p),
    ("paintOrders" A..=) <$> (dOMSnapshotLayoutTreeSnapshotPaintOrders p),
    ("offsetRects" A..=) <$> (dOMSnapshotLayoutTreeSnapshotOffsetRects p),
    ("scrollRects" A..=) <$> (dOMSnapshotLayoutTreeSnapshotScrollRects p),
    ("clientRects" A..=) <$> (dOMSnapshotLayoutTreeSnapshotClientRects p),
    ("blendedBackgroundColors" A..=) <$> (dOMSnapshotLayoutTreeSnapshotBlendedBackgroundColors p),
    ("textColorOpacities" A..=) <$> (dOMSnapshotLayoutTreeSnapshotTextColorOpacities p)
    ]

-- | Type 'DOMSnapshot.TextBoxSnapshot'.
--   Table of details of the post layout rendered text positions. The exact layout should not be regarded as
--   stable and may change between versions.
data DOMSnapshotTextBoxSnapshot = DOMSnapshotTextBoxSnapshot
  {
    -- | Index of the layout tree node that owns this box collection.
    dOMSnapshotTextBoxSnapshotLayoutIndex :: [Int],
    -- | The absolute position bounding box.
    dOMSnapshotTextBoxSnapshotBounds :: [DOMSnapshotRectangle],
    -- | The starting index in characters, for this post layout textbox substring. Characters that
    --   would be represented as a surrogate pair in UTF-16 have length 2.
    dOMSnapshotTextBoxSnapshotStart :: [Int],
    -- | The number of characters in this post layout textbox substring. Characters that would be
    --   represented as a surrogate pair in UTF-16 have length 2.
    dOMSnapshotTextBoxSnapshotLength :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotTextBoxSnapshot where
  parseJSON = A.withObject "DOMSnapshotTextBoxSnapshot" $ \o -> DOMSnapshotTextBoxSnapshot
    <$> o A..: "layoutIndex"
    <*> o A..: "bounds"
    <*> o A..: "start"
    <*> o A..: "length"
instance ToJSON DOMSnapshotTextBoxSnapshot where
  toJSON p = A.object $ catMaybes [
    ("layoutIndex" A..=) <$> Just (dOMSnapshotTextBoxSnapshotLayoutIndex p),
    ("bounds" A..=) <$> Just (dOMSnapshotTextBoxSnapshotBounds p),
    ("start" A..=) <$> Just (dOMSnapshotTextBoxSnapshotStart p),
    ("length" A..=) <$> Just (dOMSnapshotTextBoxSnapshotLength p)
    ]

-- | Disables DOM snapshot agent for the given page.

-- | Parameters of the 'DOMSnapshot.disable' command.
data PDOMSnapshotDisable = PDOMSnapshotDisable
  deriving (Eq, Show)
pDOMSnapshotDisable
  :: PDOMSnapshotDisable
pDOMSnapshotDisable
  = PDOMSnapshotDisable
instance ToJSON PDOMSnapshotDisable where
  toJSON _ = A.Null
instance Command PDOMSnapshotDisable where
  type CommandResponse PDOMSnapshotDisable = ()
  commandName _ = "DOMSnapshot.disable"
  fromJSON = const . A.Success . const ()

-- | Enables DOM snapshot agent for the given page.

-- | Parameters of the 'DOMSnapshot.enable' command.
data PDOMSnapshotEnable = PDOMSnapshotEnable
  deriving (Eq, Show)
pDOMSnapshotEnable
  :: PDOMSnapshotEnable
pDOMSnapshotEnable
  = PDOMSnapshotEnable
instance ToJSON PDOMSnapshotEnable where
  toJSON _ = A.Null
instance Command PDOMSnapshotEnable where
  type CommandResponse PDOMSnapshotEnable = ()
  commandName _ = "DOMSnapshot.enable"
  fromJSON = const . A.Success . const ()

-- | Returns a document snapshot, including the full DOM tree of the root node (including iframes,
--   template contents, and imported documents) in a flattened array, as well as layout and
--   white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
--   flattened.

-- | Parameters of the 'DOMSnapshot.captureSnapshot' command.
data PDOMSnapshotCaptureSnapshot = PDOMSnapshotCaptureSnapshot
  {
    -- | Whitelist of computed styles to return.
    pDOMSnapshotCaptureSnapshotComputedStyles :: [T.Text],
    -- | Whether to include layout object paint orders into the snapshot.
    pDOMSnapshotCaptureSnapshotIncludePaintOrder :: Maybe Bool,
    -- | Whether to include DOM rectangles (offsetRects, clientRects, scrollRects) into the snapshot
    pDOMSnapshotCaptureSnapshotIncludeDOMRects :: Maybe Bool,
    -- | Whether to include blended background colors in the snapshot (default: false).
    --   Blended background color is achieved by blending background colors of all elements
    --   that overlap with the current element.
    pDOMSnapshotCaptureSnapshotIncludeBlendedBackgroundColors :: Maybe Bool,
    -- | Whether to include text color opacity in the snapshot (default: false).
    --   An element might have the opacity property set that affects the text color of the element.
    --   The final text color opacity is computed based on the opacity of all overlapping elements.
    pDOMSnapshotCaptureSnapshotIncludeTextColorOpacities :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMSnapshotCaptureSnapshot
  {-
  -- | Whitelist of computed styles to return.
  -}
  :: [T.Text]
  -> PDOMSnapshotCaptureSnapshot
pDOMSnapshotCaptureSnapshot
  arg_pDOMSnapshotCaptureSnapshotComputedStyles
  = PDOMSnapshotCaptureSnapshot
    arg_pDOMSnapshotCaptureSnapshotComputedStyles
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMSnapshotCaptureSnapshot where
  toJSON p = A.object $ catMaybes [
    ("computedStyles" A..=) <$> Just (pDOMSnapshotCaptureSnapshotComputedStyles p),
    ("includePaintOrder" A..=) <$> (pDOMSnapshotCaptureSnapshotIncludePaintOrder p),
    ("includeDOMRects" A..=) <$> (pDOMSnapshotCaptureSnapshotIncludeDOMRects p),
    ("includeBlendedBackgroundColors" A..=) <$> (pDOMSnapshotCaptureSnapshotIncludeBlendedBackgroundColors p),
    ("includeTextColorOpacities" A..=) <$> (pDOMSnapshotCaptureSnapshotIncludeTextColorOpacities p)
    ]
data DOMSnapshotCaptureSnapshot = DOMSnapshotCaptureSnapshot
  {
    -- | The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
    dOMSnapshotCaptureSnapshotDocuments :: [DOMSnapshotDocumentSnapshot],
    -- | Shared string table that all string properties refer to with indexes.
    dOMSnapshotCaptureSnapshotStrings :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON DOMSnapshotCaptureSnapshot where
  parseJSON = A.withObject "DOMSnapshotCaptureSnapshot" $ \o -> DOMSnapshotCaptureSnapshot
    <$> o A..: "documents"
    <*> o A..: "strings"
instance Command PDOMSnapshotCaptureSnapshot where
  type CommandResponse PDOMSnapshotCaptureSnapshot = DOMSnapshotCaptureSnapshot
  commandName _ = "DOMSnapshot.captureSnapshot"

