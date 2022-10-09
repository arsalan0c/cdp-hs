{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  DOMSnapshot :
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

import CDP.Internal.Runtime
import CDP.Handle


import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'DOMSnapshot.DOMNode'.
--   A Node in the DOM tree.
data DOMSnapshotDOMNode = DOMSnapshotDOMNode {
  -- | `Node`'s nodeType.
  dOMSnapshotDOMNodeNodeType :: Int,
  -- | `Node`'s nodeName.
  dOMSnapshotDOMNodeNodeName :: String,
  -- | `Node`'s nodeValue.
  dOMSnapshotDOMNodeNodeValue :: String,
  -- | Only set for textarea elements, contains the text value.
  dOMSnapshotDOMNodeTextValue :: Maybe String,
  -- | Only set for input elements, contains the input's associated text value.
  dOMSnapshotDOMNodeInputValue :: Maybe String,
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
  dOMSnapshotDOMNodeDocumentURL :: Maybe String,
  -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
  dOMSnapshotDOMNodeBaseURL :: Maybe String,
  -- | Only set for documents, contains the document's content language.
  dOMSnapshotDOMNodeContentLanguage :: Maybe String,
  -- | Only set for documents, contains the document's character set encoding.
  dOMSnapshotDOMNodeDocumentEncoding :: Maybe String,
  -- | `DocumentType` node's publicId.
  dOMSnapshotDOMNodePublicId :: Maybe String,
  -- | `DocumentType` node's systemId.
  dOMSnapshotDOMNodeSystemId :: Maybe String,
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
  dOMSnapshotDOMNodeCurrentSourceURL :: Maybe String,
  -- | The url of the script (if any) that generates this node.
  dOMSnapshotDOMNodeOriginURL :: Maybe String,
  -- | Scroll offsets, set when this node is a Document.
  dOMSnapshotDOMNodeScrollOffsetX :: Maybe Double,
  dOMSnapshotDOMNodeScrollOffsetY :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotDOMNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotDOMNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'DOMSnapshot.InlineTextBox'.
--   Details of post layout rendered text positions. The exact layout should not be regarded as
--   stable and may change between versions.
data DOMSnapshotInlineTextBox = DOMSnapshotInlineTextBox {
  -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
  dOMSnapshotInlineTextBoxBoundingBox :: DOMPageNetworkEmulationSecurity.DOMRect,
  -- | The starting index in characters, for this post layout textbox substring. Characters that
  --   would be represented as a surrogate pair in UTF-16 have length 2.
  dOMSnapshotInlineTextBoxStartCharacterIndex :: Int,
  -- | The number of characters in this post layout textbox substring. Characters that would be
  --   represented as a surrogate pair in UTF-16 have length 2.
  dOMSnapshotInlineTextBoxNumCharacters :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotInlineTextBox  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotInlineTextBox where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'DOMSnapshot.LayoutTreeNode'.
--   Details of an element in the DOM tree with a LayoutObject.
data DOMSnapshotLayoutTreeNode = DOMSnapshotLayoutTreeNode {
  -- | The index of the related DOM node in the `domNodes` array returned by `getSnapshot`.
  dOMSnapshotLayoutTreeNodeDomNodeIndex :: Int,
  -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
  dOMSnapshotLayoutTreeNodeBoundingBox :: DOMPageNetworkEmulationSecurity.DOMRect,
  -- | Contents of the LayoutText, if any.
  dOMSnapshotLayoutTreeNodeLayoutText :: Maybe String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotLayoutTreeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotLayoutTreeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'DOMSnapshot.ComputedStyle'.
--   A subset of the full ComputedStyle as defined by the request whitelist.
data DOMSnapshotComputedStyle = DOMSnapshotComputedStyle {
  -- | Name/value pairs of computed style properties.
  dOMSnapshotComputedStyleProperties :: [DOMSnapshotNameValue]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotComputedStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotComputedStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'DOMSnapshot.NameValue'.
--   A name/value pair.
data DOMSnapshotNameValue = DOMSnapshotNameValue {
  -- | Attribute/property name.
  dOMSnapshotNameValueName :: String,
  -- | Attribute/property value.
  dOMSnapshotNameValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotNameValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotNameValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'DOMSnapshot.StringIndex'.
--   Index of the string in the strings table.
type DOMSnapshotStringIndex = Int

-- | Type 'DOMSnapshot.ArrayOfStrings'.
--   Index of the string in the strings table.
type DOMSnapshotArrayOfStrings = [DOMSnapshotStringIndex]

-- | Type 'DOMSnapshot.RareStringData'.
--   Data that is only present on rare nodes.
data DOMSnapshotRareStringData = DOMSnapshotRareStringData {
  dOMSnapshotRareStringDataIndex :: [Int],
  dOMSnapshotRareStringDataValue :: [DOMSnapshotStringIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotRareStringData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotRareStringData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'DOMSnapshot.RareBooleanData'.
data DOMSnapshotRareBooleanData = DOMSnapshotRareBooleanData {
  dOMSnapshotRareBooleanDataIndex :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotRareBooleanData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotRareBooleanData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.RareIntegerData'.
data DOMSnapshotRareIntegerData = DOMSnapshotRareIntegerData {
  dOMSnapshotRareIntegerDataIndex :: [Int],
  dOMSnapshotRareIntegerDataValue :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotRareIntegerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotRareIntegerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.Rectangle'.
type DOMSnapshotRectangle = [Double]

-- | Type 'DOMSnapshot.DocumentSnapshot'.
--   Document snapshot.
data DOMSnapshotDocumentSnapshot = DOMSnapshotDocumentSnapshot {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotDocumentSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotDocumentSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'DOMSnapshot.NodeTreeSnapshot'.
--   Table containing nodes.
data DOMSnapshotNodeTreeSnapshot = DOMSnapshotNodeTreeSnapshot {
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
  -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
  --   event listeners attached via JavaScript as well as anchor tags that naturally navigate when
  --   clicked.
  dOMSnapshotNodeTreeSnapshotIsClickable :: Maybe DOMSnapshotRareBooleanData,
  -- | The selected url for nodes with a srcset attribute.
  dOMSnapshotNodeTreeSnapshotCurrentSourceURL :: Maybe DOMSnapshotRareStringData,
  -- | The url of the script (if any) that generates this node.
  dOMSnapshotNodeTreeSnapshotOriginURL :: Maybe DOMSnapshotRareStringData
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotNodeTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotNodeTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'DOMSnapshot.LayoutTreeSnapshot'.
--   Table of details of an element in the DOM tree with a LayoutObject.
data DOMSnapshotLayoutTreeSnapshot = DOMSnapshotLayoutTreeSnapshot {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotLayoutTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotLayoutTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type 'DOMSnapshot.TextBoxSnapshot'.
--   Table of details of the post layout rendered text positions. The exact layout should not be regarded as
--   stable and may change between versions.
data DOMSnapshotTextBoxSnapshot = DOMSnapshotTextBoxSnapshot {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSnapshotTextBoxSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DOMSnapshotTextBoxSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }







-- | Function for the 'DOMSnapshot.disable' command.
--   Disables DOM snapshot agent for the given page.
dOMSnapshotDisable :: Handle ev -> IO ()
dOMSnapshotDisable handle = sendReceiveCommand handle "DOMSnapshot.disable" (Nothing :: Maybe ())


-- | Function for the 'DOMSnapshot.enable' command.
--   Enables DOM snapshot agent for the given page.
dOMSnapshotEnable :: Handle ev -> IO ()
dOMSnapshotEnable handle = sendReceiveCommand handle "DOMSnapshot.enable" (Nothing :: Maybe ())


-- | Parameters of the 'dOMSnapshotCaptureSnapshot' command.
data PDOMSnapshotCaptureSnapshot = PDOMSnapshotCaptureSnapshot {
  -- | Whitelist of computed styles to return.
  pDOMSnapshotCaptureSnapshotComputedStyles :: [String],
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSnapshotCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDOMSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'DOMSnapshot.captureSnapshot' command.
--   Returns a document snapshot, including the full DOM tree of the root node (including iframes,
--   template contents, and imported documents) in a flattened array, as well as layout and
--   white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
--   flattened.
--   Parameters: 'PDOMSnapshotCaptureSnapshot'
--   Returns: 'DOMSnapshotCaptureSnapshot'
dOMSnapshotCaptureSnapshot :: Handle ev -> PDOMSnapshotCaptureSnapshot -> IO DOMSnapshotCaptureSnapshot
dOMSnapshotCaptureSnapshot handle params = sendReceiveCommandResult handle "DOMSnapshot.captureSnapshot" (Just params)

-- | Return type of the 'dOMSnapshotCaptureSnapshot' command.
data DOMSnapshotCaptureSnapshot = DOMSnapshotCaptureSnapshot {
  -- | The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
  dOMSnapshotCaptureSnapshotDocuments :: [DOMSnapshotDocumentSnapshot],
  -- | Shared string table that all string properties refer to with indexes.
  dOMSnapshotCaptureSnapshotStrings :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DOMSnapshotCaptureSnapshot where
   commandName _ = "DOMSnapshot.captureSnapshot"




