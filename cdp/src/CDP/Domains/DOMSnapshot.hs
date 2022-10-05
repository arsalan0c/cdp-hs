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


-- | A Node in the DOM tree.
data DomSnapshotDomNode = DomSnapshotDomNode {
  -- | `Node`'s nodeType.
  domSnapshotDomNodeNodeType :: Int,
  -- | `Node`'s nodeName.
  domSnapshotDomNodeNodeName :: String,
  -- | `Node`'s nodeValue.
  domSnapshotDomNodeNodeValue :: String,
  -- | Only set for textarea elements, contains the text value.
  domSnapshotDomNodeTextValue :: Maybe String,
  -- | Only set for input elements, contains the input's associated text value.
  domSnapshotDomNodeInputValue :: Maybe String,
  -- | Only set for radio and checkbox input elements, indicates if the element has been checked
  domSnapshotDomNodeInputChecked :: Maybe Bool,
  -- | Only set for option elements, indicates if the element has been selected
  domSnapshotDomNodeOptionSelected :: Maybe Bool,
  -- | `Node`'s id, corresponds to DOM.Node.backendNodeId.
  domSnapshotDomNodeBackendNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  -- | The indexes of the node's child nodes in the `domNodes` array returned by `getSnapshot`, if
  -- any.
  domSnapshotDomNodeChildNodeIndexes :: Maybe [Int],
  -- | Attributes of an `Element` node.
  domSnapshotDomNodeAttributes :: Maybe [DomSnapshotNameValue],
  -- | Indexes of pseudo elements associated with this node in the `domNodes` array returned by
  -- `getSnapshot`, if any.
  domSnapshotDomNodePseudoElementIndexes :: Maybe [Int],
  -- | The index of the node's related layout tree node in the `layoutTreeNodes` array returned by
  -- `getSnapshot`, if any.
  domSnapshotDomNodeLayoutNodeIndex :: Maybe Int,
  -- | Document URL that `Document` or `FrameOwner` node points to.
  domSnapshotDomNodeDocumentUrl :: Maybe String,
  -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
  domSnapshotDomNodeBaseUrl :: Maybe String,
  -- | Only set for documents, contains the document's content language.
  domSnapshotDomNodeContentLanguage :: Maybe String,
  -- | Only set for documents, contains the document's character set encoding.
  domSnapshotDomNodeDocumentEncoding :: Maybe String,
  -- | `DocumentType` node's publicId.
  domSnapshotDomNodePublicId :: Maybe String,
  -- | `DocumentType` node's systemId.
  domSnapshotDomNodeSystemId :: Maybe String,
  -- | Frame ID for frame owner elements and also for the document node.
  domSnapshotDomNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
  -- | The index of a frame owner element's content document in the `domNodes` array returned by
  -- `getSnapshot`, if any.
  domSnapshotDomNodeContentDocumentIndex :: Maybe Int,
  -- | Type of a pseudo element node.
  domSnapshotDomNodePseudoType :: Maybe DOMPageNetworkEmulationSecurity.DomPseudoType,
  -- | Shadow root type.
  domSnapshotDomNodeShadowRootType :: Maybe DOMPageNetworkEmulationSecurity.DomShadowRootType,
  -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
  -- event listeners attached via JavaScript as well as anchor tags that naturally navigate when
  -- clicked.
  domSnapshotDomNodeIsClickable :: Maybe Bool,
  -- | Details of the node's event listeners, if any.
  domSnapshotDomNodeEventListeners :: Maybe [DOMDebugger.DomDebuggerEventListener],
  -- | The selected url for nodes with a srcset attribute.
  domSnapshotDomNodeCurrentSourceUrl :: Maybe String,
  -- | The url of the script (if any) that generates this node.
  domSnapshotDomNodeOriginUrl :: Maybe String,
  -- | Scroll offsets, set when this node is a Document.
  domSnapshotDomNodeScrollOffsetX :: Maybe Double,
  domSnapshotDomNodeScrollOffsetY :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Details of post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data DomSnapshotInlineTextBox = DomSnapshotInlineTextBox {
  -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
  domSnapshotInlineTextBoxBoundingBox :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | The starting index in characters, for this post layout textbox substring. Characters that
  -- would be represented as a surrogate pair in UTF-16 have length 2.
  domSnapshotInlineTextBoxStartCharacterIndex :: Int,
  -- | The number of characters in this post layout textbox substring. Characters that would be
  -- represented as a surrogate pair in UTF-16 have length 2.
  domSnapshotInlineTextBoxNumCharacters :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotInlineTextBox  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotInlineTextBox where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Details of an element in the DOM tree with a LayoutObject.
data DomSnapshotLayoutTreeNode = DomSnapshotLayoutTreeNode {
  -- | The index of the related DOM node in the `domNodes` array returned by `getSnapshot`.
  domSnapshotLayoutTreeNodeDomNodeIndex :: Int,
  -- | The bounding box in document coordinates. Note that scroll offset of the document is ignored.
  domSnapshotLayoutTreeNodeBoundingBox :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | Contents of the LayoutText, if any.
  domSnapshotLayoutTreeNodeLayoutText :: Maybe String,
  -- | The post-layout inline text nodes, if any.
  domSnapshotLayoutTreeNodeInlineTextNodes :: Maybe [DomSnapshotInlineTextBox],
  -- | Index into the `computedStyles` array returned by `getSnapshot`.
  domSnapshotLayoutTreeNodeStyleIndex :: Maybe Int,
  -- | Global paint order index, which is determined by the stacking order of the nodes. Nodes
  -- that are painted together will have the same index. Only provided if includePaintOrder in
  -- getSnapshot was true.
  domSnapshotLayoutTreeNodePaintOrder :: Maybe Int,
  -- | Set to true to indicate the element begins a new stacking context.
  domSnapshotLayoutTreeNodeIsStackingContext :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | A subset of the full ComputedStyle as defined by the request whitelist.
data DomSnapshotComputedStyle = DomSnapshotComputedStyle {
  -- | Name/value pairs of computed style properties.
  domSnapshotComputedStyleProperties :: [DomSnapshotNameValue]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotComputedStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotComputedStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | A name/value pair.
data DomSnapshotNameValue = DomSnapshotNameValue {
  -- | Attribute/property name.
  domSnapshotNameValueName :: String,
  -- | Attribute/property value.
  domSnapshotNameValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotNameValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotNameValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Index of the string in the strings table.
type DomSnapshotStringIndex = Int

-- | Index of the string in the strings table.
type DomSnapshotArrayOfStrings = [DomSnapshotStringIndex]

-- | Data that is only present on rare nodes.
data DomSnapshotRareStringData = DomSnapshotRareStringData {
  domSnapshotRareStringDataIndex :: [Int],
  domSnapshotRareStringDataValue :: [DomSnapshotStringIndex]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareStringData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareStringData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'DOMSnapshot.RareBooleanData' .
data DomSnapshotRareBooleanData = DomSnapshotRareBooleanData {
  domSnapshotRareBooleanDataIndex :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareBooleanData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareBooleanData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.RareIntegerData' .
data DomSnapshotRareIntegerData = DomSnapshotRareIntegerData {
  domSnapshotRareIntegerDataIndex :: [Int],
  domSnapshotRareIntegerDataValue :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareIntegerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareIntegerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.Rectangle' .
type DomSnapshotRectangle = [Double]

-- | Document snapshot.
data DomSnapshotDocumentSnapshot = DomSnapshotDocumentSnapshot {
  -- | Document URL that `Document` or `FrameOwner` node points to.
  domSnapshotDocumentSnapshotDocumentUrl :: DomSnapshotStringIndex,
  -- | Document title.
  domSnapshotDocumentSnapshotTitle :: DomSnapshotStringIndex,
  -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
  domSnapshotDocumentSnapshotBaseUrl :: DomSnapshotStringIndex,
  -- | Contains the document's content language.
  domSnapshotDocumentSnapshotContentLanguage :: DomSnapshotStringIndex,
  -- | Contains the document's character set encoding.
  domSnapshotDocumentSnapshotEncodingName :: DomSnapshotStringIndex,
  -- | `DocumentType` node's publicId.
  domSnapshotDocumentSnapshotPublicId :: DomSnapshotStringIndex,
  -- | `DocumentType` node's systemId.
  domSnapshotDocumentSnapshotSystemId :: DomSnapshotStringIndex,
  -- | Frame ID for frame owner elements and also for the document node.
  domSnapshotDocumentSnapshotFrameId :: DomSnapshotStringIndex,
  -- | A table with dom nodes.
  domSnapshotDocumentSnapshotNodes :: DomSnapshotNodeTreeSnapshot,
  -- | The nodes in the layout tree.
  domSnapshotDocumentSnapshotLayout :: DomSnapshotLayoutTreeSnapshot,
  -- | The post-layout inline text nodes.
  domSnapshotDocumentSnapshotTextBoxes :: DomSnapshotTextBoxSnapshot,
  -- | Horizontal scroll offset.
  domSnapshotDocumentSnapshotScrollOffsetX :: Maybe Double,
  -- | Vertical scroll offset.
  domSnapshotDocumentSnapshotScrollOffsetY :: Maybe Double,
  -- | Document content width.
  domSnapshotDocumentSnapshotContentWidth :: Maybe Double,
  -- | Document content height.
  domSnapshotDocumentSnapshotContentHeight :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDocumentSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDocumentSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Table containing nodes.
data DomSnapshotNodeTreeSnapshot = DomSnapshotNodeTreeSnapshot {
  -- | Parent node index.
  domSnapshotNodeTreeSnapshotParentIndex :: Maybe [Int],
  -- | `Node`'s nodeType.
  domSnapshotNodeTreeSnapshotNodeType :: Maybe [Int],
  -- | Type of the shadow root the `Node` is in. String values are equal to the `ShadowRootType` enum.
  domSnapshotNodeTreeSnapshotShadowRootType :: Maybe DomSnapshotRareStringData,
  -- | `Node`'s nodeName.
  domSnapshotNodeTreeSnapshotNodeName :: Maybe [DomSnapshotStringIndex],
  -- | `Node`'s nodeValue.
  domSnapshotNodeTreeSnapshotNodeValue :: Maybe [DomSnapshotStringIndex],
  -- | `Node`'s id, corresponds to DOM.Node.backendNodeId.
  domSnapshotNodeTreeSnapshotBackendNodeId :: Maybe [DOMPageNetworkEmulationSecurity.DomBackendNodeId],
  -- | Attributes of an `Element` node. Flatten name, value pairs.
  domSnapshotNodeTreeSnapshotAttributes :: Maybe [DomSnapshotArrayOfStrings],
  -- | Only set for textarea elements, contains the text value.
  domSnapshotNodeTreeSnapshotTextValue :: Maybe DomSnapshotRareStringData,
  -- | Only set for input elements, contains the input's associated text value.
  domSnapshotNodeTreeSnapshotInputValue :: Maybe DomSnapshotRareStringData,
  -- | Only set for radio and checkbox input elements, indicates if the element has been checked
  domSnapshotNodeTreeSnapshotInputChecked :: Maybe DomSnapshotRareBooleanData,
  -- | Only set for option elements, indicates if the element has been selected
  domSnapshotNodeTreeSnapshotOptionSelected :: Maybe DomSnapshotRareBooleanData,
  -- | The index of the document in the list of the snapshot documents.
  domSnapshotNodeTreeSnapshotContentDocumentIndex :: Maybe DomSnapshotRareIntegerData,
  -- | Type of a pseudo element node.
  domSnapshotNodeTreeSnapshotPseudoType :: Maybe DomSnapshotRareStringData,
  -- | Whether this DOM node responds to mouse clicks. This includes nodes that have had click
  -- event listeners attached via JavaScript as well as anchor tags that naturally navigate when
  -- clicked.
  domSnapshotNodeTreeSnapshotIsClickable :: Maybe DomSnapshotRareBooleanData,
  -- | The selected url for nodes with a srcset attribute.
  domSnapshotNodeTreeSnapshotCurrentSourceUrl :: Maybe DomSnapshotRareStringData,
  -- | The url of the script (if any) that generates this node.
  domSnapshotNodeTreeSnapshotOriginUrl :: Maybe DomSnapshotRareStringData
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotNodeTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotNodeTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Table of details of an element in the DOM tree with a LayoutObject.
data DomSnapshotLayoutTreeSnapshot = DomSnapshotLayoutTreeSnapshot {
  -- | Index of the corresponding node in the `NodeTreeSnapshot` array returned by `captureSnapshot`.
  domSnapshotLayoutTreeSnapshotNodeIndex :: [Int],
  -- | Array of indexes specifying computed style strings, filtered according to the `computedStyles` parameter passed to `captureSnapshot`.
  domSnapshotLayoutTreeSnapshotStyles :: [DomSnapshotArrayOfStrings],
  -- | The absolute position bounding box.
  domSnapshotLayoutTreeSnapshotBounds :: [DomSnapshotRectangle],
  -- | Contents of the LayoutText, if any.
  domSnapshotLayoutTreeSnapshotText :: [DomSnapshotStringIndex],
  -- | Stacking context information.
  domSnapshotLayoutTreeSnapshotStackingContexts :: DomSnapshotRareBooleanData,
  -- | Global paint order index, which is determined by the stacking order of the nodes. Nodes
  -- that are painted together will have the same index. Only provided if includePaintOrder in
  -- captureSnapshot was true.
  domSnapshotLayoutTreeSnapshotPaintOrders :: Maybe [Int],
  -- | The offset rect of nodes. Only available when includeDOMRects is set to true
  domSnapshotLayoutTreeSnapshotOffsetRects :: Maybe [DomSnapshotRectangle],
  -- | The scroll rect of nodes. Only available when includeDOMRects is set to true
  domSnapshotLayoutTreeSnapshotScrollRects :: Maybe [DomSnapshotRectangle],
  -- | The client rect of nodes. Only available when includeDOMRects is set to true
  domSnapshotLayoutTreeSnapshotClientRects :: Maybe [DomSnapshotRectangle],
  -- | The list of background colors that are blended with colors of overlapping elements.
  domSnapshotLayoutTreeSnapshotBlendedBackgroundColors :: Maybe [DomSnapshotStringIndex],
  -- | The list of computed text opacities.
  domSnapshotLayoutTreeSnapshotTextColorOpacities :: Maybe [Double]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Table of details of the post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data DomSnapshotTextBoxSnapshot = DomSnapshotTextBoxSnapshot {
  -- | Index of the layout tree node that owns this box collection.
  domSnapshotTextBoxSnapshotLayoutIndex :: [Int],
  -- | The absolute position bounding box.
  domSnapshotTextBoxSnapshotBounds :: [DomSnapshotRectangle],
  -- | The starting index in characters, for this post layout textbox substring. Characters that
  -- would be represented as a surrogate pair in UTF-16 have length 2.
  domSnapshotTextBoxSnapshotStart :: [Int],
  -- | The number of characters in this post layout textbox substring. Characters that would be
  -- represented as a surrogate pair in UTF-16 have length 2.
  domSnapshotTextBoxSnapshotLength :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotTextBoxSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotTextBoxSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }







-- | Function for the 'DOMSnapshot.disable' command.
-- Disables DOM snapshot agent for the given page.
domSnapshotDisable :: Handle ev -> IO (Maybe Error)
domSnapshotDisable handle = sendReceiveCommand handle "DOMSnapshot.disable" (Nothing :: Maybe ())


-- | Function for the 'DOMSnapshot.enable' command.
-- Enables DOM snapshot agent for the given page.
domSnapshotEnable :: Handle ev -> IO (Maybe Error)
domSnapshotEnable handle = sendReceiveCommand handle "DOMSnapshot.enable" (Nothing :: Maybe ())


-- | Parameters of the 'domSnapshotCaptureSnapshot' command.
data PDomSnapshotCaptureSnapshot = PDomSnapshotCaptureSnapshot {
  -- | Whitelist of computed styles to return.
  pDomSnapshotCaptureSnapshotComputedStyles :: [String],
  -- | Whether to include layout object paint orders into the snapshot.
  pDomSnapshotCaptureSnapshotIncludePaintOrder :: Maybe Bool,
  -- | Whether to include DOM rectangles (offsetRects, clientRects, scrollRects) into the snapshot
  pDomSnapshotCaptureSnapshotIncludeDomRects :: Maybe Bool,
  -- | Whether to include blended background colors in the snapshot (default: false).
  -- Blended background color is achieved by blending background colors of all elements
  -- that overlap with the current element.
  pDomSnapshotCaptureSnapshotIncludeBlendedBackgroundColors :: Maybe Bool,
  -- | Whether to include text color opacity in the snapshot (default: false).
  -- An element might have the opacity property set that affects the text color of the element.
  -- The final text color opacity is computed based on the opacity of all overlapping elements.
  pDomSnapshotCaptureSnapshotIncludeTextColorOpacities :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSnapshotCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'DOMSnapshot.captureSnapshot' command.
-- Returns a document snapshot, including the full DOM tree of the root node (including iframes,
-- template contents, and imported documents) in a flattened array, as well as layout and
-- white-listed computed style information for the nodes. Shadow DOM in the returned DOM tree is
-- flattened.
-- Parameters: 'PDomSnapshotCaptureSnapshot'
-- Returns: 'DomSnapshotCaptureSnapshot'
domSnapshotCaptureSnapshot :: Handle ev -> PDomSnapshotCaptureSnapshot -> IO (Either Error DomSnapshotCaptureSnapshot)
domSnapshotCaptureSnapshot handle params = sendReceiveCommandResult handle "DOMSnapshot.captureSnapshot" (Just params)

-- | Return type of the 'domSnapshotCaptureSnapshot' command.
data DomSnapshotCaptureSnapshot = DomSnapshotCaptureSnapshot {
  -- | The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
  domSnapshotCaptureSnapshotDocuments :: [DomSnapshotDocumentSnapshot],
  -- | Shared string table that all string properties refer to with indexes.
  domSnapshotCaptureSnapshotStrings :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DomSnapshotCaptureSnapshot where
   commandName _ = "DOMSnapshot.captureSnapshot"




