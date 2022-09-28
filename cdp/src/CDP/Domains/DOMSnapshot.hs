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
   domSnapshotDomNodeNodeType :: DomSnapshotDomNodeNodeType, -- ^ `Node`'s nodeType.
   domSnapshotDomNodeNodeName :: DomSnapshotDomNodeNodeName, -- ^ `Node`'s nodeName.
   domSnapshotDomNodeNodeValue :: DomSnapshotDomNodeNodeValue, -- ^ `Node`'s nodeValue.
   domSnapshotDomNodeTextValue :: DomSnapshotDomNodeTextValue, -- ^ Only set for textarea elements, contains the text value.
   domSnapshotDomNodeInputValue :: DomSnapshotDomNodeInputValue, -- ^ Only set for input elements, contains the input's associated text value.
   domSnapshotDomNodeInputChecked :: DomSnapshotDomNodeInputChecked, -- ^ Only set for radio and checkbox input elements, indicates if the element has been checked
   domSnapshotDomNodeOptionSelected :: DomSnapshotDomNodeOptionSelected, -- ^ Only set for option elements, indicates if the element has been selected
   domSnapshotDomNodeBackendNodeId :: DomSnapshotDomNodeBackendNodeId, -- ^ `Node`'s id, corresponds to DOM.Node.backendNodeId.
   domSnapshotDomNodeChildNodeIndexes :: DomSnapshotDomNodeChildNodeIndexes, -- ^ The indexes of the node's child nodes in the `domNodes` array returned by `getSnapshot`, if
any.
   domSnapshotDomNodeAttributes :: DomSnapshotDomNodeAttributes, -- ^ Attributes of an `Element` node.
   domSnapshotDomNodePseudoElementIndexes :: DomSnapshotDomNodePseudoElementIndexes, -- ^ Indexes of pseudo elements associated with this node in the `domNodes` array returned by
`getSnapshot`, if any.
   domSnapshotDomNodeLayoutNodeIndex :: DomSnapshotDomNodeLayoutNodeIndex, -- ^ The index of the node's related layout tree node in the `layoutTreeNodes` array returned by
`getSnapshot`, if any.
   domSnapshotDomNodeDocumentUrl :: DomSnapshotDomNodeDocumentUrl, -- ^ Document URL that `Document` or `FrameOwner` node points to.
   domSnapshotDomNodeBaseUrl :: DomSnapshotDomNodeBaseUrl, -- ^ Base URL that `Document` or `FrameOwner` node uses for URL completion.
   domSnapshotDomNodeContentLanguage :: DomSnapshotDomNodeContentLanguage, -- ^ Only set for documents, contains the document's content language.
   domSnapshotDomNodeDocumentEncoding :: DomSnapshotDomNodeDocumentEncoding, -- ^ Only set for documents, contains the document's character set encoding.
   domSnapshotDomNodePublicId :: DomSnapshotDomNodePublicId, -- ^ `DocumentType` node's publicId.
   domSnapshotDomNodeSystemId :: DomSnapshotDomNodeSystemId, -- ^ `DocumentType` node's systemId.
   domSnapshotDomNodeFrameId :: DomSnapshotDomNodeFrameId, -- ^ Frame ID for frame owner elements and also for the document node.
   domSnapshotDomNodeContentDocumentIndex :: DomSnapshotDomNodeContentDocumentIndex, -- ^ The index of a frame owner element's content document in the `domNodes` array returned by
`getSnapshot`, if any.
   domSnapshotDomNodePseudoType :: DomSnapshotDomNodePseudoType, -- ^ Type of a pseudo element node.
   domSnapshotDomNodeShadowRootType :: DomSnapshotDomNodeShadowRootType, -- ^ Shadow root type.
   domSnapshotDomNodeIsClickable :: DomSnapshotDomNodeIsClickable, -- ^ Whether this DOM node responds to mouse clicks. This includes nodes that have had click
event listeners attached via JavaScript as well as anchor tags that naturally navigate when
clicked.
   domSnapshotDomNodeEventListeners :: DomSnapshotDomNodeEventListeners, -- ^ Details of the node's event listeners, if any.
   domSnapshotDomNodeCurrentSourceUrl :: DomSnapshotDomNodeCurrentSourceUrl, -- ^ The selected url for nodes with a srcset attribute.
   domSnapshotDomNodeOriginUrl :: DomSnapshotDomNodeOriginUrl, -- ^ The url of the script (if any) that generates this node.
   domSnapshotDomNodeScrollOffsetX :: DomSnapshotDomNodeScrollOffsetX, -- ^ Scroll offsets, set when this node is a Document.

} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Details of post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data DomSnapshotInlineTextBox = DomSnapshotInlineTextBox {
   domSnapshotInlineTextBoxBoundingBox :: DomSnapshotInlineTextBoxBoundingBox, -- ^ The bounding box in document coordinates. Note that scroll offset of the document is ignored.
   domSnapshotInlineTextBoxStartCharacterIndex :: DomSnapshotInlineTextBoxStartCharacterIndex, -- ^ The starting index in characters, for this post layout textbox substring. Characters that
would be represented as a surrogate pair in UTF-16 have length 2.
   domSnapshotInlineTextBoxNumCharacters :: DomSnapshotInlineTextBoxNumCharacters -- ^ The number of characters in this post layout textbox substring. Characters that would be
represented as a surrogate pair in UTF-16 have length 2.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotInlineTextBox  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotInlineTextBox where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Details of an element in the DOM tree with a LayoutObject.
data DomSnapshotLayoutTreeNode = DomSnapshotLayoutTreeNode {
   domSnapshotLayoutTreeNodeDomNodeIndex :: DomSnapshotLayoutTreeNodeDomNodeIndex, -- ^ The index of the related DOM node in the `domNodes` array returned by `getSnapshot`.
   domSnapshotLayoutTreeNodeBoundingBox :: DomSnapshotLayoutTreeNodeBoundingBox, -- ^ The bounding box in document coordinates. Note that scroll offset of the document is ignored.
   domSnapshotLayoutTreeNodeLayoutText :: DomSnapshotLayoutTreeNodeLayoutText, -- ^ Contents of the LayoutText, if any.
   domSnapshotLayoutTreeNodeInlineTextNodes :: DomSnapshotLayoutTreeNodeInlineTextNodes, -- ^ The post-layout inline text nodes, if any.
   domSnapshotLayoutTreeNodeStyleIndex :: DomSnapshotLayoutTreeNodeStyleIndex, -- ^ Index into the `computedStyles` array returned by `getSnapshot`.
   domSnapshotLayoutTreeNodePaintOrder :: DomSnapshotLayoutTreeNodePaintOrder, -- ^ Global paint order index, which is determined by the stacking order of the nodes. Nodes
that are painted together will have the same index. Only provided if includePaintOrder in
getSnapshot was true.
   domSnapshotLayoutTreeNodeIsStackingContext :: DomSnapshotLayoutTreeNodeIsStackingContext -- ^ Set to true to indicate the element begins a new stacking context.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | A subset of the full ComputedStyle as defined by the request whitelist.
data DomSnapshotComputedStyle = DomSnapshotComputedStyle {
   domSnapshotComputedStyleProperties :: DomSnapshotComputedStyleProperties -- ^ Name/value pairs of computed style properties.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotComputedStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotComputedStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | A name/value pair.
data DomSnapshotNameValue = DomSnapshotNameValue {
   domSnapshotNameValueName :: DomSnapshotNameValueName, -- ^ Attribute/property name.
   domSnapshotNameValueValue :: DomSnapshotNameValueValue -- ^ Attribute/property value.
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


} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareStringData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareStringData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'DOMSnapshot.RareBooleanData' .
data DomSnapshotRareBooleanData = DomSnapshotRareBooleanData {
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareBooleanData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareBooleanData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.RareIntegerData' .
data DomSnapshotRareIntegerData = DomSnapshotRareIntegerData {


} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotRareIntegerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotRareIntegerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'DOMSnapshot.Rectangle' .
type DomSnapshotRectangle = [Double]

-- | Document snapshot.
data DomSnapshotDocumentSnapshot = DomSnapshotDocumentSnapshot {
   domSnapshotDocumentSnapshotDocumentUrl :: DomSnapshotDocumentSnapshotDocumentUrl, -- ^ Document URL that `Document` or `FrameOwner` node points to.
   domSnapshotDocumentSnapshotTitle :: DomSnapshotDocumentSnapshotTitle, -- ^ Document title.
   domSnapshotDocumentSnapshotBaseUrl :: DomSnapshotDocumentSnapshotBaseUrl, -- ^ Base URL that `Document` or `FrameOwner` node uses for URL completion.
   domSnapshotDocumentSnapshotContentLanguage :: DomSnapshotDocumentSnapshotContentLanguage, -- ^ Contains the document's content language.
   domSnapshotDocumentSnapshotEncodingName :: DomSnapshotDocumentSnapshotEncodingName, -- ^ Contains the document's character set encoding.
   domSnapshotDocumentSnapshotPublicId :: DomSnapshotDocumentSnapshotPublicId, -- ^ `DocumentType` node's publicId.
   domSnapshotDocumentSnapshotSystemId :: DomSnapshotDocumentSnapshotSystemId, -- ^ `DocumentType` node's systemId.
   domSnapshotDocumentSnapshotFrameId :: DomSnapshotDocumentSnapshotFrameId, -- ^ Frame ID for frame owner elements and also for the document node.
   domSnapshotDocumentSnapshotNodes :: DomSnapshotDocumentSnapshotNodes, -- ^ A table with dom nodes.
   domSnapshotDocumentSnapshotLayout :: DomSnapshotDocumentSnapshotLayout, -- ^ The nodes in the layout tree.
   domSnapshotDocumentSnapshotTextBoxes :: DomSnapshotDocumentSnapshotTextBoxes, -- ^ The post-layout inline text nodes.
   domSnapshotDocumentSnapshotScrollOffsetX :: DomSnapshotDocumentSnapshotScrollOffsetX, -- ^ Horizontal scroll offset.
   domSnapshotDocumentSnapshotScrollOffsetY :: DomSnapshotDocumentSnapshotScrollOffsetY, -- ^ Vertical scroll offset.
   domSnapshotDocumentSnapshotContentWidth :: DomSnapshotDocumentSnapshotContentWidth, -- ^ Document content width.
   domSnapshotDocumentSnapshotContentHeight :: DomSnapshotDocumentSnapshotContentHeight -- ^ Document content height.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotDocumentSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotDocumentSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Table containing nodes.
data DomSnapshotNodeTreeSnapshot = DomSnapshotNodeTreeSnapshot {
   domSnapshotNodeTreeSnapshotParentIndex :: DomSnapshotNodeTreeSnapshotParentIndex, -- ^ Parent node index.
   domSnapshotNodeTreeSnapshotNodeType :: DomSnapshotNodeTreeSnapshotNodeType, -- ^ `Node`'s nodeType.
   domSnapshotNodeTreeSnapshotShadowRootType :: DomSnapshotNodeTreeSnapshotShadowRootType, -- ^ Type of the shadow root the `Node` is in. String values are equal to the `ShadowRootType` enum.
   domSnapshotNodeTreeSnapshotNodeName :: DomSnapshotNodeTreeSnapshotNodeName, -- ^ `Node`'s nodeName.
   domSnapshotNodeTreeSnapshotNodeValue :: DomSnapshotNodeTreeSnapshotNodeValue, -- ^ `Node`'s nodeValue.
   domSnapshotNodeTreeSnapshotBackendNodeId :: DomSnapshotNodeTreeSnapshotBackendNodeId, -- ^ `Node`'s id, corresponds to DOM.Node.backendNodeId.
   domSnapshotNodeTreeSnapshotAttributes :: DomSnapshotNodeTreeSnapshotAttributes, -- ^ Attributes of an `Element` node. Flatten name, value pairs.
   domSnapshotNodeTreeSnapshotTextValue :: DomSnapshotNodeTreeSnapshotTextValue, -- ^ Only set for textarea elements, contains the text value.
   domSnapshotNodeTreeSnapshotInputValue :: DomSnapshotNodeTreeSnapshotInputValue, -- ^ Only set for input elements, contains the input's associated text value.
   domSnapshotNodeTreeSnapshotInputChecked :: DomSnapshotNodeTreeSnapshotInputChecked, -- ^ Only set for radio and checkbox input elements, indicates if the element has been checked
   domSnapshotNodeTreeSnapshotOptionSelected :: DomSnapshotNodeTreeSnapshotOptionSelected, -- ^ Only set for option elements, indicates if the element has been selected
   domSnapshotNodeTreeSnapshotContentDocumentIndex :: DomSnapshotNodeTreeSnapshotContentDocumentIndex, -- ^ The index of the document in the list of the snapshot documents.
   domSnapshotNodeTreeSnapshotPseudoType :: DomSnapshotNodeTreeSnapshotPseudoType, -- ^ Type of a pseudo element node.
   domSnapshotNodeTreeSnapshotIsClickable :: DomSnapshotNodeTreeSnapshotIsClickable, -- ^ Whether this DOM node responds to mouse clicks. This includes nodes that have had click
event listeners attached via JavaScript as well as anchor tags that naturally navigate when
clicked.
   domSnapshotNodeTreeSnapshotCurrentSourceUrl :: DomSnapshotNodeTreeSnapshotCurrentSourceUrl, -- ^ The selected url for nodes with a srcset attribute.
   domSnapshotNodeTreeSnapshotOriginUrl :: DomSnapshotNodeTreeSnapshotOriginUrl -- ^ The url of the script (if any) that generates this node.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotNodeTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotNodeTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Table of details of an element in the DOM tree with a LayoutObject.
data DomSnapshotLayoutTreeSnapshot = DomSnapshotLayoutTreeSnapshot {
   domSnapshotLayoutTreeSnapshotNodeIndex :: DomSnapshotLayoutTreeSnapshotNodeIndex, -- ^ Index of the corresponding node in the `NodeTreeSnapshot` array returned by `captureSnapshot`.
   domSnapshotLayoutTreeSnapshotStyles :: DomSnapshotLayoutTreeSnapshotStyles, -- ^ Array of indexes specifying computed style strings, filtered according to the `computedStyles` parameter passed to `captureSnapshot`.
   domSnapshotLayoutTreeSnapshotBounds :: DomSnapshotLayoutTreeSnapshotBounds, -- ^ The absolute position bounding box.
   domSnapshotLayoutTreeSnapshotText :: DomSnapshotLayoutTreeSnapshotText, -- ^ Contents of the LayoutText, if any.
   domSnapshotLayoutTreeSnapshotStackingContexts :: DomSnapshotLayoutTreeSnapshotStackingContexts, -- ^ Stacking context information.
   domSnapshotLayoutTreeSnapshotPaintOrders :: DomSnapshotLayoutTreeSnapshotPaintOrders, -- ^ Global paint order index, which is determined by the stacking order of the nodes. Nodes
that are painted together will have the same index. Only provided if includePaintOrder in
captureSnapshot was true.
   domSnapshotLayoutTreeSnapshotOffsetRects :: DomSnapshotLayoutTreeSnapshotOffsetRects, -- ^ The offset rect of nodes. Only available when includeDOMRects is set to true
   domSnapshotLayoutTreeSnapshotScrollRects :: DomSnapshotLayoutTreeSnapshotScrollRects, -- ^ The scroll rect of nodes. Only available when includeDOMRects is set to true
   domSnapshotLayoutTreeSnapshotClientRects :: DomSnapshotLayoutTreeSnapshotClientRects, -- ^ The client rect of nodes. Only available when includeDOMRects is set to true
   domSnapshotLayoutTreeSnapshotBlendedBackgroundColors :: DomSnapshotLayoutTreeSnapshotBlendedBackgroundColors, -- ^ The list of background colors that are blended with colors of overlapping elements.
   domSnapshotLayoutTreeSnapshotTextColorOpacities :: DomSnapshotLayoutTreeSnapshotTextColorOpacities -- ^ The list of computed text opacities.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotLayoutTreeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotLayoutTreeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Table of details of the post layout rendered text positions. The exact layout should not be regarded as
-- stable and may change between versions.
data DomSnapshotTextBoxSnapshot = DomSnapshotTextBoxSnapshot {
   domSnapshotTextBoxSnapshotLayoutIndex :: DomSnapshotTextBoxSnapshotLayoutIndex, -- ^ Index of the layout tree node that owns this box collection.
   domSnapshotTextBoxSnapshotBounds :: DomSnapshotTextBoxSnapshotBounds, -- ^ The absolute position bounding box.
   domSnapshotTextBoxSnapshotStart :: DomSnapshotTextBoxSnapshotStart, -- ^ The starting index in characters, for this post layout textbox substring. Characters that
would be represented as a surrogate pair in UTF-16 have length 2.
   domSnapshotTextBoxSnapshotLength :: DomSnapshotTextBoxSnapshotLength -- ^ The number of characters in this post layout textbox substring. Characters that would be
represented as a surrogate pair in UTF-16 have length 2.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSnapshotTextBoxSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomSnapshotTextBoxSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }







-- | Function for the command 'DOMSnapshot.disable'.
-- Disables DOM snapshot agent for the given page.
domSnapshotDisable :: Handle ev -> IO (Maybe Error)
domSnapshotDisable handle = sendReceiveCommand handle "DOMSnapshot.disable" (Nothing :: Maybe ())


-- | Function for the command 'DOMSnapshot.enable'.
-- Enables DOM snapshot agent for the given page.
domSnapshotEnable :: Handle ev -> IO (Maybe Error)
domSnapshotEnable handle = sendReceiveCommand handle "DOMSnapshot.enable" (Nothing :: Maybe ())


-- | Parameters of the 'domSnapshotCaptureSnapshot' command.
data PDomSnapshotCaptureSnapshot = PDomSnapshotCaptureSnapshot {
   pDomSnapshotCaptureSnapshotComputedStyles :: PDomSnapshotCaptureSnapshotComputedStyles, -- ^ Whitelist of computed styles to return.
   pDomSnapshotCaptureSnapshotIncludePaintOrder :: PDomSnapshotCaptureSnapshotIncludePaintOrder, -- ^ Whether to include layout object paint orders into the snapshot.
   pDomSnapshotCaptureSnapshotIncludeDomRects :: PDomSnapshotCaptureSnapshotIncludeDomRects, -- ^ Whether to include DOM rectangles (offsetRects, clientRects, scrollRects) into the snapshot
   pDomSnapshotCaptureSnapshotIncludeBlendedBackgroundColors :: PDomSnapshotCaptureSnapshotIncludeBlendedBackgroundColors, -- ^ Whether to include blended background colors in the snapshot (default: false).
Blended background color is achieved by blending background colors of all elements
that overlap with the current element.
   pDomSnapshotCaptureSnapshotIncludeTextColorOpacities :: PDomSnapshotCaptureSnapshotIncludeTextColorOpacities -- ^ Whether to include text color opacity in the snapshot (default: false).
An element might have the opacity property set that affects the text color of the element.
The final text color opacity is computed based on the opacity of all overlapping elements.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSnapshotCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PDomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'DOMSnapshot.captureSnapshot'.
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
   domSnapshotCaptureSnapshotDocuments :: [DomSnapshotDocumentSnapshot], -- ^ The nodes in the DOM tree. The DOMNode at index 0 corresponds to the root document.
   domSnapshotCaptureSnapshotStrings :: [String] -- ^ Shared string table that all string properties refer to with indexes.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSnapshotCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command DomSnapshotCaptureSnapshot where
   commandName _ = "DOMSnapshot.captureSnapshot"




