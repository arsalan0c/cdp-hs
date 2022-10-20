{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= DOM

This domain exposes DOM read/write operations. Each DOM Node is represented with its mirror object
that has an `id`. This `id` can be used to get additional information on the Node, resolve it into
the JavaScript object wrapper, etc. It is important that client receives DOM events only for the
nodes that are known to the client. Backend keeps track of the nodes that were sent to the client
and never sends the same node twice. It is client's responsibility to collect information about
the nodes that were sent to the client.<p>Note that `iframe` owner elements will return
corresponding document elements as their child nodes.</p>
= Emulation

This domain emulates different environments for the page.
= Network

Network domain allows tracking network activities of the page. It exposes information about http,
file, data and other requests and responses, their headers, bodies, timing, etc.
= Page

Actions and events related to the inspected page belong to the page domain.
= Security

Security
-}


module CDP.Domains.DOMPageNetworkEmulationSecurity (module CDP.Domains.DOMPageNetworkEmulationSecurity) where

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


import CDP.Domains.Debugger as Debugger
import CDP.Domains.IO as IO
import CDP.Domains.Runtime as Runtime


-- | Type 'DOM.NodeId'.
--   Unique DOM node identifier.
type DOMNodeId = Int

-- | Type 'DOM.BackendNodeId'.
--   Unique DOM node identifier used to reference a node that may not have been pushed to the
--   front-end.
type DOMBackendNodeId = Int

-- | Type 'DOM.BackendNode'.
--   Backend node with a friendly name.
data DOMBackendNode = DOMBackendNode
  {
    -- | `Node`'s nodeType.
    dOMBackendNodeNodeType :: Int,
    -- | `Node`'s nodeName.
    dOMBackendNodeNodeName :: T.Text,
    dOMBackendNodeBackendNodeId :: DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMBackendNode where
  parseJSON = A.withObject "DOMBackendNode" $ \o -> DOMBackendNode
    <$> o A..: "nodeType"
    <*> o A..: "nodeName"
    <*> o A..: "backendNodeId"
instance ToJSON DOMBackendNode where
  toJSON p = A.object $ catMaybes [
    ("nodeType" A..=) <$> Just (dOMBackendNodeNodeType p),
    ("nodeName" A..=) <$> Just (dOMBackendNodeNodeName p),
    ("backendNodeId" A..=) <$> Just (dOMBackendNodeBackendNodeId p)
    ]

-- | Type 'DOM.PseudoType'.
--   Pseudo element type.
data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
  deriving (Ord, Eq, Show, Read)
instance FromJSON DOMPseudoType where
  parseJSON = A.withText "DOMPseudoType" $ \v -> case v of
    "first-line" -> pure DOMPseudoTypeFirstLine
    "first-letter" -> pure DOMPseudoTypeFirstLetter
    "before" -> pure DOMPseudoTypeBefore
    "after" -> pure DOMPseudoTypeAfter
    "marker" -> pure DOMPseudoTypeMarker
    "backdrop" -> pure DOMPseudoTypeBackdrop
    "selection" -> pure DOMPseudoTypeSelection
    "target-text" -> pure DOMPseudoTypeTargetText
    "spelling-error" -> pure DOMPseudoTypeSpellingError
    "grammar-error" -> pure DOMPseudoTypeGrammarError
    "highlight" -> pure DOMPseudoTypeHighlight
    "first-line-inherited" -> pure DOMPseudoTypeFirstLineInherited
    "scrollbar" -> pure DOMPseudoTypeScrollbar
    "scrollbar-thumb" -> pure DOMPseudoTypeScrollbarThumb
    "scrollbar-button" -> pure DOMPseudoTypeScrollbarButton
    "scrollbar-track" -> pure DOMPseudoTypeScrollbarTrack
    "scrollbar-track-piece" -> pure DOMPseudoTypeScrollbarTrackPiece
    "scrollbar-corner" -> pure DOMPseudoTypeScrollbarCorner
    "resizer" -> pure DOMPseudoTypeResizer
    "input-list-button" -> pure DOMPseudoTypeInputListButton
    "page-transition" -> pure DOMPseudoTypePageTransition
    "page-transition-container" -> pure DOMPseudoTypePageTransitionContainer
    "page-transition-image-wrapper" -> pure DOMPseudoTypePageTransitionImageWrapper
    "page-transition-outgoing-image" -> pure DOMPseudoTypePageTransitionOutgoingImage
    "page-transition-incoming-image" -> pure DOMPseudoTypePageTransitionIncomingImage
    "_" -> fail "failed to parse DOMPseudoType"
instance ToJSON DOMPseudoType where
  toJSON v = A.String $ case v of
    DOMPseudoTypeFirstLine -> "first-line"
    DOMPseudoTypeFirstLetter -> "first-letter"
    DOMPseudoTypeBefore -> "before"
    DOMPseudoTypeAfter -> "after"
    DOMPseudoTypeMarker -> "marker"
    DOMPseudoTypeBackdrop -> "backdrop"
    DOMPseudoTypeSelection -> "selection"
    DOMPseudoTypeTargetText -> "target-text"
    DOMPseudoTypeSpellingError -> "spelling-error"
    DOMPseudoTypeGrammarError -> "grammar-error"
    DOMPseudoTypeHighlight -> "highlight"
    DOMPseudoTypeFirstLineInherited -> "first-line-inherited"
    DOMPseudoTypeScrollbar -> "scrollbar"
    DOMPseudoTypeScrollbarThumb -> "scrollbar-thumb"
    DOMPseudoTypeScrollbarButton -> "scrollbar-button"
    DOMPseudoTypeScrollbarTrack -> "scrollbar-track"
    DOMPseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
    DOMPseudoTypeScrollbarCorner -> "scrollbar-corner"
    DOMPseudoTypeResizer -> "resizer"
    DOMPseudoTypeInputListButton -> "input-list-button"
    DOMPseudoTypePageTransition -> "page-transition"
    DOMPseudoTypePageTransitionContainer -> "page-transition-container"
    DOMPseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
    DOMPseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
    DOMPseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"

-- | Type 'DOM.ShadowRootType'.
--   Shadow root type.
data DOMShadowRootType = DOMShadowRootTypeUserAgent | DOMShadowRootTypeOpen | DOMShadowRootTypeClosed
  deriving (Ord, Eq, Show, Read)
instance FromJSON DOMShadowRootType where
  parseJSON = A.withText "DOMShadowRootType" $ \v -> case v of
    "user-agent" -> pure DOMShadowRootTypeUserAgent
    "open" -> pure DOMShadowRootTypeOpen
    "closed" -> pure DOMShadowRootTypeClosed
    "_" -> fail "failed to parse DOMShadowRootType"
instance ToJSON DOMShadowRootType where
  toJSON v = A.String $ case v of
    DOMShadowRootTypeUserAgent -> "user-agent"
    DOMShadowRootTypeOpen -> "open"
    DOMShadowRootTypeClosed -> "closed"

-- | Type 'DOM.CompatibilityMode'.
--   Document compatibility mode.
data DOMCompatibilityMode = DOMCompatibilityModeQuirksMode | DOMCompatibilityModeLimitedQuirksMode | DOMCompatibilityModeNoQuirksMode
  deriving (Ord, Eq, Show, Read)
instance FromJSON DOMCompatibilityMode where
  parseJSON = A.withText "DOMCompatibilityMode" $ \v -> case v of
    "QuirksMode" -> pure DOMCompatibilityModeQuirksMode
    "LimitedQuirksMode" -> pure DOMCompatibilityModeLimitedQuirksMode
    "NoQuirksMode" -> pure DOMCompatibilityModeNoQuirksMode
    "_" -> fail "failed to parse DOMCompatibilityMode"
instance ToJSON DOMCompatibilityMode where
  toJSON v = A.String $ case v of
    DOMCompatibilityModeQuirksMode -> "QuirksMode"
    DOMCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
    DOMCompatibilityModeNoQuirksMode -> "NoQuirksMode"

-- | Type 'DOM.Node'.
--   DOM interaction is implemented in terms of mirror objects that represent the actual DOM nodes.
--   DOMNode is a base node mirror type.
data DOMNode = DOMNode
  {
    -- | Node identifier that is passed into the rest of the DOM messages as the `nodeId`. Backend
    --   will only push node with given `id` once. It is aware of all requested nodes and will only
    --   fire DOM events for nodes known to the client.
    dOMNodeNodeId :: DOMNodeId,
    -- | The id of the parent node if any.
    dOMNodeParentId :: Maybe DOMNodeId,
    -- | The BackendNodeId for this node.
    dOMNodeBackendNodeId :: DOMBackendNodeId,
    -- | `Node`'s nodeType.
    dOMNodeNodeType :: Int,
    -- | `Node`'s nodeName.
    dOMNodeNodeName :: T.Text,
    -- | `Node`'s localName.
    dOMNodeLocalName :: T.Text,
    -- | `Node`'s nodeValue.
    dOMNodeNodeValue :: T.Text,
    -- | Child count for `Container` nodes.
    dOMNodeChildNodeCount :: Maybe Int,
    -- | Child nodes of this node when requested with children.
    dOMNodeChildren :: Maybe [DOMNode],
    -- | Attributes of the `Element` node in the form of flat array `[name1, value1, name2, value2]`.
    dOMNodeAttributes :: Maybe [T.Text],
    -- | Document URL that `Document` or `FrameOwner` node points to.
    dOMNodeDocumentURL :: Maybe T.Text,
    -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
    dOMNodeBaseURL :: Maybe T.Text,
    -- | `DocumentType`'s publicId.
    dOMNodePublicId :: Maybe T.Text,
    -- | `DocumentType`'s systemId.
    dOMNodeSystemId :: Maybe T.Text,
    -- | `DocumentType`'s internalSubset.
    dOMNodeInternalSubset :: Maybe T.Text,
    -- | `Document`'s XML version in case of XML documents.
    dOMNodeXmlVersion :: Maybe T.Text,
    -- | `Attr`'s name.
    dOMNodeName :: Maybe T.Text,
    -- | `Attr`'s value.
    dOMNodeValue :: Maybe T.Text,
    -- | Pseudo element type for this node.
    dOMNodePseudoType :: Maybe DOMPseudoType,
    -- | Pseudo element identifier for this node. Only present if there is a
    --   valid pseudoType.
    dOMNodePseudoIdentifier :: Maybe T.Text,
    -- | Shadow root type.
    dOMNodeShadowRootType :: Maybe DOMShadowRootType,
    -- | Frame ID for frame owner elements.
    dOMNodeFrameId :: Maybe PageFrameId,
    -- | Content document for frame owner elements.
    dOMNodeContentDocument :: Maybe DOMNode,
    -- | Shadow root list for given element host.
    dOMNodeShadowRoots :: Maybe [DOMNode],
    -- | Content document fragment for template elements.
    dOMNodeTemplateContent :: Maybe DOMNode,
    -- | Pseudo elements associated with this node.
    dOMNodePseudoElements :: Maybe [DOMNode],
    -- | Distributed nodes for given insertion point.
    dOMNodeDistributedNodes :: Maybe [DOMBackendNode],
    -- | Whether the node is SVG.
    dOMNodeIsSVG :: Maybe Bool,
    dOMNodeCompatibilityMode :: Maybe DOMCompatibilityMode,
    dOMNodeAssignedSlot :: Maybe DOMBackendNode
  }
  deriving (Eq, Show)
instance FromJSON DOMNode where
  parseJSON = A.withObject "DOMNode" $ \o -> DOMNode
    <$> o A..: "nodeId"
    <*> o A..:? "parentId"
    <*> o A..: "backendNodeId"
    <*> o A..: "nodeType"
    <*> o A..: "nodeName"
    <*> o A..: "localName"
    <*> o A..: "nodeValue"
    <*> o A..:? "childNodeCount"
    <*> o A..:? "children"
    <*> o A..:? "attributes"
    <*> o A..:? "documentURL"
    <*> o A..:? "baseURL"
    <*> o A..:? "publicId"
    <*> o A..:? "systemId"
    <*> o A..:? "internalSubset"
    <*> o A..:? "xmlVersion"
    <*> o A..:? "name"
    <*> o A..:? "value"
    <*> o A..:? "pseudoType"
    <*> o A..:? "pseudoIdentifier"
    <*> o A..:? "shadowRootType"
    <*> o A..:? "frameId"
    <*> o A..:? "contentDocument"
    <*> o A..:? "shadowRoots"
    <*> o A..:? "templateContent"
    <*> o A..:? "pseudoElements"
    <*> o A..:? "distributedNodes"
    <*> o A..:? "isSVG"
    <*> o A..:? "compatibilityMode"
    <*> o A..:? "assignedSlot"
instance ToJSON DOMNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (dOMNodeNodeId p),
    ("parentId" A..=) <$> (dOMNodeParentId p),
    ("backendNodeId" A..=) <$> Just (dOMNodeBackendNodeId p),
    ("nodeType" A..=) <$> Just (dOMNodeNodeType p),
    ("nodeName" A..=) <$> Just (dOMNodeNodeName p),
    ("localName" A..=) <$> Just (dOMNodeLocalName p),
    ("nodeValue" A..=) <$> Just (dOMNodeNodeValue p),
    ("childNodeCount" A..=) <$> (dOMNodeChildNodeCount p),
    ("children" A..=) <$> (dOMNodeChildren p),
    ("attributes" A..=) <$> (dOMNodeAttributes p),
    ("documentURL" A..=) <$> (dOMNodeDocumentURL p),
    ("baseURL" A..=) <$> (dOMNodeBaseURL p),
    ("publicId" A..=) <$> (dOMNodePublicId p),
    ("systemId" A..=) <$> (dOMNodeSystemId p),
    ("internalSubset" A..=) <$> (dOMNodeInternalSubset p),
    ("xmlVersion" A..=) <$> (dOMNodeXmlVersion p),
    ("name" A..=) <$> (dOMNodeName p),
    ("value" A..=) <$> (dOMNodeValue p),
    ("pseudoType" A..=) <$> (dOMNodePseudoType p),
    ("pseudoIdentifier" A..=) <$> (dOMNodePseudoIdentifier p),
    ("shadowRootType" A..=) <$> (dOMNodeShadowRootType p),
    ("frameId" A..=) <$> (dOMNodeFrameId p),
    ("contentDocument" A..=) <$> (dOMNodeContentDocument p),
    ("shadowRoots" A..=) <$> (dOMNodeShadowRoots p),
    ("templateContent" A..=) <$> (dOMNodeTemplateContent p),
    ("pseudoElements" A..=) <$> (dOMNodePseudoElements p),
    ("distributedNodes" A..=) <$> (dOMNodeDistributedNodes p),
    ("isSVG" A..=) <$> (dOMNodeIsSVG p),
    ("compatibilityMode" A..=) <$> (dOMNodeCompatibilityMode p),
    ("assignedSlot" A..=) <$> (dOMNodeAssignedSlot p)
    ]

-- | Type 'DOM.RGBA'.
--   A structure holding an RGBA color.
data DOMRGBA = DOMRGBA
  {
    -- | The red component, in the [0-255] range.
    dOMRGBAR :: Int,
    -- | The green component, in the [0-255] range.
    dOMRGBAG :: Int,
    -- | The blue component, in the [0-255] range.
    dOMRGBAB :: Int,
    -- | The alpha component, in the [0-1] range (default: 1).
    dOMRGBAA :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON DOMRGBA where
  parseJSON = A.withObject "DOMRGBA" $ \o -> DOMRGBA
    <$> o A..: "r"
    <*> o A..: "g"
    <*> o A..: "b"
    <*> o A..:? "a"
instance ToJSON DOMRGBA where
  toJSON p = A.object $ catMaybes [
    ("r" A..=) <$> Just (dOMRGBAR p),
    ("g" A..=) <$> Just (dOMRGBAG p),
    ("b" A..=) <$> Just (dOMRGBAB p),
    ("a" A..=) <$> (dOMRGBAA p)
    ]

-- | Type 'DOM.Quad'.
--   An array of quad vertices, x immediately followed by y for each point, points clock-wise.
type DOMQuad = [Double]

-- | Type 'DOM.BoxModel'.
--   Box model.
data DOMBoxModel = DOMBoxModel
  {
    -- | Content box
    dOMBoxModelContent :: DOMQuad,
    -- | Padding box
    dOMBoxModelPadding :: DOMQuad,
    -- | Border box
    dOMBoxModelBorder :: DOMQuad,
    -- | Margin box
    dOMBoxModelMargin :: DOMQuad,
    -- | Node width
    dOMBoxModelWidth :: Int,
    -- | Node height
    dOMBoxModelHeight :: Int,
    -- | Shape outside coordinates
    dOMBoxModelShapeOutside :: Maybe DOMShapeOutsideInfo
  }
  deriving (Eq, Show)
instance FromJSON DOMBoxModel where
  parseJSON = A.withObject "DOMBoxModel" $ \o -> DOMBoxModel
    <$> o A..: "content"
    <*> o A..: "padding"
    <*> o A..: "border"
    <*> o A..: "margin"
    <*> o A..: "width"
    <*> o A..: "height"
    <*> o A..:? "shapeOutside"
instance ToJSON DOMBoxModel where
  toJSON p = A.object $ catMaybes [
    ("content" A..=) <$> Just (dOMBoxModelContent p),
    ("padding" A..=) <$> Just (dOMBoxModelPadding p),
    ("border" A..=) <$> Just (dOMBoxModelBorder p),
    ("margin" A..=) <$> Just (dOMBoxModelMargin p),
    ("width" A..=) <$> Just (dOMBoxModelWidth p),
    ("height" A..=) <$> Just (dOMBoxModelHeight p),
    ("shapeOutside" A..=) <$> (dOMBoxModelShapeOutside p)
    ]

-- | Type 'DOM.ShapeOutsideInfo'.
--   CSS Shape Outside details.
data DOMShapeOutsideInfo = DOMShapeOutsideInfo
  {
    -- | Shape bounds
    dOMShapeOutsideInfoBounds :: DOMQuad,
    -- | Shape coordinate details
    dOMShapeOutsideInfoShape :: [A.Value],
    -- | Margin shape bounds
    dOMShapeOutsideInfoMarginShape :: [A.Value]
  }
  deriving (Eq, Show)
instance FromJSON DOMShapeOutsideInfo where
  parseJSON = A.withObject "DOMShapeOutsideInfo" $ \o -> DOMShapeOutsideInfo
    <$> o A..: "bounds"
    <*> o A..: "shape"
    <*> o A..: "marginShape"
instance ToJSON DOMShapeOutsideInfo where
  toJSON p = A.object $ catMaybes [
    ("bounds" A..=) <$> Just (dOMShapeOutsideInfoBounds p),
    ("shape" A..=) <$> Just (dOMShapeOutsideInfoShape p),
    ("marginShape" A..=) <$> Just (dOMShapeOutsideInfoMarginShape p)
    ]

-- | Type 'DOM.Rect'.
--   Rectangle.
data DOMRect = DOMRect
  {
    -- | X coordinate
    dOMRectX :: Double,
    -- | Y coordinate
    dOMRectY :: Double,
    -- | Rectangle width
    dOMRectWidth :: Double,
    -- | Rectangle height
    dOMRectHeight :: Double
  }
  deriving (Eq, Show)
instance FromJSON DOMRect where
  parseJSON = A.withObject "DOMRect" $ \o -> DOMRect
    <$> o A..: "x"
    <*> o A..: "y"
    <*> o A..: "width"
    <*> o A..: "height"
instance ToJSON DOMRect where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (dOMRectX p),
    ("y" A..=) <$> Just (dOMRectY p),
    ("width" A..=) <$> Just (dOMRectWidth p),
    ("height" A..=) <$> Just (dOMRectHeight p)
    ]

-- | Type 'DOM.CSSComputedStyleProperty'.
data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty
  {
    -- | Computed style property name.
    dOMCSSComputedStylePropertyName :: T.Text,
    -- | Computed style property value.
    dOMCSSComputedStylePropertyValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMCSSComputedStyleProperty where
  parseJSON = A.withObject "DOMCSSComputedStyleProperty" $ \o -> DOMCSSComputedStyleProperty
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON DOMCSSComputedStyleProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (dOMCSSComputedStylePropertyName p),
    ("value" A..=) <$> Just (dOMCSSComputedStylePropertyValue p)
    ]

-- | Type of the 'DOM.attributeModified' event.
data DOMAttributeModified = DOMAttributeModified
  {
    -- | Id of the node that has changed.
    dOMAttributeModifiedNodeId :: DOMNodeId,
    -- | Attribute name.
    dOMAttributeModifiedName :: T.Text,
    -- | Attribute value.
    dOMAttributeModifiedValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMAttributeModified where
  parseJSON = A.withObject "DOMAttributeModified" $ \o -> DOMAttributeModified
    <$> o A..: "nodeId"
    <*> o A..: "name"
    <*> o A..: "value"
instance Event DOMAttributeModified where
  eventName _ = "DOM.attributeModified"

-- | Type of the 'DOM.attributeRemoved' event.
data DOMAttributeRemoved = DOMAttributeRemoved
  {
    -- | Id of the node that has changed.
    dOMAttributeRemovedNodeId :: DOMNodeId,
    -- | A ttribute name.
    dOMAttributeRemovedName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMAttributeRemoved where
  parseJSON = A.withObject "DOMAttributeRemoved" $ \o -> DOMAttributeRemoved
    <$> o A..: "nodeId"
    <*> o A..: "name"
instance Event DOMAttributeRemoved where
  eventName _ = "DOM.attributeRemoved"

-- | Type of the 'DOM.characterDataModified' event.
data DOMCharacterDataModified = DOMCharacterDataModified
  {
    -- | Id of the node that has changed.
    dOMCharacterDataModifiedNodeId :: DOMNodeId,
    -- | New text value.
    dOMCharacterDataModifiedCharacterData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMCharacterDataModified where
  parseJSON = A.withObject "DOMCharacterDataModified" $ \o -> DOMCharacterDataModified
    <$> o A..: "nodeId"
    <*> o A..: "characterData"
instance Event DOMCharacterDataModified where
  eventName _ = "DOM.characterDataModified"

-- | Type of the 'DOM.childNodeCountUpdated' event.
data DOMChildNodeCountUpdated = DOMChildNodeCountUpdated
  {
    -- | Id of the node that has changed.
    dOMChildNodeCountUpdatedNodeId :: DOMNodeId,
    -- | New node count.
    dOMChildNodeCountUpdatedChildNodeCount :: Int
  }
  deriving (Eq, Show)
instance FromJSON DOMChildNodeCountUpdated where
  parseJSON = A.withObject "DOMChildNodeCountUpdated" $ \o -> DOMChildNodeCountUpdated
    <$> o A..: "nodeId"
    <*> o A..: "childNodeCount"
instance Event DOMChildNodeCountUpdated where
  eventName _ = "DOM.childNodeCountUpdated"

-- | Type of the 'DOM.childNodeInserted' event.
data DOMChildNodeInserted = DOMChildNodeInserted
  {
    -- | Id of the node that has changed.
    dOMChildNodeInsertedParentNodeId :: DOMNodeId,
    -- | Id of the previous sibling.
    dOMChildNodeInsertedPreviousNodeId :: DOMNodeId,
    -- | Inserted node data.
    dOMChildNodeInsertedNode :: DOMNode
  }
  deriving (Eq, Show)
instance FromJSON DOMChildNodeInserted where
  parseJSON = A.withObject "DOMChildNodeInserted" $ \o -> DOMChildNodeInserted
    <$> o A..: "parentNodeId"
    <*> o A..: "previousNodeId"
    <*> o A..: "node"
instance Event DOMChildNodeInserted where
  eventName _ = "DOM.childNodeInserted"

-- | Type of the 'DOM.childNodeRemoved' event.
data DOMChildNodeRemoved = DOMChildNodeRemoved
  {
    -- | Parent id.
    dOMChildNodeRemovedParentNodeId :: DOMNodeId,
    -- | Id of the node that has been removed.
    dOMChildNodeRemovedNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMChildNodeRemoved where
  parseJSON = A.withObject "DOMChildNodeRemoved" $ \o -> DOMChildNodeRemoved
    <$> o A..: "parentNodeId"
    <*> o A..: "nodeId"
instance Event DOMChildNodeRemoved where
  eventName _ = "DOM.childNodeRemoved"

-- | Type of the 'DOM.distributedNodesUpdated' event.
data DOMDistributedNodesUpdated = DOMDistributedNodesUpdated
  {
    -- | Insertion point where distributed nodes were updated.
    dOMDistributedNodesUpdatedInsertionPointId :: DOMNodeId,
    -- | Distributed nodes for given insertion point.
    dOMDistributedNodesUpdatedDistributedNodes :: [DOMBackendNode]
  }
  deriving (Eq, Show)
instance FromJSON DOMDistributedNodesUpdated where
  parseJSON = A.withObject "DOMDistributedNodesUpdated" $ \o -> DOMDistributedNodesUpdated
    <$> o A..: "insertionPointId"
    <*> o A..: "distributedNodes"
instance Event DOMDistributedNodesUpdated where
  eventName _ = "DOM.distributedNodesUpdated"

-- | Type of the 'DOM.documentUpdated' event.
data DOMDocumentUpdated = DOMDocumentUpdated
  deriving (Eq, Show, Read)
instance FromJSON DOMDocumentUpdated where
  parseJSON _ = pure DOMDocumentUpdated
instance Event DOMDocumentUpdated where
  eventName _ = "DOM.documentUpdated"

-- | Type of the 'DOM.inlineStyleInvalidated' event.
data DOMInlineStyleInvalidated = DOMInlineStyleInvalidated
  {
    -- | Ids of the nodes for which the inline styles have been invalidated.
    dOMInlineStyleInvalidatedNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMInlineStyleInvalidated where
  parseJSON = A.withObject "DOMInlineStyleInvalidated" $ \o -> DOMInlineStyleInvalidated
    <$> o A..: "nodeIds"
instance Event DOMInlineStyleInvalidated where
  eventName _ = "DOM.inlineStyleInvalidated"

-- | Type of the 'DOM.pseudoElementAdded' event.
data DOMPseudoElementAdded = DOMPseudoElementAdded
  {
    -- | Pseudo element's parent element id.
    dOMPseudoElementAddedParentId :: DOMNodeId,
    -- | The added pseudo element.
    dOMPseudoElementAddedPseudoElement :: DOMNode
  }
  deriving (Eq, Show)
instance FromJSON DOMPseudoElementAdded where
  parseJSON = A.withObject "DOMPseudoElementAdded" $ \o -> DOMPseudoElementAdded
    <$> o A..: "parentId"
    <*> o A..: "pseudoElement"
instance Event DOMPseudoElementAdded where
  eventName _ = "DOM.pseudoElementAdded"

-- | Type of the 'DOM.topLayerElementsUpdated' event.
data DOMTopLayerElementsUpdated = DOMTopLayerElementsUpdated
  deriving (Eq, Show, Read)
instance FromJSON DOMTopLayerElementsUpdated where
  parseJSON _ = pure DOMTopLayerElementsUpdated
instance Event DOMTopLayerElementsUpdated where
  eventName _ = "DOM.topLayerElementsUpdated"

-- | Type of the 'DOM.pseudoElementRemoved' event.
data DOMPseudoElementRemoved = DOMPseudoElementRemoved
  {
    -- | Pseudo element's parent element id.
    dOMPseudoElementRemovedParentId :: DOMNodeId,
    -- | The removed pseudo element id.
    dOMPseudoElementRemovedPseudoElementId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMPseudoElementRemoved where
  parseJSON = A.withObject "DOMPseudoElementRemoved" $ \o -> DOMPseudoElementRemoved
    <$> o A..: "parentId"
    <*> o A..: "pseudoElementId"
instance Event DOMPseudoElementRemoved where
  eventName _ = "DOM.pseudoElementRemoved"

-- | Type of the 'DOM.setChildNodes' event.
data DOMSetChildNodes = DOMSetChildNodes
  {
    -- | Parent node id to populate with children.
    dOMSetChildNodesParentId :: DOMNodeId,
    -- | Child nodes array.
    dOMSetChildNodesNodes :: [DOMNode]
  }
  deriving (Eq, Show)
instance FromJSON DOMSetChildNodes where
  parseJSON = A.withObject "DOMSetChildNodes" $ \o -> DOMSetChildNodes
    <$> o A..: "parentId"
    <*> o A..: "nodes"
instance Event DOMSetChildNodes where
  eventName _ = "DOM.setChildNodes"

-- | Type of the 'DOM.shadowRootPopped' event.
data DOMShadowRootPopped = DOMShadowRootPopped
  {
    -- | Host element id.
    dOMShadowRootPoppedHostId :: DOMNodeId,
    -- | Shadow root id.
    dOMShadowRootPoppedRootId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMShadowRootPopped where
  parseJSON = A.withObject "DOMShadowRootPopped" $ \o -> DOMShadowRootPopped
    <$> o A..: "hostId"
    <*> o A..: "rootId"
instance Event DOMShadowRootPopped where
  eventName _ = "DOM.shadowRootPopped"

-- | Type of the 'DOM.shadowRootPushed' event.
data DOMShadowRootPushed = DOMShadowRootPushed
  {
    -- | Host element id.
    dOMShadowRootPushedHostId :: DOMNodeId,
    -- | Shadow root.
    dOMShadowRootPushedRoot :: DOMNode
  }
  deriving (Eq, Show)
instance FromJSON DOMShadowRootPushed where
  parseJSON = A.withObject "DOMShadowRootPushed" $ \o -> DOMShadowRootPushed
    <$> o A..: "hostId"
    <*> o A..: "root"
instance Event DOMShadowRootPushed where
  eventName _ = "DOM.shadowRootPushed"

-- | Collects class names for the node with given id and all of it's child nodes.

-- | Parameters of the 'DOM.collectClassNamesFromSubtree' command.
data PDOMCollectClassNamesFromSubtree = PDOMCollectClassNamesFromSubtree
  {
    -- | Id of the node to collect class names.
    pDOMCollectClassNamesFromSubtreeNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMCollectClassNamesFromSubtree
  -- | Id of the node to collect class names.
  :: DOMNodeId
  -> PDOMCollectClassNamesFromSubtree
pDOMCollectClassNamesFromSubtree
  arg_pDOMCollectClassNamesFromSubtreeNodeId
  = PDOMCollectClassNamesFromSubtree
    arg_pDOMCollectClassNamesFromSubtreeNodeId
instance ToJSON PDOMCollectClassNamesFromSubtree where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMCollectClassNamesFromSubtreeNodeId p)
    ]
data DOMCollectClassNamesFromSubtree = DOMCollectClassNamesFromSubtree
  {
    -- | Class name list.
    dOMCollectClassNamesFromSubtreeClassNames :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON DOMCollectClassNamesFromSubtree where
  parseJSON = A.withObject "DOMCollectClassNamesFromSubtree" $ \o -> DOMCollectClassNamesFromSubtree
    <$> o A..: "classNames"
instance Command PDOMCollectClassNamesFromSubtree where
  type CommandResponse PDOMCollectClassNamesFromSubtree = DOMCollectClassNamesFromSubtree
  commandName _ = "DOM.collectClassNamesFromSubtree"

-- | Creates a deep copy of the specified node and places it into the target container before the
--   given anchor.

-- | Parameters of the 'DOM.copyTo' command.
data PDOMCopyTo = PDOMCopyTo
  {
    -- | Id of the node to copy.
    pDOMCopyToNodeId :: DOMNodeId,
    -- | Id of the element to drop the copy into.
    pDOMCopyToTargetNodeId :: DOMNodeId,
    -- | Drop the copy before this node (if absent, the copy becomes the last child of
    --   `targetNodeId`).
    pDOMCopyToInsertBeforeNodeId :: Maybe DOMNodeId
  }
  deriving (Eq, Show)
pDOMCopyTo
  -- | Id of the node to copy.
  :: DOMNodeId
  -- | Id of the element to drop the copy into.
  -> DOMNodeId
  -> PDOMCopyTo
pDOMCopyTo
  arg_pDOMCopyToNodeId
  arg_pDOMCopyToTargetNodeId
  = PDOMCopyTo
    arg_pDOMCopyToNodeId
    arg_pDOMCopyToTargetNodeId
    Nothing
instance ToJSON PDOMCopyTo where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMCopyToNodeId p),
    ("targetNodeId" A..=) <$> Just (pDOMCopyToTargetNodeId p),
    ("insertBeforeNodeId" A..=) <$> (pDOMCopyToInsertBeforeNodeId p)
    ]
data DOMCopyTo = DOMCopyTo
  {
    -- | Id of the node clone.
    dOMCopyToNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMCopyTo where
  parseJSON = A.withObject "DOMCopyTo" $ \o -> DOMCopyTo
    <$> o A..: "nodeId"
instance Command PDOMCopyTo where
  type CommandResponse PDOMCopyTo = DOMCopyTo
  commandName _ = "DOM.copyTo"

-- | Describes node given its id, does not require domain to be enabled. Does not start tracking any
--   objects, can be used for automation.

-- | Parameters of the 'DOM.describeNode' command.
data PDOMDescribeNode = PDOMDescribeNode
  {
    -- | Identifier of the node.
    pDOMDescribeNodeNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMDescribeNodeBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMDescribeNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
    -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
    --   entire subtree or provide an integer larger than 0.
    pDOMDescribeNodeDepth :: Maybe Int,
    -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
    --   (default is false).
    pDOMDescribeNodePierce :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMDescribeNode
  :: PDOMDescribeNode
pDOMDescribeNode
  = PDOMDescribeNode
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMDescribeNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMDescribeNodeNodeId p),
    ("backendNodeId" A..=) <$> (pDOMDescribeNodeBackendNodeId p),
    ("objectId" A..=) <$> (pDOMDescribeNodeObjectId p),
    ("depth" A..=) <$> (pDOMDescribeNodeDepth p),
    ("pierce" A..=) <$> (pDOMDescribeNodePierce p)
    ]
data DOMDescribeNode = DOMDescribeNode
  {
    -- | Node description.
    dOMDescribeNodeNode :: DOMNode
  }
  deriving (Eq, Show)
instance FromJSON DOMDescribeNode where
  parseJSON = A.withObject "DOMDescribeNode" $ \o -> DOMDescribeNode
    <$> o A..: "node"
instance Command PDOMDescribeNode where
  type CommandResponse PDOMDescribeNode = DOMDescribeNode
  commandName _ = "DOM.describeNode"

-- | Scrolls the specified rect of the given node into view if not already visible.
--   Note: exactly one between nodeId, backendNodeId and objectId should be passed
--   to identify the node.

-- | Parameters of the 'DOM.scrollIntoViewIfNeeded' command.
data PDOMScrollIntoViewIfNeeded = PDOMScrollIntoViewIfNeeded
  {
    -- | Identifier of the node.
    pDOMScrollIntoViewIfNeededNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMScrollIntoViewIfNeededBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMScrollIntoViewIfNeededObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
    -- | The rect to be scrolled into view, relative to the node's border box, in CSS pixels.
    --   When omitted, center of the node will be used, similar to Element.scrollIntoView.
    pDOMScrollIntoViewIfNeededRect :: Maybe DOMRect
  }
  deriving (Eq, Show)
pDOMScrollIntoViewIfNeeded
  :: PDOMScrollIntoViewIfNeeded
pDOMScrollIntoViewIfNeeded
  = PDOMScrollIntoViewIfNeeded
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMScrollIntoViewIfNeeded where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMScrollIntoViewIfNeededNodeId p),
    ("backendNodeId" A..=) <$> (pDOMScrollIntoViewIfNeededBackendNodeId p),
    ("objectId" A..=) <$> (pDOMScrollIntoViewIfNeededObjectId p),
    ("rect" A..=) <$> (pDOMScrollIntoViewIfNeededRect p)
    ]
instance Command PDOMScrollIntoViewIfNeeded where
  type CommandResponse PDOMScrollIntoViewIfNeeded = ()
  commandName _ = "DOM.scrollIntoViewIfNeeded"
  fromJSON = const . A.Success . const ()

-- | Disables DOM agent for the given page.

-- | Parameters of the 'DOM.disable' command.
data PDOMDisable = PDOMDisable
  deriving (Eq, Show)
pDOMDisable
  :: PDOMDisable
pDOMDisable
  = PDOMDisable
instance ToJSON PDOMDisable where
  toJSON _ = A.Null
instance Command PDOMDisable where
  type CommandResponse PDOMDisable = ()
  commandName _ = "DOM.disable"
  fromJSON = const . A.Success . const ()

-- | Discards search results from the session with the given id. `getSearchResults` should no longer
--   be called for that search.

-- | Parameters of the 'DOM.discardSearchResults' command.
data PDOMDiscardSearchResults = PDOMDiscardSearchResults
  {
    -- | Unique search session identifier.
    pDOMDiscardSearchResultsSearchId :: T.Text
  }
  deriving (Eq, Show)
pDOMDiscardSearchResults
  -- | Unique search session identifier.
  :: T.Text
  -> PDOMDiscardSearchResults
pDOMDiscardSearchResults
  arg_pDOMDiscardSearchResultsSearchId
  = PDOMDiscardSearchResults
    arg_pDOMDiscardSearchResultsSearchId
instance ToJSON PDOMDiscardSearchResults where
  toJSON p = A.object $ catMaybes [
    ("searchId" A..=) <$> Just (pDOMDiscardSearchResultsSearchId p)
    ]
instance Command PDOMDiscardSearchResults where
  type CommandResponse PDOMDiscardSearchResults = ()
  commandName _ = "DOM.discardSearchResults"
  fromJSON = const . A.Success . const ()

-- | Enables DOM agent for the given page.

-- | Parameters of the 'DOM.enable' command.
data PDOMEnableIncludeWhitespace = PDOMEnableIncludeWhitespaceNone | PDOMEnableIncludeWhitespaceAll
  deriving (Ord, Eq, Show, Read)
instance FromJSON PDOMEnableIncludeWhitespace where
  parseJSON = A.withText "PDOMEnableIncludeWhitespace" $ \v -> case v of
    "none" -> pure PDOMEnableIncludeWhitespaceNone
    "all" -> pure PDOMEnableIncludeWhitespaceAll
    "_" -> fail "failed to parse PDOMEnableIncludeWhitespace"
instance ToJSON PDOMEnableIncludeWhitespace where
  toJSON v = A.String $ case v of
    PDOMEnableIncludeWhitespaceNone -> "none"
    PDOMEnableIncludeWhitespaceAll -> "all"
data PDOMEnable = PDOMEnable
  {
    -- | Whether to include whitespaces in the children array of returned Nodes.
    pDOMEnableIncludeWhitespace :: Maybe PDOMEnableIncludeWhitespace
  }
  deriving (Eq, Show)
pDOMEnable
  :: PDOMEnable
pDOMEnable
  = PDOMEnable
    Nothing
instance ToJSON PDOMEnable where
  toJSON p = A.object $ catMaybes [
    ("includeWhitespace" A..=) <$> (pDOMEnableIncludeWhitespace p)
    ]
instance Command PDOMEnable where
  type CommandResponse PDOMEnable = ()
  commandName _ = "DOM.enable"
  fromJSON = const . A.Success . const ()

-- | Focuses the given element.

-- | Parameters of the 'DOM.focus' command.
data PDOMFocus = PDOMFocus
  {
    -- | Identifier of the node.
    pDOMFocusNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMFocusBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMFocusObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMFocus
  :: PDOMFocus
pDOMFocus
  = PDOMFocus
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMFocus where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMFocusNodeId p),
    ("backendNodeId" A..=) <$> (pDOMFocusBackendNodeId p),
    ("objectId" A..=) <$> (pDOMFocusObjectId p)
    ]
instance Command PDOMFocus where
  type CommandResponse PDOMFocus = ()
  commandName _ = "DOM.focus"
  fromJSON = const . A.Success . const ()

-- | Returns attributes for the specified node.

-- | Parameters of the 'DOM.getAttributes' command.
data PDOMGetAttributes = PDOMGetAttributes
  {
    -- | Id of the node to retrieve attibutes for.
    pDOMGetAttributesNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMGetAttributes
  -- | Id of the node to retrieve attibutes for.
  :: DOMNodeId
  -> PDOMGetAttributes
pDOMGetAttributes
  arg_pDOMGetAttributesNodeId
  = PDOMGetAttributes
    arg_pDOMGetAttributesNodeId
instance ToJSON PDOMGetAttributes where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetAttributesNodeId p)
    ]
data DOMGetAttributes = DOMGetAttributes
  {
    -- | An interleaved array of node attribute names and values.
    dOMGetAttributesAttributes :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetAttributes where
  parseJSON = A.withObject "DOMGetAttributes" $ \o -> DOMGetAttributes
    <$> o A..: "attributes"
instance Command PDOMGetAttributes where
  type CommandResponse PDOMGetAttributes = DOMGetAttributes
  commandName _ = "DOM.getAttributes"

-- | Returns boxes for the given node.

-- | Parameters of the 'DOM.getBoxModel' command.
data PDOMGetBoxModel = PDOMGetBoxModel
  {
    -- | Identifier of the node.
    pDOMGetBoxModelNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMGetBoxModelBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMGetBoxModelObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMGetBoxModel
  :: PDOMGetBoxModel
pDOMGetBoxModel
  = PDOMGetBoxModel
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMGetBoxModel where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMGetBoxModelNodeId p),
    ("backendNodeId" A..=) <$> (pDOMGetBoxModelBackendNodeId p),
    ("objectId" A..=) <$> (pDOMGetBoxModelObjectId p)
    ]
data DOMGetBoxModel = DOMGetBoxModel
  {
    -- | Box model for the node.
    dOMGetBoxModelModel :: DOMBoxModel
  }
  deriving (Eq, Show)
instance FromJSON DOMGetBoxModel where
  parseJSON = A.withObject "DOMGetBoxModel" $ \o -> DOMGetBoxModel
    <$> o A..: "model"
instance Command PDOMGetBoxModel where
  type CommandResponse PDOMGetBoxModel = DOMGetBoxModel
  commandName _ = "DOM.getBoxModel"

-- | Returns quads that describe node position on the page. This method
--   might return multiple quads for inline nodes.

-- | Parameters of the 'DOM.getContentQuads' command.
data PDOMGetContentQuads = PDOMGetContentQuads
  {
    -- | Identifier of the node.
    pDOMGetContentQuadsNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMGetContentQuadsBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMGetContentQuadsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMGetContentQuads
  :: PDOMGetContentQuads
pDOMGetContentQuads
  = PDOMGetContentQuads
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMGetContentQuads where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMGetContentQuadsNodeId p),
    ("backendNodeId" A..=) <$> (pDOMGetContentQuadsBackendNodeId p),
    ("objectId" A..=) <$> (pDOMGetContentQuadsObjectId p)
    ]
data DOMGetContentQuads = DOMGetContentQuads
  {
    -- | Quads that describe node layout relative to viewport.
    dOMGetContentQuadsQuads :: [DOMQuad]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetContentQuads where
  parseJSON = A.withObject "DOMGetContentQuads" $ \o -> DOMGetContentQuads
    <$> o A..: "quads"
instance Command PDOMGetContentQuads where
  type CommandResponse PDOMGetContentQuads = DOMGetContentQuads
  commandName _ = "DOM.getContentQuads"

-- | Returns the root DOM node (and optionally the subtree) to the caller.

-- | Parameters of the 'DOM.getDocument' command.
data PDOMGetDocument = PDOMGetDocument
  {
    -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
    --   entire subtree or provide an integer larger than 0.
    pDOMGetDocumentDepth :: Maybe Int,
    -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
    --   (default is false).
    pDOMGetDocumentPierce :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMGetDocument
  :: PDOMGetDocument
pDOMGetDocument
  = PDOMGetDocument
    Nothing
    Nothing
instance ToJSON PDOMGetDocument where
  toJSON p = A.object $ catMaybes [
    ("depth" A..=) <$> (pDOMGetDocumentDepth p),
    ("pierce" A..=) <$> (pDOMGetDocumentPierce p)
    ]
data DOMGetDocument = DOMGetDocument
  {
    -- | Resulting node.
    dOMGetDocumentRoot :: DOMNode
  }
  deriving (Eq, Show)
instance FromJSON DOMGetDocument where
  parseJSON = A.withObject "DOMGetDocument" $ \o -> DOMGetDocument
    <$> o A..: "root"
instance Command PDOMGetDocument where
  type CommandResponse PDOMGetDocument = DOMGetDocument
  commandName _ = "DOM.getDocument"

-- | Finds nodes with a given computed style in a subtree.

-- | Parameters of the 'DOM.getNodesForSubtreeByStyle' command.
data PDOMGetNodesForSubtreeByStyle = PDOMGetNodesForSubtreeByStyle
  {
    -- | Node ID pointing to the root of a subtree.
    pDOMGetNodesForSubtreeByStyleNodeId :: DOMNodeId,
    -- | The style to filter nodes by (includes nodes if any of properties matches).
    pDOMGetNodesForSubtreeByStyleComputedStyles :: [DOMCSSComputedStyleProperty],
    -- | Whether or not iframes and shadow roots in the same target should be traversed when returning the
    --   results (default is false).
    pDOMGetNodesForSubtreeByStylePierce :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMGetNodesForSubtreeByStyle
  -- | Node ID pointing to the root of a subtree.
  :: DOMNodeId
  -- | The style to filter nodes by (includes nodes if any of properties matches).
  -> [DOMCSSComputedStyleProperty]
  -> PDOMGetNodesForSubtreeByStyle
pDOMGetNodesForSubtreeByStyle
  arg_pDOMGetNodesForSubtreeByStyleNodeId
  arg_pDOMGetNodesForSubtreeByStyleComputedStyles
  = PDOMGetNodesForSubtreeByStyle
    arg_pDOMGetNodesForSubtreeByStyleNodeId
    arg_pDOMGetNodesForSubtreeByStyleComputedStyles
    Nothing
instance ToJSON PDOMGetNodesForSubtreeByStyle where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetNodesForSubtreeByStyleNodeId p),
    ("computedStyles" A..=) <$> Just (pDOMGetNodesForSubtreeByStyleComputedStyles p),
    ("pierce" A..=) <$> (pDOMGetNodesForSubtreeByStylePierce p)
    ]
data DOMGetNodesForSubtreeByStyle = DOMGetNodesForSubtreeByStyle
  {
    -- | Resulting nodes.
    dOMGetNodesForSubtreeByStyleNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetNodesForSubtreeByStyle where
  parseJSON = A.withObject "DOMGetNodesForSubtreeByStyle" $ \o -> DOMGetNodesForSubtreeByStyle
    <$> o A..: "nodeIds"
instance Command PDOMGetNodesForSubtreeByStyle where
  type CommandResponse PDOMGetNodesForSubtreeByStyle = DOMGetNodesForSubtreeByStyle
  commandName _ = "DOM.getNodesForSubtreeByStyle"

-- | Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
--   either returned or not.

-- | Parameters of the 'DOM.getNodeForLocation' command.
data PDOMGetNodeForLocation = PDOMGetNodeForLocation
  {
    -- | X coordinate.
    pDOMGetNodeForLocationX :: Int,
    -- | Y coordinate.
    pDOMGetNodeForLocationY :: Int,
    -- | False to skip to the nearest non-UA shadow root ancestor (default: false).
    pDOMGetNodeForLocationIncludeUserAgentShadowDOM :: Maybe Bool,
    -- | Whether to ignore pointer-events: none on elements and hit test them.
    pDOMGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMGetNodeForLocation
  -- | X coordinate.
  :: Int
  -- | Y coordinate.
  -> Int
  -> PDOMGetNodeForLocation
pDOMGetNodeForLocation
  arg_pDOMGetNodeForLocationX
  arg_pDOMGetNodeForLocationY
  = PDOMGetNodeForLocation
    arg_pDOMGetNodeForLocationX
    arg_pDOMGetNodeForLocationY
    Nothing
    Nothing
instance ToJSON PDOMGetNodeForLocation where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pDOMGetNodeForLocationX p),
    ("y" A..=) <$> Just (pDOMGetNodeForLocationY p),
    ("includeUserAgentShadowDOM" A..=) <$> (pDOMGetNodeForLocationIncludeUserAgentShadowDOM p),
    ("ignorePointerEventsNone" A..=) <$> (pDOMGetNodeForLocationIgnorePointerEventsNone p)
    ]
data DOMGetNodeForLocation = DOMGetNodeForLocation
  {
    -- | Resulting node.
    dOMGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    -- | Frame this node belongs to.
    dOMGetNodeForLocationFrameId :: PageFrameId,
    -- | Id of the node at given coordinates, only when enabled and requested document.
    dOMGetNodeForLocationNodeId :: Maybe DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMGetNodeForLocation where
  parseJSON = A.withObject "DOMGetNodeForLocation" $ \o -> DOMGetNodeForLocation
    <$> o A..: "backendNodeId"
    <*> o A..: "frameId"
    <*> o A..:? "nodeId"
instance Command PDOMGetNodeForLocation where
  type CommandResponse PDOMGetNodeForLocation = DOMGetNodeForLocation
  commandName _ = "DOM.getNodeForLocation"

-- | Returns node's HTML markup.

-- | Parameters of the 'DOM.getOuterHTML' command.
data PDOMGetOuterHTML = PDOMGetOuterHTML
  {
    -- | Identifier of the node.
    pDOMGetOuterHTMLNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMGetOuterHTMLBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMGetOuterHTMLObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMGetOuterHTML
  :: PDOMGetOuterHTML
pDOMGetOuterHTML
  = PDOMGetOuterHTML
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMGetOuterHTML where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMGetOuterHTMLNodeId p),
    ("backendNodeId" A..=) <$> (pDOMGetOuterHTMLBackendNodeId p),
    ("objectId" A..=) <$> (pDOMGetOuterHTMLObjectId p)
    ]
data DOMGetOuterHTML = DOMGetOuterHTML
  {
    -- | Outer HTML markup.
    dOMGetOuterHTMLOuterHTML :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMGetOuterHTML where
  parseJSON = A.withObject "DOMGetOuterHTML" $ \o -> DOMGetOuterHTML
    <$> o A..: "outerHTML"
instance Command PDOMGetOuterHTML where
  type CommandResponse PDOMGetOuterHTML = DOMGetOuterHTML
  commandName _ = "DOM.getOuterHTML"

-- | Returns the id of the nearest ancestor that is a relayout boundary.

-- | Parameters of the 'DOM.getRelayoutBoundary' command.
data PDOMGetRelayoutBoundary = PDOMGetRelayoutBoundary
  {
    -- | Id of the node.
    pDOMGetRelayoutBoundaryNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMGetRelayoutBoundary
  -- | Id of the node.
  :: DOMNodeId
  -> PDOMGetRelayoutBoundary
pDOMGetRelayoutBoundary
  arg_pDOMGetRelayoutBoundaryNodeId
  = PDOMGetRelayoutBoundary
    arg_pDOMGetRelayoutBoundaryNodeId
instance ToJSON PDOMGetRelayoutBoundary where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetRelayoutBoundaryNodeId p)
    ]
data DOMGetRelayoutBoundary = DOMGetRelayoutBoundary
  {
    -- | Relayout boundary node id for the given node.
    dOMGetRelayoutBoundaryNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMGetRelayoutBoundary where
  parseJSON = A.withObject "DOMGetRelayoutBoundary" $ \o -> DOMGetRelayoutBoundary
    <$> o A..: "nodeId"
instance Command PDOMGetRelayoutBoundary where
  type CommandResponse PDOMGetRelayoutBoundary = DOMGetRelayoutBoundary
  commandName _ = "DOM.getRelayoutBoundary"

-- | Returns search results from given `fromIndex` to given `toIndex` from the search with the given
--   identifier.

-- | Parameters of the 'DOM.getSearchResults' command.
data PDOMGetSearchResults = PDOMGetSearchResults
  {
    -- | Unique search session identifier.
    pDOMGetSearchResultsSearchId :: T.Text,
    -- | Start index of the search result to be returned.
    pDOMGetSearchResultsFromIndex :: Int,
    -- | End index of the search result to be returned.
    pDOMGetSearchResultsToIndex :: Int
  }
  deriving (Eq, Show)
pDOMGetSearchResults
  -- | Unique search session identifier.
  :: T.Text
  -- | Start index of the search result to be returned.
  -> Int
  -- | End index of the search result to be returned.
  -> Int
  -> PDOMGetSearchResults
pDOMGetSearchResults
  arg_pDOMGetSearchResultsSearchId
  arg_pDOMGetSearchResultsFromIndex
  arg_pDOMGetSearchResultsToIndex
  = PDOMGetSearchResults
    arg_pDOMGetSearchResultsSearchId
    arg_pDOMGetSearchResultsFromIndex
    arg_pDOMGetSearchResultsToIndex
instance ToJSON PDOMGetSearchResults where
  toJSON p = A.object $ catMaybes [
    ("searchId" A..=) <$> Just (pDOMGetSearchResultsSearchId p),
    ("fromIndex" A..=) <$> Just (pDOMGetSearchResultsFromIndex p),
    ("toIndex" A..=) <$> Just (pDOMGetSearchResultsToIndex p)
    ]
data DOMGetSearchResults = DOMGetSearchResults
  {
    -- | Ids of the search result nodes.
    dOMGetSearchResultsNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetSearchResults where
  parseJSON = A.withObject "DOMGetSearchResults" $ \o -> DOMGetSearchResults
    <$> o A..: "nodeIds"
instance Command PDOMGetSearchResults where
  type CommandResponse PDOMGetSearchResults = DOMGetSearchResults
  commandName _ = "DOM.getSearchResults"

-- | Hides any highlight.

-- | Parameters of the 'DOM.hideHighlight' command.
data PDOMHideHighlight = PDOMHideHighlight
  deriving (Eq, Show)
pDOMHideHighlight
  :: PDOMHideHighlight
pDOMHideHighlight
  = PDOMHideHighlight
instance ToJSON PDOMHideHighlight where
  toJSON _ = A.Null
instance Command PDOMHideHighlight where
  type CommandResponse PDOMHideHighlight = ()
  commandName _ = "DOM.hideHighlight"
  fromJSON = const . A.Success . const ()

-- | Highlights DOM node.

-- | Parameters of the 'DOM.highlightNode' command.
data PDOMHighlightNode = PDOMHighlightNode
  deriving (Eq, Show)
pDOMHighlightNode
  :: PDOMHighlightNode
pDOMHighlightNode
  = PDOMHighlightNode
instance ToJSON PDOMHighlightNode where
  toJSON _ = A.Null
instance Command PDOMHighlightNode where
  type CommandResponse PDOMHighlightNode = ()
  commandName _ = "DOM.highlightNode"
  fromJSON = const . A.Success . const ()

-- | Highlights given rectangle.

-- | Parameters of the 'DOM.highlightRect' command.
data PDOMHighlightRect = PDOMHighlightRect
  deriving (Eq, Show)
pDOMHighlightRect
  :: PDOMHighlightRect
pDOMHighlightRect
  = PDOMHighlightRect
instance ToJSON PDOMHighlightRect where
  toJSON _ = A.Null
instance Command PDOMHighlightRect where
  type CommandResponse PDOMHighlightRect = ()
  commandName _ = "DOM.highlightRect"
  fromJSON = const . A.Success . const ()

-- | Marks last undoable state.

-- | Parameters of the 'DOM.markUndoableState' command.
data PDOMMarkUndoableState = PDOMMarkUndoableState
  deriving (Eq, Show)
pDOMMarkUndoableState
  :: PDOMMarkUndoableState
pDOMMarkUndoableState
  = PDOMMarkUndoableState
instance ToJSON PDOMMarkUndoableState where
  toJSON _ = A.Null
instance Command PDOMMarkUndoableState where
  type CommandResponse PDOMMarkUndoableState = ()
  commandName _ = "DOM.markUndoableState"
  fromJSON = const . A.Success . const ()

-- | Moves node into the new container, places it before the given anchor.

-- | Parameters of the 'DOM.moveTo' command.
data PDOMMoveTo = PDOMMoveTo
  {
    -- | Id of the node to move.
    pDOMMoveToNodeId :: DOMNodeId,
    -- | Id of the element to drop the moved node into.
    pDOMMoveToTargetNodeId :: DOMNodeId,
    -- | Drop node before this one (if absent, the moved node becomes the last child of
    --   `targetNodeId`).
    pDOMMoveToInsertBeforeNodeId :: Maybe DOMNodeId
  }
  deriving (Eq, Show)
pDOMMoveTo
  -- | Id of the node to move.
  :: DOMNodeId
  -- | Id of the element to drop the moved node into.
  -> DOMNodeId
  -> PDOMMoveTo
pDOMMoveTo
  arg_pDOMMoveToNodeId
  arg_pDOMMoveToTargetNodeId
  = PDOMMoveTo
    arg_pDOMMoveToNodeId
    arg_pDOMMoveToTargetNodeId
    Nothing
instance ToJSON PDOMMoveTo where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMMoveToNodeId p),
    ("targetNodeId" A..=) <$> Just (pDOMMoveToTargetNodeId p),
    ("insertBeforeNodeId" A..=) <$> (pDOMMoveToInsertBeforeNodeId p)
    ]
data DOMMoveTo = DOMMoveTo
  {
    -- | New id of the moved node.
    dOMMoveToNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMMoveTo where
  parseJSON = A.withObject "DOMMoveTo" $ \o -> DOMMoveTo
    <$> o A..: "nodeId"
instance Command PDOMMoveTo where
  type CommandResponse PDOMMoveTo = DOMMoveTo
  commandName _ = "DOM.moveTo"

-- | Searches for a given string in the DOM tree. Use `getSearchResults` to access search results or
--   `cancelSearch` to end this search session.

-- | Parameters of the 'DOM.performSearch' command.
data PDOMPerformSearch = PDOMPerformSearch
  {
    -- | Plain text or query selector or XPath search query.
    pDOMPerformSearchQuery :: T.Text,
    -- | True to search in user agent shadow DOM.
    pDOMPerformSearchIncludeUserAgentShadowDOM :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMPerformSearch
  -- | Plain text or query selector or XPath search query.
  :: T.Text
  -> PDOMPerformSearch
pDOMPerformSearch
  arg_pDOMPerformSearchQuery
  = PDOMPerformSearch
    arg_pDOMPerformSearchQuery
    Nothing
instance ToJSON PDOMPerformSearch where
  toJSON p = A.object $ catMaybes [
    ("query" A..=) <$> Just (pDOMPerformSearchQuery p),
    ("includeUserAgentShadowDOM" A..=) <$> (pDOMPerformSearchIncludeUserAgentShadowDOM p)
    ]
data DOMPerformSearch = DOMPerformSearch
  {
    -- | Unique search session identifier.
    dOMPerformSearchSearchId :: T.Text,
    -- | Number of search results.
    dOMPerformSearchResultCount :: Int
  }
  deriving (Eq, Show)
instance FromJSON DOMPerformSearch where
  parseJSON = A.withObject "DOMPerformSearch" $ \o -> DOMPerformSearch
    <$> o A..: "searchId"
    <*> o A..: "resultCount"
instance Command PDOMPerformSearch where
  type CommandResponse PDOMPerformSearch = DOMPerformSearch
  commandName _ = "DOM.performSearch"

-- | Requests that the node is sent to the caller given its path. // FIXME, use XPath

-- | Parameters of the 'DOM.pushNodeByPathToFrontend' command.
data PDOMPushNodeByPathToFrontend = PDOMPushNodeByPathToFrontend
  {
    -- | Path to node in the proprietary format.
    pDOMPushNodeByPathToFrontendPath :: T.Text
  }
  deriving (Eq, Show)
pDOMPushNodeByPathToFrontend
  -- | Path to node in the proprietary format.
  :: T.Text
  -> PDOMPushNodeByPathToFrontend
pDOMPushNodeByPathToFrontend
  arg_pDOMPushNodeByPathToFrontendPath
  = PDOMPushNodeByPathToFrontend
    arg_pDOMPushNodeByPathToFrontendPath
instance ToJSON PDOMPushNodeByPathToFrontend where
  toJSON p = A.object $ catMaybes [
    ("path" A..=) <$> Just (pDOMPushNodeByPathToFrontendPath p)
    ]
data DOMPushNodeByPathToFrontend = DOMPushNodeByPathToFrontend
  {
    -- | Id of the node for given path.
    dOMPushNodeByPathToFrontendNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMPushNodeByPathToFrontend where
  parseJSON = A.withObject "DOMPushNodeByPathToFrontend" $ \o -> DOMPushNodeByPathToFrontend
    <$> o A..: "nodeId"
instance Command PDOMPushNodeByPathToFrontend where
  type CommandResponse PDOMPushNodeByPathToFrontend = DOMPushNodeByPathToFrontend
  commandName _ = "DOM.pushNodeByPathToFrontend"

-- | Requests that a batch of nodes is sent to the caller given their backend node ids.

-- | Parameters of the 'DOM.pushNodesByBackendIdsToFrontend' command.
data PDOMPushNodesByBackendIdsToFrontend = PDOMPushNodesByBackendIdsToFrontend
  {
    -- | The array of backend node ids.
    pDOMPushNodesByBackendIdsToFrontendBackendNodeIds :: [DOMBackendNodeId]
  }
  deriving (Eq, Show)
pDOMPushNodesByBackendIdsToFrontend
  -- | The array of backend node ids.
  :: [DOMBackendNodeId]
  -> PDOMPushNodesByBackendIdsToFrontend
pDOMPushNodesByBackendIdsToFrontend
  arg_pDOMPushNodesByBackendIdsToFrontendBackendNodeIds
  = PDOMPushNodesByBackendIdsToFrontend
    arg_pDOMPushNodesByBackendIdsToFrontendBackendNodeIds
instance ToJSON PDOMPushNodesByBackendIdsToFrontend where
  toJSON p = A.object $ catMaybes [
    ("backendNodeIds" A..=) <$> Just (pDOMPushNodesByBackendIdsToFrontendBackendNodeIds p)
    ]
data DOMPushNodesByBackendIdsToFrontend = DOMPushNodesByBackendIdsToFrontend
  {
    -- | The array of ids of pushed nodes that correspond to the backend ids specified in
    --   backendNodeIds.
    dOMPushNodesByBackendIdsToFrontendNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMPushNodesByBackendIdsToFrontend where
  parseJSON = A.withObject "DOMPushNodesByBackendIdsToFrontend" $ \o -> DOMPushNodesByBackendIdsToFrontend
    <$> o A..: "nodeIds"
instance Command PDOMPushNodesByBackendIdsToFrontend where
  type CommandResponse PDOMPushNodesByBackendIdsToFrontend = DOMPushNodesByBackendIdsToFrontend
  commandName _ = "DOM.pushNodesByBackendIdsToFrontend"

-- | Executes `querySelector` on a given node.

-- | Parameters of the 'DOM.querySelector' command.
data PDOMQuerySelector = PDOMQuerySelector
  {
    -- | Id of the node to query upon.
    pDOMQuerySelectorNodeId :: DOMNodeId,
    -- | Selector string.
    pDOMQuerySelectorSelector :: T.Text
  }
  deriving (Eq, Show)
pDOMQuerySelector
  -- | Id of the node to query upon.
  :: DOMNodeId
  -- | Selector string.
  -> T.Text
  -> PDOMQuerySelector
pDOMQuerySelector
  arg_pDOMQuerySelectorNodeId
  arg_pDOMQuerySelectorSelector
  = PDOMQuerySelector
    arg_pDOMQuerySelectorNodeId
    arg_pDOMQuerySelectorSelector
instance ToJSON PDOMQuerySelector where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMQuerySelectorNodeId p),
    ("selector" A..=) <$> Just (pDOMQuerySelectorSelector p)
    ]
data DOMQuerySelector = DOMQuerySelector
  {
    -- | Query selector result.
    dOMQuerySelectorNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMQuerySelector where
  parseJSON = A.withObject "DOMQuerySelector" $ \o -> DOMQuerySelector
    <$> o A..: "nodeId"
instance Command PDOMQuerySelector where
  type CommandResponse PDOMQuerySelector = DOMQuerySelector
  commandName _ = "DOM.querySelector"

-- | Executes `querySelectorAll` on a given node.

-- | Parameters of the 'DOM.querySelectorAll' command.
data PDOMQuerySelectorAll = PDOMQuerySelectorAll
  {
    -- | Id of the node to query upon.
    pDOMQuerySelectorAllNodeId :: DOMNodeId,
    -- | Selector string.
    pDOMQuerySelectorAllSelector :: T.Text
  }
  deriving (Eq, Show)
pDOMQuerySelectorAll
  -- | Id of the node to query upon.
  :: DOMNodeId
  -- | Selector string.
  -> T.Text
  -> PDOMQuerySelectorAll
pDOMQuerySelectorAll
  arg_pDOMQuerySelectorAllNodeId
  arg_pDOMQuerySelectorAllSelector
  = PDOMQuerySelectorAll
    arg_pDOMQuerySelectorAllNodeId
    arg_pDOMQuerySelectorAllSelector
instance ToJSON PDOMQuerySelectorAll where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMQuerySelectorAllNodeId p),
    ("selector" A..=) <$> Just (pDOMQuerySelectorAllSelector p)
    ]
data DOMQuerySelectorAll = DOMQuerySelectorAll
  {
    -- | Query selector result.
    dOMQuerySelectorAllNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMQuerySelectorAll where
  parseJSON = A.withObject "DOMQuerySelectorAll" $ \o -> DOMQuerySelectorAll
    <$> o A..: "nodeIds"
instance Command PDOMQuerySelectorAll where
  type CommandResponse PDOMQuerySelectorAll = DOMQuerySelectorAll
  commandName _ = "DOM.querySelectorAll"

-- | Returns NodeIds of current top layer elements.
--   Top layer is rendered closest to the user within a viewport, therefore its elements always
--   appear on top of all other content.

-- | Parameters of the 'DOM.getTopLayerElements' command.
data PDOMGetTopLayerElements = PDOMGetTopLayerElements
  deriving (Eq, Show)
pDOMGetTopLayerElements
  :: PDOMGetTopLayerElements
pDOMGetTopLayerElements
  = PDOMGetTopLayerElements
instance ToJSON PDOMGetTopLayerElements where
  toJSON _ = A.Null
data DOMGetTopLayerElements = DOMGetTopLayerElements
  {
    -- | NodeIds of top layer elements
    dOMGetTopLayerElementsNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetTopLayerElements where
  parseJSON = A.withObject "DOMGetTopLayerElements" $ \o -> DOMGetTopLayerElements
    <$> o A..: "nodeIds"
instance Command PDOMGetTopLayerElements where
  type CommandResponse PDOMGetTopLayerElements = DOMGetTopLayerElements
  commandName _ = "DOM.getTopLayerElements"

-- | Re-does the last undone action.

-- | Parameters of the 'DOM.redo' command.
data PDOMRedo = PDOMRedo
  deriving (Eq, Show)
pDOMRedo
  :: PDOMRedo
pDOMRedo
  = PDOMRedo
instance ToJSON PDOMRedo where
  toJSON _ = A.Null
instance Command PDOMRedo where
  type CommandResponse PDOMRedo = ()
  commandName _ = "DOM.redo"
  fromJSON = const . A.Success . const ()

-- | Removes attribute with given name from an element with given id.

-- | Parameters of the 'DOM.removeAttribute' command.
data PDOMRemoveAttribute = PDOMRemoveAttribute
  {
    -- | Id of the element to remove attribute from.
    pDOMRemoveAttributeNodeId :: DOMNodeId,
    -- | Name of the attribute to remove.
    pDOMRemoveAttributeName :: T.Text
  }
  deriving (Eq, Show)
pDOMRemoveAttribute
  -- | Id of the element to remove attribute from.
  :: DOMNodeId
  -- | Name of the attribute to remove.
  -> T.Text
  -> PDOMRemoveAttribute
pDOMRemoveAttribute
  arg_pDOMRemoveAttributeNodeId
  arg_pDOMRemoveAttributeName
  = PDOMRemoveAttribute
    arg_pDOMRemoveAttributeNodeId
    arg_pDOMRemoveAttributeName
instance ToJSON PDOMRemoveAttribute where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMRemoveAttributeNodeId p),
    ("name" A..=) <$> Just (pDOMRemoveAttributeName p)
    ]
instance Command PDOMRemoveAttribute where
  type CommandResponse PDOMRemoveAttribute = ()
  commandName _ = "DOM.removeAttribute"
  fromJSON = const . A.Success . const ()

-- | Removes node with given id.

-- | Parameters of the 'DOM.removeNode' command.
data PDOMRemoveNode = PDOMRemoveNode
  {
    -- | Id of the node to remove.
    pDOMRemoveNodeNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMRemoveNode
  -- | Id of the node to remove.
  :: DOMNodeId
  -> PDOMRemoveNode
pDOMRemoveNode
  arg_pDOMRemoveNodeNodeId
  = PDOMRemoveNode
    arg_pDOMRemoveNodeNodeId
instance ToJSON PDOMRemoveNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMRemoveNodeNodeId p)
    ]
instance Command PDOMRemoveNode where
  type CommandResponse PDOMRemoveNode = ()
  commandName _ = "DOM.removeNode"
  fromJSON = const . A.Success . const ()

-- | Requests that children of the node with given id are returned to the caller in form of
--   `setChildNodes` events where not only immediate children are retrieved, but all children down to
--   the specified depth.

-- | Parameters of the 'DOM.requestChildNodes' command.
data PDOMRequestChildNodes = PDOMRequestChildNodes
  {
    -- | Id of the node to get children for.
    pDOMRequestChildNodesNodeId :: DOMNodeId,
    -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
    --   entire subtree or provide an integer larger than 0.
    pDOMRequestChildNodesDepth :: Maybe Int,
    -- | Whether or not iframes and shadow roots should be traversed when returning the sub-tree
    --   (default is false).
    pDOMRequestChildNodesPierce :: Maybe Bool
  }
  deriving (Eq, Show)
pDOMRequestChildNodes
  -- | Id of the node to get children for.
  :: DOMNodeId
  -> PDOMRequestChildNodes
pDOMRequestChildNodes
  arg_pDOMRequestChildNodesNodeId
  = PDOMRequestChildNodes
    arg_pDOMRequestChildNodesNodeId
    Nothing
    Nothing
instance ToJSON PDOMRequestChildNodes where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMRequestChildNodesNodeId p),
    ("depth" A..=) <$> (pDOMRequestChildNodesDepth p),
    ("pierce" A..=) <$> (pDOMRequestChildNodesPierce p)
    ]
instance Command PDOMRequestChildNodes where
  type CommandResponse PDOMRequestChildNodes = ()
  commandName _ = "DOM.requestChildNodes"
  fromJSON = const . A.Success . const ()

-- | Requests that the node is sent to the caller given the JavaScript node object reference. All
--   nodes that form the path from the node to the root are also sent to the client as a series of
--   `setChildNodes` notifications.

-- | Parameters of the 'DOM.requestNode' command.
data PDOMRequestNode = PDOMRequestNode
  {
    -- | JavaScript object id to convert into node.
    pDOMRequestNodeObjectId :: Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMRequestNode
  -- | JavaScript object id to convert into node.
  :: Runtime.RuntimeRemoteObjectId
  -> PDOMRequestNode
pDOMRequestNode
  arg_pDOMRequestNodeObjectId
  = PDOMRequestNode
    arg_pDOMRequestNodeObjectId
instance ToJSON PDOMRequestNode where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pDOMRequestNodeObjectId p)
    ]
data DOMRequestNode = DOMRequestNode
  {
    -- | Node id for given object.
    dOMRequestNodeNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMRequestNode where
  parseJSON = A.withObject "DOMRequestNode" $ \o -> DOMRequestNode
    <$> o A..: "nodeId"
instance Command PDOMRequestNode where
  type CommandResponse PDOMRequestNode = DOMRequestNode
  commandName _ = "DOM.requestNode"

-- | Resolves the JavaScript node object for a given NodeId or BackendNodeId.

-- | Parameters of the 'DOM.resolveNode' command.
data PDOMResolveNode = PDOMResolveNode
  {
    -- | Id of the node to resolve.
    pDOMResolveNodeNodeId :: Maybe DOMNodeId,
    -- | Backend identifier of the node to resolve.
    pDOMResolveNodeBackendNodeId :: Maybe DOMBackendNodeId,
    -- | Symbolic group name that can be used to release multiple objects.
    pDOMResolveNodeObjectGroup :: Maybe T.Text,
    -- | Execution context in which to resolve the node.
    pDOMResolveNodeExecutionContextId :: Maybe Runtime.RuntimeExecutionContextId
  }
  deriving (Eq, Show)
pDOMResolveNode
  :: PDOMResolveNode
pDOMResolveNode
  = PDOMResolveNode
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMResolveNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pDOMResolveNodeNodeId p),
    ("backendNodeId" A..=) <$> (pDOMResolveNodeBackendNodeId p),
    ("objectGroup" A..=) <$> (pDOMResolveNodeObjectGroup p),
    ("executionContextId" A..=) <$> (pDOMResolveNodeExecutionContextId p)
    ]
data DOMResolveNode = DOMResolveNode
  {
    -- | JavaScript object wrapper for given node.
    dOMResolveNodeObject :: Runtime.RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON DOMResolveNode where
  parseJSON = A.withObject "DOMResolveNode" $ \o -> DOMResolveNode
    <$> o A..: "object"
instance Command PDOMResolveNode where
  type CommandResponse PDOMResolveNode = DOMResolveNode
  commandName _ = "DOM.resolveNode"

-- | Sets attribute for an element with given id.

-- | Parameters of the 'DOM.setAttributeValue' command.
data PDOMSetAttributeValue = PDOMSetAttributeValue
  {
    -- | Id of the element to set attribute for.
    pDOMSetAttributeValueNodeId :: DOMNodeId,
    -- | Attribute name.
    pDOMSetAttributeValueName :: T.Text,
    -- | Attribute value.
    pDOMSetAttributeValueValue :: T.Text
  }
  deriving (Eq, Show)
pDOMSetAttributeValue
  -- | Id of the element to set attribute for.
  :: DOMNodeId
  -- | Attribute name.
  -> T.Text
  -- | Attribute value.
  -> T.Text
  -> PDOMSetAttributeValue
pDOMSetAttributeValue
  arg_pDOMSetAttributeValueNodeId
  arg_pDOMSetAttributeValueName
  arg_pDOMSetAttributeValueValue
  = PDOMSetAttributeValue
    arg_pDOMSetAttributeValueNodeId
    arg_pDOMSetAttributeValueName
    arg_pDOMSetAttributeValueValue
instance ToJSON PDOMSetAttributeValue where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetAttributeValueNodeId p),
    ("name" A..=) <$> Just (pDOMSetAttributeValueName p),
    ("value" A..=) <$> Just (pDOMSetAttributeValueValue p)
    ]
instance Command PDOMSetAttributeValue where
  type CommandResponse PDOMSetAttributeValue = ()
  commandName _ = "DOM.setAttributeValue"
  fromJSON = const . A.Success . const ()

-- | Sets attributes on element with given id. This method is useful when user edits some existing
--   attribute value and types in several attribute name/value pairs.

-- | Parameters of the 'DOM.setAttributesAsText' command.
data PDOMSetAttributesAsText = PDOMSetAttributesAsText
  {
    -- | Id of the element to set attributes for.
    pDOMSetAttributesAsTextNodeId :: DOMNodeId,
    -- | Text with a number of attributes. Will parse this text using HTML parser.
    pDOMSetAttributesAsTextText :: T.Text,
    -- | Attribute name to replace with new attributes derived from text in case text parsed
    --   successfully.
    pDOMSetAttributesAsTextName :: Maybe T.Text
  }
  deriving (Eq, Show)
pDOMSetAttributesAsText
  -- | Id of the element to set attributes for.
  :: DOMNodeId
  -- | Text with a number of attributes. Will parse this text using HTML parser.
  -> T.Text
  -> PDOMSetAttributesAsText
pDOMSetAttributesAsText
  arg_pDOMSetAttributesAsTextNodeId
  arg_pDOMSetAttributesAsTextText
  = PDOMSetAttributesAsText
    arg_pDOMSetAttributesAsTextNodeId
    arg_pDOMSetAttributesAsTextText
    Nothing
instance ToJSON PDOMSetAttributesAsText where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetAttributesAsTextNodeId p),
    ("text" A..=) <$> Just (pDOMSetAttributesAsTextText p),
    ("name" A..=) <$> (pDOMSetAttributesAsTextName p)
    ]
instance Command PDOMSetAttributesAsText where
  type CommandResponse PDOMSetAttributesAsText = ()
  commandName _ = "DOM.setAttributesAsText"
  fromJSON = const . A.Success . const ()

-- | Sets files for the given file input element.

-- | Parameters of the 'DOM.setFileInputFiles' command.
data PDOMSetFileInputFiles = PDOMSetFileInputFiles
  {
    -- | Array of file paths to set.
    pDOMSetFileInputFilesFiles :: [T.Text],
    -- | Identifier of the node.
    pDOMSetFileInputFilesNodeId :: Maybe DOMNodeId,
    -- | Identifier of the backend node.
    pDOMSetFileInputFilesBackendNodeId :: Maybe DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper.
    pDOMSetFileInputFilesObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMSetFileInputFiles
  -- | Array of file paths to set.
  :: [T.Text]
  -> PDOMSetFileInputFiles
pDOMSetFileInputFiles
  arg_pDOMSetFileInputFilesFiles
  = PDOMSetFileInputFiles
    arg_pDOMSetFileInputFilesFiles
    Nothing
    Nothing
    Nothing
instance ToJSON PDOMSetFileInputFiles where
  toJSON p = A.object $ catMaybes [
    ("files" A..=) <$> Just (pDOMSetFileInputFilesFiles p),
    ("nodeId" A..=) <$> (pDOMSetFileInputFilesNodeId p),
    ("backendNodeId" A..=) <$> (pDOMSetFileInputFilesBackendNodeId p),
    ("objectId" A..=) <$> (pDOMSetFileInputFilesObjectId p)
    ]
instance Command PDOMSetFileInputFiles where
  type CommandResponse PDOMSetFileInputFiles = ()
  commandName _ = "DOM.setFileInputFiles"
  fromJSON = const . A.Success . const ()

-- | Sets if stack traces should be captured for Nodes. See `Node.getNodeStackTraces`. Default is disabled.

-- | Parameters of the 'DOM.setNodeStackTracesEnabled' command.
data PDOMSetNodeStackTracesEnabled = PDOMSetNodeStackTracesEnabled
  {
    -- | Enable or disable.
    pDOMSetNodeStackTracesEnabledEnable :: Bool
  }
  deriving (Eq, Show)
pDOMSetNodeStackTracesEnabled
  -- | Enable or disable.
  :: Bool
  -> PDOMSetNodeStackTracesEnabled
pDOMSetNodeStackTracesEnabled
  arg_pDOMSetNodeStackTracesEnabledEnable
  = PDOMSetNodeStackTracesEnabled
    arg_pDOMSetNodeStackTracesEnabledEnable
instance ToJSON PDOMSetNodeStackTracesEnabled where
  toJSON p = A.object $ catMaybes [
    ("enable" A..=) <$> Just (pDOMSetNodeStackTracesEnabledEnable p)
    ]
instance Command PDOMSetNodeStackTracesEnabled where
  type CommandResponse PDOMSetNodeStackTracesEnabled = ()
  commandName _ = "DOM.setNodeStackTracesEnabled"
  fromJSON = const . A.Success . const ()

-- | Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.

-- | Parameters of the 'DOM.getNodeStackTraces' command.
data PDOMGetNodeStackTraces = PDOMGetNodeStackTraces
  {
    -- | Id of the node to get stack traces for.
    pDOMGetNodeStackTracesNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMGetNodeStackTraces
  -- | Id of the node to get stack traces for.
  :: DOMNodeId
  -> PDOMGetNodeStackTraces
pDOMGetNodeStackTraces
  arg_pDOMGetNodeStackTracesNodeId
  = PDOMGetNodeStackTraces
    arg_pDOMGetNodeStackTracesNodeId
instance ToJSON PDOMGetNodeStackTraces where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetNodeStackTracesNodeId p)
    ]
data DOMGetNodeStackTraces = DOMGetNodeStackTraces
  {
    -- | Creation stack trace, if available.
    dOMGetNodeStackTracesCreation :: Maybe Runtime.RuntimeStackTrace
  }
  deriving (Eq, Show)
instance FromJSON DOMGetNodeStackTraces where
  parseJSON = A.withObject "DOMGetNodeStackTraces" $ \o -> DOMGetNodeStackTraces
    <$> o A..:? "creation"
instance Command PDOMGetNodeStackTraces where
  type CommandResponse PDOMGetNodeStackTraces = DOMGetNodeStackTraces
  commandName _ = "DOM.getNodeStackTraces"

-- | Returns file information for the given
--   File wrapper.

-- | Parameters of the 'DOM.getFileInfo' command.
data PDOMGetFileInfo = PDOMGetFileInfo
  {
    -- | JavaScript object id of the node wrapper.
    pDOMGetFileInfoObjectId :: Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pDOMGetFileInfo
  -- | JavaScript object id of the node wrapper.
  :: Runtime.RuntimeRemoteObjectId
  -> PDOMGetFileInfo
pDOMGetFileInfo
  arg_pDOMGetFileInfoObjectId
  = PDOMGetFileInfo
    arg_pDOMGetFileInfoObjectId
instance ToJSON PDOMGetFileInfo where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pDOMGetFileInfoObjectId p)
    ]
data DOMGetFileInfo = DOMGetFileInfo
  {
    dOMGetFileInfoPath :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON DOMGetFileInfo where
  parseJSON = A.withObject "DOMGetFileInfo" $ \o -> DOMGetFileInfo
    <$> o A..: "path"
instance Command PDOMGetFileInfo where
  type CommandResponse PDOMGetFileInfo = DOMGetFileInfo
  commandName _ = "DOM.getFileInfo"

-- | Enables console to refer to the node with given id via $x (see Command Line API for more details
--   $x functions).

-- | Parameters of the 'DOM.setInspectedNode' command.
data PDOMSetInspectedNode = PDOMSetInspectedNode
  {
    -- | DOM node id to be accessible by means of $x command line API.
    pDOMSetInspectedNodeNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMSetInspectedNode
  -- | DOM node id to be accessible by means of $x command line API.
  :: DOMNodeId
  -> PDOMSetInspectedNode
pDOMSetInspectedNode
  arg_pDOMSetInspectedNodeNodeId
  = PDOMSetInspectedNode
    arg_pDOMSetInspectedNodeNodeId
instance ToJSON PDOMSetInspectedNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetInspectedNodeNodeId p)
    ]
instance Command PDOMSetInspectedNode where
  type CommandResponse PDOMSetInspectedNode = ()
  commandName _ = "DOM.setInspectedNode"
  fromJSON = const . A.Success . const ()

-- | Sets node name for a node with given id.

-- | Parameters of the 'DOM.setNodeName' command.
data PDOMSetNodeName = PDOMSetNodeName
  {
    -- | Id of the node to set name for.
    pDOMSetNodeNameNodeId :: DOMNodeId,
    -- | New node's name.
    pDOMSetNodeNameName :: T.Text
  }
  deriving (Eq, Show)
pDOMSetNodeName
  -- | Id of the node to set name for.
  :: DOMNodeId
  -- | New node's name.
  -> T.Text
  -> PDOMSetNodeName
pDOMSetNodeName
  arg_pDOMSetNodeNameNodeId
  arg_pDOMSetNodeNameName
  = PDOMSetNodeName
    arg_pDOMSetNodeNameNodeId
    arg_pDOMSetNodeNameName
instance ToJSON PDOMSetNodeName where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetNodeNameNodeId p),
    ("name" A..=) <$> Just (pDOMSetNodeNameName p)
    ]
data DOMSetNodeName = DOMSetNodeName
  {
    -- | New node's id.
    dOMSetNodeNameNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMSetNodeName where
  parseJSON = A.withObject "DOMSetNodeName" $ \o -> DOMSetNodeName
    <$> o A..: "nodeId"
instance Command PDOMSetNodeName where
  type CommandResponse PDOMSetNodeName = DOMSetNodeName
  commandName _ = "DOM.setNodeName"

-- | Sets node value for a node with given id.

-- | Parameters of the 'DOM.setNodeValue' command.
data PDOMSetNodeValue = PDOMSetNodeValue
  {
    -- | Id of the node to set value for.
    pDOMSetNodeValueNodeId :: DOMNodeId,
    -- | New node's value.
    pDOMSetNodeValueValue :: T.Text
  }
  deriving (Eq, Show)
pDOMSetNodeValue
  -- | Id of the node to set value for.
  :: DOMNodeId
  -- | New node's value.
  -> T.Text
  -> PDOMSetNodeValue
pDOMSetNodeValue
  arg_pDOMSetNodeValueNodeId
  arg_pDOMSetNodeValueValue
  = PDOMSetNodeValue
    arg_pDOMSetNodeValueNodeId
    arg_pDOMSetNodeValueValue
instance ToJSON PDOMSetNodeValue where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetNodeValueNodeId p),
    ("value" A..=) <$> Just (pDOMSetNodeValueValue p)
    ]
instance Command PDOMSetNodeValue where
  type CommandResponse PDOMSetNodeValue = ()
  commandName _ = "DOM.setNodeValue"
  fromJSON = const . A.Success . const ()

-- | Sets node HTML markup, returns new node id.

-- | Parameters of the 'DOM.setOuterHTML' command.
data PDOMSetOuterHTML = PDOMSetOuterHTML
  {
    -- | Id of the node to set markup for.
    pDOMSetOuterHTMLNodeId :: DOMNodeId,
    -- | Outer HTML markup to set.
    pDOMSetOuterHTMLOuterHTML :: T.Text
  }
  deriving (Eq, Show)
pDOMSetOuterHTML
  -- | Id of the node to set markup for.
  :: DOMNodeId
  -- | Outer HTML markup to set.
  -> T.Text
  -> PDOMSetOuterHTML
pDOMSetOuterHTML
  arg_pDOMSetOuterHTMLNodeId
  arg_pDOMSetOuterHTMLOuterHTML
  = PDOMSetOuterHTML
    arg_pDOMSetOuterHTMLNodeId
    arg_pDOMSetOuterHTMLOuterHTML
instance ToJSON PDOMSetOuterHTML where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMSetOuterHTMLNodeId p),
    ("outerHTML" A..=) <$> Just (pDOMSetOuterHTMLOuterHTML p)
    ]
instance Command PDOMSetOuterHTML where
  type CommandResponse PDOMSetOuterHTML = ()
  commandName _ = "DOM.setOuterHTML"
  fromJSON = const . A.Success . const ()

-- | Undoes the last performed action.

-- | Parameters of the 'DOM.undo' command.
data PDOMUndo = PDOMUndo
  deriving (Eq, Show)
pDOMUndo
  :: PDOMUndo
pDOMUndo
  = PDOMUndo
instance ToJSON PDOMUndo where
  toJSON _ = A.Null
instance Command PDOMUndo where
  type CommandResponse PDOMUndo = ()
  commandName _ = "DOM.undo"
  fromJSON = const . A.Success . const ()

-- | Returns iframe node that owns iframe with the given domain.

-- | Parameters of the 'DOM.getFrameOwner' command.
data PDOMGetFrameOwner = PDOMGetFrameOwner
  {
    pDOMGetFrameOwnerFrameId :: PageFrameId
  }
  deriving (Eq, Show)
pDOMGetFrameOwner
  :: PageFrameId
  -> PDOMGetFrameOwner
pDOMGetFrameOwner
  arg_pDOMGetFrameOwnerFrameId
  = PDOMGetFrameOwner
    arg_pDOMGetFrameOwnerFrameId
instance ToJSON PDOMGetFrameOwner where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pDOMGetFrameOwnerFrameId p)
    ]
data DOMGetFrameOwner = DOMGetFrameOwner
  {
    -- | Resulting node.
    dOMGetFrameOwnerBackendNodeId :: DOMBackendNodeId,
    -- | Id of the node at given coordinates, only when enabled and requested document.
    dOMGetFrameOwnerNodeId :: Maybe DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMGetFrameOwner where
  parseJSON = A.withObject "DOMGetFrameOwner" $ \o -> DOMGetFrameOwner
    <$> o A..: "backendNodeId"
    <*> o A..:? "nodeId"
instance Command PDOMGetFrameOwner where
  type CommandResponse PDOMGetFrameOwner = DOMGetFrameOwner
  commandName _ = "DOM.getFrameOwner"

-- | Returns the container of the given node based on container query conditions.
--   If containerName is given, it will find the nearest container with a matching name;
--   otherwise it will find the nearest container regardless of its container name.

-- | Parameters of the 'DOM.getContainerForNode' command.
data PDOMGetContainerForNode = PDOMGetContainerForNode
  {
    pDOMGetContainerForNodeNodeId :: DOMNodeId,
    pDOMGetContainerForNodeContainerName :: Maybe T.Text
  }
  deriving (Eq, Show)
pDOMGetContainerForNode
  :: DOMNodeId
  -> PDOMGetContainerForNode
pDOMGetContainerForNode
  arg_pDOMGetContainerForNodeNodeId
  = PDOMGetContainerForNode
    arg_pDOMGetContainerForNodeNodeId
    Nothing
instance ToJSON PDOMGetContainerForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetContainerForNodeNodeId p),
    ("containerName" A..=) <$> (pDOMGetContainerForNodeContainerName p)
    ]
data DOMGetContainerForNode = DOMGetContainerForNode
  {
    -- | The container node for the given node, or null if not found.
    dOMGetContainerForNodeNodeId :: Maybe DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON DOMGetContainerForNode where
  parseJSON = A.withObject "DOMGetContainerForNode" $ \o -> DOMGetContainerForNode
    <$> o A..:? "nodeId"
instance Command PDOMGetContainerForNode where
  type CommandResponse PDOMGetContainerForNode = DOMGetContainerForNode
  commandName _ = "DOM.getContainerForNode"

-- | Returns the descendants of a container query container that have
--   container queries against this container.

-- | Parameters of the 'DOM.getQueryingDescendantsForContainer' command.
data PDOMGetQueryingDescendantsForContainer = PDOMGetQueryingDescendantsForContainer
  {
    -- | Id of the container node to find querying descendants from.
    pDOMGetQueryingDescendantsForContainerNodeId :: DOMNodeId
  }
  deriving (Eq, Show)
pDOMGetQueryingDescendantsForContainer
  -- | Id of the container node to find querying descendants from.
  :: DOMNodeId
  -> PDOMGetQueryingDescendantsForContainer
pDOMGetQueryingDescendantsForContainer
  arg_pDOMGetQueryingDescendantsForContainerNodeId
  = PDOMGetQueryingDescendantsForContainer
    arg_pDOMGetQueryingDescendantsForContainerNodeId
instance ToJSON PDOMGetQueryingDescendantsForContainer where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pDOMGetQueryingDescendantsForContainerNodeId p)
    ]
data DOMGetQueryingDescendantsForContainer = DOMGetQueryingDescendantsForContainer
  {
    -- | Descendant nodes with container queries against the given container.
    dOMGetQueryingDescendantsForContainerNodeIds :: [DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON DOMGetQueryingDescendantsForContainer where
  parseJSON = A.withObject "DOMGetQueryingDescendantsForContainer" $ \o -> DOMGetQueryingDescendantsForContainer
    <$> o A..: "nodeIds"
instance Command PDOMGetQueryingDescendantsForContainer where
  type CommandResponse PDOMGetQueryingDescendantsForContainer = DOMGetQueryingDescendantsForContainer
  commandName _ = "DOM.getQueryingDescendantsForContainer"

-- | Type 'Emulation.ScreenOrientation'.
--   Screen orientation.
data EmulationScreenOrientationType = EmulationScreenOrientationTypePortraitPrimary | EmulationScreenOrientationTypePortraitSecondary | EmulationScreenOrientationTypeLandscapePrimary | EmulationScreenOrientationTypeLandscapeSecondary
  deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationScreenOrientationType where
  parseJSON = A.withText "EmulationScreenOrientationType" $ \v -> case v of
    "portraitPrimary" -> pure EmulationScreenOrientationTypePortraitPrimary
    "portraitSecondary" -> pure EmulationScreenOrientationTypePortraitSecondary
    "landscapePrimary" -> pure EmulationScreenOrientationTypeLandscapePrimary
    "landscapeSecondary" -> pure EmulationScreenOrientationTypeLandscapeSecondary
    "_" -> fail "failed to parse EmulationScreenOrientationType"
instance ToJSON EmulationScreenOrientationType where
  toJSON v = A.String $ case v of
    EmulationScreenOrientationTypePortraitPrimary -> "portraitPrimary"
    EmulationScreenOrientationTypePortraitSecondary -> "portraitSecondary"
    EmulationScreenOrientationTypeLandscapePrimary -> "landscapePrimary"
    EmulationScreenOrientationTypeLandscapeSecondary -> "landscapeSecondary"
data EmulationScreenOrientation = EmulationScreenOrientation
  {
    -- | Orientation type.
    emulationScreenOrientationType :: EmulationScreenOrientationType,
    -- | Orientation angle.
    emulationScreenOrientationAngle :: Int
  }
  deriving (Eq, Show)
instance FromJSON EmulationScreenOrientation where
  parseJSON = A.withObject "EmulationScreenOrientation" $ \o -> EmulationScreenOrientation
    <$> o A..: "type"
    <*> o A..: "angle"
instance ToJSON EmulationScreenOrientation where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (emulationScreenOrientationType p),
    ("angle" A..=) <$> Just (emulationScreenOrientationAngle p)
    ]

-- | Type 'Emulation.DisplayFeature'.
data EmulationDisplayFeatureOrientation = EmulationDisplayFeatureOrientationVertical | EmulationDisplayFeatureOrientationHorizontal
  deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisplayFeatureOrientation where
  parseJSON = A.withText "EmulationDisplayFeatureOrientation" $ \v -> case v of
    "vertical" -> pure EmulationDisplayFeatureOrientationVertical
    "horizontal" -> pure EmulationDisplayFeatureOrientationHorizontal
    "_" -> fail "failed to parse EmulationDisplayFeatureOrientation"
instance ToJSON EmulationDisplayFeatureOrientation where
  toJSON v = A.String $ case v of
    EmulationDisplayFeatureOrientationVertical -> "vertical"
    EmulationDisplayFeatureOrientationHorizontal -> "horizontal"
data EmulationDisplayFeature = EmulationDisplayFeature
  {
    -- | Orientation of a display feature in relation to screen
    emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation,
    -- | The offset from the screen origin in either the x (for vertical
    --   orientation) or y (for horizontal orientation) direction.
    emulationDisplayFeatureOffset :: Int,
    -- | A display feature may mask content such that it is not physically
    --   displayed - this length along with the offset describes this area.
    --   A display feature that only splits content will have a 0 mask_length.
    emulationDisplayFeatureMaskLength :: Int
  }
  deriving (Eq, Show)
instance FromJSON EmulationDisplayFeature where
  parseJSON = A.withObject "EmulationDisplayFeature" $ \o -> EmulationDisplayFeature
    <$> o A..: "orientation"
    <*> o A..: "offset"
    <*> o A..: "maskLength"
instance ToJSON EmulationDisplayFeature where
  toJSON p = A.object $ catMaybes [
    ("orientation" A..=) <$> Just (emulationDisplayFeatureOrientation p),
    ("offset" A..=) <$> Just (emulationDisplayFeatureOffset p),
    ("maskLength" A..=) <$> Just (emulationDisplayFeatureMaskLength p)
    ]

-- | Type 'Emulation.MediaFeature'.
data EmulationMediaFeature = EmulationMediaFeature
  {
    emulationMediaFeatureName :: T.Text,
    emulationMediaFeatureValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON EmulationMediaFeature where
  parseJSON = A.withObject "EmulationMediaFeature" $ \o -> EmulationMediaFeature
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON EmulationMediaFeature where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (emulationMediaFeatureName p),
    ("value" A..=) <$> Just (emulationMediaFeatureValue p)
    ]

-- | Type 'Emulation.VirtualTimePolicy'.
--   advance: If the scheduler runs out of immediate work, the virtual time base may fast forward to
--   allow the next delayed task (if any) to run; pause: The virtual time base may not advance;
--   pauseIfNetworkFetchesPending: The virtual time base may not advance if there are any pending
--   resource fetches.
data EmulationVirtualTimePolicy = EmulationVirtualTimePolicyAdvance | EmulationVirtualTimePolicyPause | EmulationVirtualTimePolicyPauseIfNetworkFetchesPending
  deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationVirtualTimePolicy where
  parseJSON = A.withText "EmulationVirtualTimePolicy" $ \v -> case v of
    "advance" -> pure EmulationVirtualTimePolicyAdvance
    "pause" -> pure EmulationVirtualTimePolicyPause
    "pauseIfNetworkFetchesPending" -> pure EmulationVirtualTimePolicyPauseIfNetworkFetchesPending
    "_" -> fail "failed to parse EmulationVirtualTimePolicy"
instance ToJSON EmulationVirtualTimePolicy where
  toJSON v = A.String $ case v of
    EmulationVirtualTimePolicyAdvance -> "advance"
    EmulationVirtualTimePolicyPause -> "pause"
    EmulationVirtualTimePolicyPauseIfNetworkFetchesPending -> "pauseIfNetworkFetchesPending"

-- | Type 'Emulation.UserAgentBrandVersion'.
--   Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
data EmulationUserAgentBrandVersion = EmulationUserAgentBrandVersion
  {
    emulationUserAgentBrandVersionBrand :: T.Text,
    emulationUserAgentBrandVersionVersion :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON EmulationUserAgentBrandVersion where
  parseJSON = A.withObject "EmulationUserAgentBrandVersion" $ \o -> EmulationUserAgentBrandVersion
    <$> o A..: "brand"
    <*> o A..: "version"
instance ToJSON EmulationUserAgentBrandVersion where
  toJSON p = A.object $ catMaybes [
    ("brand" A..=) <$> Just (emulationUserAgentBrandVersionBrand p),
    ("version" A..=) <$> Just (emulationUserAgentBrandVersionVersion p)
    ]

-- | Type 'Emulation.UserAgentMetadata'.
--   Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
--   Missing optional values will be filled in by the target with what it would normally use.
data EmulationUserAgentMetadata = EmulationUserAgentMetadata
  {
    emulationUserAgentMetadataBrands :: Maybe [EmulationUserAgentBrandVersion],
    emulationUserAgentMetadataFullVersionList :: Maybe [EmulationUserAgentBrandVersion],
    emulationUserAgentMetadataPlatform :: T.Text,
    emulationUserAgentMetadataPlatformVersion :: T.Text,
    emulationUserAgentMetadataArchitecture :: T.Text,
    emulationUserAgentMetadataModel :: T.Text,
    emulationUserAgentMetadataMobile :: Bool,
    emulationUserAgentMetadataBitness :: Maybe T.Text,
    emulationUserAgentMetadataWow64 :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON EmulationUserAgentMetadata where
  parseJSON = A.withObject "EmulationUserAgentMetadata" $ \o -> EmulationUserAgentMetadata
    <$> o A..:? "brands"
    <*> o A..:? "fullVersionList"
    <*> o A..: "platform"
    <*> o A..: "platformVersion"
    <*> o A..: "architecture"
    <*> o A..: "model"
    <*> o A..: "mobile"
    <*> o A..:? "bitness"
    <*> o A..:? "wow64"
instance ToJSON EmulationUserAgentMetadata where
  toJSON p = A.object $ catMaybes [
    ("brands" A..=) <$> (emulationUserAgentMetadataBrands p),
    ("fullVersionList" A..=) <$> (emulationUserAgentMetadataFullVersionList p),
    ("platform" A..=) <$> Just (emulationUserAgentMetadataPlatform p),
    ("platformVersion" A..=) <$> Just (emulationUserAgentMetadataPlatformVersion p),
    ("architecture" A..=) <$> Just (emulationUserAgentMetadataArchitecture p),
    ("model" A..=) <$> Just (emulationUserAgentMetadataModel p),
    ("mobile" A..=) <$> Just (emulationUserAgentMetadataMobile p),
    ("bitness" A..=) <$> (emulationUserAgentMetadataBitness p),
    ("wow64" A..=) <$> (emulationUserAgentMetadataWow64 p)
    ]

-- | Type 'Emulation.DisabledImageType'.
--   Enum of image types that can be disabled.
data EmulationDisabledImageType = EmulationDisabledImageTypeAvif | EmulationDisabledImageTypeJxl | EmulationDisabledImageTypeWebp
  deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisabledImageType where
  parseJSON = A.withText "EmulationDisabledImageType" $ \v -> case v of
    "avif" -> pure EmulationDisabledImageTypeAvif
    "jxl" -> pure EmulationDisabledImageTypeJxl
    "webp" -> pure EmulationDisabledImageTypeWebp
    "_" -> fail "failed to parse EmulationDisabledImageType"
instance ToJSON EmulationDisabledImageType where
  toJSON v = A.String $ case v of
    EmulationDisabledImageTypeAvif -> "avif"
    EmulationDisabledImageTypeJxl -> "jxl"
    EmulationDisabledImageTypeWebp -> "webp"

-- | Type of the 'Emulation.virtualTimeBudgetExpired' event.
data EmulationVirtualTimeBudgetExpired = EmulationVirtualTimeBudgetExpired
  deriving (Eq, Show, Read)
instance FromJSON EmulationVirtualTimeBudgetExpired where
  parseJSON _ = pure EmulationVirtualTimeBudgetExpired
instance Event EmulationVirtualTimeBudgetExpired where
  eventName _ = "Emulation.virtualTimeBudgetExpired"

-- | Tells whether emulation is supported.

-- | Parameters of the 'Emulation.canEmulate' command.
data PEmulationCanEmulate = PEmulationCanEmulate
  deriving (Eq, Show)
pEmulationCanEmulate
  :: PEmulationCanEmulate
pEmulationCanEmulate
  = PEmulationCanEmulate
instance ToJSON PEmulationCanEmulate where
  toJSON _ = A.Null
data EmulationCanEmulate = EmulationCanEmulate
  {
    -- | True if emulation is supported.
    emulationCanEmulateResult :: Bool
  }
  deriving (Eq, Show)
instance FromJSON EmulationCanEmulate where
  parseJSON = A.withObject "EmulationCanEmulate" $ \o -> EmulationCanEmulate
    <$> o A..: "result"
instance Command PEmulationCanEmulate where
  type CommandResponse PEmulationCanEmulate = EmulationCanEmulate
  commandName _ = "Emulation.canEmulate"

-- | Clears the overridden device metrics.

-- | Parameters of the 'Emulation.clearDeviceMetricsOverride' command.
data PEmulationClearDeviceMetricsOverride = PEmulationClearDeviceMetricsOverride
  deriving (Eq, Show)
pEmulationClearDeviceMetricsOverride
  :: PEmulationClearDeviceMetricsOverride
pEmulationClearDeviceMetricsOverride
  = PEmulationClearDeviceMetricsOverride
instance ToJSON PEmulationClearDeviceMetricsOverride where
  toJSON _ = A.Null
instance Command PEmulationClearDeviceMetricsOverride where
  type CommandResponse PEmulationClearDeviceMetricsOverride = ()
  commandName _ = "Emulation.clearDeviceMetricsOverride"
  fromJSON = const . A.Success . const ()

-- | Clears the overridden Geolocation Position and Error.

-- | Parameters of the 'Emulation.clearGeolocationOverride' command.
data PEmulationClearGeolocationOverride = PEmulationClearGeolocationOverride
  deriving (Eq, Show)
pEmulationClearGeolocationOverride
  :: PEmulationClearGeolocationOverride
pEmulationClearGeolocationOverride
  = PEmulationClearGeolocationOverride
instance ToJSON PEmulationClearGeolocationOverride where
  toJSON _ = A.Null
instance Command PEmulationClearGeolocationOverride where
  type CommandResponse PEmulationClearGeolocationOverride = ()
  commandName _ = "Emulation.clearGeolocationOverride"
  fromJSON = const . A.Success . const ()

-- | Requests that page scale factor is reset to initial values.

-- | Parameters of the 'Emulation.resetPageScaleFactor' command.
data PEmulationResetPageScaleFactor = PEmulationResetPageScaleFactor
  deriving (Eq, Show)
pEmulationResetPageScaleFactor
  :: PEmulationResetPageScaleFactor
pEmulationResetPageScaleFactor
  = PEmulationResetPageScaleFactor
instance ToJSON PEmulationResetPageScaleFactor where
  toJSON _ = A.Null
instance Command PEmulationResetPageScaleFactor where
  type CommandResponse PEmulationResetPageScaleFactor = ()
  commandName _ = "Emulation.resetPageScaleFactor"
  fromJSON = const . A.Success . const ()

-- | Enables or disables simulating a focused and active page.

-- | Parameters of the 'Emulation.setFocusEmulationEnabled' command.
data PEmulationSetFocusEmulationEnabled = PEmulationSetFocusEmulationEnabled
  {
    -- | Whether to enable to disable focus emulation.
    pEmulationSetFocusEmulationEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pEmulationSetFocusEmulationEnabled
  -- | Whether to enable to disable focus emulation.
  :: Bool
  -> PEmulationSetFocusEmulationEnabled
pEmulationSetFocusEmulationEnabled
  arg_pEmulationSetFocusEmulationEnabledEnabled
  = PEmulationSetFocusEmulationEnabled
    arg_pEmulationSetFocusEmulationEnabledEnabled
instance ToJSON PEmulationSetFocusEmulationEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pEmulationSetFocusEmulationEnabledEnabled p)
    ]
instance Command PEmulationSetFocusEmulationEnabled where
  type CommandResponse PEmulationSetFocusEmulationEnabled = ()
  commandName _ = "Emulation.setFocusEmulationEnabled"
  fromJSON = const . A.Success . const ()

-- | Automatically render all web contents using a dark theme.

-- | Parameters of the 'Emulation.setAutoDarkModeOverride' command.
data PEmulationSetAutoDarkModeOverride = PEmulationSetAutoDarkModeOverride
  {
    -- | Whether to enable or disable automatic dark mode.
    --   If not specified, any existing override will be cleared.
    pEmulationSetAutoDarkModeOverrideEnabled :: Maybe Bool
  }
  deriving (Eq, Show)
pEmulationSetAutoDarkModeOverride
  :: PEmulationSetAutoDarkModeOverride
pEmulationSetAutoDarkModeOverride
  = PEmulationSetAutoDarkModeOverride
    Nothing
instance ToJSON PEmulationSetAutoDarkModeOverride where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> (pEmulationSetAutoDarkModeOverrideEnabled p)
    ]
instance Command PEmulationSetAutoDarkModeOverride where
  type CommandResponse PEmulationSetAutoDarkModeOverride = ()
  commandName _ = "Emulation.setAutoDarkModeOverride"
  fromJSON = const . A.Success . const ()

-- | Enables CPU throttling to emulate slow CPUs.

-- | Parameters of the 'Emulation.setCPUThrottlingRate' command.
data PEmulationSetCPUThrottlingRate = PEmulationSetCPUThrottlingRate
  {
    -- | Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
    pEmulationSetCPUThrottlingRateRate :: Double
  }
  deriving (Eq, Show)
pEmulationSetCPUThrottlingRate
  -- | Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
  :: Double
  -> PEmulationSetCPUThrottlingRate
pEmulationSetCPUThrottlingRate
  arg_pEmulationSetCPUThrottlingRateRate
  = PEmulationSetCPUThrottlingRate
    arg_pEmulationSetCPUThrottlingRateRate
instance ToJSON PEmulationSetCPUThrottlingRate where
  toJSON p = A.object $ catMaybes [
    ("rate" A..=) <$> Just (pEmulationSetCPUThrottlingRateRate p)
    ]
instance Command PEmulationSetCPUThrottlingRate where
  type CommandResponse PEmulationSetCPUThrottlingRate = ()
  commandName _ = "Emulation.setCPUThrottlingRate"
  fromJSON = const . A.Success . const ()

-- | Sets or clears an override of the default background color of the frame. This override is used
--   if the content does not specify one.

-- | Parameters of the 'Emulation.setDefaultBackgroundColorOverride' command.
data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride
  {
    -- | RGBA of the default background color. If not specified, any existing override will be
    --   cleared.
    pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMRGBA
  }
  deriving (Eq, Show)
pEmulationSetDefaultBackgroundColorOverride
  :: PEmulationSetDefaultBackgroundColorOverride
pEmulationSetDefaultBackgroundColorOverride
  = PEmulationSetDefaultBackgroundColorOverride
    Nothing
instance ToJSON PEmulationSetDefaultBackgroundColorOverride where
  toJSON p = A.object $ catMaybes [
    ("color" A..=) <$> (pEmulationSetDefaultBackgroundColorOverrideColor p)
    ]
instance Command PEmulationSetDefaultBackgroundColorOverride where
  type CommandResponse PEmulationSetDefaultBackgroundColorOverride = ()
  commandName _ = "Emulation.setDefaultBackgroundColorOverride"
  fromJSON = const . A.Success . const ()

-- | Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
--   window.innerWidth, window.innerHeight, and "device-width"/"device-height"-related CSS media
--   query results).

-- | Parameters of the 'Emulation.setDeviceMetricsOverride' command.
data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride
  {
    -- | Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.
    pEmulationSetDeviceMetricsOverrideWidth :: Int,
    -- | Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.
    pEmulationSetDeviceMetricsOverrideHeight :: Int,
    -- | Overriding device scale factor value. 0 disables the override.
    pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
    -- | Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text
    --   autosizing and more.
    pEmulationSetDeviceMetricsOverrideMobile :: Bool,
    -- | Scale to apply to resulting view image.
    pEmulationSetDeviceMetricsOverrideScale :: Maybe Double,
    -- | Overriding screen width value in pixels (minimum 0, maximum 10000000).
    pEmulationSetDeviceMetricsOverrideScreenWidth :: Maybe Int,
    -- | Overriding screen height value in pixels (minimum 0, maximum 10000000).
    pEmulationSetDeviceMetricsOverrideScreenHeight :: Maybe Int,
    -- | Overriding view X position on screen in pixels (minimum 0, maximum 10000000).
    pEmulationSetDeviceMetricsOverridePositionX :: Maybe Int,
    -- | Overriding view Y position on screen in pixels (minimum 0, maximum 10000000).
    pEmulationSetDeviceMetricsOverridePositionY :: Maybe Int,
    -- | Do not set visible view size, rely upon explicit setVisibleSize call.
    pEmulationSetDeviceMetricsOverrideDontSetVisibleSize :: Maybe Bool,
    -- | Screen orientation override.
    pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation,
    -- | If set, the visible area of the page will be overridden to this viewport. This viewport
    --   change is not observed by the page, e.g. viewport-relative elements do not change positions.
    pEmulationSetDeviceMetricsOverrideViewport :: Maybe PageViewport,
    -- | If set, the display feature of a multi-segment screen. If not set, multi-segment support
    --   is turned-off.
    pEmulationSetDeviceMetricsOverrideDisplayFeature :: Maybe EmulationDisplayFeature
  }
  deriving (Eq, Show)
pEmulationSetDeviceMetricsOverride
  -- | Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.
  :: Int
  -- | Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.
  -> Int
  -- | Overriding device scale factor value. 0 disables the override.
  -> Double
  -- | Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text
  --   autosizing and more.
  -> Bool
  -> PEmulationSetDeviceMetricsOverride
pEmulationSetDeviceMetricsOverride
  arg_pEmulationSetDeviceMetricsOverrideWidth
  arg_pEmulationSetDeviceMetricsOverrideHeight
  arg_pEmulationSetDeviceMetricsOverrideDeviceScaleFactor
  arg_pEmulationSetDeviceMetricsOverrideMobile
  = PEmulationSetDeviceMetricsOverride
    arg_pEmulationSetDeviceMetricsOverrideWidth
    arg_pEmulationSetDeviceMetricsOverrideHeight
    arg_pEmulationSetDeviceMetricsOverrideDeviceScaleFactor
    arg_pEmulationSetDeviceMetricsOverrideMobile
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PEmulationSetDeviceMetricsOverride where
  toJSON p = A.object $ catMaybes [
    ("width" A..=) <$> Just (pEmulationSetDeviceMetricsOverrideWidth p),
    ("height" A..=) <$> Just (pEmulationSetDeviceMetricsOverrideHeight p),
    ("deviceScaleFactor" A..=) <$> Just (pEmulationSetDeviceMetricsOverrideDeviceScaleFactor p),
    ("mobile" A..=) <$> Just (pEmulationSetDeviceMetricsOverrideMobile p),
    ("scale" A..=) <$> (pEmulationSetDeviceMetricsOverrideScale p),
    ("screenWidth" A..=) <$> (pEmulationSetDeviceMetricsOverrideScreenWidth p),
    ("screenHeight" A..=) <$> (pEmulationSetDeviceMetricsOverrideScreenHeight p),
    ("positionX" A..=) <$> (pEmulationSetDeviceMetricsOverridePositionX p),
    ("positionY" A..=) <$> (pEmulationSetDeviceMetricsOverridePositionY p),
    ("dontSetVisibleSize" A..=) <$> (pEmulationSetDeviceMetricsOverrideDontSetVisibleSize p),
    ("screenOrientation" A..=) <$> (pEmulationSetDeviceMetricsOverrideScreenOrientation p),
    ("viewport" A..=) <$> (pEmulationSetDeviceMetricsOverrideViewport p),
    ("displayFeature" A..=) <$> (pEmulationSetDeviceMetricsOverrideDisplayFeature p)
    ]
instance Command PEmulationSetDeviceMetricsOverride where
  type CommandResponse PEmulationSetDeviceMetricsOverride = ()
  commandName _ = "Emulation.setDeviceMetricsOverride"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Emulation.setScrollbarsHidden' command.
data PEmulationSetScrollbarsHidden = PEmulationSetScrollbarsHidden
  {
    -- | Whether scrollbars should be always hidden.
    pEmulationSetScrollbarsHiddenHidden :: Bool
  }
  deriving (Eq, Show)
pEmulationSetScrollbarsHidden
  -- | Whether scrollbars should be always hidden.
  :: Bool
  -> PEmulationSetScrollbarsHidden
pEmulationSetScrollbarsHidden
  arg_pEmulationSetScrollbarsHiddenHidden
  = PEmulationSetScrollbarsHidden
    arg_pEmulationSetScrollbarsHiddenHidden
instance ToJSON PEmulationSetScrollbarsHidden where
  toJSON p = A.object $ catMaybes [
    ("hidden" A..=) <$> Just (pEmulationSetScrollbarsHiddenHidden p)
    ]
instance Command PEmulationSetScrollbarsHidden where
  type CommandResponse PEmulationSetScrollbarsHidden = ()
  commandName _ = "Emulation.setScrollbarsHidden"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Emulation.setDocumentCookieDisabled' command.
data PEmulationSetDocumentCookieDisabled = PEmulationSetDocumentCookieDisabled
  {
    -- | Whether document.coookie API should be disabled.
    pEmulationSetDocumentCookieDisabledDisabled :: Bool
  }
  deriving (Eq, Show)
pEmulationSetDocumentCookieDisabled
  -- | Whether document.coookie API should be disabled.
  :: Bool
  -> PEmulationSetDocumentCookieDisabled
pEmulationSetDocumentCookieDisabled
  arg_pEmulationSetDocumentCookieDisabledDisabled
  = PEmulationSetDocumentCookieDisabled
    arg_pEmulationSetDocumentCookieDisabledDisabled
instance ToJSON PEmulationSetDocumentCookieDisabled where
  toJSON p = A.object $ catMaybes [
    ("disabled" A..=) <$> Just (pEmulationSetDocumentCookieDisabledDisabled p)
    ]
instance Command PEmulationSetDocumentCookieDisabled where
  type CommandResponse PEmulationSetDocumentCookieDisabled = ()
  commandName _ = "Emulation.setDocumentCookieDisabled"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Emulation.setEmitTouchEventsForMouse' command.
data PEmulationSetEmitTouchEventsForMouseConfiguration = PEmulationSetEmitTouchEventsForMouseConfigurationMobile | PEmulationSetEmitTouchEventsForMouseConfigurationDesktop
  deriving (Ord, Eq, Show, Read)
instance FromJSON PEmulationSetEmitTouchEventsForMouseConfiguration where
  parseJSON = A.withText "PEmulationSetEmitTouchEventsForMouseConfiguration" $ \v -> case v of
    "mobile" -> pure PEmulationSetEmitTouchEventsForMouseConfigurationMobile
    "desktop" -> pure PEmulationSetEmitTouchEventsForMouseConfigurationDesktop
    "_" -> fail "failed to parse PEmulationSetEmitTouchEventsForMouseConfiguration"
instance ToJSON PEmulationSetEmitTouchEventsForMouseConfiguration where
  toJSON v = A.String $ case v of
    PEmulationSetEmitTouchEventsForMouseConfigurationMobile -> "mobile"
    PEmulationSetEmitTouchEventsForMouseConfigurationDesktop -> "desktop"
data PEmulationSetEmitTouchEventsForMouse = PEmulationSetEmitTouchEventsForMouse
  {
    -- | Whether touch emulation based on mouse input should be enabled.
    pEmulationSetEmitTouchEventsForMouseEnabled :: Bool,
    -- | Touch/gesture events configuration. Default: current platform.
    pEmulationSetEmitTouchEventsForMouseConfiguration :: Maybe PEmulationSetEmitTouchEventsForMouseConfiguration
  }
  deriving (Eq, Show)
pEmulationSetEmitTouchEventsForMouse
  -- | Whether touch emulation based on mouse input should be enabled.
  :: Bool
  -> PEmulationSetEmitTouchEventsForMouse
pEmulationSetEmitTouchEventsForMouse
  arg_pEmulationSetEmitTouchEventsForMouseEnabled
  = PEmulationSetEmitTouchEventsForMouse
    arg_pEmulationSetEmitTouchEventsForMouseEnabled
    Nothing
instance ToJSON PEmulationSetEmitTouchEventsForMouse where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pEmulationSetEmitTouchEventsForMouseEnabled p),
    ("configuration" A..=) <$> (pEmulationSetEmitTouchEventsForMouseConfiguration p)
    ]
instance Command PEmulationSetEmitTouchEventsForMouse where
  type CommandResponse PEmulationSetEmitTouchEventsForMouse = ()
  commandName _ = "Emulation.setEmitTouchEventsForMouse"
  fromJSON = const . A.Success . const ()

-- | Emulates the given media type or media feature for CSS media queries.

-- | Parameters of the 'Emulation.setEmulatedMedia' command.
data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia
  {
    -- | Media type to emulate. Empty string disables the override.
    pEmulationSetEmulatedMediaMedia :: Maybe T.Text,
    -- | Media features to emulate.
    pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
  }
  deriving (Eq, Show)
pEmulationSetEmulatedMedia
  :: PEmulationSetEmulatedMedia
pEmulationSetEmulatedMedia
  = PEmulationSetEmulatedMedia
    Nothing
    Nothing
instance ToJSON PEmulationSetEmulatedMedia where
  toJSON p = A.object $ catMaybes [
    ("media" A..=) <$> (pEmulationSetEmulatedMediaMedia p),
    ("features" A..=) <$> (pEmulationSetEmulatedMediaFeatures p)
    ]
instance Command PEmulationSetEmulatedMedia where
  type CommandResponse PEmulationSetEmulatedMedia = ()
  commandName _ = "Emulation.setEmulatedMedia"
  fromJSON = const . A.Success . const ()

-- | Emulates the given vision deficiency.

-- | Parameters of the 'Emulation.setEmulatedVisionDeficiency' command.
data PEmulationSetEmulatedVisionDeficiencyType = PEmulationSetEmulatedVisionDeficiencyTypeNone | PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia | PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision | PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia | PEmulationSetEmulatedVisionDeficiencyTypeProtanopia | PEmulationSetEmulatedVisionDeficiencyTypeTritanopia
  deriving (Ord, Eq, Show, Read)
instance FromJSON PEmulationSetEmulatedVisionDeficiencyType where
  parseJSON = A.withText "PEmulationSetEmulatedVisionDeficiencyType" $ \v -> case v of
    "none" -> pure PEmulationSetEmulatedVisionDeficiencyTypeNone
    "achromatopsia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia
    "blurredVision" -> pure PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision
    "deuteranopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia
    "protanopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeProtanopia
    "tritanopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeTritanopia
    "_" -> fail "failed to parse PEmulationSetEmulatedVisionDeficiencyType"
instance ToJSON PEmulationSetEmulatedVisionDeficiencyType where
  toJSON v = A.String $ case v of
    PEmulationSetEmulatedVisionDeficiencyTypeNone -> "none"
    PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia -> "achromatopsia"
    PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision -> "blurredVision"
    PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia -> "deuteranopia"
    PEmulationSetEmulatedVisionDeficiencyTypeProtanopia -> "protanopia"
    PEmulationSetEmulatedVisionDeficiencyTypeTritanopia -> "tritanopia"
data PEmulationSetEmulatedVisionDeficiency = PEmulationSetEmulatedVisionDeficiency
  {
    -- | Vision deficiency to emulate.
    pEmulationSetEmulatedVisionDeficiencyType :: PEmulationSetEmulatedVisionDeficiencyType
  }
  deriving (Eq, Show)
pEmulationSetEmulatedVisionDeficiency
  -- | Vision deficiency to emulate.
  :: PEmulationSetEmulatedVisionDeficiencyType
  -> PEmulationSetEmulatedVisionDeficiency
pEmulationSetEmulatedVisionDeficiency
  arg_pEmulationSetEmulatedVisionDeficiencyType
  = PEmulationSetEmulatedVisionDeficiency
    arg_pEmulationSetEmulatedVisionDeficiencyType
instance ToJSON PEmulationSetEmulatedVisionDeficiency where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pEmulationSetEmulatedVisionDeficiencyType p)
    ]
instance Command PEmulationSetEmulatedVisionDeficiency where
  type CommandResponse PEmulationSetEmulatedVisionDeficiency = ()
  commandName _ = "Emulation.setEmulatedVisionDeficiency"
  fromJSON = const . A.Success . const ()

-- | Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
--   unavailable.

-- | Parameters of the 'Emulation.setGeolocationOverride' command.
data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride
  {
    -- | Mock latitude
    pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
    -- | Mock longitude
    pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
    -- | Mock accuracy
    pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
  }
  deriving (Eq, Show)
pEmulationSetGeolocationOverride
  :: PEmulationSetGeolocationOverride
pEmulationSetGeolocationOverride
  = PEmulationSetGeolocationOverride
    Nothing
    Nothing
    Nothing
instance ToJSON PEmulationSetGeolocationOverride where
  toJSON p = A.object $ catMaybes [
    ("latitude" A..=) <$> (pEmulationSetGeolocationOverrideLatitude p),
    ("longitude" A..=) <$> (pEmulationSetGeolocationOverrideLongitude p),
    ("accuracy" A..=) <$> (pEmulationSetGeolocationOverrideAccuracy p)
    ]
instance Command PEmulationSetGeolocationOverride where
  type CommandResponse PEmulationSetGeolocationOverride = ()
  commandName _ = "Emulation.setGeolocationOverride"
  fromJSON = const . A.Success . const ()

-- | Overrides the Idle state.

-- | Parameters of the 'Emulation.setIdleOverride' command.
data PEmulationSetIdleOverride = PEmulationSetIdleOverride
  {
    -- | Mock isUserActive
    pEmulationSetIdleOverrideIsUserActive :: Bool,
    -- | Mock isScreenUnlocked
    pEmulationSetIdleOverrideIsScreenUnlocked :: Bool
  }
  deriving (Eq, Show)
pEmulationSetIdleOverride
  -- | Mock isUserActive
  :: Bool
  -- | Mock isScreenUnlocked
  -> Bool
  -> PEmulationSetIdleOverride
pEmulationSetIdleOverride
  arg_pEmulationSetIdleOverrideIsUserActive
  arg_pEmulationSetIdleOverrideIsScreenUnlocked
  = PEmulationSetIdleOverride
    arg_pEmulationSetIdleOverrideIsUserActive
    arg_pEmulationSetIdleOverrideIsScreenUnlocked
instance ToJSON PEmulationSetIdleOverride where
  toJSON p = A.object $ catMaybes [
    ("isUserActive" A..=) <$> Just (pEmulationSetIdleOverrideIsUserActive p),
    ("isScreenUnlocked" A..=) <$> Just (pEmulationSetIdleOverrideIsScreenUnlocked p)
    ]
instance Command PEmulationSetIdleOverride where
  type CommandResponse PEmulationSetIdleOverride = ()
  commandName _ = "Emulation.setIdleOverride"
  fromJSON = const . A.Success . const ()

-- | Clears Idle state overrides.

-- | Parameters of the 'Emulation.clearIdleOverride' command.
data PEmulationClearIdleOverride = PEmulationClearIdleOverride
  deriving (Eq, Show)
pEmulationClearIdleOverride
  :: PEmulationClearIdleOverride
pEmulationClearIdleOverride
  = PEmulationClearIdleOverride
instance ToJSON PEmulationClearIdleOverride where
  toJSON _ = A.Null
instance Command PEmulationClearIdleOverride where
  type CommandResponse PEmulationClearIdleOverride = ()
  commandName _ = "Emulation.clearIdleOverride"
  fromJSON = const . A.Success . const ()

-- | Sets a specified page scale factor.

-- | Parameters of the 'Emulation.setPageScaleFactor' command.
data PEmulationSetPageScaleFactor = PEmulationSetPageScaleFactor
  {
    -- | Page scale factor.
    pEmulationSetPageScaleFactorPageScaleFactor :: Double
  }
  deriving (Eq, Show)
pEmulationSetPageScaleFactor
  -- | Page scale factor.
  :: Double
  -> PEmulationSetPageScaleFactor
pEmulationSetPageScaleFactor
  arg_pEmulationSetPageScaleFactorPageScaleFactor
  = PEmulationSetPageScaleFactor
    arg_pEmulationSetPageScaleFactorPageScaleFactor
instance ToJSON PEmulationSetPageScaleFactor where
  toJSON p = A.object $ catMaybes [
    ("pageScaleFactor" A..=) <$> Just (pEmulationSetPageScaleFactorPageScaleFactor p)
    ]
instance Command PEmulationSetPageScaleFactor where
  type CommandResponse PEmulationSetPageScaleFactor = ()
  commandName _ = "Emulation.setPageScaleFactor"
  fromJSON = const . A.Success . const ()

-- | Switches script execution in the page.

-- | Parameters of the 'Emulation.setScriptExecutionDisabled' command.
data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled
  {
    -- | Whether script execution should be disabled in the page.
    pEmulationSetScriptExecutionDisabledValue :: Bool
  }
  deriving (Eq, Show)
pEmulationSetScriptExecutionDisabled
  -- | Whether script execution should be disabled in the page.
  :: Bool
  -> PEmulationSetScriptExecutionDisabled
pEmulationSetScriptExecutionDisabled
  arg_pEmulationSetScriptExecutionDisabledValue
  = PEmulationSetScriptExecutionDisabled
    arg_pEmulationSetScriptExecutionDisabledValue
instance ToJSON PEmulationSetScriptExecutionDisabled where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (pEmulationSetScriptExecutionDisabledValue p)
    ]
instance Command PEmulationSetScriptExecutionDisabled where
  type CommandResponse PEmulationSetScriptExecutionDisabled = ()
  commandName _ = "Emulation.setScriptExecutionDisabled"
  fromJSON = const . A.Success . const ()

-- | Enables touch on platforms which do not support them.

-- | Parameters of the 'Emulation.setTouchEmulationEnabled' command.
data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled
  {
    -- | Whether the touch event emulation should be enabled.
    pEmulationSetTouchEmulationEnabledEnabled :: Bool,
    -- | Maximum touch points supported. Defaults to one.
    pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
  }
  deriving (Eq, Show)
pEmulationSetTouchEmulationEnabled
  -- | Whether the touch event emulation should be enabled.
  :: Bool
  -> PEmulationSetTouchEmulationEnabled
pEmulationSetTouchEmulationEnabled
  arg_pEmulationSetTouchEmulationEnabledEnabled
  = PEmulationSetTouchEmulationEnabled
    arg_pEmulationSetTouchEmulationEnabledEnabled
    Nothing
instance ToJSON PEmulationSetTouchEmulationEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pEmulationSetTouchEmulationEnabledEnabled p),
    ("maxTouchPoints" A..=) <$> (pEmulationSetTouchEmulationEnabledMaxTouchPoints p)
    ]
instance Command PEmulationSetTouchEmulationEnabled where
  type CommandResponse PEmulationSetTouchEmulationEnabled = ()
  commandName _ = "Emulation.setTouchEmulationEnabled"
  fromJSON = const . A.Success . const ()

-- | Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
--   the current virtual time policy.  Note this supersedes any previous time budget.

-- | Parameters of the 'Emulation.setVirtualTimePolicy' command.
data PEmulationSetVirtualTimePolicy = PEmulationSetVirtualTimePolicy
  {
    pEmulationSetVirtualTimePolicyPolicy :: EmulationVirtualTimePolicy,
    -- | If set, after this many virtual milliseconds have elapsed virtual time will be paused and a
    --   virtualTimeBudgetExpired event is sent.
    pEmulationSetVirtualTimePolicyBudget :: Maybe Double,
    -- | If set this specifies the maximum number of tasks that can be run before virtual is forced
    --   forwards to prevent deadlock.
    pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount :: Maybe Int,
    -- | If set, base::Time::Now will be overridden to initially return this value.
    pEmulationSetVirtualTimePolicyInitialVirtualTime :: Maybe NetworkTimeSinceEpoch
  }
  deriving (Eq, Show)
pEmulationSetVirtualTimePolicy
  :: EmulationVirtualTimePolicy
  -> PEmulationSetVirtualTimePolicy
pEmulationSetVirtualTimePolicy
  arg_pEmulationSetVirtualTimePolicyPolicy
  = PEmulationSetVirtualTimePolicy
    arg_pEmulationSetVirtualTimePolicyPolicy
    Nothing
    Nothing
    Nothing
instance ToJSON PEmulationSetVirtualTimePolicy where
  toJSON p = A.object $ catMaybes [
    ("policy" A..=) <$> Just (pEmulationSetVirtualTimePolicyPolicy p),
    ("budget" A..=) <$> (pEmulationSetVirtualTimePolicyBudget p),
    ("maxVirtualTimeTaskStarvationCount" A..=) <$> (pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount p),
    ("initialVirtualTime" A..=) <$> (pEmulationSetVirtualTimePolicyInitialVirtualTime p)
    ]
data EmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy
  {
    -- | Absolute timestamp at which virtual time was first enabled (up time in milliseconds).
    emulationSetVirtualTimePolicyVirtualTimeTicksBase :: Double
  }
  deriving (Eq, Show)
instance FromJSON EmulationSetVirtualTimePolicy where
  parseJSON = A.withObject "EmulationSetVirtualTimePolicy" $ \o -> EmulationSetVirtualTimePolicy
    <$> o A..: "virtualTimeTicksBase"
instance Command PEmulationSetVirtualTimePolicy where
  type CommandResponse PEmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy
  commandName _ = "Emulation.setVirtualTimePolicy"

-- | Overrides default host system locale with the specified one.

-- | Parameters of the 'Emulation.setLocaleOverride' command.
data PEmulationSetLocaleOverride = PEmulationSetLocaleOverride
  {
    -- | ICU style C locale (e.g. "en_US"). If not specified or empty, disables the override and
    --   restores default host system locale.
    pEmulationSetLocaleOverrideLocale :: Maybe T.Text
  }
  deriving (Eq, Show)
pEmulationSetLocaleOverride
  :: PEmulationSetLocaleOverride
pEmulationSetLocaleOverride
  = PEmulationSetLocaleOverride
    Nothing
instance ToJSON PEmulationSetLocaleOverride where
  toJSON p = A.object $ catMaybes [
    ("locale" A..=) <$> (pEmulationSetLocaleOverrideLocale p)
    ]
instance Command PEmulationSetLocaleOverride where
  type CommandResponse PEmulationSetLocaleOverride = ()
  commandName _ = "Emulation.setLocaleOverride"
  fromJSON = const . A.Success . const ()

-- | Overrides default host system timezone with the specified one.

-- | Parameters of the 'Emulation.setTimezoneOverride' command.
data PEmulationSetTimezoneOverride = PEmulationSetTimezoneOverride
  {
    -- | The timezone identifier. If empty, disables the override and
    --   restores default host system timezone.
    pEmulationSetTimezoneOverrideTimezoneId :: T.Text
  }
  deriving (Eq, Show)
pEmulationSetTimezoneOverride
  -- | The timezone identifier. If empty, disables the override and
  --   restores default host system timezone.
  :: T.Text
  -> PEmulationSetTimezoneOverride
pEmulationSetTimezoneOverride
  arg_pEmulationSetTimezoneOverrideTimezoneId
  = PEmulationSetTimezoneOverride
    arg_pEmulationSetTimezoneOverrideTimezoneId
instance ToJSON PEmulationSetTimezoneOverride where
  toJSON p = A.object $ catMaybes [
    ("timezoneId" A..=) <$> Just (pEmulationSetTimezoneOverrideTimezoneId p)
    ]
instance Command PEmulationSetTimezoneOverride where
  type CommandResponse PEmulationSetTimezoneOverride = ()
  commandName _ = "Emulation.setTimezoneOverride"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Emulation.setDisabledImageTypes' command.
data PEmulationSetDisabledImageTypes = PEmulationSetDisabledImageTypes
  {
    -- | Image types to disable.
    pEmulationSetDisabledImageTypesImageTypes :: [EmulationDisabledImageType]
  }
  deriving (Eq, Show)
pEmulationSetDisabledImageTypes
  -- | Image types to disable.
  :: [EmulationDisabledImageType]
  -> PEmulationSetDisabledImageTypes
pEmulationSetDisabledImageTypes
  arg_pEmulationSetDisabledImageTypesImageTypes
  = PEmulationSetDisabledImageTypes
    arg_pEmulationSetDisabledImageTypesImageTypes
instance ToJSON PEmulationSetDisabledImageTypes where
  toJSON p = A.object $ catMaybes [
    ("imageTypes" A..=) <$> Just (pEmulationSetDisabledImageTypesImageTypes p)
    ]
instance Command PEmulationSetDisabledImageTypes where
  type CommandResponse PEmulationSetDisabledImageTypes = ()
  commandName _ = "Emulation.setDisabledImageTypes"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Emulation.setHardwareConcurrencyOverride' command.
data PEmulationSetHardwareConcurrencyOverride = PEmulationSetHardwareConcurrencyOverride
  {
    -- | Hardware concurrency to report
    pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency :: Int
  }
  deriving (Eq, Show)
pEmulationSetHardwareConcurrencyOverride
  -- | Hardware concurrency to report
  :: Int
  -> PEmulationSetHardwareConcurrencyOverride
pEmulationSetHardwareConcurrencyOverride
  arg_pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency
  = PEmulationSetHardwareConcurrencyOverride
    arg_pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency
instance ToJSON PEmulationSetHardwareConcurrencyOverride where
  toJSON p = A.object $ catMaybes [
    ("hardwareConcurrency" A..=) <$> Just (pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency p)
    ]
instance Command PEmulationSetHardwareConcurrencyOverride where
  type CommandResponse PEmulationSetHardwareConcurrencyOverride = ()
  commandName _ = "Emulation.setHardwareConcurrencyOverride"
  fromJSON = const . A.Success . const ()

-- | Allows overriding user agent with the given string.

-- | Parameters of the 'Emulation.setUserAgentOverride' command.
data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride
  {
    -- | User agent to use.
    pEmulationSetUserAgentOverrideUserAgent :: T.Text,
    -- | Browser langugage to emulate.
    pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe T.Text,
    -- | The platform navigator.platform should return.
    pEmulationSetUserAgentOverridePlatform :: Maybe T.Text,
    -- | To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
    pEmulationSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
  }
  deriving (Eq, Show)
pEmulationSetUserAgentOverride
  -- | User agent to use.
  :: T.Text
  -> PEmulationSetUserAgentOverride
pEmulationSetUserAgentOverride
  arg_pEmulationSetUserAgentOverrideUserAgent
  = PEmulationSetUserAgentOverride
    arg_pEmulationSetUserAgentOverrideUserAgent
    Nothing
    Nothing
    Nothing
instance ToJSON PEmulationSetUserAgentOverride where
  toJSON p = A.object $ catMaybes [
    ("userAgent" A..=) <$> Just (pEmulationSetUserAgentOverrideUserAgent p),
    ("acceptLanguage" A..=) <$> (pEmulationSetUserAgentOverrideAcceptLanguage p),
    ("platform" A..=) <$> (pEmulationSetUserAgentOverridePlatform p),
    ("userAgentMetadata" A..=) <$> (pEmulationSetUserAgentOverrideUserAgentMetadata p)
    ]
instance Command PEmulationSetUserAgentOverride where
  type CommandResponse PEmulationSetUserAgentOverride = ()
  commandName _ = "Emulation.setUserAgentOverride"
  fromJSON = const . A.Success . const ()

-- | Allows overriding the automation flag.

-- | Parameters of the 'Emulation.setAutomationOverride' command.
data PEmulationSetAutomationOverride = PEmulationSetAutomationOverride
  {
    -- | Whether the override should be enabled.
    pEmulationSetAutomationOverrideEnabled :: Bool
  }
  deriving (Eq, Show)
pEmulationSetAutomationOverride
  -- | Whether the override should be enabled.
  :: Bool
  -> PEmulationSetAutomationOverride
pEmulationSetAutomationOverride
  arg_pEmulationSetAutomationOverrideEnabled
  = PEmulationSetAutomationOverride
    arg_pEmulationSetAutomationOverrideEnabled
instance ToJSON PEmulationSetAutomationOverride where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pEmulationSetAutomationOverrideEnabled p)
    ]
instance Command PEmulationSetAutomationOverride where
  type CommandResponse PEmulationSetAutomationOverride = ()
  commandName _ = "Emulation.setAutomationOverride"
  fromJSON = const . A.Success . const ()

-- | Type 'Network.ResourceType'.
--   Resource type as it was perceived by the rendering engine.
data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXHR | NetworkResourceTypeFetch | NetworkResourceTypePrefetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCSPViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkResourceType where
  parseJSON = A.withText "NetworkResourceType" $ \v -> case v of
    "Document" -> pure NetworkResourceTypeDocument
    "Stylesheet" -> pure NetworkResourceTypeStylesheet
    "Image" -> pure NetworkResourceTypeImage
    "Media" -> pure NetworkResourceTypeMedia
    "Font" -> pure NetworkResourceTypeFont
    "Script" -> pure NetworkResourceTypeScript
    "TextTrack" -> pure NetworkResourceTypeTextTrack
    "XHR" -> pure NetworkResourceTypeXHR
    "Fetch" -> pure NetworkResourceTypeFetch
    "Prefetch" -> pure NetworkResourceTypePrefetch
    "EventSource" -> pure NetworkResourceTypeEventSource
    "WebSocket" -> pure NetworkResourceTypeWebSocket
    "Manifest" -> pure NetworkResourceTypeManifest
    "SignedExchange" -> pure NetworkResourceTypeSignedExchange
    "Ping" -> pure NetworkResourceTypePing
    "CSPViolationReport" -> pure NetworkResourceTypeCSPViolationReport
    "Preflight" -> pure NetworkResourceTypePreflight
    "Other" -> pure NetworkResourceTypeOther
    "_" -> fail "failed to parse NetworkResourceType"
instance ToJSON NetworkResourceType where
  toJSON v = A.String $ case v of
    NetworkResourceTypeDocument -> "Document"
    NetworkResourceTypeStylesheet -> "Stylesheet"
    NetworkResourceTypeImage -> "Image"
    NetworkResourceTypeMedia -> "Media"
    NetworkResourceTypeFont -> "Font"
    NetworkResourceTypeScript -> "Script"
    NetworkResourceTypeTextTrack -> "TextTrack"
    NetworkResourceTypeXHR -> "XHR"
    NetworkResourceTypeFetch -> "Fetch"
    NetworkResourceTypePrefetch -> "Prefetch"
    NetworkResourceTypeEventSource -> "EventSource"
    NetworkResourceTypeWebSocket -> "WebSocket"
    NetworkResourceTypeManifest -> "Manifest"
    NetworkResourceTypeSignedExchange -> "SignedExchange"
    NetworkResourceTypePing -> "Ping"
    NetworkResourceTypeCSPViolationReport -> "CSPViolationReport"
    NetworkResourceTypePreflight -> "Preflight"
    NetworkResourceTypeOther -> "Other"

-- | Type 'Network.LoaderId'.
--   Unique loader identifier.
type NetworkLoaderId = T.Text

-- | Type 'Network.RequestId'.
--   Unique request identifier.
type NetworkRequestId = T.Text

-- | Type 'Network.InterceptionId'.
--   Unique intercepted request identifier.
type NetworkInterceptionId = T.Text

-- | Type 'Network.ErrorReason'.
--   Network level fetch failure reason.
data NetworkErrorReason = NetworkErrorReasonFailed | NetworkErrorReasonAborted | NetworkErrorReasonTimedOut | NetworkErrorReasonAccessDenied | NetworkErrorReasonConnectionClosed | NetworkErrorReasonConnectionReset | NetworkErrorReasonConnectionRefused | NetworkErrorReasonConnectionAborted | NetworkErrorReasonConnectionFailed | NetworkErrorReasonNameNotResolved | NetworkErrorReasonInternetDisconnected | NetworkErrorReasonAddressUnreachable | NetworkErrorReasonBlockedByClient | NetworkErrorReasonBlockedByResponse
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkErrorReason where
  parseJSON = A.withText "NetworkErrorReason" $ \v -> case v of
    "Failed" -> pure NetworkErrorReasonFailed
    "Aborted" -> pure NetworkErrorReasonAborted
    "TimedOut" -> pure NetworkErrorReasonTimedOut
    "AccessDenied" -> pure NetworkErrorReasonAccessDenied
    "ConnectionClosed" -> pure NetworkErrorReasonConnectionClosed
    "ConnectionReset" -> pure NetworkErrorReasonConnectionReset
    "ConnectionRefused" -> pure NetworkErrorReasonConnectionRefused
    "ConnectionAborted" -> pure NetworkErrorReasonConnectionAborted
    "ConnectionFailed" -> pure NetworkErrorReasonConnectionFailed
    "NameNotResolved" -> pure NetworkErrorReasonNameNotResolved
    "InternetDisconnected" -> pure NetworkErrorReasonInternetDisconnected
    "AddressUnreachable" -> pure NetworkErrorReasonAddressUnreachable
    "BlockedByClient" -> pure NetworkErrorReasonBlockedByClient
    "BlockedByResponse" -> pure NetworkErrorReasonBlockedByResponse
    "_" -> fail "failed to parse NetworkErrorReason"
instance ToJSON NetworkErrorReason where
  toJSON v = A.String $ case v of
    NetworkErrorReasonFailed -> "Failed"
    NetworkErrorReasonAborted -> "Aborted"
    NetworkErrorReasonTimedOut -> "TimedOut"
    NetworkErrorReasonAccessDenied -> "AccessDenied"
    NetworkErrorReasonConnectionClosed -> "ConnectionClosed"
    NetworkErrorReasonConnectionReset -> "ConnectionReset"
    NetworkErrorReasonConnectionRefused -> "ConnectionRefused"
    NetworkErrorReasonConnectionAborted -> "ConnectionAborted"
    NetworkErrorReasonConnectionFailed -> "ConnectionFailed"
    NetworkErrorReasonNameNotResolved -> "NameNotResolved"
    NetworkErrorReasonInternetDisconnected -> "InternetDisconnected"
    NetworkErrorReasonAddressUnreachable -> "AddressUnreachable"
    NetworkErrorReasonBlockedByClient -> "BlockedByClient"
    NetworkErrorReasonBlockedByResponse -> "BlockedByResponse"

-- | Type 'Network.TimeSinceEpoch'.
--   UTC time in seconds, counted from January 1, 1970.
type NetworkTimeSinceEpoch = Double

-- | Type 'Network.MonotonicTime'.
--   Monotonically increasing time in seconds since an arbitrary point in the past.
type NetworkMonotonicTime = Double

-- | Type 'Network.Headers'.
--   Request / response headers as keys / values of JSON object.
type NetworkHeaders = [(T.Text, T.Text)]

-- | Type 'Network.ConnectionType'.
--   The underlying connection technology that the browser is supposedly using.
data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkConnectionType where
  parseJSON = A.withText "NetworkConnectionType" $ \v -> case v of
    "none" -> pure NetworkConnectionTypeNone
    "cellular2g" -> pure NetworkConnectionTypeCellular2g
    "cellular3g" -> pure NetworkConnectionTypeCellular3g
    "cellular4g" -> pure NetworkConnectionTypeCellular4g
    "bluetooth" -> pure NetworkConnectionTypeBluetooth
    "ethernet" -> pure NetworkConnectionTypeEthernet
    "wifi" -> pure NetworkConnectionTypeWifi
    "wimax" -> pure NetworkConnectionTypeWimax
    "other" -> pure NetworkConnectionTypeOther
    "_" -> fail "failed to parse NetworkConnectionType"
instance ToJSON NetworkConnectionType where
  toJSON v = A.String $ case v of
    NetworkConnectionTypeNone -> "none"
    NetworkConnectionTypeCellular2g -> "cellular2g"
    NetworkConnectionTypeCellular3g -> "cellular3g"
    NetworkConnectionTypeCellular4g -> "cellular4g"
    NetworkConnectionTypeBluetooth -> "bluetooth"
    NetworkConnectionTypeEthernet -> "ethernet"
    NetworkConnectionTypeWifi -> "wifi"
    NetworkConnectionTypeWimax -> "wimax"
    NetworkConnectionTypeOther -> "other"

-- | Type 'Network.CookieSameSite'.
--   Represents the cookie's 'SameSite' status:
--   https://tools.ietf.org/html/draft-west-first-party-cookies
data NetworkCookieSameSite = NetworkCookieSameSiteStrict | NetworkCookieSameSiteLax | NetworkCookieSameSiteNone
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieSameSite where
  parseJSON = A.withText "NetworkCookieSameSite" $ \v -> case v of
    "Strict" -> pure NetworkCookieSameSiteStrict
    "Lax" -> pure NetworkCookieSameSiteLax
    "None" -> pure NetworkCookieSameSiteNone
    "_" -> fail "failed to parse NetworkCookieSameSite"
instance ToJSON NetworkCookieSameSite where
  toJSON v = A.String $ case v of
    NetworkCookieSameSiteStrict -> "Strict"
    NetworkCookieSameSiteLax -> "Lax"
    NetworkCookieSameSiteNone -> "None"

-- | Type 'Network.CookiePriority'.
--   Represents the cookie's 'Priority' status:
--   https://tools.ietf.org/html/draft-west-cookie-priority-00
data NetworkCookiePriority = NetworkCookiePriorityLow | NetworkCookiePriorityMedium | NetworkCookiePriorityHigh
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookiePriority where
  parseJSON = A.withText "NetworkCookiePriority" $ \v -> case v of
    "Low" -> pure NetworkCookiePriorityLow
    "Medium" -> pure NetworkCookiePriorityMedium
    "High" -> pure NetworkCookiePriorityHigh
    "_" -> fail "failed to parse NetworkCookiePriority"
instance ToJSON NetworkCookiePriority where
  toJSON v = A.String $ case v of
    NetworkCookiePriorityLow -> "Low"
    NetworkCookiePriorityMedium -> "Medium"
    NetworkCookiePriorityHigh -> "High"

-- | Type 'Network.CookieSourceScheme'.
--   Represents the source scheme of the origin that originally set the cookie.
--   A value of "Unset" allows protocol clients to emulate legacy cookie scope for the scheme.
--   This is a temporary ability and it will be removed in the future.
data NetworkCookieSourceScheme = NetworkCookieSourceSchemeUnset | NetworkCookieSourceSchemeNonSecure | NetworkCookieSourceSchemeSecure
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieSourceScheme where
  parseJSON = A.withText "NetworkCookieSourceScheme" $ \v -> case v of
    "Unset" -> pure NetworkCookieSourceSchemeUnset
    "NonSecure" -> pure NetworkCookieSourceSchemeNonSecure
    "Secure" -> pure NetworkCookieSourceSchemeSecure
    "_" -> fail "failed to parse NetworkCookieSourceScheme"
instance ToJSON NetworkCookieSourceScheme where
  toJSON v = A.String $ case v of
    NetworkCookieSourceSchemeUnset -> "Unset"
    NetworkCookieSourceSchemeNonSecure -> "NonSecure"
    NetworkCookieSourceSchemeSecure -> "Secure"

-- | Type 'Network.ResourceTiming'.
--   Timing information for the request.
data NetworkResourceTiming = NetworkResourceTiming
  {
    -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
    --   milliseconds relatively to this requestTime.
    networkResourceTimingRequestTime :: Double,
    -- | Started resolving proxy.
    networkResourceTimingProxyStart :: Double,
    -- | Finished resolving proxy.
    networkResourceTimingProxyEnd :: Double,
    -- | Started DNS address resolve.
    networkResourceTimingDnsStart :: Double,
    -- | Finished DNS address resolve.
    networkResourceTimingDnsEnd :: Double,
    -- | Started connecting to the remote host.
    networkResourceTimingConnectStart :: Double,
    -- | Connected to the remote host.
    networkResourceTimingConnectEnd :: Double,
    -- | Started SSL handshake.
    networkResourceTimingSslStart :: Double,
    -- | Finished SSL handshake.
    networkResourceTimingSslEnd :: Double,
    -- | Started running ServiceWorker.
    networkResourceTimingWorkerStart :: Double,
    -- | Finished Starting ServiceWorker.
    networkResourceTimingWorkerReady :: Double,
    -- | Started fetch event.
    networkResourceTimingWorkerFetchStart :: Double,
    -- | Settled fetch event respondWith promise.
    networkResourceTimingWorkerRespondWithSettled :: Double,
    -- | Started sending request.
    networkResourceTimingSendStart :: Double,
    -- | Finished sending request.
    networkResourceTimingSendEnd :: Double,
    -- | Time the server started pushing request.
    networkResourceTimingPushStart :: Double,
    -- | Time the server finished pushing request.
    networkResourceTimingPushEnd :: Double,
    -- | Finished receiving response headers.
    networkResourceTimingReceiveHeadersEnd :: Double
  }
  deriving (Eq, Show)
instance FromJSON NetworkResourceTiming where
  parseJSON = A.withObject "NetworkResourceTiming" $ \o -> NetworkResourceTiming
    <$> o A..: "requestTime"
    <*> o A..: "proxyStart"
    <*> o A..: "proxyEnd"
    <*> o A..: "dnsStart"
    <*> o A..: "dnsEnd"
    <*> o A..: "connectStart"
    <*> o A..: "connectEnd"
    <*> o A..: "sslStart"
    <*> o A..: "sslEnd"
    <*> o A..: "workerStart"
    <*> o A..: "workerReady"
    <*> o A..: "workerFetchStart"
    <*> o A..: "workerRespondWithSettled"
    <*> o A..: "sendStart"
    <*> o A..: "sendEnd"
    <*> o A..: "pushStart"
    <*> o A..: "pushEnd"
    <*> o A..: "receiveHeadersEnd"
instance ToJSON NetworkResourceTiming where
  toJSON p = A.object $ catMaybes [
    ("requestTime" A..=) <$> Just (networkResourceTimingRequestTime p),
    ("proxyStart" A..=) <$> Just (networkResourceTimingProxyStart p),
    ("proxyEnd" A..=) <$> Just (networkResourceTimingProxyEnd p),
    ("dnsStart" A..=) <$> Just (networkResourceTimingDnsStart p),
    ("dnsEnd" A..=) <$> Just (networkResourceTimingDnsEnd p),
    ("connectStart" A..=) <$> Just (networkResourceTimingConnectStart p),
    ("connectEnd" A..=) <$> Just (networkResourceTimingConnectEnd p),
    ("sslStart" A..=) <$> Just (networkResourceTimingSslStart p),
    ("sslEnd" A..=) <$> Just (networkResourceTimingSslEnd p),
    ("workerStart" A..=) <$> Just (networkResourceTimingWorkerStart p),
    ("workerReady" A..=) <$> Just (networkResourceTimingWorkerReady p),
    ("workerFetchStart" A..=) <$> Just (networkResourceTimingWorkerFetchStart p),
    ("workerRespondWithSettled" A..=) <$> Just (networkResourceTimingWorkerRespondWithSettled p),
    ("sendStart" A..=) <$> Just (networkResourceTimingSendStart p),
    ("sendEnd" A..=) <$> Just (networkResourceTimingSendEnd p),
    ("pushStart" A..=) <$> Just (networkResourceTimingPushStart p),
    ("pushEnd" A..=) <$> Just (networkResourceTimingPushEnd p),
    ("receiveHeadersEnd" A..=) <$> Just (networkResourceTimingReceiveHeadersEnd p)
    ]

-- | Type 'Network.ResourcePriority'.
--   Loading priority of a resource request.
data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkResourcePriority where
  parseJSON = A.withText "NetworkResourcePriority" $ \v -> case v of
    "VeryLow" -> pure NetworkResourcePriorityVeryLow
    "Low" -> pure NetworkResourcePriorityLow
    "Medium" -> pure NetworkResourcePriorityMedium
    "High" -> pure NetworkResourcePriorityHigh
    "VeryHigh" -> pure NetworkResourcePriorityVeryHigh
    "_" -> fail "failed to parse NetworkResourcePriority"
instance ToJSON NetworkResourcePriority where
  toJSON v = A.String $ case v of
    NetworkResourcePriorityVeryLow -> "VeryLow"
    NetworkResourcePriorityLow -> "Low"
    NetworkResourcePriorityMedium -> "Medium"
    NetworkResourcePriorityHigh -> "High"
    NetworkResourcePriorityVeryHigh -> "VeryHigh"

-- | Type 'Network.PostDataEntry'.
--   Post data entry for HTTP request
data NetworkPostDataEntry = NetworkPostDataEntry
  {
    networkPostDataEntryBytes :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkPostDataEntry where
  parseJSON = A.withObject "NetworkPostDataEntry" $ \o -> NetworkPostDataEntry
    <$> o A..:? "bytes"
instance ToJSON NetworkPostDataEntry where
  toJSON p = A.object $ catMaybes [
    ("bytes" A..=) <$> (networkPostDataEntryBytes p)
    ]

-- | Type 'Network.Request'.
--   HTTP request data.
data NetworkRequestReferrerPolicy = NetworkRequestReferrerPolicyUnsafeUrl | NetworkRequestReferrerPolicyNoReferrerWhenDowngrade | NetworkRequestReferrerPolicyNoReferrer | NetworkRequestReferrerPolicyOrigin | NetworkRequestReferrerPolicyOriginWhenCrossOrigin | NetworkRequestReferrerPolicySameOrigin | NetworkRequestReferrerPolicyStrictOrigin | NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkRequestReferrerPolicy where
  parseJSON = A.withText "NetworkRequestReferrerPolicy" $ \v -> case v of
    "unsafe-url" -> pure NetworkRequestReferrerPolicyUnsafeUrl
    "no-referrer-when-downgrade" -> pure NetworkRequestReferrerPolicyNoReferrerWhenDowngrade
    "no-referrer" -> pure NetworkRequestReferrerPolicyNoReferrer
    "origin" -> pure NetworkRequestReferrerPolicyOrigin
    "origin-when-cross-origin" -> pure NetworkRequestReferrerPolicyOriginWhenCrossOrigin
    "same-origin" -> pure NetworkRequestReferrerPolicySameOrigin
    "strict-origin" -> pure NetworkRequestReferrerPolicyStrictOrigin
    "strict-origin-when-cross-origin" -> pure NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
    "_" -> fail "failed to parse NetworkRequestReferrerPolicy"
instance ToJSON NetworkRequestReferrerPolicy where
  toJSON v = A.String $ case v of
    NetworkRequestReferrerPolicyUnsafeUrl -> "unsafe-url"
    NetworkRequestReferrerPolicyNoReferrerWhenDowngrade -> "no-referrer-when-downgrade"
    NetworkRequestReferrerPolicyNoReferrer -> "no-referrer"
    NetworkRequestReferrerPolicyOrigin -> "origin"
    NetworkRequestReferrerPolicyOriginWhenCrossOrigin -> "origin-when-cross-origin"
    NetworkRequestReferrerPolicySameOrigin -> "same-origin"
    NetworkRequestReferrerPolicyStrictOrigin -> "strict-origin"
    NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"
data NetworkRequest = NetworkRequest
  {
    -- | Request URL (without fragment).
    networkRequestUrl :: T.Text,
    -- | Fragment of the requested URL starting with hash, if present.
    networkRequestUrlFragment :: Maybe T.Text,
    -- | HTTP request method.
    networkRequestMethod :: T.Text,
    -- | HTTP request headers.
    networkRequestHeaders :: NetworkHeaders,
    -- | HTTP POST request data.
    networkRequestPostData :: Maybe T.Text,
    -- | True when the request has POST data. Note that postData might still be omitted when this flag is true when the data is too long.
    networkRequestHasPostData :: Maybe Bool,
    -- | Request body elements. This will be converted from base64 to binary
    networkRequestPostDataEntries :: Maybe [NetworkPostDataEntry],
    -- | The mixed content type of the request.
    networkRequestMixedContentType :: Maybe SecurityMixedContentType,
    -- | Priority of the resource request at the time request is sent.
    networkRequestInitialPriority :: NetworkResourcePriority,
    -- | The referrer policy of the request, as defined in https://www.w3.org/TR/referrer-policy/
    networkRequestReferrerPolicy :: NetworkRequestReferrerPolicy,
    -- | Whether is loaded via link preload.
    networkRequestIsLinkPreload :: Maybe Bool,
    -- | Set for requests when the TrustToken API is used. Contains the parameters
    --   passed by the developer (e.g. via "fetch") as understood by the backend.
    networkRequestTrustTokenParams :: Maybe NetworkTrustTokenParams,
    -- | True if this resource request is considered to be the 'same site' as the
    --   request correspondinfg to the main frame.
    networkRequestIsSameSite :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkRequest where
  parseJSON = A.withObject "NetworkRequest" $ \o -> NetworkRequest
    <$> o A..: "url"
    <*> o A..:? "urlFragment"
    <*> o A..: "method"
    <*> o A..: "headers"
    <*> o A..:? "postData"
    <*> o A..:? "hasPostData"
    <*> o A..:? "postDataEntries"
    <*> o A..:? "mixedContentType"
    <*> o A..: "initialPriority"
    <*> o A..: "referrerPolicy"
    <*> o A..:? "isLinkPreload"
    <*> o A..:? "trustTokenParams"
    <*> o A..:? "isSameSite"
instance ToJSON NetworkRequest where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (networkRequestUrl p),
    ("urlFragment" A..=) <$> (networkRequestUrlFragment p),
    ("method" A..=) <$> Just (networkRequestMethod p),
    ("headers" A..=) <$> Just (networkRequestHeaders p),
    ("postData" A..=) <$> (networkRequestPostData p),
    ("hasPostData" A..=) <$> (networkRequestHasPostData p),
    ("postDataEntries" A..=) <$> (networkRequestPostDataEntries p),
    ("mixedContentType" A..=) <$> (networkRequestMixedContentType p),
    ("initialPriority" A..=) <$> Just (networkRequestInitialPriority p),
    ("referrerPolicy" A..=) <$> Just (networkRequestReferrerPolicy p),
    ("isLinkPreload" A..=) <$> (networkRequestIsLinkPreload p),
    ("trustTokenParams" A..=) <$> (networkRequestTrustTokenParams p),
    ("isSameSite" A..=) <$> (networkRequestIsSameSite p)
    ]

-- | Type 'Network.SignedCertificateTimestamp'.
--   Details of a signed certificate timestamp (SCT).
data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp
  {
    -- | Validation status.
    networkSignedCertificateTimestampStatus :: T.Text,
    -- | Origin.
    networkSignedCertificateTimestampOrigin :: T.Text,
    -- | Log name / description.
    networkSignedCertificateTimestampLogDescription :: T.Text,
    -- | Log ID.
    networkSignedCertificateTimestampLogId :: T.Text,
    -- | Issuance date. Unlike TimeSinceEpoch, this contains the number of
    --   milliseconds since January 1, 1970, UTC, not the number of seconds.
    networkSignedCertificateTimestampTimestamp :: Double,
    -- | Hash algorithm.
    networkSignedCertificateTimestampHashAlgorithm :: T.Text,
    -- | Signature algorithm.
    networkSignedCertificateTimestampSignatureAlgorithm :: T.Text,
    -- | Signature data.
    networkSignedCertificateTimestampSignatureData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedCertificateTimestamp where
  parseJSON = A.withObject "NetworkSignedCertificateTimestamp" $ \o -> NetworkSignedCertificateTimestamp
    <$> o A..: "status"
    <*> o A..: "origin"
    <*> o A..: "logDescription"
    <*> o A..: "logId"
    <*> o A..: "timestamp"
    <*> o A..: "hashAlgorithm"
    <*> o A..: "signatureAlgorithm"
    <*> o A..: "signatureData"
instance ToJSON NetworkSignedCertificateTimestamp where
  toJSON p = A.object $ catMaybes [
    ("status" A..=) <$> Just (networkSignedCertificateTimestampStatus p),
    ("origin" A..=) <$> Just (networkSignedCertificateTimestampOrigin p),
    ("logDescription" A..=) <$> Just (networkSignedCertificateTimestampLogDescription p),
    ("logId" A..=) <$> Just (networkSignedCertificateTimestampLogId p),
    ("timestamp" A..=) <$> Just (networkSignedCertificateTimestampTimestamp p),
    ("hashAlgorithm" A..=) <$> Just (networkSignedCertificateTimestampHashAlgorithm p),
    ("signatureAlgorithm" A..=) <$> Just (networkSignedCertificateTimestampSignatureAlgorithm p),
    ("signatureData" A..=) <$> Just (networkSignedCertificateTimestampSignatureData p)
    ]

-- | Type 'Network.SecurityDetails'.
--   Security details about a request.
data NetworkSecurityDetails = NetworkSecurityDetails
  {
    -- | Protocol name (e.g. "TLS 1.2" or "QUIC").
    networkSecurityDetailsProtocol :: T.Text,
    -- | Key Exchange used by the connection, or the empty string if not applicable.
    networkSecurityDetailsKeyExchange :: T.Text,
    -- | (EC)DH group used by the connection, if applicable.
    networkSecurityDetailsKeyExchangeGroup :: Maybe T.Text,
    -- | Cipher name.
    networkSecurityDetailsCipher :: T.Text,
    -- | TLS MAC. Note that AEAD ciphers do not have separate MACs.
    networkSecurityDetailsMac :: Maybe T.Text,
    -- | Certificate ID value.
    networkSecurityDetailsCertificateId :: SecurityCertificateId,
    -- | Certificate subject name.
    networkSecurityDetailsSubjectName :: T.Text,
    -- | Subject Alternative Name (SAN) DNS names and IP addresses.
    networkSecurityDetailsSanList :: [T.Text],
    -- | Name of the issuing CA.
    networkSecurityDetailsIssuer :: T.Text,
    -- | Certificate valid from date.
    networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
    -- | Certificate valid to (expiration) date
    networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
    -- | List of signed certificate timestamps (SCTs).
    networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
    -- | Whether the request complied with Certificate Transparency policy
    networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance,
    -- | The signature algorithm used by the server in the TLS server signature,
    --   represented as a TLS SignatureScheme code point. Omitted if not
    --   applicable or not known.
    networkSecurityDetailsServerSignatureAlgorithm :: Maybe Int,
    -- | Whether the connection used Encrypted ClientHello
    networkSecurityDetailsEncryptedClientHello :: Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkSecurityDetails where
  parseJSON = A.withObject "NetworkSecurityDetails" $ \o -> NetworkSecurityDetails
    <$> o A..: "protocol"
    <*> o A..: "keyExchange"
    <*> o A..:? "keyExchangeGroup"
    <*> o A..: "cipher"
    <*> o A..:? "mac"
    <*> o A..: "certificateId"
    <*> o A..: "subjectName"
    <*> o A..: "sanList"
    <*> o A..: "issuer"
    <*> o A..: "validFrom"
    <*> o A..: "validTo"
    <*> o A..: "signedCertificateTimestampList"
    <*> o A..: "certificateTransparencyCompliance"
    <*> o A..:? "serverSignatureAlgorithm"
    <*> o A..: "encryptedClientHello"
instance ToJSON NetworkSecurityDetails where
  toJSON p = A.object $ catMaybes [
    ("protocol" A..=) <$> Just (networkSecurityDetailsProtocol p),
    ("keyExchange" A..=) <$> Just (networkSecurityDetailsKeyExchange p),
    ("keyExchangeGroup" A..=) <$> (networkSecurityDetailsKeyExchangeGroup p),
    ("cipher" A..=) <$> Just (networkSecurityDetailsCipher p),
    ("mac" A..=) <$> (networkSecurityDetailsMac p),
    ("certificateId" A..=) <$> Just (networkSecurityDetailsCertificateId p),
    ("subjectName" A..=) <$> Just (networkSecurityDetailsSubjectName p),
    ("sanList" A..=) <$> Just (networkSecurityDetailsSanList p),
    ("issuer" A..=) <$> Just (networkSecurityDetailsIssuer p),
    ("validFrom" A..=) <$> Just (networkSecurityDetailsValidFrom p),
    ("validTo" A..=) <$> Just (networkSecurityDetailsValidTo p),
    ("signedCertificateTimestampList" A..=) <$> Just (networkSecurityDetailsSignedCertificateTimestampList p),
    ("certificateTransparencyCompliance" A..=) <$> Just (networkSecurityDetailsCertificateTransparencyCompliance p),
    ("serverSignatureAlgorithm" A..=) <$> (networkSecurityDetailsServerSignatureAlgorithm p),
    ("encryptedClientHello" A..=) <$> Just (networkSecurityDetailsEncryptedClientHello p)
    ]

-- | Type 'Network.CertificateTransparencyCompliance'.
--   Whether the request complied with Certificate Transparency policy.
data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCertificateTransparencyCompliance where
  parseJSON = A.withText "NetworkCertificateTransparencyCompliance" $ \v -> case v of
    "unknown" -> pure NetworkCertificateTransparencyComplianceUnknown
    "not-compliant" -> pure NetworkCertificateTransparencyComplianceNotCompliant
    "compliant" -> pure NetworkCertificateTransparencyComplianceCompliant
    "_" -> fail "failed to parse NetworkCertificateTransparencyCompliance"
instance ToJSON NetworkCertificateTransparencyCompliance where
  toJSON v = A.String $ case v of
    NetworkCertificateTransparencyComplianceUnknown -> "unknown"
    NetworkCertificateTransparencyComplianceNotCompliant -> "not-compliant"
    NetworkCertificateTransparencyComplianceCompliant -> "compliant"

-- | Type 'Network.BlockedReason'.
--   The reason why request was blocked.
data NetworkBlockedReason = NetworkBlockedReasonOther | NetworkBlockedReasonCsp | NetworkBlockedReasonMixedContent | NetworkBlockedReasonOrigin | NetworkBlockedReasonInspector | NetworkBlockedReasonSubresourceFilter | NetworkBlockedReasonContentType | NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader | NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | NetworkBlockedReasonCorpNotSameOrigin | NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | NetworkBlockedReasonCorpNotSameSite
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkBlockedReason where
  parseJSON = A.withText "NetworkBlockedReason" $ \v -> case v of
    "other" -> pure NetworkBlockedReasonOther
    "csp" -> pure NetworkBlockedReasonCsp
    "mixed-content" -> pure NetworkBlockedReasonMixedContent
    "origin" -> pure NetworkBlockedReasonOrigin
    "inspector" -> pure NetworkBlockedReasonInspector
    "subresource-filter" -> pure NetworkBlockedReasonSubresourceFilter
    "content-type" -> pure NetworkBlockedReasonContentType
    "coep-frame-resource-needs-coep-header" -> pure NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader
    "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> pure NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
    "corp-not-same-origin" -> pure NetworkBlockedReasonCorpNotSameOrigin
    "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> pure NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
    "corp-not-same-site" -> pure NetworkBlockedReasonCorpNotSameSite
    "_" -> fail "failed to parse NetworkBlockedReason"
instance ToJSON NetworkBlockedReason where
  toJSON v = A.String $ case v of
    NetworkBlockedReasonOther -> "other"
    NetworkBlockedReasonCsp -> "csp"
    NetworkBlockedReasonMixedContent -> "mixed-content"
    NetworkBlockedReasonOrigin -> "origin"
    NetworkBlockedReasonInspector -> "inspector"
    NetworkBlockedReasonSubresourceFilter -> "subresource-filter"
    NetworkBlockedReasonContentType -> "content-type"
    NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader -> "coep-frame-resource-needs-coep-header"
    NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage -> "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
    NetworkBlockedReasonCorpNotSameOrigin -> "corp-not-same-origin"
    NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
    NetworkBlockedReasonCorpNotSameSite -> "corp-not-same-site"

-- | Type 'Network.CorsError'.
--   The reason why request was blocked.
data NetworkCorsError = NetworkCorsErrorDisallowedByMode | NetworkCorsErrorInvalidResponse | NetworkCorsErrorWildcardOriginNotAllowed | NetworkCorsErrorMissingAllowOriginHeader | NetworkCorsErrorMultipleAllowOriginValues | NetworkCorsErrorInvalidAllowOriginValue | NetworkCorsErrorAllowOriginMismatch | NetworkCorsErrorInvalidAllowCredentials | NetworkCorsErrorCorsDisabledScheme | NetworkCorsErrorPreflightInvalidStatus | NetworkCorsErrorPreflightDisallowedRedirect | NetworkCorsErrorPreflightWildcardOriginNotAllowed | NetworkCorsErrorPreflightMissingAllowOriginHeader | NetworkCorsErrorPreflightMultipleAllowOriginValues | NetworkCorsErrorPreflightInvalidAllowOriginValue | NetworkCorsErrorPreflightAllowOriginMismatch | NetworkCorsErrorPreflightInvalidAllowCredentials | NetworkCorsErrorPreflightMissingAllowExternal | NetworkCorsErrorPreflightInvalidAllowExternal | NetworkCorsErrorPreflightMissingAllowPrivateNetwork | NetworkCorsErrorPreflightInvalidAllowPrivateNetwork | NetworkCorsErrorInvalidAllowMethodsPreflightResponse | NetworkCorsErrorInvalidAllowHeadersPreflightResponse | NetworkCorsErrorMethodDisallowedByPreflightResponse | NetworkCorsErrorHeaderDisallowedByPreflightResponse | NetworkCorsErrorRedirectContainsCredentials | NetworkCorsErrorInsecurePrivateNetwork | NetworkCorsErrorInvalidPrivateNetworkAccess | NetworkCorsErrorUnexpectedPrivateNetworkAccess | NetworkCorsErrorNoCorsRedirectModeNotFollow
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCorsError where
  parseJSON = A.withText "NetworkCorsError" $ \v -> case v of
    "DisallowedByMode" -> pure NetworkCorsErrorDisallowedByMode
    "InvalidResponse" -> pure NetworkCorsErrorInvalidResponse
    "WildcardOriginNotAllowed" -> pure NetworkCorsErrorWildcardOriginNotAllowed
    "MissingAllowOriginHeader" -> pure NetworkCorsErrorMissingAllowOriginHeader
    "MultipleAllowOriginValues" -> pure NetworkCorsErrorMultipleAllowOriginValues
    "InvalidAllowOriginValue" -> pure NetworkCorsErrorInvalidAllowOriginValue
    "AllowOriginMismatch" -> pure NetworkCorsErrorAllowOriginMismatch
    "InvalidAllowCredentials" -> pure NetworkCorsErrorInvalidAllowCredentials
    "CorsDisabledScheme" -> pure NetworkCorsErrorCorsDisabledScheme
    "PreflightInvalidStatus" -> pure NetworkCorsErrorPreflightInvalidStatus
    "PreflightDisallowedRedirect" -> pure NetworkCorsErrorPreflightDisallowedRedirect
    "PreflightWildcardOriginNotAllowed" -> pure NetworkCorsErrorPreflightWildcardOriginNotAllowed
    "PreflightMissingAllowOriginHeader" -> pure NetworkCorsErrorPreflightMissingAllowOriginHeader
    "PreflightMultipleAllowOriginValues" -> pure NetworkCorsErrorPreflightMultipleAllowOriginValues
    "PreflightInvalidAllowOriginValue" -> pure NetworkCorsErrorPreflightInvalidAllowOriginValue
    "PreflightAllowOriginMismatch" -> pure NetworkCorsErrorPreflightAllowOriginMismatch
    "PreflightInvalidAllowCredentials" -> pure NetworkCorsErrorPreflightInvalidAllowCredentials
    "PreflightMissingAllowExternal" -> pure NetworkCorsErrorPreflightMissingAllowExternal
    "PreflightInvalidAllowExternal" -> pure NetworkCorsErrorPreflightInvalidAllowExternal
    "PreflightMissingAllowPrivateNetwork" -> pure NetworkCorsErrorPreflightMissingAllowPrivateNetwork
    "PreflightInvalidAllowPrivateNetwork" -> pure NetworkCorsErrorPreflightInvalidAllowPrivateNetwork
    "InvalidAllowMethodsPreflightResponse" -> pure NetworkCorsErrorInvalidAllowMethodsPreflightResponse
    "InvalidAllowHeadersPreflightResponse" -> pure NetworkCorsErrorInvalidAllowHeadersPreflightResponse
    "MethodDisallowedByPreflightResponse" -> pure NetworkCorsErrorMethodDisallowedByPreflightResponse
    "HeaderDisallowedByPreflightResponse" -> pure NetworkCorsErrorHeaderDisallowedByPreflightResponse
    "RedirectContainsCredentials" -> pure NetworkCorsErrorRedirectContainsCredentials
    "InsecurePrivateNetwork" -> pure NetworkCorsErrorInsecurePrivateNetwork
    "InvalidPrivateNetworkAccess" -> pure NetworkCorsErrorInvalidPrivateNetworkAccess
    "UnexpectedPrivateNetworkAccess" -> pure NetworkCorsErrorUnexpectedPrivateNetworkAccess
    "NoCorsRedirectModeNotFollow" -> pure NetworkCorsErrorNoCorsRedirectModeNotFollow
    "_" -> fail "failed to parse NetworkCorsError"
instance ToJSON NetworkCorsError where
  toJSON v = A.String $ case v of
    NetworkCorsErrorDisallowedByMode -> "DisallowedByMode"
    NetworkCorsErrorInvalidResponse -> "InvalidResponse"
    NetworkCorsErrorWildcardOriginNotAllowed -> "WildcardOriginNotAllowed"
    NetworkCorsErrorMissingAllowOriginHeader -> "MissingAllowOriginHeader"
    NetworkCorsErrorMultipleAllowOriginValues -> "MultipleAllowOriginValues"
    NetworkCorsErrorInvalidAllowOriginValue -> "InvalidAllowOriginValue"
    NetworkCorsErrorAllowOriginMismatch -> "AllowOriginMismatch"
    NetworkCorsErrorInvalidAllowCredentials -> "InvalidAllowCredentials"
    NetworkCorsErrorCorsDisabledScheme -> "CorsDisabledScheme"
    NetworkCorsErrorPreflightInvalidStatus -> "PreflightInvalidStatus"
    NetworkCorsErrorPreflightDisallowedRedirect -> "PreflightDisallowedRedirect"
    NetworkCorsErrorPreflightWildcardOriginNotAllowed -> "PreflightWildcardOriginNotAllowed"
    NetworkCorsErrorPreflightMissingAllowOriginHeader -> "PreflightMissingAllowOriginHeader"
    NetworkCorsErrorPreflightMultipleAllowOriginValues -> "PreflightMultipleAllowOriginValues"
    NetworkCorsErrorPreflightInvalidAllowOriginValue -> "PreflightInvalidAllowOriginValue"
    NetworkCorsErrorPreflightAllowOriginMismatch -> "PreflightAllowOriginMismatch"
    NetworkCorsErrorPreflightInvalidAllowCredentials -> "PreflightInvalidAllowCredentials"
    NetworkCorsErrorPreflightMissingAllowExternal -> "PreflightMissingAllowExternal"
    NetworkCorsErrorPreflightInvalidAllowExternal -> "PreflightInvalidAllowExternal"
    NetworkCorsErrorPreflightMissingAllowPrivateNetwork -> "PreflightMissingAllowPrivateNetwork"
    NetworkCorsErrorPreflightInvalidAllowPrivateNetwork -> "PreflightInvalidAllowPrivateNetwork"
    NetworkCorsErrorInvalidAllowMethodsPreflightResponse -> "InvalidAllowMethodsPreflightResponse"
    NetworkCorsErrorInvalidAllowHeadersPreflightResponse -> "InvalidAllowHeadersPreflightResponse"
    NetworkCorsErrorMethodDisallowedByPreflightResponse -> "MethodDisallowedByPreflightResponse"
    NetworkCorsErrorHeaderDisallowedByPreflightResponse -> "HeaderDisallowedByPreflightResponse"
    NetworkCorsErrorRedirectContainsCredentials -> "RedirectContainsCredentials"
    NetworkCorsErrorInsecurePrivateNetwork -> "InsecurePrivateNetwork"
    NetworkCorsErrorInvalidPrivateNetworkAccess -> "InvalidPrivateNetworkAccess"
    NetworkCorsErrorUnexpectedPrivateNetworkAccess -> "UnexpectedPrivateNetworkAccess"
    NetworkCorsErrorNoCorsRedirectModeNotFollow -> "NoCorsRedirectModeNotFollow"

-- | Type 'Network.CorsErrorStatus'.
data NetworkCorsErrorStatus = NetworkCorsErrorStatus
  {
    networkCorsErrorStatusCorsError :: NetworkCorsError,
    networkCorsErrorStatusFailedParameter :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkCorsErrorStatus where
  parseJSON = A.withObject "NetworkCorsErrorStatus" $ \o -> NetworkCorsErrorStatus
    <$> o A..: "corsError"
    <*> o A..: "failedParameter"
instance ToJSON NetworkCorsErrorStatus where
  toJSON p = A.object $ catMaybes [
    ("corsError" A..=) <$> Just (networkCorsErrorStatusCorsError p),
    ("failedParameter" A..=) <$> Just (networkCorsErrorStatusFailedParameter p)
    ]

-- | Type 'Network.ServiceWorkerResponseSource'.
--   Source of serviceworker response.
data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkServiceWorkerResponseSource where
  parseJSON = A.withText "NetworkServiceWorkerResponseSource" $ \v -> case v of
    "cache-storage" -> pure NetworkServiceWorkerResponseSourceCacheStorage
    "http-cache" -> pure NetworkServiceWorkerResponseSourceHttpCache
    "fallback-code" -> pure NetworkServiceWorkerResponseSourceFallbackCode
    "network" -> pure NetworkServiceWorkerResponseSourceNetwork
    "_" -> fail "failed to parse NetworkServiceWorkerResponseSource"
instance ToJSON NetworkServiceWorkerResponseSource where
  toJSON v = A.String $ case v of
    NetworkServiceWorkerResponseSourceCacheStorage -> "cache-storage"
    NetworkServiceWorkerResponseSourceHttpCache -> "http-cache"
    NetworkServiceWorkerResponseSourceFallbackCode -> "fallback-code"
    NetworkServiceWorkerResponseSourceNetwork -> "network"

-- | Type 'Network.TrustTokenParams'.
--   Determines what type of Trust Token operation is executed and
--   depending on the type, some additional parameters. The values
--   are specified in third_party/blink/renderer/core/fetch/trust_token.idl.
data NetworkTrustTokenParamsRefreshPolicy = NetworkTrustTokenParamsRefreshPolicyUseCached | NetworkTrustTokenParamsRefreshPolicyRefresh
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenParamsRefreshPolicy where
  parseJSON = A.withText "NetworkTrustTokenParamsRefreshPolicy" $ \v -> case v of
    "UseCached" -> pure NetworkTrustTokenParamsRefreshPolicyUseCached
    "Refresh" -> pure NetworkTrustTokenParamsRefreshPolicyRefresh
    "_" -> fail "failed to parse NetworkTrustTokenParamsRefreshPolicy"
instance ToJSON NetworkTrustTokenParamsRefreshPolicy where
  toJSON v = A.String $ case v of
    NetworkTrustTokenParamsRefreshPolicyUseCached -> "UseCached"
    NetworkTrustTokenParamsRefreshPolicyRefresh -> "Refresh"
data NetworkTrustTokenParams = NetworkTrustTokenParams
  {
    networkTrustTokenParamsType :: NetworkTrustTokenOperationType,
    -- | Only set for "token-redemption" type and determine whether
    --   to request a fresh SRR or use a still valid cached SRR.
    networkTrustTokenParamsRefreshPolicy :: NetworkTrustTokenParamsRefreshPolicy,
    -- | Origins of issuers from whom to request tokens or redemption
    --   records.
    networkTrustTokenParamsIssuers :: Maybe [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON NetworkTrustTokenParams where
  parseJSON = A.withObject "NetworkTrustTokenParams" $ \o -> NetworkTrustTokenParams
    <$> o A..: "type"
    <*> o A..: "refreshPolicy"
    <*> o A..:? "issuers"
instance ToJSON NetworkTrustTokenParams where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (networkTrustTokenParamsType p),
    ("refreshPolicy" A..=) <$> Just (networkTrustTokenParamsRefreshPolicy p),
    ("issuers" A..=) <$> (networkTrustTokenParamsIssuers p)
    ]

-- | Type 'Network.TrustTokenOperationType'.
data NetworkTrustTokenOperationType = NetworkTrustTokenOperationTypeIssuance | NetworkTrustTokenOperationTypeRedemption | NetworkTrustTokenOperationTypeSigning
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenOperationType where
  parseJSON = A.withText "NetworkTrustTokenOperationType" $ \v -> case v of
    "Issuance" -> pure NetworkTrustTokenOperationTypeIssuance
    "Redemption" -> pure NetworkTrustTokenOperationTypeRedemption
    "Signing" -> pure NetworkTrustTokenOperationTypeSigning
    "_" -> fail "failed to parse NetworkTrustTokenOperationType"
instance ToJSON NetworkTrustTokenOperationType where
  toJSON v = A.String $ case v of
    NetworkTrustTokenOperationTypeIssuance -> "Issuance"
    NetworkTrustTokenOperationTypeRedemption -> "Redemption"
    NetworkTrustTokenOperationTypeSigning -> "Signing"

-- | Type 'Network.AlternateProtocolUsage'.
--   The reason why Chrome uses a specific transport protocol for HTTP semantics.
data NetworkAlternateProtocolUsage = NetworkAlternateProtocolUsageAlternativeJobWonWithoutRace | NetworkAlternateProtocolUsageAlternativeJobWonRace | NetworkAlternateProtocolUsageMainJobWonRace | NetworkAlternateProtocolUsageMappingMissing | NetworkAlternateProtocolUsageBroken | NetworkAlternateProtocolUsageDnsAlpnH3JobWonWithoutRace | NetworkAlternateProtocolUsageDnsAlpnH3JobWonRace | NetworkAlternateProtocolUsageUnspecifiedReason
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkAlternateProtocolUsage where
  parseJSON = A.withText "NetworkAlternateProtocolUsage" $ \v -> case v of
    "alternativeJobWonWithoutRace" -> pure NetworkAlternateProtocolUsageAlternativeJobWonWithoutRace
    "alternativeJobWonRace" -> pure NetworkAlternateProtocolUsageAlternativeJobWonRace
    "mainJobWonRace" -> pure NetworkAlternateProtocolUsageMainJobWonRace
    "mappingMissing" -> pure NetworkAlternateProtocolUsageMappingMissing
    "broken" -> pure NetworkAlternateProtocolUsageBroken
    "dnsAlpnH3JobWonWithoutRace" -> pure NetworkAlternateProtocolUsageDnsAlpnH3JobWonWithoutRace
    "dnsAlpnH3JobWonRace" -> pure NetworkAlternateProtocolUsageDnsAlpnH3JobWonRace
    "unspecifiedReason" -> pure NetworkAlternateProtocolUsageUnspecifiedReason
    "_" -> fail "failed to parse NetworkAlternateProtocolUsage"
instance ToJSON NetworkAlternateProtocolUsage where
  toJSON v = A.String $ case v of
    NetworkAlternateProtocolUsageAlternativeJobWonWithoutRace -> "alternativeJobWonWithoutRace"
    NetworkAlternateProtocolUsageAlternativeJobWonRace -> "alternativeJobWonRace"
    NetworkAlternateProtocolUsageMainJobWonRace -> "mainJobWonRace"
    NetworkAlternateProtocolUsageMappingMissing -> "mappingMissing"
    NetworkAlternateProtocolUsageBroken -> "broken"
    NetworkAlternateProtocolUsageDnsAlpnH3JobWonWithoutRace -> "dnsAlpnH3JobWonWithoutRace"
    NetworkAlternateProtocolUsageDnsAlpnH3JobWonRace -> "dnsAlpnH3JobWonRace"
    NetworkAlternateProtocolUsageUnspecifiedReason -> "unspecifiedReason"

-- | Type 'Network.Response'.
--   HTTP response data.
data NetworkResponse = NetworkResponse
  {
    -- | Response URL. This URL can be different from CachedResource.url in case of redirect.
    networkResponseUrl :: T.Text,
    -- | HTTP response status code.
    networkResponseStatus :: Int,
    -- | HTTP response status text.
    networkResponseStatusText :: T.Text,
    -- | HTTP response headers.
    networkResponseHeaders :: NetworkHeaders,
    -- | Resource mimeType as determined by the browser.
    networkResponseMimeType :: T.Text,
    -- | Refined HTTP request headers that were actually transmitted over the network.
    networkResponseRequestHeaders :: Maybe NetworkHeaders,
    -- | Specifies whether physical connection was actually reused for this request.
    networkResponseConnectionReused :: Bool,
    -- | Physical connection id that was actually used for this request.
    networkResponseConnectionId :: Double,
    -- | Remote IP address.
    networkResponseRemoteIPAddress :: Maybe T.Text,
    -- | Remote port.
    networkResponseRemotePort :: Maybe Int,
    -- | Specifies that the request was served from the disk cache.
    networkResponseFromDiskCache :: Maybe Bool,
    -- | Specifies that the request was served from the ServiceWorker.
    networkResponseFromServiceWorker :: Maybe Bool,
    -- | Specifies that the request was served from the prefetch cache.
    networkResponseFromPrefetchCache :: Maybe Bool,
    -- | Total number of bytes received for this request so far.
    networkResponseEncodedDataLength :: Double,
    -- | Timing information for the given request.
    networkResponseTiming :: Maybe NetworkResourceTiming,
    -- | Response source of response from ServiceWorker.
    networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
    -- | The time at which the returned response was generated.
    networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
    -- | Cache Storage Cache Name.
    networkResponseCacheStorageCacheName :: Maybe T.Text,
    -- | Protocol used to fetch this request.
    networkResponseProtocol :: Maybe T.Text,
    -- | The reason why Chrome uses a specific transport protocol for HTTP semantics.
    networkResponseAlternateProtocolUsage :: Maybe NetworkAlternateProtocolUsage,
    -- | Security state of the request resource.
    networkResponseSecurityState :: SecuritySecurityState,
    -- | Security details for the request.
    networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
  }
  deriving (Eq, Show)
instance FromJSON NetworkResponse where
  parseJSON = A.withObject "NetworkResponse" $ \o -> NetworkResponse
    <$> o A..: "url"
    <*> o A..: "status"
    <*> o A..: "statusText"
    <*> o A..: "headers"
    <*> o A..: "mimeType"
    <*> o A..:? "requestHeaders"
    <*> o A..: "connectionReused"
    <*> o A..: "connectionId"
    <*> o A..:? "remoteIPAddress"
    <*> o A..:? "remotePort"
    <*> o A..:? "fromDiskCache"
    <*> o A..:? "fromServiceWorker"
    <*> o A..:? "fromPrefetchCache"
    <*> o A..: "encodedDataLength"
    <*> o A..:? "timing"
    <*> o A..:? "serviceWorkerResponseSource"
    <*> o A..:? "responseTime"
    <*> o A..:? "cacheStorageCacheName"
    <*> o A..:? "protocol"
    <*> o A..:? "alternateProtocolUsage"
    <*> o A..: "securityState"
    <*> o A..:? "securityDetails"
instance ToJSON NetworkResponse where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (networkResponseUrl p),
    ("status" A..=) <$> Just (networkResponseStatus p),
    ("statusText" A..=) <$> Just (networkResponseStatusText p),
    ("headers" A..=) <$> Just (networkResponseHeaders p),
    ("mimeType" A..=) <$> Just (networkResponseMimeType p),
    ("requestHeaders" A..=) <$> (networkResponseRequestHeaders p),
    ("connectionReused" A..=) <$> Just (networkResponseConnectionReused p),
    ("connectionId" A..=) <$> Just (networkResponseConnectionId p),
    ("remoteIPAddress" A..=) <$> (networkResponseRemoteIPAddress p),
    ("remotePort" A..=) <$> (networkResponseRemotePort p),
    ("fromDiskCache" A..=) <$> (networkResponseFromDiskCache p),
    ("fromServiceWorker" A..=) <$> (networkResponseFromServiceWorker p),
    ("fromPrefetchCache" A..=) <$> (networkResponseFromPrefetchCache p),
    ("encodedDataLength" A..=) <$> Just (networkResponseEncodedDataLength p),
    ("timing" A..=) <$> (networkResponseTiming p),
    ("serviceWorkerResponseSource" A..=) <$> (networkResponseServiceWorkerResponseSource p),
    ("responseTime" A..=) <$> (networkResponseResponseTime p),
    ("cacheStorageCacheName" A..=) <$> (networkResponseCacheStorageCacheName p),
    ("protocol" A..=) <$> (networkResponseProtocol p),
    ("alternateProtocolUsage" A..=) <$> (networkResponseAlternateProtocolUsage p),
    ("securityState" A..=) <$> Just (networkResponseSecurityState p),
    ("securityDetails" A..=) <$> (networkResponseSecurityDetails p)
    ]

-- | Type 'Network.WebSocketRequest'.
--   WebSocket request data.
data NetworkWebSocketRequest = NetworkWebSocketRequest
  {
    -- | HTTP request headers.
    networkWebSocketRequestHeaders :: NetworkHeaders
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketRequest where
  parseJSON = A.withObject "NetworkWebSocketRequest" $ \o -> NetworkWebSocketRequest
    <$> o A..: "headers"
instance ToJSON NetworkWebSocketRequest where
  toJSON p = A.object $ catMaybes [
    ("headers" A..=) <$> Just (networkWebSocketRequestHeaders p)
    ]

-- | Type 'Network.WebSocketResponse'.
--   WebSocket response data.
data NetworkWebSocketResponse = NetworkWebSocketResponse
  {
    -- | HTTP response status code.
    networkWebSocketResponseStatus :: Int,
    -- | HTTP response status text.
    networkWebSocketResponseStatusText :: T.Text,
    -- | HTTP response headers.
    networkWebSocketResponseHeaders :: NetworkHeaders,
    -- | HTTP response headers text.
    networkWebSocketResponseHeadersText :: Maybe T.Text,
    -- | HTTP request headers.
    networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
    -- | HTTP request headers text.
    networkWebSocketResponseRequestHeadersText :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketResponse where
  parseJSON = A.withObject "NetworkWebSocketResponse" $ \o -> NetworkWebSocketResponse
    <$> o A..: "status"
    <*> o A..: "statusText"
    <*> o A..: "headers"
    <*> o A..:? "headersText"
    <*> o A..:? "requestHeaders"
    <*> o A..:? "requestHeadersText"
instance ToJSON NetworkWebSocketResponse where
  toJSON p = A.object $ catMaybes [
    ("status" A..=) <$> Just (networkWebSocketResponseStatus p),
    ("statusText" A..=) <$> Just (networkWebSocketResponseStatusText p),
    ("headers" A..=) <$> Just (networkWebSocketResponseHeaders p),
    ("headersText" A..=) <$> (networkWebSocketResponseHeadersText p),
    ("requestHeaders" A..=) <$> (networkWebSocketResponseRequestHeaders p),
    ("requestHeadersText" A..=) <$> (networkWebSocketResponseRequestHeadersText p)
    ]

-- | Type 'Network.WebSocketFrame'.
--   WebSocket message data. This represents an entire WebSocket message, not just a fragmented frame as the name suggests.
data NetworkWebSocketFrame = NetworkWebSocketFrame
  {
    -- | WebSocket message opcode.
    networkWebSocketFrameOpcode :: Double,
    -- | WebSocket message mask.
    networkWebSocketFrameMask :: Bool,
    -- | WebSocket message payload data.
    --   If the opcode is 1, this is a text message and payloadData is a UTF-8 string.
    --   If the opcode isn't 1, then payloadData is a base64 encoded string representing binary data.
    networkWebSocketFramePayloadData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketFrame where
  parseJSON = A.withObject "NetworkWebSocketFrame" $ \o -> NetworkWebSocketFrame
    <$> o A..: "opcode"
    <*> o A..: "mask"
    <*> o A..: "payloadData"
instance ToJSON NetworkWebSocketFrame where
  toJSON p = A.object $ catMaybes [
    ("opcode" A..=) <$> Just (networkWebSocketFrameOpcode p),
    ("mask" A..=) <$> Just (networkWebSocketFrameMask p),
    ("payloadData" A..=) <$> Just (networkWebSocketFramePayloadData p)
    ]

-- | Type 'Network.CachedResource'.
--   Information about the cached resource.
data NetworkCachedResource = NetworkCachedResource
  {
    -- | Resource URL. This is the url of the original network request.
    networkCachedResourceUrl :: T.Text,
    -- | Type of this resource.
    networkCachedResourceType :: NetworkResourceType,
    -- | Cached response data.
    networkCachedResourceResponse :: Maybe NetworkResponse,
    -- | Cached response body size.
    networkCachedResourceBodySize :: Double
  }
  deriving (Eq, Show)
instance FromJSON NetworkCachedResource where
  parseJSON = A.withObject "NetworkCachedResource" $ \o -> NetworkCachedResource
    <$> o A..: "url"
    <*> o A..: "type"
    <*> o A..:? "response"
    <*> o A..: "bodySize"
instance ToJSON NetworkCachedResource where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (networkCachedResourceUrl p),
    ("type" A..=) <$> Just (networkCachedResourceType p),
    ("response" A..=) <$> (networkCachedResourceResponse p),
    ("bodySize" A..=) <$> Just (networkCachedResourceBodySize p)
    ]

-- | Type 'Network.Initiator'.
--   Information about the request initiator.
data NetworkInitiatorType = NetworkInitiatorTypeParser | NetworkInitiatorTypeScript | NetworkInitiatorTypePreload | NetworkInitiatorTypeSignedExchange | NetworkInitiatorTypePreflight | NetworkInitiatorTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkInitiatorType where
  parseJSON = A.withText "NetworkInitiatorType" $ \v -> case v of
    "parser" -> pure NetworkInitiatorTypeParser
    "script" -> pure NetworkInitiatorTypeScript
    "preload" -> pure NetworkInitiatorTypePreload
    "SignedExchange" -> pure NetworkInitiatorTypeSignedExchange
    "preflight" -> pure NetworkInitiatorTypePreflight
    "other" -> pure NetworkInitiatorTypeOther
    "_" -> fail "failed to parse NetworkInitiatorType"
instance ToJSON NetworkInitiatorType where
  toJSON v = A.String $ case v of
    NetworkInitiatorTypeParser -> "parser"
    NetworkInitiatorTypeScript -> "script"
    NetworkInitiatorTypePreload -> "preload"
    NetworkInitiatorTypeSignedExchange -> "SignedExchange"
    NetworkInitiatorTypePreflight -> "preflight"
    NetworkInitiatorTypeOther -> "other"
data NetworkInitiator = NetworkInitiator
  {
    -- | Type of this initiator.
    networkInitiatorType :: NetworkInitiatorType,
    -- | Initiator JavaScript stack trace, set for Script only.
    networkInitiatorStack :: Maybe Runtime.RuntimeStackTrace,
    -- | Initiator URL, set for Parser type or for Script type (when script is importing module) or for SignedExchange type.
    networkInitiatorUrl :: Maybe T.Text,
    -- | Initiator line number, set for Parser type or for Script type (when script is importing
    --   module) (0-based).
    networkInitiatorLineNumber :: Maybe Double,
    -- | Initiator column number, set for Parser type or for Script type (when script is importing
    --   module) (0-based).
    networkInitiatorColumnNumber :: Maybe Double,
    -- | Set if another request triggered this request (e.g. preflight).
    networkInitiatorRequestId :: Maybe NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON NetworkInitiator where
  parseJSON = A.withObject "NetworkInitiator" $ \o -> NetworkInitiator
    <$> o A..: "type"
    <*> o A..:? "stack"
    <*> o A..:? "url"
    <*> o A..:? "lineNumber"
    <*> o A..:? "columnNumber"
    <*> o A..:? "requestId"
instance ToJSON NetworkInitiator where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (networkInitiatorType p),
    ("stack" A..=) <$> (networkInitiatorStack p),
    ("url" A..=) <$> (networkInitiatorUrl p),
    ("lineNumber" A..=) <$> (networkInitiatorLineNumber p),
    ("columnNumber" A..=) <$> (networkInitiatorColumnNumber p),
    ("requestId" A..=) <$> (networkInitiatorRequestId p)
    ]

-- | Type 'Network.Cookie'.
--   Cookie object
data NetworkCookie = NetworkCookie
  {
    -- | Cookie name.
    networkCookieName :: T.Text,
    -- | Cookie value.
    networkCookieValue :: T.Text,
    -- | Cookie domain.
    networkCookieDomain :: T.Text,
    -- | Cookie path.
    networkCookiePath :: T.Text,
    -- | Cookie expiration date as the number of seconds since the UNIX epoch.
    networkCookieExpires :: Double,
    -- | Cookie size.
    networkCookieSize :: Int,
    -- | True if cookie is http-only.
    networkCookieHttpOnly :: Bool,
    -- | True if cookie is secure.
    networkCookieSecure :: Bool,
    -- | True in case of session cookie.
    networkCookieSession :: Bool,
    -- | Cookie SameSite type.
    networkCookieSameSite :: Maybe NetworkCookieSameSite,
    -- | Cookie Priority
    networkCookiePriority :: NetworkCookiePriority,
    -- | True if cookie is SameParty.
    networkCookieSameParty :: Bool,
    -- | Cookie source scheme type.
    networkCookieSourceScheme :: NetworkCookieSourceScheme,
    -- | Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
    --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
    --   This is a temporary ability and it will be removed in the future.
    networkCookieSourcePort :: Int,
    -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
    --   of the request to the endpoint that set the cookie.
    networkCookiePartitionKey :: Maybe T.Text,
    -- | True if cookie partition key is opaque.
    networkCookiePartitionKeyOpaque :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkCookie where
  parseJSON = A.withObject "NetworkCookie" $ \o -> NetworkCookie
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..: "domain"
    <*> o A..: "path"
    <*> o A..: "expires"
    <*> o A..: "size"
    <*> o A..: "httpOnly"
    <*> o A..: "secure"
    <*> o A..: "session"
    <*> o A..:? "sameSite"
    <*> o A..: "priority"
    <*> o A..: "sameParty"
    <*> o A..: "sourceScheme"
    <*> o A..: "sourcePort"
    <*> o A..:? "partitionKey"
    <*> o A..:? "partitionKeyOpaque"
instance ToJSON NetworkCookie where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (networkCookieName p),
    ("value" A..=) <$> Just (networkCookieValue p),
    ("domain" A..=) <$> Just (networkCookieDomain p),
    ("path" A..=) <$> Just (networkCookiePath p),
    ("expires" A..=) <$> Just (networkCookieExpires p),
    ("size" A..=) <$> Just (networkCookieSize p),
    ("httpOnly" A..=) <$> Just (networkCookieHttpOnly p),
    ("secure" A..=) <$> Just (networkCookieSecure p),
    ("session" A..=) <$> Just (networkCookieSession p),
    ("sameSite" A..=) <$> (networkCookieSameSite p),
    ("priority" A..=) <$> Just (networkCookiePriority p),
    ("sameParty" A..=) <$> Just (networkCookieSameParty p),
    ("sourceScheme" A..=) <$> Just (networkCookieSourceScheme p),
    ("sourcePort" A..=) <$> Just (networkCookieSourcePort p),
    ("partitionKey" A..=) <$> (networkCookiePartitionKey p),
    ("partitionKeyOpaque" A..=) <$> (networkCookiePartitionKeyOpaque p)
    ]

-- | Type 'Network.SetCookieBlockedReason'.
--   Types of reasons why a cookie may not be stored from a response.
data NetworkSetCookieBlockedReason = NetworkSetCookieBlockedReasonSecureOnly | NetworkSetCookieBlockedReasonSameSiteStrict | NetworkSetCookieBlockedReasonSameSiteLax | NetworkSetCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax | NetworkSetCookieBlockedReasonSameSiteNoneInsecure | NetworkSetCookieBlockedReasonUserPreferences | NetworkSetCookieBlockedReasonSyntaxError | NetworkSetCookieBlockedReasonSchemeNotSupported | NetworkSetCookieBlockedReasonOverwriteSecure | NetworkSetCookieBlockedReasonInvalidDomain | NetworkSetCookieBlockedReasonInvalidPrefix | NetworkSetCookieBlockedReasonUnknownError | NetworkSetCookieBlockedReasonSchemefulSameSiteStrict | NetworkSetCookieBlockedReasonSchemefulSameSiteLax | NetworkSetCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax | NetworkSetCookieBlockedReasonSamePartyFromCrossPartyContext | NetworkSetCookieBlockedReasonSamePartyConflictsWithOtherAttributes | NetworkSetCookieBlockedReasonNameValuePairExceedsMaxSize
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkSetCookieBlockedReason where
  parseJSON = A.withText "NetworkSetCookieBlockedReason" $ \v -> case v of
    "SecureOnly" -> pure NetworkSetCookieBlockedReasonSecureOnly
    "SameSiteStrict" -> pure NetworkSetCookieBlockedReasonSameSiteStrict
    "SameSiteLax" -> pure NetworkSetCookieBlockedReasonSameSiteLax
    "SameSiteUnspecifiedTreatedAsLax" -> pure NetworkSetCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax
    "SameSiteNoneInsecure" -> pure NetworkSetCookieBlockedReasonSameSiteNoneInsecure
    "UserPreferences" -> pure NetworkSetCookieBlockedReasonUserPreferences
    "SyntaxError" -> pure NetworkSetCookieBlockedReasonSyntaxError
    "SchemeNotSupported" -> pure NetworkSetCookieBlockedReasonSchemeNotSupported
    "OverwriteSecure" -> pure NetworkSetCookieBlockedReasonOverwriteSecure
    "InvalidDomain" -> pure NetworkSetCookieBlockedReasonInvalidDomain
    "InvalidPrefix" -> pure NetworkSetCookieBlockedReasonInvalidPrefix
    "UnknownError" -> pure NetworkSetCookieBlockedReasonUnknownError
    "SchemefulSameSiteStrict" -> pure NetworkSetCookieBlockedReasonSchemefulSameSiteStrict
    "SchemefulSameSiteLax" -> pure NetworkSetCookieBlockedReasonSchemefulSameSiteLax
    "SchemefulSameSiteUnspecifiedTreatedAsLax" -> pure NetworkSetCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax
    "SamePartyFromCrossPartyContext" -> pure NetworkSetCookieBlockedReasonSamePartyFromCrossPartyContext
    "SamePartyConflictsWithOtherAttributes" -> pure NetworkSetCookieBlockedReasonSamePartyConflictsWithOtherAttributes
    "NameValuePairExceedsMaxSize" -> pure NetworkSetCookieBlockedReasonNameValuePairExceedsMaxSize
    "_" -> fail "failed to parse NetworkSetCookieBlockedReason"
instance ToJSON NetworkSetCookieBlockedReason where
  toJSON v = A.String $ case v of
    NetworkSetCookieBlockedReasonSecureOnly -> "SecureOnly"
    NetworkSetCookieBlockedReasonSameSiteStrict -> "SameSiteStrict"
    NetworkSetCookieBlockedReasonSameSiteLax -> "SameSiteLax"
    NetworkSetCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax -> "SameSiteUnspecifiedTreatedAsLax"
    NetworkSetCookieBlockedReasonSameSiteNoneInsecure -> "SameSiteNoneInsecure"
    NetworkSetCookieBlockedReasonUserPreferences -> "UserPreferences"
    NetworkSetCookieBlockedReasonSyntaxError -> "SyntaxError"
    NetworkSetCookieBlockedReasonSchemeNotSupported -> "SchemeNotSupported"
    NetworkSetCookieBlockedReasonOverwriteSecure -> "OverwriteSecure"
    NetworkSetCookieBlockedReasonInvalidDomain -> "InvalidDomain"
    NetworkSetCookieBlockedReasonInvalidPrefix -> "InvalidPrefix"
    NetworkSetCookieBlockedReasonUnknownError -> "UnknownError"
    NetworkSetCookieBlockedReasonSchemefulSameSiteStrict -> "SchemefulSameSiteStrict"
    NetworkSetCookieBlockedReasonSchemefulSameSiteLax -> "SchemefulSameSiteLax"
    NetworkSetCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax -> "SchemefulSameSiteUnspecifiedTreatedAsLax"
    NetworkSetCookieBlockedReasonSamePartyFromCrossPartyContext -> "SamePartyFromCrossPartyContext"
    NetworkSetCookieBlockedReasonSamePartyConflictsWithOtherAttributes -> "SamePartyConflictsWithOtherAttributes"
    NetworkSetCookieBlockedReasonNameValuePairExceedsMaxSize -> "NameValuePairExceedsMaxSize"

-- | Type 'Network.CookieBlockedReason'.
--   Types of reasons why a cookie may not be sent with a request.
data NetworkCookieBlockedReason = NetworkCookieBlockedReasonSecureOnly | NetworkCookieBlockedReasonNotOnPath | NetworkCookieBlockedReasonDomainMismatch | NetworkCookieBlockedReasonSameSiteStrict | NetworkCookieBlockedReasonSameSiteLax | NetworkCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax | NetworkCookieBlockedReasonSameSiteNoneInsecure | NetworkCookieBlockedReasonUserPreferences | NetworkCookieBlockedReasonUnknownError | NetworkCookieBlockedReasonSchemefulSameSiteStrict | NetworkCookieBlockedReasonSchemefulSameSiteLax | NetworkCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax | NetworkCookieBlockedReasonSamePartyFromCrossPartyContext | NetworkCookieBlockedReasonNameValuePairExceedsMaxSize
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieBlockedReason where
  parseJSON = A.withText "NetworkCookieBlockedReason" $ \v -> case v of
    "SecureOnly" -> pure NetworkCookieBlockedReasonSecureOnly
    "NotOnPath" -> pure NetworkCookieBlockedReasonNotOnPath
    "DomainMismatch" -> pure NetworkCookieBlockedReasonDomainMismatch
    "SameSiteStrict" -> pure NetworkCookieBlockedReasonSameSiteStrict
    "SameSiteLax" -> pure NetworkCookieBlockedReasonSameSiteLax
    "SameSiteUnspecifiedTreatedAsLax" -> pure NetworkCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax
    "SameSiteNoneInsecure" -> pure NetworkCookieBlockedReasonSameSiteNoneInsecure
    "UserPreferences" -> pure NetworkCookieBlockedReasonUserPreferences
    "UnknownError" -> pure NetworkCookieBlockedReasonUnknownError
    "SchemefulSameSiteStrict" -> pure NetworkCookieBlockedReasonSchemefulSameSiteStrict
    "SchemefulSameSiteLax" -> pure NetworkCookieBlockedReasonSchemefulSameSiteLax
    "SchemefulSameSiteUnspecifiedTreatedAsLax" -> pure NetworkCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax
    "SamePartyFromCrossPartyContext" -> pure NetworkCookieBlockedReasonSamePartyFromCrossPartyContext
    "NameValuePairExceedsMaxSize" -> pure NetworkCookieBlockedReasonNameValuePairExceedsMaxSize
    "_" -> fail "failed to parse NetworkCookieBlockedReason"
instance ToJSON NetworkCookieBlockedReason where
  toJSON v = A.String $ case v of
    NetworkCookieBlockedReasonSecureOnly -> "SecureOnly"
    NetworkCookieBlockedReasonNotOnPath -> "NotOnPath"
    NetworkCookieBlockedReasonDomainMismatch -> "DomainMismatch"
    NetworkCookieBlockedReasonSameSiteStrict -> "SameSiteStrict"
    NetworkCookieBlockedReasonSameSiteLax -> "SameSiteLax"
    NetworkCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax -> "SameSiteUnspecifiedTreatedAsLax"
    NetworkCookieBlockedReasonSameSiteNoneInsecure -> "SameSiteNoneInsecure"
    NetworkCookieBlockedReasonUserPreferences -> "UserPreferences"
    NetworkCookieBlockedReasonUnknownError -> "UnknownError"
    NetworkCookieBlockedReasonSchemefulSameSiteStrict -> "SchemefulSameSiteStrict"
    NetworkCookieBlockedReasonSchemefulSameSiteLax -> "SchemefulSameSiteLax"
    NetworkCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax -> "SchemefulSameSiteUnspecifiedTreatedAsLax"
    NetworkCookieBlockedReasonSamePartyFromCrossPartyContext -> "SamePartyFromCrossPartyContext"
    NetworkCookieBlockedReasonNameValuePairExceedsMaxSize -> "NameValuePairExceedsMaxSize"

-- | Type 'Network.BlockedSetCookieWithReason'.
--   A cookie which was not stored from a response with the corresponding reason.
data NetworkBlockedSetCookieWithReason = NetworkBlockedSetCookieWithReason
  {
    -- | The reason(s) this cookie was blocked.
    networkBlockedSetCookieWithReasonBlockedReasons :: [NetworkSetCookieBlockedReason],
    -- | The string representing this individual cookie as it would appear in the header.
    --   This is not the entire "cookie" or "set-cookie" header which could have multiple cookies.
    networkBlockedSetCookieWithReasonCookieLine :: T.Text,
    -- | The cookie object which represents the cookie which was not stored. It is optional because
    --   sometimes complete cookie information is not available, such as in the case of parsing
    --   errors.
    networkBlockedSetCookieWithReasonCookie :: Maybe NetworkCookie
  }
  deriving (Eq, Show)
instance FromJSON NetworkBlockedSetCookieWithReason where
  parseJSON = A.withObject "NetworkBlockedSetCookieWithReason" $ \o -> NetworkBlockedSetCookieWithReason
    <$> o A..: "blockedReasons"
    <*> o A..: "cookieLine"
    <*> o A..:? "cookie"
instance ToJSON NetworkBlockedSetCookieWithReason where
  toJSON p = A.object $ catMaybes [
    ("blockedReasons" A..=) <$> Just (networkBlockedSetCookieWithReasonBlockedReasons p),
    ("cookieLine" A..=) <$> Just (networkBlockedSetCookieWithReasonCookieLine p),
    ("cookie" A..=) <$> (networkBlockedSetCookieWithReasonCookie p)
    ]

-- | Type 'Network.BlockedCookieWithReason'.
--   A cookie with was not sent with a request with the corresponding reason.
data NetworkBlockedCookieWithReason = NetworkBlockedCookieWithReason
  {
    -- | The reason(s) the cookie was blocked.
    networkBlockedCookieWithReasonBlockedReasons :: [NetworkCookieBlockedReason],
    -- | The cookie object representing the cookie which was not sent.
    networkBlockedCookieWithReasonCookie :: NetworkCookie
  }
  deriving (Eq, Show)
instance FromJSON NetworkBlockedCookieWithReason where
  parseJSON = A.withObject "NetworkBlockedCookieWithReason" $ \o -> NetworkBlockedCookieWithReason
    <$> o A..: "blockedReasons"
    <*> o A..: "cookie"
instance ToJSON NetworkBlockedCookieWithReason where
  toJSON p = A.object $ catMaybes [
    ("blockedReasons" A..=) <$> Just (networkBlockedCookieWithReasonBlockedReasons p),
    ("cookie" A..=) <$> Just (networkBlockedCookieWithReasonCookie p)
    ]

-- | Type 'Network.CookieParam'.
--   Cookie parameter object
data NetworkCookieParam = NetworkCookieParam
  {
    -- | Cookie name.
    networkCookieParamName :: T.Text,
    -- | Cookie value.
    networkCookieParamValue :: T.Text,
    -- | The request-URI to associate with the setting of the cookie. This value can affect the
    --   default domain, path, source port, and source scheme values of the created cookie.
    networkCookieParamUrl :: Maybe T.Text,
    -- | Cookie domain.
    networkCookieParamDomain :: Maybe T.Text,
    -- | Cookie path.
    networkCookieParamPath :: Maybe T.Text,
    -- | True if cookie is secure.
    networkCookieParamSecure :: Maybe Bool,
    -- | True if cookie is http-only.
    networkCookieParamHttpOnly :: Maybe Bool,
    -- | Cookie SameSite type.
    networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
    -- | Cookie expiration date, session cookie if not set
    networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch,
    -- | Cookie Priority.
    networkCookieParamPriority :: Maybe NetworkCookiePriority,
    -- | True if cookie is SameParty.
    networkCookieParamSameParty :: Maybe Bool,
    -- | Cookie source scheme type.
    networkCookieParamSourceScheme :: Maybe NetworkCookieSourceScheme,
    -- | Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
    --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
    --   This is a temporary ability and it will be removed in the future.
    networkCookieParamSourcePort :: Maybe Int,
    -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
    --   of the request to the endpoint that set the cookie.
    --   If not set, the cookie will be set as not partitioned.
    networkCookieParamPartitionKey :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkCookieParam where
  parseJSON = A.withObject "NetworkCookieParam" $ \o -> NetworkCookieParam
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..:? "url"
    <*> o A..:? "domain"
    <*> o A..:? "path"
    <*> o A..:? "secure"
    <*> o A..:? "httpOnly"
    <*> o A..:? "sameSite"
    <*> o A..:? "expires"
    <*> o A..:? "priority"
    <*> o A..:? "sameParty"
    <*> o A..:? "sourceScheme"
    <*> o A..:? "sourcePort"
    <*> o A..:? "partitionKey"
instance ToJSON NetworkCookieParam where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (networkCookieParamName p),
    ("value" A..=) <$> Just (networkCookieParamValue p),
    ("url" A..=) <$> (networkCookieParamUrl p),
    ("domain" A..=) <$> (networkCookieParamDomain p),
    ("path" A..=) <$> (networkCookieParamPath p),
    ("secure" A..=) <$> (networkCookieParamSecure p),
    ("httpOnly" A..=) <$> (networkCookieParamHttpOnly p),
    ("sameSite" A..=) <$> (networkCookieParamSameSite p),
    ("expires" A..=) <$> (networkCookieParamExpires p),
    ("priority" A..=) <$> (networkCookieParamPriority p),
    ("sameParty" A..=) <$> (networkCookieParamSameParty p),
    ("sourceScheme" A..=) <$> (networkCookieParamSourceScheme p),
    ("sourcePort" A..=) <$> (networkCookieParamSourcePort p),
    ("partitionKey" A..=) <$> (networkCookieParamPartitionKey p)
    ]

-- | Type 'Network.AuthChallenge'.
--   Authorization challenge for HTTP status code 401 or 407.
data NetworkAuthChallengeSource = NetworkAuthChallengeSourceServer | NetworkAuthChallengeSourceProxy
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkAuthChallengeSource where
  parseJSON = A.withText "NetworkAuthChallengeSource" $ \v -> case v of
    "Server" -> pure NetworkAuthChallengeSourceServer
    "Proxy" -> pure NetworkAuthChallengeSourceProxy
    "_" -> fail "failed to parse NetworkAuthChallengeSource"
instance ToJSON NetworkAuthChallengeSource where
  toJSON v = A.String $ case v of
    NetworkAuthChallengeSourceServer -> "Server"
    NetworkAuthChallengeSourceProxy -> "Proxy"
data NetworkAuthChallenge = NetworkAuthChallenge
  {
    -- | Source of the authentication challenge.
    networkAuthChallengeSource :: Maybe NetworkAuthChallengeSource,
    -- | Origin of the challenger.
    networkAuthChallengeOrigin :: T.Text,
    -- | The authentication scheme used, such as basic or digest
    networkAuthChallengeScheme :: T.Text,
    -- | The realm of the challenge. May be empty.
    networkAuthChallengeRealm :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkAuthChallenge where
  parseJSON = A.withObject "NetworkAuthChallenge" $ \o -> NetworkAuthChallenge
    <$> o A..:? "source"
    <*> o A..: "origin"
    <*> o A..: "scheme"
    <*> o A..: "realm"
instance ToJSON NetworkAuthChallenge where
  toJSON p = A.object $ catMaybes [
    ("source" A..=) <$> (networkAuthChallengeSource p),
    ("origin" A..=) <$> Just (networkAuthChallengeOrigin p),
    ("scheme" A..=) <$> Just (networkAuthChallengeScheme p),
    ("realm" A..=) <$> Just (networkAuthChallengeRealm p)
    ]

-- | Type 'Network.AuthChallengeResponse'.
--   Response to an AuthChallenge.
data NetworkAuthChallengeResponseResponse = NetworkAuthChallengeResponseResponseDefault | NetworkAuthChallengeResponseResponseCancelAuth | NetworkAuthChallengeResponseResponseProvideCredentials
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkAuthChallengeResponseResponse where
  parseJSON = A.withText "NetworkAuthChallengeResponseResponse" $ \v -> case v of
    "Default" -> pure NetworkAuthChallengeResponseResponseDefault
    "CancelAuth" -> pure NetworkAuthChallengeResponseResponseCancelAuth
    "ProvideCredentials" -> pure NetworkAuthChallengeResponseResponseProvideCredentials
    "_" -> fail "failed to parse NetworkAuthChallengeResponseResponse"
instance ToJSON NetworkAuthChallengeResponseResponse where
  toJSON v = A.String $ case v of
    NetworkAuthChallengeResponseResponseDefault -> "Default"
    NetworkAuthChallengeResponseResponseCancelAuth -> "CancelAuth"
    NetworkAuthChallengeResponseResponseProvideCredentials -> "ProvideCredentials"
data NetworkAuthChallengeResponse = NetworkAuthChallengeResponse
  {
    -- | The decision on what to do in response to the authorization challenge.  Default means
    --   deferring to the default behavior of the net stack, which will likely either the Cancel
    --   authentication or display a popup dialog box.
    networkAuthChallengeResponseResponse :: NetworkAuthChallengeResponseResponse,
    -- | The username to provide, possibly empty. Should only be set if response is
    --   ProvideCredentials.
    networkAuthChallengeResponseUsername :: Maybe T.Text,
    -- | The password to provide, possibly empty. Should only be set if response is
    --   ProvideCredentials.
    networkAuthChallengeResponsePassword :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkAuthChallengeResponse where
  parseJSON = A.withObject "NetworkAuthChallengeResponse" $ \o -> NetworkAuthChallengeResponse
    <$> o A..: "response"
    <*> o A..:? "username"
    <*> o A..:? "password"
instance ToJSON NetworkAuthChallengeResponse where
  toJSON p = A.object $ catMaybes [
    ("response" A..=) <$> Just (networkAuthChallengeResponseResponse p),
    ("username" A..=) <$> (networkAuthChallengeResponseUsername p),
    ("password" A..=) <$> (networkAuthChallengeResponsePassword p)
    ]

-- | Type 'Network.InterceptionStage'.
--   Stages of the interception to begin intercepting. Request will intercept before the request is
--   sent. Response will intercept after the response is received.
data NetworkInterceptionStage = NetworkInterceptionStageRequest | NetworkInterceptionStageHeadersReceived
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkInterceptionStage where
  parseJSON = A.withText "NetworkInterceptionStage" $ \v -> case v of
    "Request" -> pure NetworkInterceptionStageRequest
    "HeadersReceived" -> pure NetworkInterceptionStageHeadersReceived
    "_" -> fail "failed to parse NetworkInterceptionStage"
instance ToJSON NetworkInterceptionStage where
  toJSON v = A.String $ case v of
    NetworkInterceptionStageRequest -> "Request"
    NetworkInterceptionStageHeadersReceived -> "HeadersReceived"

-- | Type 'Network.RequestPattern'.
--   Request pattern for interception.
data NetworkRequestPattern = NetworkRequestPattern
  {
    -- | Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
    --   backslash. Omitting is equivalent to `"*"`.
    networkRequestPatternUrlPattern :: Maybe T.Text,
    -- | If set, only requests for matching resource types will be intercepted.
    networkRequestPatternResourceType :: Maybe NetworkResourceType,
    -- | Stage at which to begin intercepting requests. Default is Request.
    networkRequestPatternInterceptionStage :: Maybe NetworkInterceptionStage
  }
  deriving (Eq, Show)
instance FromJSON NetworkRequestPattern where
  parseJSON = A.withObject "NetworkRequestPattern" $ \o -> NetworkRequestPattern
    <$> o A..:? "urlPattern"
    <*> o A..:? "resourceType"
    <*> o A..:? "interceptionStage"
instance ToJSON NetworkRequestPattern where
  toJSON p = A.object $ catMaybes [
    ("urlPattern" A..=) <$> (networkRequestPatternUrlPattern p),
    ("resourceType" A..=) <$> (networkRequestPatternResourceType p),
    ("interceptionStage" A..=) <$> (networkRequestPatternInterceptionStage p)
    ]

-- | Type 'Network.SignedExchangeSignature'.
--   Information about a signed exchange signature.
--   https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#rfc.section.3.1
data NetworkSignedExchangeSignature = NetworkSignedExchangeSignature
  {
    -- | Signed exchange signature label.
    networkSignedExchangeSignatureLabel :: T.Text,
    -- | The hex string of signed exchange signature.
    networkSignedExchangeSignatureSignature :: T.Text,
    -- | Signed exchange signature integrity.
    networkSignedExchangeSignatureIntegrity :: T.Text,
    -- | Signed exchange signature cert Url.
    networkSignedExchangeSignatureCertUrl :: Maybe T.Text,
    -- | The hex string of signed exchange signature cert sha256.
    networkSignedExchangeSignatureCertSha256 :: Maybe T.Text,
    -- | Signed exchange signature validity Url.
    networkSignedExchangeSignatureValidityUrl :: T.Text,
    -- | Signed exchange signature date.
    networkSignedExchangeSignatureDate :: Int,
    -- | Signed exchange signature expires.
    networkSignedExchangeSignatureExpires :: Int,
    -- | The encoded certificates.
    networkSignedExchangeSignatureCertificates :: Maybe [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedExchangeSignature where
  parseJSON = A.withObject "NetworkSignedExchangeSignature" $ \o -> NetworkSignedExchangeSignature
    <$> o A..: "label"
    <*> o A..: "signature"
    <*> o A..: "integrity"
    <*> o A..:? "certUrl"
    <*> o A..:? "certSha256"
    <*> o A..: "validityUrl"
    <*> o A..: "date"
    <*> o A..: "expires"
    <*> o A..:? "certificates"
instance ToJSON NetworkSignedExchangeSignature where
  toJSON p = A.object $ catMaybes [
    ("label" A..=) <$> Just (networkSignedExchangeSignatureLabel p),
    ("signature" A..=) <$> Just (networkSignedExchangeSignatureSignature p),
    ("integrity" A..=) <$> Just (networkSignedExchangeSignatureIntegrity p),
    ("certUrl" A..=) <$> (networkSignedExchangeSignatureCertUrl p),
    ("certSha256" A..=) <$> (networkSignedExchangeSignatureCertSha256 p),
    ("validityUrl" A..=) <$> Just (networkSignedExchangeSignatureValidityUrl p),
    ("date" A..=) <$> Just (networkSignedExchangeSignatureDate p),
    ("expires" A..=) <$> Just (networkSignedExchangeSignatureExpires p),
    ("certificates" A..=) <$> (networkSignedExchangeSignatureCertificates p)
    ]

-- | Type 'Network.SignedExchangeHeader'.
--   Information about a signed exchange header.
--   https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#cbor-representation
data NetworkSignedExchangeHeader = NetworkSignedExchangeHeader
  {
    -- | Signed exchange request URL.
    networkSignedExchangeHeaderRequestUrl :: T.Text,
    -- | Signed exchange response code.
    networkSignedExchangeHeaderResponseCode :: Int,
    -- | Signed exchange response headers.
    networkSignedExchangeHeaderResponseHeaders :: NetworkHeaders,
    -- | Signed exchange response signature.
    networkSignedExchangeHeaderSignatures :: [NetworkSignedExchangeSignature],
    -- | Signed exchange header integrity hash in the form of "sha256-<base64-hash-value>".
    networkSignedExchangeHeaderHeaderIntegrity :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedExchangeHeader where
  parseJSON = A.withObject "NetworkSignedExchangeHeader" $ \o -> NetworkSignedExchangeHeader
    <$> o A..: "requestUrl"
    <*> o A..: "responseCode"
    <*> o A..: "responseHeaders"
    <*> o A..: "signatures"
    <*> o A..: "headerIntegrity"
instance ToJSON NetworkSignedExchangeHeader where
  toJSON p = A.object $ catMaybes [
    ("requestUrl" A..=) <$> Just (networkSignedExchangeHeaderRequestUrl p),
    ("responseCode" A..=) <$> Just (networkSignedExchangeHeaderResponseCode p),
    ("responseHeaders" A..=) <$> Just (networkSignedExchangeHeaderResponseHeaders p),
    ("signatures" A..=) <$> Just (networkSignedExchangeHeaderSignatures p),
    ("headerIntegrity" A..=) <$> Just (networkSignedExchangeHeaderHeaderIntegrity p)
    ]

-- | Type 'Network.SignedExchangeErrorField'.
--   Field type for a signed exchange related error.
data NetworkSignedExchangeErrorField = NetworkSignedExchangeErrorFieldSignatureSig | NetworkSignedExchangeErrorFieldSignatureIntegrity | NetworkSignedExchangeErrorFieldSignatureCertUrl | NetworkSignedExchangeErrorFieldSignatureCertSha256 | NetworkSignedExchangeErrorFieldSignatureValidityUrl | NetworkSignedExchangeErrorFieldSignatureTimestamps
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkSignedExchangeErrorField where
  parseJSON = A.withText "NetworkSignedExchangeErrorField" $ \v -> case v of
    "signatureSig" -> pure NetworkSignedExchangeErrorFieldSignatureSig
    "signatureIntegrity" -> pure NetworkSignedExchangeErrorFieldSignatureIntegrity
    "signatureCertUrl" -> pure NetworkSignedExchangeErrorFieldSignatureCertUrl
    "signatureCertSha256" -> pure NetworkSignedExchangeErrorFieldSignatureCertSha256
    "signatureValidityUrl" -> pure NetworkSignedExchangeErrorFieldSignatureValidityUrl
    "signatureTimestamps" -> pure NetworkSignedExchangeErrorFieldSignatureTimestamps
    "_" -> fail "failed to parse NetworkSignedExchangeErrorField"
instance ToJSON NetworkSignedExchangeErrorField where
  toJSON v = A.String $ case v of
    NetworkSignedExchangeErrorFieldSignatureSig -> "signatureSig"
    NetworkSignedExchangeErrorFieldSignatureIntegrity -> "signatureIntegrity"
    NetworkSignedExchangeErrorFieldSignatureCertUrl -> "signatureCertUrl"
    NetworkSignedExchangeErrorFieldSignatureCertSha256 -> "signatureCertSha256"
    NetworkSignedExchangeErrorFieldSignatureValidityUrl -> "signatureValidityUrl"
    NetworkSignedExchangeErrorFieldSignatureTimestamps -> "signatureTimestamps"

-- | Type 'Network.SignedExchangeError'.
--   Information about a signed exchange response.
data NetworkSignedExchangeError = NetworkSignedExchangeError
  {
    -- | Error message.
    networkSignedExchangeErrorMessage :: T.Text,
    -- | The index of the signature which caused the error.
    networkSignedExchangeErrorSignatureIndex :: Maybe Int,
    -- | The field which caused the error.
    networkSignedExchangeErrorErrorField :: Maybe NetworkSignedExchangeErrorField
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedExchangeError where
  parseJSON = A.withObject "NetworkSignedExchangeError" $ \o -> NetworkSignedExchangeError
    <$> o A..: "message"
    <*> o A..:? "signatureIndex"
    <*> o A..:? "errorField"
instance ToJSON NetworkSignedExchangeError where
  toJSON p = A.object $ catMaybes [
    ("message" A..=) <$> Just (networkSignedExchangeErrorMessage p),
    ("signatureIndex" A..=) <$> (networkSignedExchangeErrorSignatureIndex p),
    ("errorField" A..=) <$> (networkSignedExchangeErrorErrorField p)
    ]

-- | Type 'Network.SignedExchangeInfo'.
--   Information about a signed exchange response.
data NetworkSignedExchangeInfo = NetworkSignedExchangeInfo
  {
    -- | The outer response of signed HTTP exchange which was received from network.
    networkSignedExchangeInfoOuterResponse :: NetworkResponse,
    -- | Information about the signed exchange header.
    networkSignedExchangeInfoHeader :: Maybe NetworkSignedExchangeHeader,
    -- | Security details for the signed exchange header.
    networkSignedExchangeInfoSecurityDetails :: Maybe NetworkSecurityDetails,
    -- | Errors occurred while handling the signed exchagne.
    networkSignedExchangeInfoErrors :: Maybe [NetworkSignedExchangeError]
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedExchangeInfo where
  parseJSON = A.withObject "NetworkSignedExchangeInfo" $ \o -> NetworkSignedExchangeInfo
    <$> o A..: "outerResponse"
    <*> o A..:? "header"
    <*> o A..:? "securityDetails"
    <*> o A..:? "errors"
instance ToJSON NetworkSignedExchangeInfo where
  toJSON p = A.object $ catMaybes [
    ("outerResponse" A..=) <$> Just (networkSignedExchangeInfoOuterResponse p),
    ("header" A..=) <$> (networkSignedExchangeInfoHeader p),
    ("securityDetails" A..=) <$> (networkSignedExchangeInfoSecurityDetails p),
    ("errors" A..=) <$> (networkSignedExchangeInfoErrors p)
    ]

-- | Type 'Network.ContentEncoding'.
--   List of content encodings supported by the backend.
data NetworkContentEncoding = NetworkContentEncodingDeflate | NetworkContentEncodingGzip | NetworkContentEncodingBr
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkContentEncoding where
  parseJSON = A.withText "NetworkContentEncoding" $ \v -> case v of
    "deflate" -> pure NetworkContentEncodingDeflate
    "gzip" -> pure NetworkContentEncodingGzip
    "br" -> pure NetworkContentEncodingBr
    "_" -> fail "failed to parse NetworkContentEncoding"
instance ToJSON NetworkContentEncoding where
  toJSON v = A.String $ case v of
    NetworkContentEncodingDeflate -> "deflate"
    NetworkContentEncodingGzip -> "gzip"
    NetworkContentEncodingBr -> "br"

-- | Type 'Network.PrivateNetworkRequestPolicy'.
data NetworkPrivateNetworkRequestPolicy = NetworkPrivateNetworkRequestPolicyAllow | NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate | NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate | NetworkPrivateNetworkRequestPolicyPreflightBlock | NetworkPrivateNetworkRequestPolicyPreflightWarn
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkPrivateNetworkRequestPolicy where
  parseJSON = A.withText "NetworkPrivateNetworkRequestPolicy" $ \v -> case v of
    "Allow" -> pure NetworkPrivateNetworkRequestPolicyAllow
    "BlockFromInsecureToMorePrivate" -> pure NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate
    "WarnFromInsecureToMorePrivate" -> pure NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate
    "PreflightBlock" -> pure NetworkPrivateNetworkRequestPolicyPreflightBlock
    "PreflightWarn" -> pure NetworkPrivateNetworkRequestPolicyPreflightWarn
    "_" -> fail "failed to parse NetworkPrivateNetworkRequestPolicy"
instance ToJSON NetworkPrivateNetworkRequestPolicy where
  toJSON v = A.String $ case v of
    NetworkPrivateNetworkRequestPolicyAllow -> "Allow"
    NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate -> "BlockFromInsecureToMorePrivate"
    NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate -> "WarnFromInsecureToMorePrivate"
    NetworkPrivateNetworkRequestPolicyPreflightBlock -> "PreflightBlock"
    NetworkPrivateNetworkRequestPolicyPreflightWarn -> "PreflightWarn"

-- | Type 'Network.IPAddressSpace'.
data NetworkIPAddressSpace = NetworkIPAddressSpaceLocal | NetworkIPAddressSpacePrivate | NetworkIPAddressSpacePublic | NetworkIPAddressSpaceUnknown
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkIPAddressSpace where
  parseJSON = A.withText "NetworkIPAddressSpace" $ \v -> case v of
    "Local" -> pure NetworkIPAddressSpaceLocal
    "Private" -> pure NetworkIPAddressSpacePrivate
    "Public" -> pure NetworkIPAddressSpacePublic
    "Unknown" -> pure NetworkIPAddressSpaceUnknown
    "_" -> fail "failed to parse NetworkIPAddressSpace"
instance ToJSON NetworkIPAddressSpace where
  toJSON v = A.String $ case v of
    NetworkIPAddressSpaceLocal -> "Local"
    NetworkIPAddressSpacePrivate -> "Private"
    NetworkIPAddressSpacePublic -> "Public"
    NetworkIPAddressSpaceUnknown -> "Unknown"

-- | Type 'Network.ConnectTiming'.
data NetworkConnectTiming = NetworkConnectTiming
  {
    -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
    --   milliseconds relatively to this requestTime. Matches ResourceTiming's requestTime for
    --   the same request (but not for redirected requests).
    networkConnectTimingRequestTime :: Double
  }
  deriving (Eq, Show)
instance FromJSON NetworkConnectTiming where
  parseJSON = A.withObject "NetworkConnectTiming" $ \o -> NetworkConnectTiming
    <$> o A..: "requestTime"
instance ToJSON NetworkConnectTiming where
  toJSON p = A.object $ catMaybes [
    ("requestTime" A..=) <$> Just (networkConnectTimingRequestTime p)
    ]

-- | Type 'Network.ClientSecurityState'.
data NetworkClientSecurityState = NetworkClientSecurityState
  {
    networkClientSecurityStateInitiatorIsSecureContext :: Bool,
    networkClientSecurityStateInitiatorIPAddressSpace :: NetworkIPAddressSpace,
    networkClientSecurityStatePrivateNetworkRequestPolicy :: NetworkPrivateNetworkRequestPolicy
  }
  deriving (Eq, Show)
instance FromJSON NetworkClientSecurityState where
  parseJSON = A.withObject "NetworkClientSecurityState" $ \o -> NetworkClientSecurityState
    <$> o A..: "initiatorIsSecureContext"
    <*> o A..: "initiatorIPAddressSpace"
    <*> o A..: "privateNetworkRequestPolicy"
instance ToJSON NetworkClientSecurityState where
  toJSON p = A.object $ catMaybes [
    ("initiatorIsSecureContext" A..=) <$> Just (networkClientSecurityStateInitiatorIsSecureContext p),
    ("initiatorIPAddressSpace" A..=) <$> Just (networkClientSecurityStateInitiatorIPAddressSpace p),
    ("privateNetworkRequestPolicy" A..=) <$> Just (networkClientSecurityStatePrivateNetworkRequestPolicy p)
    ]

-- | Type 'Network.CrossOriginOpenerPolicyValue'.
data NetworkCrossOriginOpenerPolicyValue = NetworkCrossOriginOpenerPolicyValueSameOrigin | NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups | NetworkCrossOriginOpenerPolicyValueRestrictProperties | NetworkCrossOriginOpenerPolicyValueUnsafeNone | NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep | NetworkCrossOriginOpenerPolicyValueRestrictPropertiesPlusCoep
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCrossOriginOpenerPolicyValue where
  parseJSON = A.withText "NetworkCrossOriginOpenerPolicyValue" $ \v -> case v of
    "SameOrigin" -> pure NetworkCrossOriginOpenerPolicyValueSameOrigin
    "SameOriginAllowPopups" -> pure NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups
    "RestrictProperties" -> pure NetworkCrossOriginOpenerPolicyValueRestrictProperties
    "UnsafeNone" -> pure NetworkCrossOriginOpenerPolicyValueUnsafeNone
    "SameOriginPlusCoep" -> pure NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep
    "RestrictPropertiesPlusCoep" -> pure NetworkCrossOriginOpenerPolicyValueRestrictPropertiesPlusCoep
    "_" -> fail "failed to parse NetworkCrossOriginOpenerPolicyValue"
instance ToJSON NetworkCrossOriginOpenerPolicyValue where
  toJSON v = A.String $ case v of
    NetworkCrossOriginOpenerPolicyValueSameOrigin -> "SameOrigin"
    NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups -> "SameOriginAllowPopups"
    NetworkCrossOriginOpenerPolicyValueRestrictProperties -> "RestrictProperties"
    NetworkCrossOriginOpenerPolicyValueUnsafeNone -> "UnsafeNone"
    NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep -> "SameOriginPlusCoep"
    NetworkCrossOriginOpenerPolicyValueRestrictPropertiesPlusCoep -> "RestrictPropertiesPlusCoep"

-- | Type 'Network.CrossOriginOpenerPolicyStatus'.
data NetworkCrossOriginOpenerPolicyStatus = NetworkCrossOriginOpenerPolicyStatus
  {
    networkCrossOriginOpenerPolicyStatusValue :: NetworkCrossOriginOpenerPolicyValue,
    networkCrossOriginOpenerPolicyStatusReportOnlyValue :: NetworkCrossOriginOpenerPolicyValue,
    networkCrossOriginOpenerPolicyStatusReportingEndpoint :: Maybe T.Text,
    networkCrossOriginOpenerPolicyStatusReportOnlyReportingEndpoint :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkCrossOriginOpenerPolicyStatus where
  parseJSON = A.withObject "NetworkCrossOriginOpenerPolicyStatus" $ \o -> NetworkCrossOriginOpenerPolicyStatus
    <$> o A..: "value"
    <*> o A..: "reportOnlyValue"
    <*> o A..:? "reportingEndpoint"
    <*> o A..:? "reportOnlyReportingEndpoint"
instance ToJSON NetworkCrossOriginOpenerPolicyStatus where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (networkCrossOriginOpenerPolicyStatusValue p),
    ("reportOnlyValue" A..=) <$> Just (networkCrossOriginOpenerPolicyStatusReportOnlyValue p),
    ("reportingEndpoint" A..=) <$> (networkCrossOriginOpenerPolicyStatusReportingEndpoint p),
    ("reportOnlyReportingEndpoint" A..=) <$> (networkCrossOriginOpenerPolicyStatusReportOnlyReportingEndpoint p)
    ]

-- | Type 'Network.CrossOriginEmbedderPolicyValue'.
data NetworkCrossOriginEmbedderPolicyValue = NetworkCrossOriginEmbedderPolicyValueNone | NetworkCrossOriginEmbedderPolicyValueCredentialless | NetworkCrossOriginEmbedderPolicyValueRequireCorp
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCrossOriginEmbedderPolicyValue where
  parseJSON = A.withText "NetworkCrossOriginEmbedderPolicyValue" $ \v -> case v of
    "None" -> pure NetworkCrossOriginEmbedderPolicyValueNone
    "Credentialless" -> pure NetworkCrossOriginEmbedderPolicyValueCredentialless
    "RequireCorp" -> pure NetworkCrossOriginEmbedderPolicyValueRequireCorp
    "_" -> fail "failed to parse NetworkCrossOriginEmbedderPolicyValue"
instance ToJSON NetworkCrossOriginEmbedderPolicyValue where
  toJSON v = A.String $ case v of
    NetworkCrossOriginEmbedderPolicyValueNone -> "None"
    NetworkCrossOriginEmbedderPolicyValueCredentialless -> "Credentialless"
    NetworkCrossOriginEmbedderPolicyValueRequireCorp -> "RequireCorp"

-- | Type 'Network.CrossOriginEmbedderPolicyStatus'.
data NetworkCrossOriginEmbedderPolicyStatus = NetworkCrossOriginEmbedderPolicyStatus
  {
    networkCrossOriginEmbedderPolicyStatusValue :: NetworkCrossOriginEmbedderPolicyValue,
    networkCrossOriginEmbedderPolicyStatusReportOnlyValue :: NetworkCrossOriginEmbedderPolicyValue,
    networkCrossOriginEmbedderPolicyStatusReportingEndpoint :: Maybe T.Text,
    networkCrossOriginEmbedderPolicyStatusReportOnlyReportingEndpoint :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkCrossOriginEmbedderPolicyStatus where
  parseJSON = A.withObject "NetworkCrossOriginEmbedderPolicyStatus" $ \o -> NetworkCrossOriginEmbedderPolicyStatus
    <$> o A..: "value"
    <*> o A..: "reportOnlyValue"
    <*> o A..:? "reportingEndpoint"
    <*> o A..:? "reportOnlyReportingEndpoint"
instance ToJSON NetworkCrossOriginEmbedderPolicyStatus where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (networkCrossOriginEmbedderPolicyStatusValue p),
    ("reportOnlyValue" A..=) <$> Just (networkCrossOriginEmbedderPolicyStatusReportOnlyValue p),
    ("reportingEndpoint" A..=) <$> (networkCrossOriginEmbedderPolicyStatusReportingEndpoint p),
    ("reportOnlyReportingEndpoint" A..=) <$> (networkCrossOriginEmbedderPolicyStatusReportOnlyReportingEndpoint p)
    ]

-- | Type 'Network.SecurityIsolationStatus'.
data NetworkSecurityIsolationStatus = NetworkSecurityIsolationStatus
  {
    networkSecurityIsolationStatusCoop :: Maybe NetworkCrossOriginOpenerPolicyStatus,
    networkSecurityIsolationStatusCoep :: Maybe NetworkCrossOriginEmbedderPolicyStatus
  }
  deriving (Eq, Show)
instance FromJSON NetworkSecurityIsolationStatus where
  parseJSON = A.withObject "NetworkSecurityIsolationStatus" $ \o -> NetworkSecurityIsolationStatus
    <$> o A..:? "coop"
    <*> o A..:? "coep"
instance ToJSON NetworkSecurityIsolationStatus where
  toJSON p = A.object $ catMaybes [
    ("coop" A..=) <$> (networkSecurityIsolationStatusCoop p),
    ("coep" A..=) <$> (networkSecurityIsolationStatusCoep p)
    ]

-- | Type 'Network.ReportStatus'.
--   The status of a Reporting API report.
data NetworkReportStatus = NetworkReportStatusQueued | NetworkReportStatusPending | NetworkReportStatusMarkedForRemoval | NetworkReportStatusSuccess
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkReportStatus where
  parseJSON = A.withText "NetworkReportStatus" $ \v -> case v of
    "Queued" -> pure NetworkReportStatusQueued
    "Pending" -> pure NetworkReportStatusPending
    "MarkedForRemoval" -> pure NetworkReportStatusMarkedForRemoval
    "Success" -> pure NetworkReportStatusSuccess
    "_" -> fail "failed to parse NetworkReportStatus"
instance ToJSON NetworkReportStatus where
  toJSON v = A.String $ case v of
    NetworkReportStatusQueued -> "Queued"
    NetworkReportStatusPending -> "Pending"
    NetworkReportStatusMarkedForRemoval -> "MarkedForRemoval"
    NetworkReportStatusSuccess -> "Success"

-- | Type 'Network.ReportId'.
type NetworkReportId = T.Text

-- | Type 'Network.ReportingApiReport'.
--   An object representing a report generated by the Reporting API.
data NetworkReportingApiReport = NetworkReportingApiReport
  {
    networkReportingApiReportId :: NetworkReportId,
    -- | The URL of the document that triggered the report.
    networkReportingApiReportInitiatorUrl :: T.Text,
    -- | The name of the endpoint group that should be used to deliver the report.
    networkReportingApiReportDestination :: T.Text,
    -- | The type of the report (specifies the set of data that is contained in the report body).
    networkReportingApiReportType :: T.Text,
    -- | When the report was generated.
    networkReportingApiReportTimestamp :: NetworkTimeSinceEpoch,
    -- | How many uploads deep the related request was.
    networkReportingApiReportDepth :: Int,
    -- | The number of delivery attempts made so far, not including an active attempt.
    networkReportingApiReportCompletedAttempts :: Int,
    networkReportingApiReportBody :: [(T.Text, T.Text)],
    networkReportingApiReportStatus :: NetworkReportStatus
  }
  deriving (Eq, Show)
instance FromJSON NetworkReportingApiReport where
  parseJSON = A.withObject "NetworkReportingApiReport" $ \o -> NetworkReportingApiReport
    <$> o A..: "id"
    <*> o A..: "initiatorUrl"
    <*> o A..: "destination"
    <*> o A..: "type"
    <*> o A..: "timestamp"
    <*> o A..: "depth"
    <*> o A..: "completedAttempts"
    <*> o A..: "body"
    <*> o A..: "status"
instance ToJSON NetworkReportingApiReport where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (networkReportingApiReportId p),
    ("initiatorUrl" A..=) <$> Just (networkReportingApiReportInitiatorUrl p),
    ("destination" A..=) <$> Just (networkReportingApiReportDestination p),
    ("type" A..=) <$> Just (networkReportingApiReportType p),
    ("timestamp" A..=) <$> Just (networkReportingApiReportTimestamp p),
    ("depth" A..=) <$> Just (networkReportingApiReportDepth p),
    ("completedAttempts" A..=) <$> Just (networkReportingApiReportCompletedAttempts p),
    ("body" A..=) <$> Just (networkReportingApiReportBody p),
    ("status" A..=) <$> Just (networkReportingApiReportStatus p)
    ]

-- | Type 'Network.ReportingApiEndpoint'.
data NetworkReportingApiEndpoint = NetworkReportingApiEndpoint
  {
    -- | The URL of the endpoint to which reports may be delivered.
    networkReportingApiEndpointUrl :: T.Text,
    -- | Name of the endpoint group.
    networkReportingApiEndpointGroupName :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkReportingApiEndpoint where
  parseJSON = A.withObject "NetworkReportingApiEndpoint" $ \o -> NetworkReportingApiEndpoint
    <$> o A..: "url"
    <*> o A..: "groupName"
instance ToJSON NetworkReportingApiEndpoint where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (networkReportingApiEndpointUrl p),
    ("groupName" A..=) <$> Just (networkReportingApiEndpointGroupName p)
    ]

-- | Type 'Network.LoadNetworkResourcePageResult'.
--   An object providing the result of a network resource load.
data NetworkLoadNetworkResourcePageResult = NetworkLoadNetworkResourcePageResult
  {
    networkLoadNetworkResourcePageResultSuccess :: Bool,
    -- | Optional values used for error reporting.
    networkLoadNetworkResourcePageResultNetError :: Maybe Double,
    networkLoadNetworkResourcePageResultNetErrorName :: Maybe T.Text,
    networkLoadNetworkResourcePageResultHttpStatusCode :: Maybe Double,
    -- | If successful, one of the following two fields holds the result.
    networkLoadNetworkResourcePageResultStream :: Maybe IO.IOStreamHandle,
    -- | Response headers.
    networkLoadNetworkResourcePageResultHeaders :: Maybe NetworkHeaders
  }
  deriving (Eq, Show)
instance FromJSON NetworkLoadNetworkResourcePageResult where
  parseJSON = A.withObject "NetworkLoadNetworkResourcePageResult" $ \o -> NetworkLoadNetworkResourcePageResult
    <$> o A..: "success"
    <*> o A..:? "netError"
    <*> o A..:? "netErrorName"
    <*> o A..:? "httpStatusCode"
    <*> o A..:? "stream"
    <*> o A..:? "headers"
instance ToJSON NetworkLoadNetworkResourcePageResult where
  toJSON p = A.object $ catMaybes [
    ("success" A..=) <$> Just (networkLoadNetworkResourcePageResultSuccess p),
    ("netError" A..=) <$> (networkLoadNetworkResourcePageResultNetError p),
    ("netErrorName" A..=) <$> (networkLoadNetworkResourcePageResultNetErrorName p),
    ("httpStatusCode" A..=) <$> (networkLoadNetworkResourcePageResultHttpStatusCode p),
    ("stream" A..=) <$> (networkLoadNetworkResourcePageResultStream p),
    ("headers" A..=) <$> (networkLoadNetworkResourcePageResultHeaders p)
    ]

-- | Type 'Network.LoadNetworkResourceOptions'.
--   An options object that may be extended later to better support CORS,
--   CORB and streaming.
data NetworkLoadNetworkResourceOptions = NetworkLoadNetworkResourceOptions
  {
    networkLoadNetworkResourceOptionsDisableCache :: Bool,
    networkLoadNetworkResourceOptionsIncludeCredentials :: Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkLoadNetworkResourceOptions where
  parseJSON = A.withObject "NetworkLoadNetworkResourceOptions" $ \o -> NetworkLoadNetworkResourceOptions
    <$> o A..: "disableCache"
    <*> o A..: "includeCredentials"
instance ToJSON NetworkLoadNetworkResourceOptions where
  toJSON p = A.object $ catMaybes [
    ("disableCache" A..=) <$> Just (networkLoadNetworkResourceOptionsDisableCache p),
    ("includeCredentials" A..=) <$> Just (networkLoadNetworkResourceOptionsIncludeCredentials p)
    ]

-- | Type of the 'Network.dataReceived' event.
data NetworkDataReceived = NetworkDataReceived
  {
    -- | Request identifier.
    networkDataReceivedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkDataReceivedTimestamp :: NetworkMonotonicTime,
    -- | Data chunk length.
    networkDataReceivedDataLength :: Int,
    -- | Actual bytes received (might be less than dataLength for compressed encodings).
    networkDataReceivedEncodedDataLength :: Int
  }
  deriving (Eq, Show)
instance FromJSON NetworkDataReceived where
  parseJSON = A.withObject "NetworkDataReceived" $ \o -> NetworkDataReceived
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "dataLength"
    <*> o A..: "encodedDataLength"
instance Event NetworkDataReceived where
  eventName _ = "Network.dataReceived"

-- | Type of the 'Network.eventSourceMessageReceived' event.
data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived
  {
    -- | Request identifier.
    networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
    -- | Message type.
    networkEventSourceMessageReceivedEventName :: T.Text,
    -- | Message identifier.
    networkEventSourceMessageReceivedEventId :: T.Text,
    -- | Message content.
    networkEventSourceMessageReceivedData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkEventSourceMessageReceived where
  parseJSON = A.withObject "NetworkEventSourceMessageReceived" $ \o -> NetworkEventSourceMessageReceived
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "eventName"
    <*> o A..: "eventId"
    <*> o A..: "data"
instance Event NetworkEventSourceMessageReceived where
  eventName _ = "Network.eventSourceMessageReceived"

-- | Type of the 'Network.loadingFailed' event.
data NetworkLoadingFailed = NetworkLoadingFailed
  {
    -- | Request identifier.
    networkLoadingFailedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkLoadingFailedTimestamp :: NetworkMonotonicTime,
    -- | Resource type.
    networkLoadingFailedType :: NetworkResourceType,
    -- | User friendly error message.
    networkLoadingFailedErrorText :: T.Text,
    -- | True if loading was canceled.
    networkLoadingFailedCanceled :: Maybe Bool,
    -- | The reason why loading was blocked, if any.
    networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
    -- | The reason why loading was blocked by CORS, if any.
    networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
  }
  deriving (Eq, Show)
instance FromJSON NetworkLoadingFailed where
  parseJSON = A.withObject "NetworkLoadingFailed" $ \o -> NetworkLoadingFailed
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "type"
    <*> o A..: "errorText"
    <*> o A..:? "canceled"
    <*> o A..:? "blockedReason"
    <*> o A..:? "corsErrorStatus"
instance Event NetworkLoadingFailed where
  eventName _ = "Network.loadingFailed"

-- | Type of the 'Network.loadingFinished' event.
data NetworkLoadingFinished = NetworkLoadingFinished
  {
    -- | Request identifier.
    networkLoadingFinishedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
    -- | Total number of bytes received for this request.
    networkLoadingFinishedEncodedDataLength :: Double,
    -- | Set when 1) response was blocked by Cross-Origin Read Blocking and also
    --   2) this needs to be reported to the DevTools console.
    networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkLoadingFinished where
  parseJSON = A.withObject "NetworkLoadingFinished" $ \o -> NetworkLoadingFinished
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "encodedDataLength"
    <*> o A..:? "shouldReportCorbBlocking"
instance Event NetworkLoadingFinished where
  eventName _ = "Network.loadingFinished"

-- | Type of the 'Network.requestServedFromCache' event.
data NetworkRequestServedFromCache = NetworkRequestServedFromCache
  {
    -- | Request identifier.
    networkRequestServedFromCacheRequestId :: NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON NetworkRequestServedFromCache where
  parseJSON = A.withObject "NetworkRequestServedFromCache" $ \o -> NetworkRequestServedFromCache
    <$> o A..: "requestId"
instance Event NetworkRequestServedFromCache where
  eventName _ = "Network.requestServedFromCache"

-- | Type of the 'Network.requestWillBeSent' event.
data NetworkRequestWillBeSent = NetworkRequestWillBeSent
  {
    -- | Request identifier.
    networkRequestWillBeSentRequestId :: NetworkRequestId,
    -- | Loader identifier. Empty string if the request is fetched from worker.
    networkRequestWillBeSentLoaderId :: NetworkLoaderId,
    -- | URL of the document this request is loaded for.
    networkRequestWillBeSentDocumentURL :: T.Text,
    -- | Request data.
    networkRequestWillBeSentRequest :: NetworkRequest,
    -- | Timestamp.
    networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
    -- | Timestamp.
    networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
    -- | Request initiator.
    networkRequestWillBeSentInitiator :: NetworkInitiator,
    -- | In the case that redirectResponse is populated, this flag indicates whether
    --   requestWillBeSentExtraInfo and responseReceivedExtraInfo events will be or were emitted
    --   for the request which was just redirected.
    networkRequestWillBeSentRedirectHasExtraInfo :: Bool,
    -- | Redirect response data.
    networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
    -- | Type of this resource.
    networkRequestWillBeSentType :: Maybe NetworkResourceType,
    -- | Frame identifier.
    networkRequestWillBeSentFrameId :: Maybe PageFrameId,
    -- | Whether the request is initiated by a user gesture. Defaults to false.
    networkRequestWillBeSentHasUserGesture :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkRequestWillBeSent where
  parseJSON = A.withObject "NetworkRequestWillBeSent" $ \o -> NetworkRequestWillBeSent
    <$> o A..: "requestId"
    <*> o A..: "loaderId"
    <*> o A..: "documentURL"
    <*> o A..: "request"
    <*> o A..: "timestamp"
    <*> o A..: "wallTime"
    <*> o A..: "initiator"
    <*> o A..: "redirectHasExtraInfo"
    <*> o A..:? "redirectResponse"
    <*> o A..:? "type"
    <*> o A..:? "frameId"
    <*> o A..:? "hasUserGesture"
instance Event NetworkRequestWillBeSent where
  eventName _ = "Network.requestWillBeSent"

-- | Type of the 'Network.resourceChangedPriority' event.
data NetworkResourceChangedPriority = NetworkResourceChangedPriority
  {
    -- | Request identifier.
    networkResourceChangedPriorityRequestId :: NetworkRequestId,
    -- | New priority
    networkResourceChangedPriorityNewPriority :: NetworkResourcePriority,
    -- | Timestamp.
    networkResourceChangedPriorityTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON NetworkResourceChangedPriority where
  parseJSON = A.withObject "NetworkResourceChangedPriority" $ \o -> NetworkResourceChangedPriority
    <$> o A..: "requestId"
    <*> o A..: "newPriority"
    <*> o A..: "timestamp"
instance Event NetworkResourceChangedPriority where
  eventName _ = "Network.resourceChangedPriority"

-- | Type of the 'Network.signedExchangeReceived' event.
data NetworkSignedExchangeReceived = NetworkSignedExchangeReceived
  {
    -- | Request identifier.
    networkSignedExchangeReceivedRequestId :: NetworkRequestId,
    -- | Information about the signed exchange response.
    networkSignedExchangeReceivedInfo :: NetworkSignedExchangeInfo
  }
  deriving (Eq, Show)
instance FromJSON NetworkSignedExchangeReceived where
  parseJSON = A.withObject "NetworkSignedExchangeReceived" $ \o -> NetworkSignedExchangeReceived
    <$> o A..: "requestId"
    <*> o A..: "info"
instance Event NetworkSignedExchangeReceived where
  eventName _ = "Network.signedExchangeReceived"

-- | Type of the 'Network.responseReceived' event.
data NetworkResponseReceived = NetworkResponseReceived
  {
    -- | Request identifier.
    networkResponseReceivedRequestId :: NetworkRequestId,
    -- | Loader identifier. Empty string if the request is fetched from worker.
    networkResponseReceivedLoaderId :: NetworkLoaderId,
    -- | Timestamp.
    networkResponseReceivedTimestamp :: NetworkMonotonicTime,
    -- | Resource type.
    networkResponseReceivedType :: NetworkResourceType,
    -- | Response data.
    networkResponseReceivedResponse :: NetworkResponse,
    -- | Indicates whether requestWillBeSentExtraInfo and responseReceivedExtraInfo events will be
    --   or were emitted for this request.
    networkResponseReceivedHasExtraInfo :: Bool,
    -- | Frame identifier.
    networkResponseReceivedFrameId :: Maybe PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON NetworkResponseReceived where
  parseJSON = A.withObject "NetworkResponseReceived" $ \o -> NetworkResponseReceived
    <$> o A..: "requestId"
    <*> o A..: "loaderId"
    <*> o A..: "timestamp"
    <*> o A..: "type"
    <*> o A..: "response"
    <*> o A..: "hasExtraInfo"
    <*> o A..:? "frameId"
instance Event NetworkResponseReceived where
  eventName _ = "Network.responseReceived"

-- | Type of the 'Network.webSocketClosed' event.
data NetworkWebSocketClosed = NetworkWebSocketClosed
  {
    -- | Request identifier.
    networkWebSocketClosedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketClosedTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketClosed where
  parseJSON = A.withObject "NetworkWebSocketClosed" $ \o -> NetworkWebSocketClosed
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
instance Event NetworkWebSocketClosed where
  eventName _ = "Network.webSocketClosed"

-- | Type of the 'Network.webSocketCreated' event.
data NetworkWebSocketCreated = NetworkWebSocketCreated
  {
    -- | Request identifier.
    networkWebSocketCreatedRequestId :: NetworkRequestId,
    -- | WebSocket request URL.
    networkWebSocketCreatedUrl :: T.Text,
    -- | Request initiator.
    networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketCreated where
  parseJSON = A.withObject "NetworkWebSocketCreated" $ \o -> NetworkWebSocketCreated
    <$> o A..: "requestId"
    <*> o A..: "url"
    <*> o A..:? "initiator"
instance Event NetworkWebSocketCreated where
  eventName _ = "Network.webSocketCreated"

-- | Type of the 'Network.webSocketFrameError' event.
data NetworkWebSocketFrameError = NetworkWebSocketFrameError
  {
    -- | Request identifier.
    networkWebSocketFrameErrorRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
    -- | WebSocket error message.
    networkWebSocketFrameErrorErrorMessage :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketFrameError where
  parseJSON = A.withObject "NetworkWebSocketFrameError" $ \o -> NetworkWebSocketFrameError
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "errorMessage"
instance Event NetworkWebSocketFrameError where
  eventName _ = "Network.webSocketFrameError"

-- | Type of the 'Network.webSocketFrameReceived' event.
data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived
  {
    -- | Request identifier.
    networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
    -- | WebSocket response data.
    networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketFrameReceived where
  parseJSON = A.withObject "NetworkWebSocketFrameReceived" $ \o -> NetworkWebSocketFrameReceived
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "response"
instance Event NetworkWebSocketFrameReceived where
  eventName _ = "Network.webSocketFrameReceived"

-- | Type of the 'Network.webSocketFrameSent' event.
data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent
  {
    -- | Request identifier.
    networkWebSocketFrameSentRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
    -- | WebSocket response data.
    networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketFrameSent where
  parseJSON = A.withObject "NetworkWebSocketFrameSent" $ \o -> NetworkWebSocketFrameSent
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "response"
instance Event NetworkWebSocketFrameSent where
  eventName _ = "Network.webSocketFrameSent"

-- | Type of the 'Network.webSocketHandshakeResponseReceived' event.
data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived
  {
    -- | Request identifier.
    networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
    -- | WebSocket response data.
    networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketHandshakeResponseReceived where
  parseJSON = A.withObject "NetworkWebSocketHandshakeResponseReceived" $ \o -> NetworkWebSocketHandshakeResponseReceived
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "response"
instance Event NetworkWebSocketHandshakeResponseReceived where
  eventName _ = "Network.webSocketHandshakeResponseReceived"

-- | Type of the 'Network.webSocketWillSendHandshakeRequest' event.
data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest
  {
    -- | Request identifier.
    networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
    -- | Timestamp.
    networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
    -- | UTC Timestamp.
    networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
    -- | WebSocket request data.
    networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebSocketWillSendHandshakeRequest where
  parseJSON = A.withObject "NetworkWebSocketWillSendHandshakeRequest" $ \o -> NetworkWebSocketWillSendHandshakeRequest
    <$> o A..: "requestId"
    <*> o A..: "timestamp"
    <*> o A..: "wallTime"
    <*> o A..: "request"
instance Event NetworkWebSocketWillSendHandshakeRequest where
  eventName _ = "Network.webSocketWillSendHandshakeRequest"

-- | Type of the 'Network.webTransportCreated' event.
data NetworkWebTransportCreated = NetworkWebTransportCreated
  {
    -- | WebTransport identifier.
    networkWebTransportCreatedTransportId :: NetworkRequestId,
    -- | WebTransport request URL.
    networkWebTransportCreatedUrl :: T.Text,
    -- | Timestamp.
    networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
    -- | Request initiator.
    networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebTransportCreated where
  parseJSON = A.withObject "NetworkWebTransportCreated" $ \o -> NetworkWebTransportCreated
    <$> o A..: "transportId"
    <*> o A..: "url"
    <*> o A..: "timestamp"
    <*> o A..:? "initiator"
instance Event NetworkWebTransportCreated where
  eventName _ = "Network.webTransportCreated"

-- | Type of the 'Network.webTransportConnectionEstablished' event.
data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished
  {
    -- | WebTransport identifier.
    networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
    -- | Timestamp.
    networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebTransportConnectionEstablished where
  parseJSON = A.withObject "NetworkWebTransportConnectionEstablished" $ \o -> NetworkWebTransportConnectionEstablished
    <$> o A..: "transportId"
    <*> o A..: "timestamp"
instance Event NetworkWebTransportConnectionEstablished where
  eventName _ = "Network.webTransportConnectionEstablished"

-- | Type of the 'Network.webTransportClosed' event.
data NetworkWebTransportClosed = NetworkWebTransportClosed
  {
    -- | WebTransport identifier.
    networkWebTransportClosedTransportId :: NetworkRequestId,
    -- | Timestamp.
    networkWebTransportClosedTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON NetworkWebTransportClosed where
  parseJSON = A.withObject "NetworkWebTransportClosed" $ \o -> NetworkWebTransportClosed
    <$> o A..: "transportId"
    <*> o A..: "timestamp"
instance Event NetworkWebTransportClosed where
  eventName _ = "Network.webTransportClosed"

-- | Type of the 'Network.requestWillBeSentExtraInfo' event.
data NetworkRequestWillBeSentExtraInfo = NetworkRequestWillBeSentExtraInfo
  {
    -- | Request identifier. Used to match this information to an existing requestWillBeSent event.
    networkRequestWillBeSentExtraInfoRequestId :: NetworkRequestId,
    -- | A list of cookies potentially associated to the requested URL. This includes both cookies sent with
    --   the request and the ones not sent; the latter are distinguished by having blockedReason field set.
    networkRequestWillBeSentExtraInfoAssociatedCookies :: [NetworkBlockedCookieWithReason],
    -- | Raw request headers as they will be sent over the wire.
    networkRequestWillBeSentExtraInfoHeaders :: NetworkHeaders,
    -- | Connection timing information for the request.
    networkRequestWillBeSentExtraInfoConnectTiming :: NetworkConnectTiming,
    -- | The client security state set for the request.
    networkRequestWillBeSentExtraInfoClientSecurityState :: Maybe NetworkClientSecurityState
  }
  deriving (Eq, Show)
instance FromJSON NetworkRequestWillBeSentExtraInfo where
  parseJSON = A.withObject "NetworkRequestWillBeSentExtraInfo" $ \o -> NetworkRequestWillBeSentExtraInfo
    <$> o A..: "requestId"
    <*> o A..: "associatedCookies"
    <*> o A..: "headers"
    <*> o A..: "connectTiming"
    <*> o A..:? "clientSecurityState"
instance Event NetworkRequestWillBeSentExtraInfo where
  eventName _ = "Network.requestWillBeSentExtraInfo"

-- | Type of the 'Network.responseReceivedExtraInfo' event.
data NetworkResponseReceivedExtraInfo = NetworkResponseReceivedExtraInfo
  {
    -- | Request identifier. Used to match this information to another responseReceived event.
    networkResponseReceivedExtraInfoRequestId :: NetworkRequestId,
    -- | A list of cookies which were not stored from the response along with the corresponding
    --   reasons for blocking. The cookies here may not be valid due to syntax errors, which
    --   are represented by the invalid cookie line string instead of a proper cookie.
    networkResponseReceivedExtraInfoBlockedCookies :: [NetworkBlockedSetCookieWithReason],
    -- | Raw response headers as they were received over the wire.
    networkResponseReceivedExtraInfoHeaders :: NetworkHeaders,
    -- | The IP address space of the resource. The address space can only be determined once the transport
    --   established the connection, so we can't send it in `requestWillBeSentExtraInfo`.
    networkResponseReceivedExtraInfoResourceIPAddressSpace :: NetworkIPAddressSpace,
    -- | The status code of the response. This is useful in cases the request failed and no responseReceived
    --   event is triggered, which is the case for, e.g., CORS errors. This is also the correct status code
    --   for cached requests, where the status in responseReceived is a 200 and this will be 304.
    networkResponseReceivedExtraInfoStatusCode :: Int,
    -- | Raw response header text as it was received over the wire. The raw text may not always be
    --   available, such as in the case of HTTP/2 or QUIC.
    networkResponseReceivedExtraInfoHeadersText :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkResponseReceivedExtraInfo where
  parseJSON = A.withObject "NetworkResponseReceivedExtraInfo" $ \o -> NetworkResponseReceivedExtraInfo
    <$> o A..: "requestId"
    <*> o A..: "blockedCookies"
    <*> o A..: "headers"
    <*> o A..: "resourceIPAddressSpace"
    <*> o A..: "statusCode"
    <*> o A..:? "headersText"
instance Event NetworkResponseReceivedExtraInfo where
  eventName _ = "Network.responseReceivedExtraInfo"

-- | Type of the 'Network.trustTokenOperationDone' event.
data NetworkTrustTokenOperationDoneStatus = NetworkTrustTokenOperationDoneStatusOk | NetworkTrustTokenOperationDoneStatusInvalidArgument | NetworkTrustTokenOperationDoneStatusFailedPrecondition | NetworkTrustTokenOperationDoneStatusResourceExhausted | NetworkTrustTokenOperationDoneStatusAlreadyExists | NetworkTrustTokenOperationDoneStatusUnavailable | NetworkTrustTokenOperationDoneStatusBadResponse | NetworkTrustTokenOperationDoneStatusInternalError | NetworkTrustTokenOperationDoneStatusUnknownError | NetworkTrustTokenOperationDoneStatusFulfilledLocally
  deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenOperationDoneStatus where
  parseJSON = A.withText "NetworkTrustTokenOperationDoneStatus" $ \v -> case v of
    "Ok" -> pure NetworkTrustTokenOperationDoneStatusOk
    "InvalidArgument" -> pure NetworkTrustTokenOperationDoneStatusInvalidArgument
    "FailedPrecondition" -> pure NetworkTrustTokenOperationDoneStatusFailedPrecondition
    "ResourceExhausted" -> pure NetworkTrustTokenOperationDoneStatusResourceExhausted
    "AlreadyExists" -> pure NetworkTrustTokenOperationDoneStatusAlreadyExists
    "Unavailable" -> pure NetworkTrustTokenOperationDoneStatusUnavailable
    "BadResponse" -> pure NetworkTrustTokenOperationDoneStatusBadResponse
    "InternalError" -> pure NetworkTrustTokenOperationDoneStatusInternalError
    "UnknownError" -> pure NetworkTrustTokenOperationDoneStatusUnknownError
    "FulfilledLocally" -> pure NetworkTrustTokenOperationDoneStatusFulfilledLocally
    "_" -> fail "failed to parse NetworkTrustTokenOperationDoneStatus"
instance ToJSON NetworkTrustTokenOperationDoneStatus where
  toJSON v = A.String $ case v of
    NetworkTrustTokenOperationDoneStatusOk -> "Ok"
    NetworkTrustTokenOperationDoneStatusInvalidArgument -> "InvalidArgument"
    NetworkTrustTokenOperationDoneStatusFailedPrecondition -> "FailedPrecondition"
    NetworkTrustTokenOperationDoneStatusResourceExhausted -> "ResourceExhausted"
    NetworkTrustTokenOperationDoneStatusAlreadyExists -> "AlreadyExists"
    NetworkTrustTokenOperationDoneStatusUnavailable -> "Unavailable"
    NetworkTrustTokenOperationDoneStatusBadResponse -> "BadResponse"
    NetworkTrustTokenOperationDoneStatusInternalError -> "InternalError"
    NetworkTrustTokenOperationDoneStatusUnknownError -> "UnknownError"
    NetworkTrustTokenOperationDoneStatusFulfilledLocally -> "FulfilledLocally"
data NetworkTrustTokenOperationDone = NetworkTrustTokenOperationDone
  {
    -- | Detailed success or error status of the operation.
    --   'AlreadyExists' also signifies a successful operation, as the result
    --   of the operation already exists und thus, the operation was abort
    --   preemptively (e.g. a cache hit).
    networkTrustTokenOperationDoneStatus :: NetworkTrustTokenOperationDoneStatus,
    networkTrustTokenOperationDoneType :: NetworkTrustTokenOperationType,
    networkTrustTokenOperationDoneRequestId :: NetworkRequestId,
    -- | Top level origin. The context in which the operation was attempted.
    networkTrustTokenOperationDoneTopLevelOrigin :: Maybe T.Text,
    -- | Origin of the issuer in case of a "Issuance" or "Redemption" operation.
    networkTrustTokenOperationDoneIssuerOrigin :: Maybe T.Text,
    -- | The number of obtained Trust Tokens on a successful "Issuance" operation.
    networkTrustTokenOperationDoneIssuedTokenCount :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON NetworkTrustTokenOperationDone where
  parseJSON = A.withObject "NetworkTrustTokenOperationDone" $ \o -> NetworkTrustTokenOperationDone
    <$> o A..: "status"
    <*> o A..: "type"
    <*> o A..: "requestId"
    <*> o A..:? "topLevelOrigin"
    <*> o A..:? "issuerOrigin"
    <*> o A..:? "issuedTokenCount"
instance Event NetworkTrustTokenOperationDone where
  eventName _ = "Network.trustTokenOperationDone"

-- | Type of the 'Network.subresourceWebBundleMetadataReceived' event.
data NetworkSubresourceWebBundleMetadataReceived = NetworkSubresourceWebBundleMetadataReceived
  {
    -- | Request identifier. Used to match this information to another event.
    networkSubresourceWebBundleMetadataReceivedRequestId :: NetworkRequestId,
    -- | A list of URLs of resources in the subresource Web Bundle.
    networkSubresourceWebBundleMetadataReceivedUrls :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON NetworkSubresourceWebBundleMetadataReceived where
  parseJSON = A.withObject "NetworkSubresourceWebBundleMetadataReceived" $ \o -> NetworkSubresourceWebBundleMetadataReceived
    <$> o A..: "requestId"
    <*> o A..: "urls"
instance Event NetworkSubresourceWebBundleMetadataReceived where
  eventName _ = "Network.subresourceWebBundleMetadataReceived"

-- | Type of the 'Network.subresourceWebBundleMetadataError' event.
data NetworkSubresourceWebBundleMetadataError = NetworkSubresourceWebBundleMetadataError
  {
    -- | Request identifier. Used to match this information to another event.
    networkSubresourceWebBundleMetadataErrorRequestId :: NetworkRequestId,
    -- | Error message
    networkSubresourceWebBundleMetadataErrorErrorMessage :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkSubresourceWebBundleMetadataError where
  parseJSON = A.withObject "NetworkSubresourceWebBundleMetadataError" $ \o -> NetworkSubresourceWebBundleMetadataError
    <$> o A..: "requestId"
    <*> o A..: "errorMessage"
instance Event NetworkSubresourceWebBundleMetadataError where
  eventName _ = "Network.subresourceWebBundleMetadataError"

-- | Type of the 'Network.subresourceWebBundleInnerResponseParsed' event.
data NetworkSubresourceWebBundleInnerResponseParsed = NetworkSubresourceWebBundleInnerResponseParsed
  {
    -- | Request identifier of the subresource request
    networkSubresourceWebBundleInnerResponseParsedInnerRequestId :: NetworkRequestId,
    -- | URL of the subresource resource.
    networkSubresourceWebBundleInnerResponseParsedInnerRequestURL :: T.Text,
    -- | Bundle request identifier. Used to match this information to another event.
    --   This made be absent in case when the instrumentation was enabled only
    --   after webbundle was parsed.
    networkSubresourceWebBundleInnerResponseParsedBundleRequestId :: Maybe NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON NetworkSubresourceWebBundleInnerResponseParsed where
  parseJSON = A.withObject "NetworkSubresourceWebBundleInnerResponseParsed" $ \o -> NetworkSubresourceWebBundleInnerResponseParsed
    <$> o A..: "innerRequestId"
    <*> o A..: "innerRequestURL"
    <*> o A..:? "bundleRequestId"
instance Event NetworkSubresourceWebBundleInnerResponseParsed where
  eventName _ = "Network.subresourceWebBundleInnerResponseParsed"

-- | Type of the 'Network.subresourceWebBundleInnerResponseError' event.
data NetworkSubresourceWebBundleInnerResponseError = NetworkSubresourceWebBundleInnerResponseError
  {
    -- | Request identifier of the subresource request
    networkSubresourceWebBundleInnerResponseErrorInnerRequestId :: NetworkRequestId,
    -- | URL of the subresource resource.
    networkSubresourceWebBundleInnerResponseErrorInnerRequestURL :: T.Text,
    -- | Error message
    networkSubresourceWebBundleInnerResponseErrorErrorMessage :: T.Text,
    -- | Bundle request identifier. Used to match this information to another event.
    --   This made be absent in case when the instrumentation was enabled only
    --   after webbundle was parsed.
    networkSubresourceWebBundleInnerResponseErrorBundleRequestId :: Maybe NetworkRequestId
  }
  deriving (Eq, Show)
instance FromJSON NetworkSubresourceWebBundleInnerResponseError where
  parseJSON = A.withObject "NetworkSubresourceWebBundleInnerResponseError" $ \o -> NetworkSubresourceWebBundleInnerResponseError
    <$> o A..: "innerRequestId"
    <*> o A..: "innerRequestURL"
    <*> o A..: "errorMessage"
    <*> o A..:? "bundleRequestId"
instance Event NetworkSubresourceWebBundleInnerResponseError where
  eventName _ = "Network.subresourceWebBundleInnerResponseError"

-- | Type of the 'Network.reportingApiReportAdded' event.
data NetworkReportingApiReportAdded = NetworkReportingApiReportAdded
  {
    networkReportingApiReportAddedReport :: NetworkReportingApiReport
  }
  deriving (Eq, Show)
instance FromJSON NetworkReportingApiReportAdded where
  parseJSON = A.withObject "NetworkReportingApiReportAdded" $ \o -> NetworkReportingApiReportAdded
    <$> o A..: "report"
instance Event NetworkReportingApiReportAdded where
  eventName _ = "Network.reportingApiReportAdded"

-- | Type of the 'Network.reportingApiReportUpdated' event.
data NetworkReportingApiReportUpdated = NetworkReportingApiReportUpdated
  {
    networkReportingApiReportUpdatedReport :: NetworkReportingApiReport
  }
  deriving (Eq, Show)
instance FromJSON NetworkReportingApiReportUpdated where
  parseJSON = A.withObject "NetworkReportingApiReportUpdated" $ \o -> NetworkReportingApiReportUpdated
    <$> o A..: "report"
instance Event NetworkReportingApiReportUpdated where
  eventName _ = "Network.reportingApiReportUpdated"

-- | Type of the 'Network.reportingApiEndpointsChangedForOrigin' event.
data NetworkReportingApiEndpointsChangedForOrigin = NetworkReportingApiEndpointsChangedForOrigin
  {
    -- | Origin of the document(s) which configured the endpoints.
    networkReportingApiEndpointsChangedForOriginOrigin :: T.Text,
    networkReportingApiEndpointsChangedForOriginEndpoints :: [NetworkReportingApiEndpoint]
  }
  deriving (Eq, Show)
instance FromJSON NetworkReportingApiEndpointsChangedForOrigin where
  parseJSON = A.withObject "NetworkReportingApiEndpointsChangedForOrigin" $ \o -> NetworkReportingApiEndpointsChangedForOrigin
    <$> o A..: "origin"
    <*> o A..: "endpoints"
instance Event NetworkReportingApiEndpointsChangedForOrigin where
  eventName _ = "Network.reportingApiEndpointsChangedForOrigin"

-- | Sets a list of content encodings that will be accepted. Empty list means no encoding is accepted.

-- | Parameters of the 'Network.setAcceptedEncodings' command.
data PNetworkSetAcceptedEncodings = PNetworkSetAcceptedEncodings
  {
    -- | List of accepted content encodings.
    pNetworkSetAcceptedEncodingsEncodings :: [NetworkContentEncoding]
  }
  deriving (Eq, Show)
pNetworkSetAcceptedEncodings
  -- | List of accepted content encodings.
  :: [NetworkContentEncoding]
  -> PNetworkSetAcceptedEncodings
pNetworkSetAcceptedEncodings
  arg_pNetworkSetAcceptedEncodingsEncodings
  = PNetworkSetAcceptedEncodings
    arg_pNetworkSetAcceptedEncodingsEncodings
instance ToJSON PNetworkSetAcceptedEncodings where
  toJSON p = A.object $ catMaybes [
    ("encodings" A..=) <$> Just (pNetworkSetAcceptedEncodingsEncodings p)
    ]
instance Command PNetworkSetAcceptedEncodings where
  type CommandResponse PNetworkSetAcceptedEncodings = ()
  commandName _ = "Network.setAcceptedEncodings"
  fromJSON = const . A.Success . const ()

-- | Clears accepted encodings set by setAcceptedEncodings

-- | Parameters of the 'Network.clearAcceptedEncodingsOverride' command.
data PNetworkClearAcceptedEncodingsOverride = PNetworkClearAcceptedEncodingsOverride
  deriving (Eq, Show)
pNetworkClearAcceptedEncodingsOverride
  :: PNetworkClearAcceptedEncodingsOverride
pNetworkClearAcceptedEncodingsOverride
  = PNetworkClearAcceptedEncodingsOverride
instance ToJSON PNetworkClearAcceptedEncodingsOverride where
  toJSON _ = A.Null
instance Command PNetworkClearAcceptedEncodingsOverride where
  type CommandResponse PNetworkClearAcceptedEncodingsOverride = ()
  commandName _ = "Network.clearAcceptedEncodingsOverride"
  fromJSON = const . A.Success . const ()

-- | Clears browser cache.

-- | Parameters of the 'Network.clearBrowserCache' command.
data PNetworkClearBrowserCache = PNetworkClearBrowserCache
  deriving (Eq, Show)
pNetworkClearBrowserCache
  :: PNetworkClearBrowserCache
pNetworkClearBrowserCache
  = PNetworkClearBrowserCache
instance ToJSON PNetworkClearBrowserCache where
  toJSON _ = A.Null
instance Command PNetworkClearBrowserCache where
  type CommandResponse PNetworkClearBrowserCache = ()
  commandName _ = "Network.clearBrowserCache"
  fromJSON = const . A.Success . const ()

-- | Clears browser cookies.

-- | Parameters of the 'Network.clearBrowserCookies' command.
data PNetworkClearBrowserCookies = PNetworkClearBrowserCookies
  deriving (Eq, Show)
pNetworkClearBrowserCookies
  :: PNetworkClearBrowserCookies
pNetworkClearBrowserCookies
  = PNetworkClearBrowserCookies
instance ToJSON PNetworkClearBrowserCookies where
  toJSON _ = A.Null
instance Command PNetworkClearBrowserCookies where
  type CommandResponse PNetworkClearBrowserCookies = ()
  commandName _ = "Network.clearBrowserCookies"
  fromJSON = const . A.Success . const ()

-- | Deletes browser cookies with matching name and url or domain/path pair.

-- | Parameters of the 'Network.deleteCookies' command.
data PNetworkDeleteCookies = PNetworkDeleteCookies
  {
    -- | Name of the cookies to remove.
    pNetworkDeleteCookiesName :: T.Text,
    -- | If specified, deletes all the cookies with the given name where domain and path match
    --   provided URL.
    pNetworkDeleteCookiesUrl :: Maybe T.Text,
    -- | If specified, deletes only cookies with the exact domain.
    pNetworkDeleteCookiesDomain :: Maybe T.Text,
    -- | If specified, deletes only cookies with the exact path.
    pNetworkDeleteCookiesPath :: Maybe T.Text
  }
  deriving (Eq, Show)
pNetworkDeleteCookies
  -- | Name of the cookies to remove.
  :: T.Text
  -> PNetworkDeleteCookies
pNetworkDeleteCookies
  arg_pNetworkDeleteCookiesName
  = PNetworkDeleteCookies
    arg_pNetworkDeleteCookiesName
    Nothing
    Nothing
    Nothing
instance ToJSON PNetworkDeleteCookies where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pNetworkDeleteCookiesName p),
    ("url" A..=) <$> (pNetworkDeleteCookiesUrl p),
    ("domain" A..=) <$> (pNetworkDeleteCookiesDomain p),
    ("path" A..=) <$> (pNetworkDeleteCookiesPath p)
    ]
instance Command PNetworkDeleteCookies where
  type CommandResponse PNetworkDeleteCookies = ()
  commandName _ = "Network.deleteCookies"
  fromJSON = const . A.Success . const ()

-- | Disables network tracking, prevents network events from being sent to the client.

-- | Parameters of the 'Network.disable' command.
data PNetworkDisable = PNetworkDisable
  deriving (Eq, Show)
pNetworkDisable
  :: PNetworkDisable
pNetworkDisable
  = PNetworkDisable
instance ToJSON PNetworkDisable where
  toJSON _ = A.Null
instance Command PNetworkDisable where
  type CommandResponse PNetworkDisable = ()
  commandName _ = "Network.disable"
  fromJSON = const . A.Success . const ()

-- | Activates emulation of network conditions.

-- | Parameters of the 'Network.emulateNetworkConditions' command.
data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions
  {
    -- | True to emulate internet disconnection.
    pNetworkEmulateNetworkConditionsOffline :: Bool,
    -- | Minimum latency from request sent to response headers received (ms).
    pNetworkEmulateNetworkConditionsLatency :: Double,
    -- | Maximal aggregated download throughput (bytes/sec). -1 disables download throttling.
    pNetworkEmulateNetworkConditionsDownloadThroughput :: Double,
    -- | Maximal aggregated upload throughput (bytes/sec).  -1 disables upload throttling.
    pNetworkEmulateNetworkConditionsUploadThroughput :: Double,
    -- | Connection type if known.
    pNetworkEmulateNetworkConditionsConnectionType :: Maybe NetworkConnectionType
  }
  deriving (Eq, Show)
pNetworkEmulateNetworkConditions
  -- | True to emulate internet disconnection.
  :: Bool
  -- | Minimum latency from request sent to response headers received (ms).
  -> Double
  -- | Maximal aggregated download throughput (bytes/sec). -1 disables download throttling.
  -> Double
  -- | Maximal aggregated upload throughput (bytes/sec).  -1 disables upload throttling.
  -> Double
  -> PNetworkEmulateNetworkConditions
pNetworkEmulateNetworkConditions
  arg_pNetworkEmulateNetworkConditionsOffline
  arg_pNetworkEmulateNetworkConditionsLatency
  arg_pNetworkEmulateNetworkConditionsDownloadThroughput
  arg_pNetworkEmulateNetworkConditionsUploadThroughput
  = PNetworkEmulateNetworkConditions
    arg_pNetworkEmulateNetworkConditionsOffline
    arg_pNetworkEmulateNetworkConditionsLatency
    arg_pNetworkEmulateNetworkConditionsDownloadThroughput
    arg_pNetworkEmulateNetworkConditionsUploadThroughput
    Nothing
instance ToJSON PNetworkEmulateNetworkConditions where
  toJSON p = A.object $ catMaybes [
    ("offline" A..=) <$> Just (pNetworkEmulateNetworkConditionsOffline p),
    ("latency" A..=) <$> Just (pNetworkEmulateNetworkConditionsLatency p),
    ("downloadThroughput" A..=) <$> Just (pNetworkEmulateNetworkConditionsDownloadThroughput p),
    ("uploadThroughput" A..=) <$> Just (pNetworkEmulateNetworkConditionsUploadThroughput p),
    ("connectionType" A..=) <$> (pNetworkEmulateNetworkConditionsConnectionType p)
    ]
instance Command PNetworkEmulateNetworkConditions where
  type CommandResponse PNetworkEmulateNetworkConditions = ()
  commandName _ = "Network.emulateNetworkConditions"
  fromJSON = const . A.Success . const ()

-- | Enables network tracking, network events will now be delivered to the client.

-- | Parameters of the 'Network.enable' command.
data PNetworkEnable = PNetworkEnable
  {
    -- | Buffer size in bytes to use when preserving network payloads (XHRs, etc).
    pNetworkEnableMaxTotalBufferSize :: Maybe Int,
    -- | Per-resource buffer size in bytes to use when preserving network payloads (XHRs, etc).
    pNetworkEnableMaxResourceBufferSize :: Maybe Int,
    -- | Longest post body size (in bytes) that would be included in requestWillBeSent notification
    pNetworkEnableMaxPostDataSize :: Maybe Int
  }
  deriving (Eq, Show)
pNetworkEnable
  :: PNetworkEnable
pNetworkEnable
  = PNetworkEnable
    Nothing
    Nothing
    Nothing
instance ToJSON PNetworkEnable where
  toJSON p = A.object $ catMaybes [
    ("maxTotalBufferSize" A..=) <$> (pNetworkEnableMaxTotalBufferSize p),
    ("maxResourceBufferSize" A..=) <$> (pNetworkEnableMaxResourceBufferSize p),
    ("maxPostDataSize" A..=) <$> (pNetworkEnableMaxPostDataSize p)
    ]
instance Command PNetworkEnable where
  type CommandResponse PNetworkEnable = ()
  commandName _ = "Network.enable"
  fromJSON = const . A.Success . const ()

-- | Returns all browser cookies. Depending on the backend support, will return detailed cookie
--   information in the `cookies` field.

-- | Parameters of the 'Network.getAllCookies' command.
data PNetworkGetAllCookies = PNetworkGetAllCookies
  deriving (Eq, Show)
pNetworkGetAllCookies
  :: PNetworkGetAllCookies
pNetworkGetAllCookies
  = PNetworkGetAllCookies
instance ToJSON PNetworkGetAllCookies where
  toJSON _ = A.Null
data NetworkGetAllCookies = NetworkGetAllCookies
  {
    -- | Array of cookie objects.
    networkGetAllCookiesCookies :: [NetworkCookie]
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetAllCookies where
  parseJSON = A.withObject "NetworkGetAllCookies" $ \o -> NetworkGetAllCookies
    <$> o A..: "cookies"
instance Command PNetworkGetAllCookies where
  type CommandResponse PNetworkGetAllCookies = NetworkGetAllCookies
  commandName _ = "Network.getAllCookies"

-- | Returns the DER-encoded certificate.

-- | Parameters of the 'Network.getCertificate' command.
data PNetworkGetCertificate = PNetworkGetCertificate
  {
    -- | Origin to get certificate for.
    pNetworkGetCertificateOrigin :: T.Text
  }
  deriving (Eq, Show)
pNetworkGetCertificate
  -- | Origin to get certificate for.
  :: T.Text
  -> PNetworkGetCertificate
pNetworkGetCertificate
  arg_pNetworkGetCertificateOrigin
  = PNetworkGetCertificate
    arg_pNetworkGetCertificateOrigin
instance ToJSON PNetworkGetCertificate where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pNetworkGetCertificateOrigin p)
    ]
data NetworkGetCertificate = NetworkGetCertificate
  {
    networkGetCertificateTableNames :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetCertificate where
  parseJSON = A.withObject "NetworkGetCertificate" $ \o -> NetworkGetCertificate
    <$> o A..: "tableNames"
instance Command PNetworkGetCertificate where
  type CommandResponse PNetworkGetCertificate = NetworkGetCertificate
  commandName _ = "Network.getCertificate"

-- | Returns all browser cookies for the current URL. Depending on the backend support, will return
--   detailed cookie information in the `cookies` field.

-- | Parameters of the 'Network.getCookies' command.
data PNetworkGetCookies = PNetworkGetCookies
  {
    -- | The list of URLs for which applicable cookies will be fetched.
    --   If not specified, it's assumed to be set to the list containing
    --   the URLs of the page and all of its subframes.
    pNetworkGetCookiesUrls :: Maybe [T.Text]
  }
  deriving (Eq, Show)
pNetworkGetCookies
  :: PNetworkGetCookies
pNetworkGetCookies
  = PNetworkGetCookies
    Nothing
instance ToJSON PNetworkGetCookies where
  toJSON p = A.object $ catMaybes [
    ("urls" A..=) <$> (pNetworkGetCookiesUrls p)
    ]
data NetworkGetCookies = NetworkGetCookies
  {
    -- | Array of cookie objects.
    networkGetCookiesCookies :: [NetworkCookie]
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetCookies where
  parseJSON = A.withObject "NetworkGetCookies" $ \o -> NetworkGetCookies
    <$> o A..: "cookies"
instance Command PNetworkGetCookies where
  type CommandResponse PNetworkGetCookies = NetworkGetCookies
  commandName _ = "Network.getCookies"

-- | Returns content served for the given request.

-- | Parameters of the 'Network.getResponseBody' command.
data PNetworkGetResponseBody = PNetworkGetResponseBody
  {
    -- | Identifier of the network request to get content for.
    pNetworkGetResponseBodyRequestId :: NetworkRequestId
  }
  deriving (Eq, Show)
pNetworkGetResponseBody
  -- | Identifier of the network request to get content for.
  :: NetworkRequestId
  -> PNetworkGetResponseBody
pNetworkGetResponseBody
  arg_pNetworkGetResponseBodyRequestId
  = PNetworkGetResponseBody
    arg_pNetworkGetResponseBodyRequestId
instance ToJSON PNetworkGetResponseBody where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pNetworkGetResponseBodyRequestId p)
    ]
data NetworkGetResponseBody = NetworkGetResponseBody
  {
    -- | Response body.
    networkGetResponseBodyBody :: T.Text,
    -- | True, if content was sent as base64.
    networkGetResponseBodyBase64Encoded :: Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetResponseBody where
  parseJSON = A.withObject "NetworkGetResponseBody" $ \o -> NetworkGetResponseBody
    <$> o A..: "body"
    <*> o A..: "base64Encoded"
instance Command PNetworkGetResponseBody where
  type CommandResponse PNetworkGetResponseBody = NetworkGetResponseBody
  commandName _ = "Network.getResponseBody"

-- | Returns post data sent with the request. Returns an error when no data was sent with the request.

-- | Parameters of the 'Network.getRequestPostData' command.
data PNetworkGetRequestPostData = PNetworkGetRequestPostData
  {
    -- | Identifier of the network request to get content for.
    pNetworkGetRequestPostDataRequestId :: NetworkRequestId
  }
  deriving (Eq, Show)
pNetworkGetRequestPostData
  -- | Identifier of the network request to get content for.
  :: NetworkRequestId
  -> PNetworkGetRequestPostData
pNetworkGetRequestPostData
  arg_pNetworkGetRequestPostDataRequestId
  = PNetworkGetRequestPostData
    arg_pNetworkGetRequestPostDataRequestId
instance ToJSON PNetworkGetRequestPostData where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pNetworkGetRequestPostDataRequestId p)
    ]
data NetworkGetRequestPostData = NetworkGetRequestPostData
  {
    -- | Request body string, omitting files from multipart requests
    networkGetRequestPostDataPostData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetRequestPostData where
  parseJSON = A.withObject "NetworkGetRequestPostData" $ \o -> NetworkGetRequestPostData
    <$> o A..: "postData"
instance Command PNetworkGetRequestPostData where
  type CommandResponse PNetworkGetRequestPostData = NetworkGetRequestPostData
  commandName _ = "Network.getRequestPostData"

-- | Returns content served for the given currently intercepted request.

-- | Parameters of the 'Network.getResponseBodyForInterception' command.
data PNetworkGetResponseBodyForInterception = PNetworkGetResponseBodyForInterception
  {
    -- | Identifier for the intercepted request to get body for.
    pNetworkGetResponseBodyForInterceptionInterceptionId :: NetworkInterceptionId
  }
  deriving (Eq, Show)
pNetworkGetResponseBodyForInterception
  -- | Identifier for the intercepted request to get body for.
  :: NetworkInterceptionId
  -> PNetworkGetResponseBodyForInterception
pNetworkGetResponseBodyForInterception
  arg_pNetworkGetResponseBodyForInterceptionInterceptionId
  = PNetworkGetResponseBodyForInterception
    arg_pNetworkGetResponseBodyForInterceptionInterceptionId
instance ToJSON PNetworkGetResponseBodyForInterception where
  toJSON p = A.object $ catMaybes [
    ("interceptionId" A..=) <$> Just (pNetworkGetResponseBodyForInterceptionInterceptionId p)
    ]
data NetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception
  {
    -- | Response body.
    networkGetResponseBodyForInterceptionBody :: T.Text,
    -- | True, if content was sent as base64.
    networkGetResponseBodyForInterceptionBase64Encoded :: Bool
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetResponseBodyForInterception where
  parseJSON = A.withObject "NetworkGetResponseBodyForInterception" $ \o -> NetworkGetResponseBodyForInterception
    <$> o A..: "body"
    <*> o A..: "base64Encoded"
instance Command PNetworkGetResponseBodyForInterception where
  type CommandResponse PNetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception
  commandName _ = "Network.getResponseBodyForInterception"

-- | Returns a handle to the stream representing the response body. Note that after this command,
--   the intercepted request can't be continued as is -- you either need to cancel it or to provide
--   the response body. The stream only supports sequential read, IO.read will fail if the position
--   is specified.

-- | Parameters of the 'Network.takeResponseBodyForInterceptionAsStream' command.
data PNetworkTakeResponseBodyForInterceptionAsStream = PNetworkTakeResponseBodyForInterceptionAsStream
  {
    pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId :: NetworkInterceptionId
  }
  deriving (Eq, Show)
pNetworkTakeResponseBodyForInterceptionAsStream
  :: NetworkInterceptionId
  -> PNetworkTakeResponseBodyForInterceptionAsStream
pNetworkTakeResponseBodyForInterceptionAsStream
  arg_pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId
  = PNetworkTakeResponseBodyForInterceptionAsStream
    arg_pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId
instance ToJSON PNetworkTakeResponseBodyForInterceptionAsStream where
  toJSON p = A.object $ catMaybes [
    ("interceptionId" A..=) <$> Just (pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId p)
    ]
data NetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream
  {
    networkTakeResponseBodyForInterceptionAsStreamStream :: IO.IOStreamHandle
  }
  deriving (Eq, Show)
instance FromJSON NetworkTakeResponseBodyForInterceptionAsStream where
  parseJSON = A.withObject "NetworkTakeResponseBodyForInterceptionAsStream" $ \o -> NetworkTakeResponseBodyForInterceptionAsStream
    <$> o A..: "stream"
instance Command PNetworkTakeResponseBodyForInterceptionAsStream where
  type CommandResponse PNetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream
  commandName _ = "Network.takeResponseBodyForInterceptionAsStream"

-- | This method sends a new XMLHttpRequest which is identical to the original one. The following
--   parameters should be identical: method, url, async, request body, extra headers, withCredentials
--   attribute, user, password.

-- | Parameters of the 'Network.replayXHR' command.
data PNetworkReplayXHR = PNetworkReplayXHR
  {
    -- | Identifier of XHR to replay.
    pNetworkReplayXHRRequestId :: NetworkRequestId
  }
  deriving (Eq, Show)
pNetworkReplayXHR
  -- | Identifier of XHR to replay.
  :: NetworkRequestId
  -> PNetworkReplayXHR
pNetworkReplayXHR
  arg_pNetworkReplayXHRRequestId
  = PNetworkReplayXHR
    arg_pNetworkReplayXHRRequestId
instance ToJSON PNetworkReplayXHR where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pNetworkReplayXHRRequestId p)
    ]
instance Command PNetworkReplayXHR where
  type CommandResponse PNetworkReplayXHR = ()
  commandName _ = "Network.replayXHR"
  fromJSON = const . A.Success . const ()

-- | Searches for given string in response content.

-- | Parameters of the 'Network.searchInResponseBody' command.
data PNetworkSearchInResponseBody = PNetworkSearchInResponseBody
  {
    -- | Identifier of the network response to search.
    pNetworkSearchInResponseBodyRequestId :: NetworkRequestId,
    -- | String to search for.
    pNetworkSearchInResponseBodyQuery :: T.Text,
    -- | If true, search is case sensitive.
    pNetworkSearchInResponseBodyCaseSensitive :: Maybe Bool,
    -- | If true, treats string parameter as regex.
    pNetworkSearchInResponseBodyIsRegex :: Maybe Bool
  }
  deriving (Eq, Show)
pNetworkSearchInResponseBody
  -- | Identifier of the network response to search.
  :: NetworkRequestId
  -- | String to search for.
  -> T.Text
  -> PNetworkSearchInResponseBody
pNetworkSearchInResponseBody
  arg_pNetworkSearchInResponseBodyRequestId
  arg_pNetworkSearchInResponseBodyQuery
  = PNetworkSearchInResponseBody
    arg_pNetworkSearchInResponseBodyRequestId
    arg_pNetworkSearchInResponseBodyQuery
    Nothing
    Nothing
instance ToJSON PNetworkSearchInResponseBody where
  toJSON p = A.object $ catMaybes [
    ("requestId" A..=) <$> Just (pNetworkSearchInResponseBodyRequestId p),
    ("query" A..=) <$> Just (pNetworkSearchInResponseBodyQuery p),
    ("caseSensitive" A..=) <$> (pNetworkSearchInResponseBodyCaseSensitive p),
    ("isRegex" A..=) <$> (pNetworkSearchInResponseBodyIsRegex p)
    ]
data NetworkSearchInResponseBody = NetworkSearchInResponseBody
  {
    -- | List of search matches.
    networkSearchInResponseBodyResult :: [Debugger.DebuggerSearchMatch]
  }
  deriving (Eq, Show)
instance FromJSON NetworkSearchInResponseBody where
  parseJSON = A.withObject "NetworkSearchInResponseBody" $ \o -> NetworkSearchInResponseBody
    <$> o A..: "result"
instance Command PNetworkSearchInResponseBody where
  type CommandResponse PNetworkSearchInResponseBody = NetworkSearchInResponseBody
  commandName _ = "Network.searchInResponseBody"

-- | Blocks URLs from loading.

-- | Parameters of the 'Network.setBlockedURLs' command.
data PNetworkSetBlockedURLs = PNetworkSetBlockedURLs
  {
    -- | URL patterns to block. Wildcards ('*') are allowed.
    pNetworkSetBlockedURLsUrls :: [T.Text]
  }
  deriving (Eq, Show)
pNetworkSetBlockedURLs
  -- | URL patterns to block. Wildcards ('*') are allowed.
  :: [T.Text]
  -> PNetworkSetBlockedURLs
pNetworkSetBlockedURLs
  arg_pNetworkSetBlockedURLsUrls
  = PNetworkSetBlockedURLs
    arg_pNetworkSetBlockedURLsUrls
instance ToJSON PNetworkSetBlockedURLs where
  toJSON p = A.object $ catMaybes [
    ("urls" A..=) <$> Just (pNetworkSetBlockedURLsUrls p)
    ]
instance Command PNetworkSetBlockedURLs where
  type CommandResponse PNetworkSetBlockedURLs = ()
  commandName _ = "Network.setBlockedURLs"
  fromJSON = const . A.Success . const ()

-- | Toggles ignoring of service worker for each request.

-- | Parameters of the 'Network.setBypassServiceWorker' command.
data PNetworkSetBypassServiceWorker = PNetworkSetBypassServiceWorker
  {
    -- | Bypass service worker and load from network.
    pNetworkSetBypassServiceWorkerBypass :: Bool
  }
  deriving (Eq, Show)
pNetworkSetBypassServiceWorker
  -- | Bypass service worker and load from network.
  :: Bool
  -> PNetworkSetBypassServiceWorker
pNetworkSetBypassServiceWorker
  arg_pNetworkSetBypassServiceWorkerBypass
  = PNetworkSetBypassServiceWorker
    arg_pNetworkSetBypassServiceWorkerBypass
instance ToJSON PNetworkSetBypassServiceWorker where
  toJSON p = A.object $ catMaybes [
    ("bypass" A..=) <$> Just (pNetworkSetBypassServiceWorkerBypass p)
    ]
instance Command PNetworkSetBypassServiceWorker where
  type CommandResponse PNetworkSetBypassServiceWorker = ()
  commandName _ = "Network.setBypassServiceWorker"
  fromJSON = const . A.Success . const ()

-- | Toggles ignoring cache for each request. If `true`, cache will not be used.

-- | Parameters of the 'Network.setCacheDisabled' command.
data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled
  {
    -- | Cache disabled state.
    pNetworkSetCacheDisabledCacheDisabled :: Bool
  }
  deriving (Eq, Show)
pNetworkSetCacheDisabled
  -- | Cache disabled state.
  :: Bool
  -> PNetworkSetCacheDisabled
pNetworkSetCacheDisabled
  arg_pNetworkSetCacheDisabledCacheDisabled
  = PNetworkSetCacheDisabled
    arg_pNetworkSetCacheDisabledCacheDisabled
instance ToJSON PNetworkSetCacheDisabled where
  toJSON p = A.object $ catMaybes [
    ("cacheDisabled" A..=) <$> Just (pNetworkSetCacheDisabledCacheDisabled p)
    ]
instance Command PNetworkSetCacheDisabled where
  type CommandResponse PNetworkSetCacheDisabled = ()
  commandName _ = "Network.setCacheDisabled"
  fromJSON = const . A.Success . const ()

-- | Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.

-- | Parameters of the 'Network.setCookie' command.
data PNetworkSetCookie = PNetworkSetCookie
  {
    -- | Cookie name.
    pNetworkSetCookieName :: T.Text,
    -- | Cookie value.
    pNetworkSetCookieValue :: T.Text,
    -- | The request-URI to associate with the setting of the cookie. This value can affect the
    --   default domain, path, source port, and source scheme values of the created cookie.
    pNetworkSetCookieUrl :: Maybe T.Text,
    -- | Cookie domain.
    pNetworkSetCookieDomain :: Maybe T.Text,
    -- | Cookie path.
    pNetworkSetCookiePath :: Maybe T.Text,
    -- | True if cookie is secure.
    pNetworkSetCookieSecure :: Maybe Bool,
    -- | True if cookie is http-only.
    pNetworkSetCookieHttpOnly :: Maybe Bool,
    -- | Cookie SameSite type.
    pNetworkSetCookieSameSite :: Maybe NetworkCookieSameSite,
    -- | Cookie expiration date, session cookie if not set
    pNetworkSetCookieExpires :: Maybe NetworkTimeSinceEpoch,
    -- | Cookie Priority type.
    pNetworkSetCookiePriority :: Maybe NetworkCookiePriority,
    -- | True if cookie is SameParty.
    pNetworkSetCookieSameParty :: Maybe Bool,
    -- | Cookie source scheme type.
    pNetworkSetCookieSourceScheme :: Maybe NetworkCookieSourceScheme,
    -- | Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
    --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
    --   This is a temporary ability and it will be removed in the future.
    pNetworkSetCookieSourcePort :: Maybe Int,
    -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
    --   of the request to the endpoint that set the cookie.
    --   If not set, the cookie will be set as not partitioned.
    pNetworkSetCookiePartitionKey :: Maybe T.Text
  }
  deriving (Eq, Show)
pNetworkSetCookie
  -- | Cookie name.
  :: T.Text
  -- | Cookie value.
  -> T.Text
  -> PNetworkSetCookie
pNetworkSetCookie
  arg_pNetworkSetCookieName
  arg_pNetworkSetCookieValue
  = PNetworkSetCookie
    arg_pNetworkSetCookieName
    arg_pNetworkSetCookieValue
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PNetworkSetCookie where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pNetworkSetCookieName p),
    ("value" A..=) <$> Just (pNetworkSetCookieValue p),
    ("url" A..=) <$> (pNetworkSetCookieUrl p),
    ("domain" A..=) <$> (pNetworkSetCookieDomain p),
    ("path" A..=) <$> (pNetworkSetCookiePath p),
    ("secure" A..=) <$> (pNetworkSetCookieSecure p),
    ("httpOnly" A..=) <$> (pNetworkSetCookieHttpOnly p),
    ("sameSite" A..=) <$> (pNetworkSetCookieSameSite p),
    ("expires" A..=) <$> (pNetworkSetCookieExpires p),
    ("priority" A..=) <$> (pNetworkSetCookiePriority p),
    ("sameParty" A..=) <$> (pNetworkSetCookieSameParty p),
    ("sourceScheme" A..=) <$> (pNetworkSetCookieSourceScheme p),
    ("sourcePort" A..=) <$> (pNetworkSetCookieSourcePort p),
    ("partitionKey" A..=) <$> (pNetworkSetCookiePartitionKey p)
    ]
instance Command PNetworkSetCookie where
  type CommandResponse PNetworkSetCookie = ()
  commandName _ = "Network.setCookie"
  fromJSON = const . A.Success . const ()

-- | Sets given cookies.

-- | Parameters of the 'Network.setCookies' command.
data PNetworkSetCookies = PNetworkSetCookies
  {
    -- | Cookies to be set.
    pNetworkSetCookiesCookies :: [NetworkCookieParam]
  }
  deriving (Eq, Show)
pNetworkSetCookies
  -- | Cookies to be set.
  :: [NetworkCookieParam]
  -> PNetworkSetCookies
pNetworkSetCookies
  arg_pNetworkSetCookiesCookies
  = PNetworkSetCookies
    arg_pNetworkSetCookiesCookies
instance ToJSON PNetworkSetCookies where
  toJSON p = A.object $ catMaybes [
    ("cookies" A..=) <$> Just (pNetworkSetCookiesCookies p)
    ]
instance Command PNetworkSetCookies where
  type CommandResponse PNetworkSetCookies = ()
  commandName _ = "Network.setCookies"
  fromJSON = const . A.Success . const ()

-- | Specifies whether to always send extra HTTP headers with the requests from this page.

-- | Parameters of the 'Network.setExtraHTTPHeaders' command.
data PNetworkSetExtraHTTPHeaders = PNetworkSetExtraHTTPHeaders
  {
    -- | Map with extra HTTP headers.
    pNetworkSetExtraHTTPHeadersHeaders :: NetworkHeaders
  }
  deriving (Eq, Show)
pNetworkSetExtraHTTPHeaders
  -- | Map with extra HTTP headers.
  :: NetworkHeaders
  -> PNetworkSetExtraHTTPHeaders
pNetworkSetExtraHTTPHeaders
  arg_pNetworkSetExtraHTTPHeadersHeaders
  = PNetworkSetExtraHTTPHeaders
    arg_pNetworkSetExtraHTTPHeadersHeaders
instance ToJSON PNetworkSetExtraHTTPHeaders where
  toJSON p = A.object $ catMaybes [
    ("headers" A..=) <$> Just (pNetworkSetExtraHTTPHeadersHeaders p)
    ]
instance Command PNetworkSetExtraHTTPHeaders where
  type CommandResponse PNetworkSetExtraHTTPHeaders = ()
  commandName _ = "Network.setExtraHTTPHeaders"
  fromJSON = const . A.Success . const ()

-- | Specifies whether to attach a page script stack id in requests

-- | Parameters of the 'Network.setAttachDebugStack' command.
data PNetworkSetAttachDebugStack = PNetworkSetAttachDebugStack
  {
    -- | Whether to attach a page script stack for debugging purpose.
    pNetworkSetAttachDebugStackEnabled :: Bool
  }
  deriving (Eq, Show)
pNetworkSetAttachDebugStack
  -- | Whether to attach a page script stack for debugging purpose.
  :: Bool
  -> PNetworkSetAttachDebugStack
pNetworkSetAttachDebugStack
  arg_pNetworkSetAttachDebugStackEnabled
  = PNetworkSetAttachDebugStack
    arg_pNetworkSetAttachDebugStackEnabled
instance ToJSON PNetworkSetAttachDebugStack where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pNetworkSetAttachDebugStackEnabled p)
    ]
instance Command PNetworkSetAttachDebugStack where
  type CommandResponse PNetworkSetAttachDebugStack = ()
  commandName _ = "Network.setAttachDebugStack"
  fromJSON = const . A.Success . const ()

-- | Allows overriding user agent with the given string.

-- | Parameters of the 'Network.setUserAgentOverride' command.
data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride
  {
    -- | User agent to use.
    pNetworkSetUserAgentOverrideUserAgent :: T.Text,
    -- | Browser langugage to emulate.
    pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe T.Text,
    -- | The platform navigator.platform should return.
    pNetworkSetUserAgentOverridePlatform :: Maybe T.Text,
    -- | To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
    pNetworkSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
  }
  deriving (Eq, Show)
pNetworkSetUserAgentOverride
  -- | User agent to use.
  :: T.Text
  -> PNetworkSetUserAgentOverride
pNetworkSetUserAgentOverride
  arg_pNetworkSetUserAgentOverrideUserAgent
  = PNetworkSetUserAgentOverride
    arg_pNetworkSetUserAgentOverrideUserAgent
    Nothing
    Nothing
    Nothing
instance ToJSON PNetworkSetUserAgentOverride where
  toJSON p = A.object $ catMaybes [
    ("userAgent" A..=) <$> Just (pNetworkSetUserAgentOverrideUserAgent p),
    ("acceptLanguage" A..=) <$> (pNetworkSetUserAgentOverrideAcceptLanguage p),
    ("platform" A..=) <$> (pNetworkSetUserAgentOverridePlatform p),
    ("userAgentMetadata" A..=) <$> (pNetworkSetUserAgentOverrideUserAgentMetadata p)
    ]
instance Command PNetworkSetUserAgentOverride where
  type CommandResponse PNetworkSetUserAgentOverride = ()
  commandName _ = "Network.setUserAgentOverride"
  fromJSON = const . A.Success . const ()

-- | Returns information about the COEP/COOP isolation status.

-- | Parameters of the 'Network.getSecurityIsolationStatus' command.
data PNetworkGetSecurityIsolationStatus = PNetworkGetSecurityIsolationStatus
  {
    -- | If no frameId is provided, the status of the target is provided.
    pNetworkGetSecurityIsolationStatusFrameId :: Maybe PageFrameId
  }
  deriving (Eq, Show)
pNetworkGetSecurityIsolationStatus
  :: PNetworkGetSecurityIsolationStatus
pNetworkGetSecurityIsolationStatus
  = PNetworkGetSecurityIsolationStatus
    Nothing
instance ToJSON PNetworkGetSecurityIsolationStatus where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> (pNetworkGetSecurityIsolationStatusFrameId p)
    ]
data NetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus
  {
    networkGetSecurityIsolationStatusStatus :: NetworkSecurityIsolationStatus
  }
  deriving (Eq, Show)
instance FromJSON NetworkGetSecurityIsolationStatus where
  parseJSON = A.withObject "NetworkGetSecurityIsolationStatus" $ \o -> NetworkGetSecurityIsolationStatus
    <$> o A..: "status"
instance Command PNetworkGetSecurityIsolationStatus where
  type CommandResponse PNetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus
  commandName _ = "Network.getSecurityIsolationStatus"

-- | Enables tracking for the Reporting API, events generated by the Reporting API will now be delivered to the client.
--   Enabling triggers 'reportingApiReportAdded' for all existing reports.

-- | Parameters of the 'Network.enableReportingApi' command.
data PNetworkEnableReportingApi = PNetworkEnableReportingApi
  {
    -- | Whether to enable or disable events for the Reporting API
    pNetworkEnableReportingApiEnable :: Bool
  }
  deriving (Eq, Show)
pNetworkEnableReportingApi
  -- | Whether to enable or disable events for the Reporting API
  :: Bool
  -> PNetworkEnableReportingApi
pNetworkEnableReportingApi
  arg_pNetworkEnableReportingApiEnable
  = PNetworkEnableReportingApi
    arg_pNetworkEnableReportingApiEnable
instance ToJSON PNetworkEnableReportingApi where
  toJSON p = A.object $ catMaybes [
    ("enable" A..=) <$> Just (pNetworkEnableReportingApiEnable p)
    ]
instance Command PNetworkEnableReportingApi where
  type CommandResponse PNetworkEnableReportingApi = ()
  commandName _ = "Network.enableReportingApi"
  fromJSON = const . A.Success . const ()

-- | Fetches the resource and returns the content.

-- | Parameters of the 'Network.loadNetworkResource' command.
data PNetworkLoadNetworkResource = PNetworkLoadNetworkResource
  {
    -- | Frame id to get the resource for. Mandatory for frame targets, and
    --   should be omitted for worker targets.
    pNetworkLoadNetworkResourceFrameId :: Maybe PageFrameId,
    -- | URL of the resource to get content for.
    pNetworkLoadNetworkResourceUrl :: T.Text,
    -- | Options for the request.
    pNetworkLoadNetworkResourceOptions :: NetworkLoadNetworkResourceOptions
  }
  deriving (Eq, Show)
pNetworkLoadNetworkResource
  -- | URL of the resource to get content for.
  :: T.Text
  -- | Options for the request.
  -> NetworkLoadNetworkResourceOptions
  -> PNetworkLoadNetworkResource
pNetworkLoadNetworkResource
  arg_pNetworkLoadNetworkResourceUrl
  arg_pNetworkLoadNetworkResourceOptions
  = PNetworkLoadNetworkResource
    Nothing
    arg_pNetworkLoadNetworkResourceUrl
    arg_pNetworkLoadNetworkResourceOptions
instance ToJSON PNetworkLoadNetworkResource where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> (pNetworkLoadNetworkResourceFrameId p),
    ("url" A..=) <$> Just (pNetworkLoadNetworkResourceUrl p),
    ("options" A..=) <$> Just (pNetworkLoadNetworkResourceOptions p)
    ]
data NetworkLoadNetworkResource = NetworkLoadNetworkResource
  {
    networkLoadNetworkResourceResource :: NetworkLoadNetworkResourcePageResult
  }
  deriving (Eq, Show)
instance FromJSON NetworkLoadNetworkResource where
  parseJSON = A.withObject "NetworkLoadNetworkResource" $ \o -> NetworkLoadNetworkResource
    <$> o A..: "resource"
instance Command PNetworkLoadNetworkResource where
  type CommandResponse PNetworkLoadNetworkResource = NetworkLoadNetworkResource
  commandName _ = "Network.loadNetworkResource"

-- | Type 'Page.FrameId'.
--   Unique frame identifier.
type PageFrameId = T.Text

-- | Type 'Page.AdFrameType'.
--   Indicates whether a frame has been identified as an ad.
data PageAdFrameType = PageAdFrameTypeNone | PageAdFrameTypeChild | PageAdFrameTypeRoot
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageAdFrameType where
  parseJSON = A.withText "PageAdFrameType" $ \v -> case v of
    "none" -> pure PageAdFrameTypeNone
    "child" -> pure PageAdFrameTypeChild
    "root" -> pure PageAdFrameTypeRoot
    "_" -> fail "failed to parse PageAdFrameType"
instance ToJSON PageAdFrameType where
  toJSON v = A.String $ case v of
    PageAdFrameTypeNone -> "none"
    PageAdFrameTypeChild -> "child"
    PageAdFrameTypeRoot -> "root"

-- | Type 'Page.AdFrameExplanation'.
data PageAdFrameExplanation = PageAdFrameExplanationParentIsAd | PageAdFrameExplanationCreatedByAdScript | PageAdFrameExplanationMatchedBlockingRule
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageAdFrameExplanation where
  parseJSON = A.withText "PageAdFrameExplanation" $ \v -> case v of
    "ParentIsAd" -> pure PageAdFrameExplanationParentIsAd
    "CreatedByAdScript" -> pure PageAdFrameExplanationCreatedByAdScript
    "MatchedBlockingRule" -> pure PageAdFrameExplanationMatchedBlockingRule
    "_" -> fail "failed to parse PageAdFrameExplanation"
instance ToJSON PageAdFrameExplanation where
  toJSON v = A.String $ case v of
    PageAdFrameExplanationParentIsAd -> "ParentIsAd"
    PageAdFrameExplanationCreatedByAdScript -> "CreatedByAdScript"
    PageAdFrameExplanationMatchedBlockingRule -> "MatchedBlockingRule"

-- | Type 'Page.AdFrameStatus'.
--   Indicates whether a frame has been identified as an ad and why.
data PageAdFrameStatus = PageAdFrameStatus
  {
    pageAdFrameStatusAdFrameType :: PageAdFrameType,
    pageAdFrameStatusExplanations :: Maybe [PageAdFrameExplanation]
  }
  deriving (Eq, Show)
instance FromJSON PageAdFrameStatus where
  parseJSON = A.withObject "PageAdFrameStatus" $ \o -> PageAdFrameStatus
    <$> o A..: "adFrameType"
    <*> o A..:? "explanations"
instance ToJSON PageAdFrameStatus where
  toJSON p = A.object $ catMaybes [
    ("adFrameType" A..=) <$> Just (pageAdFrameStatusAdFrameType p),
    ("explanations" A..=) <$> (pageAdFrameStatusExplanations p)
    ]

-- | Type 'Page.AdScriptId'.
--   Identifies the bottom-most script which caused the frame to be labelled
--   as an ad.
data PageAdScriptId = PageAdScriptId
  {
    -- | Script Id of the bottom-most script which caused the frame to be labelled
    --   as an ad.
    pageAdScriptIdScriptId :: Runtime.RuntimeScriptId,
    -- | Id of adScriptId's debugger.
    pageAdScriptIdDebuggerId :: Runtime.RuntimeUniqueDebuggerId
  }
  deriving (Eq, Show)
instance FromJSON PageAdScriptId where
  parseJSON = A.withObject "PageAdScriptId" $ \o -> PageAdScriptId
    <$> o A..: "scriptId"
    <*> o A..: "debuggerId"
instance ToJSON PageAdScriptId where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pageAdScriptIdScriptId p),
    ("debuggerId" A..=) <$> Just (pageAdScriptIdDebuggerId p)
    ]

-- | Type 'Page.SecureContextType'.
--   Indicates whether the frame is a secure context and why it is the case.
data PageSecureContextType = PageSecureContextTypeSecure | PageSecureContextTypeSecureLocalhost | PageSecureContextTypeInsecureScheme | PageSecureContextTypeInsecureAncestor
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageSecureContextType where
  parseJSON = A.withText "PageSecureContextType" $ \v -> case v of
    "Secure" -> pure PageSecureContextTypeSecure
    "SecureLocalhost" -> pure PageSecureContextTypeSecureLocalhost
    "InsecureScheme" -> pure PageSecureContextTypeInsecureScheme
    "InsecureAncestor" -> pure PageSecureContextTypeInsecureAncestor
    "_" -> fail "failed to parse PageSecureContextType"
instance ToJSON PageSecureContextType where
  toJSON v = A.String $ case v of
    PageSecureContextTypeSecure -> "Secure"
    PageSecureContextTypeSecureLocalhost -> "SecureLocalhost"
    PageSecureContextTypeInsecureScheme -> "InsecureScheme"
    PageSecureContextTypeInsecureAncestor -> "InsecureAncestor"

-- | Type 'Page.CrossOriginIsolatedContextType'.
--   Indicates whether the frame is cross-origin isolated and why it is the case.
data PageCrossOriginIsolatedContextType = PageCrossOriginIsolatedContextTypeIsolated | PageCrossOriginIsolatedContextTypeNotIsolated | PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageCrossOriginIsolatedContextType where
  parseJSON = A.withText "PageCrossOriginIsolatedContextType" $ \v -> case v of
    "Isolated" -> pure PageCrossOriginIsolatedContextTypeIsolated
    "NotIsolated" -> pure PageCrossOriginIsolatedContextTypeNotIsolated
    "NotIsolatedFeatureDisabled" -> pure PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled
    "_" -> fail "failed to parse PageCrossOriginIsolatedContextType"
instance ToJSON PageCrossOriginIsolatedContextType where
  toJSON v = A.String $ case v of
    PageCrossOriginIsolatedContextTypeIsolated -> "Isolated"
    PageCrossOriginIsolatedContextTypeNotIsolated -> "NotIsolated"
    PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled -> "NotIsolatedFeatureDisabled"

-- | Type 'Page.GatedAPIFeatures'.
data PageGatedAPIFeatures = PageGatedAPIFeaturesSharedArrayBuffers | PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed | PageGatedAPIFeaturesPerformanceMeasureMemory | PageGatedAPIFeaturesPerformanceProfile
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageGatedAPIFeatures where
  parseJSON = A.withText "PageGatedAPIFeatures" $ \v -> case v of
    "SharedArrayBuffers" -> pure PageGatedAPIFeaturesSharedArrayBuffers
    "SharedArrayBuffersTransferAllowed" -> pure PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed
    "PerformanceMeasureMemory" -> pure PageGatedAPIFeaturesPerformanceMeasureMemory
    "PerformanceProfile" -> pure PageGatedAPIFeaturesPerformanceProfile
    "_" -> fail "failed to parse PageGatedAPIFeatures"
instance ToJSON PageGatedAPIFeatures where
  toJSON v = A.String $ case v of
    PageGatedAPIFeaturesSharedArrayBuffers -> "SharedArrayBuffers"
    PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed -> "SharedArrayBuffersTransferAllowed"
    PageGatedAPIFeaturesPerformanceMeasureMemory -> "PerformanceMeasureMemory"
    PageGatedAPIFeaturesPerformanceProfile -> "PerformanceProfile"

-- | Type 'Page.PermissionsPolicyFeature'.
--   All Permissions Policy features. This enum should match the one defined
--   in third_party/blink/renderer/core/permissions_policy/permissions_policy_features.json5.
data PagePermissionsPolicyFeature = PagePermissionsPolicyFeatureAccelerometer | PagePermissionsPolicyFeatureAmbientLightSensor | PagePermissionsPolicyFeatureAttributionReporting | PagePermissionsPolicyFeatureAutoplay | PagePermissionsPolicyFeatureBluetooth | PagePermissionsPolicyFeatureBrowsingTopics | PagePermissionsPolicyFeatureCamera | PagePermissionsPolicyFeatureChDpr | PagePermissionsPolicyFeatureChDeviceMemory | PagePermissionsPolicyFeatureChDownlink | PagePermissionsPolicyFeatureChEct | PagePermissionsPolicyFeatureChPrefersColorScheme | PagePermissionsPolicyFeatureChPrefersReducedMotion | PagePermissionsPolicyFeatureChRtt | PagePermissionsPolicyFeatureChSaveData | PagePermissionsPolicyFeatureChUa | PagePermissionsPolicyFeatureChUaArch | PagePermissionsPolicyFeatureChUaBitness | PagePermissionsPolicyFeatureChUaPlatform | PagePermissionsPolicyFeatureChUaModel | PagePermissionsPolicyFeatureChUaMobile | PagePermissionsPolicyFeatureChUaFull | PagePermissionsPolicyFeatureChUaFullVersion | PagePermissionsPolicyFeatureChUaFullVersionList | PagePermissionsPolicyFeatureChUaPlatformVersion | PagePermissionsPolicyFeatureChUaReduced | PagePermissionsPolicyFeatureChUaWow64 | PagePermissionsPolicyFeatureChViewportHeight | PagePermissionsPolicyFeatureChViewportWidth | PagePermissionsPolicyFeatureChWidth | PagePermissionsPolicyFeatureClipboardRead | PagePermissionsPolicyFeatureClipboardWrite | PagePermissionsPolicyFeatureCrossOriginIsolated | PagePermissionsPolicyFeatureDirectSockets | PagePermissionsPolicyFeatureDisplayCapture | PagePermissionsPolicyFeatureDocumentDomain | PagePermissionsPolicyFeatureEncryptedMedia | PagePermissionsPolicyFeatureExecutionWhileOutOfViewport | PagePermissionsPolicyFeatureExecutionWhileNotRendered | PagePermissionsPolicyFeatureFocusWithoutUserActivation | PagePermissionsPolicyFeatureFullscreen | PagePermissionsPolicyFeatureFrobulate | PagePermissionsPolicyFeatureGamepad | PagePermissionsPolicyFeatureGeolocation | PagePermissionsPolicyFeatureGyroscope | PagePermissionsPolicyFeatureHid | PagePermissionsPolicyFeatureIdentityCredentialsGet | PagePermissionsPolicyFeatureIdleDetection | PagePermissionsPolicyFeatureInterestCohort | PagePermissionsPolicyFeatureJoinAdInterestGroup | PagePermissionsPolicyFeatureKeyboardMap | PagePermissionsPolicyFeatureLocalFonts | PagePermissionsPolicyFeatureMagnetometer | PagePermissionsPolicyFeatureMicrophone | PagePermissionsPolicyFeatureMidi | PagePermissionsPolicyFeatureOtpCredentials | PagePermissionsPolicyFeaturePayment | PagePermissionsPolicyFeaturePictureInPicture | PagePermissionsPolicyFeaturePublickeyCredentialsGet | PagePermissionsPolicyFeatureRunAdAuction | PagePermissionsPolicyFeatureScreenWakeLock | PagePermissionsPolicyFeatureSerial | PagePermissionsPolicyFeatureSharedAutofill | PagePermissionsPolicyFeatureSharedStorage | PagePermissionsPolicyFeatureStorageAccess | PagePermissionsPolicyFeatureSyncXhr | PagePermissionsPolicyFeatureTrustTokenRedemption | PagePermissionsPolicyFeatureUnload | PagePermissionsPolicyFeatureUsb | PagePermissionsPolicyFeatureVerticalScroll | PagePermissionsPolicyFeatureWebShare | PagePermissionsPolicyFeatureWindowPlacement | PagePermissionsPolicyFeatureXrSpatialTracking
  deriving (Ord, Eq, Show, Read)
instance FromJSON PagePermissionsPolicyFeature where
  parseJSON = A.withText "PagePermissionsPolicyFeature" $ \v -> case v of
    "accelerometer" -> pure PagePermissionsPolicyFeatureAccelerometer
    "ambient-light-sensor" -> pure PagePermissionsPolicyFeatureAmbientLightSensor
    "attribution-reporting" -> pure PagePermissionsPolicyFeatureAttributionReporting
    "autoplay" -> pure PagePermissionsPolicyFeatureAutoplay
    "bluetooth" -> pure PagePermissionsPolicyFeatureBluetooth
    "browsing-topics" -> pure PagePermissionsPolicyFeatureBrowsingTopics
    "camera" -> pure PagePermissionsPolicyFeatureCamera
    "ch-dpr" -> pure PagePermissionsPolicyFeatureChDpr
    "ch-device-memory" -> pure PagePermissionsPolicyFeatureChDeviceMemory
    "ch-downlink" -> pure PagePermissionsPolicyFeatureChDownlink
    "ch-ect" -> pure PagePermissionsPolicyFeatureChEct
    "ch-prefers-color-scheme" -> pure PagePermissionsPolicyFeatureChPrefersColorScheme
    "ch-prefers-reduced-motion" -> pure PagePermissionsPolicyFeatureChPrefersReducedMotion
    "ch-rtt" -> pure PagePermissionsPolicyFeatureChRtt
    "ch-save-data" -> pure PagePermissionsPolicyFeatureChSaveData
    "ch-ua" -> pure PagePermissionsPolicyFeatureChUa
    "ch-ua-arch" -> pure PagePermissionsPolicyFeatureChUaArch
    "ch-ua-bitness" -> pure PagePermissionsPolicyFeatureChUaBitness
    "ch-ua-platform" -> pure PagePermissionsPolicyFeatureChUaPlatform
    "ch-ua-model" -> pure PagePermissionsPolicyFeatureChUaModel
    "ch-ua-mobile" -> pure PagePermissionsPolicyFeatureChUaMobile
    "ch-ua-full" -> pure PagePermissionsPolicyFeatureChUaFull
    "ch-ua-full-version" -> pure PagePermissionsPolicyFeatureChUaFullVersion
    "ch-ua-full-version-list" -> pure PagePermissionsPolicyFeatureChUaFullVersionList
    "ch-ua-platform-version" -> pure PagePermissionsPolicyFeatureChUaPlatformVersion
    "ch-ua-reduced" -> pure PagePermissionsPolicyFeatureChUaReduced
    "ch-ua-wow64" -> pure PagePermissionsPolicyFeatureChUaWow64
    "ch-viewport-height" -> pure PagePermissionsPolicyFeatureChViewportHeight
    "ch-viewport-width" -> pure PagePermissionsPolicyFeatureChViewportWidth
    "ch-width" -> pure PagePermissionsPolicyFeatureChWidth
    "clipboard-read" -> pure PagePermissionsPolicyFeatureClipboardRead
    "clipboard-write" -> pure PagePermissionsPolicyFeatureClipboardWrite
    "cross-origin-isolated" -> pure PagePermissionsPolicyFeatureCrossOriginIsolated
    "direct-sockets" -> pure PagePermissionsPolicyFeatureDirectSockets
    "display-capture" -> pure PagePermissionsPolicyFeatureDisplayCapture
    "document-domain" -> pure PagePermissionsPolicyFeatureDocumentDomain
    "encrypted-media" -> pure PagePermissionsPolicyFeatureEncryptedMedia
    "execution-while-out-of-viewport" -> pure PagePermissionsPolicyFeatureExecutionWhileOutOfViewport
    "execution-while-not-rendered" -> pure PagePermissionsPolicyFeatureExecutionWhileNotRendered
    "focus-without-user-activation" -> pure PagePermissionsPolicyFeatureFocusWithoutUserActivation
    "fullscreen" -> pure PagePermissionsPolicyFeatureFullscreen
    "frobulate" -> pure PagePermissionsPolicyFeatureFrobulate
    "gamepad" -> pure PagePermissionsPolicyFeatureGamepad
    "geolocation" -> pure PagePermissionsPolicyFeatureGeolocation
    "gyroscope" -> pure PagePermissionsPolicyFeatureGyroscope
    "hid" -> pure PagePermissionsPolicyFeatureHid
    "identity-credentials-get" -> pure PagePermissionsPolicyFeatureIdentityCredentialsGet
    "idle-detection" -> pure PagePermissionsPolicyFeatureIdleDetection
    "interest-cohort" -> pure PagePermissionsPolicyFeatureInterestCohort
    "join-ad-interest-group" -> pure PagePermissionsPolicyFeatureJoinAdInterestGroup
    "keyboard-map" -> pure PagePermissionsPolicyFeatureKeyboardMap
    "local-fonts" -> pure PagePermissionsPolicyFeatureLocalFonts
    "magnetometer" -> pure PagePermissionsPolicyFeatureMagnetometer
    "microphone" -> pure PagePermissionsPolicyFeatureMicrophone
    "midi" -> pure PagePermissionsPolicyFeatureMidi
    "otp-credentials" -> pure PagePermissionsPolicyFeatureOtpCredentials
    "payment" -> pure PagePermissionsPolicyFeaturePayment
    "picture-in-picture" -> pure PagePermissionsPolicyFeaturePictureInPicture
    "publickey-credentials-get" -> pure PagePermissionsPolicyFeaturePublickeyCredentialsGet
    "run-ad-auction" -> pure PagePermissionsPolicyFeatureRunAdAuction
    "screen-wake-lock" -> pure PagePermissionsPolicyFeatureScreenWakeLock
    "serial" -> pure PagePermissionsPolicyFeatureSerial
    "shared-autofill" -> pure PagePermissionsPolicyFeatureSharedAutofill
    "shared-storage" -> pure PagePermissionsPolicyFeatureSharedStorage
    "storage-access" -> pure PagePermissionsPolicyFeatureStorageAccess
    "sync-xhr" -> pure PagePermissionsPolicyFeatureSyncXhr
    "trust-token-redemption" -> pure PagePermissionsPolicyFeatureTrustTokenRedemption
    "unload" -> pure PagePermissionsPolicyFeatureUnload
    "usb" -> pure PagePermissionsPolicyFeatureUsb
    "vertical-scroll" -> pure PagePermissionsPolicyFeatureVerticalScroll
    "web-share" -> pure PagePermissionsPolicyFeatureWebShare
    "window-placement" -> pure PagePermissionsPolicyFeatureWindowPlacement
    "xr-spatial-tracking" -> pure PagePermissionsPolicyFeatureXrSpatialTracking
    "_" -> fail "failed to parse PagePermissionsPolicyFeature"
instance ToJSON PagePermissionsPolicyFeature where
  toJSON v = A.String $ case v of
    PagePermissionsPolicyFeatureAccelerometer -> "accelerometer"
    PagePermissionsPolicyFeatureAmbientLightSensor -> "ambient-light-sensor"
    PagePermissionsPolicyFeatureAttributionReporting -> "attribution-reporting"
    PagePermissionsPolicyFeatureAutoplay -> "autoplay"
    PagePermissionsPolicyFeatureBluetooth -> "bluetooth"
    PagePermissionsPolicyFeatureBrowsingTopics -> "browsing-topics"
    PagePermissionsPolicyFeatureCamera -> "camera"
    PagePermissionsPolicyFeatureChDpr -> "ch-dpr"
    PagePermissionsPolicyFeatureChDeviceMemory -> "ch-device-memory"
    PagePermissionsPolicyFeatureChDownlink -> "ch-downlink"
    PagePermissionsPolicyFeatureChEct -> "ch-ect"
    PagePermissionsPolicyFeatureChPrefersColorScheme -> "ch-prefers-color-scheme"
    PagePermissionsPolicyFeatureChPrefersReducedMotion -> "ch-prefers-reduced-motion"
    PagePermissionsPolicyFeatureChRtt -> "ch-rtt"
    PagePermissionsPolicyFeatureChSaveData -> "ch-save-data"
    PagePermissionsPolicyFeatureChUa -> "ch-ua"
    PagePermissionsPolicyFeatureChUaArch -> "ch-ua-arch"
    PagePermissionsPolicyFeatureChUaBitness -> "ch-ua-bitness"
    PagePermissionsPolicyFeatureChUaPlatform -> "ch-ua-platform"
    PagePermissionsPolicyFeatureChUaModel -> "ch-ua-model"
    PagePermissionsPolicyFeatureChUaMobile -> "ch-ua-mobile"
    PagePermissionsPolicyFeatureChUaFull -> "ch-ua-full"
    PagePermissionsPolicyFeatureChUaFullVersion -> "ch-ua-full-version"
    PagePermissionsPolicyFeatureChUaFullVersionList -> "ch-ua-full-version-list"
    PagePermissionsPolicyFeatureChUaPlatformVersion -> "ch-ua-platform-version"
    PagePermissionsPolicyFeatureChUaReduced -> "ch-ua-reduced"
    PagePermissionsPolicyFeatureChUaWow64 -> "ch-ua-wow64"
    PagePermissionsPolicyFeatureChViewportHeight -> "ch-viewport-height"
    PagePermissionsPolicyFeatureChViewportWidth -> "ch-viewport-width"
    PagePermissionsPolicyFeatureChWidth -> "ch-width"
    PagePermissionsPolicyFeatureClipboardRead -> "clipboard-read"
    PagePermissionsPolicyFeatureClipboardWrite -> "clipboard-write"
    PagePermissionsPolicyFeatureCrossOriginIsolated -> "cross-origin-isolated"
    PagePermissionsPolicyFeatureDirectSockets -> "direct-sockets"
    PagePermissionsPolicyFeatureDisplayCapture -> "display-capture"
    PagePermissionsPolicyFeatureDocumentDomain -> "document-domain"
    PagePermissionsPolicyFeatureEncryptedMedia -> "encrypted-media"
    PagePermissionsPolicyFeatureExecutionWhileOutOfViewport -> "execution-while-out-of-viewport"
    PagePermissionsPolicyFeatureExecutionWhileNotRendered -> "execution-while-not-rendered"
    PagePermissionsPolicyFeatureFocusWithoutUserActivation -> "focus-without-user-activation"
    PagePermissionsPolicyFeatureFullscreen -> "fullscreen"
    PagePermissionsPolicyFeatureFrobulate -> "frobulate"
    PagePermissionsPolicyFeatureGamepad -> "gamepad"
    PagePermissionsPolicyFeatureGeolocation -> "geolocation"
    PagePermissionsPolicyFeatureGyroscope -> "gyroscope"
    PagePermissionsPolicyFeatureHid -> "hid"
    PagePermissionsPolicyFeatureIdentityCredentialsGet -> "identity-credentials-get"
    PagePermissionsPolicyFeatureIdleDetection -> "idle-detection"
    PagePermissionsPolicyFeatureInterestCohort -> "interest-cohort"
    PagePermissionsPolicyFeatureJoinAdInterestGroup -> "join-ad-interest-group"
    PagePermissionsPolicyFeatureKeyboardMap -> "keyboard-map"
    PagePermissionsPolicyFeatureLocalFonts -> "local-fonts"
    PagePermissionsPolicyFeatureMagnetometer -> "magnetometer"
    PagePermissionsPolicyFeatureMicrophone -> "microphone"
    PagePermissionsPolicyFeatureMidi -> "midi"
    PagePermissionsPolicyFeatureOtpCredentials -> "otp-credentials"
    PagePermissionsPolicyFeaturePayment -> "payment"
    PagePermissionsPolicyFeaturePictureInPicture -> "picture-in-picture"
    PagePermissionsPolicyFeaturePublickeyCredentialsGet -> "publickey-credentials-get"
    PagePermissionsPolicyFeatureRunAdAuction -> "run-ad-auction"
    PagePermissionsPolicyFeatureScreenWakeLock -> "screen-wake-lock"
    PagePermissionsPolicyFeatureSerial -> "serial"
    PagePermissionsPolicyFeatureSharedAutofill -> "shared-autofill"
    PagePermissionsPolicyFeatureSharedStorage -> "shared-storage"
    PagePermissionsPolicyFeatureStorageAccess -> "storage-access"
    PagePermissionsPolicyFeatureSyncXhr -> "sync-xhr"
    PagePermissionsPolicyFeatureTrustTokenRedemption -> "trust-token-redemption"
    PagePermissionsPolicyFeatureUnload -> "unload"
    PagePermissionsPolicyFeatureUsb -> "usb"
    PagePermissionsPolicyFeatureVerticalScroll -> "vertical-scroll"
    PagePermissionsPolicyFeatureWebShare -> "web-share"
    PagePermissionsPolicyFeatureWindowPlacement -> "window-placement"
    PagePermissionsPolicyFeatureXrSpatialTracking -> "xr-spatial-tracking"

-- | Type 'Page.PermissionsPolicyBlockReason'.
--   Reason for a permissions policy feature to be disabled.
data PagePermissionsPolicyBlockReason = PagePermissionsPolicyBlockReasonHeader | PagePermissionsPolicyBlockReasonIframeAttribute | PagePermissionsPolicyBlockReasonInFencedFrameTree | PagePermissionsPolicyBlockReasonInIsolatedApp
  deriving (Ord, Eq, Show, Read)
instance FromJSON PagePermissionsPolicyBlockReason where
  parseJSON = A.withText "PagePermissionsPolicyBlockReason" $ \v -> case v of
    "Header" -> pure PagePermissionsPolicyBlockReasonHeader
    "IframeAttribute" -> pure PagePermissionsPolicyBlockReasonIframeAttribute
    "InFencedFrameTree" -> pure PagePermissionsPolicyBlockReasonInFencedFrameTree
    "InIsolatedApp" -> pure PagePermissionsPolicyBlockReasonInIsolatedApp
    "_" -> fail "failed to parse PagePermissionsPolicyBlockReason"
instance ToJSON PagePermissionsPolicyBlockReason where
  toJSON v = A.String $ case v of
    PagePermissionsPolicyBlockReasonHeader -> "Header"
    PagePermissionsPolicyBlockReasonIframeAttribute -> "IframeAttribute"
    PagePermissionsPolicyBlockReasonInFencedFrameTree -> "InFencedFrameTree"
    PagePermissionsPolicyBlockReasonInIsolatedApp -> "InIsolatedApp"

-- | Type 'Page.PermissionsPolicyBlockLocator'.
data PagePermissionsPolicyBlockLocator = PagePermissionsPolicyBlockLocator
  {
    pagePermissionsPolicyBlockLocatorFrameId :: PageFrameId,
    pagePermissionsPolicyBlockLocatorBlockReason :: PagePermissionsPolicyBlockReason
  }
  deriving (Eq, Show)
instance FromJSON PagePermissionsPolicyBlockLocator where
  parseJSON = A.withObject "PagePermissionsPolicyBlockLocator" $ \o -> PagePermissionsPolicyBlockLocator
    <$> o A..: "frameId"
    <*> o A..: "blockReason"
instance ToJSON PagePermissionsPolicyBlockLocator where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pagePermissionsPolicyBlockLocatorFrameId p),
    ("blockReason" A..=) <$> Just (pagePermissionsPolicyBlockLocatorBlockReason p)
    ]

-- | Type 'Page.PermissionsPolicyFeatureState'.
data PagePermissionsPolicyFeatureState = PagePermissionsPolicyFeatureState
  {
    pagePermissionsPolicyFeatureStateFeature :: PagePermissionsPolicyFeature,
    pagePermissionsPolicyFeatureStateAllowed :: Bool,
    pagePermissionsPolicyFeatureStateLocator :: Maybe PagePermissionsPolicyBlockLocator
  }
  deriving (Eq, Show)
instance FromJSON PagePermissionsPolicyFeatureState where
  parseJSON = A.withObject "PagePermissionsPolicyFeatureState" $ \o -> PagePermissionsPolicyFeatureState
    <$> o A..: "feature"
    <*> o A..: "allowed"
    <*> o A..:? "locator"
instance ToJSON PagePermissionsPolicyFeatureState where
  toJSON p = A.object $ catMaybes [
    ("feature" A..=) <$> Just (pagePermissionsPolicyFeatureStateFeature p),
    ("allowed" A..=) <$> Just (pagePermissionsPolicyFeatureStateAllowed p),
    ("locator" A..=) <$> (pagePermissionsPolicyFeatureStateLocator p)
    ]

-- | Type 'Page.OriginTrialTokenStatus'.
--   Origin Trial(https://www.chromium.org/blink/origin-trials) support.
--   Status for an Origin Trial token.
data PageOriginTrialTokenStatus = PageOriginTrialTokenStatusSuccess | PageOriginTrialTokenStatusNotSupported | PageOriginTrialTokenStatusInsecure | PageOriginTrialTokenStatusExpired | PageOriginTrialTokenStatusWrongOrigin | PageOriginTrialTokenStatusInvalidSignature | PageOriginTrialTokenStatusMalformed | PageOriginTrialTokenStatusWrongVersion | PageOriginTrialTokenStatusFeatureDisabled | PageOriginTrialTokenStatusTokenDisabled | PageOriginTrialTokenStatusFeatureDisabledForUser | PageOriginTrialTokenStatusUnknownTrial
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialTokenStatus where
  parseJSON = A.withText "PageOriginTrialTokenStatus" $ \v -> case v of
    "Success" -> pure PageOriginTrialTokenStatusSuccess
    "NotSupported" -> pure PageOriginTrialTokenStatusNotSupported
    "Insecure" -> pure PageOriginTrialTokenStatusInsecure
    "Expired" -> pure PageOriginTrialTokenStatusExpired
    "WrongOrigin" -> pure PageOriginTrialTokenStatusWrongOrigin
    "InvalidSignature" -> pure PageOriginTrialTokenStatusInvalidSignature
    "Malformed" -> pure PageOriginTrialTokenStatusMalformed
    "WrongVersion" -> pure PageOriginTrialTokenStatusWrongVersion
    "FeatureDisabled" -> pure PageOriginTrialTokenStatusFeatureDisabled
    "TokenDisabled" -> pure PageOriginTrialTokenStatusTokenDisabled
    "FeatureDisabledForUser" -> pure PageOriginTrialTokenStatusFeatureDisabledForUser
    "UnknownTrial" -> pure PageOriginTrialTokenStatusUnknownTrial
    "_" -> fail "failed to parse PageOriginTrialTokenStatus"
instance ToJSON PageOriginTrialTokenStatus where
  toJSON v = A.String $ case v of
    PageOriginTrialTokenStatusSuccess -> "Success"
    PageOriginTrialTokenStatusNotSupported -> "NotSupported"
    PageOriginTrialTokenStatusInsecure -> "Insecure"
    PageOriginTrialTokenStatusExpired -> "Expired"
    PageOriginTrialTokenStatusWrongOrigin -> "WrongOrigin"
    PageOriginTrialTokenStatusInvalidSignature -> "InvalidSignature"
    PageOriginTrialTokenStatusMalformed -> "Malformed"
    PageOriginTrialTokenStatusWrongVersion -> "WrongVersion"
    PageOriginTrialTokenStatusFeatureDisabled -> "FeatureDisabled"
    PageOriginTrialTokenStatusTokenDisabled -> "TokenDisabled"
    PageOriginTrialTokenStatusFeatureDisabledForUser -> "FeatureDisabledForUser"
    PageOriginTrialTokenStatusUnknownTrial -> "UnknownTrial"

-- | Type 'Page.OriginTrialStatus'.
--   Status for an Origin Trial.
data PageOriginTrialStatus = PageOriginTrialStatusEnabled | PageOriginTrialStatusValidTokenNotProvided | PageOriginTrialStatusOSNotSupported | PageOriginTrialStatusTrialNotAllowed
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialStatus where
  parseJSON = A.withText "PageOriginTrialStatus" $ \v -> case v of
    "Enabled" -> pure PageOriginTrialStatusEnabled
    "ValidTokenNotProvided" -> pure PageOriginTrialStatusValidTokenNotProvided
    "OSNotSupported" -> pure PageOriginTrialStatusOSNotSupported
    "TrialNotAllowed" -> pure PageOriginTrialStatusTrialNotAllowed
    "_" -> fail "failed to parse PageOriginTrialStatus"
instance ToJSON PageOriginTrialStatus where
  toJSON v = A.String $ case v of
    PageOriginTrialStatusEnabled -> "Enabled"
    PageOriginTrialStatusValidTokenNotProvided -> "ValidTokenNotProvided"
    PageOriginTrialStatusOSNotSupported -> "OSNotSupported"
    PageOriginTrialStatusTrialNotAllowed -> "TrialNotAllowed"

-- | Type 'Page.OriginTrialUsageRestriction'.
data PageOriginTrialUsageRestriction = PageOriginTrialUsageRestrictionNone | PageOriginTrialUsageRestrictionSubset
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialUsageRestriction where
  parseJSON = A.withText "PageOriginTrialUsageRestriction" $ \v -> case v of
    "None" -> pure PageOriginTrialUsageRestrictionNone
    "Subset" -> pure PageOriginTrialUsageRestrictionSubset
    "_" -> fail "failed to parse PageOriginTrialUsageRestriction"
instance ToJSON PageOriginTrialUsageRestriction where
  toJSON v = A.String $ case v of
    PageOriginTrialUsageRestrictionNone -> "None"
    PageOriginTrialUsageRestrictionSubset -> "Subset"

-- | Type 'Page.OriginTrialToken'.
data PageOriginTrialToken = PageOriginTrialToken
  {
    pageOriginTrialTokenOrigin :: T.Text,
    pageOriginTrialTokenMatchSubDomains :: Bool,
    pageOriginTrialTokenTrialName :: T.Text,
    pageOriginTrialTokenExpiryTime :: NetworkTimeSinceEpoch,
    pageOriginTrialTokenIsThirdParty :: Bool,
    pageOriginTrialTokenUsageRestriction :: PageOriginTrialUsageRestriction
  }
  deriving (Eq, Show)
instance FromJSON PageOriginTrialToken where
  parseJSON = A.withObject "PageOriginTrialToken" $ \o -> PageOriginTrialToken
    <$> o A..: "origin"
    <*> o A..: "matchSubDomains"
    <*> o A..: "trialName"
    <*> o A..: "expiryTime"
    <*> o A..: "isThirdParty"
    <*> o A..: "usageRestriction"
instance ToJSON PageOriginTrialToken where
  toJSON p = A.object $ catMaybes [
    ("origin" A..=) <$> Just (pageOriginTrialTokenOrigin p),
    ("matchSubDomains" A..=) <$> Just (pageOriginTrialTokenMatchSubDomains p),
    ("trialName" A..=) <$> Just (pageOriginTrialTokenTrialName p),
    ("expiryTime" A..=) <$> Just (pageOriginTrialTokenExpiryTime p),
    ("isThirdParty" A..=) <$> Just (pageOriginTrialTokenIsThirdParty p),
    ("usageRestriction" A..=) <$> Just (pageOriginTrialTokenUsageRestriction p)
    ]

-- | Type 'Page.OriginTrialTokenWithStatus'.
data PageOriginTrialTokenWithStatus = PageOriginTrialTokenWithStatus
  {
    pageOriginTrialTokenWithStatusRawTokenText :: T.Text,
    -- | `parsedToken` is present only when the token is extractable and
    --   parsable.
    pageOriginTrialTokenWithStatusParsedToken :: Maybe PageOriginTrialToken,
    pageOriginTrialTokenWithStatusStatus :: PageOriginTrialTokenStatus
  }
  deriving (Eq, Show)
instance FromJSON PageOriginTrialTokenWithStatus where
  parseJSON = A.withObject "PageOriginTrialTokenWithStatus" $ \o -> PageOriginTrialTokenWithStatus
    <$> o A..: "rawTokenText"
    <*> o A..:? "parsedToken"
    <*> o A..: "status"
instance ToJSON PageOriginTrialTokenWithStatus where
  toJSON p = A.object $ catMaybes [
    ("rawTokenText" A..=) <$> Just (pageOriginTrialTokenWithStatusRawTokenText p),
    ("parsedToken" A..=) <$> (pageOriginTrialTokenWithStatusParsedToken p),
    ("status" A..=) <$> Just (pageOriginTrialTokenWithStatusStatus p)
    ]

-- | Type 'Page.OriginTrial'.
data PageOriginTrial = PageOriginTrial
  {
    pageOriginTrialTrialName :: T.Text,
    pageOriginTrialStatus :: PageOriginTrialStatus,
    pageOriginTrialTokensWithStatus :: [PageOriginTrialTokenWithStatus]
  }
  deriving (Eq, Show)
instance FromJSON PageOriginTrial where
  parseJSON = A.withObject "PageOriginTrial" $ \o -> PageOriginTrial
    <$> o A..: "trialName"
    <*> o A..: "status"
    <*> o A..: "tokensWithStatus"
instance ToJSON PageOriginTrial where
  toJSON p = A.object $ catMaybes [
    ("trialName" A..=) <$> Just (pageOriginTrialTrialName p),
    ("status" A..=) <$> Just (pageOriginTrialStatus p),
    ("tokensWithStatus" A..=) <$> Just (pageOriginTrialTokensWithStatus p)
    ]

-- | Type 'Page.Frame'.
--   Information about the Frame on the page.
data PageFrame = PageFrame
  {
    -- | Frame unique identifier.
    pageFrameId :: PageFrameId,
    -- | Parent frame identifier.
    pageFrameParentId :: Maybe PageFrameId,
    -- | Identifier of the loader associated with this frame.
    pageFrameLoaderId :: NetworkLoaderId,
    -- | Frame's name as specified in the tag.
    pageFrameName :: Maybe T.Text,
    -- | Frame document's URL without fragment.
    pageFrameUrl :: T.Text,
    -- | Frame document's URL fragment including the '#'.
    pageFrameUrlFragment :: Maybe T.Text,
    -- | Frame document's registered domain, taking the public suffixes list into account.
    --   Extracted from the Frame's url.
    --   Example URLs: http://www.google.com/file.html -> "google.com"
    --                 http://a.b.co.uk/file.html      -> "b.co.uk"
    pageFrameDomainAndRegistry :: T.Text,
    -- | Frame document's security origin.
    pageFrameSecurityOrigin :: T.Text,
    -- | Frame document's mimeType as determined by the browser.
    pageFrameMimeType :: T.Text,
    -- | If the frame failed to load, this contains the URL that could not be loaded. Note that unlike url above, this URL may contain a fragment.
    pageFrameUnreachableUrl :: Maybe T.Text,
    -- | Indicates whether this frame was tagged as an ad and why.
    pageFrameAdFrameStatus :: Maybe PageAdFrameStatus,
    -- | Indicates whether the main document is a secure context and explains why that is the case.
    pageFrameSecureContextType :: PageSecureContextType,
    -- | Indicates whether this is a cross origin isolated context.
    pageFrameCrossOriginIsolatedContextType :: PageCrossOriginIsolatedContextType,
    -- | Indicated which gated APIs / features are available.
    pageFrameGatedAPIFeatures :: [PageGatedAPIFeatures]
  }
  deriving (Eq, Show)
instance FromJSON PageFrame where
  parseJSON = A.withObject "PageFrame" $ \o -> PageFrame
    <$> o A..: "id"
    <*> o A..:? "parentId"
    <*> o A..: "loaderId"
    <*> o A..:? "name"
    <*> o A..: "url"
    <*> o A..:? "urlFragment"
    <*> o A..: "domainAndRegistry"
    <*> o A..: "securityOrigin"
    <*> o A..: "mimeType"
    <*> o A..:? "unreachableUrl"
    <*> o A..:? "adFrameStatus"
    <*> o A..: "secureContextType"
    <*> o A..: "crossOriginIsolatedContextType"
    <*> o A..: "gatedAPIFeatures"
instance ToJSON PageFrame where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pageFrameId p),
    ("parentId" A..=) <$> (pageFrameParentId p),
    ("loaderId" A..=) <$> Just (pageFrameLoaderId p),
    ("name" A..=) <$> (pageFrameName p),
    ("url" A..=) <$> Just (pageFrameUrl p),
    ("urlFragment" A..=) <$> (pageFrameUrlFragment p),
    ("domainAndRegistry" A..=) <$> Just (pageFrameDomainAndRegistry p),
    ("securityOrigin" A..=) <$> Just (pageFrameSecurityOrigin p),
    ("mimeType" A..=) <$> Just (pageFrameMimeType p),
    ("unreachableUrl" A..=) <$> (pageFrameUnreachableUrl p),
    ("adFrameStatus" A..=) <$> (pageFrameAdFrameStatus p),
    ("secureContextType" A..=) <$> Just (pageFrameSecureContextType p),
    ("crossOriginIsolatedContextType" A..=) <$> Just (pageFrameCrossOriginIsolatedContextType p),
    ("gatedAPIFeatures" A..=) <$> Just (pageFrameGatedAPIFeatures p)
    ]

-- | Type 'Page.FrameResource'.
--   Information about the Resource on the page.
data PageFrameResource = PageFrameResource
  {
    -- | Resource URL.
    pageFrameResourceUrl :: T.Text,
    -- | Type of this resource.
    pageFrameResourceType :: NetworkResourceType,
    -- | Resource mimeType as determined by the browser.
    pageFrameResourceMimeType :: T.Text,
    -- | last-modified timestamp as reported by server.
    pageFrameResourceLastModified :: Maybe NetworkTimeSinceEpoch,
    -- | Resource content size.
    pageFrameResourceContentSize :: Maybe Double,
    -- | True if the resource failed to load.
    pageFrameResourceFailed :: Maybe Bool,
    -- | True if the resource was canceled during loading.
    pageFrameResourceCanceled :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON PageFrameResource where
  parseJSON = A.withObject "PageFrameResource" $ \o -> PageFrameResource
    <$> o A..: "url"
    <*> o A..: "type"
    <*> o A..: "mimeType"
    <*> o A..:? "lastModified"
    <*> o A..:? "contentSize"
    <*> o A..:? "failed"
    <*> o A..:? "canceled"
instance ToJSON PageFrameResource where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pageFrameResourceUrl p),
    ("type" A..=) <$> Just (pageFrameResourceType p),
    ("mimeType" A..=) <$> Just (pageFrameResourceMimeType p),
    ("lastModified" A..=) <$> (pageFrameResourceLastModified p),
    ("contentSize" A..=) <$> (pageFrameResourceContentSize p),
    ("failed" A..=) <$> (pageFrameResourceFailed p),
    ("canceled" A..=) <$> (pageFrameResourceCanceled p)
    ]

-- | Type 'Page.FrameResourceTree'.
--   Information about the Frame hierarchy along with their cached resources.
data PageFrameResourceTree = PageFrameResourceTree
  {
    -- | Frame information for this tree item.
    pageFrameResourceTreeFrame :: PageFrame,
    -- | Child frames.
    pageFrameResourceTreeChildFrames :: Maybe [PageFrameResourceTree],
    -- | Information about frame resources.
    pageFrameResourceTreeResources :: [PageFrameResource]
  }
  deriving (Eq, Show)
instance FromJSON PageFrameResourceTree where
  parseJSON = A.withObject "PageFrameResourceTree" $ \o -> PageFrameResourceTree
    <$> o A..: "frame"
    <*> o A..:? "childFrames"
    <*> o A..: "resources"
instance ToJSON PageFrameResourceTree where
  toJSON p = A.object $ catMaybes [
    ("frame" A..=) <$> Just (pageFrameResourceTreeFrame p),
    ("childFrames" A..=) <$> (pageFrameResourceTreeChildFrames p),
    ("resources" A..=) <$> Just (pageFrameResourceTreeResources p)
    ]

-- | Type 'Page.FrameTree'.
--   Information about the Frame hierarchy.
data PageFrameTree = PageFrameTree
  {
    -- | Frame information for this tree item.
    pageFrameTreeFrame :: PageFrame,
    -- | Child frames.
    pageFrameTreeChildFrames :: Maybe [PageFrameTree]
  }
  deriving (Eq, Show)
instance FromJSON PageFrameTree where
  parseJSON = A.withObject "PageFrameTree" $ \o -> PageFrameTree
    <$> o A..: "frame"
    <*> o A..:? "childFrames"
instance ToJSON PageFrameTree where
  toJSON p = A.object $ catMaybes [
    ("frame" A..=) <$> Just (pageFrameTreeFrame p),
    ("childFrames" A..=) <$> (pageFrameTreeChildFrames p)
    ]

-- | Type 'Page.ScriptIdentifier'.
--   Unique script identifier.
type PageScriptIdentifier = T.Text

-- | Type 'Page.TransitionType'.
--   Transition type.
data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddress_bar | PageTransitionTypeAuto_bookmark | PageTransitionTypeAuto_subframe | PageTransitionTypeManual_subframe | PageTransitionTypeGenerated | PageTransitionTypeAuto_toplevel | PageTransitionTypeForm_submit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeyword_generated | PageTransitionTypeOther
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageTransitionType where
  parseJSON = A.withText "PageTransitionType" $ \v -> case v of
    "link" -> pure PageTransitionTypeLink
    "typed" -> pure PageTransitionTypeTyped
    "address_bar" -> pure PageTransitionTypeAddress_bar
    "auto_bookmark" -> pure PageTransitionTypeAuto_bookmark
    "auto_subframe" -> pure PageTransitionTypeAuto_subframe
    "manual_subframe" -> pure PageTransitionTypeManual_subframe
    "generated" -> pure PageTransitionTypeGenerated
    "auto_toplevel" -> pure PageTransitionTypeAuto_toplevel
    "form_submit" -> pure PageTransitionTypeForm_submit
    "reload" -> pure PageTransitionTypeReload
    "keyword" -> pure PageTransitionTypeKeyword
    "keyword_generated" -> pure PageTransitionTypeKeyword_generated
    "other" -> pure PageTransitionTypeOther
    "_" -> fail "failed to parse PageTransitionType"
instance ToJSON PageTransitionType where
  toJSON v = A.String $ case v of
    PageTransitionTypeLink -> "link"
    PageTransitionTypeTyped -> "typed"
    PageTransitionTypeAddress_bar -> "address_bar"
    PageTransitionTypeAuto_bookmark -> "auto_bookmark"
    PageTransitionTypeAuto_subframe -> "auto_subframe"
    PageTransitionTypeManual_subframe -> "manual_subframe"
    PageTransitionTypeGenerated -> "generated"
    PageTransitionTypeAuto_toplevel -> "auto_toplevel"
    PageTransitionTypeForm_submit -> "form_submit"
    PageTransitionTypeReload -> "reload"
    PageTransitionTypeKeyword -> "keyword"
    PageTransitionTypeKeyword_generated -> "keyword_generated"
    PageTransitionTypeOther -> "other"

-- | Type 'Page.NavigationEntry'.
--   Navigation history entry.
data PageNavigationEntry = PageNavigationEntry
  {
    -- | Unique id of the navigation history entry.
    pageNavigationEntryId :: Int,
    -- | URL of the navigation history entry.
    pageNavigationEntryUrl :: T.Text,
    -- | URL that the user typed in the url bar.
    pageNavigationEntryUserTypedURL :: T.Text,
    -- | Title of the navigation history entry.
    pageNavigationEntryTitle :: T.Text,
    -- | Transition type.
    pageNavigationEntryTransitionType :: PageTransitionType
  }
  deriving (Eq, Show)
instance FromJSON PageNavigationEntry where
  parseJSON = A.withObject "PageNavigationEntry" $ \o -> PageNavigationEntry
    <$> o A..: "id"
    <*> o A..: "url"
    <*> o A..: "userTypedURL"
    <*> o A..: "title"
    <*> o A..: "transitionType"
instance ToJSON PageNavigationEntry where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pageNavigationEntryId p),
    ("url" A..=) <$> Just (pageNavigationEntryUrl p),
    ("userTypedURL" A..=) <$> Just (pageNavigationEntryUserTypedURL p),
    ("title" A..=) <$> Just (pageNavigationEntryTitle p),
    ("transitionType" A..=) <$> Just (pageNavigationEntryTransitionType p)
    ]

-- | Type 'Page.ScreencastFrameMetadata'.
--   Screencast frame metadata.
data PageScreencastFrameMetadata = PageScreencastFrameMetadata
  {
    -- | Top offset in DIP.
    pageScreencastFrameMetadataOffsetTop :: Double,
    -- | Page scale factor.
    pageScreencastFrameMetadataPageScaleFactor :: Double,
    -- | Device screen width in DIP.
    pageScreencastFrameMetadataDeviceWidth :: Double,
    -- | Device screen height in DIP.
    pageScreencastFrameMetadataDeviceHeight :: Double,
    -- | Position of horizontal scroll in CSS pixels.
    pageScreencastFrameMetadataScrollOffsetX :: Double,
    -- | Position of vertical scroll in CSS pixels.
    pageScreencastFrameMetadataScrollOffsetY :: Double,
    -- | Frame swap timestamp.
    pageScreencastFrameMetadataTimestamp :: Maybe NetworkTimeSinceEpoch
  }
  deriving (Eq, Show)
instance FromJSON PageScreencastFrameMetadata where
  parseJSON = A.withObject "PageScreencastFrameMetadata" $ \o -> PageScreencastFrameMetadata
    <$> o A..: "offsetTop"
    <*> o A..: "pageScaleFactor"
    <*> o A..: "deviceWidth"
    <*> o A..: "deviceHeight"
    <*> o A..: "scrollOffsetX"
    <*> o A..: "scrollOffsetY"
    <*> o A..:? "timestamp"
instance ToJSON PageScreencastFrameMetadata where
  toJSON p = A.object $ catMaybes [
    ("offsetTop" A..=) <$> Just (pageScreencastFrameMetadataOffsetTop p),
    ("pageScaleFactor" A..=) <$> Just (pageScreencastFrameMetadataPageScaleFactor p),
    ("deviceWidth" A..=) <$> Just (pageScreencastFrameMetadataDeviceWidth p),
    ("deviceHeight" A..=) <$> Just (pageScreencastFrameMetadataDeviceHeight p),
    ("scrollOffsetX" A..=) <$> Just (pageScreencastFrameMetadataScrollOffsetX p),
    ("scrollOffsetY" A..=) <$> Just (pageScreencastFrameMetadataScrollOffsetY p),
    ("timestamp" A..=) <$> (pageScreencastFrameMetadataTimestamp p)
    ]

-- | Type 'Page.DialogType'.
--   Javascript dialog type.
data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageDialogType where
  parseJSON = A.withText "PageDialogType" $ \v -> case v of
    "alert" -> pure PageDialogTypeAlert
    "confirm" -> pure PageDialogTypeConfirm
    "prompt" -> pure PageDialogTypePrompt
    "beforeunload" -> pure PageDialogTypeBeforeunload
    "_" -> fail "failed to parse PageDialogType"
instance ToJSON PageDialogType where
  toJSON v = A.String $ case v of
    PageDialogTypeAlert -> "alert"
    PageDialogTypeConfirm -> "confirm"
    PageDialogTypePrompt -> "prompt"
    PageDialogTypeBeforeunload -> "beforeunload"

-- | Type 'Page.AppManifestError'.
--   Error while paring app manifest.
data PageAppManifestError = PageAppManifestError
  {
    -- | Error message.
    pageAppManifestErrorMessage :: T.Text,
    -- | If criticial, this is a non-recoverable parse error.
    pageAppManifestErrorCritical :: Int,
    -- | Error line.
    pageAppManifestErrorLine :: Int,
    -- | Error column.
    pageAppManifestErrorColumn :: Int
  }
  deriving (Eq, Show)
instance FromJSON PageAppManifestError where
  parseJSON = A.withObject "PageAppManifestError" $ \o -> PageAppManifestError
    <$> o A..: "message"
    <*> o A..: "critical"
    <*> o A..: "line"
    <*> o A..: "column"
instance ToJSON PageAppManifestError where
  toJSON p = A.object $ catMaybes [
    ("message" A..=) <$> Just (pageAppManifestErrorMessage p),
    ("critical" A..=) <$> Just (pageAppManifestErrorCritical p),
    ("line" A..=) <$> Just (pageAppManifestErrorLine p),
    ("column" A..=) <$> Just (pageAppManifestErrorColumn p)
    ]

-- | Type 'Page.AppManifestParsedProperties'.
--   Parsed app manifest properties.
data PageAppManifestParsedProperties = PageAppManifestParsedProperties
  {
    -- | Computed scope value
    pageAppManifestParsedPropertiesScope :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageAppManifestParsedProperties where
  parseJSON = A.withObject "PageAppManifestParsedProperties" $ \o -> PageAppManifestParsedProperties
    <$> o A..: "scope"
instance ToJSON PageAppManifestParsedProperties where
  toJSON p = A.object $ catMaybes [
    ("scope" A..=) <$> Just (pageAppManifestParsedPropertiesScope p)
    ]

-- | Type 'Page.LayoutViewport'.
--   Layout viewport position and dimensions.
data PageLayoutViewport = PageLayoutViewport
  {
    -- | Horizontal offset relative to the document (CSS pixels).
    pageLayoutViewportPageX :: Int,
    -- | Vertical offset relative to the document (CSS pixels).
    pageLayoutViewportPageY :: Int,
    -- | Width (CSS pixels), excludes scrollbar if present.
    pageLayoutViewportClientWidth :: Int,
    -- | Height (CSS pixels), excludes scrollbar if present.
    pageLayoutViewportClientHeight :: Int
  }
  deriving (Eq, Show)
instance FromJSON PageLayoutViewport where
  parseJSON = A.withObject "PageLayoutViewport" $ \o -> PageLayoutViewport
    <$> o A..: "pageX"
    <*> o A..: "pageY"
    <*> o A..: "clientWidth"
    <*> o A..: "clientHeight"
instance ToJSON PageLayoutViewport where
  toJSON p = A.object $ catMaybes [
    ("pageX" A..=) <$> Just (pageLayoutViewportPageX p),
    ("pageY" A..=) <$> Just (pageLayoutViewportPageY p),
    ("clientWidth" A..=) <$> Just (pageLayoutViewportClientWidth p),
    ("clientHeight" A..=) <$> Just (pageLayoutViewportClientHeight p)
    ]

-- | Type 'Page.VisualViewport'.
--   Visual viewport position, dimensions, and scale.
data PageVisualViewport = PageVisualViewport
  {
    -- | Horizontal offset relative to the layout viewport (CSS pixels).
    pageVisualViewportOffsetX :: Double,
    -- | Vertical offset relative to the layout viewport (CSS pixels).
    pageVisualViewportOffsetY :: Double,
    -- | Horizontal offset relative to the document (CSS pixels).
    pageVisualViewportPageX :: Double,
    -- | Vertical offset relative to the document (CSS pixels).
    pageVisualViewportPageY :: Double,
    -- | Width (CSS pixels), excludes scrollbar if present.
    pageVisualViewportClientWidth :: Double,
    -- | Height (CSS pixels), excludes scrollbar if present.
    pageVisualViewportClientHeight :: Double,
    -- | Scale relative to the ideal viewport (size at width=device-width).
    pageVisualViewportScale :: Double,
    -- | Page zoom factor (CSS to device independent pixels ratio).
    pageVisualViewportZoom :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON PageVisualViewport where
  parseJSON = A.withObject "PageVisualViewport" $ \o -> PageVisualViewport
    <$> o A..: "offsetX"
    <*> o A..: "offsetY"
    <*> o A..: "pageX"
    <*> o A..: "pageY"
    <*> o A..: "clientWidth"
    <*> o A..: "clientHeight"
    <*> o A..: "scale"
    <*> o A..:? "zoom"
instance ToJSON PageVisualViewport where
  toJSON p = A.object $ catMaybes [
    ("offsetX" A..=) <$> Just (pageVisualViewportOffsetX p),
    ("offsetY" A..=) <$> Just (pageVisualViewportOffsetY p),
    ("pageX" A..=) <$> Just (pageVisualViewportPageX p),
    ("pageY" A..=) <$> Just (pageVisualViewportPageY p),
    ("clientWidth" A..=) <$> Just (pageVisualViewportClientWidth p),
    ("clientHeight" A..=) <$> Just (pageVisualViewportClientHeight p),
    ("scale" A..=) <$> Just (pageVisualViewportScale p),
    ("zoom" A..=) <$> (pageVisualViewportZoom p)
    ]

-- | Type 'Page.Viewport'.
--   Viewport for capturing screenshot.
data PageViewport = PageViewport
  {
    -- | X offset in device independent pixels (dip).
    pageViewportX :: Double,
    -- | Y offset in device independent pixels (dip).
    pageViewportY :: Double,
    -- | Rectangle width in device independent pixels (dip).
    pageViewportWidth :: Double,
    -- | Rectangle height in device independent pixels (dip).
    pageViewportHeight :: Double,
    -- | Page scale factor.
    pageViewportScale :: Double
  }
  deriving (Eq, Show)
instance FromJSON PageViewport where
  parseJSON = A.withObject "PageViewport" $ \o -> PageViewport
    <$> o A..: "x"
    <*> o A..: "y"
    <*> o A..: "width"
    <*> o A..: "height"
    <*> o A..: "scale"
instance ToJSON PageViewport where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pageViewportX p),
    ("y" A..=) <$> Just (pageViewportY p),
    ("width" A..=) <$> Just (pageViewportWidth p),
    ("height" A..=) <$> Just (pageViewportHeight p),
    ("scale" A..=) <$> Just (pageViewportScale p)
    ]

-- | Type 'Page.FontFamilies'.
--   Generic font families collection.
data PageFontFamilies = PageFontFamilies
  {
    -- | The standard font-family.
    pageFontFamiliesStandard :: Maybe T.Text,
    -- | The fixed font-family.
    pageFontFamiliesFixed :: Maybe T.Text,
    -- | The serif font-family.
    pageFontFamiliesSerif :: Maybe T.Text,
    -- | The sansSerif font-family.
    pageFontFamiliesSansSerif :: Maybe T.Text,
    -- | The cursive font-family.
    pageFontFamiliesCursive :: Maybe T.Text,
    -- | The fantasy font-family.
    pageFontFamiliesFantasy :: Maybe T.Text,
    -- | The math font-family.
    pageFontFamiliesMath :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageFontFamilies where
  parseJSON = A.withObject "PageFontFamilies" $ \o -> PageFontFamilies
    <$> o A..:? "standard"
    <*> o A..:? "fixed"
    <*> o A..:? "serif"
    <*> o A..:? "sansSerif"
    <*> o A..:? "cursive"
    <*> o A..:? "fantasy"
    <*> o A..:? "math"
instance ToJSON PageFontFamilies where
  toJSON p = A.object $ catMaybes [
    ("standard" A..=) <$> (pageFontFamiliesStandard p),
    ("fixed" A..=) <$> (pageFontFamiliesFixed p),
    ("serif" A..=) <$> (pageFontFamiliesSerif p),
    ("sansSerif" A..=) <$> (pageFontFamiliesSansSerif p),
    ("cursive" A..=) <$> (pageFontFamiliesCursive p),
    ("fantasy" A..=) <$> (pageFontFamiliesFantasy p),
    ("math" A..=) <$> (pageFontFamiliesMath p)
    ]

-- | Type 'Page.ScriptFontFamilies'.
--   Font families collection for a script.
data PageScriptFontFamilies = PageScriptFontFamilies
  {
    -- | Name of the script which these font families are defined for.
    pageScriptFontFamiliesScript :: T.Text,
    -- | Generic font families collection for the script.
    pageScriptFontFamiliesFontFamilies :: PageFontFamilies
  }
  deriving (Eq, Show)
instance FromJSON PageScriptFontFamilies where
  parseJSON = A.withObject "PageScriptFontFamilies" $ \o -> PageScriptFontFamilies
    <$> o A..: "script"
    <*> o A..: "fontFamilies"
instance ToJSON PageScriptFontFamilies where
  toJSON p = A.object $ catMaybes [
    ("script" A..=) <$> Just (pageScriptFontFamiliesScript p),
    ("fontFamilies" A..=) <$> Just (pageScriptFontFamiliesFontFamilies p)
    ]

-- | Type 'Page.FontSizes'.
--   Default font sizes.
data PageFontSizes = PageFontSizes
  {
    -- | Default standard font size.
    pageFontSizesStandard :: Maybe Int,
    -- | Default fixed font size.
    pageFontSizesFixed :: Maybe Int
  }
  deriving (Eq, Show)
instance FromJSON PageFontSizes where
  parseJSON = A.withObject "PageFontSizes" $ \o -> PageFontSizes
    <$> o A..:? "standard"
    <*> o A..:? "fixed"
instance ToJSON PageFontSizes where
  toJSON p = A.object $ catMaybes [
    ("standard" A..=) <$> (pageFontSizesStandard p),
    ("fixed" A..=) <$> (pageFontSizesFixed p)
    ]

-- | Type 'Page.ClientNavigationReason'.
data PageClientNavigationReason = PageClientNavigationReasonFormSubmissionGet | PageClientNavigationReasonFormSubmissionPost | PageClientNavigationReasonHttpHeaderRefresh | PageClientNavigationReasonScriptInitiated | PageClientNavigationReasonMetaTagRefresh | PageClientNavigationReasonPageBlockInterstitial | PageClientNavigationReasonReload | PageClientNavigationReasonAnchorClick
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageClientNavigationReason where
  parseJSON = A.withText "PageClientNavigationReason" $ \v -> case v of
    "formSubmissionGet" -> pure PageClientNavigationReasonFormSubmissionGet
    "formSubmissionPost" -> pure PageClientNavigationReasonFormSubmissionPost
    "httpHeaderRefresh" -> pure PageClientNavigationReasonHttpHeaderRefresh
    "scriptInitiated" -> pure PageClientNavigationReasonScriptInitiated
    "metaTagRefresh" -> pure PageClientNavigationReasonMetaTagRefresh
    "pageBlockInterstitial" -> pure PageClientNavigationReasonPageBlockInterstitial
    "reload" -> pure PageClientNavigationReasonReload
    "anchorClick" -> pure PageClientNavigationReasonAnchorClick
    "_" -> fail "failed to parse PageClientNavigationReason"
instance ToJSON PageClientNavigationReason where
  toJSON v = A.String $ case v of
    PageClientNavigationReasonFormSubmissionGet -> "formSubmissionGet"
    PageClientNavigationReasonFormSubmissionPost -> "formSubmissionPost"
    PageClientNavigationReasonHttpHeaderRefresh -> "httpHeaderRefresh"
    PageClientNavigationReasonScriptInitiated -> "scriptInitiated"
    PageClientNavigationReasonMetaTagRefresh -> "metaTagRefresh"
    PageClientNavigationReasonPageBlockInterstitial -> "pageBlockInterstitial"
    PageClientNavigationReasonReload -> "reload"
    PageClientNavigationReasonAnchorClick -> "anchorClick"

-- | Type 'Page.ClientNavigationDisposition'.
data PageClientNavigationDisposition = PageClientNavigationDispositionCurrentTab | PageClientNavigationDispositionNewTab | PageClientNavigationDispositionNewWindow | PageClientNavigationDispositionDownload
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageClientNavigationDisposition where
  parseJSON = A.withText "PageClientNavigationDisposition" $ \v -> case v of
    "currentTab" -> pure PageClientNavigationDispositionCurrentTab
    "newTab" -> pure PageClientNavigationDispositionNewTab
    "newWindow" -> pure PageClientNavigationDispositionNewWindow
    "download" -> pure PageClientNavigationDispositionDownload
    "_" -> fail "failed to parse PageClientNavigationDisposition"
instance ToJSON PageClientNavigationDisposition where
  toJSON v = A.String $ case v of
    PageClientNavigationDispositionCurrentTab -> "currentTab"
    PageClientNavigationDispositionNewTab -> "newTab"
    PageClientNavigationDispositionNewWindow -> "newWindow"
    PageClientNavigationDispositionDownload -> "download"

-- | Type 'Page.InstallabilityErrorArgument'.
data PageInstallabilityErrorArgument = PageInstallabilityErrorArgument
  {
    -- | Argument name (e.g. name:'minimum-icon-size-in-pixels').
    pageInstallabilityErrorArgumentName :: T.Text,
    -- | Argument value (e.g. value:'64').
    pageInstallabilityErrorArgumentValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageInstallabilityErrorArgument where
  parseJSON = A.withObject "PageInstallabilityErrorArgument" $ \o -> PageInstallabilityErrorArgument
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON PageInstallabilityErrorArgument where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pageInstallabilityErrorArgumentName p),
    ("value" A..=) <$> Just (pageInstallabilityErrorArgumentValue p)
    ]

-- | Type 'Page.InstallabilityError'.
--   The installability error
data PageInstallabilityError = PageInstallabilityError
  {
    -- | The error id (e.g. 'manifest-missing-suitable-icon').
    pageInstallabilityErrorErrorId :: T.Text,
    -- | The list of error arguments (e.g. {name:'minimum-icon-size-in-pixels', value:'64'}).
    pageInstallabilityErrorErrorArguments :: [PageInstallabilityErrorArgument]
  }
  deriving (Eq, Show)
instance FromJSON PageInstallabilityError where
  parseJSON = A.withObject "PageInstallabilityError" $ \o -> PageInstallabilityError
    <$> o A..: "errorId"
    <*> o A..: "errorArguments"
instance ToJSON PageInstallabilityError where
  toJSON p = A.object $ catMaybes [
    ("errorId" A..=) <$> Just (pageInstallabilityErrorErrorId p),
    ("errorArguments" A..=) <$> Just (pageInstallabilityErrorErrorArguments p)
    ]

-- | Type 'Page.ReferrerPolicy'.
--   The referring-policy used for the navigation.
data PageReferrerPolicy = PageReferrerPolicyNoReferrer | PageReferrerPolicyNoReferrerWhenDowngrade | PageReferrerPolicyOrigin | PageReferrerPolicyOriginWhenCrossOrigin | PageReferrerPolicySameOrigin | PageReferrerPolicyStrictOrigin | PageReferrerPolicyStrictOriginWhenCrossOrigin | PageReferrerPolicyUnsafeUrl
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageReferrerPolicy where
  parseJSON = A.withText "PageReferrerPolicy" $ \v -> case v of
    "noReferrer" -> pure PageReferrerPolicyNoReferrer
    "noReferrerWhenDowngrade" -> pure PageReferrerPolicyNoReferrerWhenDowngrade
    "origin" -> pure PageReferrerPolicyOrigin
    "originWhenCrossOrigin" -> pure PageReferrerPolicyOriginWhenCrossOrigin
    "sameOrigin" -> pure PageReferrerPolicySameOrigin
    "strictOrigin" -> pure PageReferrerPolicyStrictOrigin
    "strictOriginWhenCrossOrigin" -> pure PageReferrerPolicyStrictOriginWhenCrossOrigin
    "unsafeUrl" -> pure PageReferrerPolicyUnsafeUrl
    "_" -> fail "failed to parse PageReferrerPolicy"
instance ToJSON PageReferrerPolicy where
  toJSON v = A.String $ case v of
    PageReferrerPolicyNoReferrer -> "noReferrer"
    PageReferrerPolicyNoReferrerWhenDowngrade -> "noReferrerWhenDowngrade"
    PageReferrerPolicyOrigin -> "origin"
    PageReferrerPolicyOriginWhenCrossOrigin -> "originWhenCrossOrigin"
    PageReferrerPolicySameOrigin -> "sameOrigin"
    PageReferrerPolicyStrictOrigin -> "strictOrigin"
    PageReferrerPolicyStrictOriginWhenCrossOrigin -> "strictOriginWhenCrossOrigin"
    PageReferrerPolicyUnsafeUrl -> "unsafeUrl"

-- | Type 'Page.CompilationCacheParams'.
--   Per-script compilation cache parameters for `Page.produceCompilationCache`
data PageCompilationCacheParams = PageCompilationCacheParams
  {
    -- | The URL of the script to produce a compilation cache entry for.
    pageCompilationCacheParamsUrl :: T.Text,
    -- | A hint to the backend whether eager compilation is recommended.
    --   (the actual compilation mode used is upon backend discretion).
    pageCompilationCacheParamsEager :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON PageCompilationCacheParams where
  parseJSON = A.withObject "PageCompilationCacheParams" $ \o -> PageCompilationCacheParams
    <$> o A..: "url"
    <*> o A..:? "eager"
instance ToJSON PageCompilationCacheParams where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pageCompilationCacheParamsUrl p),
    ("eager" A..=) <$> (pageCompilationCacheParamsEager p)
    ]

-- | Type 'Page.NavigationType'.
--   The type of a frameNavigated event.
data PageNavigationType = PageNavigationTypeNavigation | PageNavigationTypeBackForwardCacheRestore
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageNavigationType where
  parseJSON = A.withText "PageNavigationType" $ \v -> case v of
    "Navigation" -> pure PageNavigationTypeNavigation
    "BackForwardCacheRestore" -> pure PageNavigationTypeBackForwardCacheRestore
    "_" -> fail "failed to parse PageNavigationType"
instance ToJSON PageNavigationType where
  toJSON v = A.String $ case v of
    PageNavigationTypeNavigation -> "Navigation"
    PageNavigationTypeBackForwardCacheRestore -> "BackForwardCacheRestore"

-- | Type 'Page.BackForwardCacheNotRestoredReason'.
--   List of not restored reasons for back-forward cache.
data PageBackForwardCacheNotRestoredReason = PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled | PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist | PageBackForwardCacheNotRestoredReasonHTTPStatusNotOK | PageBackForwardCacheNotRestoredReasonSchemeNotHTTPOrHTTPS | PageBackForwardCacheNotRestoredReasonLoading | PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess | PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled | PageBackForwardCacheNotRestoredReasonDomainNotAllowed | PageBackForwardCacheNotRestoredReasonHTTPMethodNotGET | PageBackForwardCacheNotRestoredReasonSubframeIsNavigating | PageBackForwardCacheNotRestoredReasonTimeout | PageBackForwardCacheNotRestoredReasonCacheLimit | PageBackForwardCacheNotRestoredReasonJavaScriptExecution | PageBackForwardCacheNotRestoredReasonRendererProcessKilled | PageBackForwardCacheNotRestoredReasonRendererProcessCrashed | PageBackForwardCacheNotRestoredReasonSchedulerTrackedFeatureUsed | PageBackForwardCacheNotRestoredReasonConflictingBrowsingInstance | PageBackForwardCacheNotRestoredReasonCacheFlushed | PageBackForwardCacheNotRestoredReasonServiceWorkerVersionActivation | PageBackForwardCacheNotRestoredReasonSessionRestored | PageBackForwardCacheNotRestoredReasonServiceWorkerPostMessage | PageBackForwardCacheNotRestoredReasonEnteredBackForwardCacheBeforeServiceWorkerHostAdded | PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_SameSite | PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_CrossSite | PageBackForwardCacheNotRestoredReasonServiceWorkerClaim | PageBackForwardCacheNotRestoredReasonIgnoreEventAndEvict | PageBackForwardCacheNotRestoredReasonHaveInnerContents | PageBackForwardCacheNotRestoredReasonTimeoutPuttingInCache | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByLowMemory | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByCommandLine | PageBackForwardCacheNotRestoredReasonNetworkRequestDatapipeDrainedAsBytesConsumer | PageBackForwardCacheNotRestoredReasonNetworkRequestRedirected | PageBackForwardCacheNotRestoredReasonNetworkRequestTimeout | PageBackForwardCacheNotRestoredReasonNetworkExceedsBufferLimit | PageBackForwardCacheNotRestoredReasonNavigationCancelledWhileRestoring | PageBackForwardCacheNotRestoredReasonNotMostRecentNavigationEntry | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForPrerender | PageBackForwardCacheNotRestoredReasonUserAgentOverrideDiffers | PageBackForwardCacheNotRestoredReasonForegroundCacheLimit | PageBackForwardCacheNotRestoredReasonBrowsingInstanceNotSwapped | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForDelegate | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInMainFrame | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInSubFrame | PageBackForwardCacheNotRestoredReasonServiceWorkerUnregistration | PageBackForwardCacheNotRestoredReasonCacheControlNoStore | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreCookieModified | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHTTPOnlyCookieModified | PageBackForwardCacheNotRestoredReasonNoResponseHead | PageBackForwardCacheNotRestoredReasonUnknown | PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857 | PageBackForwardCacheNotRestoredReasonErrorDocument | PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder | PageBackForwardCacheNotRestoredReasonWebSocket | PageBackForwardCacheNotRestoredReasonWebTransport | PageBackForwardCacheNotRestoredReasonWebRTC | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonContainsPlugins | PageBackForwardCacheNotRestoredReasonDocumentLoaded | PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers | PageBackForwardCacheNotRestoredReasonOutstandingIndexedDBTransaction | PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission | PageBackForwardCacheNotRestoredReasonRequestedMIDIPermission | PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors | PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission | PageBackForwardCacheNotRestoredReasonBroadcastChannel | PageBackForwardCacheNotRestoredReasonIndexedDBConnection | PageBackForwardCacheNotRestoredReasonWebXR | PageBackForwardCacheNotRestoredReasonSharedWorker | PageBackForwardCacheNotRestoredReasonWebLocks | PageBackForwardCacheNotRestoredReasonWebHID | PageBackForwardCacheNotRestoredReasonWebShare | PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant | PageBackForwardCacheNotRestoredReasonWebNfc | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXHR | PageBackForwardCacheNotRestoredReasonAppBanner | PageBackForwardCacheNotRestoredReasonPrinting | PageBackForwardCacheNotRestoredReasonWebDatabase | PageBackForwardCacheNotRestoredReasonPictureInPicture | PageBackForwardCacheNotRestoredReasonPortal | PageBackForwardCacheNotRestoredReasonSpeechRecognizer | PageBackForwardCacheNotRestoredReasonIdleManager | PageBackForwardCacheNotRestoredReasonPaymentManager | PageBackForwardCacheNotRestoredReasonSpeechSynthesis | PageBackForwardCacheNotRestoredReasonKeyboardLock | PageBackForwardCacheNotRestoredReasonWebOTPService | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket | PageBackForwardCacheNotRestoredReasonInjectedJavascript | PageBackForwardCacheNotRestoredReasonInjectedStyleSheet | PageBackForwardCacheNotRestoredReasonDummy | PageBackForwardCacheNotRestoredReasonContentSecurityHandler | PageBackForwardCacheNotRestoredReasonContentWebAuthenticationAPI | PageBackForwardCacheNotRestoredReasonContentFileChooser | PageBackForwardCacheNotRestoredReasonContentSerial | PageBackForwardCacheNotRestoredReasonContentFileSystemAccess | PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost | PageBackForwardCacheNotRestoredReasonContentWebBluetooth | PageBackForwardCacheNotRestoredReasonContentWebUSB | PageBackForwardCacheNotRestoredReasonContentMediaSessionService | PageBackForwardCacheNotRestoredReasonContentScreenReader | PageBackForwardCacheNotRestoredReasonEmbedderPopupBlockerTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingTriggeredPopupBlocker | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingThreatDetails | PageBackForwardCacheNotRestoredReasonEmbedderAppBannerManager | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerViewerSource | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerSelfDeletingRequestDelegate | PageBackForwardCacheNotRestoredReasonEmbedderOomInterventionTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderOfflinePage | PageBackForwardCacheNotRestoredReasonEmbedderChromePasswordManagerClientBindCredentialManager | PageBackForwardCacheNotRestoredReasonEmbedderPermissionRequestManager | PageBackForwardCacheNotRestoredReasonEmbedderModalDialog | PageBackForwardCacheNotRestoredReasonEmbedderExtensions | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessaging | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessagingForOpenPort | PageBackForwardCacheNotRestoredReasonEmbedderExtensionSentMessageToCachedFrame
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageBackForwardCacheNotRestoredReason where
  parseJSON = A.withText "PageBackForwardCacheNotRestoredReason" $ \v -> case v of
    "NotPrimaryMainFrame" -> pure PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame
    "BackForwardCacheDisabled" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled
    "RelatedActiveContentsExist" -> pure PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist
    "HTTPStatusNotOK" -> pure PageBackForwardCacheNotRestoredReasonHTTPStatusNotOK
    "SchemeNotHTTPOrHTTPS" -> pure PageBackForwardCacheNotRestoredReasonSchemeNotHTTPOrHTTPS
    "Loading" -> pure PageBackForwardCacheNotRestoredReasonLoading
    "WasGrantedMediaAccess" -> pure PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess
    "DisableForRenderFrameHostCalled" -> pure PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled
    "DomainNotAllowed" -> pure PageBackForwardCacheNotRestoredReasonDomainNotAllowed
    "HTTPMethodNotGET" -> pure PageBackForwardCacheNotRestoredReasonHTTPMethodNotGET
    "SubframeIsNavigating" -> pure PageBackForwardCacheNotRestoredReasonSubframeIsNavigating
    "Timeout" -> pure PageBackForwardCacheNotRestoredReasonTimeout
    "CacheLimit" -> pure PageBackForwardCacheNotRestoredReasonCacheLimit
    "JavaScriptExecution" -> pure PageBackForwardCacheNotRestoredReasonJavaScriptExecution
    "RendererProcessKilled" -> pure PageBackForwardCacheNotRestoredReasonRendererProcessKilled
    "RendererProcessCrashed" -> pure PageBackForwardCacheNotRestoredReasonRendererProcessCrashed
    "SchedulerTrackedFeatureUsed" -> pure PageBackForwardCacheNotRestoredReasonSchedulerTrackedFeatureUsed
    "ConflictingBrowsingInstance" -> pure PageBackForwardCacheNotRestoredReasonConflictingBrowsingInstance
    "CacheFlushed" -> pure PageBackForwardCacheNotRestoredReasonCacheFlushed
    "ServiceWorkerVersionActivation" -> pure PageBackForwardCacheNotRestoredReasonServiceWorkerVersionActivation
    "SessionRestored" -> pure PageBackForwardCacheNotRestoredReasonSessionRestored
    "ServiceWorkerPostMessage" -> pure PageBackForwardCacheNotRestoredReasonServiceWorkerPostMessage
    "EnteredBackForwardCacheBeforeServiceWorkerHostAdded" -> pure PageBackForwardCacheNotRestoredReasonEnteredBackForwardCacheBeforeServiceWorkerHostAdded
    "RenderFrameHostReused_SameSite" -> pure PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_SameSite
    "RenderFrameHostReused_CrossSite" -> pure PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_CrossSite
    "ServiceWorkerClaim" -> pure PageBackForwardCacheNotRestoredReasonServiceWorkerClaim
    "IgnoreEventAndEvict" -> pure PageBackForwardCacheNotRestoredReasonIgnoreEventAndEvict
    "HaveInnerContents" -> pure PageBackForwardCacheNotRestoredReasonHaveInnerContents
    "TimeoutPuttingInCache" -> pure PageBackForwardCacheNotRestoredReasonTimeoutPuttingInCache
    "BackForwardCacheDisabledByLowMemory" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByLowMemory
    "BackForwardCacheDisabledByCommandLine" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByCommandLine
    "NetworkRequestDatapipeDrainedAsBytesConsumer" -> pure PageBackForwardCacheNotRestoredReasonNetworkRequestDatapipeDrainedAsBytesConsumer
    "NetworkRequestRedirected" -> pure PageBackForwardCacheNotRestoredReasonNetworkRequestRedirected
    "NetworkRequestTimeout" -> pure PageBackForwardCacheNotRestoredReasonNetworkRequestTimeout
    "NetworkExceedsBufferLimit" -> pure PageBackForwardCacheNotRestoredReasonNetworkExceedsBufferLimit
    "NavigationCancelledWhileRestoring" -> pure PageBackForwardCacheNotRestoredReasonNavigationCancelledWhileRestoring
    "NotMostRecentNavigationEntry" -> pure PageBackForwardCacheNotRestoredReasonNotMostRecentNavigationEntry
    "BackForwardCacheDisabledForPrerender" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForPrerender
    "UserAgentOverrideDiffers" -> pure PageBackForwardCacheNotRestoredReasonUserAgentOverrideDiffers
    "ForegroundCacheLimit" -> pure PageBackForwardCacheNotRestoredReasonForegroundCacheLimit
    "BrowsingInstanceNotSwapped" -> pure PageBackForwardCacheNotRestoredReasonBrowsingInstanceNotSwapped
    "BackForwardCacheDisabledForDelegate" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForDelegate
    "UnloadHandlerExistsInMainFrame" -> pure PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInMainFrame
    "UnloadHandlerExistsInSubFrame" -> pure PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInSubFrame
    "ServiceWorkerUnregistration" -> pure PageBackForwardCacheNotRestoredReasonServiceWorkerUnregistration
    "CacheControlNoStore" -> pure PageBackForwardCacheNotRestoredReasonCacheControlNoStore
    "CacheControlNoStoreCookieModified" -> pure PageBackForwardCacheNotRestoredReasonCacheControlNoStoreCookieModified
    "CacheControlNoStoreHTTPOnlyCookieModified" -> pure PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHTTPOnlyCookieModified
    "NoResponseHead" -> pure PageBackForwardCacheNotRestoredReasonNoResponseHead
    "Unknown" -> pure PageBackForwardCacheNotRestoredReasonUnknown
    "ActivationNavigationsDisallowedForBug1234857" -> pure PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857
    "ErrorDocument" -> pure PageBackForwardCacheNotRestoredReasonErrorDocument
    "FencedFramesEmbedder" -> pure PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder
    "WebSocket" -> pure PageBackForwardCacheNotRestoredReasonWebSocket
    "WebTransport" -> pure PageBackForwardCacheNotRestoredReasonWebTransport
    "WebRTC" -> pure PageBackForwardCacheNotRestoredReasonWebRTC
    "MainResourceHasCacheControlNoStore" -> pure PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore
    "MainResourceHasCacheControlNoCache" -> pure PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache
    "SubresourceHasCacheControlNoStore" -> pure PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore
    "SubresourceHasCacheControlNoCache" -> pure PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache
    "ContainsPlugins" -> pure PageBackForwardCacheNotRestoredReasonContainsPlugins
    "DocumentLoaded" -> pure PageBackForwardCacheNotRestoredReasonDocumentLoaded
    "DedicatedWorkerOrWorklet" -> pure PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet
    "OutstandingNetworkRequestOthers" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers
    "OutstandingIndexedDBTransaction" -> pure PageBackForwardCacheNotRestoredReasonOutstandingIndexedDBTransaction
    "RequestedNotificationsPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission
    "RequestedMIDIPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedMIDIPermission
    "RequestedAudioCapturePermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission
    "RequestedVideoCapturePermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission
    "RequestedBackForwardCacheBlockedSensors" -> pure PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors
    "RequestedBackgroundWorkPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission
    "BroadcastChannel" -> pure PageBackForwardCacheNotRestoredReasonBroadcastChannel
    "IndexedDBConnection" -> pure PageBackForwardCacheNotRestoredReasonIndexedDBConnection
    "WebXR" -> pure PageBackForwardCacheNotRestoredReasonWebXR
    "SharedWorker" -> pure PageBackForwardCacheNotRestoredReasonSharedWorker
    "WebLocks" -> pure PageBackForwardCacheNotRestoredReasonWebLocks
    "WebHID" -> pure PageBackForwardCacheNotRestoredReasonWebHID
    "WebShare" -> pure PageBackForwardCacheNotRestoredReasonWebShare
    "RequestedStorageAccessGrant" -> pure PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant
    "WebNfc" -> pure PageBackForwardCacheNotRestoredReasonWebNfc
    "OutstandingNetworkRequestFetch" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch
    "OutstandingNetworkRequestXHR" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXHR
    "AppBanner" -> pure PageBackForwardCacheNotRestoredReasonAppBanner
    "Printing" -> pure PageBackForwardCacheNotRestoredReasonPrinting
    "WebDatabase" -> pure PageBackForwardCacheNotRestoredReasonWebDatabase
    "PictureInPicture" -> pure PageBackForwardCacheNotRestoredReasonPictureInPicture
    "Portal" -> pure PageBackForwardCacheNotRestoredReasonPortal
    "SpeechRecognizer" -> pure PageBackForwardCacheNotRestoredReasonSpeechRecognizer
    "IdleManager" -> pure PageBackForwardCacheNotRestoredReasonIdleManager
    "PaymentManager" -> pure PageBackForwardCacheNotRestoredReasonPaymentManager
    "SpeechSynthesis" -> pure PageBackForwardCacheNotRestoredReasonSpeechSynthesis
    "KeyboardLock" -> pure PageBackForwardCacheNotRestoredReasonKeyboardLock
    "WebOTPService" -> pure PageBackForwardCacheNotRestoredReasonWebOTPService
    "OutstandingNetworkRequestDirectSocket" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket
    "InjectedJavascript" -> pure PageBackForwardCacheNotRestoredReasonInjectedJavascript
    "InjectedStyleSheet" -> pure PageBackForwardCacheNotRestoredReasonInjectedStyleSheet
    "Dummy" -> pure PageBackForwardCacheNotRestoredReasonDummy
    "ContentSecurityHandler" -> pure PageBackForwardCacheNotRestoredReasonContentSecurityHandler
    "ContentWebAuthenticationAPI" -> pure PageBackForwardCacheNotRestoredReasonContentWebAuthenticationAPI
    "ContentFileChooser" -> pure PageBackForwardCacheNotRestoredReasonContentFileChooser
    "ContentSerial" -> pure PageBackForwardCacheNotRestoredReasonContentSerial
    "ContentFileSystemAccess" -> pure PageBackForwardCacheNotRestoredReasonContentFileSystemAccess
    "ContentMediaDevicesDispatcherHost" -> pure PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost
    "ContentWebBluetooth" -> pure PageBackForwardCacheNotRestoredReasonContentWebBluetooth
    "ContentWebUSB" -> pure PageBackForwardCacheNotRestoredReasonContentWebUSB
    "ContentMediaSessionService" -> pure PageBackForwardCacheNotRestoredReasonContentMediaSessionService
    "ContentScreenReader" -> pure PageBackForwardCacheNotRestoredReasonContentScreenReader
    "EmbedderPopupBlockerTabHelper" -> pure PageBackForwardCacheNotRestoredReasonEmbedderPopupBlockerTabHelper
    "EmbedderSafeBrowsingTriggeredPopupBlocker" -> pure PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingTriggeredPopupBlocker
    "EmbedderSafeBrowsingThreatDetails" -> pure PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingThreatDetails
    "EmbedderAppBannerManager" -> pure PageBackForwardCacheNotRestoredReasonEmbedderAppBannerManager
    "EmbedderDomDistillerViewerSource" -> pure PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerViewerSource
    "EmbedderDomDistillerSelfDeletingRequestDelegate" -> pure PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerSelfDeletingRequestDelegate
    "EmbedderOomInterventionTabHelper" -> pure PageBackForwardCacheNotRestoredReasonEmbedderOomInterventionTabHelper
    "EmbedderOfflinePage" -> pure PageBackForwardCacheNotRestoredReasonEmbedderOfflinePage
    "EmbedderChromePasswordManagerClientBindCredentialManager" -> pure PageBackForwardCacheNotRestoredReasonEmbedderChromePasswordManagerClientBindCredentialManager
    "EmbedderPermissionRequestManager" -> pure PageBackForwardCacheNotRestoredReasonEmbedderPermissionRequestManager
    "EmbedderModalDialog" -> pure PageBackForwardCacheNotRestoredReasonEmbedderModalDialog
    "EmbedderExtensions" -> pure PageBackForwardCacheNotRestoredReasonEmbedderExtensions
    "EmbedderExtensionMessaging" -> pure PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessaging
    "EmbedderExtensionMessagingForOpenPort" -> pure PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessagingForOpenPort
    "EmbedderExtensionSentMessageToCachedFrame" -> pure PageBackForwardCacheNotRestoredReasonEmbedderExtensionSentMessageToCachedFrame
    "_" -> fail "failed to parse PageBackForwardCacheNotRestoredReason"
instance ToJSON PageBackForwardCacheNotRestoredReason where
  toJSON v = A.String $ case v of
    PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame -> "NotPrimaryMainFrame"
    PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled -> "BackForwardCacheDisabled"
    PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist -> "RelatedActiveContentsExist"
    PageBackForwardCacheNotRestoredReasonHTTPStatusNotOK -> "HTTPStatusNotOK"
    PageBackForwardCacheNotRestoredReasonSchemeNotHTTPOrHTTPS -> "SchemeNotHTTPOrHTTPS"
    PageBackForwardCacheNotRestoredReasonLoading -> "Loading"
    PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess -> "WasGrantedMediaAccess"
    PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled -> "DisableForRenderFrameHostCalled"
    PageBackForwardCacheNotRestoredReasonDomainNotAllowed -> "DomainNotAllowed"
    PageBackForwardCacheNotRestoredReasonHTTPMethodNotGET -> "HTTPMethodNotGET"
    PageBackForwardCacheNotRestoredReasonSubframeIsNavigating -> "SubframeIsNavigating"
    PageBackForwardCacheNotRestoredReasonTimeout -> "Timeout"
    PageBackForwardCacheNotRestoredReasonCacheLimit -> "CacheLimit"
    PageBackForwardCacheNotRestoredReasonJavaScriptExecution -> "JavaScriptExecution"
    PageBackForwardCacheNotRestoredReasonRendererProcessKilled -> "RendererProcessKilled"
    PageBackForwardCacheNotRestoredReasonRendererProcessCrashed -> "RendererProcessCrashed"
    PageBackForwardCacheNotRestoredReasonSchedulerTrackedFeatureUsed -> "SchedulerTrackedFeatureUsed"
    PageBackForwardCacheNotRestoredReasonConflictingBrowsingInstance -> "ConflictingBrowsingInstance"
    PageBackForwardCacheNotRestoredReasonCacheFlushed -> "CacheFlushed"
    PageBackForwardCacheNotRestoredReasonServiceWorkerVersionActivation -> "ServiceWorkerVersionActivation"
    PageBackForwardCacheNotRestoredReasonSessionRestored -> "SessionRestored"
    PageBackForwardCacheNotRestoredReasonServiceWorkerPostMessage -> "ServiceWorkerPostMessage"
    PageBackForwardCacheNotRestoredReasonEnteredBackForwardCacheBeforeServiceWorkerHostAdded -> "EnteredBackForwardCacheBeforeServiceWorkerHostAdded"
    PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_SameSite -> "RenderFrameHostReused_SameSite"
    PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_CrossSite -> "RenderFrameHostReused_CrossSite"
    PageBackForwardCacheNotRestoredReasonServiceWorkerClaim -> "ServiceWorkerClaim"
    PageBackForwardCacheNotRestoredReasonIgnoreEventAndEvict -> "IgnoreEventAndEvict"
    PageBackForwardCacheNotRestoredReasonHaveInnerContents -> "HaveInnerContents"
    PageBackForwardCacheNotRestoredReasonTimeoutPuttingInCache -> "TimeoutPuttingInCache"
    PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByLowMemory -> "BackForwardCacheDisabledByLowMemory"
    PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByCommandLine -> "BackForwardCacheDisabledByCommandLine"
    PageBackForwardCacheNotRestoredReasonNetworkRequestDatapipeDrainedAsBytesConsumer -> "NetworkRequestDatapipeDrainedAsBytesConsumer"
    PageBackForwardCacheNotRestoredReasonNetworkRequestRedirected -> "NetworkRequestRedirected"
    PageBackForwardCacheNotRestoredReasonNetworkRequestTimeout -> "NetworkRequestTimeout"
    PageBackForwardCacheNotRestoredReasonNetworkExceedsBufferLimit -> "NetworkExceedsBufferLimit"
    PageBackForwardCacheNotRestoredReasonNavigationCancelledWhileRestoring -> "NavigationCancelledWhileRestoring"
    PageBackForwardCacheNotRestoredReasonNotMostRecentNavigationEntry -> "NotMostRecentNavigationEntry"
    PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForPrerender -> "BackForwardCacheDisabledForPrerender"
    PageBackForwardCacheNotRestoredReasonUserAgentOverrideDiffers -> "UserAgentOverrideDiffers"
    PageBackForwardCacheNotRestoredReasonForegroundCacheLimit -> "ForegroundCacheLimit"
    PageBackForwardCacheNotRestoredReasonBrowsingInstanceNotSwapped -> "BrowsingInstanceNotSwapped"
    PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForDelegate -> "BackForwardCacheDisabledForDelegate"
    PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInMainFrame -> "UnloadHandlerExistsInMainFrame"
    PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInSubFrame -> "UnloadHandlerExistsInSubFrame"
    PageBackForwardCacheNotRestoredReasonServiceWorkerUnregistration -> "ServiceWorkerUnregistration"
    PageBackForwardCacheNotRestoredReasonCacheControlNoStore -> "CacheControlNoStore"
    PageBackForwardCacheNotRestoredReasonCacheControlNoStoreCookieModified -> "CacheControlNoStoreCookieModified"
    PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHTTPOnlyCookieModified -> "CacheControlNoStoreHTTPOnlyCookieModified"
    PageBackForwardCacheNotRestoredReasonNoResponseHead -> "NoResponseHead"
    PageBackForwardCacheNotRestoredReasonUnknown -> "Unknown"
    PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857 -> "ActivationNavigationsDisallowedForBug1234857"
    PageBackForwardCacheNotRestoredReasonErrorDocument -> "ErrorDocument"
    PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder -> "FencedFramesEmbedder"
    PageBackForwardCacheNotRestoredReasonWebSocket -> "WebSocket"
    PageBackForwardCacheNotRestoredReasonWebTransport -> "WebTransport"
    PageBackForwardCacheNotRestoredReasonWebRTC -> "WebRTC"
    PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore -> "MainResourceHasCacheControlNoStore"
    PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache -> "MainResourceHasCacheControlNoCache"
    PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore -> "SubresourceHasCacheControlNoStore"
    PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache -> "SubresourceHasCacheControlNoCache"
    PageBackForwardCacheNotRestoredReasonContainsPlugins -> "ContainsPlugins"
    PageBackForwardCacheNotRestoredReasonDocumentLoaded -> "DocumentLoaded"
    PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet -> "DedicatedWorkerOrWorklet"
    PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers -> "OutstandingNetworkRequestOthers"
    PageBackForwardCacheNotRestoredReasonOutstandingIndexedDBTransaction -> "OutstandingIndexedDBTransaction"
    PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission -> "RequestedNotificationsPermission"
    PageBackForwardCacheNotRestoredReasonRequestedMIDIPermission -> "RequestedMIDIPermission"
    PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission -> "RequestedAudioCapturePermission"
    PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission -> "RequestedVideoCapturePermission"
    PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors -> "RequestedBackForwardCacheBlockedSensors"
    PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission -> "RequestedBackgroundWorkPermission"
    PageBackForwardCacheNotRestoredReasonBroadcastChannel -> "BroadcastChannel"
    PageBackForwardCacheNotRestoredReasonIndexedDBConnection -> "IndexedDBConnection"
    PageBackForwardCacheNotRestoredReasonWebXR -> "WebXR"
    PageBackForwardCacheNotRestoredReasonSharedWorker -> "SharedWorker"
    PageBackForwardCacheNotRestoredReasonWebLocks -> "WebLocks"
    PageBackForwardCacheNotRestoredReasonWebHID -> "WebHID"
    PageBackForwardCacheNotRestoredReasonWebShare -> "WebShare"
    PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant -> "RequestedStorageAccessGrant"
    PageBackForwardCacheNotRestoredReasonWebNfc -> "WebNfc"
    PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch -> "OutstandingNetworkRequestFetch"
    PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXHR -> "OutstandingNetworkRequestXHR"
    PageBackForwardCacheNotRestoredReasonAppBanner -> "AppBanner"
    PageBackForwardCacheNotRestoredReasonPrinting -> "Printing"
    PageBackForwardCacheNotRestoredReasonWebDatabase -> "WebDatabase"
    PageBackForwardCacheNotRestoredReasonPictureInPicture -> "PictureInPicture"
    PageBackForwardCacheNotRestoredReasonPortal -> "Portal"
    PageBackForwardCacheNotRestoredReasonSpeechRecognizer -> "SpeechRecognizer"
    PageBackForwardCacheNotRestoredReasonIdleManager -> "IdleManager"
    PageBackForwardCacheNotRestoredReasonPaymentManager -> "PaymentManager"
    PageBackForwardCacheNotRestoredReasonSpeechSynthesis -> "SpeechSynthesis"
    PageBackForwardCacheNotRestoredReasonKeyboardLock -> "KeyboardLock"
    PageBackForwardCacheNotRestoredReasonWebOTPService -> "WebOTPService"
    PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket -> "OutstandingNetworkRequestDirectSocket"
    PageBackForwardCacheNotRestoredReasonInjectedJavascript -> "InjectedJavascript"
    PageBackForwardCacheNotRestoredReasonInjectedStyleSheet -> "InjectedStyleSheet"
    PageBackForwardCacheNotRestoredReasonDummy -> "Dummy"
    PageBackForwardCacheNotRestoredReasonContentSecurityHandler -> "ContentSecurityHandler"
    PageBackForwardCacheNotRestoredReasonContentWebAuthenticationAPI -> "ContentWebAuthenticationAPI"
    PageBackForwardCacheNotRestoredReasonContentFileChooser -> "ContentFileChooser"
    PageBackForwardCacheNotRestoredReasonContentSerial -> "ContentSerial"
    PageBackForwardCacheNotRestoredReasonContentFileSystemAccess -> "ContentFileSystemAccess"
    PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost -> "ContentMediaDevicesDispatcherHost"
    PageBackForwardCacheNotRestoredReasonContentWebBluetooth -> "ContentWebBluetooth"
    PageBackForwardCacheNotRestoredReasonContentWebUSB -> "ContentWebUSB"
    PageBackForwardCacheNotRestoredReasonContentMediaSessionService -> "ContentMediaSessionService"
    PageBackForwardCacheNotRestoredReasonContentScreenReader -> "ContentScreenReader"
    PageBackForwardCacheNotRestoredReasonEmbedderPopupBlockerTabHelper -> "EmbedderPopupBlockerTabHelper"
    PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingTriggeredPopupBlocker -> "EmbedderSafeBrowsingTriggeredPopupBlocker"
    PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingThreatDetails -> "EmbedderSafeBrowsingThreatDetails"
    PageBackForwardCacheNotRestoredReasonEmbedderAppBannerManager -> "EmbedderAppBannerManager"
    PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerViewerSource -> "EmbedderDomDistillerViewerSource"
    PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerSelfDeletingRequestDelegate -> "EmbedderDomDistillerSelfDeletingRequestDelegate"
    PageBackForwardCacheNotRestoredReasonEmbedderOomInterventionTabHelper -> "EmbedderOomInterventionTabHelper"
    PageBackForwardCacheNotRestoredReasonEmbedderOfflinePage -> "EmbedderOfflinePage"
    PageBackForwardCacheNotRestoredReasonEmbedderChromePasswordManagerClientBindCredentialManager -> "EmbedderChromePasswordManagerClientBindCredentialManager"
    PageBackForwardCacheNotRestoredReasonEmbedderPermissionRequestManager -> "EmbedderPermissionRequestManager"
    PageBackForwardCacheNotRestoredReasonEmbedderModalDialog -> "EmbedderModalDialog"
    PageBackForwardCacheNotRestoredReasonEmbedderExtensions -> "EmbedderExtensions"
    PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessaging -> "EmbedderExtensionMessaging"
    PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessagingForOpenPort -> "EmbedderExtensionMessagingForOpenPort"
    PageBackForwardCacheNotRestoredReasonEmbedderExtensionSentMessageToCachedFrame -> "EmbedderExtensionSentMessageToCachedFrame"

-- | Type 'Page.BackForwardCacheNotRestoredReasonType'.
--   Types of not restored reasons for back-forward cache.
data PageBackForwardCacheNotRestoredReasonType = PageBackForwardCacheNotRestoredReasonTypeSupportPending | PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded | PageBackForwardCacheNotRestoredReasonTypeCircumstantial
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageBackForwardCacheNotRestoredReasonType where
  parseJSON = A.withText "PageBackForwardCacheNotRestoredReasonType" $ \v -> case v of
    "SupportPending" -> pure PageBackForwardCacheNotRestoredReasonTypeSupportPending
    "PageSupportNeeded" -> pure PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded
    "Circumstantial" -> pure PageBackForwardCacheNotRestoredReasonTypeCircumstantial
    "_" -> fail "failed to parse PageBackForwardCacheNotRestoredReasonType"
instance ToJSON PageBackForwardCacheNotRestoredReasonType where
  toJSON v = A.String $ case v of
    PageBackForwardCacheNotRestoredReasonTypeSupportPending -> "SupportPending"
    PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded -> "PageSupportNeeded"
    PageBackForwardCacheNotRestoredReasonTypeCircumstantial -> "Circumstantial"

-- | Type 'Page.BackForwardCacheNotRestoredExplanation'.
data PageBackForwardCacheNotRestoredExplanation = PageBackForwardCacheNotRestoredExplanation
  {
    -- | Type of the reason
    pageBackForwardCacheNotRestoredExplanationType :: PageBackForwardCacheNotRestoredReasonType,
    -- | Not restored reason
    pageBackForwardCacheNotRestoredExplanationReason :: PageBackForwardCacheNotRestoredReason,
    -- | Context associated with the reason. The meaning of this context is
    --   dependent on the reason:
    --   - EmbedderExtensionSentMessageToCachedFrame: the extension ID.
    pageBackForwardCacheNotRestoredExplanationContext :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageBackForwardCacheNotRestoredExplanation where
  parseJSON = A.withObject "PageBackForwardCacheNotRestoredExplanation" $ \o -> PageBackForwardCacheNotRestoredExplanation
    <$> o A..: "type"
    <*> o A..: "reason"
    <*> o A..:? "context"
instance ToJSON PageBackForwardCacheNotRestoredExplanation where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pageBackForwardCacheNotRestoredExplanationType p),
    ("reason" A..=) <$> Just (pageBackForwardCacheNotRestoredExplanationReason p),
    ("context" A..=) <$> (pageBackForwardCacheNotRestoredExplanationContext p)
    ]

-- | Type 'Page.BackForwardCacheNotRestoredExplanationTree'.
data PageBackForwardCacheNotRestoredExplanationTree = PageBackForwardCacheNotRestoredExplanationTree
  {
    -- | URL of each frame
    pageBackForwardCacheNotRestoredExplanationTreeUrl :: T.Text,
    -- | Not restored reasons of each frame
    pageBackForwardCacheNotRestoredExplanationTreeExplanations :: [PageBackForwardCacheNotRestoredExplanation],
    -- | Array of children frame
    pageBackForwardCacheNotRestoredExplanationTreeChildren :: [PageBackForwardCacheNotRestoredExplanationTree]
  }
  deriving (Eq, Show)
instance FromJSON PageBackForwardCacheNotRestoredExplanationTree where
  parseJSON = A.withObject "PageBackForwardCacheNotRestoredExplanationTree" $ \o -> PageBackForwardCacheNotRestoredExplanationTree
    <$> o A..: "url"
    <*> o A..: "explanations"
    <*> o A..: "children"
instance ToJSON PageBackForwardCacheNotRestoredExplanationTree where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pageBackForwardCacheNotRestoredExplanationTreeUrl p),
    ("explanations" A..=) <$> Just (pageBackForwardCacheNotRestoredExplanationTreeExplanations p),
    ("children" A..=) <$> Just (pageBackForwardCacheNotRestoredExplanationTreeChildren p)
    ]

-- | Type 'Page.PrerenderFinalStatus'.
--   List of FinalStatus reasons for Prerender2.
data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusMemoryLimitExceeded | PagePrerenderFinalStatusFailToGetMemoryUsage | PagePrerenderFinalStatusDataSaverEnabled | PagePrerenderFinalStatusHasEffectiveUrl | PagePrerenderFinalStatusActivatedBeforeStarted | PagePrerenderFinalStatusInactivePageRestriction | PagePrerenderFinalStatusStartFailed | PagePrerenderFinalStatusTimeoutBackgrounded
  deriving (Ord, Eq, Show, Read)
instance FromJSON PagePrerenderFinalStatus where
  parseJSON = A.withText "PagePrerenderFinalStatus" $ \v -> case v of
    "Activated" -> pure PagePrerenderFinalStatusActivated
    "Destroyed" -> pure PagePrerenderFinalStatusDestroyed
    "LowEndDevice" -> pure PagePrerenderFinalStatusLowEndDevice
    "CrossOriginRedirect" -> pure PagePrerenderFinalStatusCrossOriginRedirect
    "CrossOriginNavigation" -> pure PagePrerenderFinalStatusCrossOriginNavigation
    "InvalidSchemeRedirect" -> pure PagePrerenderFinalStatusInvalidSchemeRedirect
    "InvalidSchemeNavigation" -> pure PagePrerenderFinalStatusInvalidSchemeNavigation
    "InProgressNavigation" -> pure PagePrerenderFinalStatusInProgressNavigation
    "NavigationRequestBlockedByCsp" -> pure PagePrerenderFinalStatusNavigationRequestBlockedByCsp
    "MainFrameNavigation" -> pure PagePrerenderFinalStatusMainFrameNavigation
    "MojoBinderPolicy" -> pure PagePrerenderFinalStatusMojoBinderPolicy
    "RendererProcessCrashed" -> pure PagePrerenderFinalStatusRendererProcessCrashed
    "RendererProcessKilled" -> pure PagePrerenderFinalStatusRendererProcessKilled
    "Download" -> pure PagePrerenderFinalStatusDownload
    "TriggerDestroyed" -> pure PagePrerenderFinalStatusTriggerDestroyed
    "NavigationNotCommitted" -> pure PagePrerenderFinalStatusNavigationNotCommitted
    "NavigationBadHttpStatus" -> pure PagePrerenderFinalStatusNavigationBadHttpStatus
    "ClientCertRequested" -> pure PagePrerenderFinalStatusClientCertRequested
    "NavigationRequestNetworkError" -> pure PagePrerenderFinalStatusNavigationRequestNetworkError
    "MaxNumOfRunningPrerendersExceeded" -> pure PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded
    "CancelAllHostsForTesting" -> pure PagePrerenderFinalStatusCancelAllHostsForTesting
    "DidFailLoad" -> pure PagePrerenderFinalStatusDidFailLoad
    "Stop" -> pure PagePrerenderFinalStatusStop
    "SslCertificateError" -> pure PagePrerenderFinalStatusSslCertificateError
    "LoginAuthRequested" -> pure PagePrerenderFinalStatusLoginAuthRequested
    "UaChangeRequiresReload" -> pure PagePrerenderFinalStatusUaChangeRequiresReload
    "BlockedByClient" -> pure PagePrerenderFinalStatusBlockedByClient
    "AudioOutputDeviceRequested" -> pure PagePrerenderFinalStatusAudioOutputDeviceRequested
    "MixedContent" -> pure PagePrerenderFinalStatusMixedContent
    "TriggerBackgrounded" -> pure PagePrerenderFinalStatusTriggerBackgrounded
    "EmbedderTriggeredAndCrossOriginRedirected" -> pure PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
    "MemoryLimitExceeded" -> pure PagePrerenderFinalStatusMemoryLimitExceeded
    "FailToGetMemoryUsage" -> pure PagePrerenderFinalStatusFailToGetMemoryUsage
    "DataSaverEnabled" -> pure PagePrerenderFinalStatusDataSaverEnabled
    "HasEffectiveUrl" -> pure PagePrerenderFinalStatusHasEffectiveUrl
    "ActivatedBeforeStarted" -> pure PagePrerenderFinalStatusActivatedBeforeStarted
    "InactivePageRestriction" -> pure PagePrerenderFinalStatusInactivePageRestriction
    "StartFailed" -> pure PagePrerenderFinalStatusStartFailed
    "TimeoutBackgrounded" -> pure PagePrerenderFinalStatusTimeoutBackgrounded
    "_" -> fail "failed to parse PagePrerenderFinalStatus"
instance ToJSON PagePrerenderFinalStatus where
  toJSON v = A.String $ case v of
    PagePrerenderFinalStatusActivated -> "Activated"
    PagePrerenderFinalStatusDestroyed -> "Destroyed"
    PagePrerenderFinalStatusLowEndDevice -> "LowEndDevice"
    PagePrerenderFinalStatusCrossOriginRedirect -> "CrossOriginRedirect"
    PagePrerenderFinalStatusCrossOriginNavigation -> "CrossOriginNavigation"
    PagePrerenderFinalStatusInvalidSchemeRedirect -> "InvalidSchemeRedirect"
    PagePrerenderFinalStatusInvalidSchemeNavigation -> "InvalidSchemeNavigation"
    PagePrerenderFinalStatusInProgressNavigation -> "InProgressNavigation"
    PagePrerenderFinalStatusNavigationRequestBlockedByCsp -> "NavigationRequestBlockedByCsp"
    PagePrerenderFinalStatusMainFrameNavigation -> "MainFrameNavigation"
    PagePrerenderFinalStatusMojoBinderPolicy -> "MojoBinderPolicy"
    PagePrerenderFinalStatusRendererProcessCrashed -> "RendererProcessCrashed"
    PagePrerenderFinalStatusRendererProcessKilled -> "RendererProcessKilled"
    PagePrerenderFinalStatusDownload -> "Download"
    PagePrerenderFinalStatusTriggerDestroyed -> "TriggerDestroyed"
    PagePrerenderFinalStatusNavigationNotCommitted -> "NavigationNotCommitted"
    PagePrerenderFinalStatusNavigationBadHttpStatus -> "NavigationBadHttpStatus"
    PagePrerenderFinalStatusClientCertRequested -> "ClientCertRequested"
    PagePrerenderFinalStatusNavigationRequestNetworkError -> "NavigationRequestNetworkError"
    PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded -> "MaxNumOfRunningPrerendersExceeded"
    PagePrerenderFinalStatusCancelAllHostsForTesting -> "CancelAllHostsForTesting"
    PagePrerenderFinalStatusDidFailLoad -> "DidFailLoad"
    PagePrerenderFinalStatusStop -> "Stop"
    PagePrerenderFinalStatusSslCertificateError -> "SslCertificateError"
    PagePrerenderFinalStatusLoginAuthRequested -> "LoginAuthRequested"
    PagePrerenderFinalStatusUaChangeRequiresReload -> "UaChangeRequiresReload"
    PagePrerenderFinalStatusBlockedByClient -> "BlockedByClient"
    PagePrerenderFinalStatusAudioOutputDeviceRequested -> "AudioOutputDeviceRequested"
    PagePrerenderFinalStatusMixedContent -> "MixedContent"
    PagePrerenderFinalStatusTriggerBackgrounded -> "TriggerBackgrounded"
    PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected -> "EmbedderTriggeredAndCrossOriginRedirected"
    PagePrerenderFinalStatusMemoryLimitExceeded -> "MemoryLimitExceeded"
    PagePrerenderFinalStatusFailToGetMemoryUsage -> "FailToGetMemoryUsage"
    PagePrerenderFinalStatusDataSaverEnabled -> "DataSaverEnabled"
    PagePrerenderFinalStatusHasEffectiveUrl -> "HasEffectiveUrl"
    PagePrerenderFinalStatusActivatedBeforeStarted -> "ActivatedBeforeStarted"
    PagePrerenderFinalStatusInactivePageRestriction -> "InactivePageRestriction"
    PagePrerenderFinalStatusStartFailed -> "StartFailed"
    PagePrerenderFinalStatusTimeoutBackgrounded -> "TimeoutBackgrounded"

-- | Type of the 'Page.domContentEventFired' event.
data PageDomContentEventFired = PageDomContentEventFired
  {
    pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON PageDomContentEventFired where
  parseJSON = A.withObject "PageDomContentEventFired" $ \o -> PageDomContentEventFired
    <$> o A..: "timestamp"
instance Event PageDomContentEventFired where
  eventName _ = "Page.domContentEventFired"

-- | Type of the 'Page.fileChooserOpened' event.
data PageFileChooserOpenedMode = PageFileChooserOpenedModeSelectSingle | PageFileChooserOpenedModeSelectMultiple
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageFileChooserOpenedMode where
  parseJSON = A.withText "PageFileChooserOpenedMode" $ \v -> case v of
    "selectSingle" -> pure PageFileChooserOpenedModeSelectSingle
    "selectMultiple" -> pure PageFileChooserOpenedModeSelectMultiple
    "_" -> fail "failed to parse PageFileChooserOpenedMode"
instance ToJSON PageFileChooserOpenedMode where
  toJSON v = A.String $ case v of
    PageFileChooserOpenedModeSelectSingle -> "selectSingle"
    PageFileChooserOpenedModeSelectMultiple -> "selectMultiple"
data PageFileChooserOpened = PageFileChooserOpened
  {
    -- | Id of the frame containing input node.
    pageFileChooserOpenedFrameId :: PageFrameId,
    -- | Input mode.
    pageFileChooserOpenedMode :: PageFileChooserOpenedMode,
    -- | Input node id. Only present for file choosers opened via an <input type="file"> element.
    pageFileChooserOpenedBackendNodeId :: Maybe DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON PageFileChooserOpened where
  parseJSON = A.withObject "PageFileChooserOpened" $ \o -> PageFileChooserOpened
    <$> o A..: "frameId"
    <*> o A..: "mode"
    <*> o A..:? "backendNodeId"
instance Event PageFileChooserOpened where
  eventName _ = "Page.fileChooserOpened"

-- | Type of the 'Page.frameAttached' event.
data PageFrameAttached = PageFrameAttached
  {
    -- | Id of the frame that has been attached.
    pageFrameAttachedFrameId :: PageFrameId,
    -- | Parent frame identifier.
    pageFrameAttachedParentFrameId :: PageFrameId,
    -- | JavaScript stack trace of when frame was attached, only set if frame initiated from script.
    pageFrameAttachedStack :: Maybe Runtime.RuntimeStackTrace
  }
  deriving (Eq, Show)
instance FromJSON PageFrameAttached where
  parseJSON = A.withObject "PageFrameAttached" $ \o -> PageFrameAttached
    <$> o A..: "frameId"
    <*> o A..: "parentFrameId"
    <*> o A..:? "stack"
instance Event PageFrameAttached where
  eventName _ = "Page.frameAttached"

-- | Type of the 'Page.frameDetached' event.
data PageFrameDetachedReason = PageFrameDetachedReasonRemove | PageFrameDetachedReasonSwap
  deriving (Ord, Eq, Show, Read)
instance FromJSON PageFrameDetachedReason where
  parseJSON = A.withText "PageFrameDetachedReason" $ \v -> case v of
    "remove" -> pure PageFrameDetachedReasonRemove
    "swap" -> pure PageFrameDetachedReasonSwap
    "_" -> fail "failed to parse PageFrameDetachedReason"
instance ToJSON PageFrameDetachedReason where
  toJSON v = A.String $ case v of
    PageFrameDetachedReasonRemove -> "remove"
    PageFrameDetachedReasonSwap -> "swap"
data PageFrameDetached = PageFrameDetached
  {
    -- | Id of the frame that has been detached.
    pageFrameDetachedFrameId :: PageFrameId,
    pageFrameDetachedReason :: PageFrameDetachedReason
  }
  deriving (Eq, Show)
instance FromJSON PageFrameDetached where
  parseJSON = A.withObject "PageFrameDetached" $ \o -> PageFrameDetached
    <$> o A..: "frameId"
    <*> o A..: "reason"
instance Event PageFrameDetached where
  eventName _ = "Page.frameDetached"

-- | Type of the 'Page.frameNavigated' event.
data PageFrameNavigated = PageFrameNavigated
  {
    -- | Frame object.
    pageFrameNavigatedFrame :: PageFrame,
    pageFrameNavigatedType :: PageNavigationType
  }
  deriving (Eq, Show)
instance FromJSON PageFrameNavigated where
  parseJSON = A.withObject "PageFrameNavigated" $ \o -> PageFrameNavigated
    <$> o A..: "frame"
    <*> o A..: "type"
instance Event PageFrameNavigated where
  eventName _ = "Page.frameNavigated"

-- | Type of the 'Page.documentOpened' event.
data PageDocumentOpened = PageDocumentOpened
  {
    -- | Frame object.
    pageDocumentOpenedFrame :: PageFrame
  }
  deriving (Eq, Show)
instance FromJSON PageDocumentOpened where
  parseJSON = A.withObject "PageDocumentOpened" $ \o -> PageDocumentOpened
    <$> o A..: "frame"
instance Event PageDocumentOpened where
  eventName _ = "Page.documentOpened"

-- | Type of the 'Page.frameResized' event.
data PageFrameResized = PageFrameResized
  deriving (Eq, Show, Read)
instance FromJSON PageFrameResized where
  parseJSON _ = pure PageFrameResized
instance Event PageFrameResized where
  eventName _ = "Page.frameResized"

-- | Type of the 'Page.frameRequestedNavigation' event.
data PageFrameRequestedNavigation = PageFrameRequestedNavigation
  {
    -- | Id of the frame that is being navigated.
    pageFrameRequestedNavigationFrameId :: PageFrameId,
    -- | The reason for the navigation.
    pageFrameRequestedNavigationReason :: PageClientNavigationReason,
    -- | The destination URL for the requested navigation.
    pageFrameRequestedNavigationUrl :: T.Text,
    -- | The disposition for the navigation.
    pageFrameRequestedNavigationDisposition :: PageClientNavigationDisposition
  }
  deriving (Eq, Show)
instance FromJSON PageFrameRequestedNavigation where
  parseJSON = A.withObject "PageFrameRequestedNavigation" $ \o -> PageFrameRequestedNavigation
    <$> o A..: "frameId"
    <*> o A..: "reason"
    <*> o A..: "url"
    <*> o A..: "disposition"
instance Event PageFrameRequestedNavigation where
  eventName _ = "Page.frameRequestedNavigation"

-- | Type of the 'Page.frameStartedLoading' event.
data PageFrameStartedLoading = PageFrameStartedLoading
  {
    -- | Id of the frame that has started loading.
    pageFrameStartedLoadingFrameId :: PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON PageFrameStartedLoading where
  parseJSON = A.withObject "PageFrameStartedLoading" $ \o -> PageFrameStartedLoading
    <$> o A..: "frameId"
instance Event PageFrameStartedLoading where
  eventName _ = "Page.frameStartedLoading"

-- | Type of the 'Page.frameStoppedLoading' event.
data PageFrameStoppedLoading = PageFrameStoppedLoading
  {
    -- | Id of the frame that has stopped loading.
    pageFrameStoppedLoadingFrameId :: PageFrameId
  }
  deriving (Eq, Show)
instance FromJSON PageFrameStoppedLoading where
  parseJSON = A.withObject "PageFrameStoppedLoading" $ \o -> PageFrameStoppedLoading
    <$> o A..: "frameId"
instance Event PageFrameStoppedLoading where
  eventName _ = "Page.frameStoppedLoading"

-- | Type of the 'Page.interstitialHidden' event.
data PageInterstitialHidden = PageInterstitialHidden
  deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
  parseJSON _ = pure PageInterstitialHidden
instance Event PageInterstitialHidden where
  eventName _ = "Page.interstitialHidden"

-- | Type of the 'Page.interstitialShown' event.
data PageInterstitialShown = PageInterstitialShown
  deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
  parseJSON _ = pure PageInterstitialShown
instance Event PageInterstitialShown where
  eventName _ = "Page.interstitialShown"

-- | Type of the 'Page.javascriptDialogClosed' event.
data PageJavascriptDialogClosed = PageJavascriptDialogClosed
  {
    -- | Whether dialog was confirmed.
    pageJavascriptDialogClosedResult :: Bool,
    -- | User input in case of prompt.
    pageJavascriptDialogClosedUserInput :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageJavascriptDialogClosed where
  parseJSON = A.withObject "PageJavascriptDialogClosed" $ \o -> PageJavascriptDialogClosed
    <$> o A..: "result"
    <*> o A..: "userInput"
instance Event PageJavascriptDialogClosed where
  eventName _ = "Page.javascriptDialogClosed"

-- | Type of the 'Page.javascriptDialogOpening' event.
data PageJavascriptDialogOpening = PageJavascriptDialogOpening
  {
    -- | Frame url.
    pageJavascriptDialogOpeningUrl :: T.Text,
    -- | Message that will be displayed by the dialog.
    pageJavascriptDialogOpeningMessage :: T.Text,
    -- | Dialog type.
    pageJavascriptDialogOpeningType :: PageDialogType,
    -- | True iff browser is capable showing or acting on the given dialog. When browser has no
    --   dialog handler for given target, calling alert while Page domain is engaged will stall
    --   the page execution. Execution can be resumed via calling Page.handleJavaScriptDialog.
    pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
    -- | Default dialog prompt.
    pageJavascriptDialogOpeningDefaultPrompt :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageJavascriptDialogOpening where
  parseJSON = A.withObject "PageJavascriptDialogOpening" $ \o -> PageJavascriptDialogOpening
    <$> o A..: "url"
    <*> o A..: "message"
    <*> o A..: "type"
    <*> o A..: "hasBrowserHandler"
    <*> o A..:? "defaultPrompt"
instance Event PageJavascriptDialogOpening where
  eventName _ = "Page.javascriptDialogOpening"

-- | Type of the 'Page.lifecycleEvent' event.
data PageLifecycleEvent = PageLifecycleEvent
  {
    -- | Id of the frame.
    pageLifecycleEventFrameId :: PageFrameId,
    -- | Loader identifier. Empty string if the request is fetched from worker.
    pageLifecycleEventLoaderId :: NetworkLoaderId,
    pageLifecycleEventName :: T.Text,
    pageLifecycleEventTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON PageLifecycleEvent where
  parseJSON = A.withObject "PageLifecycleEvent" $ \o -> PageLifecycleEvent
    <$> o A..: "frameId"
    <*> o A..: "loaderId"
    <*> o A..: "name"
    <*> o A..: "timestamp"
instance Event PageLifecycleEvent where
  eventName _ = "Page.lifecycleEvent"

-- | Type of the 'Page.backForwardCacheNotUsed' event.
data PageBackForwardCacheNotUsed = PageBackForwardCacheNotUsed
  {
    -- | The loader id for the associated navgation.
    pageBackForwardCacheNotUsedLoaderId :: NetworkLoaderId,
    -- | The frame id of the associated frame.
    pageBackForwardCacheNotUsedFrameId :: PageFrameId,
    -- | Array of reasons why the page could not be cached. This must not be empty.
    pageBackForwardCacheNotUsedNotRestoredExplanations :: [PageBackForwardCacheNotRestoredExplanation],
    -- | Tree structure of reasons why the page could not be cached for each frame.
    pageBackForwardCacheNotUsedNotRestoredExplanationsTree :: Maybe PageBackForwardCacheNotRestoredExplanationTree
  }
  deriving (Eq, Show)
instance FromJSON PageBackForwardCacheNotUsed where
  parseJSON = A.withObject "PageBackForwardCacheNotUsed" $ \o -> PageBackForwardCacheNotUsed
    <$> o A..: "loaderId"
    <*> o A..: "frameId"
    <*> o A..: "notRestoredExplanations"
    <*> o A..:? "notRestoredExplanationsTree"
instance Event PageBackForwardCacheNotUsed where
  eventName _ = "Page.backForwardCacheNotUsed"

-- | Type of the 'Page.prerenderAttemptCompleted' event.
data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted
  {
    -- | The frame id of the frame initiating prerendering.
    pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
    pagePrerenderAttemptCompletedPrerenderingUrl :: T.Text,
    pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus,
    -- | This is used to give users more information about the name of the API call
    --   that is incompatible with prerender and has caused the cancellation of the attempt
    pagePrerenderAttemptCompletedDisallowedApiMethod :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PagePrerenderAttemptCompleted where
  parseJSON = A.withObject "PagePrerenderAttemptCompleted" $ \o -> PagePrerenderAttemptCompleted
    <$> o A..: "initiatingFrameId"
    <*> o A..: "prerenderingUrl"
    <*> o A..: "finalStatus"
    <*> o A..:? "disallowedApiMethod"
instance Event PagePrerenderAttemptCompleted where
  eventName _ = "Page.prerenderAttemptCompleted"

-- | Type of the 'Page.loadEventFired' event.
data PageLoadEventFired = PageLoadEventFired
  {
    pageLoadEventFiredTimestamp :: NetworkMonotonicTime
  }
  deriving (Eq, Show)
instance FromJSON PageLoadEventFired where
  parseJSON = A.withObject "PageLoadEventFired" $ \o -> PageLoadEventFired
    <$> o A..: "timestamp"
instance Event PageLoadEventFired where
  eventName _ = "Page.loadEventFired"

-- | Type of the 'Page.navigatedWithinDocument' event.
data PageNavigatedWithinDocument = PageNavigatedWithinDocument
  {
    -- | Id of the frame.
    pageNavigatedWithinDocumentFrameId :: PageFrameId,
    -- | Frame's new url.
    pageNavigatedWithinDocumentUrl :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageNavigatedWithinDocument where
  parseJSON = A.withObject "PageNavigatedWithinDocument" $ \o -> PageNavigatedWithinDocument
    <$> o A..: "frameId"
    <*> o A..: "url"
instance Event PageNavigatedWithinDocument where
  eventName _ = "Page.navigatedWithinDocument"

-- | Type of the 'Page.screencastFrame' event.
data PageScreencastFrame = PageScreencastFrame
  {
    -- | Base64-encoded compressed image. (Encoded as a base64 string when passed over JSON)
    pageScreencastFrameData :: T.Text,
    -- | Screencast frame metadata.
    pageScreencastFrameMetadata :: PageScreencastFrameMetadata,
    -- | Frame number.
    pageScreencastFrameSessionId :: Int
  }
  deriving (Eq, Show)
instance FromJSON PageScreencastFrame where
  parseJSON = A.withObject "PageScreencastFrame" $ \o -> PageScreencastFrame
    <$> o A..: "data"
    <*> o A..: "metadata"
    <*> o A..: "sessionId"
instance Event PageScreencastFrame where
  eventName _ = "Page.screencastFrame"

-- | Type of the 'Page.screencastVisibilityChanged' event.
data PageScreencastVisibilityChanged = PageScreencastVisibilityChanged
  {
    -- | True if the page is visible.
    pageScreencastVisibilityChangedVisible :: Bool
  }
  deriving (Eq, Show)
instance FromJSON PageScreencastVisibilityChanged where
  parseJSON = A.withObject "PageScreencastVisibilityChanged" $ \o -> PageScreencastVisibilityChanged
    <$> o A..: "visible"
instance Event PageScreencastVisibilityChanged where
  eventName _ = "Page.screencastVisibilityChanged"

-- | Type of the 'Page.windowOpen' event.
data PageWindowOpen = PageWindowOpen
  {
    -- | The URL for the new window.
    pageWindowOpenUrl :: T.Text,
    -- | Window name.
    pageWindowOpenWindowName :: T.Text,
    -- | An array of enabled window features.
    pageWindowOpenWindowFeatures :: [T.Text],
    -- | Whether or not it was triggered by user gesture.
    pageWindowOpenUserGesture :: Bool
  }
  deriving (Eq, Show)
instance FromJSON PageWindowOpen where
  parseJSON = A.withObject "PageWindowOpen" $ \o -> PageWindowOpen
    <$> o A..: "url"
    <*> o A..: "windowName"
    <*> o A..: "windowFeatures"
    <*> o A..: "userGesture"
instance Event PageWindowOpen where
  eventName _ = "Page.windowOpen"

-- | Type of the 'Page.compilationCacheProduced' event.
data PageCompilationCacheProduced = PageCompilationCacheProduced
  {
    pageCompilationCacheProducedUrl :: T.Text,
    -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
    pageCompilationCacheProducedData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageCompilationCacheProduced where
  parseJSON = A.withObject "PageCompilationCacheProduced" $ \o -> PageCompilationCacheProduced
    <$> o A..: "url"
    <*> o A..: "data"
instance Event PageCompilationCacheProduced where
  eventName _ = "Page.compilationCacheProduced"

-- | Evaluates given script in every frame upon creation (before loading frame's scripts).

-- | Parameters of the 'Page.addScriptToEvaluateOnNewDocument' command.
data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument
  {
    pPageAddScriptToEvaluateOnNewDocumentSource :: T.Text,
    -- | If specified, creates an isolated world with the given name and evaluates given script in it.
    --   This world name will be used as the ExecutionContextDescription::name when the corresponding
    --   event is emitted.
    pPageAddScriptToEvaluateOnNewDocumentWorldName :: Maybe T.Text,
    -- | Specifies whether command line API should be available to the script, defaults
    --   to false.
    pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineAPI :: Maybe Bool
  }
  deriving (Eq, Show)
pPageAddScriptToEvaluateOnNewDocument
  :: T.Text
  -> PPageAddScriptToEvaluateOnNewDocument
pPageAddScriptToEvaluateOnNewDocument
  arg_pPageAddScriptToEvaluateOnNewDocumentSource
  = PPageAddScriptToEvaluateOnNewDocument
    arg_pPageAddScriptToEvaluateOnNewDocumentSource
    Nothing
    Nothing
instance ToJSON PPageAddScriptToEvaluateOnNewDocument where
  toJSON p = A.object $ catMaybes [
    ("source" A..=) <$> Just (pPageAddScriptToEvaluateOnNewDocumentSource p),
    ("worldName" A..=) <$> (pPageAddScriptToEvaluateOnNewDocumentWorldName p),
    ("includeCommandLineAPI" A..=) <$> (pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineAPI p)
    ]
data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument
  {
    -- | Identifier of the added script.
    pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
  }
  deriving (Eq, Show)
instance FromJSON PageAddScriptToEvaluateOnNewDocument where
  parseJSON = A.withObject "PageAddScriptToEvaluateOnNewDocument" $ \o -> PageAddScriptToEvaluateOnNewDocument
    <$> o A..: "identifier"
instance Command PPageAddScriptToEvaluateOnNewDocument where
  type CommandResponse PPageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument
  commandName _ = "Page.addScriptToEvaluateOnNewDocument"

-- | Brings page to front (activates tab).

-- | Parameters of the 'Page.bringToFront' command.
data PPageBringToFront = PPageBringToFront
  deriving (Eq, Show)
pPageBringToFront
  :: PPageBringToFront
pPageBringToFront
  = PPageBringToFront
instance ToJSON PPageBringToFront where
  toJSON _ = A.Null
instance Command PPageBringToFront where
  type CommandResponse PPageBringToFront = ()
  commandName _ = "Page.bringToFront"
  fromJSON = const . A.Success . const ()

-- | Capture page screenshot.

-- | Parameters of the 'Page.captureScreenshot' command.
data PPageCaptureScreenshotFormat = PPageCaptureScreenshotFormatJpeg | PPageCaptureScreenshotFormatPng | PPageCaptureScreenshotFormatWebp
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPageCaptureScreenshotFormat where
  parseJSON = A.withText "PPageCaptureScreenshotFormat" $ \v -> case v of
    "jpeg" -> pure PPageCaptureScreenshotFormatJpeg
    "png" -> pure PPageCaptureScreenshotFormatPng
    "webp" -> pure PPageCaptureScreenshotFormatWebp
    "_" -> fail "failed to parse PPageCaptureScreenshotFormat"
instance ToJSON PPageCaptureScreenshotFormat where
  toJSON v = A.String $ case v of
    PPageCaptureScreenshotFormatJpeg -> "jpeg"
    PPageCaptureScreenshotFormatPng -> "png"
    PPageCaptureScreenshotFormatWebp -> "webp"
data PPageCaptureScreenshot = PPageCaptureScreenshot
  {
    -- | Image compression format (defaults to png).
    pPageCaptureScreenshotFormat :: Maybe PPageCaptureScreenshotFormat,
    -- | Compression quality from range [0..100] (jpeg only).
    pPageCaptureScreenshotQuality :: Maybe Int,
    -- | Capture the screenshot of a given region only.
    pPageCaptureScreenshotClip :: Maybe PageViewport,
    -- | Capture the screenshot from the surface, rather than the view. Defaults to true.
    pPageCaptureScreenshotFromSurface :: Maybe Bool,
    -- | Capture the screenshot beyond the viewport. Defaults to false.
    pPageCaptureScreenshotCaptureBeyondViewport :: Maybe Bool
  }
  deriving (Eq, Show)
pPageCaptureScreenshot
  :: PPageCaptureScreenshot
pPageCaptureScreenshot
  = PPageCaptureScreenshot
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PPageCaptureScreenshot where
  toJSON p = A.object $ catMaybes [
    ("format" A..=) <$> (pPageCaptureScreenshotFormat p),
    ("quality" A..=) <$> (pPageCaptureScreenshotQuality p),
    ("clip" A..=) <$> (pPageCaptureScreenshotClip p),
    ("fromSurface" A..=) <$> (pPageCaptureScreenshotFromSurface p),
    ("captureBeyondViewport" A..=) <$> (pPageCaptureScreenshotCaptureBeyondViewport p)
    ]
data PageCaptureScreenshot = PageCaptureScreenshot
  {
    -- | Base64-encoded image data. (Encoded as a base64 string when passed over JSON)
    pageCaptureScreenshotData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageCaptureScreenshot where
  parseJSON = A.withObject "PageCaptureScreenshot" $ \o -> PageCaptureScreenshot
    <$> o A..: "data"
instance Command PPageCaptureScreenshot where
  type CommandResponse PPageCaptureScreenshot = PageCaptureScreenshot
  commandName _ = "Page.captureScreenshot"

-- | Returns a snapshot of the page as a string. For MHTML format, the serialization includes
--   iframes, shadow DOM, external resources, and element-inline styles.

-- | Parameters of the 'Page.captureSnapshot' command.
data PPageCaptureSnapshotFormat = PPageCaptureSnapshotFormatMhtml
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPageCaptureSnapshotFormat where
  parseJSON = A.withText "PPageCaptureSnapshotFormat" $ \v -> case v of
    "mhtml" -> pure PPageCaptureSnapshotFormatMhtml
    "_" -> fail "failed to parse PPageCaptureSnapshotFormat"
instance ToJSON PPageCaptureSnapshotFormat where
  toJSON v = A.String $ case v of
    PPageCaptureSnapshotFormatMhtml -> "mhtml"
data PPageCaptureSnapshot = PPageCaptureSnapshot
  {
    -- | Format (defaults to mhtml).
    pPageCaptureSnapshotFormat :: Maybe PPageCaptureSnapshotFormat
  }
  deriving (Eq, Show)
pPageCaptureSnapshot
  :: PPageCaptureSnapshot
pPageCaptureSnapshot
  = PPageCaptureSnapshot
    Nothing
instance ToJSON PPageCaptureSnapshot where
  toJSON p = A.object $ catMaybes [
    ("format" A..=) <$> (pPageCaptureSnapshotFormat p)
    ]
data PageCaptureSnapshot = PageCaptureSnapshot
  {
    -- | Serialized page data.
    pageCaptureSnapshotData :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageCaptureSnapshot where
  parseJSON = A.withObject "PageCaptureSnapshot" $ \o -> PageCaptureSnapshot
    <$> o A..: "data"
instance Command PPageCaptureSnapshot where
  type CommandResponse PPageCaptureSnapshot = PageCaptureSnapshot
  commandName _ = "Page.captureSnapshot"

-- | Creates an isolated world for the given frame.

-- | Parameters of the 'Page.createIsolatedWorld' command.
data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld
  {
    -- | Id of the frame in which the isolated world should be created.
    pPageCreateIsolatedWorldFrameId :: PageFrameId,
    -- | An optional name which is reported in the Execution Context.
    pPageCreateIsolatedWorldWorldName :: Maybe T.Text,
    -- | Whether or not universal access should be granted to the isolated world. This is a powerful
    --   option, use with caution.
    pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
  }
  deriving (Eq, Show)
pPageCreateIsolatedWorld
  -- | Id of the frame in which the isolated world should be created.
  :: PageFrameId
  -> PPageCreateIsolatedWorld
pPageCreateIsolatedWorld
  arg_pPageCreateIsolatedWorldFrameId
  = PPageCreateIsolatedWorld
    arg_pPageCreateIsolatedWorldFrameId
    Nothing
    Nothing
instance ToJSON PPageCreateIsolatedWorld where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageCreateIsolatedWorldFrameId p),
    ("worldName" A..=) <$> (pPageCreateIsolatedWorldWorldName p),
    ("grantUniveralAccess" A..=) <$> (pPageCreateIsolatedWorldGrantUniveralAccess p)
    ]
data PageCreateIsolatedWorld = PageCreateIsolatedWorld
  {
    -- | Execution context of the isolated world.
    pageCreateIsolatedWorldExecutionContextId :: Runtime.RuntimeExecutionContextId
  }
  deriving (Eq, Show)
instance FromJSON PageCreateIsolatedWorld where
  parseJSON = A.withObject "PageCreateIsolatedWorld" $ \o -> PageCreateIsolatedWorld
    <$> o A..: "executionContextId"
instance Command PPageCreateIsolatedWorld where
  type CommandResponse PPageCreateIsolatedWorld = PageCreateIsolatedWorld
  commandName _ = "Page.createIsolatedWorld"

-- | Disables page domain notifications.

-- | Parameters of the 'Page.disable' command.
data PPageDisable = PPageDisable
  deriving (Eq, Show)
pPageDisable
  :: PPageDisable
pPageDisable
  = PPageDisable
instance ToJSON PPageDisable where
  toJSON _ = A.Null
instance Command PPageDisable where
  type CommandResponse PPageDisable = ()
  commandName _ = "Page.disable"
  fromJSON = const . A.Success . const ()

-- | Enables page domain notifications.

-- | Parameters of the 'Page.enable' command.
data PPageEnable = PPageEnable
  deriving (Eq, Show)
pPageEnable
  :: PPageEnable
pPageEnable
  = PPageEnable
instance ToJSON PPageEnable where
  toJSON _ = A.Null
instance Command PPageEnable where
  type CommandResponse PPageEnable = ()
  commandName _ = "Page.enable"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Page.getAppManifest' command.
data PPageGetAppManifest = PPageGetAppManifest
  deriving (Eq, Show)
pPageGetAppManifest
  :: PPageGetAppManifest
pPageGetAppManifest
  = PPageGetAppManifest
instance ToJSON PPageGetAppManifest where
  toJSON _ = A.Null
data PageGetAppManifest = PageGetAppManifest
  {
    -- | Manifest location.
    pageGetAppManifestUrl :: T.Text,
    pageGetAppManifestErrors :: [PageAppManifestError],
    -- | Manifest content.
    pageGetAppManifestData :: Maybe T.Text,
    -- | Parsed manifest properties
    pageGetAppManifestParsed :: Maybe PageAppManifestParsedProperties
  }
  deriving (Eq, Show)
instance FromJSON PageGetAppManifest where
  parseJSON = A.withObject "PageGetAppManifest" $ \o -> PageGetAppManifest
    <$> o A..: "url"
    <*> o A..: "errors"
    <*> o A..:? "data"
    <*> o A..:? "parsed"
instance Command PPageGetAppManifest where
  type CommandResponse PPageGetAppManifest = PageGetAppManifest
  commandName _ = "Page.getAppManifest"


-- | Parameters of the 'Page.getInstallabilityErrors' command.
data PPageGetInstallabilityErrors = PPageGetInstallabilityErrors
  deriving (Eq, Show)
pPageGetInstallabilityErrors
  :: PPageGetInstallabilityErrors
pPageGetInstallabilityErrors
  = PPageGetInstallabilityErrors
instance ToJSON PPageGetInstallabilityErrors where
  toJSON _ = A.Null
data PageGetInstallabilityErrors = PageGetInstallabilityErrors
  {
    pageGetInstallabilityErrorsInstallabilityErrors :: [PageInstallabilityError]
  }
  deriving (Eq, Show)
instance FromJSON PageGetInstallabilityErrors where
  parseJSON = A.withObject "PageGetInstallabilityErrors" $ \o -> PageGetInstallabilityErrors
    <$> o A..: "installabilityErrors"
instance Command PPageGetInstallabilityErrors where
  type CommandResponse PPageGetInstallabilityErrors = PageGetInstallabilityErrors
  commandName _ = "Page.getInstallabilityErrors"


-- | Parameters of the 'Page.getManifestIcons' command.
data PPageGetManifestIcons = PPageGetManifestIcons
  deriving (Eq, Show)
pPageGetManifestIcons
  :: PPageGetManifestIcons
pPageGetManifestIcons
  = PPageGetManifestIcons
instance ToJSON PPageGetManifestIcons where
  toJSON _ = A.Null
data PageGetManifestIcons = PageGetManifestIcons
  {
    pageGetManifestIconsPrimaryIcon :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageGetManifestIcons where
  parseJSON = A.withObject "PageGetManifestIcons" $ \o -> PageGetManifestIcons
    <$> o A..:? "primaryIcon"
instance Command PPageGetManifestIcons where
  type CommandResponse PPageGetManifestIcons = PageGetManifestIcons
  commandName _ = "Page.getManifestIcons"

-- | Returns the unique (PWA) app id.
--   Only returns values if the feature flag 'WebAppEnableManifestId' is enabled

-- | Parameters of the 'Page.getAppId' command.
data PPageGetAppId = PPageGetAppId
  deriving (Eq, Show)
pPageGetAppId
  :: PPageGetAppId
pPageGetAppId
  = PPageGetAppId
instance ToJSON PPageGetAppId where
  toJSON _ = A.Null
data PageGetAppId = PageGetAppId
  {
    -- | App id, either from manifest's id attribute or computed from start_url
    pageGetAppIdAppId :: Maybe T.Text,
    -- | Recommendation for manifest's id attribute to match current id computed from start_url
    pageGetAppIdRecommendedId :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageGetAppId where
  parseJSON = A.withObject "PageGetAppId" $ \o -> PageGetAppId
    <$> o A..:? "appId"
    <*> o A..:? "recommendedId"
instance Command PPageGetAppId where
  type CommandResponse PPageGetAppId = PageGetAppId
  commandName _ = "Page.getAppId"


-- | Parameters of the 'Page.getAdScriptId' command.
data PPageGetAdScriptId = PPageGetAdScriptId
  {
    pPageGetAdScriptIdFrameId :: PageFrameId
  }
  deriving (Eq, Show)
pPageGetAdScriptId
  :: PageFrameId
  -> PPageGetAdScriptId
pPageGetAdScriptId
  arg_pPageGetAdScriptIdFrameId
  = PPageGetAdScriptId
    arg_pPageGetAdScriptIdFrameId
instance ToJSON PPageGetAdScriptId where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageGetAdScriptIdFrameId p)
    ]
data PageGetAdScriptId = PageGetAdScriptId
  {
    -- | Identifies the bottom-most script which caused the frame to be labelled
    --   as an ad. Only sent if frame is labelled as an ad and id is available.
    pageGetAdScriptIdAdScriptId :: Maybe PageAdScriptId
  }
  deriving (Eq, Show)
instance FromJSON PageGetAdScriptId where
  parseJSON = A.withObject "PageGetAdScriptId" $ \o -> PageGetAdScriptId
    <$> o A..:? "adScriptId"
instance Command PPageGetAdScriptId where
  type CommandResponse PPageGetAdScriptId = PageGetAdScriptId
  commandName _ = "Page.getAdScriptId"

-- | Returns present frame tree structure.

-- | Parameters of the 'Page.getFrameTree' command.
data PPageGetFrameTree = PPageGetFrameTree
  deriving (Eq, Show)
pPageGetFrameTree
  :: PPageGetFrameTree
pPageGetFrameTree
  = PPageGetFrameTree
instance ToJSON PPageGetFrameTree where
  toJSON _ = A.Null
data PageGetFrameTree = PageGetFrameTree
  {
    -- | Present frame tree structure.
    pageGetFrameTreeFrameTree :: PageFrameTree
  }
  deriving (Eq, Show)
instance FromJSON PageGetFrameTree where
  parseJSON = A.withObject "PageGetFrameTree" $ \o -> PageGetFrameTree
    <$> o A..: "frameTree"
instance Command PPageGetFrameTree where
  type CommandResponse PPageGetFrameTree = PageGetFrameTree
  commandName _ = "Page.getFrameTree"

-- | Returns metrics relating to the layouting of the page, such as viewport bounds/scale.

-- | Parameters of the 'Page.getLayoutMetrics' command.
data PPageGetLayoutMetrics = PPageGetLayoutMetrics
  deriving (Eq, Show)
pPageGetLayoutMetrics
  :: PPageGetLayoutMetrics
pPageGetLayoutMetrics
  = PPageGetLayoutMetrics
instance ToJSON PPageGetLayoutMetrics where
  toJSON _ = A.Null
data PageGetLayoutMetrics = PageGetLayoutMetrics
  {
    -- | Metrics relating to the layout viewport in CSS pixels.
    pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
    -- | Metrics relating to the visual viewport in CSS pixels.
    pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
    -- | Size of scrollable area in CSS pixels.
    pageGetLayoutMetricsCssContentSize :: DOMRect
  }
  deriving (Eq, Show)
instance FromJSON PageGetLayoutMetrics where
  parseJSON = A.withObject "PageGetLayoutMetrics" $ \o -> PageGetLayoutMetrics
    <$> o A..: "cssLayoutViewport"
    <*> o A..: "cssVisualViewport"
    <*> o A..: "cssContentSize"
instance Command PPageGetLayoutMetrics where
  type CommandResponse PPageGetLayoutMetrics = PageGetLayoutMetrics
  commandName _ = "Page.getLayoutMetrics"

-- | Returns navigation history for the current page.

-- | Parameters of the 'Page.getNavigationHistory' command.
data PPageGetNavigationHistory = PPageGetNavigationHistory
  deriving (Eq, Show)
pPageGetNavigationHistory
  :: PPageGetNavigationHistory
pPageGetNavigationHistory
  = PPageGetNavigationHistory
instance ToJSON PPageGetNavigationHistory where
  toJSON _ = A.Null
data PageGetNavigationHistory = PageGetNavigationHistory
  {
    -- | Index of the current navigation history entry.
    pageGetNavigationHistoryCurrentIndex :: Int,
    -- | Array of navigation history entries.
    pageGetNavigationHistoryEntries :: [PageNavigationEntry]
  }
  deriving (Eq, Show)
instance FromJSON PageGetNavigationHistory where
  parseJSON = A.withObject "PageGetNavigationHistory" $ \o -> PageGetNavigationHistory
    <$> o A..: "currentIndex"
    <*> o A..: "entries"
instance Command PPageGetNavigationHistory where
  type CommandResponse PPageGetNavigationHistory = PageGetNavigationHistory
  commandName _ = "Page.getNavigationHistory"

-- | Resets navigation history for the current page.

-- | Parameters of the 'Page.resetNavigationHistory' command.
data PPageResetNavigationHistory = PPageResetNavigationHistory
  deriving (Eq, Show)
pPageResetNavigationHistory
  :: PPageResetNavigationHistory
pPageResetNavigationHistory
  = PPageResetNavigationHistory
instance ToJSON PPageResetNavigationHistory where
  toJSON _ = A.Null
instance Command PPageResetNavigationHistory where
  type CommandResponse PPageResetNavigationHistory = ()
  commandName _ = "Page.resetNavigationHistory"
  fromJSON = const . A.Success . const ()

-- | Returns content of the given resource.

-- | Parameters of the 'Page.getResourceContent' command.
data PPageGetResourceContent = PPageGetResourceContent
  {
    -- | Frame id to get resource for.
    pPageGetResourceContentFrameId :: PageFrameId,
    -- | URL of the resource to get content for.
    pPageGetResourceContentUrl :: T.Text
  }
  deriving (Eq, Show)
pPageGetResourceContent
  -- | Frame id to get resource for.
  :: PageFrameId
  -- | URL of the resource to get content for.
  -> T.Text
  -> PPageGetResourceContent
pPageGetResourceContent
  arg_pPageGetResourceContentFrameId
  arg_pPageGetResourceContentUrl
  = PPageGetResourceContent
    arg_pPageGetResourceContentFrameId
    arg_pPageGetResourceContentUrl
instance ToJSON PPageGetResourceContent where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageGetResourceContentFrameId p),
    ("url" A..=) <$> Just (pPageGetResourceContentUrl p)
    ]
data PageGetResourceContent = PageGetResourceContent
  {
    -- | Resource content.
    pageGetResourceContentContent :: T.Text,
    -- | True, if content was served as base64.
    pageGetResourceContentBase64Encoded :: Bool
  }
  deriving (Eq, Show)
instance FromJSON PageGetResourceContent where
  parseJSON = A.withObject "PageGetResourceContent" $ \o -> PageGetResourceContent
    <$> o A..: "content"
    <*> o A..: "base64Encoded"
instance Command PPageGetResourceContent where
  type CommandResponse PPageGetResourceContent = PageGetResourceContent
  commandName _ = "Page.getResourceContent"

-- | Returns present frame / resource tree structure.

-- | Parameters of the 'Page.getResourceTree' command.
data PPageGetResourceTree = PPageGetResourceTree
  deriving (Eq, Show)
pPageGetResourceTree
  :: PPageGetResourceTree
pPageGetResourceTree
  = PPageGetResourceTree
instance ToJSON PPageGetResourceTree where
  toJSON _ = A.Null
data PageGetResourceTree = PageGetResourceTree
  {
    -- | Present frame / resource tree structure.
    pageGetResourceTreeFrameTree :: PageFrameResourceTree
  }
  deriving (Eq, Show)
instance FromJSON PageGetResourceTree where
  parseJSON = A.withObject "PageGetResourceTree" $ \o -> PageGetResourceTree
    <$> o A..: "frameTree"
instance Command PPageGetResourceTree where
  type CommandResponse PPageGetResourceTree = PageGetResourceTree
  commandName _ = "Page.getResourceTree"

-- | Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).

-- | Parameters of the 'Page.handleJavaScriptDialog' command.
data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog
  {
    -- | Whether to accept or dismiss the dialog.
    pPageHandleJavaScriptDialogAccept :: Bool,
    -- | The text to enter into the dialog prompt before accepting. Used only if this is a prompt
    --   dialog.
    pPageHandleJavaScriptDialogPromptText :: Maybe T.Text
  }
  deriving (Eq, Show)
pPageHandleJavaScriptDialog
  -- | Whether to accept or dismiss the dialog.
  :: Bool
  -> PPageHandleJavaScriptDialog
pPageHandleJavaScriptDialog
  arg_pPageHandleJavaScriptDialogAccept
  = PPageHandleJavaScriptDialog
    arg_pPageHandleJavaScriptDialogAccept
    Nothing
instance ToJSON PPageHandleJavaScriptDialog where
  toJSON p = A.object $ catMaybes [
    ("accept" A..=) <$> Just (pPageHandleJavaScriptDialogAccept p),
    ("promptText" A..=) <$> (pPageHandleJavaScriptDialogPromptText p)
    ]
instance Command PPageHandleJavaScriptDialog where
  type CommandResponse PPageHandleJavaScriptDialog = ()
  commandName _ = "Page.handleJavaScriptDialog"
  fromJSON = const . A.Success . const ()

-- | Navigates current page to the given URL.

-- | Parameters of the 'Page.navigate' command.
data PPageNavigate = PPageNavigate
  {
    -- | URL to navigate the page to.
    pPageNavigateUrl :: T.Text,
    -- | Referrer URL.
    pPageNavigateReferrer :: Maybe T.Text,
    -- | Intended transition type.
    pPageNavigateTransitionType :: Maybe PageTransitionType,
    -- | Frame id to navigate, if not specified navigates the top frame.
    pPageNavigateFrameId :: Maybe PageFrameId,
    -- | Referrer-policy used for the navigation.
    pPageNavigateReferrerPolicy :: Maybe PageReferrerPolicy
  }
  deriving (Eq, Show)
pPageNavigate
  -- | URL to navigate the page to.
  :: T.Text
  -> PPageNavigate
pPageNavigate
  arg_pPageNavigateUrl
  = PPageNavigate
    arg_pPageNavigateUrl
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PPageNavigate where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pPageNavigateUrl p),
    ("referrer" A..=) <$> (pPageNavigateReferrer p),
    ("transitionType" A..=) <$> (pPageNavigateTransitionType p),
    ("frameId" A..=) <$> (pPageNavigateFrameId p),
    ("referrerPolicy" A..=) <$> (pPageNavigateReferrerPolicy p)
    ]
data PageNavigate = PageNavigate
  {
    -- | Frame id that has navigated (or failed to navigate)
    pageNavigateFrameId :: PageFrameId,
    -- | Loader identifier. This is omitted in case of same-document navigation,
    --   as the previously committed loaderId would not change.
    pageNavigateLoaderId :: Maybe NetworkLoaderId,
    -- | User friendly error message, present if and only if navigation has failed.
    pageNavigateErrorText :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON PageNavigate where
  parseJSON = A.withObject "PageNavigate" $ \o -> PageNavigate
    <$> o A..: "frameId"
    <*> o A..:? "loaderId"
    <*> o A..:? "errorText"
instance Command PPageNavigate where
  type CommandResponse PPageNavigate = PageNavigate
  commandName _ = "Page.navigate"

-- | Navigates current page to the given history entry.

-- | Parameters of the 'Page.navigateToHistoryEntry' command.
data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry
  {
    -- | Unique id of the entry to navigate to.
    pPageNavigateToHistoryEntryEntryId :: Int
  }
  deriving (Eq, Show)
pPageNavigateToHistoryEntry
  -- | Unique id of the entry to navigate to.
  :: Int
  -> PPageNavigateToHistoryEntry
pPageNavigateToHistoryEntry
  arg_pPageNavigateToHistoryEntryEntryId
  = PPageNavigateToHistoryEntry
    arg_pPageNavigateToHistoryEntryEntryId
instance ToJSON PPageNavigateToHistoryEntry where
  toJSON p = A.object $ catMaybes [
    ("entryId" A..=) <$> Just (pPageNavigateToHistoryEntryEntryId p)
    ]
instance Command PPageNavigateToHistoryEntry where
  type CommandResponse PPageNavigateToHistoryEntry = ()
  commandName _ = "Page.navigateToHistoryEntry"
  fromJSON = const . A.Success . const ()

-- | Print page as PDF.

-- | Parameters of the 'Page.printToPDF' command.
data PPagePrintToPDFTransferMode = PPagePrintToPDFTransferModeReturnAsBase64 | PPagePrintToPDFTransferModeReturnAsStream
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPagePrintToPDFTransferMode where
  parseJSON = A.withText "PPagePrintToPDFTransferMode" $ \v -> case v of
    "ReturnAsBase64" -> pure PPagePrintToPDFTransferModeReturnAsBase64
    "ReturnAsStream" -> pure PPagePrintToPDFTransferModeReturnAsStream
    "_" -> fail "failed to parse PPagePrintToPDFTransferMode"
instance ToJSON PPagePrintToPDFTransferMode where
  toJSON v = A.String $ case v of
    PPagePrintToPDFTransferModeReturnAsBase64 -> "ReturnAsBase64"
    PPagePrintToPDFTransferModeReturnAsStream -> "ReturnAsStream"
data PPagePrintToPDF = PPagePrintToPDF
  {
    -- | Paper orientation. Defaults to false.
    pPagePrintToPDFLandscape :: Maybe Bool,
    -- | Display header and footer. Defaults to false.
    pPagePrintToPDFDisplayHeaderFooter :: Maybe Bool,
    -- | Print background graphics. Defaults to false.
    pPagePrintToPDFPrintBackground :: Maybe Bool,
    -- | Scale of the webpage rendering. Defaults to 1.
    pPagePrintToPDFScale :: Maybe Double,
    -- | Paper width in inches. Defaults to 8.5 inches.
    pPagePrintToPDFPaperWidth :: Maybe Double,
    -- | Paper height in inches. Defaults to 11 inches.
    pPagePrintToPDFPaperHeight :: Maybe Double,
    -- | Top margin in inches. Defaults to 1cm (~0.4 inches).
    pPagePrintToPDFMarginTop :: Maybe Double,
    -- | Bottom margin in inches. Defaults to 1cm (~0.4 inches).
    pPagePrintToPDFMarginBottom :: Maybe Double,
    -- | Left margin in inches. Defaults to 1cm (~0.4 inches).
    pPagePrintToPDFMarginLeft :: Maybe Double,
    -- | Right margin in inches. Defaults to 1cm (~0.4 inches).
    pPagePrintToPDFMarginRight :: Maybe Double,
    -- | Paper ranges to print, one based, e.g., '1-5, 8, 11-13'. Pages are
    --   printed in the document order, not in the order specified, and no
    --   more than once.
    --   Defaults to empty string, which implies the entire document is printed.
    --   The page numbers are quietly capped to actual page count of the
    --   document, and ranges beyond the end of the document are ignored.
    --   If this results in no pages to print, an error is reported.
    --   It is an error to specify a range with start greater than end.
    pPagePrintToPDFPageRanges :: Maybe T.Text,
    -- | HTML template for the print header. Should be valid HTML markup with following
    --   classes used to inject printing values into them:
    --   - `date`: formatted print date
    --   - `title`: document title
    --   - `url`: document location
    --   - `pageNumber`: current page number
    --   - `totalPages`: total pages in the document
    --   
    --   For example, `<span class=title></span>` would generate span containing the title.
    pPagePrintToPDFHeaderTemplate :: Maybe T.Text,
    -- | HTML template for the print footer. Should use the same format as the `headerTemplate`.
    pPagePrintToPDFFooterTemplate :: Maybe T.Text,
    -- | Whether or not to prefer page size as defined by css. Defaults to false,
    --   in which case the content will be scaled to fit the paper size.
    pPagePrintToPDFPreferCSSPageSize :: Maybe Bool,
    -- | return as stream
    pPagePrintToPDFTransferMode :: Maybe PPagePrintToPDFTransferMode
  }
  deriving (Eq, Show)
pPagePrintToPDF
  :: PPagePrintToPDF
pPagePrintToPDF
  = PPagePrintToPDF
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PPagePrintToPDF where
  toJSON p = A.object $ catMaybes [
    ("landscape" A..=) <$> (pPagePrintToPDFLandscape p),
    ("displayHeaderFooter" A..=) <$> (pPagePrintToPDFDisplayHeaderFooter p),
    ("printBackground" A..=) <$> (pPagePrintToPDFPrintBackground p),
    ("scale" A..=) <$> (pPagePrintToPDFScale p),
    ("paperWidth" A..=) <$> (pPagePrintToPDFPaperWidth p),
    ("paperHeight" A..=) <$> (pPagePrintToPDFPaperHeight p),
    ("marginTop" A..=) <$> (pPagePrintToPDFMarginTop p),
    ("marginBottom" A..=) <$> (pPagePrintToPDFMarginBottom p),
    ("marginLeft" A..=) <$> (pPagePrintToPDFMarginLeft p),
    ("marginRight" A..=) <$> (pPagePrintToPDFMarginRight p),
    ("pageRanges" A..=) <$> (pPagePrintToPDFPageRanges p),
    ("headerTemplate" A..=) <$> (pPagePrintToPDFHeaderTemplate p),
    ("footerTemplate" A..=) <$> (pPagePrintToPDFFooterTemplate p),
    ("preferCSSPageSize" A..=) <$> (pPagePrintToPDFPreferCSSPageSize p),
    ("transferMode" A..=) <$> (pPagePrintToPDFTransferMode p)
    ]
data PagePrintToPDF = PagePrintToPDF
  {
    -- | Base64-encoded pdf data. Empty if |returnAsStream| is specified. (Encoded as a base64 string when passed over JSON)
    pagePrintToPDFData :: T.Text,
    -- | A handle of the stream that holds resulting PDF data.
    pagePrintToPDFStream :: Maybe IO.IOStreamHandle
  }
  deriving (Eq, Show)
instance FromJSON PagePrintToPDF where
  parseJSON = A.withObject "PagePrintToPDF" $ \o -> PagePrintToPDF
    <$> o A..: "data"
    <*> o A..:? "stream"
instance Command PPagePrintToPDF where
  type CommandResponse PPagePrintToPDF = PagePrintToPDF
  commandName _ = "Page.printToPDF"

-- | Reloads given page optionally ignoring the cache.

-- | Parameters of the 'Page.reload' command.
data PPageReload = PPageReload
  {
    -- | If true, browser cache is ignored (as if the user pressed Shift+refresh).
    pPageReloadIgnoreCache :: Maybe Bool,
    -- | If set, the script will be injected into all frames of the inspected page after reload.
    --   Argument will be ignored if reloading dataURL origin.
    pPageReloadScriptToEvaluateOnLoad :: Maybe T.Text
  }
  deriving (Eq, Show)
pPageReload
  :: PPageReload
pPageReload
  = PPageReload
    Nothing
    Nothing
instance ToJSON PPageReload where
  toJSON p = A.object $ catMaybes [
    ("ignoreCache" A..=) <$> (pPageReloadIgnoreCache p),
    ("scriptToEvaluateOnLoad" A..=) <$> (pPageReloadScriptToEvaluateOnLoad p)
    ]
instance Command PPageReload where
  type CommandResponse PPageReload = ()
  commandName _ = "Page.reload"
  fromJSON = const . A.Success . const ()

-- | Removes given script from the list.

-- | Parameters of the 'Page.removeScriptToEvaluateOnNewDocument' command.
data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument
  {
    pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
  }
  deriving (Eq, Show)
pPageRemoveScriptToEvaluateOnNewDocument
  :: PageScriptIdentifier
  -> PPageRemoveScriptToEvaluateOnNewDocument
pPageRemoveScriptToEvaluateOnNewDocument
  arg_pPageRemoveScriptToEvaluateOnNewDocumentIdentifier
  = PPageRemoveScriptToEvaluateOnNewDocument
    arg_pPageRemoveScriptToEvaluateOnNewDocumentIdentifier
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument where
  toJSON p = A.object $ catMaybes [
    ("identifier" A..=) <$> Just (pPageRemoveScriptToEvaluateOnNewDocumentIdentifier p)
    ]
instance Command PPageRemoveScriptToEvaluateOnNewDocument where
  type CommandResponse PPageRemoveScriptToEvaluateOnNewDocument = ()
  commandName _ = "Page.removeScriptToEvaluateOnNewDocument"
  fromJSON = const . A.Success . const ()

-- | Acknowledges that a screencast frame has been received by the frontend.

-- | Parameters of the 'Page.screencastFrameAck' command.
data PPageScreencastFrameAck = PPageScreencastFrameAck
  {
    -- | Frame number.
    pPageScreencastFrameAckSessionId :: Int
  }
  deriving (Eq, Show)
pPageScreencastFrameAck
  -- | Frame number.
  :: Int
  -> PPageScreencastFrameAck
pPageScreencastFrameAck
  arg_pPageScreencastFrameAckSessionId
  = PPageScreencastFrameAck
    arg_pPageScreencastFrameAckSessionId
instance ToJSON PPageScreencastFrameAck where
  toJSON p = A.object $ catMaybes [
    ("sessionId" A..=) <$> Just (pPageScreencastFrameAckSessionId p)
    ]
instance Command PPageScreencastFrameAck where
  type CommandResponse PPageScreencastFrameAck = ()
  commandName _ = "Page.screencastFrameAck"
  fromJSON = const . A.Success . const ()

-- | Searches for given string in resource content.

-- | Parameters of the 'Page.searchInResource' command.
data PPageSearchInResource = PPageSearchInResource
  {
    -- | Frame id for resource to search in.
    pPageSearchInResourceFrameId :: PageFrameId,
    -- | URL of the resource to search in.
    pPageSearchInResourceUrl :: T.Text,
    -- | String to search for.
    pPageSearchInResourceQuery :: T.Text,
    -- | If true, search is case sensitive.
    pPageSearchInResourceCaseSensitive :: Maybe Bool,
    -- | If true, treats string parameter as regex.
    pPageSearchInResourceIsRegex :: Maybe Bool
  }
  deriving (Eq, Show)
pPageSearchInResource
  -- | Frame id for resource to search in.
  :: PageFrameId
  -- | URL of the resource to search in.
  -> T.Text
  -- | String to search for.
  -> T.Text
  -> PPageSearchInResource
pPageSearchInResource
  arg_pPageSearchInResourceFrameId
  arg_pPageSearchInResourceUrl
  arg_pPageSearchInResourceQuery
  = PPageSearchInResource
    arg_pPageSearchInResourceFrameId
    arg_pPageSearchInResourceUrl
    arg_pPageSearchInResourceQuery
    Nothing
    Nothing
instance ToJSON PPageSearchInResource where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageSearchInResourceFrameId p),
    ("url" A..=) <$> Just (pPageSearchInResourceUrl p),
    ("query" A..=) <$> Just (pPageSearchInResourceQuery p),
    ("caseSensitive" A..=) <$> (pPageSearchInResourceCaseSensitive p),
    ("isRegex" A..=) <$> (pPageSearchInResourceIsRegex p)
    ]
data PageSearchInResource = PageSearchInResource
  {
    -- | List of search matches.
    pageSearchInResourceResult :: [Debugger.DebuggerSearchMatch]
  }
  deriving (Eq, Show)
instance FromJSON PageSearchInResource where
  parseJSON = A.withObject "PageSearchInResource" $ \o -> PageSearchInResource
    <$> o A..: "result"
instance Command PPageSearchInResource where
  type CommandResponse PPageSearchInResource = PageSearchInResource
  commandName _ = "Page.searchInResource"

-- | Enable Chrome's experimental ad filter on all sites.

-- | Parameters of the 'Page.setAdBlockingEnabled' command.
data PPageSetAdBlockingEnabled = PPageSetAdBlockingEnabled
  {
    -- | Whether to block ads.
    pPageSetAdBlockingEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pPageSetAdBlockingEnabled
  -- | Whether to block ads.
  :: Bool
  -> PPageSetAdBlockingEnabled
pPageSetAdBlockingEnabled
  arg_pPageSetAdBlockingEnabledEnabled
  = PPageSetAdBlockingEnabled
    arg_pPageSetAdBlockingEnabledEnabled
instance ToJSON PPageSetAdBlockingEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pPageSetAdBlockingEnabledEnabled p)
    ]
instance Command PPageSetAdBlockingEnabled where
  type CommandResponse PPageSetAdBlockingEnabled = ()
  commandName _ = "Page.setAdBlockingEnabled"
  fromJSON = const . A.Success . const ()

-- | Enable page Content Security Policy by-passing.

-- | Parameters of the 'Page.setBypassCSP' command.
data PPageSetBypassCSP = PPageSetBypassCSP
  {
    -- | Whether to bypass page CSP.
    pPageSetBypassCSPEnabled :: Bool
  }
  deriving (Eq, Show)
pPageSetBypassCSP
  -- | Whether to bypass page CSP.
  :: Bool
  -> PPageSetBypassCSP
pPageSetBypassCSP
  arg_pPageSetBypassCSPEnabled
  = PPageSetBypassCSP
    arg_pPageSetBypassCSPEnabled
instance ToJSON PPageSetBypassCSP where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pPageSetBypassCSPEnabled p)
    ]
instance Command PPageSetBypassCSP where
  type CommandResponse PPageSetBypassCSP = ()
  commandName _ = "Page.setBypassCSP"
  fromJSON = const . A.Success . const ()

-- | Get Permissions Policy state on given frame.

-- | Parameters of the 'Page.getPermissionsPolicyState' command.
data PPageGetPermissionsPolicyState = PPageGetPermissionsPolicyState
  {
    pPageGetPermissionsPolicyStateFrameId :: PageFrameId
  }
  deriving (Eq, Show)
pPageGetPermissionsPolicyState
  :: PageFrameId
  -> PPageGetPermissionsPolicyState
pPageGetPermissionsPolicyState
  arg_pPageGetPermissionsPolicyStateFrameId
  = PPageGetPermissionsPolicyState
    arg_pPageGetPermissionsPolicyStateFrameId
instance ToJSON PPageGetPermissionsPolicyState where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageGetPermissionsPolicyStateFrameId p)
    ]
data PageGetPermissionsPolicyState = PageGetPermissionsPolicyState
  {
    pageGetPermissionsPolicyStateStates :: [PagePermissionsPolicyFeatureState]
  }
  deriving (Eq, Show)
instance FromJSON PageGetPermissionsPolicyState where
  parseJSON = A.withObject "PageGetPermissionsPolicyState" $ \o -> PageGetPermissionsPolicyState
    <$> o A..: "states"
instance Command PPageGetPermissionsPolicyState where
  type CommandResponse PPageGetPermissionsPolicyState = PageGetPermissionsPolicyState
  commandName _ = "Page.getPermissionsPolicyState"

-- | Get Origin Trials on given frame.

-- | Parameters of the 'Page.getOriginTrials' command.
data PPageGetOriginTrials = PPageGetOriginTrials
  {
    pPageGetOriginTrialsFrameId :: PageFrameId
  }
  deriving (Eq, Show)
pPageGetOriginTrials
  :: PageFrameId
  -> PPageGetOriginTrials
pPageGetOriginTrials
  arg_pPageGetOriginTrialsFrameId
  = PPageGetOriginTrials
    arg_pPageGetOriginTrialsFrameId
instance ToJSON PPageGetOriginTrials where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageGetOriginTrialsFrameId p)
    ]
data PageGetOriginTrials = PageGetOriginTrials
  {
    pageGetOriginTrialsOriginTrials :: [PageOriginTrial]
  }
  deriving (Eq, Show)
instance FromJSON PageGetOriginTrials where
  parseJSON = A.withObject "PageGetOriginTrials" $ \o -> PageGetOriginTrials
    <$> o A..: "originTrials"
instance Command PPageGetOriginTrials where
  type CommandResponse PPageGetOriginTrials = PageGetOriginTrials
  commandName _ = "Page.getOriginTrials"

-- | Set generic font families.

-- | Parameters of the 'Page.setFontFamilies' command.
data PPageSetFontFamilies = PPageSetFontFamilies
  {
    -- | Specifies font families to set. If a font family is not specified, it won't be changed.
    pPageSetFontFamiliesFontFamilies :: PageFontFamilies,
    -- | Specifies font families to set for individual scripts.
    pPageSetFontFamiliesForScripts :: Maybe [PageScriptFontFamilies]
  }
  deriving (Eq, Show)
pPageSetFontFamilies
  -- | Specifies font families to set. If a font family is not specified, it won't be changed.
  :: PageFontFamilies
  -> PPageSetFontFamilies
pPageSetFontFamilies
  arg_pPageSetFontFamiliesFontFamilies
  = PPageSetFontFamilies
    arg_pPageSetFontFamiliesFontFamilies
    Nothing
instance ToJSON PPageSetFontFamilies where
  toJSON p = A.object $ catMaybes [
    ("fontFamilies" A..=) <$> Just (pPageSetFontFamiliesFontFamilies p),
    ("forScripts" A..=) <$> (pPageSetFontFamiliesForScripts p)
    ]
instance Command PPageSetFontFamilies where
  type CommandResponse PPageSetFontFamilies = ()
  commandName _ = "Page.setFontFamilies"
  fromJSON = const . A.Success . const ()

-- | Set default font sizes.

-- | Parameters of the 'Page.setFontSizes' command.
data PPageSetFontSizes = PPageSetFontSizes
  {
    -- | Specifies font sizes to set. If a font size is not specified, it won't be changed.
    pPageSetFontSizesFontSizes :: PageFontSizes
  }
  deriving (Eq, Show)
pPageSetFontSizes
  -- | Specifies font sizes to set. If a font size is not specified, it won't be changed.
  :: PageFontSizes
  -> PPageSetFontSizes
pPageSetFontSizes
  arg_pPageSetFontSizesFontSizes
  = PPageSetFontSizes
    arg_pPageSetFontSizesFontSizes
instance ToJSON PPageSetFontSizes where
  toJSON p = A.object $ catMaybes [
    ("fontSizes" A..=) <$> Just (pPageSetFontSizesFontSizes p)
    ]
instance Command PPageSetFontSizes where
  type CommandResponse PPageSetFontSizes = ()
  commandName _ = "Page.setFontSizes"
  fromJSON = const . A.Success . const ()

-- | Sets given markup as the document's HTML.

-- | Parameters of the 'Page.setDocumentContent' command.
data PPageSetDocumentContent = PPageSetDocumentContent
  {
    -- | Frame id to set HTML for.
    pPageSetDocumentContentFrameId :: PageFrameId,
    -- | HTML content to set.
    pPageSetDocumentContentHtml :: T.Text
  }
  deriving (Eq, Show)
pPageSetDocumentContent
  -- | Frame id to set HTML for.
  :: PageFrameId
  -- | HTML content to set.
  -> T.Text
  -> PPageSetDocumentContent
pPageSetDocumentContent
  arg_pPageSetDocumentContentFrameId
  arg_pPageSetDocumentContentHtml
  = PPageSetDocumentContent
    arg_pPageSetDocumentContentFrameId
    arg_pPageSetDocumentContentHtml
instance ToJSON PPageSetDocumentContent where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pPageSetDocumentContentFrameId p),
    ("html" A..=) <$> Just (pPageSetDocumentContentHtml p)
    ]
instance Command PPageSetDocumentContent where
  type CommandResponse PPageSetDocumentContent = ()
  commandName _ = "Page.setDocumentContent"
  fromJSON = const . A.Success . const ()

-- | Controls whether page will emit lifecycle events.

-- | Parameters of the 'Page.setLifecycleEventsEnabled' command.
data PPageSetLifecycleEventsEnabled = PPageSetLifecycleEventsEnabled
  {
    -- | If true, starts emitting lifecycle events.
    pPageSetLifecycleEventsEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pPageSetLifecycleEventsEnabled
  -- | If true, starts emitting lifecycle events.
  :: Bool
  -> PPageSetLifecycleEventsEnabled
pPageSetLifecycleEventsEnabled
  arg_pPageSetLifecycleEventsEnabledEnabled
  = PPageSetLifecycleEventsEnabled
    arg_pPageSetLifecycleEventsEnabledEnabled
instance ToJSON PPageSetLifecycleEventsEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pPageSetLifecycleEventsEnabledEnabled p)
    ]
instance Command PPageSetLifecycleEventsEnabled where
  type CommandResponse PPageSetLifecycleEventsEnabled = ()
  commandName _ = "Page.setLifecycleEventsEnabled"
  fromJSON = const . A.Success . const ()

-- | Starts sending each frame using the `screencastFrame` event.

-- | Parameters of the 'Page.startScreencast' command.
data PPageStartScreencastFormat = PPageStartScreencastFormatJpeg | PPageStartScreencastFormatPng
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPageStartScreencastFormat where
  parseJSON = A.withText "PPageStartScreencastFormat" $ \v -> case v of
    "jpeg" -> pure PPageStartScreencastFormatJpeg
    "png" -> pure PPageStartScreencastFormatPng
    "_" -> fail "failed to parse PPageStartScreencastFormat"
instance ToJSON PPageStartScreencastFormat where
  toJSON v = A.String $ case v of
    PPageStartScreencastFormatJpeg -> "jpeg"
    PPageStartScreencastFormatPng -> "png"
data PPageStartScreencast = PPageStartScreencast
  {
    -- | Image compression format.
    pPageStartScreencastFormat :: Maybe PPageStartScreencastFormat,
    -- | Compression quality from range [0..100].
    pPageStartScreencastQuality :: Maybe Int,
    -- | Maximum screenshot width.
    pPageStartScreencastMaxWidth :: Maybe Int,
    -- | Maximum screenshot height.
    pPageStartScreencastMaxHeight :: Maybe Int,
    -- | Send every n-th frame.
    pPageStartScreencastEveryNthFrame :: Maybe Int
  }
  deriving (Eq, Show)
pPageStartScreencast
  :: PPageStartScreencast
pPageStartScreencast
  = PPageStartScreencast
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PPageStartScreencast where
  toJSON p = A.object $ catMaybes [
    ("format" A..=) <$> (pPageStartScreencastFormat p),
    ("quality" A..=) <$> (pPageStartScreencastQuality p),
    ("maxWidth" A..=) <$> (pPageStartScreencastMaxWidth p),
    ("maxHeight" A..=) <$> (pPageStartScreencastMaxHeight p),
    ("everyNthFrame" A..=) <$> (pPageStartScreencastEveryNthFrame p)
    ]
instance Command PPageStartScreencast where
  type CommandResponse PPageStartScreencast = ()
  commandName _ = "Page.startScreencast"
  fromJSON = const . A.Success . const ()

-- | Force the page stop all navigations and pending resource fetches.

-- | Parameters of the 'Page.stopLoading' command.
data PPageStopLoading = PPageStopLoading
  deriving (Eq, Show)
pPageStopLoading
  :: PPageStopLoading
pPageStopLoading
  = PPageStopLoading
instance ToJSON PPageStopLoading where
  toJSON _ = A.Null
instance Command PPageStopLoading where
  type CommandResponse PPageStopLoading = ()
  commandName _ = "Page.stopLoading"
  fromJSON = const . A.Success . const ()

-- | Crashes renderer on the IO thread, generates minidumps.

-- | Parameters of the 'Page.crash' command.
data PPageCrash = PPageCrash
  deriving (Eq, Show)
pPageCrash
  :: PPageCrash
pPageCrash
  = PPageCrash
instance ToJSON PPageCrash where
  toJSON _ = A.Null
instance Command PPageCrash where
  type CommandResponse PPageCrash = ()
  commandName _ = "Page.crash"
  fromJSON = const . A.Success . const ()

-- | Tries to close page, running its beforeunload hooks, if any.

-- | Parameters of the 'Page.close' command.
data PPageClose = PPageClose
  deriving (Eq, Show)
pPageClose
  :: PPageClose
pPageClose
  = PPageClose
instance ToJSON PPageClose where
  toJSON _ = A.Null
instance Command PPageClose where
  type CommandResponse PPageClose = ()
  commandName _ = "Page.close"
  fromJSON = const . A.Success . const ()

-- | Tries to update the web lifecycle state of the page.
--   It will transition the page to the given state according to:
--   https://github.com/WICG/web-lifecycle/

-- | Parameters of the 'Page.setWebLifecycleState' command.
data PPageSetWebLifecycleStateState = PPageSetWebLifecycleStateStateFrozen | PPageSetWebLifecycleStateStateActive
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPageSetWebLifecycleStateState where
  parseJSON = A.withText "PPageSetWebLifecycleStateState" $ \v -> case v of
    "frozen" -> pure PPageSetWebLifecycleStateStateFrozen
    "active" -> pure PPageSetWebLifecycleStateStateActive
    "_" -> fail "failed to parse PPageSetWebLifecycleStateState"
instance ToJSON PPageSetWebLifecycleStateState where
  toJSON v = A.String $ case v of
    PPageSetWebLifecycleStateStateFrozen -> "frozen"
    PPageSetWebLifecycleStateStateActive -> "active"
data PPageSetWebLifecycleState = PPageSetWebLifecycleState
  {
    -- | Target lifecycle state
    pPageSetWebLifecycleStateState :: PPageSetWebLifecycleStateState
  }
  deriving (Eq, Show)
pPageSetWebLifecycleState
  -- | Target lifecycle state
  :: PPageSetWebLifecycleStateState
  -> PPageSetWebLifecycleState
pPageSetWebLifecycleState
  arg_pPageSetWebLifecycleStateState
  = PPageSetWebLifecycleState
    arg_pPageSetWebLifecycleStateState
instance ToJSON PPageSetWebLifecycleState where
  toJSON p = A.object $ catMaybes [
    ("state" A..=) <$> Just (pPageSetWebLifecycleStateState p)
    ]
instance Command PPageSetWebLifecycleState where
  type CommandResponse PPageSetWebLifecycleState = ()
  commandName _ = "Page.setWebLifecycleState"
  fromJSON = const . A.Success . const ()

-- | Stops sending each frame in the `screencastFrame`.

-- | Parameters of the 'Page.stopScreencast' command.
data PPageStopScreencast = PPageStopScreencast
  deriving (Eq, Show)
pPageStopScreencast
  :: PPageStopScreencast
pPageStopScreencast
  = PPageStopScreencast
instance ToJSON PPageStopScreencast where
  toJSON _ = A.Null
instance Command PPageStopScreencast where
  type CommandResponse PPageStopScreencast = ()
  commandName _ = "Page.stopScreencast"
  fromJSON = const . A.Success . const ()

-- | Requests backend to produce compilation cache for the specified scripts.
--   `scripts` are appeneded to the list of scripts for which the cache
--   would be produced. The list may be reset during page navigation.
--   When script with a matching URL is encountered, the cache is optionally
--   produced upon backend discretion, based on internal heuristics.
--   See also: `Page.compilationCacheProduced`.

-- | Parameters of the 'Page.produceCompilationCache' command.
data PPageProduceCompilationCache = PPageProduceCompilationCache
  {
    pPageProduceCompilationCacheScripts :: [PageCompilationCacheParams]
  }
  deriving (Eq, Show)
pPageProduceCompilationCache
  :: [PageCompilationCacheParams]
  -> PPageProduceCompilationCache
pPageProduceCompilationCache
  arg_pPageProduceCompilationCacheScripts
  = PPageProduceCompilationCache
    arg_pPageProduceCompilationCacheScripts
instance ToJSON PPageProduceCompilationCache where
  toJSON p = A.object $ catMaybes [
    ("scripts" A..=) <$> Just (pPageProduceCompilationCacheScripts p)
    ]
instance Command PPageProduceCompilationCache where
  type CommandResponse PPageProduceCompilationCache = ()
  commandName _ = "Page.produceCompilationCache"
  fromJSON = const . A.Success . const ()

-- | Seeds compilation cache for given url. Compilation cache does not survive
--   cross-process navigation.

-- | Parameters of the 'Page.addCompilationCache' command.
data PPageAddCompilationCache = PPageAddCompilationCache
  {
    pPageAddCompilationCacheUrl :: T.Text,
    -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
    pPageAddCompilationCacheData :: T.Text
  }
  deriving (Eq, Show)
pPageAddCompilationCache
  :: T.Text
  -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
  -> T.Text
  -> PPageAddCompilationCache
pPageAddCompilationCache
  arg_pPageAddCompilationCacheUrl
  arg_pPageAddCompilationCacheData
  = PPageAddCompilationCache
    arg_pPageAddCompilationCacheUrl
    arg_pPageAddCompilationCacheData
instance ToJSON PPageAddCompilationCache where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pPageAddCompilationCacheUrl p),
    ("data" A..=) <$> Just (pPageAddCompilationCacheData p)
    ]
instance Command PPageAddCompilationCache where
  type CommandResponse PPageAddCompilationCache = ()
  commandName _ = "Page.addCompilationCache"
  fromJSON = const . A.Success . const ()

-- | Clears seeded compilation cache.

-- | Parameters of the 'Page.clearCompilationCache' command.
data PPageClearCompilationCache = PPageClearCompilationCache
  deriving (Eq, Show)
pPageClearCompilationCache
  :: PPageClearCompilationCache
pPageClearCompilationCache
  = PPageClearCompilationCache
instance ToJSON PPageClearCompilationCache where
  toJSON _ = A.Null
instance Command PPageClearCompilationCache where
  type CommandResponse PPageClearCompilationCache = ()
  commandName _ = "Page.clearCompilationCache"
  fromJSON = const . A.Success . const ()

-- | Sets the Secure Payment Confirmation transaction mode.
--   https://w3c.github.io/secure-payment-confirmation/#sctn-automation-set-spc-transaction-mode

-- | Parameters of the 'Page.setSPCTransactionMode' command.
data PPageSetSPCTransactionModeMode = PPageSetSPCTransactionModeModeNone | PPageSetSPCTransactionModeModeAutoaccept | PPageSetSPCTransactionModeModeAutoreject
  deriving (Ord, Eq, Show, Read)
instance FromJSON PPageSetSPCTransactionModeMode where
  parseJSON = A.withText "PPageSetSPCTransactionModeMode" $ \v -> case v of
    "none" -> pure PPageSetSPCTransactionModeModeNone
    "autoaccept" -> pure PPageSetSPCTransactionModeModeAutoaccept
    "autoreject" -> pure PPageSetSPCTransactionModeModeAutoreject
    "_" -> fail "failed to parse PPageSetSPCTransactionModeMode"
instance ToJSON PPageSetSPCTransactionModeMode where
  toJSON v = A.String $ case v of
    PPageSetSPCTransactionModeModeNone -> "none"
    PPageSetSPCTransactionModeModeAutoaccept -> "autoaccept"
    PPageSetSPCTransactionModeModeAutoreject -> "autoreject"
data PPageSetSPCTransactionMode = PPageSetSPCTransactionMode
  {
    pPageSetSPCTransactionModeMode :: PPageSetSPCTransactionModeMode
  }
  deriving (Eq, Show)
pPageSetSPCTransactionMode
  :: PPageSetSPCTransactionModeMode
  -> PPageSetSPCTransactionMode
pPageSetSPCTransactionMode
  arg_pPageSetSPCTransactionModeMode
  = PPageSetSPCTransactionMode
    arg_pPageSetSPCTransactionModeMode
instance ToJSON PPageSetSPCTransactionMode where
  toJSON p = A.object $ catMaybes [
    ("mode" A..=) <$> Just (pPageSetSPCTransactionModeMode p)
    ]
instance Command PPageSetSPCTransactionMode where
  type CommandResponse PPageSetSPCTransactionMode = ()
  commandName _ = "Page.setSPCTransactionMode"
  fromJSON = const . A.Success . const ()

-- | Generates a report for testing.

-- | Parameters of the 'Page.generateTestReport' command.
data PPageGenerateTestReport = PPageGenerateTestReport
  {
    -- | Message to be displayed in the report.
    pPageGenerateTestReportMessage :: T.Text,
    -- | Specifies the endpoint group to deliver the report to.
    pPageGenerateTestReportGroup :: Maybe T.Text
  }
  deriving (Eq, Show)
pPageGenerateTestReport
  -- | Message to be displayed in the report.
  :: T.Text
  -> PPageGenerateTestReport
pPageGenerateTestReport
  arg_pPageGenerateTestReportMessage
  = PPageGenerateTestReport
    arg_pPageGenerateTestReportMessage
    Nothing
instance ToJSON PPageGenerateTestReport where
  toJSON p = A.object $ catMaybes [
    ("message" A..=) <$> Just (pPageGenerateTestReportMessage p),
    ("group" A..=) <$> (pPageGenerateTestReportGroup p)
    ]
instance Command PPageGenerateTestReport where
  type CommandResponse PPageGenerateTestReport = ()
  commandName _ = "Page.generateTestReport"
  fromJSON = const . A.Success . const ()

-- | Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.

-- | Parameters of the 'Page.waitForDebugger' command.
data PPageWaitForDebugger = PPageWaitForDebugger
  deriving (Eq, Show)
pPageWaitForDebugger
  :: PPageWaitForDebugger
pPageWaitForDebugger
  = PPageWaitForDebugger
instance ToJSON PPageWaitForDebugger where
  toJSON _ = A.Null
instance Command PPageWaitForDebugger where
  type CommandResponse PPageWaitForDebugger = ()
  commandName _ = "Page.waitForDebugger"
  fromJSON = const . A.Success . const ()

-- | Intercept file chooser requests and transfer control to protocol clients.
--   When file chooser interception is enabled, native file chooser dialog is not shown.
--   Instead, a protocol event `Page.fileChooserOpened` is emitted.

-- | Parameters of the 'Page.setInterceptFileChooserDialog' command.
data PPageSetInterceptFileChooserDialog = PPageSetInterceptFileChooserDialog
  {
    pPageSetInterceptFileChooserDialogEnabled :: Bool
  }
  deriving (Eq, Show)
pPageSetInterceptFileChooserDialog
  :: Bool
  -> PPageSetInterceptFileChooserDialog
pPageSetInterceptFileChooserDialog
  arg_pPageSetInterceptFileChooserDialogEnabled
  = PPageSetInterceptFileChooserDialog
    arg_pPageSetInterceptFileChooserDialogEnabled
instance ToJSON PPageSetInterceptFileChooserDialog where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pPageSetInterceptFileChooserDialogEnabled p)
    ]
instance Command PPageSetInterceptFileChooserDialog where
  type CommandResponse PPageSetInterceptFileChooserDialog = ()
  commandName _ = "Page.setInterceptFileChooserDialog"
  fromJSON = const . A.Success . const ()

-- | Type 'Security.CertificateId'.
--   An internal certificate ID value.
type SecurityCertificateId = Int

-- | Type 'Security.MixedContentType'.
--   A description of mixed content (HTTP resources on HTTPS pages), as defined by
--   https://www.w3.org/TR/mixed-content/#categories
data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
  deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
  parseJSON = A.withText "SecurityMixedContentType" $ \v -> case v of
    "blockable" -> pure SecurityMixedContentTypeBlockable
    "optionally-blockable" -> pure SecurityMixedContentTypeOptionallyBlockable
    "none" -> pure SecurityMixedContentTypeNone
    "_" -> fail "failed to parse SecurityMixedContentType"
instance ToJSON SecurityMixedContentType where
  toJSON v = A.String $ case v of
    SecurityMixedContentTypeBlockable -> "blockable"
    SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
    SecurityMixedContentTypeNone -> "none"

-- | Type 'Security.SecurityState'.
--   The security level of a page or resource.
data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
  deriving (Ord, Eq, Show, Read)
instance FromJSON SecuritySecurityState where
  parseJSON = A.withText "SecuritySecurityState" $ \v -> case v of
    "unknown" -> pure SecuritySecurityStateUnknown
    "neutral" -> pure SecuritySecurityStateNeutral
    "insecure" -> pure SecuritySecurityStateInsecure
    "secure" -> pure SecuritySecurityStateSecure
    "info" -> pure SecuritySecurityStateInfo
    "insecure-broken" -> pure SecuritySecurityStateInsecureBroken
    "_" -> fail "failed to parse SecuritySecurityState"
instance ToJSON SecuritySecurityState where
  toJSON v = A.String $ case v of
    SecuritySecurityStateUnknown -> "unknown"
    SecuritySecurityStateNeutral -> "neutral"
    SecuritySecurityStateInsecure -> "insecure"
    SecuritySecurityStateSecure -> "secure"
    SecuritySecurityStateInfo -> "info"
    SecuritySecurityStateInsecureBroken -> "insecure-broken"

-- | Type 'Security.CertificateSecurityState'.
--   Details about the security state of the page certificate.
data SecurityCertificateSecurityState = SecurityCertificateSecurityState
  {
    -- | Protocol name (e.g. "TLS 1.2" or "QUIC").
    securityCertificateSecurityStateProtocol :: T.Text,
    -- | Key Exchange used by the connection, or the empty string if not applicable.
    securityCertificateSecurityStateKeyExchange :: T.Text,
    -- | (EC)DH group used by the connection, if applicable.
    securityCertificateSecurityStateKeyExchangeGroup :: Maybe T.Text,
    -- | Cipher name.
    securityCertificateSecurityStateCipher :: T.Text,
    -- | TLS MAC. Note that AEAD ciphers do not have separate MACs.
    securityCertificateSecurityStateMac :: Maybe T.Text,
    -- | Page certificate.
    securityCertificateSecurityStateCertificate :: [T.Text],
    -- | Certificate subject name.
    securityCertificateSecurityStateSubjectName :: T.Text,
    -- | Name of the issuing CA.
    securityCertificateSecurityStateIssuer :: T.Text,
    -- | Certificate valid from date.
    securityCertificateSecurityStateValidFrom :: NetworkTimeSinceEpoch,
    -- | Certificate valid to (expiration) date
    securityCertificateSecurityStateValidTo :: NetworkTimeSinceEpoch,
    -- | The highest priority network error code, if the certificate has an error.
    securityCertificateSecurityStateCertificateNetworkError :: Maybe T.Text,
    -- | True if the certificate uses a weak signature aglorithm.
    securityCertificateSecurityStateCertificateHasWeakSignature :: Bool,
    -- | True if the certificate has a SHA1 signature in the chain.
    securityCertificateSecurityStateCertificateHasSha1Signature :: Bool,
    -- | True if modern SSL
    securityCertificateSecurityStateModernSSL :: Bool,
    -- | True if the connection is using an obsolete SSL protocol.
    securityCertificateSecurityStateObsoleteSslProtocol :: Bool,
    -- | True if the connection is using an obsolete SSL key exchange.
    securityCertificateSecurityStateObsoleteSslKeyExchange :: Bool,
    -- | True if the connection is using an obsolete SSL cipher.
    securityCertificateSecurityStateObsoleteSslCipher :: Bool,
    -- | True if the connection is using an obsolete SSL signature.
    securityCertificateSecurityStateObsoleteSslSignature :: Bool
  }
  deriving (Eq, Show)
instance FromJSON SecurityCertificateSecurityState where
  parseJSON = A.withObject "SecurityCertificateSecurityState" $ \o -> SecurityCertificateSecurityState
    <$> o A..: "protocol"
    <*> o A..: "keyExchange"
    <*> o A..:? "keyExchangeGroup"
    <*> o A..: "cipher"
    <*> o A..:? "mac"
    <*> o A..: "certificate"
    <*> o A..: "subjectName"
    <*> o A..: "issuer"
    <*> o A..: "validFrom"
    <*> o A..: "validTo"
    <*> o A..:? "certificateNetworkError"
    <*> o A..: "certificateHasWeakSignature"
    <*> o A..: "certificateHasSha1Signature"
    <*> o A..: "modernSSL"
    <*> o A..: "obsoleteSslProtocol"
    <*> o A..: "obsoleteSslKeyExchange"
    <*> o A..: "obsoleteSslCipher"
    <*> o A..: "obsoleteSslSignature"
instance ToJSON SecurityCertificateSecurityState where
  toJSON p = A.object $ catMaybes [
    ("protocol" A..=) <$> Just (securityCertificateSecurityStateProtocol p),
    ("keyExchange" A..=) <$> Just (securityCertificateSecurityStateKeyExchange p),
    ("keyExchangeGroup" A..=) <$> (securityCertificateSecurityStateKeyExchangeGroup p),
    ("cipher" A..=) <$> Just (securityCertificateSecurityStateCipher p),
    ("mac" A..=) <$> (securityCertificateSecurityStateMac p),
    ("certificate" A..=) <$> Just (securityCertificateSecurityStateCertificate p),
    ("subjectName" A..=) <$> Just (securityCertificateSecurityStateSubjectName p),
    ("issuer" A..=) <$> Just (securityCertificateSecurityStateIssuer p),
    ("validFrom" A..=) <$> Just (securityCertificateSecurityStateValidFrom p),
    ("validTo" A..=) <$> Just (securityCertificateSecurityStateValidTo p),
    ("certificateNetworkError" A..=) <$> (securityCertificateSecurityStateCertificateNetworkError p),
    ("certificateHasWeakSignature" A..=) <$> Just (securityCertificateSecurityStateCertificateHasWeakSignature p),
    ("certificateHasSha1Signature" A..=) <$> Just (securityCertificateSecurityStateCertificateHasSha1Signature p),
    ("modernSSL" A..=) <$> Just (securityCertificateSecurityStateModernSSL p),
    ("obsoleteSslProtocol" A..=) <$> Just (securityCertificateSecurityStateObsoleteSslProtocol p),
    ("obsoleteSslKeyExchange" A..=) <$> Just (securityCertificateSecurityStateObsoleteSslKeyExchange p),
    ("obsoleteSslCipher" A..=) <$> Just (securityCertificateSecurityStateObsoleteSslCipher p),
    ("obsoleteSslSignature" A..=) <$> Just (securityCertificateSecurityStateObsoleteSslSignature p)
    ]

-- | Type 'Security.SafetyTipStatus'.
data SecuritySafetyTipStatus = SecuritySafetyTipStatusBadReputation | SecuritySafetyTipStatusLookalike
  deriving (Ord, Eq, Show, Read)
instance FromJSON SecuritySafetyTipStatus where
  parseJSON = A.withText "SecuritySafetyTipStatus" $ \v -> case v of
    "badReputation" -> pure SecuritySafetyTipStatusBadReputation
    "lookalike" -> pure SecuritySafetyTipStatusLookalike
    "_" -> fail "failed to parse SecuritySafetyTipStatus"
instance ToJSON SecuritySafetyTipStatus where
  toJSON v = A.String $ case v of
    SecuritySafetyTipStatusBadReputation -> "badReputation"
    SecuritySafetyTipStatusLookalike -> "lookalike"

-- | Type 'Security.SafetyTipInfo'.
data SecuritySafetyTipInfo = SecuritySafetyTipInfo
  {
    -- | Describes whether the page triggers any safety tips or reputation warnings. Default is unknown.
    securitySafetyTipInfoSafetyTipStatus :: SecuritySafetyTipStatus,
    -- | The URL the safety tip suggested ("Did you mean?"). Only filled in for lookalike matches.
    securitySafetyTipInfoSafeUrl :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON SecuritySafetyTipInfo where
  parseJSON = A.withObject "SecuritySafetyTipInfo" $ \o -> SecuritySafetyTipInfo
    <$> o A..: "safetyTipStatus"
    <*> o A..:? "safeUrl"
instance ToJSON SecuritySafetyTipInfo where
  toJSON p = A.object $ catMaybes [
    ("safetyTipStatus" A..=) <$> Just (securitySafetyTipInfoSafetyTipStatus p),
    ("safeUrl" A..=) <$> (securitySafetyTipInfoSafeUrl p)
    ]

-- | Type 'Security.VisibleSecurityState'.
--   Security state information about the page.
data SecurityVisibleSecurityState = SecurityVisibleSecurityState
  {
    -- | The security level of the page.
    securityVisibleSecurityStateSecurityState :: SecuritySecurityState,
    -- | Security state details about the page certificate.
    securityVisibleSecurityStateCertificateSecurityState :: Maybe SecurityCertificateSecurityState,
    -- | The type of Safety Tip triggered on the page. Note that this field will be set even if the Safety Tip UI was not actually shown.
    securityVisibleSecurityStateSafetyTipInfo :: Maybe SecuritySafetyTipInfo,
    -- | Array of security state issues ids.
    securityVisibleSecurityStateSecurityStateIssueIds :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON SecurityVisibleSecurityState where
  parseJSON = A.withObject "SecurityVisibleSecurityState" $ \o -> SecurityVisibleSecurityState
    <$> o A..: "securityState"
    <*> o A..:? "certificateSecurityState"
    <*> o A..:? "safetyTipInfo"
    <*> o A..: "securityStateIssueIds"
instance ToJSON SecurityVisibleSecurityState where
  toJSON p = A.object $ catMaybes [
    ("securityState" A..=) <$> Just (securityVisibleSecurityStateSecurityState p),
    ("certificateSecurityState" A..=) <$> (securityVisibleSecurityStateCertificateSecurityState p),
    ("safetyTipInfo" A..=) <$> (securityVisibleSecurityStateSafetyTipInfo p),
    ("securityStateIssueIds" A..=) <$> Just (securityVisibleSecurityStateSecurityStateIssueIds p)
    ]

-- | Type 'Security.SecurityStateExplanation'.
--   An explanation of an factor contributing to the security state.
data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation
  {
    -- | Security state representing the severity of the factor being explained.
    securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
    -- | Title describing the type of factor.
    securitySecurityStateExplanationTitle :: T.Text,
    -- | Short phrase describing the type of factor.
    securitySecurityStateExplanationSummary :: T.Text,
    -- | Full text explanation of the factor.
    securitySecurityStateExplanationDescription :: T.Text,
    -- | The type of mixed content described by the explanation.
    securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
    -- | Page certificate.
    securitySecurityStateExplanationCertificate :: [T.Text],
    -- | Recommendations to fix any issues.
    securitySecurityStateExplanationRecommendations :: Maybe [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON SecuritySecurityStateExplanation where
  parseJSON = A.withObject "SecuritySecurityStateExplanation" $ \o -> SecuritySecurityStateExplanation
    <$> o A..: "securityState"
    <*> o A..: "title"
    <*> o A..: "summary"
    <*> o A..: "description"
    <*> o A..: "mixedContentType"
    <*> o A..: "certificate"
    <*> o A..:? "recommendations"
instance ToJSON SecuritySecurityStateExplanation where
  toJSON p = A.object $ catMaybes [
    ("securityState" A..=) <$> Just (securitySecurityStateExplanationSecurityState p),
    ("title" A..=) <$> Just (securitySecurityStateExplanationTitle p),
    ("summary" A..=) <$> Just (securitySecurityStateExplanationSummary p),
    ("description" A..=) <$> Just (securitySecurityStateExplanationDescription p),
    ("mixedContentType" A..=) <$> Just (securitySecurityStateExplanationMixedContentType p),
    ("certificate" A..=) <$> Just (securitySecurityStateExplanationCertificate p),
    ("recommendations" A..=) <$> (securitySecurityStateExplanationRecommendations p)
    ]

-- | Type 'Security.CertificateErrorAction'.
--   The action to take when a certificate error occurs. continue will continue processing the
--   request and cancel will cancel the request.
data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
  deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityCertificateErrorAction where
  parseJSON = A.withText "SecurityCertificateErrorAction" $ \v -> case v of
    "continue" -> pure SecurityCertificateErrorActionContinue
    "cancel" -> pure SecurityCertificateErrorActionCancel
    "_" -> fail "failed to parse SecurityCertificateErrorAction"
instance ToJSON SecurityCertificateErrorAction where
  toJSON v = A.String $ case v of
    SecurityCertificateErrorActionContinue -> "continue"
    SecurityCertificateErrorActionCancel -> "cancel"

-- | Type of the 'Security.visibleSecurityStateChanged' event.
data SecurityVisibleSecurityStateChanged = SecurityVisibleSecurityStateChanged
  {
    -- | Security state information about the page.
    securityVisibleSecurityStateChangedVisibleSecurityState :: SecurityVisibleSecurityState
  }
  deriving (Eq, Show)
instance FromJSON SecurityVisibleSecurityStateChanged where
  parseJSON = A.withObject "SecurityVisibleSecurityStateChanged" $ \o -> SecurityVisibleSecurityStateChanged
    <$> o A..: "visibleSecurityState"
instance Event SecurityVisibleSecurityStateChanged where
  eventName _ = "Security.visibleSecurityStateChanged"

-- | Disables tracking security state changes.

-- | Parameters of the 'Security.disable' command.
data PSecurityDisable = PSecurityDisable
  deriving (Eq, Show)
pSecurityDisable
  :: PSecurityDisable
pSecurityDisable
  = PSecurityDisable
instance ToJSON PSecurityDisable where
  toJSON _ = A.Null
instance Command PSecurityDisable where
  type CommandResponse PSecurityDisable = ()
  commandName _ = "Security.disable"
  fromJSON = const . A.Success . const ()

-- | Enables tracking security state changes.

-- | Parameters of the 'Security.enable' command.
data PSecurityEnable = PSecurityEnable
  deriving (Eq, Show)
pSecurityEnable
  :: PSecurityEnable
pSecurityEnable
  = PSecurityEnable
instance ToJSON PSecurityEnable where
  toJSON _ = A.Null
instance Command PSecurityEnable where
  type CommandResponse PSecurityEnable = ()
  commandName _ = "Security.enable"
  fromJSON = const . A.Success . const ()

-- | Enable/disable whether all certificate errors should be ignored.

-- | Parameters of the 'Security.setIgnoreCertificateErrors' command.
data PSecuritySetIgnoreCertificateErrors = PSecuritySetIgnoreCertificateErrors
  {
    -- | If true, all certificate errors will be ignored.
    pSecuritySetIgnoreCertificateErrorsIgnore :: Bool
  }
  deriving (Eq, Show)
pSecuritySetIgnoreCertificateErrors
  -- | If true, all certificate errors will be ignored.
  :: Bool
  -> PSecuritySetIgnoreCertificateErrors
pSecuritySetIgnoreCertificateErrors
  arg_pSecuritySetIgnoreCertificateErrorsIgnore
  = PSecuritySetIgnoreCertificateErrors
    arg_pSecuritySetIgnoreCertificateErrorsIgnore
instance ToJSON PSecuritySetIgnoreCertificateErrors where
  toJSON p = A.object $ catMaybes [
    ("ignore" A..=) <$> Just (pSecuritySetIgnoreCertificateErrorsIgnore p)
    ]
instance Command PSecuritySetIgnoreCertificateErrors where
  type CommandResponse PSecuritySetIgnoreCertificateErrors = ()
  commandName _ = "Security.setIgnoreCertificateErrors"
  fromJSON = const . A.Success . const ()

