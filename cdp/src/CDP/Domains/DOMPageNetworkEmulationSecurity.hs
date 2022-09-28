{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  DOM :
     This domain exposes DOM read/write operations. Each DOM Node is represented with its mirror object
     that has an `id`. This `id` can be used to get additional information on the Node, resolve it into
     the JavaScript object wrapper, etc. It is important that client receives DOM events only for the
     nodes that are known to the client. Backend keeps track of the nodes that were sent to the client
     and never sends the same node twice. It is client's responsibility to collect information about
     the nodes that were sent to the client.<p>Note that `iframe` owner elements will return
     corresponding document elements as their child nodes.</p>

  Emulation :
     This domain emulates different environments for the page.

  Network :
     Network domain allows tracking network activities of the page. It exposes information about http,
     file, data and other requests and responses, their headers, bodies, timing, etc.

  Page :
     Actions and events related to the inspected page belong to the page domain.

  Security :
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

import CDP.Domains.Debugger as Debugger
import CDP.Domains.IO as IO
import CDP.Domains.Runtime as Runtime


-- | Unique DOM node identifier.
type DomNodeId = Int

-- | Unique DOM node identifier used to reference a node that may not have been pushed to the
-- front-end.
type DomBackendNodeId = Int

-- | Backend node with a friendly name.
data DomBackendNode = DomBackendNode {
   domBackendNodeNodeType :: DomBackendNodeNodeType, -- ^ `Node`'s nodeType.
   domBackendNodeNodeName :: DomBackendNodeNodeName, -- ^ `Node`'s nodeName.

} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBackendNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DomBackendNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Pseudo element type.
data DomPseudoType = DomPseudoTypeFirstLine | DomPseudoTypeFirstLetter | DomPseudoTypeBefore | DomPseudoTypeAfter | DomPseudoTypeMarker | DomPseudoTypeBackdrop | DomPseudoTypeSelection | DomPseudoTypeTargetText | DomPseudoTypeSpellingError | DomPseudoTypeGrammarError | DomPseudoTypeHighlight | DomPseudoTypeFirstLineInherited | DomPseudoTypeScrollbar | DomPseudoTypeScrollbarThumb | DomPseudoTypeScrollbarButton | DomPseudoTypeScrollbarTrack | DomPseudoTypeScrollbarTrackPiece | DomPseudoTypeScrollbarCorner | DomPseudoTypeResizer | DomPseudoTypeInputListButton | DomPseudoTypePageTransition | DomPseudoTypePageTransitionContainer | DomPseudoTypePageTransitionImageWrapper | DomPseudoTypePageTransitionOutgoingImage | DomPseudoTypePageTransitionIncomingImage
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomPseudoType where
   parseJSON = A.withText  "DomPseudoType"  $ \v -> do
      case v of
         "first-line" -> pure DomPseudoTypeFirstLine
         "first-letter" -> pure DomPseudoTypeFirstLetter
         "before" -> pure DomPseudoTypeBefore
         "after" -> pure DomPseudoTypeAfter
         "marker" -> pure DomPseudoTypeMarker
         "backdrop" -> pure DomPseudoTypeBackdrop
         "selection" -> pure DomPseudoTypeSelection
         "target-text" -> pure DomPseudoTypeTargetText
         "spelling-error" -> pure DomPseudoTypeSpellingError
         "grammar-error" -> pure DomPseudoTypeGrammarError
         "highlight" -> pure DomPseudoTypeHighlight
         "first-line-inherited" -> pure DomPseudoTypeFirstLineInherited
         "scrollbar" -> pure DomPseudoTypeScrollbar
         "scrollbar-thumb" -> pure DomPseudoTypeScrollbarThumb
         "scrollbar-button" -> pure DomPseudoTypeScrollbarButton
         "scrollbar-track" -> pure DomPseudoTypeScrollbarTrack
         "scrollbar-track-piece" -> pure DomPseudoTypeScrollbarTrackPiece
         "scrollbar-corner" -> pure DomPseudoTypeScrollbarCorner
         "resizer" -> pure DomPseudoTypeResizer
         "input-list-button" -> pure DomPseudoTypeInputListButton
         "page-transition" -> pure DomPseudoTypePageTransition
         "page-transition-container" -> pure DomPseudoTypePageTransitionContainer
         "page-transition-image-wrapper" -> pure DomPseudoTypePageTransitionImageWrapper
         "page-transition-outgoing-image" -> pure DomPseudoTypePageTransitionOutgoingImage
         "page-transition-incoming-image" -> pure DomPseudoTypePageTransitionIncomingImage
         _ -> fail "failed to parse DomPseudoType"

instance ToJSON DomPseudoType where
   toJSON v = A.String $
      case v of
         DomPseudoTypeFirstLine -> "first-line"
         DomPseudoTypeFirstLetter -> "first-letter"
         DomPseudoTypeBefore -> "before"
         DomPseudoTypeAfter -> "after"
         DomPseudoTypeMarker -> "marker"
         DomPseudoTypeBackdrop -> "backdrop"
         DomPseudoTypeSelection -> "selection"
         DomPseudoTypeTargetText -> "target-text"
         DomPseudoTypeSpellingError -> "spelling-error"
         DomPseudoTypeGrammarError -> "grammar-error"
         DomPseudoTypeHighlight -> "highlight"
         DomPseudoTypeFirstLineInherited -> "first-line-inherited"
         DomPseudoTypeScrollbar -> "scrollbar"
         DomPseudoTypeScrollbarThumb -> "scrollbar-thumb"
         DomPseudoTypeScrollbarButton -> "scrollbar-button"
         DomPseudoTypeScrollbarTrack -> "scrollbar-track"
         DomPseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
         DomPseudoTypeScrollbarCorner -> "scrollbar-corner"
         DomPseudoTypeResizer -> "resizer"
         DomPseudoTypeInputListButton -> "input-list-button"
         DomPseudoTypePageTransition -> "page-transition"
         DomPseudoTypePageTransitionContainer -> "page-transition-container"
         DomPseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
         DomPseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
         DomPseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"



-- | Shadow root type.
data DomShadowRootType = DomShadowRootTypeUserAgent | DomShadowRootTypeOpen | DomShadowRootTypeClosed
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomShadowRootType where
   parseJSON = A.withText  "DomShadowRootType"  $ \v -> do
      case v of
         "user-agent" -> pure DomShadowRootTypeUserAgent
         "open" -> pure DomShadowRootTypeOpen
         "closed" -> pure DomShadowRootTypeClosed
         _ -> fail "failed to parse DomShadowRootType"

instance ToJSON DomShadowRootType where
   toJSON v = A.String $
      case v of
         DomShadowRootTypeUserAgent -> "user-agent"
         DomShadowRootTypeOpen -> "open"
         DomShadowRootTypeClosed -> "closed"



-- | Document compatibility mode.
data DomCompatibilityMode = DomCompatibilityModeQuirksMode | DomCompatibilityModeLimitedQuirksMode | DomCompatibilityModeNoQuirksMode
   deriving (Ord, Eq, Show, Read)
instance FromJSON DomCompatibilityMode where
   parseJSON = A.withText  "DomCompatibilityMode"  $ \v -> do
      case v of
         "QuirksMode" -> pure DomCompatibilityModeQuirksMode
         "LimitedQuirksMode" -> pure DomCompatibilityModeLimitedQuirksMode
         "NoQuirksMode" -> pure DomCompatibilityModeNoQuirksMode
         _ -> fail "failed to parse DomCompatibilityMode"

instance ToJSON DomCompatibilityMode where
   toJSON v = A.String $
      case v of
         DomCompatibilityModeQuirksMode -> "QuirksMode"
         DomCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
         DomCompatibilityModeNoQuirksMode -> "NoQuirksMode"



-- | DOM interaction is implemented in terms of mirror objects that represent the actual DOM nodes.
-- DOMNode is a base node mirror type.
data DomNode = DomNode {
   domNodeNodeId :: DomNodeNodeId, -- ^ Node identifier that is passed into the rest of the DOM messages as the `nodeId`. Backend
will only push node with given `id` once. It is aware of all requested nodes and will only
fire DOM events for nodes known to the client.
   domNodeParentId :: DomNodeParentId, -- ^ The id of the parent node if any.
   domNodeBackendNodeId :: DomNodeBackendNodeId, -- ^ The BackendNodeId for this node.
   domNodeNodeType :: DomNodeNodeType, -- ^ `Node`'s nodeType.
   domNodeNodeName :: DomNodeNodeName, -- ^ `Node`'s nodeName.
   domNodeLocalName :: DomNodeLocalName, -- ^ `Node`'s localName.
   domNodeNodeValue :: DomNodeNodeValue, -- ^ `Node`'s nodeValue.
   domNodeChildNodeCount :: DomNodeChildNodeCount, -- ^ Child count for `Container` nodes.
   domNodeChildren :: DomNodeChildren, -- ^ Child nodes of this node when requested with children.
   domNodeAttributes :: DomNodeAttributes, -- ^ Attributes of the `Element` node in the form of flat array `[name1, value1, name2, value2]`.
   domNodeDocumentUrl :: DomNodeDocumentUrl, -- ^ Document URL that `Document` or `FrameOwner` node points to.
   domNodeBaseUrl :: DomNodeBaseUrl, -- ^ Base URL that `Document` or `FrameOwner` node uses for URL completion.
   domNodePublicId :: DomNodePublicId, -- ^ `DocumentType`'s publicId.
   domNodeSystemId :: DomNodeSystemId, -- ^ `DocumentType`'s systemId.
   domNodeInternalSubset :: DomNodeInternalSubset, -- ^ `DocumentType`'s internalSubset.
   domNodeXmlVersion :: DomNodeXmlVersion, -- ^ `Document`'s XML version in case of XML documents.
   domNodeName :: DomNodeName, -- ^ `Attr`'s name.
   domNodeValue :: DomNodeValue, -- ^ `Attr`'s value.
   domNodePseudoType :: DomNodePseudoType, -- ^ Pseudo element type for this node.
   domNodeShadowRootType :: DomNodeShadowRootType, -- ^ Shadow root type.
   domNodeFrameId :: DomNodeFrameId, -- ^ Frame ID for frame owner elements.
   domNodeContentDocument :: DomNodeContentDocument, -- ^ Content document for frame owner elements.
   domNodeShadowRoots :: DomNodeShadowRoots, -- ^ Shadow root list for given element host.
   domNodeTemplateContent :: DomNodeTemplateContent, -- ^ Content document fragment for template elements.
   domNodePseudoElements :: DomNodePseudoElements, -- ^ Pseudo elements associated with this node.
   domNodeDistributedNodes :: DomNodeDistributedNodes, -- ^ Distributed nodes for given insertion point.
   domNodeIsSvg :: DomNodeIsSvg, -- ^ Whether the node is SVG.


} deriving (Generic, Eq, Show, Read)
instance ToJSON DomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | A structure holding an RGBA color.
data DomRgba = DomRgba {
   domRgbaR :: DomRgbaR, -- ^ The red component, in the [0-255] range.
   domRgbaG :: DomRgbaG, -- ^ The green component, in the [0-255] range.
   domRgbaB :: DomRgbaB, -- ^ The blue component, in the [0-255] range.
   domRgbaA :: DomRgbaA -- ^ The alpha component, in the [0-1] range (default: 1).
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRgba  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRgba where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | An array of quad vertices, x immediately followed by y for each point, points clock-wise.
type DomQuad = [Double]

-- | Box model.
data DomBoxModel = DomBoxModel {
   domBoxModelContent :: DomBoxModelContent, -- ^ Content box
   domBoxModelPadding :: DomBoxModelPadding, -- ^ Padding box
   domBoxModelBorder :: DomBoxModelBorder, -- ^ Border box
   domBoxModelMargin :: DomBoxModelMargin, -- ^ Margin box
   domBoxModelWidth :: DomBoxModelWidth, -- ^ Node width
   domBoxModelHeight :: DomBoxModelHeight, -- ^ Node height
   domBoxModelShapeOutside :: DomBoxModelShapeOutside -- ^ Shape outside coordinates
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  DomBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | CSS Shape Outside details.
data DomShapeOutsideInfo = DomShapeOutsideInfo {
   domShapeOutsideInfoBounds :: DomShapeOutsideInfoBounds, -- ^ Shape bounds
   domShapeOutsideInfoShape :: DomShapeOutsideInfoShape, -- ^ Shape coordinate details
   domShapeOutsideInfoMarginShape :: DomShapeOutsideInfoMarginShape -- ^ Margin shape bounds
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShapeOutsideInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShapeOutsideInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Rectangle.
data DomRect = DomRect {
   domRectX :: DomRectX, -- ^ X coordinate
   domRectY :: DomRectY, -- ^ Y coordinate
   domRectWidth :: DomRectWidth, -- ^ Rectangle width
   domRectHeight :: DomRectHeight -- ^ Rectangle height
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | Type 'DOM.CSSComputedStyleProperty' .
data DomCssComputedStyleProperty = DomCssComputedStyleProperty {
   domCssComputedStylePropertyName :: DomCssComputedStylePropertyName, -- ^ Computed style property name.
   domCssComputedStylePropertyValue :: DomCssComputedStylePropertyValue -- ^ Computed style property value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Type of the 'DOM.attributeModified' event.
data DomAttributeModified = DomAttributeModified {
   domAttributeModifiedNodeId :: DomAttributeModifiedNodeId, -- ^ Id of the node that has changed.
   domAttributeModifiedName :: DomAttributeModifiedName, -- ^ Attribute name.
   domAttributeModifiedValue :: DomAttributeModifiedValue -- ^ Attribute value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomAttributeModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'DOM.attributeRemoved' event.
data DomAttributeRemoved = DomAttributeRemoved {
   domAttributeRemovedNodeId :: DomAttributeRemovedNodeId, -- ^ Id of the node that has changed.
   domAttributeRemovedName :: DomAttributeRemovedName -- ^ A ttribute name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomAttributeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.characterDataModified' event.
data DomCharacterDataModified = DomCharacterDataModified {
   domCharacterDataModifiedNodeId :: DomCharacterDataModifiedNodeId, -- ^ Id of the node that has changed.
   domCharacterDataModifiedCharacterData :: DomCharacterDataModifiedCharacterData -- ^ New text value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCharacterDataModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomCharacterDataModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'DOM.childNodeCountUpdated' event.
data DomChildNodeCountUpdated = DomChildNodeCountUpdated {
   domChildNodeCountUpdatedNodeId :: DomChildNodeCountUpdatedNodeId, -- ^ Id of the node that has changed.
   domChildNodeCountUpdatedChildNodeCount :: DomChildNodeCountUpdatedChildNodeCount -- ^ New node count.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeCountUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeCountUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'DOM.childNodeInserted' event.
data DomChildNodeInserted = DomChildNodeInserted {
   domChildNodeInsertedParentNodeId :: DomChildNodeInsertedParentNodeId, -- ^ Id of the node that has changed.
   domChildNodeInsertedPreviousNodeId :: DomChildNodeInsertedPreviousNodeId, -- ^ If of the previous siblint.
   domChildNodeInsertedNode :: DomChildNodeInsertedNode -- ^ Inserted node data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeInserted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeInserted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'DOM.childNodeRemoved' event.
data DomChildNodeRemoved = DomChildNodeRemoved {
   domChildNodeRemovedParentNodeId :: DomChildNodeRemovedParentNodeId, -- ^ Parent id.
   domChildNodeRemovedNodeId :: DomChildNodeRemovedNodeId -- ^ Id of the node that has been removed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.distributedNodesUpdated' event.
data DomDistributedNodesUpdated = DomDistributedNodesUpdated {
   domDistributedNodesUpdatedInsertionPointId :: DomDistributedNodesUpdatedInsertionPointId, -- ^ Insertion point where distributed nodes were updated.
   domDistributedNodesUpdatedDistributedNodes :: DomDistributedNodesUpdatedDistributedNodes -- ^ Distributed nodes for given insertion point.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomDistributedNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomDistributedNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'DOM.documentUpdated' event.
data DomDocumentUpdated = DomDocumentUpdated
   deriving (Eq, Show, Read)
instance FromJSON DomDocumentUpdated where
   parseJSON = A.withText  "DomDocumentUpdated"  $ \v -> do
      case v of
         "DomDocumentUpdated" -> pure DomDocumentUpdated
         _ -> fail "failed to parse DomDocumentUpdated"



-- | Type of the 'DOM.inlineStyleInvalidated' event.
data DomInlineStyleInvalidated = DomInlineStyleInvalidated {
   domInlineStyleInvalidatedNodeIds :: DomInlineStyleInvalidatedNodeIds -- ^ Ids of the nodes for which the inline styles have been invalidated.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomInlineStyleInvalidated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomInlineStyleInvalidated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'DOM.pseudoElementAdded' event.
data DomPseudoElementAdded = DomPseudoElementAdded {
   domPseudoElementAddedParentId :: DomPseudoElementAddedParentId, -- ^ Pseudo element's parent element id.
   domPseudoElementAddedPseudoElement :: DomPseudoElementAddedPseudoElement -- ^ The added pseudo element.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'DOM.pseudoElementRemoved' event.
data DomPseudoElementRemoved = DomPseudoElementRemoved {
   domPseudoElementRemovedParentId :: DomPseudoElementRemovedParentId, -- ^ Pseudo element's parent element id.
   domPseudoElementRemovedPseudoElementId :: DomPseudoElementRemovedPseudoElementId -- ^ The removed pseudo element id.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'DOM.setChildNodes' event.
data DomSetChildNodes = DomSetChildNodes {
   domSetChildNodesParentId :: DomSetChildNodesParentId, -- ^ Parent node id to populate with children.
   domSetChildNodesNodes :: DomSetChildNodesNodes -- ^ Child nodes array.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DomSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type of the 'DOM.shadowRootPopped' event.
data DomShadowRootPopped = DomShadowRootPopped {
   domShadowRootPoppedHostId :: DomShadowRootPoppedHostId, -- ^ Host element id.
   domShadowRootPoppedRootId :: DomShadowRootPoppedRootId -- ^ Shadow root id.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPopped  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPopped where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.shadowRootPushed' event.
data DomShadowRootPushed = DomShadowRootPushed {
   domShadowRootPushedHostId :: DomShadowRootPushedHostId, -- ^ Host element id.
   domShadowRootPushedRoot :: DomShadowRootPushedRoot -- ^ Shadow root.
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPushed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPushed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Parameters of the 'domCollectClassNamesFromSubtree' command.
data PDomCollectClassNamesFromSubtree = PDomCollectClassNamesFromSubtree {
   pDomCollectClassNamesFromSubtreeNodeId :: PDomCollectClassNamesFromSubtreeNodeId -- ^ Id of the node to collect class names.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCollectClassNamesFromSubtree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PDomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'DOM.collectClassNamesFromSubtree'.
-- Collects class names for the node with given id and all of it's child nodes.
-- Parameters: 'PDomCollectClassNamesFromSubtree'
-- Returns: 'DomCollectClassNamesFromSubtree'
domCollectClassNamesFromSubtree :: Handle ev -> PDomCollectClassNamesFromSubtree -> IO (Either Error DomCollectClassNamesFromSubtree)
domCollectClassNamesFromSubtree handle params = sendReceiveCommandResult handle "DOM.collectClassNamesFromSubtree" (Just params)

-- | Return type of the 'domCollectClassNamesFromSubtree' command.
data DomCollectClassNamesFromSubtree = DomCollectClassNamesFromSubtree {
   domCollectClassNamesFromSubtreeClassNames :: [String] -- ^ Class name list.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command DomCollectClassNamesFromSubtree where
   commandName _ = "DOM.collectClassNamesFromSubtree"



-- | Parameters of the 'domCopyTo' command.
data PDomCopyTo = PDomCopyTo {
   pDomCopyToNodeId :: PDomCopyToNodeId, -- ^ Id of the node to copy.
   pDomCopyToTargetNodeId :: PDomCopyToTargetNodeId, -- ^ Id of the element to drop the copy into.
   pDomCopyToInsertBeforeNodeId :: PDomCopyToInsertBeforeNodeId -- ^ Drop the copy before this node (if absent, the copy becomes the last child of
`targetNodeId`).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCopyTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the command 'DOM.copyTo'.
-- Creates a deep copy of the specified node and places it into the target container before the
-- given anchor.
-- Parameters: 'PDomCopyTo'
-- Returns: 'DomCopyTo'
domCopyTo :: Handle ev -> PDomCopyTo -> IO (Either Error DomCopyTo)
domCopyTo handle params = sendReceiveCommandResult handle "DOM.copyTo" (Just params)

-- | Return type of the 'domCopyTo' command.
data DomCopyTo = DomCopyTo {
   domCopyToNodeId :: DomNodeId -- ^ Id of the node clone.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomCopyTo where
   commandName _ = "DOM.copyTo"



-- | Parameters of the 'domDescribeNode' command.
data PDomDescribeNode = PDomDescribeNode {
   pDomDescribeNodeNodeId :: PDomDescribeNodeNodeId, -- ^ Identifier of the node.
   pDomDescribeNodeBackendNodeId :: PDomDescribeNodeBackendNodeId, -- ^ Identifier of the backend node.
   pDomDescribeNodeObjectId :: PDomDescribeNodeObjectId, -- ^ JavaScript object id of the node wrapper.
   pDomDescribeNodeDepth :: PDomDescribeNodeDepth, -- ^ The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
entire subtree or provide an integer larger than 0.
   pDomDescribeNodePierce :: PDomDescribeNodePierce -- ^ Whether or not iframes and shadow roots should be traversed when returning the subtree
(default is false).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDescribeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'DOM.describeNode'.
-- Describes node given its id, does not require domain to be enabled. Does not start tracking any
-- objects, can be used for automation.
-- Parameters: 'PDomDescribeNode'
-- Returns: 'DomDescribeNode'
domDescribeNode :: Handle ev -> PDomDescribeNode -> IO (Either Error DomDescribeNode)
domDescribeNode handle params = sendReceiveCommandResult handle "DOM.describeNode" (Just params)

-- | Return type of the 'domDescribeNode' command.
data DomDescribeNode = DomDescribeNode {
   domDescribeNodeNode :: DomNode -- ^ Node description.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomDescribeNode where
   commandName _ = "DOM.describeNode"



-- | Parameters of the 'domScrollIntoViewIfNeeded' command.
data PDomScrollIntoViewIfNeeded = PDomScrollIntoViewIfNeeded {
   pDomScrollIntoViewIfNeededNodeId :: PDomScrollIntoViewIfNeededNodeId, -- ^ Identifier of the node.
   pDomScrollIntoViewIfNeededBackendNodeId :: PDomScrollIntoViewIfNeededBackendNodeId, -- ^ Identifier of the backend node.
   pDomScrollIntoViewIfNeededObjectId :: PDomScrollIntoViewIfNeededObjectId, -- ^ JavaScript object id of the node wrapper.
   pDomScrollIntoViewIfNeededRect :: PDomScrollIntoViewIfNeededRect -- ^ The rect to be scrolled into view, relative to the node's border box, in CSS pixels.
When omitted, center of the node will be used, similar to Element.scrollIntoView.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomScrollIntoViewIfNeeded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PDomScrollIntoViewIfNeeded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'DOM.scrollIntoViewIfNeeded'.
-- Scrolls the specified rect of the given node into view if not already visible.
-- Note: exactly one between nodeId, backendNodeId and objectId should be passed
-- to identify the node.
-- Parameters: 'PDomScrollIntoViewIfNeeded'
domScrollIntoViewIfNeeded :: Handle ev -> PDomScrollIntoViewIfNeeded -> IO (Maybe Error)
domScrollIntoViewIfNeeded handle params = sendReceiveCommand handle "DOM.scrollIntoViewIfNeeded" (Just params)


-- | Function for the command 'DOM.disable'.
-- Disables DOM agent for the given page.
domDisable :: Handle ev -> IO (Maybe Error)
domDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())


-- | Parameters of the 'domDiscardSearchResults' command.
data PDomDiscardSearchResults = PDomDiscardSearchResults {
   pDomDiscardSearchResultsSearchId :: PDomDiscardSearchResultsSearchId -- ^ Unique search session identifier.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDiscardSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDomDiscardSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'DOM.discardSearchResults'.
-- Discards search results from the session with the given id. `getSearchResults` should no longer
-- be called for that search.
-- Parameters: 'PDomDiscardSearchResults'
domDiscardSearchResults :: Handle ev -> PDomDiscardSearchResults -> IO (Maybe Error)
domDiscardSearchResults handle params = sendReceiveCommand handle "DOM.discardSearchResults" (Just params)


-- | Parameters of the 'domEnable' command.
data PDomEnableIncludeWhitespace = PDomEnableIncludeWhitespaceNone | PDomEnableIncludeWhitespaceAll
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDomEnableIncludeWhitespace where
   parseJSON = A.withText  "PDomEnableIncludeWhitespace"  $ \v -> do
      case v of
         "none" -> pure PDomEnableIncludeWhitespaceNone
         "all" -> pure PDomEnableIncludeWhitespaceAll
         _ -> fail "failed to parse PDomEnableIncludeWhitespace"

instance ToJSON PDomEnableIncludeWhitespace where
   toJSON v = A.String $
      case v of
         PDomEnableIncludeWhitespaceNone -> "none"
         PDomEnableIncludeWhitespaceAll -> "all"



data PDomEnable = PDomEnable {
   pDomEnableIncludeWhitespace :: PDomEnableIncludeWhitespace -- ^ Whether to include whitespaces in the children array of returned Nodes.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the command 'DOM.enable'.
-- Enables DOM agent for the given page.
-- Parameters: 'PDomEnable'
domEnable :: Handle ev -> PDomEnable -> IO (Maybe Error)
domEnable handle params = sendReceiveCommand handle "DOM.enable" (Just params)


-- | Parameters of the 'domFocus' command.
data PDomFocus = PDomFocus {
   pDomFocusNodeId :: PDomFocusNodeId, -- ^ Identifier of the node.
   pDomFocusBackendNodeId :: PDomFocusBackendNodeId, -- ^ Identifier of the backend node.
   pDomFocusObjectId :: PDomFocusObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomFocus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PDomFocus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


-- | Function for the command 'DOM.focus'.
-- Focuses the given element.
-- Parameters: 'PDomFocus'
domFocus :: Handle ev -> PDomFocus -> IO (Maybe Error)
domFocus handle params = sendReceiveCommand handle "DOM.focus" (Just params)


-- | Parameters of the 'domGetAttributes' command.
data PDomGetAttributes = PDomGetAttributes {
   pDomGetAttributesNodeId :: PDomGetAttributesNodeId -- ^ Id of the node to retrieve attibutes for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetAttributes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'DOM.getAttributes'.
-- Returns attributes for the specified node.
-- Parameters: 'PDomGetAttributes'
-- Returns: 'DomGetAttributes'
domGetAttributes :: Handle ev -> PDomGetAttributes -> IO (Either Error DomGetAttributes)
domGetAttributes handle params = sendReceiveCommandResult handle "DOM.getAttributes" (Just params)

-- | Return type of the 'domGetAttributes' command.
data DomGetAttributes = DomGetAttributes {
   domGetAttributesAttributes :: [String] -- ^ An interleaved array of node attribute names and values.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetAttributes where
   commandName _ = "DOM.getAttributes"



-- | Parameters of the 'domGetBoxModel' command.
data PDomGetBoxModel = PDomGetBoxModel {
   pDomGetBoxModelNodeId :: PDomGetBoxModelNodeId, -- ^ Identifier of the node.
   pDomGetBoxModelBackendNodeId :: PDomGetBoxModelBackendNodeId, -- ^ Identifier of the backend node.
   pDomGetBoxModelObjectId :: PDomGetBoxModelObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.getBoxModel'.
-- Returns boxes for the given node.
-- Parameters: 'PDomGetBoxModel'
-- Returns: 'DomGetBoxModel'
domGetBoxModel :: Handle ev -> PDomGetBoxModel -> IO (Either Error DomGetBoxModel)
domGetBoxModel handle params = sendReceiveCommandResult handle "DOM.getBoxModel" (Just params)

-- | Return type of the 'domGetBoxModel' command.
data DomGetBoxModel = DomGetBoxModel {
   domGetBoxModelModel :: DomBoxModel -- ^ Box model for the node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetBoxModel where
   commandName _ = "DOM.getBoxModel"



-- | Parameters of the 'domGetContentQuads' command.
data PDomGetContentQuads = PDomGetContentQuads {
   pDomGetContentQuadsNodeId :: PDomGetContentQuadsNodeId, -- ^ Identifier of the node.
   pDomGetContentQuadsBackendNodeId :: PDomGetContentQuadsBackendNodeId, -- ^ Identifier of the backend node.
   pDomGetContentQuadsObjectId :: PDomGetContentQuadsObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContentQuads  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'DOM.getContentQuads'.
-- Returns quads that describe node position on the page. This method
-- might return multiple quads for inline nodes.
-- Parameters: 'PDomGetContentQuads'
-- Returns: 'DomGetContentQuads'
domGetContentQuads :: Handle ev -> PDomGetContentQuads -> IO (Either Error DomGetContentQuads)
domGetContentQuads handle params = sendReceiveCommandResult handle "DOM.getContentQuads" (Just params)

-- | Return type of the 'domGetContentQuads' command.
data DomGetContentQuads = DomGetContentQuads {
   domGetContentQuadsQuads :: [DomQuad] -- ^ Quads that describe node layout relative to viewport.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command DomGetContentQuads where
   commandName _ = "DOM.getContentQuads"



-- | Parameters of the 'domGetDocument' command.
data PDomGetDocument = PDomGetDocument {
   pDomGetDocumentDepth :: PDomGetDocumentDepth, -- ^ The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
entire subtree or provide an integer larger than 0.
   pDomGetDocumentPierce :: PDomGetDocumentPierce -- ^ Whether or not iframes and shadow roots should be traversed when returning the subtree
(default is false).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.getDocument'.
-- Returns the root DOM node (and optionally the subtree) to the caller.
-- Parameters: 'PDomGetDocument'
-- Returns: 'DomGetDocument'
domGetDocument :: Handle ev -> PDomGetDocument -> IO (Either Error DomGetDocument)
domGetDocument handle params = sendReceiveCommandResult handle "DOM.getDocument" (Just params)

-- | Return type of the 'domGetDocument' command.
data DomGetDocument = DomGetDocument {
   domGetDocumentRoot :: DomNode -- ^ Resulting node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetDocument where
   commandName _ = "DOM.getDocument"



-- | Parameters of the 'domGetNodesForSubtreeByStyle' command.
data PDomGetNodesForSubtreeByStyle = PDomGetNodesForSubtreeByStyle {
   pDomGetNodesForSubtreeByStyleNodeId :: PDomGetNodesForSubtreeByStyleNodeId, -- ^ Node ID pointing to the root of a subtree.
   pDomGetNodesForSubtreeByStyleComputedStyles :: PDomGetNodesForSubtreeByStyleComputedStyles, -- ^ The style to filter nodes by (includes nodes if any of properties matches).
   pDomGetNodesForSubtreeByStylePierce :: PDomGetNodesForSubtreeByStylePierce -- ^ Whether or not iframes and shadow roots in the same target should be traversed when returning the
results (default is false).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodesForSubtreeByStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'DOM.getNodesForSubtreeByStyle'.
-- Finds nodes with a given computed style in a subtree.
-- Parameters: 'PDomGetNodesForSubtreeByStyle'
-- Returns: 'DomGetNodesForSubtreeByStyle'
domGetNodesForSubtreeByStyle :: Handle ev -> PDomGetNodesForSubtreeByStyle -> IO (Either Error DomGetNodesForSubtreeByStyle)
domGetNodesForSubtreeByStyle handle params = sendReceiveCommandResult handle "DOM.getNodesForSubtreeByStyle" (Just params)

-- | Return type of the 'domGetNodesForSubtreeByStyle' command.
data DomGetNodesForSubtreeByStyle = DomGetNodesForSubtreeByStyle {
   domGetNodesForSubtreeByStyleNodeIds :: [DomNodeId] -- ^ Resulting nodes.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomGetNodesForSubtreeByStyle where
   commandName _ = "DOM.getNodesForSubtreeByStyle"



-- | Parameters of the 'domGetNodeForLocation' command.
data PDomGetNodeForLocation = PDomGetNodeForLocation {
   pDomGetNodeForLocationX :: PDomGetNodeForLocationX, -- ^ X coordinate.
   pDomGetNodeForLocationY :: PDomGetNodeForLocationY, -- ^ Y coordinate.
   pDomGetNodeForLocationIncludeUserAgentShadowDom :: PDomGetNodeForLocationIncludeUserAgentShadowDom, -- ^ False to skip to the nearest non-UA shadow root ancestor (default: false).
   pDomGetNodeForLocationIgnorePointerEventsNone :: PDomGetNodeForLocationIgnorePointerEventsNone -- ^ Whether to ignore pointer-events: none on elements and hit test them.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeForLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'DOM.getNodeForLocation'.
-- Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
-- either returned or not.
-- Parameters: 'PDomGetNodeForLocation'
-- Returns: 'DomGetNodeForLocation'
domGetNodeForLocation :: Handle ev -> PDomGetNodeForLocation -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation handle params = sendReceiveCommandResult handle "DOM.getNodeForLocation" (Just params)

-- | Return type of the 'domGetNodeForLocation' command.
data DomGetNodeForLocation = DomGetNodeForLocation {
   domGetNodeForLocationBackendNodeId :: DomBackendNodeId, -- ^ Resulting node.
   domGetNodeForLocationFrameId :: PageFrameId, -- ^ Frame this node belongs to.
   domGetNodeForLocationNodeId :: Maybe DomNodeId -- ^ Id of the node at given coordinates, only when enabled and requested document.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeForLocation where
   commandName _ = "DOM.getNodeForLocation"



-- | Parameters of the 'domGetOuterHtml' command.
data PDomGetOuterHtml = PDomGetOuterHtml {
   pDomGetOuterHtmlNodeId :: PDomGetOuterHtmlNodeId, -- ^ Identifier of the node.
   pDomGetOuterHtmlBackendNodeId :: PDomGetOuterHtmlBackendNodeId, -- ^ Identifier of the backend node.
   pDomGetOuterHtmlObjectId :: PDomGetOuterHtmlObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'DOM.getOuterHTML'.
-- Returns node's HTML markup.
-- Parameters: 'PDomGetOuterHtml'
-- Returns: 'DomGetOuterHtml'
domGetOuterHtml :: Handle ev -> PDomGetOuterHtml -> IO (Either Error DomGetOuterHtml)
domGetOuterHtml handle params = sendReceiveCommandResult handle "DOM.getOuterHTML" (Just params)

-- | Return type of the 'domGetOuterHtml' command.
data DomGetOuterHtml = DomGetOuterHtml {
   domGetOuterHtmlOuterHtml :: String -- ^ Outer HTML markup.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomGetOuterHtml where
   commandName _ = "DOM.getOuterHTML"



-- | Parameters of the 'domGetRelayoutBoundary' command.
data PDomGetRelayoutBoundary = PDomGetRelayoutBoundary {
   pDomGetRelayoutBoundaryNodeId :: PDomGetRelayoutBoundaryNodeId -- ^ Id of the node.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetRelayoutBoundary  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'DOM.getRelayoutBoundary'.
-- Returns the id of the nearest ancestor that is a relayout boundary.
-- Parameters: 'PDomGetRelayoutBoundary'
-- Returns: 'DomGetRelayoutBoundary'
domGetRelayoutBoundary :: Handle ev -> PDomGetRelayoutBoundary -> IO (Either Error DomGetRelayoutBoundary)
domGetRelayoutBoundary handle params = sendReceiveCommandResult handle "DOM.getRelayoutBoundary" (Just params)

-- | Return type of the 'domGetRelayoutBoundary' command.
data DomGetRelayoutBoundary = DomGetRelayoutBoundary {
   domGetRelayoutBoundaryNodeId :: DomNodeId -- ^ Relayout boundary node id for the given node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetRelayoutBoundary where
   commandName _ = "DOM.getRelayoutBoundary"



-- | Parameters of the 'domGetSearchResults' command.
data PDomGetSearchResults = PDomGetSearchResults {
   pDomGetSearchResultsSearchId :: PDomGetSearchResultsSearchId, -- ^ Unique search session identifier.
   pDomGetSearchResultsFromIndex :: PDomGetSearchResultsFromIndex, -- ^ Start index of the search result to be returned.
   pDomGetSearchResultsToIndex :: PDomGetSearchResultsToIndex -- ^ End index of the search result to be returned.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'DOM.getSearchResults'.
-- Returns search results from given `fromIndex` to given `toIndex` from the search with the given
-- identifier.
-- Parameters: 'PDomGetSearchResults'
-- Returns: 'DomGetSearchResults'
domGetSearchResults :: Handle ev -> PDomGetSearchResults -> IO (Either Error DomGetSearchResults)
domGetSearchResults handle params = sendReceiveCommandResult handle "DOM.getSearchResults" (Just params)

-- | Return type of the 'domGetSearchResults' command.
data DomGetSearchResults = DomGetSearchResults {
   domGetSearchResultsNodeIds :: [DomNodeId] -- ^ Ids of the search result nodes.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomGetSearchResults where
   commandName _ = "DOM.getSearchResults"



-- | Function for the command 'DOM.hideHighlight'.
-- Hides any highlight.
domHideHighlight :: Handle ev -> IO (Maybe Error)
domHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())


-- | Function for the command 'DOM.highlightNode'.
-- Highlights DOM node.
domHighlightNode :: Handle ev -> IO (Maybe Error)
domHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())


-- | Function for the command 'DOM.highlightRect'.
-- Highlights given rectangle.
domHighlightRect :: Handle ev -> IO (Maybe Error)
domHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())


-- | Function for the command 'DOM.markUndoableState'.
-- Marks last undoable state.
domMarkUndoableState :: Handle ev -> IO (Maybe Error)
domMarkUndoableState handle = sendReceiveCommand handle "DOM.markUndoableState" (Nothing :: Maybe ())


-- | Parameters of the 'domMoveTo' command.
data PDomMoveTo = PDomMoveTo {
   pDomMoveToNodeId :: PDomMoveToNodeId, -- ^ Id of the node to move.
   pDomMoveToTargetNodeId :: PDomMoveToTargetNodeId, -- ^ Id of the element to drop the moved node into.
   pDomMoveToInsertBeforeNodeId :: PDomMoveToInsertBeforeNodeId -- ^ Drop node before this one (if absent, the moved node becomes the last child of
`targetNodeId`).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomMoveTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the command 'DOM.moveTo'.
-- Moves node into the new container, places it before the given anchor.
-- Parameters: 'PDomMoveTo'
-- Returns: 'DomMoveTo'
domMoveTo :: Handle ev -> PDomMoveTo -> IO (Either Error DomMoveTo)
domMoveTo handle params = sendReceiveCommandResult handle "DOM.moveTo" (Just params)

-- | Return type of the 'domMoveTo' command.
data DomMoveTo = DomMoveTo {
   domMoveToNodeId :: DomNodeId -- ^ New id of the moved node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomMoveTo where
   commandName _ = "DOM.moveTo"



-- | Parameters of the 'domPerformSearch' command.
data PDomPerformSearch = PDomPerformSearch {
   pDomPerformSearchQuery :: PDomPerformSearchQuery, -- ^ Plain text or query selector or XPath search query.
   pDomPerformSearchIncludeUserAgentShadowDom :: PDomPerformSearchIncludeUserAgentShadowDom -- ^ True to search in user agent shadow DOM.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPerformSearch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'DOM.performSearch'.
-- Searches for a given string in the DOM tree. Use `getSearchResults` to access search results or
-- `cancelSearch` to end this search session.
-- Parameters: 'PDomPerformSearch'
-- Returns: 'DomPerformSearch'
domPerformSearch :: Handle ev -> PDomPerformSearch -> IO (Either Error DomPerformSearch)
domPerformSearch handle params = sendReceiveCommandResult handle "DOM.performSearch" (Just params)

-- | Return type of the 'domPerformSearch' command.
data DomPerformSearch = DomPerformSearch {
   domPerformSearchSearchId :: String, -- ^ Unique search session identifier.
   domPerformSearchResultCount :: Int -- ^ Number of search results.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomPerformSearch where
   commandName _ = "DOM.performSearch"



-- | Parameters of the 'domPushNodeByPathToFrontend' command.
data PDomPushNodeByPathToFrontend = PDomPushNodeByPathToFrontend {
   pDomPushNodeByPathToFrontendPath :: PDomPushNodeByPathToFrontendPath -- ^ Path to node in the proprietary format.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodeByPathToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'DOM.pushNodeByPathToFrontend'.
-- Requests that the node is sent to the caller given its path. // FIXME, use XPath
-- Parameters: 'PDomPushNodeByPathToFrontend'
-- Returns: 'DomPushNodeByPathToFrontend'
domPushNodeByPathToFrontend :: Handle ev -> PDomPushNodeByPathToFrontend -> IO (Either Error DomPushNodeByPathToFrontend)
domPushNodeByPathToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodeByPathToFrontend" (Just params)

-- | Return type of the 'domPushNodeByPathToFrontend' command.
data DomPushNodeByPathToFrontend = DomPushNodeByPathToFrontend {
   domPushNodeByPathToFrontendNodeId :: DomNodeId -- ^ Id of the node for given path.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DomPushNodeByPathToFrontend where
   commandName _ = "DOM.pushNodeByPathToFrontend"



-- | Parameters of the 'domPushNodesByBackendIdsToFrontend' command.
data PDomPushNodesByBackendIdsToFrontend = PDomPushNodesByBackendIdsToFrontend {
   pDomPushNodesByBackendIdsToFrontendBackendNodeIds :: PDomPushNodesByBackendIdsToFrontendBackendNodeIds -- ^ The array of backend node ids.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodesByBackendIdsToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'DOM.pushNodesByBackendIdsToFrontend'.
-- Requests that a batch of nodes is sent to the caller given their backend node ids.
-- Parameters: 'PDomPushNodesByBackendIdsToFrontend'
-- Returns: 'DomPushNodesByBackendIdsToFrontend'
domPushNodesByBackendIdsToFrontend :: Handle ev -> PDomPushNodesByBackendIdsToFrontend -> IO (Either Error DomPushNodesByBackendIdsToFrontend)
domPushNodesByBackendIdsToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodesByBackendIdsToFrontend" (Just params)

-- | Return type of the 'domPushNodesByBackendIdsToFrontend' command.
data DomPushNodesByBackendIdsToFrontend = DomPushNodesByBackendIdsToFrontend {
   domPushNodesByBackendIdsToFrontendNodeIds :: [DomNodeId] -- ^ The array of ids of pushed nodes that correspond to the backend ids specified in
backendNodeIds.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command DomPushNodesByBackendIdsToFrontend where
   commandName _ = "DOM.pushNodesByBackendIdsToFrontend"



-- | Parameters of the 'domQuerySelector' command.
data PDomQuerySelector = PDomQuerySelector {
   pDomQuerySelectorNodeId :: PDomQuerySelectorNodeId, -- ^ Id of the node to query upon.
   pDomQuerySelectorSelector :: PDomQuerySelectorSelector -- ^ Selector string.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'DOM.querySelector'.
-- Executes `querySelector` on a given node.
-- Parameters: 'PDomQuerySelector'
-- Returns: 'DomQuerySelector'
domQuerySelector :: Handle ev -> PDomQuerySelector -> IO (Either Error DomQuerySelector)
domQuerySelector handle params = sendReceiveCommandResult handle "DOM.querySelector" (Just params)

-- | Return type of the 'domQuerySelector' command.
data DomQuerySelector = DomQuerySelector {
   domQuerySelectorNodeId :: DomNodeId -- ^ Query selector result.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomQuerySelector where
   commandName _ = "DOM.querySelector"



-- | Parameters of the 'domQuerySelectorAll' command.
data PDomQuerySelectorAll = PDomQuerySelectorAll {
   pDomQuerySelectorAllNodeId :: PDomQuerySelectorAllNodeId, -- ^ Id of the node to query upon.
   pDomQuerySelectorAllSelector :: PDomQuerySelectorAllSelector -- ^ Selector string.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelectorAll  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'DOM.querySelectorAll'.
-- Executes `querySelectorAll` on a given node.
-- Parameters: 'PDomQuerySelectorAll'
-- Returns: 'DomQuerySelectorAll'
domQuerySelectorAll :: Handle ev -> PDomQuerySelectorAll -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll handle params = sendReceiveCommandResult handle "DOM.querySelectorAll" (Just params)

-- | Return type of the 'domQuerySelectorAll' command.
data DomQuerySelectorAll = DomQuerySelectorAll {
   domQuerySelectorAllNodeIds :: [DomNodeId] -- ^ Query selector result.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomQuerySelectorAll where
   commandName _ = "DOM.querySelectorAll"



-- | Function for the command 'DOM.redo'.
-- Re-does the last undone action.
domRedo :: Handle ev -> IO (Maybe Error)
domRedo handle = sendReceiveCommand handle "DOM.redo" (Nothing :: Maybe ())


-- | Parameters of the 'domRemoveAttribute' command.
data PDomRemoveAttribute = PDomRemoveAttribute {
   pDomRemoveAttributeNodeId :: PDomRemoveAttributeNodeId, -- ^ Id of the element to remove attribute from.
   pDomRemoveAttributeName :: PDomRemoveAttributeName -- ^ Name of the attribute to remove.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveAttribute  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveAttribute where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'DOM.removeAttribute'.
-- Removes attribute with given name from an element with given id.
-- Parameters: 'PDomRemoveAttribute'
domRemoveAttribute :: Handle ev -> PDomRemoveAttribute -> IO (Maybe Error)
domRemoveAttribute handle params = sendReceiveCommand handle "DOM.removeAttribute" (Just params)


-- | Parameters of the 'domRemoveNode' command.
data PDomRemoveNode = PDomRemoveNode {
   pDomRemoveNodeNodeId :: PDomRemoveNodeNodeId -- ^ Id of the node to remove.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the command 'DOM.removeNode'.
-- Removes node with given id.
-- Parameters: 'PDomRemoveNode'
domRemoveNode :: Handle ev -> PDomRemoveNode -> IO (Maybe Error)
domRemoveNode handle params = sendReceiveCommand handle "DOM.removeNode" (Just params)


-- | Parameters of the 'domRequestChildNodes' command.
data PDomRequestChildNodes = PDomRequestChildNodes {
   pDomRequestChildNodesNodeId :: PDomRequestChildNodesNodeId, -- ^ Id of the node to get children for.
   pDomRequestChildNodesDepth :: PDomRequestChildNodesDepth, -- ^ The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
entire subtree or provide an integer larger than 0.
   pDomRequestChildNodesPierce :: PDomRequestChildNodesPierce -- ^ Whether or not iframes and shadow roots should be traversed when returning the sub-tree
(default is false).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomRequestChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'DOM.requestChildNodes'.
-- Requests that children of the node with given id are returned to the caller in form of
-- `setChildNodes` events where not only immediate children are retrieved, but all children down to
-- the specified depth.
-- Parameters: 'PDomRequestChildNodes'
domRequestChildNodes :: Handle ev -> PDomRequestChildNodes -> IO (Maybe Error)
domRequestChildNodes handle params = sendReceiveCommand handle "DOM.requestChildNodes" (Just params)


-- | Parameters of the 'domRequestNode' command.
data PDomRequestNode = PDomRequestNode {
   pDomRequestNodeObjectId :: PDomRequestNodeObjectId -- ^ JavaScript object id to convert into node.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.requestNode'.
-- Requests that the node is sent to the caller given the JavaScript node object reference. All
-- nodes that form the path from the node to the root are also sent to the client as a series of
-- `setChildNodes` notifications.
-- Parameters: 'PDomRequestNode'
-- Returns: 'DomRequestNode'
domRequestNode :: Handle ev -> PDomRequestNode -> IO (Either Error DomRequestNode)
domRequestNode handle params = sendReceiveCommandResult handle "DOM.requestNode" (Just params)

-- | Return type of the 'domRequestNode' command.
data DomRequestNode = DomRequestNode {
   domRequestNodeNodeId :: DomNodeId -- ^ Node id for given object.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomRequestNode where
   commandName _ = "DOM.requestNode"



-- | Parameters of the 'domResolveNode' command.
data PDomResolveNode = PDomResolveNode {
   pDomResolveNodeNodeId :: PDomResolveNodeNodeId, -- ^ Id of the node to resolve.
   pDomResolveNodeBackendNodeId :: PDomResolveNodeBackendNodeId, -- ^ Backend identifier of the node to resolve.
   pDomResolveNodeObjectGroup :: PDomResolveNodeObjectGroup, -- ^ Symbolic group name that can be used to release multiple objects.
   pDomResolveNodeExecutionContextId :: PDomResolveNodeExecutionContextId -- ^ Execution context in which to resolve the node.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomResolveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.resolveNode'.
-- Resolves the JavaScript node object for a given NodeId or BackendNodeId.
-- Parameters: 'PDomResolveNode'
-- Returns: 'DomResolveNode'
domResolveNode :: Handle ev -> PDomResolveNode -> IO (Either Error DomResolveNode)
domResolveNode handle params = sendReceiveCommandResult handle "DOM.resolveNode" (Just params)

-- | Return type of the 'domResolveNode' command.
data DomResolveNode = DomResolveNode {
   domResolveNodeObject :: Runtime.RuntimeRemoteObject -- ^ JavaScript object wrapper for given node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomResolveNode where
   commandName _ = "DOM.resolveNode"



-- | Parameters of the 'domSetAttributeValue' command.
data PDomSetAttributeValue = PDomSetAttributeValue {
   pDomSetAttributeValueNodeId :: PDomSetAttributeValueNodeId, -- ^ Id of the element to set attribute for.
   pDomSetAttributeValueName :: PDomSetAttributeValueName, -- ^ Attribute name.
   pDomSetAttributeValueValue :: PDomSetAttributeValueValue -- ^ Attribute value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'DOM.setAttributeValue'.
-- Sets attribute for an element with given id.
-- Parameters: 'PDomSetAttributeValue'
domSetAttributeValue :: Handle ev -> PDomSetAttributeValue -> IO (Maybe Error)
domSetAttributeValue handle params = sendReceiveCommand handle "DOM.setAttributeValue" (Just params)


-- | Parameters of the 'domSetAttributesAsText' command.
data PDomSetAttributesAsText = PDomSetAttributesAsText {
   pDomSetAttributesAsTextNodeId :: PDomSetAttributesAsTextNodeId, -- ^ Id of the element to set attributes for.
   pDomSetAttributesAsTextText :: PDomSetAttributesAsTextText, -- ^ Text with a number of attributes. Will parse this text using HTML parser.
   pDomSetAttributesAsTextName :: PDomSetAttributesAsTextName -- ^ Attribute name to replace with new attributes derived from text in case text parsed
successfully.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributesAsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributesAsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'DOM.setAttributesAsText'.
-- Sets attributes on element with given id. This method is useful when user edits some existing
-- attribute value and types in several attribute name/value pairs.
-- Parameters: 'PDomSetAttributesAsText'
domSetAttributesAsText :: Handle ev -> PDomSetAttributesAsText -> IO (Maybe Error)
domSetAttributesAsText handle params = sendReceiveCommand handle "DOM.setAttributesAsText" (Just params)


-- | Parameters of the 'domSetFileInputFiles' command.
data PDomSetFileInputFiles = PDomSetFileInputFiles {
   pDomSetFileInputFilesFiles :: PDomSetFileInputFilesFiles, -- ^ Array of file paths to set.
   pDomSetFileInputFilesNodeId :: PDomSetFileInputFilesNodeId, -- ^ Identifier of the node.
   pDomSetFileInputFilesBackendNodeId :: PDomSetFileInputFilesBackendNodeId, -- ^ Identifier of the backend node.
   pDomSetFileInputFilesObjectId :: PDomSetFileInputFilesObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetFileInputFiles  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetFileInputFiles where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'DOM.setFileInputFiles'.
-- Sets files for the given file input element.
-- Parameters: 'PDomSetFileInputFiles'
domSetFileInputFiles :: Handle ev -> PDomSetFileInputFiles -> IO (Maybe Error)
domSetFileInputFiles handle params = sendReceiveCommand handle "DOM.setFileInputFiles" (Just params)


-- | Parameters of the 'domSetNodeStackTracesEnabled' command.
data PDomSetNodeStackTracesEnabled = PDomSetNodeStackTracesEnabled {
   pDomSetNodeStackTracesEnabledEnable :: PDomSetNodeStackTracesEnabledEnable -- ^ Enable or disable.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeStackTracesEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeStackTracesEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'DOM.setNodeStackTracesEnabled'.
-- Sets if stack traces should be captured for Nodes. See `Node.getNodeStackTraces`. Default is disabled.
-- Parameters: 'PDomSetNodeStackTracesEnabled'
domSetNodeStackTracesEnabled :: Handle ev -> PDomSetNodeStackTracesEnabled -> IO (Maybe Error)
domSetNodeStackTracesEnabled handle params = sendReceiveCommand handle "DOM.setNodeStackTracesEnabled" (Just params)


-- | Parameters of the 'domGetNodeStackTraces' command.
data PDomGetNodeStackTraces = PDomGetNodeStackTraces {
   pDomGetNodeStackTracesNodeId :: PDomGetNodeStackTracesNodeId -- ^ Id of the node to get stack traces for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeStackTraces  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'DOM.getNodeStackTraces'.
-- Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.
-- Parameters: 'PDomGetNodeStackTraces'
-- Returns: 'DomGetNodeStackTraces'
domGetNodeStackTraces :: Handle ev -> PDomGetNodeStackTraces -> IO (Either Error DomGetNodeStackTraces)
domGetNodeStackTraces handle params = sendReceiveCommandResult handle "DOM.getNodeStackTraces" (Just params)

-- | Return type of the 'domGetNodeStackTraces' command.
data DomGetNodeStackTraces = DomGetNodeStackTraces {
   domGetNodeStackTracesCreation :: Maybe Runtime.RuntimeStackTrace -- ^ Creation stack trace, if available.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeStackTraces where
   commandName _ = "DOM.getNodeStackTraces"



-- | Parameters of the 'domGetFileInfo' command.
data PDomGetFileInfo = PDomGetFileInfo {
   pDomGetFileInfoObjectId :: PDomGetFileInfoObjectId -- ^ JavaScript object id of the node wrapper.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFileInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.getFileInfo'.
-- Returns file information for the given
-- File wrapper.
-- Parameters: 'PDomGetFileInfo'
-- Returns: 'DomGetFileInfo'
domGetFileInfo :: Handle ev -> PDomGetFileInfo -> IO (Either Error DomGetFileInfo)
domGetFileInfo handle params = sendReceiveCommandResult handle "DOM.getFileInfo" (Just params)

-- | Return type of the 'domGetFileInfo' command.
data DomGetFileInfo = DomGetFileInfo {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetFileInfo where
   commandName _ = "DOM.getFileInfo"



-- | Parameters of the 'domSetInspectedNode' command.
data PDomSetInspectedNode = PDomSetInspectedNode {
   pDomSetInspectedNodeNodeId :: PDomSetInspectedNodeNodeId -- ^ DOM node id to be accessible by means of $x command line API.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetInspectedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomSetInspectedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'DOM.setInspectedNode'.
-- Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
-- Parameters: 'PDomSetInspectedNode'
domSetInspectedNode :: Handle ev -> PDomSetInspectedNode -> IO (Maybe Error)
domSetInspectedNode handle params = sendReceiveCommand handle "DOM.setInspectedNode" (Just params)


-- | Parameters of the 'domSetNodeName' command.
data PDomSetNodeName = PDomSetNodeName {
   pDomSetNodeNameNodeId :: PDomSetNodeNameNodeId, -- ^ Id of the node to set name for.
   pDomSetNodeNameName :: PDomSetNodeNameName -- ^ New node's name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeName  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'DOM.setNodeName'.
-- Sets node name for a node with given id.
-- Parameters: 'PDomSetNodeName'
-- Returns: 'DomSetNodeName'
domSetNodeName :: Handle ev -> PDomSetNodeName -> IO (Either Error DomSetNodeName)
domSetNodeName handle params = sendReceiveCommandResult handle "DOM.setNodeName" (Just params)

-- | Return type of the 'domSetNodeName' command.
data DomSetNodeName = DomSetNodeName {
   domSetNodeNameNodeId :: DomNodeId -- ^ New node's id.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomSetNodeName where
   commandName _ = "DOM.setNodeName"



-- | Parameters of the 'domSetNodeValue' command.
data PDomSetNodeValue = PDomSetNodeValue {
   pDomSetNodeValueNodeId :: PDomSetNodeValueNodeId, -- ^ Id of the node to set value for.
   pDomSetNodeValueValue :: PDomSetNodeValueValue -- ^ New node's value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'DOM.setNodeValue'.
-- Sets node value for a node with given id.
-- Parameters: 'PDomSetNodeValue'
domSetNodeValue :: Handle ev -> PDomSetNodeValue -> IO (Maybe Error)
domSetNodeValue handle params = sendReceiveCommand handle "DOM.setNodeValue" (Just params)


-- | Parameters of the 'domSetOuterHtml' command.
data PDomSetOuterHtml = PDomSetOuterHtml {
   pDomSetOuterHtmlNodeId :: PDomSetOuterHtmlNodeId, -- ^ Id of the node to set markup for.
   pDomSetOuterHtmlOuterHtml :: PDomSetOuterHtmlOuterHtml -- ^ Outer HTML markup to set.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'DOM.setOuterHTML'.
-- Sets node HTML markup, returns new node id.
-- Parameters: 'PDomSetOuterHtml'
domSetOuterHtml :: Handle ev -> PDomSetOuterHtml -> IO (Maybe Error)
domSetOuterHtml handle params = sendReceiveCommand handle "DOM.setOuterHTML" (Just params)


-- | Function for the command 'DOM.undo'.
-- Undoes the last performed action.
domUndo :: Handle ev -> IO (Maybe Error)
domUndo handle = sendReceiveCommand handle "DOM.undo" (Nothing :: Maybe ())


-- | Parameters of the 'domGetFrameOwner' command.
data PDomGetFrameOwner = PDomGetFrameOwner {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFrameOwner  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'DOM.getFrameOwner'.
-- Returns iframe node that owns iframe with the given domain.
-- Parameters: 'PDomGetFrameOwner'
-- Returns: 'DomGetFrameOwner'
domGetFrameOwner :: Handle ev -> PDomGetFrameOwner -> IO (Either Error DomGetFrameOwner)
domGetFrameOwner handle params = sendReceiveCommandResult handle "DOM.getFrameOwner" (Just params)

-- | Return type of the 'domGetFrameOwner' command.
data DomGetFrameOwner = DomGetFrameOwner {
   domGetFrameOwnerBackendNodeId :: DomBackendNodeId, -- ^ Resulting node.
   domGetFrameOwnerNodeId :: Maybe DomNodeId -- ^ Id of the node at given coordinates, only when enabled and requested document.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetFrameOwner where
   commandName _ = "DOM.getFrameOwner"



-- | Parameters of the 'domGetContainerForNode' command.
data PDomGetContainerForNode = PDomGetContainerForNode {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContainerForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'DOM.getContainerForNode'.
-- Returns the container of the given node based on container query conditions.
-- If containerName is given, it will find the nearest container with a matching name;
-- otherwise it will find the nearest container regardless of its container name.
-- Parameters: 'PDomGetContainerForNode'
-- Returns: 'DomGetContainerForNode'
domGetContainerForNode :: Handle ev -> PDomGetContainerForNode -> IO (Either Error DomGetContainerForNode)
domGetContainerForNode handle params = sendReceiveCommandResult handle "DOM.getContainerForNode" (Just params)

-- | Return type of the 'domGetContainerForNode' command.
data DomGetContainerForNode = DomGetContainerForNode {
   domGetContainerForNodeNodeId :: Maybe DomNodeId -- ^ The container node for the given node, or null if not found.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetContainerForNode where
   commandName _ = "DOM.getContainerForNode"



-- | Parameters of the 'domGetQueryingDescendantsForContainer' command.
data PDomGetQueryingDescendantsForContainer = PDomGetQueryingDescendantsForContainer {
   pDomGetQueryingDescendantsForContainerNodeId :: PDomGetQueryingDescendantsForContainerNodeId -- ^ Id of the container node to find querying descendants from.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetQueryingDescendantsForContainer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the command 'DOM.getQueryingDescendantsForContainer'.
-- Returns the descendants of a container query container that have
-- container queries against this container.
-- Parameters: 'PDomGetQueryingDescendantsForContainer'
-- Returns: 'DomGetQueryingDescendantsForContainer'
domGetQueryingDescendantsForContainer :: Handle ev -> PDomGetQueryingDescendantsForContainer -> IO (Either Error DomGetQueryingDescendantsForContainer)
domGetQueryingDescendantsForContainer handle params = sendReceiveCommandResult handle "DOM.getQueryingDescendantsForContainer" (Just params)

-- | Return type of the 'domGetQueryingDescendantsForContainer' command.
data DomGetQueryingDescendantsForContainer = DomGetQueryingDescendantsForContainer {
   domGetQueryingDescendantsForContainerNodeIds :: [DomNodeId] -- ^ Descendant nodes with container queries against the given container.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command DomGetQueryingDescendantsForContainer where
   commandName _ = "DOM.getQueryingDescendantsForContainer"




-- | Screen orientation.
data EmulationScreenOrientationType = EmulationScreenOrientationTypePortraitPrimary | EmulationScreenOrientationTypePortraitSecondary | EmulationScreenOrientationTypeLandscapePrimary | EmulationScreenOrientationTypeLandscapeSecondary
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationScreenOrientationType where
   parseJSON = A.withText  "EmulationScreenOrientationType"  $ \v -> do
      case v of
         "portraitPrimary" -> pure EmulationScreenOrientationTypePortraitPrimary
         "portraitSecondary" -> pure EmulationScreenOrientationTypePortraitSecondary
         "landscapePrimary" -> pure EmulationScreenOrientationTypeLandscapePrimary
         "landscapeSecondary" -> pure EmulationScreenOrientationTypeLandscapeSecondary
         _ -> fail "failed to parse EmulationScreenOrientationType"

instance ToJSON EmulationScreenOrientationType where
   toJSON v = A.String $
      case v of
         EmulationScreenOrientationTypePortraitPrimary -> "portraitPrimary"
         EmulationScreenOrientationTypePortraitSecondary -> "portraitSecondary"
         EmulationScreenOrientationTypeLandscapePrimary -> "landscapePrimary"
         EmulationScreenOrientationTypeLandscapeSecondary -> "landscapeSecondary"



data EmulationScreenOrientation = EmulationScreenOrientation {
   emulationScreenOrientationType :: EmulationScreenOrientationType, -- ^ Orientation type.
   emulationScreenOrientationAngle :: EmulationScreenOrientationAngle -- ^ Orientation angle.
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationScreenOrientation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  EmulationScreenOrientation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Emulation.DisplayFeature' .
data EmulationDisplayFeatureOrientation = EmulationDisplayFeatureOrientationVertical | EmulationDisplayFeatureOrientationHorizontal
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisplayFeatureOrientation where
   parseJSON = A.withText  "EmulationDisplayFeatureOrientation"  $ \v -> do
      case v of
         "vertical" -> pure EmulationDisplayFeatureOrientationVertical
         "horizontal" -> pure EmulationDisplayFeatureOrientationHorizontal
         _ -> fail "failed to parse EmulationDisplayFeatureOrientation"

instance ToJSON EmulationDisplayFeatureOrientation where
   toJSON v = A.String $
      case v of
         EmulationDisplayFeatureOrientationVertical -> "vertical"
         EmulationDisplayFeatureOrientationHorizontal -> "horizontal"



data EmulationDisplayFeature = EmulationDisplayFeature {
   emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation, -- ^ Orientation of a display feature in relation to screen
   emulationDisplayFeatureOffset :: EmulationDisplayFeatureOffset, -- ^ The offset from the screen origin in either the x (for vertical
orientation) or y (for horizontal orientation) direction.
   emulationDisplayFeatureMaskLength :: EmulationDisplayFeatureMaskLength -- ^ A display feature may mask content such that it is not physically
displayed - this length along with the offset describes this area.
A display feature that only splits content will have a 0 mask_length.
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Emulation.MediaFeature' .
data EmulationMediaFeature = EmulationMediaFeature {


} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationMediaFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  EmulationMediaFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | advance: If the scheduler runs out of immediate work, the virtual time base may fast forward to
-- allow the next delayed task (if any) to run; pause: The virtual time base may not advance;
-- pauseIfNetworkFetchesPending: The virtual time base may not advance if there are any pending
-- resource fetches.
data EmulationVirtualTimePolicy = EmulationVirtualTimePolicyAdvance | EmulationVirtualTimePolicyPause | EmulationVirtualTimePolicyPauseIfNetworkFetchesPending
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationVirtualTimePolicy where
   parseJSON = A.withText  "EmulationVirtualTimePolicy"  $ \v -> do
      case v of
         "advance" -> pure EmulationVirtualTimePolicyAdvance
         "pause" -> pure EmulationVirtualTimePolicyPause
         "pauseIfNetworkFetchesPending" -> pure EmulationVirtualTimePolicyPauseIfNetworkFetchesPending
         _ -> fail "failed to parse EmulationVirtualTimePolicy"

instance ToJSON EmulationVirtualTimePolicy where
   toJSON v = A.String $
      case v of
         EmulationVirtualTimePolicyAdvance -> "advance"
         EmulationVirtualTimePolicyPause -> "pause"
         EmulationVirtualTimePolicyPauseIfNetworkFetchesPending -> "pauseIfNetworkFetchesPending"



-- | Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
data EmulationUserAgentBrandVersion = EmulationUserAgentBrandVersion {


} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationUserAgentBrandVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  EmulationUserAgentBrandVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
-- Missing optional values will be filled in by the target with what it would normally use.
data EmulationUserAgentMetadata = EmulationUserAgentMetadata {









} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationUserAgentMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  EmulationUserAgentMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Enum of image types that can be disabled.
data EmulationDisabledImageType = EmulationDisabledImageTypeAvif | EmulationDisabledImageTypeJxl | EmulationDisabledImageTypeWebp
   deriving (Ord, Eq, Show, Read)
instance FromJSON EmulationDisabledImageType where
   parseJSON = A.withText  "EmulationDisabledImageType"  $ \v -> do
      case v of
         "avif" -> pure EmulationDisabledImageTypeAvif
         "jxl" -> pure EmulationDisabledImageTypeJxl
         "webp" -> pure EmulationDisabledImageTypeWebp
         _ -> fail "failed to parse EmulationDisabledImageType"

instance ToJSON EmulationDisabledImageType where
   toJSON v = A.String $
      case v of
         EmulationDisabledImageTypeAvif -> "avif"
         EmulationDisabledImageTypeJxl -> "jxl"
         EmulationDisabledImageTypeWebp -> "webp"





-- | Type of the 'Emulation.virtualTimeBudgetExpired' event.
data EmulationVirtualTimeBudgetExpired = EmulationVirtualTimeBudgetExpired
   deriving (Eq, Show, Read)
instance FromJSON EmulationVirtualTimeBudgetExpired where
   parseJSON = A.withText  "EmulationVirtualTimeBudgetExpired"  $ \v -> do
      case v of
         "EmulationVirtualTimeBudgetExpired" -> pure EmulationVirtualTimeBudgetExpired
         _ -> fail "failed to parse EmulationVirtualTimeBudgetExpired"





-- | Function for the command 'Emulation.canEmulate'.
-- Tells whether emulation is supported.
-- Returns: 'EmulationCanEmulate'
emulationCanEmulate :: Handle ev -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())

-- | Return type of the 'emulationCanEmulate' command.
data EmulationCanEmulate = EmulationCanEmulate {
   emulationCanEmulateResult :: Bool -- ^ True if emulation is supported.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command EmulationCanEmulate where
   commandName _ = "Emulation.canEmulate"



-- | Function for the command 'Emulation.clearDeviceMetricsOverride'.
-- Clears the overridden device metrics.
emulationClearDeviceMetricsOverride :: Handle ev -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())


-- | Function for the command 'Emulation.clearGeolocationOverride'.
-- Clears the overridden Geolocation Position and Error.
emulationClearGeolocationOverride :: Handle ev -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())


-- | Function for the command 'Emulation.resetPageScaleFactor'.
-- Requests that page scale factor is reset to initial values.
emulationResetPageScaleFactor :: Handle ev -> IO (Maybe Error)
emulationResetPageScaleFactor handle = sendReceiveCommand handle "Emulation.resetPageScaleFactor" (Nothing :: Maybe ())


-- | Parameters of the 'emulationSetFocusEmulationEnabled' command.
data PEmulationSetFocusEmulationEnabled = PEmulationSetFocusEmulationEnabled {
   pEmulationSetFocusEmulationEnabledEnabled :: PEmulationSetFocusEmulationEnabledEnabled -- ^ Whether to enable to disable focus emulation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetFocusEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetFocusEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Emulation.setFocusEmulationEnabled'.
-- Enables or disables simulating a focused and active page.
-- Parameters: 'PEmulationSetFocusEmulationEnabled'
emulationSetFocusEmulationEnabled :: Handle ev -> PEmulationSetFocusEmulationEnabled -> IO (Maybe Error)
emulationSetFocusEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setFocusEmulationEnabled" (Just params)


-- | Parameters of the 'emulationSetAutoDarkModeOverride' command.
data PEmulationSetAutoDarkModeOverride = PEmulationSetAutoDarkModeOverride {
   pEmulationSetAutoDarkModeOverrideEnabled :: PEmulationSetAutoDarkModeOverrideEnabled -- ^ Whether to enable or disable automatic dark mode.
If not specified, any existing override will be cleared.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutoDarkModeOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutoDarkModeOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the command 'Emulation.setAutoDarkModeOverride'.
-- Automatically render all web contents using a dark theme.
-- Parameters: 'PEmulationSetAutoDarkModeOverride'
emulationSetAutoDarkModeOverride :: Handle ev -> PEmulationSetAutoDarkModeOverride -> IO (Maybe Error)
emulationSetAutoDarkModeOverride handle params = sendReceiveCommand handle "Emulation.setAutoDarkModeOverride" (Just params)


-- | Parameters of the 'emulationSetCpuThrottlingRate' command.
data PEmulationSetCpuThrottlingRate = PEmulationSetCpuThrottlingRate {
   pEmulationSetCpuThrottlingRateRate :: PEmulationSetCpuThrottlingRateRate -- ^ Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetCpuThrottlingRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetCpuThrottlingRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Emulation.setCPUThrottlingRate'.
-- Enables CPU throttling to emulate slow CPUs.
-- Parameters: 'PEmulationSetCpuThrottlingRate'
emulationSetCpuThrottlingRate :: Handle ev -> PEmulationSetCpuThrottlingRate -> IO (Maybe Error)
emulationSetCpuThrottlingRate handle params = sendReceiveCommand handle "Emulation.setCPUThrottlingRate" (Just params)


-- | Parameters of the 'emulationSetDefaultBackgroundColorOverride' command.
data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
   pEmulationSetDefaultBackgroundColorOverrideColor :: PEmulationSetDefaultBackgroundColorOverrideColor -- ^ RGBA of the default background color. If not specified, any existing override will be
cleared.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


-- | Function for the command 'Emulation.setDefaultBackgroundColorOverride'.
-- Sets or clears an override of the default background color of the frame. This override is used
-- if the content does not specify one.
-- Parameters: 'PEmulationSetDefaultBackgroundColorOverride'
emulationSetDefaultBackgroundColorOverride :: Handle ev -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)


-- | Parameters of the 'emulationSetDeviceMetricsOverride' command.
data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
   pEmulationSetDeviceMetricsOverrideWidth :: PEmulationSetDeviceMetricsOverrideWidth, -- ^ Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.
   pEmulationSetDeviceMetricsOverrideHeight :: PEmulationSetDeviceMetricsOverrideHeight, -- ^ Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.
   pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: PEmulationSetDeviceMetricsOverrideDeviceScaleFactor, -- ^ Overriding device scale factor value. 0 disables the override.
   pEmulationSetDeviceMetricsOverrideMobile :: PEmulationSetDeviceMetricsOverrideMobile, -- ^ Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text
autosizing and more.
   pEmulationSetDeviceMetricsOverrideScale :: PEmulationSetDeviceMetricsOverrideScale, -- ^ Scale to apply to resulting view image.
   pEmulationSetDeviceMetricsOverrideScreenWidth :: PEmulationSetDeviceMetricsOverrideScreenWidth, -- ^ Overriding screen width value in pixels (minimum 0, maximum 10000000).
   pEmulationSetDeviceMetricsOverrideScreenHeight :: PEmulationSetDeviceMetricsOverrideScreenHeight, -- ^ Overriding screen height value in pixels (minimum 0, maximum 10000000).
   pEmulationSetDeviceMetricsOverridePositionX :: PEmulationSetDeviceMetricsOverridePositionX, -- ^ Overriding view X position on screen in pixels (minimum 0, maximum 10000000).
   pEmulationSetDeviceMetricsOverridePositionY :: PEmulationSetDeviceMetricsOverridePositionY, -- ^ Overriding view Y position on screen in pixels (minimum 0, maximum 10000000).
   pEmulationSetDeviceMetricsOverrideDontSetVisibleSize :: PEmulationSetDeviceMetricsOverrideDontSetVisibleSize, -- ^ Do not set visible view size, rely upon explicit setVisibleSize call.
   pEmulationSetDeviceMetricsOverrideScreenOrientation :: PEmulationSetDeviceMetricsOverrideScreenOrientation, -- ^ Screen orientation override.
   pEmulationSetDeviceMetricsOverrideViewport :: PEmulationSetDeviceMetricsOverrideViewport, -- ^ If set, the visible area of the page will be overridden to this viewport. This viewport
change is not observed by the page, e.g. viewport-relative elements do not change positions.
   pEmulationSetDeviceMetricsOverrideDisplayFeature :: PEmulationSetDeviceMetricsOverrideDisplayFeature -- ^ If set, the display feature of a multi-segment screen. If not set, multi-segment support
is turned-off.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Emulation.setDeviceMetricsOverride'.
-- Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"/"device-height"-related CSS media
-- query results).
-- Parameters: 'PEmulationSetDeviceMetricsOverride'
emulationSetDeviceMetricsOverride :: Handle ev -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)


-- | Parameters of the 'emulationSetScrollbarsHidden' command.
data PEmulationSetScrollbarsHidden = PEmulationSetScrollbarsHidden {
   pEmulationSetScrollbarsHiddenHidden :: PEmulationSetScrollbarsHiddenHidden -- ^ Whether scrollbars should be always hidden.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScrollbarsHidden  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScrollbarsHidden where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Emulation.setScrollbarsHidden'.
-- Parameters: 'PEmulationSetScrollbarsHidden'
emulationSetScrollbarsHidden :: Handle ev -> PEmulationSetScrollbarsHidden -> IO (Maybe Error)
emulationSetScrollbarsHidden handle params = sendReceiveCommand handle "Emulation.setScrollbarsHidden" (Just params)


-- | Parameters of the 'emulationSetDocumentCookieDisabled' command.
data PEmulationSetDocumentCookieDisabled = PEmulationSetDocumentCookieDisabled {
   pEmulationSetDocumentCookieDisabledDisabled :: PEmulationSetDocumentCookieDisabledDisabled -- ^ Whether document.coookie API should be disabled.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDocumentCookieDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDocumentCookieDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'Emulation.setDocumentCookieDisabled'.
-- Parameters: 'PEmulationSetDocumentCookieDisabled'
emulationSetDocumentCookieDisabled :: Handle ev -> PEmulationSetDocumentCookieDisabled -> IO (Maybe Error)
emulationSetDocumentCookieDisabled handle params = sendReceiveCommand handle "Emulation.setDocumentCookieDisabled" (Just params)


-- | Parameters of the 'emulationSetEmitTouchEventsForMouse' command.
data PEmulationSetEmitTouchEventsForMouseConfiguration = PEmulationSetEmitTouchEventsForMouseConfigurationMobile | PEmulationSetEmitTouchEventsForMouseConfigurationDesktop
   deriving (Ord, Eq, Show, Read)
instance FromJSON PEmulationSetEmitTouchEventsForMouseConfiguration where
   parseJSON = A.withText  "PEmulationSetEmitTouchEventsForMouseConfiguration"  $ \v -> do
      case v of
         "mobile" -> pure PEmulationSetEmitTouchEventsForMouseConfigurationMobile
         "desktop" -> pure PEmulationSetEmitTouchEventsForMouseConfigurationDesktop
         _ -> fail "failed to parse PEmulationSetEmitTouchEventsForMouseConfiguration"

instance ToJSON PEmulationSetEmitTouchEventsForMouseConfiguration where
   toJSON v = A.String $
      case v of
         PEmulationSetEmitTouchEventsForMouseConfigurationMobile -> "mobile"
         PEmulationSetEmitTouchEventsForMouseConfigurationDesktop -> "desktop"



data PEmulationSetEmitTouchEventsForMouse = PEmulationSetEmitTouchEventsForMouse {
   pEmulationSetEmitTouchEventsForMouseEnabled :: PEmulationSetEmitTouchEventsForMouseEnabled, -- ^ Whether touch emulation based on mouse input should be enabled.
   pEmulationSetEmitTouchEventsForMouseConfiguration :: PEmulationSetEmitTouchEventsForMouseConfiguration -- ^ Touch/gesture events configuration. Default: current platform.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmitTouchEventsForMouse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmitTouchEventsForMouse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the command 'Emulation.setEmitTouchEventsForMouse'.
-- Parameters: 'PEmulationSetEmitTouchEventsForMouse'
emulationSetEmitTouchEventsForMouse :: Handle ev -> PEmulationSetEmitTouchEventsForMouse -> IO (Maybe Error)
emulationSetEmitTouchEventsForMouse handle params = sendReceiveCommand handle "Emulation.setEmitTouchEventsForMouse" (Just params)


-- | Parameters of the 'emulationSetEmulatedMedia' command.
data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
   pEmulationSetEmulatedMediaMedia :: PEmulationSetEmulatedMediaMedia, -- ^ Media type to emulate. Empty string disables the override.
   pEmulationSetEmulatedMediaFeatures :: PEmulationSetEmulatedMediaFeatures -- ^ Media features to emulate.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Emulation.setEmulatedMedia'.
-- Emulates the given media type or media feature for CSS media queries.
-- Parameters: 'PEmulationSetEmulatedMedia'
emulationSetEmulatedMedia :: Handle ev -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia handle params = sendReceiveCommand handle "Emulation.setEmulatedMedia" (Just params)


-- | Parameters of the 'emulationSetEmulatedVisionDeficiency' command.
data PEmulationSetEmulatedVisionDeficiencyType = PEmulationSetEmulatedVisionDeficiencyTypeNone | PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia | PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision | PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia | PEmulationSetEmulatedVisionDeficiencyTypeProtanopia | PEmulationSetEmulatedVisionDeficiencyTypeTritanopia
   deriving (Ord, Eq, Show, Read)
instance FromJSON PEmulationSetEmulatedVisionDeficiencyType where
   parseJSON = A.withText  "PEmulationSetEmulatedVisionDeficiencyType"  $ \v -> do
      case v of
         "none" -> pure PEmulationSetEmulatedVisionDeficiencyTypeNone
         "achromatopsia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia
         "blurredVision" -> pure PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision
         "deuteranopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia
         "protanopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeProtanopia
         "tritanopia" -> pure PEmulationSetEmulatedVisionDeficiencyTypeTritanopia
         _ -> fail "failed to parse PEmulationSetEmulatedVisionDeficiencyType"

instance ToJSON PEmulationSetEmulatedVisionDeficiencyType where
   toJSON v = A.String $
      case v of
         PEmulationSetEmulatedVisionDeficiencyTypeNone -> "none"
         PEmulationSetEmulatedVisionDeficiencyTypeAchromatopsia -> "achromatopsia"
         PEmulationSetEmulatedVisionDeficiencyTypeBlurredVision -> "blurredVision"
         PEmulationSetEmulatedVisionDeficiencyTypeDeuteranopia -> "deuteranopia"
         PEmulationSetEmulatedVisionDeficiencyTypeProtanopia -> "protanopia"
         PEmulationSetEmulatedVisionDeficiencyTypeTritanopia -> "tritanopia"



data PEmulationSetEmulatedVisionDeficiency = PEmulationSetEmulatedVisionDeficiency {
   pEmulationSetEmulatedVisionDeficiencyType :: PEmulationSetEmulatedVisionDeficiencyType -- ^ Vision deficiency to emulate.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedVisionDeficiency  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedVisionDeficiency where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the command 'Emulation.setEmulatedVisionDeficiency'.
-- Emulates the given vision deficiency.
-- Parameters: 'PEmulationSetEmulatedVisionDeficiency'
emulationSetEmulatedVisionDeficiency :: Handle ev -> PEmulationSetEmulatedVisionDeficiency -> IO (Maybe Error)
emulationSetEmulatedVisionDeficiency handle params = sendReceiveCommand handle "Emulation.setEmulatedVisionDeficiency" (Just params)


-- | Parameters of the 'emulationSetGeolocationOverride' command.
data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
   pEmulationSetGeolocationOverrideLatitude :: PEmulationSetGeolocationOverrideLatitude, -- ^ Mock latitude
   pEmulationSetGeolocationOverrideLongitude :: PEmulationSetGeolocationOverrideLongitude, -- ^ Mock longitude
   pEmulationSetGeolocationOverrideAccuracy :: PEmulationSetGeolocationOverrideAccuracy -- ^ Mock accuracy
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetGeolocationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetGeolocationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'Emulation.setGeolocationOverride'.
-- Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
-- Parameters: 'PEmulationSetGeolocationOverride'
emulationSetGeolocationOverride :: Handle ev -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)


-- | Parameters of the 'emulationSetIdleOverride' command.
data PEmulationSetIdleOverride = PEmulationSetIdleOverride {
   pEmulationSetIdleOverrideIsUserActive :: PEmulationSetIdleOverrideIsUserActive, -- ^ Mock isUserActive
   pEmulationSetIdleOverrideIsScreenUnlocked :: PEmulationSetIdleOverrideIsScreenUnlocked -- ^ Mock isScreenUnlocked
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetIdleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetIdleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Emulation.setIdleOverride'.
-- Overrides the Idle state.
-- Parameters: 'PEmulationSetIdleOverride'
emulationSetIdleOverride :: Handle ev -> PEmulationSetIdleOverride -> IO (Maybe Error)
emulationSetIdleOverride handle params = sendReceiveCommand handle "Emulation.setIdleOverride" (Just params)


-- | Function for the command 'Emulation.clearIdleOverride'.
-- Clears Idle state overrides.
emulationClearIdleOverride :: Handle ev -> IO (Maybe Error)
emulationClearIdleOverride handle = sendReceiveCommand handle "Emulation.clearIdleOverride" (Nothing :: Maybe ())


-- | Parameters of the 'emulationSetPageScaleFactor' command.
data PEmulationSetPageScaleFactor = PEmulationSetPageScaleFactor {
   pEmulationSetPageScaleFactorPageScaleFactor :: PEmulationSetPageScaleFactorPageScaleFactor -- ^ Page scale factor.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetPageScaleFactor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetPageScaleFactor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Emulation.setPageScaleFactor'.
-- Sets a specified page scale factor.
-- Parameters: 'PEmulationSetPageScaleFactor'
emulationSetPageScaleFactor :: Handle ev -> PEmulationSetPageScaleFactor -> IO (Maybe Error)
emulationSetPageScaleFactor handle params = sendReceiveCommand handle "Emulation.setPageScaleFactor" (Just params)


-- | Parameters of the 'emulationSetScriptExecutionDisabled' command.
data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
   pEmulationSetScriptExecutionDisabledValue :: PEmulationSetScriptExecutionDisabledValue -- ^ Whether script execution should be disabled in the page.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the command 'Emulation.setScriptExecutionDisabled'.
-- Switches script execution in the page.
-- Parameters: 'PEmulationSetScriptExecutionDisabled'
emulationSetScriptExecutionDisabled :: Handle ev -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)


-- | Parameters of the 'emulationSetTouchEmulationEnabled' command.
data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
   pEmulationSetTouchEmulationEnabledEnabled :: PEmulationSetTouchEmulationEnabledEnabled, -- ^ Whether the touch event emulation should be enabled.
   pEmulationSetTouchEmulationEnabledMaxTouchPoints :: PEmulationSetTouchEmulationEnabledMaxTouchPoints -- ^ Maximum touch points supported. Defaults to one.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTouchEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTouchEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Emulation.setTouchEmulationEnabled'.
-- Enables touch on platforms which do not support them.
-- Parameters: 'PEmulationSetTouchEmulationEnabled'
emulationSetTouchEmulationEnabled :: Handle ev -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)


-- | Parameters of the 'emulationSetVirtualTimePolicy' command.
data PEmulationSetVirtualTimePolicy = PEmulationSetVirtualTimePolicy {

   pEmulationSetVirtualTimePolicyBudget :: PEmulationSetVirtualTimePolicyBudget, -- ^ If set, after this many virtual milliseconds have elapsed virtual time will be paused and a
virtualTimeBudgetExpired event is sent.
   pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount :: PEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount, -- ^ If set this specifies the maximum number of tasks that can be run before virtual is forced
forwards to prevent deadlock.
   pEmulationSetVirtualTimePolicyInitialVirtualTime :: PEmulationSetVirtualTimePolicyInitialVirtualTime -- ^ If set, base::Time::Now will be overridden to initially return this value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetVirtualTimePolicy  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Emulation.setVirtualTimePolicy'.
-- Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
-- the current virtual time policy.  Note this supersedes any previous time budget.
-- Parameters: 'PEmulationSetVirtualTimePolicy'
-- Returns: 'EmulationSetVirtualTimePolicy'
emulationSetVirtualTimePolicy :: Handle ev -> PEmulationSetVirtualTimePolicy -> IO (Either Error EmulationSetVirtualTimePolicy)
emulationSetVirtualTimePolicy handle params = sendReceiveCommandResult handle "Emulation.setVirtualTimePolicy" (Just params)

-- | Return type of the 'emulationSetVirtualTimePolicy' command.
data EmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy {
   emulationSetVirtualTimePolicyVirtualTimeTicksBase :: Double -- ^ Absolute timestamp at which virtual time was first enabled (up time in milliseconds).
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command EmulationSetVirtualTimePolicy where
   commandName _ = "Emulation.setVirtualTimePolicy"



-- | Parameters of the 'emulationSetLocaleOverride' command.
data PEmulationSetLocaleOverride = PEmulationSetLocaleOverride {
   pEmulationSetLocaleOverrideLocale :: PEmulationSetLocaleOverrideLocale -- ^ ICU style C locale (e.g. "en_US"). If not specified or empty, disables the override and
restores default host system locale.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetLocaleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetLocaleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Emulation.setLocaleOverride'.
-- Overrides default host system locale with the specified one.
-- Parameters: 'PEmulationSetLocaleOverride'
emulationSetLocaleOverride :: Handle ev -> PEmulationSetLocaleOverride -> IO (Maybe Error)
emulationSetLocaleOverride handle params = sendReceiveCommand handle "Emulation.setLocaleOverride" (Just params)


-- | Parameters of the 'emulationSetTimezoneOverride' command.
data PEmulationSetTimezoneOverride = PEmulationSetTimezoneOverride {
   pEmulationSetTimezoneOverrideTimezoneId :: PEmulationSetTimezoneOverrideTimezoneId -- ^ The timezone identifier. If empty, disables the override and
restores default host system timezone.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTimezoneOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTimezoneOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Emulation.setTimezoneOverride'.
-- Overrides default host system timezone with the specified one.
-- Parameters: 'PEmulationSetTimezoneOverride'
emulationSetTimezoneOverride :: Handle ev -> PEmulationSetTimezoneOverride -> IO (Maybe Error)
emulationSetTimezoneOverride handle params = sendReceiveCommand handle "Emulation.setTimezoneOverride" (Just params)


-- | Parameters of the 'emulationSetDisabledImageTypes' command.
data PEmulationSetDisabledImageTypes = PEmulationSetDisabledImageTypes {
   pEmulationSetDisabledImageTypesImageTypes :: PEmulationSetDisabledImageTypesImageTypes -- ^ Image types to disable.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDisabledImageTypes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDisabledImageTypes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Emulation.setDisabledImageTypes'.
-- Parameters: 'PEmulationSetDisabledImageTypes'
emulationSetDisabledImageTypes :: Handle ev -> PEmulationSetDisabledImageTypes -> IO (Maybe Error)
emulationSetDisabledImageTypes handle params = sendReceiveCommand handle "Emulation.setDisabledImageTypes" (Just params)


-- | Parameters of the 'emulationSetHardwareConcurrencyOverride' command.
data PEmulationSetHardwareConcurrencyOverride = PEmulationSetHardwareConcurrencyOverride {
   pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency :: PEmulationSetHardwareConcurrencyOverrideHardwareConcurrency -- ^ Hardware concurrency to report
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetHardwareConcurrencyOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetHardwareConcurrencyOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the command 'Emulation.setHardwareConcurrencyOverride'.
-- Parameters: 'PEmulationSetHardwareConcurrencyOverride'
emulationSetHardwareConcurrencyOverride :: Handle ev -> PEmulationSetHardwareConcurrencyOverride -> IO (Maybe Error)
emulationSetHardwareConcurrencyOverride handle params = sendReceiveCommand handle "Emulation.setHardwareConcurrencyOverride" (Just params)


-- | Parameters of the 'emulationSetUserAgentOverride' command.
data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
   pEmulationSetUserAgentOverrideUserAgent :: PEmulationSetUserAgentOverrideUserAgent, -- ^ User agent to use.
   pEmulationSetUserAgentOverrideAcceptLanguage :: PEmulationSetUserAgentOverrideAcceptLanguage, -- ^ Browser langugage to emulate.
   pEmulationSetUserAgentOverridePlatform :: PEmulationSetUserAgentOverridePlatform, -- ^ The platform navigator.platform should return.
   pEmulationSetUserAgentOverrideUserAgentMetadata :: PEmulationSetUserAgentOverrideUserAgentMetadata -- ^ To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Emulation.setUserAgentOverride'.
-- Allows overriding user agent with the given string.
-- Parameters: 'PEmulationSetUserAgentOverride'
emulationSetUserAgentOverride :: Handle ev -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)


-- | Parameters of the 'emulationSetAutomationOverride' command.
data PEmulationSetAutomationOverride = PEmulationSetAutomationOverride {
   pEmulationSetAutomationOverrideEnabled :: PEmulationSetAutomationOverrideEnabled -- ^ Whether the override should be enabled.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutomationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutomationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Emulation.setAutomationOverride'.
-- Allows overriding the automation flag.
-- Parameters: 'PEmulationSetAutomationOverride'
emulationSetAutomationOverride :: Handle ev -> PEmulationSetAutomationOverride -> IO (Maybe Error)
emulationSetAutomationOverride handle params = sendReceiveCommand handle "Emulation.setAutomationOverride" (Just params)



-- | Resource type as it was perceived by the rendering engine.
data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkResourceType where
   parseJSON = A.withText  "NetworkResourceType"  $ \v -> do
      case v of
         "Document" -> pure NetworkResourceTypeDocument
         "Stylesheet" -> pure NetworkResourceTypeStylesheet
         "Image" -> pure NetworkResourceTypeImage
         "Media" -> pure NetworkResourceTypeMedia
         "Font" -> pure NetworkResourceTypeFont
         "Script" -> pure NetworkResourceTypeScript
         "TextTrack" -> pure NetworkResourceTypeTextTrack
         "XHR" -> pure NetworkResourceTypeXhr
         "Fetch" -> pure NetworkResourceTypeFetch
         "EventSource" -> pure NetworkResourceTypeEventSource
         "WebSocket" -> pure NetworkResourceTypeWebSocket
         "Manifest" -> pure NetworkResourceTypeManifest
         "SignedExchange" -> pure NetworkResourceTypeSignedExchange
         "Ping" -> pure NetworkResourceTypePing
         "CSPViolationReport" -> pure NetworkResourceTypeCspViolationReport
         "Preflight" -> pure NetworkResourceTypePreflight
         "Other" -> pure NetworkResourceTypeOther
         _ -> fail "failed to parse NetworkResourceType"

instance ToJSON NetworkResourceType where
   toJSON v = A.String $
      case v of
         NetworkResourceTypeDocument -> "Document"
         NetworkResourceTypeStylesheet -> "Stylesheet"
         NetworkResourceTypeImage -> "Image"
         NetworkResourceTypeMedia -> "Media"
         NetworkResourceTypeFont -> "Font"
         NetworkResourceTypeScript -> "Script"
         NetworkResourceTypeTextTrack -> "TextTrack"
         NetworkResourceTypeXhr -> "XHR"
         NetworkResourceTypeFetch -> "Fetch"
         NetworkResourceTypeEventSource -> "EventSource"
         NetworkResourceTypeWebSocket -> "WebSocket"
         NetworkResourceTypeManifest -> "Manifest"
         NetworkResourceTypeSignedExchange -> "SignedExchange"
         NetworkResourceTypePing -> "Ping"
         NetworkResourceTypeCspViolationReport -> "CSPViolationReport"
         NetworkResourceTypePreflight -> "Preflight"
         NetworkResourceTypeOther -> "Other"



-- | Unique loader identifier.
type NetworkLoaderId = String

-- | Unique request identifier.
type NetworkRequestId = String

-- | Unique intercepted request identifier.
type NetworkInterceptionId = String

-- | Network level fetch failure reason.
data NetworkErrorReason = NetworkErrorReasonFailed | NetworkErrorReasonAborted | NetworkErrorReasonTimedOut | NetworkErrorReasonAccessDenied | NetworkErrorReasonConnectionClosed | NetworkErrorReasonConnectionReset | NetworkErrorReasonConnectionRefused | NetworkErrorReasonConnectionAborted | NetworkErrorReasonConnectionFailed | NetworkErrorReasonNameNotResolved | NetworkErrorReasonInternetDisconnected | NetworkErrorReasonAddressUnreachable | NetworkErrorReasonBlockedByClient | NetworkErrorReasonBlockedByResponse
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkErrorReason where
   parseJSON = A.withText  "NetworkErrorReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkErrorReason"

instance ToJSON NetworkErrorReason where
   toJSON v = A.String $
      case v of
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



-- | UTC time in seconds, counted from January 1, 1970.
type NetworkTimeSinceEpoch = Double

-- | Monotonically increasing time in seconds since an arbitrary point in the past.
type NetworkMonotonicTime = Double

-- | Request / response headers as keys / values of JSON object.
type NetworkHeaders = [(String, String)]

-- | The underlying connection technology that the browser is supposedly using.
data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkConnectionType where
   parseJSON = A.withText  "NetworkConnectionType"  $ \v -> do
      case v of
         "none" -> pure NetworkConnectionTypeNone
         "cellular2g" -> pure NetworkConnectionTypeCellular2g
         "cellular3g" -> pure NetworkConnectionTypeCellular3g
         "cellular4g" -> pure NetworkConnectionTypeCellular4g
         "bluetooth" -> pure NetworkConnectionTypeBluetooth
         "ethernet" -> pure NetworkConnectionTypeEthernet
         "wifi" -> pure NetworkConnectionTypeWifi
         "wimax" -> pure NetworkConnectionTypeWimax
         "other" -> pure NetworkConnectionTypeOther
         _ -> fail "failed to parse NetworkConnectionType"

instance ToJSON NetworkConnectionType where
   toJSON v = A.String $
      case v of
         NetworkConnectionTypeNone -> "none"
         NetworkConnectionTypeCellular2g -> "cellular2g"
         NetworkConnectionTypeCellular3g -> "cellular3g"
         NetworkConnectionTypeCellular4g -> "cellular4g"
         NetworkConnectionTypeBluetooth -> "bluetooth"
         NetworkConnectionTypeEthernet -> "ethernet"
         NetworkConnectionTypeWifi -> "wifi"
         NetworkConnectionTypeWimax -> "wimax"
         NetworkConnectionTypeOther -> "other"



-- | Represents the cookie's 'SameSite' status:
-- https://tools.ietf.org/html/draft-west-first-party-cookies
data NetworkCookieSameSite = NetworkCookieSameSiteStrict | NetworkCookieSameSiteLax | NetworkCookieSameSiteNone
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieSameSite where
   parseJSON = A.withText  "NetworkCookieSameSite"  $ \v -> do
      case v of
         "Strict" -> pure NetworkCookieSameSiteStrict
         "Lax" -> pure NetworkCookieSameSiteLax
         "None" -> pure NetworkCookieSameSiteNone
         _ -> fail "failed to parse NetworkCookieSameSite"

instance ToJSON NetworkCookieSameSite where
   toJSON v = A.String $
      case v of
         NetworkCookieSameSiteStrict -> "Strict"
         NetworkCookieSameSiteLax -> "Lax"
         NetworkCookieSameSiteNone -> "None"



-- | Represents the cookie's 'Priority' status:
-- https://tools.ietf.org/html/draft-west-cookie-priority-00
data NetworkCookiePriority = NetworkCookiePriorityLow | NetworkCookiePriorityMedium | NetworkCookiePriorityHigh
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookiePriority where
   parseJSON = A.withText  "NetworkCookiePriority"  $ \v -> do
      case v of
         "Low" -> pure NetworkCookiePriorityLow
         "Medium" -> pure NetworkCookiePriorityMedium
         "High" -> pure NetworkCookiePriorityHigh
         _ -> fail "failed to parse NetworkCookiePriority"

instance ToJSON NetworkCookiePriority where
   toJSON v = A.String $
      case v of
         NetworkCookiePriorityLow -> "Low"
         NetworkCookiePriorityMedium -> "Medium"
         NetworkCookiePriorityHigh -> "High"



-- | Represents the source scheme of the origin that originally set the cookie.
-- A value of "Unset" allows protocol clients to emulate legacy cookie scope for the scheme.
-- This is a temporary ability and it will be removed in the future.
data NetworkCookieSourceScheme = NetworkCookieSourceSchemeUnset | NetworkCookieSourceSchemeNonSecure | NetworkCookieSourceSchemeSecure
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieSourceScheme where
   parseJSON = A.withText  "NetworkCookieSourceScheme"  $ \v -> do
      case v of
         "Unset" -> pure NetworkCookieSourceSchemeUnset
         "NonSecure" -> pure NetworkCookieSourceSchemeNonSecure
         "Secure" -> pure NetworkCookieSourceSchemeSecure
         _ -> fail "failed to parse NetworkCookieSourceScheme"

instance ToJSON NetworkCookieSourceScheme where
   toJSON v = A.String $
      case v of
         NetworkCookieSourceSchemeUnset -> "Unset"
         NetworkCookieSourceSchemeNonSecure -> "NonSecure"
         NetworkCookieSourceSchemeSecure -> "Secure"



-- | Timing information for the request.
data NetworkResourceTiming = NetworkResourceTiming {
   networkResourceTimingRequestTime :: NetworkResourceTimingRequestTime, -- ^ Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
milliseconds relatively to this requestTime.
   networkResourceTimingProxyStart :: NetworkResourceTimingProxyStart, -- ^ Started resolving proxy.
   networkResourceTimingProxyEnd :: NetworkResourceTimingProxyEnd, -- ^ Finished resolving proxy.
   networkResourceTimingDnsStart :: NetworkResourceTimingDnsStart, -- ^ Started DNS address resolve.
   networkResourceTimingDnsEnd :: NetworkResourceTimingDnsEnd, -- ^ Finished DNS address resolve.
   networkResourceTimingConnectStart :: NetworkResourceTimingConnectStart, -- ^ Started connecting to the remote host.
   networkResourceTimingConnectEnd :: NetworkResourceTimingConnectEnd, -- ^ Connected to the remote host.
   networkResourceTimingSslStart :: NetworkResourceTimingSslStart, -- ^ Started SSL handshake.
   networkResourceTimingSslEnd :: NetworkResourceTimingSslEnd, -- ^ Finished SSL handshake.
   networkResourceTimingWorkerStart :: NetworkResourceTimingWorkerStart, -- ^ Started running ServiceWorker.
   networkResourceTimingWorkerReady :: NetworkResourceTimingWorkerReady, -- ^ Finished Starting ServiceWorker.
   networkResourceTimingWorkerFetchStart :: NetworkResourceTimingWorkerFetchStart, -- ^ Started fetch event.
   networkResourceTimingWorkerRespondWithSettled :: NetworkResourceTimingWorkerRespondWithSettled, -- ^ Settled fetch event respondWith promise.
   networkResourceTimingSendStart :: NetworkResourceTimingSendStart, -- ^ Started sending request.
   networkResourceTimingSendEnd :: NetworkResourceTimingSendEnd, -- ^ Finished sending request.
   networkResourceTimingPushStart :: NetworkResourceTimingPushStart, -- ^ Time the server started pushing request.
   networkResourceTimingPushEnd :: NetworkResourceTimingPushEnd, -- ^ Time the server finished pushing request.
   networkResourceTimingReceiveHeadersEnd :: NetworkResourceTimingReceiveHeadersEnd -- ^ Finished receiving response headers.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Loading priority of a resource request.
data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkResourcePriority where
   parseJSON = A.withText  "NetworkResourcePriority"  $ \v -> do
      case v of
         "VeryLow" -> pure NetworkResourcePriorityVeryLow
         "Low" -> pure NetworkResourcePriorityLow
         "Medium" -> pure NetworkResourcePriorityMedium
         "High" -> pure NetworkResourcePriorityHigh
         "VeryHigh" -> pure NetworkResourcePriorityVeryHigh
         _ -> fail "failed to parse NetworkResourcePriority"

instance ToJSON NetworkResourcePriority where
   toJSON v = A.String $
      case v of
         NetworkResourcePriorityVeryLow -> "VeryLow"
         NetworkResourcePriorityLow -> "Low"
         NetworkResourcePriorityMedium -> "Medium"
         NetworkResourcePriorityHigh -> "High"
         NetworkResourcePriorityVeryHigh -> "VeryHigh"



-- | Post data entry for HTTP request
data NetworkPostDataEntry = NetworkPostDataEntry {
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkPostDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkPostDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | HTTP request data.
data NetworkRequestReferrerPolicy = NetworkRequestReferrerPolicyUnsafeUrl | NetworkRequestReferrerPolicyNoReferrerWhenDowngrade | NetworkRequestReferrerPolicyNoReferrer | NetworkRequestReferrerPolicyOrigin | NetworkRequestReferrerPolicyOriginWhenCrossOrigin | NetworkRequestReferrerPolicySameOrigin | NetworkRequestReferrerPolicyStrictOrigin | NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkRequestReferrerPolicy where
   parseJSON = A.withText  "NetworkRequestReferrerPolicy"  $ \v -> do
      case v of
         "unsafe-url" -> pure NetworkRequestReferrerPolicyUnsafeUrl
         "no-referrer-when-downgrade" -> pure NetworkRequestReferrerPolicyNoReferrerWhenDowngrade
         "no-referrer" -> pure NetworkRequestReferrerPolicyNoReferrer
         "origin" -> pure NetworkRequestReferrerPolicyOrigin
         "origin-when-cross-origin" -> pure NetworkRequestReferrerPolicyOriginWhenCrossOrigin
         "same-origin" -> pure NetworkRequestReferrerPolicySameOrigin
         "strict-origin" -> pure NetworkRequestReferrerPolicyStrictOrigin
         "strict-origin-when-cross-origin" -> pure NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin
         _ -> fail "failed to parse NetworkRequestReferrerPolicy"

instance ToJSON NetworkRequestReferrerPolicy where
   toJSON v = A.String $
      case v of
         NetworkRequestReferrerPolicyUnsafeUrl -> "unsafe-url"
         NetworkRequestReferrerPolicyNoReferrerWhenDowngrade -> "no-referrer-when-downgrade"
         NetworkRequestReferrerPolicyNoReferrer -> "no-referrer"
         NetworkRequestReferrerPolicyOrigin -> "origin"
         NetworkRequestReferrerPolicyOriginWhenCrossOrigin -> "origin-when-cross-origin"
         NetworkRequestReferrerPolicySameOrigin -> "same-origin"
         NetworkRequestReferrerPolicyStrictOrigin -> "strict-origin"
         NetworkRequestReferrerPolicyStrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"



data NetworkRequest = NetworkRequest {
   networkRequestUrl :: NetworkRequestUrl, -- ^ Request URL (without fragment).
   networkRequestUrlFragment :: NetworkRequestUrlFragment, -- ^ Fragment of the requested URL starting with hash, if present.
   networkRequestMethod :: NetworkRequestMethod, -- ^ HTTP request method.
   networkRequestHeaders :: NetworkRequestHeaders, -- ^ HTTP request headers.
   networkRequestPostData :: NetworkRequestPostData, -- ^ HTTP POST request data.
   networkRequestHasPostData :: NetworkRequestHasPostData, -- ^ True when the request has POST data. Note that postData might still be omitted when this flag is true when the data is too long.
   networkRequestPostDataEntries :: NetworkRequestPostDataEntries, -- ^ Request body elements. This will be converted from base64 to binary
   networkRequestMixedContentType :: NetworkRequestMixedContentType, -- ^ The mixed content type of the request.
   networkRequestInitialPriority :: NetworkRequestInitialPriority, -- ^ Priority of the resource request at the time request is sent.
   networkRequestReferrerPolicy :: NetworkRequestReferrerPolicy, -- ^ The referrer policy of the request, as defined in https://www.w3.org/TR/referrer-policy/
   networkRequestIsLinkPreload :: NetworkRequestIsLinkPreload, -- ^ Whether is loaded via link preload.
   networkRequestTrustTokenParams :: NetworkRequestTrustTokenParams, -- ^ Set for requests when the TrustToken API is used. Contains the parameters
passed by the developer (e.g. via "fetch") as understood by the backend.
   networkRequestIsSameSite :: NetworkRequestIsSameSite -- ^ True if this resource request is considered to be the 'same site' as the
request correspondinfg to the main frame.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  NetworkRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Details of a signed certificate timestamp (SCT).
data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
   networkSignedCertificateTimestampStatus :: NetworkSignedCertificateTimestampStatus, -- ^ Validation status.
   networkSignedCertificateTimestampOrigin :: NetworkSignedCertificateTimestampOrigin, -- ^ Origin.
   networkSignedCertificateTimestampLogDescription :: NetworkSignedCertificateTimestampLogDescription, -- ^ Log name / description.
   networkSignedCertificateTimestampLogId :: NetworkSignedCertificateTimestampLogId, -- ^ Log ID.
   networkSignedCertificateTimestampTimestamp :: NetworkSignedCertificateTimestampTimestamp, -- ^ Issuance date. Unlike TimeSinceEpoch, this contains the number of
milliseconds since January 1, 1970, UTC, not the number of seconds.
   networkSignedCertificateTimestampHashAlgorithm :: NetworkSignedCertificateTimestampHashAlgorithm, -- ^ Hash algorithm.
   networkSignedCertificateTimestampSignatureAlgorithm :: NetworkSignedCertificateTimestampSignatureAlgorithm, -- ^ Signature algorithm.
   networkSignedCertificateTimestampSignatureData :: NetworkSignedCertificateTimestampSignatureData -- ^ Signature data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedCertificateTimestamp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedCertificateTimestamp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Security details about a request.
data NetworkSecurityDetails = NetworkSecurityDetails {
   networkSecurityDetailsProtocol :: NetworkSecurityDetailsProtocol, -- ^ Protocol name (e.g. "TLS 1.2" or "QUIC").
   networkSecurityDetailsKeyExchange :: NetworkSecurityDetailsKeyExchange, -- ^ Key Exchange used by the connection, or the empty string if not applicable.
   networkSecurityDetailsKeyExchangeGroup :: NetworkSecurityDetailsKeyExchangeGroup, -- ^ (EC)DH group used by the connection, if applicable.
   networkSecurityDetailsCipher :: NetworkSecurityDetailsCipher, -- ^ Cipher name.
   networkSecurityDetailsMac :: NetworkSecurityDetailsMac, -- ^ TLS MAC. Note that AEAD ciphers do not have separate MACs.
   networkSecurityDetailsCertificateId :: NetworkSecurityDetailsCertificateId, -- ^ Certificate ID value.
   networkSecurityDetailsSubjectName :: NetworkSecurityDetailsSubjectName, -- ^ Certificate subject name.
   networkSecurityDetailsSanList :: NetworkSecurityDetailsSanList, -- ^ Subject Alternative Name (SAN) DNS names and IP addresses.
   networkSecurityDetailsIssuer :: NetworkSecurityDetailsIssuer, -- ^ Name of the issuing CA.
   networkSecurityDetailsValidFrom :: NetworkSecurityDetailsValidFrom, -- ^ Certificate valid from date.
   networkSecurityDetailsValidTo :: NetworkSecurityDetailsValidTo, -- ^ Certificate valid to (expiration) date
   networkSecurityDetailsSignedCertificateTimestampList :: NetworkSecurityDetailsSignedCertificateTimestampList, -- ^ List of signed certificate timestamps (SCTs).
   networkSecurityDetailsCertificateTransparencyCompliance :: NetworkSecurityDetailsCertificateTransparencyCompliance -- ^ Whether the request complied with Certificate Transparency policy
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Whether the request complied with Certificate Transparency policy.
data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCertificateTransparencyCompliance where
   parseJSON = A.withText  "NetworkCertificateTransparencyCompliance"  $ \v -> do
      case v of
         "unknown" -> pure NetworkCertificateTransparencyComplianceUnknown
         "not-compliant" -> pure NetworkCertificateTransparencyComplianceNotCompliant
         "compliant" -> pure NetworkCertificateTransparencyComplianceCompliant
         _ -> fail "failed to parse NetworkCertificateTransparencyCompliance"

instance ToJSON NetworkCertificateTransparencyCompliance where
   toJSON v = A.String $
      case v of
         NetworkCertificateTransparencyComplianceUnknown -> "unknown"
         NetworkCertificateTransparencyComplianceNotCompliant -> "not-compliant"
         NetworkCertificateTransparencyComplianceCompliant -> "compliant"



-- | The reason why request was blocked.
data NetworkBlockedReason = NetworkBlockedReasonOther | NetworkBlockedReasonCsp | NetworkBlockedReasonMixedContent | NetworkBlockedReasonOrigin | NetworkBlockedReasonInspector | NetworkBlockedReasonSubresourceFilter | NetworkBlockedReasonContentType | NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader | NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | NetworkBlockedReasonCorpNotSameOrigin | NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | NetworkBlockedReasonCorpNotSameSite
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkBlockedReason where
   parseJSON = A.withText  "NetworkBlockedReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkBlockedReason"

instance ToJSON NetworkBlockedReason where
   toJSON v = A.String $
      case v of
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



-- | The reason why request was blocked.
data NetworkCorsError = NetworkCorsErrorDisallowedByMode | NetworkCorsErrorInvalidResponse | NetworkCorsErrorWildcardOriginNotAllowed | NetworkCorsErrorMissingAllowOriginHeader | NetworkCorsErrorMultipleAllowOriginValues | NetworkCorsErrorInvalidAllowOriginValue | NetworkCorsErrorAllowOriginMismatch | NetworkCorsErrorInvalidAllowCredentials | NetworkCorsErrorCorsDisabledScheme | NetworkCorsErrorPreflightInvalidStatus | NetworkCorsErrorPreflightDisallowedRedirect | NetworkCorsErrorPreflightWildcardOriginNotAllowed | NetworkCorsErrorPreflightMissingAllowOriginHeader | NetworkCorsErrorPreflightMultipleAllowOriginValues | NetworkCorsErrorPreflightInvalidAllowOriginValue | NetworkCorsErrorPreflightAllowOriginMismatch | NetworkCorsErrorPreflightInvalidAllowCredentials | NetworkCorsErrorPreflightMissingAllowExternal | NetworkCorsErrorPreflightInvalidAllowExternal | NetworkCorsErrorPreflightMissingAllowPrivateNetwork | NetworkCorsErrorPreflightInvalidAllowPrivateNetwork | NetworkCorsErrorInvalidAllowMethodsPreflightResponse | NetworkCorsErrorInvalidAllowHeadersPreflightResponse | NetworkCorsErrorMethodDisallowedByPreflightResponse | NetworkCorsErrorHeaderDisallowedByPreflightResponse | NetworkCorsErrorRedirectContainsCredentials | NetworkCorsErrorInsecurePrivateNetwork | NetworkCorsErrorInvalidPrivateNetworkAccess | NetworkCorsErrorUnexpectedPrivateNetworkAccess | NetworkCorsErrorNoCorsRedirectModeNotFollow
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCorsError where
   parseJSON = A.withText  "NetworkCorsError"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkCorsError"

instance ToJSON NetworkCorsError where
   toJSON v = A.String $
      case v of
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



-- | Type 'Network.CorsErrorStatus' .
data NetworkCorsErrorStatus = NetworkCorsErrorStatus {


} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCorsErrorStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkCorsErrorStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Source of serviceworker response.
data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkServiceWorkerResponseSource where
   parseJSON = A.withText  "NetworkServiceWorkerResponseSource"  $ \v -> do
      case v of
         "cache-storage" -> pure NetworkServiceWorkerResponseSourceCacheStorage
         "http-cache" -> pure NetworkServiceWorkerResponseSourceHttpCache
         "fallback-code" -> pure NetworkServiceWorkerResponseSourceFallbackCode
         "network" -> pure NetworkServiceWorkerResponseSourceNetwork
         _ -> fail "failed to parse NetworkServiceWorkerResponseSource"

instance ToJSON NetworkServiceWorkerResponseSource where
   toJSON v = A.String $
      case v of
         NetworkServiceWorkerResponseSourceCacheStorage -> "cache-storage"
         NetworkServiceWorkerResponseSourceHttpCache -> "http-cache"
         NetworkServiceWorkerResponseSourceFallbackCode -> "fallback-code"
         NetworkServiceWorkerResponseSourceNetwork -> "network"



-- | Determines what type of Trust Token operation is executed and
-- depending on the type, some additional parameters. The values
-- are specified in third_party/blink/renderer/core/fetch/trust_token.idl.
data NetworkTrustTokenParamsRefreshPolicy = NetworkTrustTokenParamsRefreshPolicyUseCached | NetworkTrustTokenParamsRefreshPolicyRefresh
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenParamsRefreshPolicy where
   parseJSON = A.withText  "NetworkTrustTokenParamsRefreshPolicy"  $ \v -> do
      case v of
         "UseCached" -> pure NetworkTrustTokenParamsRefreshPolicyUseCached
         "Refresh" -> pure NetworkTrustTokenParamsRefreshPolicyRefresh
         _ -> fail "failed to parse NetworkTrustTokenParamsRefreshPolicy"

instance ToJSON NetworkTrustTokenParamsRefreshPolicy where
   toJSON v = A.String $
      case v of
         NetworkTrustTokenParamsRefreshPolicyUseCached -> "UseCached"
         NetworkTrustTokenParamsRefreshPolicyRefresh -> "Refresh"



data NetworkTrustTokenParams = NetworkTrustTokenParams {

   networkTrustTokenParamsRefreshPolicy :: NetworkTrustTokenParamsRefreshPolicy, -- ^ Only set for "token-redemption" type and determine whether
to request a fresh SRR or use a still valid cached SRR.
   networkTrustTokenParamsIssuers :: NetworkTrustTokenParamsIssuers -- ^ Origins of issuers from whom to request tokens or redemption
records.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Network.TrustTokenOperationType' .
data NetworkTrustTokenOperationType = NetworkTrustTokenOperationTypeIssuance | NetworkTrustTokenOperationTypeRedemption | NetworkTrustTokenOperationTypeSigning
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenOperationType where
   parseJSON = A.withText  "NetworkTrustTokenOperationType"  $ \v -> do
      case v of
         "Issuance" -> pure NetworkTrustTokenOperationTypeIssuance
         "Redemption" -> pure NetworkTrustTokenOperationTypeRedemption
         "Signing" -> pure NetworkTrustTokenOperationTypeSigning
         _ -> fail "failed to parse NetworkTrustTokenOperationType"

instance ToJSON NetworkTrustTokenOperationType where
   toJSON v = A.String $
      case v of
         NetworkTrustTokenOperationTypeIssuance -> "Issuance"
         NetworkTrustTokenOperationTypeRedemption -> "Redemption"
         NetworkTrustTokenOperationTypeSigning -> "Signing"



-- | HTTP response data.
data NetworkResponse = NetworkResponse {
   networkResponseUrl :: NetworkResponseUrl, -- ^ Response URL. This URL can be different from CachedResource.url in case of redirect.
   networkResponseStatus :: NetworkResponseStatus, -- ^ HTTP response status code.
   networkResponseStatusText :: NetworkResponseStatusText, -- ^ HTTP response status text.
   networkResponseHeaders :: NetworkResponseHeaders, -- ^ HTTP response headers.
   networkResponseMimeType :: NetworkResponseMimeType, -- ^ Resource mimeType as determined by the browser.
   networkResponseRequestHeaders :: NetworkResponseRequestHeaders, -- ^ Refined HTTP request headers that were actually transmitted over the network.
   networkResponseConnectionReused :: NetworkResponseConnectionReused, -- ^ Specifies whether physical connection was actually reused for this request.
   networkResponseConnectionId :: NetworkResponseConnectionId, -- ^ Physical connection id that was actually used for this request.
   networkResponseRemoteIpAddress :: NetworkResponseRemoteIpAddress, -- ^ Remote IP address.
   networkResponseRemotePort :: NetworkResponseRemotePort, -- ^ Remote port.
   networkResponseFromDiskCache :: NetworkResponseFromDiskCache, -- ^ Specifies that the request was served from the disk cache.
   networkResponseFromServiceWorker :: NetworkResponseFromServiceWorker, -- ^ Specifies that the request was served from the ServiceWorker.
   networkResponseFromPrefetchCache :: NetworkResponseFromPrefetchCache, -- ^ Specifies that the request was served from the prefetch cache.
   networkResponseEncodedDataLength :: NetworkResponseEncodedDataLength, -- ^ Total number of bytes received for this request so far.
   networkResponseTiming :: NetworkResponseTiming, -- ^ Timing information for the given request.
   networkResponseServiceWorkerResponseSource :: NetworkResponseServiceWorkerResponseSource, -- ^ Response source of response from ServiceWorker.
   networkResponseResponseTime :: NetworkResponseResponseTime, -- ^ The time at which the returned response was generated.
   networkResponseCacheStorageCacheName :: NetworkResponseCacheStorageCacheName, -- ^ Cache Storage Cache Name.
   networkResponseProtocol :: NetworkResponseProtocol, -- ^ Protocol used to fetch this request.
   networkResponseSecurityState :: NetworkResponseSecurityState, -- ^ Security state of the request resource.
   networkResponseSecurityDetails :: NetworkResponseSecurityDetails -- ^ Security details for the request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  NetworkResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | WebSocket request data.
data NetworkWebSocketRequest = NetworkWebSocketRequest {
   networkWebSocketRequestHeaders :: NetworkWebSocketRequestHeaders -- ^ HTTP request headers.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | WebSocket response data.
data NetworkWebSocketResponse = NetworkWebSocketResponse {
   networkWebSocketResponseStatus :: NetworkWebSocketResponseStatus, -- ^ HTTP response status code.
   networkWebSocketResponseStatusText :: NetworkWebSocketResponseStatusText, -- ^ HTTP response status text.
   networkWebSocketResponseHeaders :: NetworkWebSocketResponseHeaders, -- ^ HTTP response headers.
   networkWebSocketResponseHeadersText :: NetworkWebSocketResponseHeadersText, -- ^ HTTP response headers text.
   networkWebSocketResponseRequestHeaders :: NetworkWebSocketResponseRequestHeaders, -- ^ HTTP request headers.
   networkWebSocketResponseRequestHeadersText :: NetworkWebSocketResponseRequestHeadersText -- ^ HTTP request headers text.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | WebSocket message data. This represents an entire WebSocket message, not just a fragmented frame as the name suggests.
data NetworkWebSocketFrame = NetworkWebSocketFrame {
   networkWebSocketFrameOpcode :: NetworkWebSocketFrameOpcode, -- ^ WebSocket message opcode.
   networkWebSocketFrameMask :: NetworkWebSocketFrameMask, -- ^ WebSocket message mask.
   networkWebSocketFramePayloadData :: NetworkWebSocketFramePayloadData -- ^ WebSocket message payload data.
If the opcode is 1, this is a text message and payloadData is a UTF-8 string.
If the opcode isn't 1, then payloadData is a base64 encoded string representing binary data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about the cached resource.
data NetworkCachedResource = NetworkCachedResource {
   networkCachedResourceUrl :: NetworkCachedResourceUrl, -- ^ Resource URL. This is the url of the original network request.
   networkCachedResourceType :: NetworkCachedResourceType, -- ^ Type of this resource.
   networkCachedResourceResponse :: NetworkCachedResourceResponse, -- ^ Cached response data.
   networkCachedResourceBodySize :: NetworkCachedResourceBodySize -- ^ Cached response body size.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCachedResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkCachedResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about the request initiator.
data NetworkInitiatorType = NetworkInitiatorTypeParser | NetworkInitiatorTypeScript | NetworkInitiatorTypePreload | NetworkInitiatorTypeSignedExchange | NetworkInitiatorTypePreflight | NetworkInitiatorTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkInitiatorType where
   parseJSON = A.withText  "NetworkInitiatorType"  $ \v -> do
      case v of
         "parser" -> pure NetworkInitiatorTypeParser
         "script" -> pure NetworkInitiatorTypeScript
         "preload" -> pure NetworkInitiatorTypePreload
         "SignedExchange" -> pure NetworkInitiatorTypeSignedExchange
         "preflight" -> pure NetworkInitiatorTypePreflight
         "other" -> pure NetworkInitiatorTypeOther
         _ -> fail "failed to parse NetworkInitiatorType"

instance ToJSON NetworkInitiatorType where
   toJSON v = A.String $
      case v of
         NetworkInitiatorTypeParser -> "parser"
         NetworkInitiatorTypeScript -> "script"
         NetworkInitiatorTypePreload -> "preload"
         NetworkInitiatorTypeSignedExchange -> "SignedExchange"
         NetworkInitiatorTypePreflight -> "preflight"
         NetworkInitiatorTypeOther -> "other"



data NetworkInitiator = NetworkInitiator {
   networkInitiatorType :: NetworkInitiatorType, -- ^ Type of this initiator.
   networkInitiatorStack :: NetworkInitiatorStack, -- ^ Initiator JavaScript stack trace, set for Script only.
   networkInitiatorUrl :: NetworkInitiatorUrl, -- ^ Initiator URL, set for Parser type or for Script type (when script is importing module) or for SignedExchange type.
   networkInitiatorLineNumber :: NetworkInitiatorLineNumber, -- ^ Initiator line number, set for Parser type or for Script type (when script is importing
module) (0-based).
   networkInitiatorColumnNumber :: NetworkInitiatorColumnNumber, -- ^ Initiator column number, set for Parser type or for Script type (when script is importing
module) (0-based).
   networkInitiatorRequestId :: NetworkInitiatorRequestId -- ^ Set if another request triggered this request (e.g. preflight).
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkInitiator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  NetworkInitiator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Cookie object
data NetworkCookie = NetworkCookie {
   networkCookieName :: NetworkCookieName, -- ^ Cookie name.
   networkCookieValue :: NetworkCookieValue, -- ^ Cookie value.
   networkCookieDomain :: NetworkCookieDomain, -- ^ Cookie domain.
   networkCookiePath :: NetworkCookiePath, -- ^ Cookie path.
   networkCookieExpires :: NetworkCookieExpires, -- ^ Cookie expiration date as the number of seconds since the UNIX epoch.
   networkCookieSize :: NetworkCookieSize, -- ^ Cookie size.
   networkCookieHttpOnly :: NetworkCookieHttpOnly, -- ^ True if cookie is http-only.
   networkCookieSecure :: NetworkCookieSecure, -- ^ True if cookie is secure.
   networkCookieSession :: NetworkCookieSession, -- ^ True in case of session cookie.
   networkCookieSameSite :: NetworkCookieSameSite, -- ^ Cookie SameSite type.
   networkCookiePriority :: NetworkCookiePriority, -- ^ Cookie Priority
   networkCookieSameParty :: NetworkCookieSameParty, -- ^ True if cookie is SameParty.
   networkCookieSourceScheme :: NetworkCookieSourceScheme, -- ^ Cookie source scheme type.
   networkCookieSourcePort :: NetworkCookieSourcePort, -- ^ Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
This is a temporary ability and it will be removed in the future.
   networkCookiePartitionKey :: NetworkCookiePartitionKey, -- ^ Cookie partition key. The site of the top-level URL the browser was visiting at the start
of the request to the endpoint that set the cookie.
   networkCookiePartitionKeyOpaque :: NetworkCookiePartitionKeyOpaque -- ^ True if cookie partition key is opaque.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  NetworkCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Types of reasons why a cookie may not be stored from a response.
data NetworkSetCookieBlockedReason = NetworkSetCookieBlockedReasonSecureOnly | NetworkSetCookieBlockedReasonSameSiteStrict | NetworkSetCookieBlockedReasonSameSiteLax | NetworkSetCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax | NetworkSetCookieBlockedReasonSameSiteNoneInsecure | NetworkSetCookieBlockedReasonUserPreferences | NetworkSetCookieBlockedReasonSyntaxError | NetworkSetCookieBlockedReasonSchemeNotSupported | NetworkSetCookieBlockedReasonOverwriteSecure | NetworkSetCookieBlockedReasonInvalidDomain | NetworkSetCookieBlockedReasonInvalidPrefix | NetworkSetCookieBlockedReasonUnknownError | NetworkSetCookieBlockedReasonSchemefulSameSiteStrict | NetworkSetCookieBlockedReasonSchemefulSameSiteLax | NetworkSetCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax | NetworkSetCookieBlockedReasonSamePartyFromCrossPartyContext | NetworkSetCookieBlockedReasonSamePartyConflictsWithOtherAttributes | NetworkSetCookieBlockedReasonNameValuePairExceedsMaxSize
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkSetCookieBlockedReason where
   parseJSON = A.withText  "NetworkSetCookieBlockedReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkSetCookieBlockedReason"

instance ToJSON NetworkSetCookieBlockedReason where
   toJSON v = A.String $
      case v of
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



-- | Types of reasons why a cookie may not be sent with a request.
data NetworkCookieBlockedReason = NetworkCookieBlockedReasonSecureOnly | NetworkCookieBlockedReasonNotOnPath | NetworkCookieBlockedReasonDomainMismatch | NetworkCookieBlockedReasonSameSiteStrict | NetworkCookieBlockedReasonSameSiteLax | NetworkCookieBlockedReasonSameSiteUnspecifiedTreatedAsLax | NetworkCookieBlockedReasonSameSiteNoneInsecure | NetworkCookieBlockedReasonUserPreferences | NetworkCookieBlockedReasonUnknownError | NetworkCookieBlockedReasonSchemefulSameSiteStrict | NetworkCookieBlockedReasonSchemefulSameSiteLax | NetworkCookieBlockedReasonSchemefulSameSiteUnspecifiedTreatedAsLax | NetworkCookieBlockedReasonSamePartyFromCrossPartyContext | NetworkCookieBlockedReasonNameValuePairExceedsMaxSize
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCookieBlockedReason where
   parseJSON = A.withText  "NetworkCookieBlockedReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkCookieBlockedReason"

instance ToJSON NetworkCookieBlockedReason where
   toJSON v = A.String $
      case v of
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



-- | A cookie which was not stored from a response with the corresponding reason.
data NetworkBlockedSetCookieWithReason = NetworkBlockedSetCookieWithReason {
   networkBlockedSetCookieWithReasonBlockedReasons :: NetworkBlockedSetCookieWithReasonBlockedReasons, -- ^ The reason(s) this cookie was blocked.
   networkBlockedSetCookieWithReasonCookieLine :: NetworkBlockedSetCookieWithReasonCookieLine, -- ^ The string representing this individual cookie as it would appear in the header.
This is not the entire "cookie" or "set-cookie" header which could have multiple cookies.
   networkBlockedSetCookieWithReasonCookie :: NetworkBlockedSetCookieWithReasonCookie -- ^ The cookie object which represents the cookie which was not stored. It is optional because
sometimes complete cookie information is not available, such as in the case of parsing
errors.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedSetCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedSetCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | A cookie with was not sent with a request with the corresponding reason.
data NetworkBlockedCookieWithReason = NetworkBlockedCookieWithReason {
   networkBlockedCookieWithReasonBlockedReasons :: NetworkBlockedCookieWithReasonBlockedReasons, -- ^ The reason(s) the cookie was blocked.
   networkBlockedCookieWithReasonCookie :: NetworkBlockedCookieWithReasonCookie -- ^ The cookie object representing the cookie which was not sent.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Cookie parameter object
data NetworkCookieParam = NetworkCookieParam {
   networkCookieParamName :: NetworkCookieParamName, -- ^ Cookie name.
   networkCookieParamValue :: NetworkCookieParamValue, -- ^ Cookie value.
   networkCookieParamUrl :: NetworkCookieParamUrl, -- ^ The request-URI to associate with the setting of the cookie. This value can affect the
default domain, path, source port, and source scheme values of the created cookie.
   networkCookieParamDomain :: NetworkCookieParamDomain, -- ^ Cookie domain.
   networkCookieParamPath :: NetworkCookieParamPath, -- ^ Cookie path.
   networkCookieParamSecure :: NetworkCookieParamSecure, -- ^ True if cookie is secure.
   networkCookieParamHttpOnly :: NetworkCookieParamHttpOnly, -- ^ True if cookie is http-only.
   networkCookieParamSameSite :: NetworkCookieParamSameSite, -- ^ Cookie SameSite type.
   networkCookieParamExpires :: NetworkCookieParamExpires, -- ^ Cookie expiration date, session cookie if not set
   networkCookieParamPriority :: NetworkCookieParamPriority, -- ^ Cookie Priority.
   networkCookieParamSameParty :: NetworkCookieParamSameParty, -- ^ True if cookie is SameParty.
   networkCookieParamSourceScheme :: NetworkCookieParamSourceScheme, -- ^ Cookie source scheme type.
   networkCookieParamSourcePort :: NetworkCookieParamSourcePort, -- ^ Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
This is a temporary ability and it will be removed in the future.
   networkCookieParamPartitionKey :: NetworkCookieParamPartitionKey -- ^ Cookie partition key. The site of the top-level URL the browser was visiting at the start
of the request to the endpoint that set the cookie.
If not set, the cookie will be set as not partitioned.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookieParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  NetworkCookieParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Authorization challenge for HTTP status code 401 or 407.
data NetworkAuthChallengeSource = NetworkAuthChallengeSourceServer | NetworkAuthChallengeSourceProxy
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkAuthChallengeSource where
   parseJSON = A.withText  "NetworkAuthChallengeSource"  $ \v -> do
      case v of
         "Server" -> pure NetworkAuthChallengeSourceServer
         "Proxy" -> pure NetworkAuthChallengeSourceProxy
         _ -> fail "failed to parse NetworkAuthChallengeSource"

instance ToJSON NetworkAuthChallengeSource where
   toJSON v = A.String $
      case v of
         NetworkAuthChallengeSourceServer -> "Server"
         NetworkAuthChallengeSourceProxy -> "Proxy"



data NetworkAuthChallenge = NetworkAuthChallenge {
   networkAuthChallengeSource :: NetworkAuthChallengeSource, -- ^ Source of the authentication challenge.
   networkAuthChallengeOrigin :: NetworkAuthChallengeOrigin, -- ^ Origin of the challenger.
   networkAuthChallengeScheme :: NetworkAuthChallengeScheme, -- ^ The authentication scheme used, such as basic or digest
   networkAuthChallengeRealm :: NetworkAuthChallengeRealm -- ^ The realm of the challenge. May be empty.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkAuthChallenge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkAuthChallenge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Response to an AuthChallenge.
data NetworkAuthChallengeResponseResponse = NetworkAuthChallengeResponseResponseDefault | NetworkAuthChallengeResponseResponseCancelAuth | NetworkAuthChallengeResponseResponseProvideCredentials
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkAuthChallengeResponseResponse where
   parseJSON = A.withText  "NetworkAuthChallengeResponseResponse"  $ \v -> do
      case v of
         "Default" -> pure NetworkAuthChallengeResponseResponseDefault
         "CancelAuth" -> pure NetworkAuthChallengeResponseResponseCancelAuth
         "ProvideCredentials" -> pure NetworkAuthChallengeResponseResponseProvideCredentials
         _ -> fail "failed to parse NetworkAuthChallengeResponseResponse"

instance ToJSON NetworkAuthChallengeResponseResponse where
   toJSON v = A.String $
      case v of
         NetworkAuthChallengeResponseResponseDefault -> "Default"
         NetworkAuthChallengeResponseResponseCancelAuth -> "CancelAuth"
         NetworkAuthChallengeResponseResponseProvideCredentials -> "ProvideCredentials"



data NetworkAuthChallengeResponse = NetworkAuthChallengeResponse {
   networkAuthChallengeResponseResponse :: NetworkAuthChallengeResponseResponse, -- ^ The decision on what to do in response to the authorization challenge.  Default means
deferring to the default behavior of the net stack, which will likely either the Cancel
authentication or display a popup dialog box.
   networkAuthChallengeResponseUsername :: NetworkAuthChallengeResponseUsername, -- ^ The username to provide, possibly empty. Should only be set if response is
ProvideCredentials.
   networkAuthChallengeResponsePassword :: NetworkAuthChallengeResponsePassword -- ^ The password to provide, possibly empty. Should only be set if response is
ProvideCredentials.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  NetworkAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Stages of the interception to begin intercepting. Request will intercept before the request is
-- sent. Response will intercept after the response is received.
data NetworkInterceptionStage = NetworkInterceptionStageRequest | NetworkInterceptionStageHeadersReceived
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkInterceptionStage where
   parseJSON = A.withText  "NetworkInterceptionStage"  $ \v -> do
      case v of
         "Request" -> pure NetworkInterceptionStageRequest
         "HeadersReceived" -> pure NetworkInterceptionStageHeadersReceived
         _ -> fail "failed to parse NetworkInterceptionStage"

instance ToJSON NetworkInterceptionStage where
   toJSON v = A.String $
      case v of
         NetworkInterceptionStageRequest -> "Request"
         NetworkInterceptionStageHeadersReceived -> "HeadersReceived"



-- | Request pattern for interception.
data NetworkRequestPattern = NetworkRequestPattern {
   networkRequestPatternUrlPattern :: NetworkRequestPatternUrlPattern, -- ^ Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
backslash. Omitting is equivalent to `"*"`.
   networkRequestPatternResourceType :: NetworkRequestPatternResourceType, -- ^ If set, only requests for matching resource types will be intercepted.
   networkRequestPatternInterceptionStage :: NetworkRequestPatternInterceptionStage -- ^ Stage at which to begin intercepting requests. Default is Request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about a signed exchange signature.
-- https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#rfc.section.3.1
data NetworkSignedExchangeSignature = NetworkSignedExchangeSignature {
   networkSignedExchangeSignatureLabel :: NetworkSignedExchangeSignatureLabel, -- ^ Signed exchange signature label.
   networkSignedExchangeSignatureSignature :: NetworkSignedExchangeSignatureSignature, -- ^ The hex string of signed exchange signature.
   networkSignedExchangeSignatureIntegrity :: NetworkSignedExchangeSignatureIntegrity, -- ^ Signed exchange signature integrity.
   networkSignedExchangeSignatureCertUrl :: NetworkSignedExchangeSignatureCertUrl, -- ^ Signed exchange signature cert Url.
   networkSignedExchangeSignatureCertSha256 :: NetworkSignedExchangeSignatureCertSha256, -- ^ The hex string of signed exchange signature cert sha256.
   networkSignedExchangeSignatureValidityUrl :: NetworkSignedExchangeSignatureValidityUrl, -- ^ Signed exchange signature validity Url.
   networkSignedExchangeSignatureDate :: NetworkSignedExchangeSignatureDate, -- ^ Signed exchange signature date.
   networkSignedExchangeSignatureExpires :: NetworkSignedExchangeSignatureExpires, -- ^ Signed exchange signature expires.
   networkSignedExchangeSignatureCertificates :: NetworkSignedExchangeSignatureCertificates -- ^ The encoded certificates.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeSignature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeSignature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Information about a signed exchange header.
-- https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#cbor-representation
data NetworkSignedExchangeHeader = NetworkSignedExchangeHeader {
   networkSignedExchangeHeaderRequestUrl :: NetworkSignedExchangeHeaderRequestUrl, -- ^ Signed exchange request URL.
   networkSignedExchangeHeaderResponseCode :: NetworkSignedExchangeHeaderResponseCode, -- ^ Signed exchange response code.
   networkSignedExchangeHeaderResponseHeaders :: NetworkSignedExchangeHeaderResponseHeaders, -- ^ Signed exchange response headers.
   networkSignedExchangeHeaderSignatures :: NetworkSignedExchangeHeaderSignatures, -- ^ Signed exchange response signature.
   networkSignedExchangeHeaderHeaderIntegrity :: NetworkSignedExchangeHeaderHeaderIntegrity -- ^ Signed exchange header integrity hash in the form of "sha256-<base64-hash-value>".
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Field type for a signed exchange related error.
data NetworkSignedExchangeErrorField = NetworkSignedExchangeErrorFieldSignatureSig | NetworkSignedExchangeErrorFieldSignatureIntegrity | NetworkSignedExchangeErrorFieldSignatureCertUrl | NetworkSignedExchangeErrorFieldSignatureCertSha256 | NetworkSignedExchangeErrorFieldSignatureValidityUrl | NetworkSignedExchangeErrorFieldSignatureTimestamps
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkSignedExchangeErrorField where
   parseJSON = A.withText  "NetworkSignedExchangeErrorField"  $ \v -> do
      case v of
         "signatureSig" -> pure NetworkSignedExchangeErrorFieldSignatureSig
         "signatureIntegrity" -> pure NetworkSignedExchangeErrorFieldSignatureIntegrity
         "signatureCertUrl" -> pure NetworkSignedExchangeErrorFieldSignatureCertUrl
         "signatureCertSha256" -> pure NetworkSignedExchangeErrorFieldSignatureCertSha256
         "signatureValidityUrl" -> pure NetworkSignedExchangeErrorFieldSignatureValidityUrl
         "signatureTimestamps" -> pure NetworkSignedExchangeErrorFieldSignatureTimestamps
         _ -> fail "failed to parse NetworkSignedExchangeErrorField"

instance ToJSON NetworkSignedExchangeErrorField where
   toJSON v = A.String $
      case v of
         NetworkSignedExchangeErrorFieldSignatureSig -> "signatureSig"
         NetworkSignedExchangeErrorFieldSignatureIntegrity -> "signatureIntegrity"
         NetworkSignedExchangeErrorFieldSignatureCertUrl -> "signatureCertUrl"
         NetworkSignedExchangeErrorFieldSignatureCertSha256 -> "signatureCertSha256"
         NetworkSignedExchangeErrorFieldSignatureValidityUrl -> "signatureValidityUrl"
         NetworkSignedExchangeErrorFieldSignatureTimestamps -> "signatureTimestamps"



-- | Information about a signed exchange response.
data NetworkSignedExchangeError = NetworkSignedExchangeError {
   networkSignedExchangeErrorMessage :: NetworkSignedExchangeErrorMessage, -- ^ Error message.
   networkSignedExchangeErrorSignatureIndex :: NetworkSignedExchangeErrorSignatureIndex, -- ^ The index of the signature which caused the error.
   networkSignedExchangeErrorErrorField :: NetworkSignedExchangeErrorErrorField -- ^ The field which caused the error.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Information about a signed exchange response.
data NetworkSignedExchangeInfo = NetworkSignedExchangeInfo {
   networkSignedExchangeInfoOuterResponse :: NetworkSignedExchangeInfoOuterResponse, -- ^ The outer response of signed HTTP exchange which was received from network.
   networkSignedExchangeInfoHeader :: NetworkSignedExchangeInfoHeader, -- ^ Information about the signed exchange header.
   networkSignedExchangeInfoSecurityDetails :: NetworkSignedExchangeInfoSecurityDetails, -- ^ Security details for the signed exchange header.
   networkSignedExchangeInfoErrors :: NetworkSignedExchangeInfoErrors -- ^ Errors occurred while handling the signed exchagne.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | List of content encodings supported by the backend.
data NetworkContentEncoding = NetworkContentEncodingDeflate | NetworkContentEncodingGzip | NetworkContentEncodingBr
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkContentEncoding where
   parseJSON = A.withText  "NetworkContentEncoding"  $ \v -> do
      case v of
         "deflate" -> pure NetworkContentEncodingDeflate
         "gzip" -> pure NetworkContentEncodingGzip
         "br" -> pure NetworkContentEncodingBr
         _ -> fail "failed to parse NetworkContentEncoding"

instance ToJSON NetworkContentEncoding where
   toJSON v = A.String $
      case v of
         NetworkContentEncodingDeflate -> "deflate"
         NetworkContentEncodingGzip -> "gzip"
         NetworkContentEncodingBr -> "br"



-- | Type 'Network.PrivateNetworkRequestPolicy' .
data NetworkPrivateNetworkRequestPolicy = NetworkPrivateNetworkRequestPolicyAllow | NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate | NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate | NetworkPrivateNetworkRequestPolicyPreflightBlock | NetworkPrivateNetworkRequestPolicyPreflightWarn
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkPrivateNetworkRequestPolicy where
   parseJSON = A.withText  "NetworkPrivateNetworkRequestPolicy"  $ \v -> do
      case v of
         "Allow" -> pure NetworkPrivateNetworkRequestPolicyAllow
         "BlockFromInsecureToMorePrivate" -> pure NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate
         "WarnFromInsecureToMorePrivate" -> pure NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate
         "PreflightBlock" -> pure NetworkPrivateNetworkRequestPolicyPreflightBlock
         "PreflightWarn" -> pure NetworkPrivateNetworkRequestPolicyPreflightWarn
         _ -> fail "failed to parse NetworkPrivateNetworkRequestPolicy"

instance ToJSON NetworkPrivateNetworkRequestPolicy where
   toJSON v = A.String $
      case v of
         NetworkPrivateNetworkRequestPolicyAllow -> "Allow"
         NetworkPrivateNetworkRequestPolicyBlockFromInsecureToMorePrivate -> "BlockFromInsecureToMorePrivate"
         NetworkPrivateNetworkRequestPolicyWarnFromInsecureToMorePrivate -> "WarnFromInsecureToMorePrivate"
         NetworkPrivateNetworkRequestPolicyPreflightBlock -> "PreflightBlock"
         NetworkPrivateNetworkRequestPolicyPreflightWarn -> "PreflightWarn"



-- | Type 'Network.IPAddressSpace' .
data NetworkIpAddressSpace = NetworkIpAddressSpaceLocal | NetworkIpAddressSpacePrivate | NetworkIpAddressSpacePublic | NetworkIpAddressSpaceUnknown
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkIpAddressSpace where
   parseJSON = A.withText  "NetworkIpAddressSpace"  $ \v -> do
      case v of
         "Local" -> pure NetworkIpAddressSpaceLocal
         "Private" -> pure NetworkIpAddressSpacePrivate
         "Public" -> pure NetworkIpAddressSpacePublic
         "Unknown" -> pure NetworkIpAddressSpaceUnknown
         _ -> fail "failed to parse NetworkIpAddressSpace"

instance ToJSON NetworkIpAddressSpace where
   toJSON v = A.String $
      case v of
         NetworkIpAddressSpaceLocal -> "Local"
         NetworkIpAddressSpacePrivate -> "Private"
         NetworkIpAddressSpacePublic -> "Public"
         NetworkIpAddressSpaceUnknown -> "Unknown"



-- | Type 'Network.ConnectTiming' .
data NetworkConnectTiming = NetworkConnectTiming {
   networkConnectTimingRequestTime :: NetworkConnectTimingRequestTime -- ^ Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
milliseconds relatively to this requestTime. Matches ResourceTiming's requestTime for
the same request (but not for redirected requests).
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkConnectTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkConnectTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Network.ClientSecurityState' .
data NetworkClientSecurityState = NetworkClientSecurityState {



} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkClientSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkClientSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Network.CrossOriginOpenerPolicyValue' .
data NetworkCrossOriginOpenerPolicyValue = NetworkCrossOriginOpenerPolicyValueSameOrigin | NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups | NetworkCrossOriginOpenerPolicyValueUnsafeNone | NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep | NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopupsPlusCoep
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCrossOriginOpenerPolicyValue where
   parseJSON = A.withText  "NetworkCrossOriginOpenerPolicyValue"  $ \v -> do
      case v of
         "SameOrigin" -> pure NetworkCrossOriginOpenerPolicyValueSameOrigin
         "SameOriginAllowPopups" -> pure NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups
         "UnsafeNone" -> pure NetworkCrossOriginOpenerPolicyValueUnsafeNone
         "SameOriginPlusCoep" -> pure NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep
         "SameOriginAllowPopupsPlusCoep" -> pure NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopupsPlusCoep
         _ -> fail "failed to parse NetworkCrossOriginOpenerPolicyValue"

instance ToJSON NetworkCrossOriginOpenerPolicyValue where
   toJSON v = A.String $
      case v of
         NetworkCrossOriginOpenerPolicyValueSameOrigin -> "SameOrigin"
         NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopups -> "SameOriginAllowPopups"
         NetworkCrossOriginOpenerPolicyValueUnsafeNone -> "UnsafeNone"
         NetworkCrossOriginOpenerPolicyValueSameOriginPlusCoep -> "SameOriginPlusCoep"
         NetworkCrossOriginOpenerPolicyValueSameOriginAllowPopupsPlusCoep -> "SameOriginAllowPopupsPlusCoep"



-- | Type 'Network.CrossOriginOpenerPolicyStatus' .
data NetworkCrossOriginOpenerPolicyStatus = NetworkCrossOriginOpenerPolicyStatus {




} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCrossOriginOpenerPolicyStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  NetworkCrossOriginOpenerPolicyStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Network.CrossOriginEmbedderPolicyValue' .
data NetworkCrossOriginEmbedderPolicyValue = NetworkCrossOriginEmbedderPolicyValueNone | NetworkCrossOriginEmbedderPolicyValueCredentialless | NetworkCrossOriginEmbedderPolicyValueRequireCorp
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkCrossOriginEmbedderPolicyValue where
   parseJSON = A.withText  "NetworkCrossOriginEmbedderPolicyValue"  $ \v -> do
      case v of
         "None" -> pure NetworkCrossOriginEmbedderPolicyValueNone
         "Credentialless" -> pure NetworkCrossOriginEmbedderPolicyValueCredentialless
         "RequireCorp" -> pure NetworkCrossOriginEmbedderPolicyValueRequireCorp
         _ -> fail "failed to parse NetworkCrossOriginEmbedderPolicyValue"

instance ToJSON NetworkCrossOriginEmbedderPolicyValue where
   toJSON v = A.String $
      case v of
         NetworkCrossOriginEmbedderPolicyValueNone -> "None"
         NetworkCrossOriginEmbedderPolicyValueCredentialless -> "Credentialless"
         NetworkCrossOriginEmbedderPolicyValueRequireCorp -> "RequireCorp"



-- | Type 'Network.CrossOriginEmbedderPolicyStatus' .
data NetworkCrossOriginEmbedderPolicyStatus = NetworkCrossOriginEmbedderPolicyStatus {




} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCrossOriginEmbedderPolicyStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  NetworkCrossOriginEmbedderPolicyStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'Network.SecurityIsolationStatus' .
data NetworkSecurityIsolationStatus = NetworkSecurityIsolationStatus {


} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | The status of a Reporting API report.
data NetworkReportStatus = NetworkReportStatusQueued | NetworkReportStatusPending | NetworkReportStatusMarkedForRemoval | NetworkReportStatusSuccess
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkReportStatus where
   parseJSON = A.withText  "NetworkReportStatus"  $ \v -> do
      case v of
         "Queued" -> pure NetworkReportStatusQueued
         "Pending" -> pure NetworkReportStatusPending
         "MarkedForRemoval" -> pure NetworkReportStatusMarkedForRemoval
         "Success" -> pure NetworkReportStatusSuccess
         _ -> fail "failed to parse NetworkReportStatus"

instance ToJSON NetworkReportStatus where
   toJSON v = A.String $
      case v of
         NetworkReportStatusQueued -> "Queued"
         NetworkReportStatusPending -> "Pending"
         NetworkReportStatusMarkedForRemoval -> "MarkedForRemoval"
         NetworkReportStatusSuccess -> "Success"



-- | Type 'Network.ReportId' .
type NetworkReportId = String

-- | An object representing a report generated by the Reporting API.
data NetworkReportingApiReport = NetworkReportingApiReport {

   networkReportingApiReportInitiatorUrl :: NetworkReportingApiReportInitiatorUrl, -- ^ The URL of the document that triggered the report.
   networkReportingApiReportDestination :: NetworkReportingApiReportDestination, -- ^ The name of the endpoint group that should be used to deliver the report.
   networkReportingApiReportType :: NetworkReportingApiReportType, -- ^ The type of the report (specifies the set of data that is contained in the report body).
   networkReportingApiReportTimestamp :: NetworkReportingApiReportTimestamp, -- ^ When the report was generated.
   networkReportingApiReportDepth :: NetworkReportingApiReportDepth, -- ^ How many uploads deep the related request was.
   networkReportingApiReportCompletedAttempts :: NetworkReportingApiReportCompletedAttempts, -- ^ The number of delivery attempts made so far, not including an active attempt.


} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'Network.ReportingApiEndpoint' .
data NetworkReportingApiEndpoint = NetworkReportingApiEndpoint {
   networkReportingApiEndpointUrl :: NetworkReportingApiEndpointUrl, -- ^ The URL of the endpoint to which reports may be delivered.
   networkReportingApiEndpointGroupName :: NetworkReportingApiEndpointGroupName -- ^ Name of the endpoint group.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | An object providing the result of a network resource load.
data NetworkLoadNetworkResourcePageResult = NetworkLoadNetworkResourcePageResult {

   networkLoadNetworkResourcePageResultNetError :: NetworkLoadNetworkResourcePageResultNetError, -- ^ Optional values used for error reporting.


   networkLoadNetworkResourcePageResultStream :: NetworkLoadNetworkResourcePageResultStream, -- ^ If successful, one of the following two fields holds the result.
   networkLoadNetworkResourcePageResultHeaders :: NetworkLoadNetworkResourcePageResultHeaders -- ^ Response headers.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourcePageResult  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourcePageResult where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | An options object that may be extended later to better support CORS,
-- CORB and streaming.
data NetworkLoadNetworkResourceOptions = NetworkLoadNetworkResourceOptions {


} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourceOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourceOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }





-- | Type of the 'Network.dataReceived' event.
data NetworkDataReceived = NetworkDataReceived {
   networkDataReceivedRequestId :: NetworkDataReceivedRequestId, -- ^ Request identifier.
   networkDataReceivedTimestamp :: NetworkDataReceivedTimestamp, -- ^ Timestamp.
   networkDataReceivedDataLength :: NetworkDataReceivedDataLength, -- ^ Data chunk length.
   networkDataReceivedEncodedDataLength :: NetworkDataReceivedEncodedDataLength -- ^ Actual bytes received (might be less than dataLength for compressed encodings).
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkDataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  NetworkDataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Network.eventSourceMessageReceived' event.
data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
   networkEventSourceMessageReceivedRequestId :: NetworkEventSourceMessageReceivedRequestId, -- ^ Request identifier.
   networkEventSourceMessageReceivedTimestamp :: NetworkEventSourceMessageReceivedTimestamp, -- ^ Timestamp.
   networkEventSourceMessageReceivedEventName :: NetworkEventSourceMessageReceivedEventName, -- ^ Message type.
   networkEventSourceMessageReceivedEventId :: NetworkEventSourceMessageReceivedEventId, -- ^ Message identifier.
   networkEventSourceMessageReceivedData :: NetworkEventSourceMessageReceivedData -- ^ Message content.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkEventSourceMessageReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkEventSourceMessageReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type of the 'Network.loadingFailed' event.
data NetworkLoadingFailed = NetworkLoadingFailed {
   networkLoadingFailedRequestId :: NetworkLoadingFailedRequestId, -- ^ Request identifier.
   networkLoadingFailedTimestamp :: NetworkLoadingFailedTimestamp, -- ^ Timestamp.
   networkLoadingFailedType :: NetworkLoadingFailedType, -- ^ Resource type.
   networkLoadingFailedErrorText :: NetworkLoadingFailedErrorText, -- ^ User friendly error message.
   networkLoadingFailedCanceled :: NetworkLoadingFailedCanceled, -- ^ True if loading was canceled.
   networkLoadingFailedBlockedReason :: NetworkLoadingFailedBlockedReason, -- ^ The reason why loading was blocked, if any.
   networkLoadingFailedCorsErrorStatus :: NetworkLoadingFailedCorsErrorStatus -- ^ The reason why loading was blocked by CORS, if any.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFailed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFailed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'Network.loadingFinished' event.
data NetworkLoadingFinished = NetworkLoadingFinished {
   networkLoadingFinishedRequestId :: NetworkLoadingFinishedRequestId, -- ^ Request identifier.
   networkLoadingFinishedTimestamp :: NetworkLoadingFinishedTimestamp, -- ^ Timestamp.
   networkLoadingFinishedEncodedDataLength :: NetworkLoadingFinishedEncodedDataLength, -- ^ Total number of bytes received for this request.
   networkLoadingFinishedShouldReportCorbBlocking :: NetworkLoadingFinishedShouldReportCorbBlocking -- ^ Set when 1) response was blocked by Cross-Origin Read Blocking and also
2) this needs to be reported to the DevTools console.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Network.requestServedFromCache' event.
data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
   networkRequestServedFromCacheRequestId :: NetworkRequestServedFromCacheRequestId -- ^ Request identifier.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestServedFromCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestServedFromCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.requestWillBeSent' event.
data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
   networkRequestWillBeSentRequestId :: NetworkRequestWillBeSentRequestId, -- ^ Request identifier.
   networkRequestWillBeSentLoaderId :: NetworkRequestWillBeSentLoaderId, -- ^ Loader identifier. Empty string if the request is fetched from worker.
   networkRequestWillBeSentDocumentUrl :: NetworkRequestWillBeSentDocumentUrl, -- ^ URL of the document this request is loaded for.
   networkRequestWillBeSentRequest :: NetworkRequestWillBeSentRequest, -- ^ Request data.
   networkRequestWillBeSentTimestamp :: NetworkRequestWillBeSentTimestamp, -- ^ Timestamp.
   networkRequestWillBeSentWallTime :: NetworkRequestWillBeSentWallTime, -- ^ Timestamp.
   networkRequestWillBeSentInitiator :: NetworkRequestWillBeSentInitiator, -- ^ Request initiator.
   networkRequestWillBeSentRedirectHasExtraInfo :: NetworkRequestWillBeSentRedirectHasExtraInfo, -- ^ In the case that redirectResponse is populated, this flag indicates whether
requestWillBeSentExtraInfo and responseReceivedExtraInfo events will be or were emitted
for the request which was just redirected.
   networkRequestWillBeSentRedirectResponse :: NetworkRequestWillBeSentRedirectResponse, -- ^ Redirect response data.
   networkRequestWillBeSentType :: NetworkRequestWillBeSentType, -- ^ Type of this resource.
   networkRequestWillBeSentFrameId :: NetworkRequestWillBeSentFrameId, -- ^ Frame identifier.
   networkRequestWillBeSentHasUserGesture :: NetworkRequestWillBeSentHasUserGesture -- ^ Whether the request is initiated by a user gesture. Defaults to false.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'Network.resourceChangedPriority' event.
data NetworkResourceChangedPriority = NetworkResourceChangedPriority {
   networkResourceChangedPriorityRequestId :: NetworkResourceChangedPriorityRequestId, -- ^ Request identifier.
   networkResourceChangedPriorityNewPriority :: NetworkResourceChangedPriorityNewPriority, -- ^ New priority
   networkResourceChangedPriorityTimestamp :: NetworkResourceChangedPriorityTimestamp -- ^ Timestamp.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceChangedPriority  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceChangedPriority where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.signedExchangeReceived' event.
data NetworkSignedExchangeReceived = NetworkSignedExchangeReceived {
   networkSignedExchangeReceivedRequestId :: NetworkSignedExchangeReceivedRequestId, -- ^ Request identifier.
   networkSignedExchangeReceivedInfo :: NetworkSignedExchangeReceivedInfo -- ^ Information about the signed exchange response.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.responseReceived' event.
data NetworkResponseReceived = NetworkResponseReceived {
   networkResponseReceivedRequestId :: NetworkResponseReceivedRequestId, -- ^ Request identifier.
   networkResponseReceivedLoaderId :: NetworkResponseReceivedLoaderId, -- ^ Loader identifier. Empty string if the request is fetched from worker.
   networkResponseReceivedTimestamp :: NetworkResponseReceivedTimestamp, -- ^ Timestamp.
   networkResponseReceivedType :: NetworkResponseReceivedType, -- ^ Resource type.
   networkResponseReceivedResponse :: NetworkResponseReceivedResponse, -- ^ Response data.
   networkResponseReceivedHasExtraInfo :: NetworkResponseReceivedHasExtraInfo, -- ^ Indicates whether requestWillBeSentExtraInfo and responseReceivedExtraInfo events will be
or were emitted for this request.
   networkResponseReceivedFrameId :: NetworkResponseReceivedFrameId -- ^ Frame identifier.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Network.webSocketClosed' event.
data NetworkWebSocketClosed = NetworkWebSocketClosed {
   networkWebSocketClosedRequestId :: NetworkWebSocketClosedRequestId, -- ^ Request identifier.
   networkWebSocketClosedTimestamp :: NetworkWebSocketClosedTimestamp -- ^ Timestamp.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Network.webSocketCreated' event.
data NetworkWebSocketCreated = NetworkWebSocketCreated {
   networkWebSocketCreatedRequestId :: NetworkWebSocketCreatedRequestId, -- ^ Request identifier.
   networkWebSocketCreatedUrl :: NetworkWebSocketCreatedUrl, -- ^ WebSocket request URL.
   networkWebSocketCreatedInitiator :: NetworkWebSocketCreatedInitiator -- ^ Request initiator.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Network.webSocketFrameError' event.
data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
   networkWebSocketFrameErrorRequestId :: NetworkWebSocketFrameErrorRequestId, -- ^ Request identifier.
   networkWebSocketFrameErrorTimestamp :: NetworkWebSocketFrameErrorTimestamp, -- ^ Timestamp.
   networkWebSocketFrameErrorErrorMessage :: NetworkWebSocketFrameErrorErrorMessage -- ^ WebSocket error message.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Network.webSocketFrameReceived' event.
data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
   networkWebSocketFrameReceivedRequestId :: NetworkWebSocketFrameReceivedRequestId, -- ^ Request identifier.
   networkWebSocketFrameReceivedTimestamp :: NetworkWebSocketFrameReceivedTimestamp, -- ^ Timestamp.
   networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrameReceivedResponse -- ^ WebSocket response data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.webSocketFrameSent' event.
data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
   networkWebSocketFrameSentRequestId :: NetworkWebSocketFrameSentRequestId, -- ^ Request identifier.
   networkWebSocketFrameSentTimestamp :: NetworkWebSocketFrameSentTimestamp, -- ^ Timestamp.
   networkWebSocketFrameSentResponse :: NetworkWebSocketFrameSentResponse -- ^ WebSocket response data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Network.webSocketHandshakeResponseReceived' event.
data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
   networkWebSocketHandshakeResponseReceivedRequestId :: NetworkWebSocketHandshakeResponseReceivedRequestId, -- ^ Request identifier.
   networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkWebSocketHandshakeResponseReceivedTimestamp, -- ^ Timestamp.
   networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketHandshakeResponseReceivedResponse -- ^ WebSocket response data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | Type of the 'Network.webSocketWillSendHandshakeRequest' event.
data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
   networkWebSocketWillSendHandshakeRequestRequestId :: NetworkWebSocketWillSendHandshakeRequestRequestId, -- ^ Request identifier.
   networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkWebSocketWillSendHandshakeRequestTimestamp, -- ^ Timestamp.
   networkWebSocketWillSendHandshakeRequestWallTime :: NetworkWebSocketWillSendHandshakeRequestWallTime, -- ^ UTC Timestamp.
   networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketWillSendHandshakeRequestRequest -- ^ WebSocket request data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.webTransportCreated' event.
data NetworkWebTransportCreated = NetworkWebTransportCreated {
   networkWebTransportCreatedTransportId :: NetworkWebTransportCreatedTransportId, -- ^ WebTransport identifier.
   networkWebTransportCreatedUrl :: NetworkWebTransportCreatedUrl, -- ^ WebTransport request URL.
   networkWebTransportCreatedTimestamp :: NetworkWebTransportCreatedTimestamp, -- ^ Timestamp.
   networkWebTransportCreatedInitiator :: NetworkWebTransportCreatedInitiator -- ^ Request initiator.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Network.webTransportConnectionEstablished' event.
data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
   networkWebTransportConnectionEstablishedTransportId :: NetworkWebTransportConnectionEstablishedTransportId, -- ^ WebTransport identifier.
   networkWebTransportConnectionEstablishedTimestamp :: NetworkWebTransportConnectionEstablishedTimestamp -- ^ Timestamp.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportConnectionEstablished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportConnectionEstablished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.webTransportClosed' event.
data NetworkWebTransportClosed = NetworkWebTransportClosed {
   networkWebTransportClosedTransportId :: NetworkWebTransportClosedTransportId, -- ^ WebTransport identifier.
   networkWebTransportClosedTimestamp :: NetworkWebTransportClosedTimestamp -- ^ Timestamp.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Network.requestWillBeSentExtraInfo' event.
data NetworkRequestWillBeSentExtraInfo = NetworkRequestWillBeSentExtraInfo {
   networkRequestWillBeSentExtraInfoRequestId :: NetworkRequestWillBeSentExtraInfoRequestId, -- ^ Request identifier. Used to match this information to an existing requestWillBeSent event.
   networkRequestWillBeSentExtraInfoAssociatedCookies :: NetworkRequestWillBeSentExtraInfoAssociatedCookies, -- ^ A list of cookies potentially associated to the requested URL. This includes both cookies sent with
the request and the ones not sent; the latter are distinguished by having blockedReason field set.
   networkRequestWillBeSentExtraInfoHeaders :: NetworkRequestWillBeSentExtraInfoHeaders, -- ^ Raw request headers as they will be sent over the wire.
   networkRequestWillBeSentExtraInfoConnectTiming :: NetworkRequestWillBeSentExtraInfoConnectTiming, -- ^ Connection timing information for the request.
   networkRequestWillBeSentExtraInfoClientSecurityState :: NetworkRequestWillBeSentExtraInfoClientSecurityState -- ^ The client security state set for the request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSentExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSentExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type of the 'Network.responseReceivedExtraInfo' event.
data NetworkResponseReceivedExtraInfo = NetworkResponseReceivedExtraInfo {
   networkResponseReceivedExtraInfoRequestId :: NetworkResponseReceivedExtraInfoRequestId, -- ^ Request identifier. Used to match this information to another responseReceived event.
   networkResponseReceivedExtraInfoBlockedCookies :: NetworkResponseReceivedExtraInfoBlockedCookies, -- ^ A list of cookies which were not stored from the response along with the corresponding
reasons for blocking. The cookies here may not be valid due to syntax errors, which
are represented by the invalid cookie line string instead of a proper cookie.
   networkResponseReceivedExtraInfoHeaders :: NetworkResponseReceivedExtraInfoHeaders, -- ^ Raw response headers as they were received over the wire.
   networkResponseReceivedExtraInfoResourceIpAddressSpace :: NetworkResponseReceivedExtraInfoResourceIpAddressSpace, -- ^ The IP address space of the resource. The address space can only be determined once the transport
established the connection, so we can't send it in `requestWillBeSentExtraInfo`.
   networkResponseReceivedExtraInfoStatusCode :: NetworkResponseReceivedExtraInfoStatusCode, -- ^ The status code of the response. This is useful in cases the request failed and no responseReceived
event is triggered, which is the case for, e.g., CORS errors. This is also the correct status code
for cached requests, where the status in responseReceived is a 200 and this will be 304.
   networkResponseReceivedExtraInfoHeadersText :: NetworkResponseReceivedExtraInfoHeadersText -- ^ Raw response header text as it was received over the wire. The raw text may not always be
available, such as in the case of HTTP/2 or QUIC.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceivedExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceivedExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type of the 'Network.trustTokenOperationDone' event.
data NetworkTrustTokenOperationDoneStatus = NetworkTrustTokenOperationDoneStatusOk | NetworkTrustTokenOperationDoneStatusInvalidArgument | NetworkTrustTokenOperationDoneStatusFailedPrecondition | NetworkTrustTokenOperationDoneStatusResourceExhausted | NetworkTrustTokenOperationDoneStatusAlreadyExists | NetworkTrustTokenOperationDoneStatusUnavailable | NetworkTrustTokenOperationDoneStatusBadResponse | NetworkTrustTokenOperationDoneStatusInternalError | NetworkTrustTokenOperationDoneStatusUnknownError | NetworkTrustTokenOperationDoneStatusFulfilledLocally
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkTrustTokenOperationDoneStatus where
   parseJSON = A.withText  "NetworkTrustTokenOperationDoneStatus"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse NetworkTrustTokenOperationDoneStatus"

instance ToJSON NetworkTrustTokenOperationDoneStatus where
   toJSON v = A.String $
      case v of
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



data NetworkTrustTokenOperationDone = NetworkTrustTokenOperationDone {
   networkTrustTokenOperationDoneStatus :: NetworkTrustTokenOperationDoneStatus, -- ^ Detailed success or error status of the operation.
'AlreadyExists' also signifies a successful operation, as the result
of the operation already exists und thus, the operation was abort
preemptively (e.g. a cache hit).


   networkTrustTokenOperationDoneTopLevelOrigin :: NetworkTrustTokenOperationDoneTopLevelOrigin, -- ^ Top level origin. The context in which the operation was attempted.
   networkTrustTokenOperationDoneIssuerOrigin :: NetworkTrustTokenOperationDoneIssuerOrigin, -- ^ Origin of the issuer in case of a "Issuance" or "Redemption" operation.
   networkTrustTokenOperationDoneIssuedTokenCount :: NetworkTrustTokenOperationDoneIssuedTokenCount -- ^ The number of obtained Trust Tokens on a successful "Issuance" operation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenOperationDone  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenOperationDone where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.subresourceWebBundleMetadataReceived' event.
data NetworkSubresourceWebBundleMetadataReceived = NetworkSubresourceWebBundleMetadataReceived {
   networkSubresourceWebBundleMetadataReceivedRequestId :: NetworkSubresourceWebBundleMetadataReceivedRequestId, -- ^ Request identifier. Used to match this information to another event.
   networkSubresourceWebBundleMetadataReceivedUrls :: NetworkSubresourceWebBundleMetadataReceivedUrls -- ^ A list of URLs of resources in the subresource Web Bundle.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }



-- | Type of the 'Network.subresourceWebBundleMetadataError' event.
data NetworkSubresourceWebBundleMetadataError = NetworkSubresourceWebBundleMetadataError {
   networkSubresourceWebBundleMetadataErrorRequestId :: NetworkSubresourceWebBundleMetadataErrorRequestId, -- ^ Request identifier. Used to match this information to another event.
   networkSubresourceWebBundleMetadataErrorErrorMessage :: NetworkSubresourceWebBundleMetadataErrorErrorMessage -- ^ Error message
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.subresourceWebBundleInnerResponseParsed' event.
data NetworkSubresourceWebBundleInnerResponseParsed = NetworkSubresourceWebBundleInnerResponseParsed {
   networkSubresourceWebBundleInnerResponseParsedInnerRequestId :: NetworkSubresourceWebBundleInnerResponseParsedInnerRequestId, -- ^ Request identifier of the subresource request
   networkSubresourceWebBundleInnerResponseParsedInnerRequestUrl :: NetworkSubresourceWebBundleInnerResponseParsedInnerRequestUrl, -- ^ URL of the subresource resource.
   networkSubresourceWebBundleInnerResponseParsedBundleRequestId :: NetworkSubresourceWebBundleInnerResponseParsedBundleRequestId -- ^ Bundle request identifier. Used to match this information to another event.
This made be absent in case when the instrumentation was enabled only
after webbundle was parsed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }



-- | Type of the 'Network.subresourceWebBundleInnerResponseError' event.
data NetworkSubresourceWebBundleInnerResponseError = NetworkSubresourceWebBundleInnerResponseError {
   networkSubresourceWebBundleInnerResponseErrorInnerRequestId :: NetworkSubresourceWebBundleInnerResponseErrorInnerRequestId, -- ^ Request identifier of the subresource request
   networkSubresourceWebBundleInnerResponseErrorInnerRequestUrl :: NetworkSubresourceWebBundleInnerResponseErrorInnerRequestUrl, -- ^ URL of the subresource resource.
   networkSubresourceWebBundleInnerResponseErrorErrorMessage :: NetworkSubresourceWebBundleInnerResponseErrorErrorMessage, -- ^ Error message
   networkSubresourceWebBundleInnerResponseErrorBundleRequestId :: NetworkSubresourceWebBundleInnerResponseErrorBundleRequestId -- ^ Bundle request identifier. Used to match this information to another event.
This made be absent in case when the instrumentation was enabled only
after webbundle was parsed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



-- | Type of the 'Network.reportingApiReportAdded' event.
data NetworkReportingApiReportAdded = NetworkReportingApiReportAdded {
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.reportingApiReportUpdated' event.
data NetworkReportingApiReportUpdated = NetworkReportingApiReportUpdated {
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type of the 'Network.reportingApiEndpointsChangedForOrigin' event.
data NetworkReportingApiEndpointsChangedForOrigin = NetworkReportingApiEndpointsChangedForOrigin {
   networkReportingApiEndpointsChangedForOriginOrigin :: NetworkReportingApiEndpointsChangedForOriginOrigin, -- ^ Origin of the document(s) which configured the endpoints.

} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpointsChangedForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpointsChangedForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }





-- | Parameters of the 'networkSetAcceptedEncodings' command.
data PNetworkSetAcceptedEncodings = PNetworkSetAcceptedEncodings {
   pNetworkSetAcceptedEncodingsEncodings :: PNetworkSetAcceptedEncodingsEncodings -- ^ List of accepted content encodings.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAcceptedEncodings  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAcceptedEncodings where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Network.setAcceptedEncodings'.
-- Sets a list of content encodings that will be accepted. Empty list means no encoding is accepted.
-- Parameters: 'PNetworkSetAcceptedEncodings'
networkSetAcceptedEncodings :: Handle ev -> PNetworkSetAcceptedEncodings -> IO (Maybe Error)
networkSetAcceptedEncodings handle params = sendReceiveCommand handle "Network.setAcceptedEncodings" (Just params)


-- | Function for the command 'Network.clearAcceptedEncodingsOverride'.
-- Clears accepted encodings set by setAcceptedEncodings
networkClearAcceptedEncodingsOverride :: Handle ev -> IO (Maybe Error)
networkClearAcceptedEncodingsOverride handle = sendReceiveCommand handle "Network.clearAcceptedEncodingsOverride" (Nothing :: Maybe ())


-- | Function for the command 'Network.clearBrowserCache'.
-- Clears browser cache.
networkClearBrowserCache :: Handle ev -> IO (Maybe Error)
networkClearBrowserCache handle = sendReceiveCommand handle "Network.clearBrowserCache" (Nothing :: Maybe ())


-- | Function for the command 'Network.clearBrowserCookies'.
-- Clears browser cookies.
networkClearBrowserCookies :: Handle ev -> IO (Maybe Error)
networkClearBrowserCookies handle = sendReceiveCommand handle "Network.clearBrowserCookies" (Nothing :: Maybe ())


-- | Parameters of the 'networkDeleteCookies' command.
data PNetworkDeleteCookies = PNetworkDeleteCookies {
   pNetworkDeleteCookiesName :: PNetworkDeleteCookiesName, -- ^ Name of the cookies to remove.
   pNetworkDeleteCookiesUrl :: PNetworkDeleteCookiesUrl, -- ^ If specified, deletes all the cookies with the given name where domain and path match
provided URL.
   pNetworkDeleteCookiesDomain :: PNetworkDeleteCookiesDomain, -- ^ If specified, deletes only cookies with the exact domain.
   pNetworkDeleteCookiesPath :: PNetworkDeleteCookiesPath -- ^ If specified, deletes only cookies with the exact path.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkDeleteCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PNetworkDeleteCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Network.deleteCookies'.
-- Deletes browser cookies with matching name and url or domain/path pair.
-- Parameters: 'PNetworkDeleteCookies'
networkDeleteCookies :: Handle ev -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies handle params = sendReceiveCommand handle "Network.deleteCookies" (Just params)


-- | Function for the command 'Network.disable'.
-- Disables network tracking, prevents network events from being sent to the client.
networkDisable :: Handle ev -> IO (Maybe Error)
networkDisable handle = sendReceiveCommand handle "Network.disable" (Nothing :: Maybe ())


-- | Parameters of the 'networkEmulateNetworkConditions' command.
data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
   pNetworkEmulateNetworkConditionsOffline :: PNetworkEmulateNetworkConditionsOffline, -- ^ True to emulate internet disconnection.
   pNetworkEmulateNetworkConditionsLatency :: PNetworkEmulateNetworkConditionsLatency, -- ^ Minimum latency from request sent to response headers received (ms).
   pNetworkEmulateNetworkConditionsDownloadThroughput :: PNetworkEmulateNetworkConditionsDownloadThroughput, -- ^ Maximal aggregated download throughput (bytes/sec). -1 disables download throttling.
   pNetworkEmulateNetworkConditionsUploadThroughput :: PNetworkEmulateNetworkConditionsUploadThroughput, -- ^ Maximal aggregated upload throughput (bytes/sec).  -1 disables upload throttling.
   pNetworkEmulateNetworkConditionsConnectionType :: PNetworkEmulateNetworkConditionsConnectionType -- ^ Connection type if known.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEmulateNetworkConditions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PNetworkEmulateNetworkConditions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'Network.emulateNetworkConditions'.
-- Activates emulation of network conditions.
-- Parameters: 'PNetworkEmulateNetworkConditions'
networkEmulateNetworkConditions :: Handle ev -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions handle params = sendReceiveCommand handle "Network.emulateNetworkConditions" (Just params)


-- | Parameters of the 'networkEnable' command.
data PNetworkEnable = PNetworkEnable {
   pNetworkEnableMaxTotalBufferSize :: PNetworkEnableMaxTotalBufferSize, -- ^ Buffer size in bytes to use when preserving network payloads (XHRs, etc).
   pNetworkEnableMaxResourceBufferSize :: PNetworkEnableMaxResourceBufferSize, -- ^ Per-resource buffer size in bytes to use when preserving network payloads (XHRs, etc).
   pNetworkEnableMaxPostDataSize :: PNetworkEnableMaxPostDataSize -- ^ Longest post body size (in bytes) that would be included in requestWillBeSent notification
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the command 'Network.enable'.
-- Enables network tracking, network events will now be delivered to the client.
-- Parameters: 'PNetworkEnable'
networkEnable :: Handle ev -> PNetworkEnable -> IO (Maybe Error)
networkEnable handle params = sendReceiveCommand handle "Network.enable" (Just params)


-- | Function for the command 'Network.getAllCookies'.
-- Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the `cookies` field.
-- Returns: 'NetworkGetAllCookies'
networkGetAllCookies :: Handle ev -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies handle = sendReceiveCommandResult handle "Network.getAllCookies" (Nothing :: Maybe ())

-- | Return type of the 'networkGetAllCookies' command.
data NetworkGetAllCookies = NetworkGetAllCookies {
   networkGetAllCookiesCookies :: [NetworkCookie] -- ^ Array of cookie objects.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetAllCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command NetworkGetAllCookies where
   commandName _ = "Network.getAllCookies"



-- | Parameters of the 'networkGetCertificate' command.
data PNetworkGetCertificate = PNetworkGetCertificate {
   pNetworkGetCertificateOrigin :: PNetworkGetCertificateOrigin -- ^ Origin to get certificate for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCertificate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Network.getCertificate'.
-- Returns the DER-encoded certificate.
-- Parameters: 'PNetworkGetCertificate'
-- Returns: 'NetworkGetCertificate'
networkGetCertificate :: Handle ev -> PNetworkGetCertificate -> IO (Either Error NetworkGetCertificate)
networkGetCertificate handle params = sendReceiveCommandResult handle "Network.getCertificate" (Just params)

-- | Return type of the 'networkGetCertificate' command.
data NetworkGetCertificate = NetworkGetCertificate {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command NetworkGetCertificate where
   commandName _ = "Network.getCertificate"



-- | Parameters of the 'networkGetCookies' command.
data PNetworkGetCookies = PNetworkGetCookies {
   pNetworkGetCookiesUrls :: PNetworkGetCookiesUrls -- ^ The list of URLs for which applicable cookies will be fetched.
If not specified, it's assumed to be set to the list containing
the URLs of the page and all of its subframes.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the command 'Network.getCookies'.
-- Returns all browser cookies for the current URL. Depending on the backend support, will return
-- detailed cookie information in the `cookies` field.
-- Parameters: 'PNetworkGetCookies'
-- Returns: 'NetworkGetCookies'
networkGetCookies :: Handle ev -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies handle params = sendReceiveCommandResult handle "Network.getCookies" (Just params)

-- | Return type of the 'networkGetCookies' command.
data NetworkGetCookies = NetworkGetCookies {
   networkGetCookiesCookies :: [NetworkCookie] -- ^ Array of cookie objects.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command NetworkGetCookies where
   commandName _ = "Network.getCookies"



-- | Parameters of the 'networkGetResponseBody' command.
data PNetworkGetResponseBody = PNetworkGetResponseBody {
   pNetworkGetResponseBodyRequestId :: PNetworkGetResponseBodyRequestId -- ^ Identifier of the network request to get content for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Network.getResponseBody'.
-- Returns content served for the given request.
-- Parameters: 'PNetworkGetResponseBody'
-- Returns: 'NetworkGetResponseBody'
networkGetResponseBody :: Handle ev -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody handle params = sendReceiveCommandResult handle "Network.getResponseBody" (Just params)

-- | Return type of the 'networkGetResponseBody' command.
data NetworkGetResponseBody = NetworkGetResponseBody {
   networkGetResponseBodyBody :: String, -- ^ Response body.
   networkGetResponseBodyBase64Encoded :: Bool -- ^ True, if content was sent as base64.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command NetworkGetResponseBody where
   commandName _ = "Network.getResponseBody"



-- | Parameters of the 'networkGetRequestPostData' command.
data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
   pNetworkGetRequestPostDataRequestId :: PNetworkGetRequestPostDataRequestId -- ^ Identifier of the network request to get content for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetRequestPostData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Network.getRequestPostData'.
-- Returns post data sent with the request. Returns an error when no data was sent with the request.
-- Parameters: 'PNetworkGetRequestPostData'
-- Returns: 'NetworkGetRequestPostData'
networkGetRequestPostData :: Handle ev -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData handle params = sendReceiveCommandResult handle "Network.getRequestPostData" (Just params)

-- | Return type of the 'networkGetRequestPostData' command.
data NetworkGetRequestPostData = NetworkGetRequestPostData {
   networkGetRequestPostDataPostData :: String -- ^ Request body string, omitting files from multipart requests
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command NetworkGetRequestPostData where
   commandName _ = "Network.getRequestPostData"



-- | Parameters of the 'networkGetResponseBodyForInterception' command.
data PNetworkGetResponseBodyForInterception = PNetworkGetResponseBodyForInterception {
   pNetworkGetResponseBodyForInterceptionInterceptionId :: PNetworkGetResponseBodyForInterceptionInterceptionId -- ^ Identifier for the intercepted request to get body for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBodyForInterception  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the command 'Network.getResponseBodyForInterception'.
-- Returns content served for the given currently intercepted request.
-- Parameters: 'PNetworkGetResponseBodyForInterception'
-- Returns: 'NetworkGetResponseBodyForInterception'
networkGetResponseBodyForInterception :: Handle ev -> PNetworkGetResponseBodyForInterception -> IO (Either Error NetworkGetResponseBodyForInterception)
networkGetResponseBodyForInterception handle params = sendReceiveCommandResult handle "Network.getResponseBodyForInterception" (Just params)

-- | Return type of the 'networkGetResponseBodyForInterception' command.
data NetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception {
   networkGetResponseBodyForInterceptionBody :: String, -- ^ Response body.
   networkGetResponseBodyForInterceptionBase64Encoded :: Bool -- ^ True, if content was sent as base64.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command NetworkGetResponseBodyForInterception where
   commandName _ = "Network.getResponseBodyForInterception"



-- | Parameters of the 'networkTakeResponseBodyForInterceptionAsStream' command.
data PNetworkTakeResponseBodyForInterceptionAsStream = PNetworkTakeResponseBodyForInterceptionAsStream {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkTakeResponseBodyForInterceptionAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  PNetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }


-- | Function for the command 'Network.takeResponseBodyForInterceptionAsStream'.
-- Returns a handle to the stream representing the response body. Note that after this command,
-- the intercepted request can't be continued as is -- you either need to cancel it or to provide
-- the response body. The stream only supports sequential read, IO.read will fail if the position
-- is specified.
-- Parameters: 'PNetworkTakeResponseBodyForInterceptionAsStream'
-- Returns: 'NetworkTakeResponseBodyForInterceptionAsStream'
networkTakeResponseBodyForInterceptionAsStream :: Handle ev -> PNetworkTakeResponseBodyForInterceptionAsStream -> IO (Either Error NetworkTakeResponseBodyForInterceptionAsStream)
networkTakeResponseBodyForInterceptionAsStream handle params = sendReceiveCommandResult handle "Network.takeResponseBodyForInterceptionAsStream" (Just params)

-- | Return type of the 'networkTakeResponseBodyForInterceptionAsStream' command.
data NetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }

instance Command NetworkTakeResponseBodyForInterceptionAsStream where
   commandName _ = "Network.takeResponseBodyForInterceptionAsStream"



-- | Parameters of the 'networkReplayXhr' command.
data PNetworkReplayXhr = PNetworkReplayXhr {
   pNetworkReplayXhrRequestId :: PNetworkReplayXhrRequestId -- ^ Identifier of XHR to replay.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkReplayXhr  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkReplayXhr where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Network.replayXHR'.
-- This method sends a new XMLHttpRequest which is identical to the original one. The following
-- parameters should be identical: method, url, async, request body, extra headers, withCredentials
-- attribute, user, password.
-- Parameters: 'PNetworkReplayXhr'
networkReplayXhr :: Handle ev -> PNetworkReplayXhr -> IO (Maybe Error)
networkReplayXhr handle params = sendReceiveCommand handle "Network.replayXHR" (Just params)


-- | Parameters of the 'networkSearchInResponseBody' command.
data PNetworkSearchInResponseBody = PNetworkSearchInResponseBody {
   pNetworkSearchInResponseBodyRequestId :: PNetworkSearchInResponseBodyRequestId, -- ^ Identifier of the network response to search.
   pNetworkSearchInResponseBodyQuery :: PNetworkSearchInResponseBodyQuery, -- ^ String to search for.
   pNetworkSearchInResponseBodyCaseSensitive :: PNetworkSearchInResponseBodyCaseSensitive, -- ^ If true, search is case sensitive.
   pNetworkSearchInResponseBodyIsRegex :: PNetworkSearchInResponseBodyIsRegex -- ^ If true, treats string parameter as regex.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSearchInResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Network.searchInResponseBody'.
-- Searches for given string in response content.
-- Parameters: 'PNetworkSearchInResponseBody'
-- Returns: 'NetworkSearchInResponseBody'
networkSearchInResponseBody :: Handle ev -> PNetworkSearchInResponseBody -> IO (Either Error NetworkSearchInResponseBody)
networkSearchInResponseBody handle params = sendReceiveCommandResult handle "Network.searchInResponseBody" (Just params)

-- | Return type of the 'networkSearchInResponseBody' command.
data NetworkSearchInResponseBody = NetworkSearchInResponseBody {
   networkSearchInResponseBodyResult :: [Debugger.DebuggerSearchMatch] -- ^ List of search matches.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command NetworkSearchInResponseBody where
   commandName _ = "Network.searchInResponseBody"



-- | Parameters of the 'networkSetBlockedUrLs' command.
data PNetworkSetBlockedUrLs = PNetworkSetBlockedUrLs {
   pNetworkSetBlockedUrLsUrls :: PNetworkSetBlockedUrLsUrls -- ^ URL patterns to block. Wildcards ('*') are allowed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBlockedUrLs  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBlockedUrLs where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Network.setBlockedURLs'.
-- Blocks URLs from loading.
-- Parameters: 'PNetworkSetBlockedUrLs'
networkSetBlockedUrLs :: Handle ev -> PNetworkSetBlockedUrLs -> IO (Maybe Error)
networkSetBlockedUrLs handle params = sendReceiveCommand handle "Network.setBlockedURLs" (Just params)


-- | Parameters of the 'networkSetBypassServiceWorker' command.
data PNetworkSetBypassServiceWorker = PNetworkSetBypassServiceWorker {
   pNetworkSetBypassServiceWorkerBypass :: PNetworkSetBypassServiceWorkerBypass -- ^ Bypass service worker and load from network.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBypassServiceWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBypassServiceWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Network.setBypassServiceWorker'.
-- Toggles ignoring of service worker for each request.
-- Parameters: 'PNetworkSetBypassServiceWorker'
networkSetBypassServiceWorker :: Handle ev -> PNetworkSetBypassServiceWorker -> IO (Maybe Error)
networkSetBypassServiceWorker handle params = sendReceiveCommand handle "Network.setBypassServiceWorker" (Just params)


-- | Parameters of the 'networkSetCacheDisabled' command.
data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
   pNetworkSetCacheDisabledCacheDisabled :: PNetworkSetCacheDisabledCacheDisabled -- ^ Cache disabled state.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCacheDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCacheDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Network.setCacheDisabled'.
-- Toggles ignoring cache for each request. If `true`, cache will not be used.
-- Parameters: 'PNetworkSetCacheDisabled'
networkSetCacheDisabled :: Handle ev -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled handle params = sendReceiveCommand handle "Network.setCacheDisabled" (Just params)


-- | Parameters of the 'networkSetCookie' command.
data PNetworkSetCookie = PNetworkSetCookie {
   pNetworkSetCookieName :: PNetworkSetCookieName, -- ^ Cookie name.
   pNetworkSetCookieValue :: PNetworkSetCookieValue, -- ^ Cookie value.
   pNetworkSetCookieUrl :: PNetworkSetCookieUrl, -- ^ The request-URI to associate with the setting of the cookie. This value can affect the
default domain, path, source port, and source scheme values of the created cookie.
   pNetworkSetCookieDomain :: PNetworkSetCookieDomain, -- ^ Cookie domain.
   pNetworkSetCookiePath :: PNetworkSetCookiePath, -- ^ Cookie path.
   pNetworkSetCookieSecure :: PNetworkSetCookieSecure, -- ^ True if cookie is secure.
   pNetworkSetCookieHttpOnly :: PNetworkSetCookieHttpOnly, -- ^ True if cookie is http-only.
   pNetworkSetCookieSameSite :: PNetworkSetCookieSameSite, -- ^ Cookie SameSite type.
   pNetworkSetCookieExpires :: PNetworkSetCookieExpires, -- ^ Cookie expiration date, session cookie if not set
   pNetworkSetCookiePriority :: PNetworkSetCookiePriority, -- ^ Cookie Priority type.
   pNetworkSetCookieSameParty :: PNetworkSetCookieSameParty, -- ^ True if cookie is SameParty.
   pNetworkSetCookieSourceScheme :: PNetworkSetCookieSourceScheme, -- ^ Cookie source scheme type.
   pNetworkSetCookieSourcePort :: PNetworkSetCookieSourcePort, -- ^ Cookie source port. Valid values are {-1, [1, 65535]}, -1 indicates an unspecified port.
An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
This is a temporary ability and it will be removed in the future.
   pNetworkSetCookiePartitionKey :: PNetworkSetCookiePartitionKey -- ^ Cookie partition key. The site of the top-level URL the browser was visiting at the start
of the request to the endpoint that set the cookie.
If not set, the cookie will be set as not partitioned.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Network.setCookie'.
-- Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.
-- Parameters: 'PNetworkSetCookie'
networkSetCookie :: Handle ev -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie handle params = sendReceiveCommand handle "Network.setCookie" (Just params)


-- | Parameters of the 'networkSetCookies' command.
data PNetworkSetCookies = PNetworkSetCookies {
   pNetworkSetCookiesCookies :: PNetworkSetCookiesCookies -- ^ Cookies to be set.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the command 'Network.setCookies'.
-- Sets given cookies.
-- Parameters: 'PNetworkSetCookies'
networkSetCookies :: Handle ev -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies handle params = sendReceiveCommand handle "Network.setCookies" (Just params)


-- | Parameters of the 'networkSetExtraHttpHeaders' command.
data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
   pNetworkSetExtraHttpHeadersHeaders :: PNetworkSetExtraHttpHeadersHeaders -- ^ Map with extra HTTP headers.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetExtraHttpHeaders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetExtraHttpHeaders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Network.setExtraHTTPHeaders'.
-- Specifies whether to always send extra HTTP headers with the requests from this page.
-- Parameters: 'PNetworkSetExtraHttpHeaders'
networkSetExtraHttpHeaders :: Handle ev -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders handle params = sendReceiveCommand handle "Network.setExtraHTTPHeaders" (Just params)


-- | Parameters of the 'networkSetAttachDebugStack' command.
data PNetworkSetAttachDebugStack = PNetworkSetAttachDebugStack {
   pNetworkSetAttachDebugStackEnabled :: PNetworkSetAttachDebugStackEnabled -- ^ Whether to attach a page script stack for debugging purpose.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAttachDebugStack  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAttachDebugStack where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Network.setAttachDebugStack'.
-- Specifies whether to attach a page script stack id in requests
-- Parameters: 'PNetworkSetAttachDebugStack'
networkSetAttachDebugStack :: Handle ev -> PNetworkSetAttachDebugStack -> IO (Maybe Error)
networkSetAttachDebugStack handle params = sendReceiveCommand handle "Network.setAttachDebugStack" (Just params)


-- | Parameters of the 'networkSetUserAgentOverride' command.
data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
   pNetworkSetUserAgentOverrideUserAgent :: PNetworkSetUserAgentOverrideUserAgent, -- ^ User agent to use.
   pNetworkSetUserAgentOverrideAcceptLanguage :: PNetworkSetUserAgentOverrideAcceptLanguage, -- ^ Browser langugage to emulate.
   pNetworkSetUserAgentOverridePlatform :: PNetworkSetUserAgentOverridePlatform, -- ^ The platform navigator.platform should return.
   pNetworkSetUserAgentOverrideUserAgentMetadata :: PNetworkSetUserAgentOverrideUserAgentMetadata -- ^ To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Network.setUserAgentOverride'.
-- Allows overriding user agent with the given string.
-- Parameters: 'PNetworkSetUserAgentOverride'
networkSetUserAgentOverride :: Handle ev -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)


-- | Parameters of the 'networkGetSecurityIsolationStatus' command.
data PNetworkGetSecurityIsolationStatus = PNetworkGetSecurityIsolationStatus {
   pNetworkGetSecurityIsolationStatusFrameId :: PNetworkGetSecurityIsolationStatusFrameId -- ^ If no frameId is provided, the status of the target is provided.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Network.getSecurityIsolationStatus'.
-- Returns information about the COEP/COOP isolation status.
-- Parameters: 'PNetworkGetSecurityIsolationStatus'
-- Returns: 'NetworkGetSecurityIsolationStatus'
networkGetSecurityIsolationStatus :: Handle ev -> PNetworkGetSecurityIsolationStatus -> IO (Either Error NetworkGetSecurityIsolationStatus)
networkGetSecurityIsolationStatus handle params = sendReceiveCommandResult handle "Network.getSecurityIsolationStatus" (Just params)

-- | Return type of the 'networkGetSecurityIsolationStatus' command.
data NetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command NetworkGetSecurityIsolationStatus where
   commandName _ = "Network.getSecurityIsolationStatus"



-- | Parameters of the 'networkEnableReportingApi' command.
data PNetworkEnableReportingApi = PNetworkEnableReportingApi {
   pNetworkEnableReportingApiEnable :: PNetworkEnableReportingApiEnable -- ^ Whether to enable or disable events for the Reporting API
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnableReportingApi  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnableReportingApi where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Network.enableReportingApi'.
-- Enables tracking for the Reporting API, events generated by the Reporting API will now be delivered to the client.
-- Enabling triggers 'reportingApiReportAdded' for all existing reports.
-- Parameters: 'PNetworkEnableReportingApi'
networkEnableReportingApi :: Handle ev -> PNetworkEnableReportingApi -> IO (Maybe Error)
networkEnableReportingApi handle params = sendReceiveCommand handle "Network.enableReportingApi" (Just params)


-- | Parameters of the 'networkLoadNetworkResource' command.
data PNetworkLoadNetworkResource = PNetworkLoadNetworkResource {
   pNetworkLoadNetworkResourceFrameId :: PNetworkLoadNetworkResourceFrameId, -- ^ Frame id to get the resource for. Mandatory for frame targets, and
should be omitted for worker targets.
   pNetworkLoadNetworkResourceUrl :: PNetworkLoadNetworkResourceUrl, -- ^ URL of the resource to get content for.
   pNetworkLoadNetworkResourceOptions :: PNetworkLoadNetworkResourceOptions -- ^ Options for the request.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkLoadNetworkResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Network.loadNetworkResource'.
-- Fetches the resource and returns the content.
-- Parameters: 'PNetworkLoadNetworkResource'
-- Returns: 'NetworkLoadNetworkResource'
networkLoadNetworkResource :: Handle ev -> PNetworkLoadNetworkResource -> IO (Either Error NetworkLoadNetworkResource)
networkLoadNetworkResource handle params = sendReceiveCommandResult handle "Network.loadNetworkResource" (Just params)

-- | Return type of the 'networkLoadNetworkResource' command.
data NetworkLoadNetworkResource = NetworkLoadNetworkResource {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command NetworkLoadNetworkResource where
   commandName _ = "Network.loadNetworkResource"




-- | Unique frame identifier.
type PageFrameId = String

-- | Indicates whether a frame has been identified as an ad.
data PageAdFrameType = PageAdFrameTypeNone | PageAdFrameTypeChild | PageAdFrameTypeRoot
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageAdFrameType where
   parseJSON = A.withText  "PageAdFrameType"  $ \v -> do
      case v of
         "none" -> pure PageAdFrameTypeNone
         "child" -> pure PageAdFrameTypeChild
         "root" -> pure PageAdFrameTypeRoot
         _ -> fail "failed to parse PageAdFrameType"

instance ToJSON PageAdFrameType where
   toJSON v = A.String $
      case v of
         PageAdFrameTypeNone -> "none"
         PageAdFrameTypeChild -> "child"
         PageAdFrameTypeRoot -> "root"



-- | Type 'Page.AdFrameExplanation' .
data PageAdFrameExplanation = PageAdFrameExplanationParentIsAd | PageAdFrameExplanationCreatedByAdScript | PageAdFrameExplanationMatchedBlockingRule
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageAdFrameExplanation where
   parseJSON = A.withText  "PageAdFrameExplanation"  $ \v -> do
      case v of
         "ParentIsAd" -> pure PageAdFrameExplanationParentIsAd
         "CreatedByAdScript" -> pure PageAdFrameExplanationCreatedByAdScript
         "MatchedBlockingRule" -> pure PageAdFrameExplanationMatchedBlockingRule
         _ -> fail "failed to parse PageAdFrameExplanation"

instance ToJSON PageAdFrameExplanation where
   toJSON v = A.String $
      case v of
         PageAdFrameExplanationParentIsAd -> "ParentIsAd"
         PageAdFrameExplanationCreatedByAdScript -> "CreatedByAdScript"
         PageAdFrameExplanationMatchedBlockingRule -> "MatchedBlockingRule"



-- | Indicates whether a frame has been identified as an ad and why.
data PageAdFrameStatus = PageAdFrameStatus {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAdFrameStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageAdFrameStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Indicates whether the frame is a secure context and why it is the case.
data PageSecureContextType = PageSecureContextTypeSecure | PageSecureContextTypeSecureLocalhost | PageSecureContextTypeInsecureScheme | PageSecureContextTypeInsecureAncestor
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageSecureContextType where
   parseJSON = A.withText  "PageSecureContextType"  $ \v -> do
      case v of
         "Secure" -> pure PageSecureContextTypeSecure
         "SecureLocalhost" -> pure PageSecureContextTypeSecureLocalhost
         "InsecureScheme" -> pure PageSecureContextTypeInsecureScheme
         "InsecureAncestor" -> pure PageSecureContextTypeInsecureAncestor
         _ -> fail "failed to parse PageSecureContextType"

instance ToJSON PageSecureContextType where
   toJSON v = A.String $
      case v of
         PageSecureContextTypeSecure -> "Secure"
         PageSecureContextTypeSecureLocalhost -> "SecureLocalhost"
         PageSecureContextTypeInsecureScheme -> "InsecureScheme"
         PageSecureContextTypeInsecureAncestor -> "InsecureAncestor"



-- | Indicates whether the frame is cross-origin isolated and why it is the case.
data PageCrossOriginIsolatedContextType = PageCrossOriginIsolatedContextTypeIsolated | PageCrossOriginIsolatedContextTypeNotIsolated | PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageCrossOriginIsolatedContextType where
   parseJSON = A.withText  "PageCrossOriginIsolatedContextType"  $ \v -> do
      case v of
         "Isolated" -> pure PageCrossOriginIsolatedContextTypeIsolated
         "NotIsolated" -> pure PageCrossOriginIsolatedContextTypeNotIsolated
         "NotIsolatedFeatureDisabled" -> pure PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled
         _ -> fail "failed to parse PageCrossOriginIsolatedContextType"

instance ToJSON PageCrossOriginIsolatedContextType where
   toJSON v = A.String $
      case v of
         PageCrossOriginIsolatedContextTypeIsolated -> "Isolated"
         PageCrossOriginIsolatedContextTypeNotIsolated -> "NotIsolated"
         PageCrossOriginIsolatedContextTypeNotIsolatedFeatureDisabled -> "NotIsolatedFeatureDisabled"



-- | Type 'Page.GatedAPIFeatures' .
data PageGatedApiFeatures = PageGatedApiFeaturesSharedArrayBuffers | PageGatedApiFeaturesSharedArrayBuffersTransferAllowed | PageGatedApiFeaturesPerformanceMeasureMemory | PageGatedApiFeaturesPerformanceProfile
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageGatedApiFeatures where
   parseJSON = A.withText  "PageGatedApiFeatures"  $ \v -> do
      case v of
         "SharedArrayBuffers" -> pure PageGatedApiFeaturesSharedArrayBuffers
         "SharedArrayBuffersTransferAllowed" -> pure PageGatedApiFeaturesSharedArrayBuffersTransferAllowed
         "PerformanceMeasureMemory" -> pure PageGatedApiFeaturesPerformanceMeasureMemory
         "PerformanceProfile" -> pure PageGatedApiFeaturesPerformanceProfile
         _ -> fail "failed to parse PageGatedApiFeatures"

instance ToJSON PageGatedApiFeatures where
   toJSON v = A.String $
      case v of
         PageGatedApiFeaturesSharedArrayBuffers -> "SharedArrayBuffers"
         PageGatedApiFeaturesSharedArrayBuffersTransferAllowed -> "SharedArrayBuffersTransferAllowed"
         PageGatedApiFeaturesPerformanceMeasureMemory -> "PerformanceMeasureMemory"
         PageGatedApiFeaturesPerformanceProfile -> "PerformanceProfile"



-- | All Permissions Policy features. This enum should match the one defined
-- in third_party/blink/renderer/core/permissions_policy/permissions_policy_features.json5.
data PagePermissionsPolicyFeature = PagePermissionsPolicyFeatureAccelerometer | PagePermissionsPolicyFeatureAmbientLightSensor | PagePermissionsPolicyFeatureAttributionReporting | PagePermissionsPolicyFeatureAutoplay | PagePermissionsPolicyFeatureBluetooth | PagePermissionsPolicyFeatureBrowsingTopics | PagePermissionsPolicyFeatureCamera | PagePermissionsPolicyFeatureChDpr | PagePermissionsPolicyFeatureChDeviceMemory | PagePermissionsPolicyFeatureChDownlink | PagePermissionsPolicyFeatureChEct | PagePermissionsPolicyFeatureChPrefersColorScheme | PagePermissionsPolicyFeatureChRtt | PagePermissionsPolicyFeatureChSaveData | PagePermissionsPolicyFeatureChUa | PagePermissionsPolicyFeatureChUaArch | PagePermissionsPolicyFeatureChUaBitness | PagePermissionsPolicyFeatureChUaPlatform | PagePermissionsPolicyFeatureChUaModel | PagePermissionsPolicyFeatureChUaMobile | PagePermissionsPolicyFeatureChUaFull | PagePermissionsPolicyFeatureChUaFullVersion | PagePermissionsPolicyFeatureChUaFullVersionList | PagePermissionsPolicyFeatureChUaPlatformVersion | PagePermissionsPolicyFeatureChUaReduced | PagePermissionsPolicyFeatureChUaWow64 | PagePermissionsPolicyFeatureChViewportHeight | PagePermissionsPolicyFeatureChViewportWidth | PagePermissionsPolicyFeatureChWidth | PagePermissionsPolicyFeatureClipboardRead | PagePermissionsPolicyFeatureClipboardWrite | PagePermissionsPolicyFeatureCrossOriginIsolated | PagePermissionsPolicyFeatureDirectSockets | PagePermissionsPolicyFeatureDisplayCapture | PagePermissionsPolicyFeatureDocumentDomain | PagePermissionsPolicyFeatureEncryptedMedia | PagePermissionsPolicyFeatureExecutionWhileOutOfViewport | PagePermissionsPolicyFeatureExecutionWhileNotRendered | PagePermissionsPolicyFeatureFocusWithoutUserActivation | PagePermissionsPolicyFeatureFullscreen | PagePermissionsPolicyFeatureFrobulate | PagePermissionsPolicyFeatureGamepad | PagePermissionsPolicyFeatureGeolocation | PagePermissionsPolicyFeatureGyroscope | PagePermissionsPolicyFeatureHid | PagePermissionsPolicyFeatureIdleDetection | PagePermissionsPolicyFeatureInterestCohort | PagePermissionsPolicyFeatureJoinAdInterestGroup | PagePermissionsPolicyFeatureKeyboardMap | PagePermissionsPolicyFeatureLocalFonts | PagePermissionsPolicyFeatureMagnetometer | PagePermissionsPolicyFeatureMicrophone | PagePermissionsPolicyFeatureMidi | PagePermissionsPolicyFeatureOtpCredentials | PagePermissionsPolicyFeaturePayment | PagePermissionsPolicyFeaturePictureInPicture | PagePermissionsPolicyFeaturePublickeyCredentialsGet | PagePermissionsPolicyFeatureRunAdAuction | PagePermissionsPolicyFeatureScreenWakeLock | PagePermissionsPolicyFeatureSerial | PagePermissionsPolicyFeatureSharedAutofill | PagePermissionsPolicyFeatureStorageAccessApi | PagePermissionsPolicyFeatureSyncXhr | PagePermissionsPolicyFeatureTrustTokenRedemption | PagePermissionsPolicyFeatureUsb | PagePermissionsPolicyFeatureVerticalScroll | PagePermissionsPolicyFeatureWebShare | PagePermissionsPolicyFeatureWindowPlacement | PagePermissionsPolicyFeatureXrSpatialTracking
   deriving (Ord, Eq, Show, Read)
instance FromJSON PagePermissionsPolicyFeature where
   parseJSON = A.withText  "PagePermissionsPolicyFeature"  $ \v -> do
      case v of
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
         "storage-access-api" -> pure PagePermissionsPolicyFeatureStorageAccessApi
         "sync-xhr" -> pure PagePermissionsPolicyFeatureSyncXhr
         "trust-token-redemption" -> pure PagePermissionsPolicyFeatureTrustTokenRedemption
         "usb" -> pure PagePermissionsPolicyFeatureUsb
         "vertical-scroll" -> pure PagePermissionsPolicyFeatureVerticalScroll
         "web-share" -> pure PagePermissionsPolicyFeatureWebShare
         "window-placement" -> pure PagePermissionsPolicyFeatureWindowPlacement
         "xr-spatial-tracking" -> pure PagePermissionsPolicyFeatureXrSpatialTracking
         _ -> fail "failed to parse PagePermissionsPolicyFeature"

instance ToJSON PagePermissionsPolicyFeature where
   toJSON v = A.String $
      case v of
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
         PagePermissionsPolicyFeatureStorageAccessApi -> "storage-access-api"
         PagePermissionsPolicyFeatureSyncXhr -> "sync-xhr"
         PagePermissionsPolicyFeatureTrustTokenRedemption -> "trust-token-redemption"
         PagePermissionsPolicyFeatureUsb -> "usb"
         PagePermissionsPolicyFeatureVerticalScroll -> "vertical-scroll"
         PagePermissionsPolicyFeatureWebShare -> "web-share"
         PagePermissionsPolicyFeatureWindowPlacement -> "window-placement"
         PagePermissionsPolicyFeatureXrSpatialTracking -> "xr-spatial-tracking"



-- | Reason for a permissions policy feature to be disabled.
data PagePermissionsPolicyBlockReason = PagePermissionsPolicyBlockReasonHeader | PagePermissionsPolicyBlockReasonIframeAttribute | PagePermissionsPolicyBlockReasonInFencedFrameTree
   deriving (Ord, Eq, Show, Read)
instance FromJSON PagePermissionsPolicyBlockReason where
   parseJSON = A.withText  "PagePermissionsPolicyBlockReason"  $ \v -> do
      case v of
         "Header" -> pure PagePermissionsPolicyBlockReasonHeader
         "IframeAttribute" -> pure PagePermissionsPolicyBlockReasonIframeAttribute
         "InFencedFrameTree" -> pure PagePermissionsPolicyBlockReasonInFencedFrameTree
         _ -> fail "failed to parse PagePermissionsPolicyBlockReason"

instance ToJSON PagePermissionsPolicyBlockReason where
   toJSON v = A.String $
      case v of
         PagePermissionsPolicyBlockReasonHeader -> "Header"
         PagePermissionsPolicyBlockReasonIframeAttribute -> "IframeAttribute"
         PagePermissionsPolicyBlockReasonInFencedFrameTree -> "InFencedFrameTree"



-- | Type 'Page.PermissionsPolicyBlockLocator' .
data PagePermissionsPolicyBlockLocator = PagePermissionsPolicyBlockLocator {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyBlockLocator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyBlockLocator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Page.PermissionsPolicyFeatureState' .
data PagePermissionsPolicyFeatureState = PagePermissionsPolicyFeatureState {



} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyFeatureState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyFeatureState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Origin Trial(https://www.chromium.org/blink/origin-trials) support.
-- Status for an Origin Trial token.
data PageOriginTrialTokenStatus = PageOriginTrialTokenStatusSuccess | PageOriginTrialTokenStatusNotSupported | PageOriginTrialTokenStatusInsecure | PageOriginTrialTokenStatusExpired | PageOriginTrialTokenStatusWrongOrigin | PageOriginTrialTokenStatusInvalidSignature | PageOriginTrialTokenStatusMalformed | PageOriginTrialTokenStatusWrongVersion | PageOriginTrialTokenStatusFeatureDisabled | PageOriginTrialTokenStatusTokenDisabled | PageOriginTrialTokenStatusFeatureDisabledForUser | PageOriginTrialTokenStatusUnknownTrial
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialTokenStatus where
   parseJSON = A.withText  "PageOriginTrialTokenStatus"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse PageOriginTrialTokenStatus"

instance ToJSON PageOriginTrialTokenStatus where
   toJSON v = A.String $
      case v of
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



-- | Status for an Origin Trial.
data PageOriginTrialStatus = PageOriginTrialStatusEnabled | PageOriginTrialStatusValidTokenNotProvided | PageOriginTrialStatusOsNotSupported | PageOriginTrialStatusTrialNotAllowed
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialStatus where
   parseJSON = A.withText  "PageOriginTrialStatus"  $ \v -> do
      case v of
         "Enabled" -> pure PageOriginTrialStatusEnabled
         "ValidTokenNotProvided" -> pure PageOriginTrialStatusValidTokenNotProvided
         "OSNotSupported" -> pure PageOriginTrialStatusOsNotSupported
         "TrialNotAllowed" -> pure PageOriginTrialStatusTrialNotAllowed
         _ -> fail "failed to parse PageOriginTrialStatus"

instance ToJSON PageOriginTrialStatus where
   toJSON v = A.String $
      case v of
         PageOriginTrialStatusEnabled -> "Enabled"
         PageOriginTrialStatusValidTokenNotProvided -> "ValidTokenNotProvided"
         PageOriginTrialStatusOsNotSupported -> "OSNotSupported"
         PageOriginTrialStatusTrialNotAllowed -> "TrialNotAllowed"



-- | Type 'Page.OriginTrialUsageRestriction' .
data PageOriginTrialUsageRestriction = PageOriginTrialUsageRestrictionNone | PageOriginTrialUsageRestrictionSubset
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialUsageRestriction where
   parseJSON = A.withText  "PageOriginTrialUsageRestriction"  $ \v -> do
      case v of
         "None" -> pure PageOriginTrialUsageRestrictionNone
         "Subset" -> pure PageOriginTrialUsageRestrictionSubset
         _ -> fail "failed to parse PageOriginTrialUsageRestriction"

instance ToJSON PageOriginTrialUsageRestriction where
   toJSON v = A.String $
      case v of
         PageOriginTrialUsageRestrictionNone -> "None"
         PageOriginTrialUsageRestrictionSubset -> "Subset"



-- | Type 'Page.OriginTrialToken' .
data PageOriginTrialToken = PageOriginTrialToken {






} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialToken  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialToken where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Page.OriginTrialTokenWithStatus' .
data PageOriginTrialTokenWithStatus = PageOriginTrialTokenWithStatus {

   pageOriginTrialTokenWithStatusParsedToken :: PageOriginTrialTokenWithStatusParsedToken, -- ^ `parsedToken` is present only when the token is extractable and
parsable.

} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialTokenWithStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialTokenWithStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Page.OriginTrial' .
data PageOriginTrial = PageOriginTrial {



} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrial  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrial where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Information about the Frame on the page.
data PageFrame = PageFrame {
   pageFrameId :: PageFrameId, -- ^ Frame unique identifier.
   pageFrameParentId :: PageFrameParentId, -- ^ Parent frame identifier.
   pageFrameLoaderId :: PageFrameLoaderId, -- ^ Identifier of the loader associated with this frame.
   pageFrameName :: PageFrameName, -- ^ Frame's name as specified in the tag.
   pageFrameUrl :: PageFrameUrl, -- ^ Frame document's URL without fragment.
   pageFrameUrlFragment :: PageFrameUrlFragment, -- ^ Frame document's URL fragment including the '#'.
   pageFrameDomainAndRegistry :: PageFrameDomainAndRegistry, -- ^ Frame document's registered domain, taking the public suffixes list into account.
Extracted from the Frame's url.
Example URLs: http://www.google.com/file.html -> "google.com"
              http://a.b.co.uk/file.html      -> "b.co.uk"
   pageFrameSecurityOrigin :: PageFrameSecurityOrigin, -- ^ Frame document's security origin.
   pageFrameMimeType :: PageFrameMimeType, -- ^ Frame document's mimeType as determined by the browser.
   pageFrameUnreachableUrl :: PageFrameUnreachableUrl, -- ^ If the frame failed to load, this contains the URL that could not be loaded. Note that unlike url above, this URL may contain a fragment.
   pageFrameAdFrameStatus :: PageFrameAdFrameStatus, -- ^ Indicates whether this frame was tagged as an ad and why.
   pageFrameSecureContextType :: PageFrameSecureContextType, -- ^ Indicates whether the main document is a secure context and explains why that is the case.
   pageFrameCrossOriginIsolatedContextType :: PageFrameCrossOriginIsolatedContextType, -- ^ Indicates whether this is a cross origin isolated context.
   pageFrameGatedApiFeatures :: PageFrameGatedApiFeatures -- ^ Indicated which gated APIs / features are available.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



-- | Information about the Resource on the page.
data PageFrameResource = PageFrameResource {
   pageFrameResourceUrl :: PageFrameResourceUrl, -- ^ Resource URL.
   pageFrameResourceType :: PageFrameResourceType, -- ^ Type of this resource.
   pageFrameResourceMimeType :: PageFrameResourceMimeType, -- ^ Resource mimeType as determined by the browser.
   pageFrameResourceLastModified :: PageFrameResourceLastModified, -- ^ last-modified timestamp as reported by server.
   pageFrameResourceContentSize :: PageFrameResourceContentSize, -- ^ Resource content size.
   pageFrameResourceFailed :: PageFrameResourceFailed, -- ^ True if the resource failed to load.
   pageFrameResourceCanceled :: PageFrameResourceCanceled -- ^ True if the resource was canceled during loading.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Information about the Frame hierarchy along with their cached resources.
data PageFrameResourceTree = PageFrameResourceTree {
   pageFrameResourceTreeFrame :: PageFrameResourceTreeFrame, -- ^ Frame information for this tree item.
   pageFrameResourceTreeChildFrames :: PageFrameResourceTreeChildFrames, -- ^ Child frames.
   pageFrameResourceTreeResources :: PageFrameResourceTreeResources -- ^ Information about frame resources.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResourceTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFrameResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about the Frame hierarchy.
data PageFrameTree = PageFrameTree {
   pageFrameTreeFrame :: PageFrameTreeFrame, -- ^ Frame information for this tree item.
   pageFrameTreeChildFrames :: PageFrameTreeChildFrames -- ^ Child frames.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PageFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Unique script identifier.
type PageScriptIdentifier = String

-- | Transition type.
data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddressBar | PageTransitionTypeAutoBookmark | PageTransitionTypeAutoSubframe | PageTransitionTypeManualSubframe | PageTransitionTypeGenerated | PageTransitionTypeAutoToplevel | PageTransitionTypeFormSubmit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeywordGenerated | PageTransitionTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageTransitionType where
   parseJSON = A.withText  "PageTransitionType"  $ \v -> do
      case v of
         "link" -> pure PageTransitionTypeLink
         "typed" -> pure PageTransitionTypeTyped
         "address_bar" -> pure PageTransitionTypeAddressBar
         "auto_bookmark" -> pure PageTransitionTypeAutoBookmark
         "auto_subframe" -> pure PageTransitionTypeAutoSubframe
         "manual_subframe" -> pure PageTransitionTypeManualSubframe
         "generated" -> pure PageTransitionTypeGenerated
         "auto_toplevel" -> pure PageTransitionTypeAutoToplevel
         "form_submit" -> pure PageTransitionTypeFormSubmit
         "reload" -> pure PageTransitionTypeReload
         "keyword" -> pure PageTransitionTypeKeyword
         "keyword_generated" -> pure PageTransitionTypeKeywordGenerated
         "other" -> pure PageTransitionTypeOther
         _ -> fail "failed to parse PageTransitionType"

instance ToJSON PageTransitionType where
   toJSON v = A.String $
      case v of
         PageTransitionTypeLink -> "link"
         PageTransitionTypeTyped -> "typed"
         PageTransitionTypeAddressBar -> "address_bar"
         PageTransitionTypeAutoBookmark -> "auto_bookmark"
         PageTransitionTypeAutoSubframe -> "auto_subframe"
         PageTransitionTypeManualSubframe -> "manual_subframe"
         PageTransitionTypeGenerated -> "generated"
         PageTransitionTypeAutoToplevel -> "auto_toplevel"
         PageTransitionTypeFormSubmit -> "form_submit"
         PageTransitionTypeReload -> "reload"
         PageTransitionTypeKeyword -> "keyword"
         PageTransitionTypeKeywordGenerated -> "keyword_generated"
         PageTransitionTypeOther -> "other"



-- | Navigation history entry.
data PageNavigationEntry = PageNavigationEntry {
   pageNavigationEntryId :: PageNavigationEntryId, -- ^ Unique id of the navigation history entry.
   pageNavigationEntryUrl :: PageNavigationEntryUrl, -- ^ URL of the navigation history entry.
   pageNavigationEntryUserTypedUrl :: PageNavigationEntryUserTypedUrl, -- ^ URL that the user typed in the url bar.
   pageNavigationEntryTitle :: PageNavigationEntryTitle, -- ^ Title of the navigation history entry.
   pageNavigationEntryTransitionType :: PageNavigationEntryTransitionType -- ^ Transition type.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigationEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageNavigationEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Screencast frame metadata.
data PageScreencastFrameMetadata = PageScreencastFrameMetadata {
   pageScreencastFrameMetadataOffsetTop :: PageScreencastFrameMetadataOffsetTop, -- ^ Top offset in DIP.
   pageScreencastFrameMetadataPageScaleFactor :: PageScreencastFrameMetadataPageScaleFactor, -- ^ Page scale factor.
   pageScreencastFrameMetadataDeviceWidth :: PageScreencastFrameMetadataDeviceWidth, -- ^ Device screen width in DIP.
   pageScreencastFrameMetadataDeviceHeight :: PageScreencastFrameMetadataDeviceHeight, -- ^ Device screen height in DIP.
   pageScreencastFrameMetadataScrollOffsetX :: PageScreencastFrameMetadataScrollOffsetX, -- ^ Position of horizontal scroll in CSS pixels.
   pageScreencastFrameMetadataScrollOffsetY :: PageScreencastFrameMetadataScrollOffsetY, -- ^ Position of vertical scroll in CSS pixels.
   pageScreencastFrameMetadataTimestamp :: PageScreencastFrameMetadataTimestamp -- ^ Frame swap timestamp.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastFrameMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageScreencastFrameMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Javascript dialog type.
data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageDialogType where
   parseJSON = A.withText  "PageDialogType"  $ \v -> do
      case v of
         "alert" -> pure PageDialogTypeAlert
         "confirm" -> pure PageDialogTypeConfirm
         "prompt" -> pure PageDialogTypePrompt
         "beforeunload" -> pure PageDialogTypeBeforeunload
         _ -> fail "failed to parse PageDialogType"

instance ToJSON PageDialogType where
   toJSON v = A.String $
      case v of
         PageDialogTypeAlert -> "alert"
         PageDialogTypeConfirm -> "confirm"
         PageDialogTypePrompt -> "prompt"
         PageDialogTypeBeforeunload -> "beforeunload"



-- | Error while paring app manifest.
data PageAppManifestError = PageAppManifestError {
   pageAppManifestErrorMessage :: PageAppManifestErrorMessage, -- ^ Error message.
   pageAppManifestErrorCritical :: PageAppManifestErrorCritical, -- ^ If criticial, this is a non-recoverable parse error.
   pageAppManifestErrorLine :: PageAppManifestErrorLine, -- ^ Error line.
   pageAppManifestErrorColumn :: PageAppManifestErrorColumn -- ^ Error column.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Parsed app manifest properties.
data PageAppManifestParsedProperties = PageAppManifestParsedProperties {
   pageAppManifestParsedPropertiesScope :: PageAppManifestParsedPropertiesScope -- ^ Computed scope value
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestParsedProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestParsedProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Layout viewport position and dimensions.
data PageLayoutViewport = PageLayoutViewport {
   pageLayoutViewportPageX :: PageLayoutViewportPageX, -- ^ Horizontal offset relative to the document (CSS pixels).
   pageLayoutViewportPageY :: PageLayoutViewportPageY, -- ^ Vertical offset relative to the document (CSS pixels).
   pageLayoutViewportClientWidth :: PageLayoutViewportClientWidth, -- ^ Width (CSS pixels), excludes scrollbar if present.
   pageLayoutViewportClientHeight :: PageLayoutViewportClientHeight -- ^ Height (CSS pixels), excludes scrollbar if present.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLayoutViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLayoutViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Visual viewport position, dimensions, and scale.
data PageVisualViewport = PageVisualViewport {
   pageVisualViewportOffsetX :: PageVisualViewportOffsetX, -- ^ Horizontal offset relative to the layout viewport (CSS pixels).
   pageVisualViewportOffsetY :: PageVisualViewportOffsetY, -- ^ Vertical offset relative to the layout viewport (CSS pixels).
   pageVisualViewportPageX :: PageVisualViewportPageX, -- ^ Horizontal offset relative to the document (CSS pixels).
   pageVisualViewportPageY :: PageVisualViewportPageY, -- ^ Vertical offset relative to the document (CSS pixels).
   pageVisualViewportClientWidth :: PageVisualViewportClientWidth, -- ^ Width (CSS pixels), excludes scrollbar if present.
   pageVisualViewportClientHeight :: PageVisualViewportClientHeight, -- ^ Height (CSS pixels), excludes scrollbar if present.
   pageVisualViewportScale :: PageVisualViewportScale, -- ^ Scale relative to the ideal viewport (size at width=device-width).
   pageVisualViewportZoom :: PageVisualViewportZoom -- ^ Page zoom factor (CSS to device independent pixels ratio).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageVisualViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageVisualViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Viewport for capturing screenshot.
data PageViewport = PageViewport {
   pageViewportX :: PageViewportX, -- ^ X offset in device independent pixels (dip).
   pageViewportY :: PageViewportY, -- ^ Y offset in device independent pixels (dip).
   pageViewportWidth :: PageViewportWidth, -- ^ Rectangle width in device independent pixels (dip).
   pageViewportHeight :: PageViewportHeight, -- ^ Rectangle height in device independent pixels (dip).
   pageViewportScale :: PageViewportScale -- ^ Page scale factor.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PageViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Generic font families collection.
data PageFontFamilies = PageFontFamilies {
   pageFontFamiliesStandard :: PageFontFamiliesStandard, -- ^ The standard font-family.
   pageFontFamiliesFixed :: PageFontFamiliesFixed, -- ^ The fixed font-family.
   pageFontFamiliesSerif :: PageFontFamiliesSerif, -- ^ The serif font-family.
   pageFontFamiliesSansSerif :: PageFontFamiliesSansSerif, -- ^ The sansSerif font-family.
   pageFontFamiliesCursive :: PageFontFamiliesCursive, -- ^ The cursive font-family.
   pageFontFamiliesFantasy :: PageFontFamiliesFantasy, -- ^ The fantasy font-family.
   pageFontFamiliesMath :: PageFontFamiliesMath -- ^ The math font-family.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PageFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Font families collection for a script.
data PageScriptFontFamilies = PageScriptFontFamilies {
   pageScriptFontFamiliesScript :: PageScriptFontFamiliesScript, -- ^ Name of the script which these font families are defined for.
   pageScriptFontFamiliesFontFamilies :: PageScriptFontFamiliesFontFamilies -- ^ Generic font families collection for the script.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScriptFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PageScriptFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Default font sizes.
data PageFontSizes = PageFontSizes {
   pageFontSizesStandard :: PageFontSizesStandard, -- ^ Default standard font size.
   pageFontSizesFixed :: PageFontSizesFixed -- ^ Default fixed font size.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PageFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Page.ClientNavigationReason' .
data PageClientNavigationReason = PageClientNavigationReasonFormSubmissionGet | PageClientNavigationReasonFormSubmissionPost | PageClientNavigationReasonHttpHeaderRefresh | PageClientNavigationReasonScriptInitiated | PageClientNavigationReasonMetaTagRefresh | PageClientNavigationReasonPageBlockInterstitial | PageClientNavigationReasonReload | PageClientNavigationReasonAnchorClick
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageClientNavigationReason where
   parseJSON = A.withText  "PageClientNavigationReason"  $ \v -> do
      case v of
         "formSubmissionGet" -> pure PageClientNavigationReasonFormSubmissionGet
         "formSubmissionPost" -> pure PageClientNavigationReasonFormSubmissionPost
         "httpHeaderRefresh" -> pure PageClientNavigationReasonHttpHeaderRefresh
         "scriptInitiated" -> pure PageClientNavigationReasonScriptInitiated
         "metaTagRefresh" -> pure PageClientNavigationReasonMetaTagRefresh
         "pageBlockInterstitial" -> pure PageClientNavigationReasonPageBlockInterstitial
         "reload" -> pure PageClientNavigationReasonReload
         "anchorClick" -> pure PageClientNavigationReasonAnchorClick
         _ -> fail "failed to parse PageClientNavigationReason"

instance ToJSON PageClientNavigationReason where
   toJSON v = A.String $
      case v of
         PageClientNavigationReasonFormSubmissionGet -> "formSubmissionGet"
         PageClientNavigationReasonFormSubmissionPost -> "formSubmissionPost"
         PageClientNavigationReasonHttpHeaderRefresh -> "httpHeaderRefresh"
         PageClientNavigationReasonScriptInitiated -> "scriptInitiated"
         PageClientNavigationReasonMetaTagRefresh -> "metaTagRefresh"
         PageClientNavigationReasonPageBlockInterstitial -> "pageBlockInterstitial"
         PageClientNavigationReasonReload -> "reload"
         PageClientNavigationReasonAnchorClick -> "anchorClick"



-- | Type 'Page.ClientNavigationDisposition' .
data PageClientNavigationDisposition = PageClientNavigationDispositionCurrentTab | PageClientNavigationDispositionNewTab | PageClientNavigationDispositionNewWindow | PageClientNavigationDispositionDownload
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageClientNavigationDisposition where
   parseJSON = A.withText  "PageClientNavigationDisposition"  $ \v -> do
      case v of
         "currentTab" -> pure PageClientNavigationDispositionCurrentTab
         "newTab" -> pure PageClientNavigationDispositionNewTab
         "newWindow" -> pure PageClientNavigationDispositionNewWindow
         "download" -> pure PageClientNavigationDispositionDownload
         _ -> fail "failed to parse PageClientNavigationDisposition"

instance ToJSON PageClientNavigationDisposition where
   toJSON v = A.String $
      case v of
         PageClientNavigationDispositionCurrentTab -> "currentTab"
         PageClientNavigationDispositionNewTab -> "newTab"
         PageClientNavigationDispositionNewWindow -> "newWindow"
         PageClientNavigationDispositionDownload -> "download"



-- | Type 'Page.InstallabilityErrorArgument' .
data PageInstallabilityErrorArgument = PageInstallabilityErrorArgument {
   pageInstallabilityErrorArgumentName :: PageInstallabilityErrorArgumentName, -- ^ Argument name (e.g. name:'minimum-icon-size-in-pixels').
   pageInstallabilityErrorArgumentValue :: PageInstallabilityErrorArgumentValue -- ^ Argument value (e.g. value:'64').
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageInstallabilityErrorArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageInstallabilityErrorArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | The installability error
data PageInstallabilityError = PageInstallabilityError {
   pageInstallabilityErrorErrorId :: PageInstallabilityErrorErrorId, -- ^ The error id (e.g. 'manifest-missing-suitable-icon').
   pageInstallabilityErrorErrorArguments :: PageInstallabilityErrorErrorArguments -- ^ The list of error arguments (e.g. {name:'minimum-icon-size-in-pixels', value:'64'}).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageInstallabilityError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageInstallabilityError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | The referring-policy used for the navigation.
data PageReferrerPolicy = PageReferrerPolicyNoReferrer | PageReferrerPolicyNoReferrerWhenDowngrade | PageReferrerPolicyOrigin | PageReferrerPolicyOriginWhenCrossOrigin | PageReferrerPolicySameOrigin | PageReferrerPolicyStrictOrigin | PageReferrerPolicyStrictOriginWhenCrossOrigin | PageReferrerPolicyUnsafeUrl
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageReferrerPolicy where
   parseJSON = A.withText  "PageReferrerPolicy"  $ \v -> do
      case v of
         "noReferrer" -> pure PageReferrerPolicyNoReferrer
         "noReferrerWhenDowngrade" -> pure PageReferrerPolicyNoReferrerWhenDowngrade
         "origin" -> pure PageReferrerPolicyOrigin
         "originWhenCrossOrigin" -> pure PageReferrerPolicyOriginWhenCrossOrigin
         "sameOrigin" -> pure PageReferrerPolicySameOrigin
         "strictOrigin" -> pure PageReferrerPolicyStrictOrigin
         "strictOriginWhenCrossOrigin" -> pure PageReferrerPolicyStrictOriginWhenCrossOrigin
         "unsafeUrl" -> pure PageReferrerPolicyUnsafeUrl
         _ -> fail "failed to parse PageReferrerPolicy"

instance ToJSON PageReferrerPolicy where
   toJSON v = A.String $
      case v of
         PageReferrerPolicyNoReferrer -> "noReferrer"
         PageReferrerPolicyNoReferrerWhenDowngrade -> "noReferrerWhenDowngrade"
         PageReferrerPolicyOrigin -> "origin"
         PageReferrerPolicyOriginWhenCrossOrigin -> "originWhenCrossOrigin"
         PageReferrerPolicySameOrigin -> "sameOrigin"
         PageReferrerPolicyStrictOrigin -> "strictOrigin"
         PageReferrerPolicyStrictOriginWhenCrossOrigin -> "strictOriginWhenCrossOrigin"
         PageReferrerPolicyUnsafeUrl -> "unsafeUrl"



-- | Per-script compilation cache parameters for `Page.produceCompilationCache`
data PageCompilationCacheParams = PageCompilationCacheParams {
   pageCompilationCacheParamsUrl :: PageCompilationCacheParamsUrl, -- ^ The URL of the script to produce a compilation cache entry for.
   pageCompilationCacheParamsEager :: PageCompilationCacheParamsEager -- ^ A hint to the backend whether eager compilation is recommended.
(the actual compilation mode used is upon backend discretion).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | The type of a frameNavigated event.
data PageNavigationType = PageNavigationTypeNavigation | PageNavigationTypeBackForwardCacheRestore
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageNavigationType where
   parseJSON = A.withText  "PageNavigationType"  $ \v -> do
      case v of
         "Navigation" -> pure PageNavigationTypeNavigation
         "BackForwardCacheRestore" -> pure PageNavigationTypeBackForwardCacheRestore
         _ -> fail "failed to parse PageNavigationType"

instance ToJSON PageNavigationType where
   toJSON v = A.String $
      case v of
         PageNavigationTypeNavigation -> "Navigation"
         PageNavigationTypeBackForwardCacheRestore -> "BackForwardCacheRestore"



-- | List of not restored reasons for back-forward cache.
data PageBackForwardCacheNotRestoredReason = PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled | PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist | PageBackForwardCacheNotRestoredReasonHttpStatusNotOk | PageBackForwardCacheNotRestoredReasonSchemeNotHttpOrHttps | PageBackForwardCacheNotRestoredReasonLoading | PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess | PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled | PageBackForwardCacheNotRestoredReasonDomainNotAllowed | PageBackForwardCacheNotRestoredReasonHttpMethodNotGet | PageBackForwardCacheNotRestoredReasonSubframeIsNavigating | PageBackForwardCacheNotRestoredReasonTimeout | PageBackForwardCacheNotRestoredReasonCacheLimit | PageBackForwardCacheNotRestoredReasonJavaScriptExecution | PageBackForwardCacheNotRestoredReasonRendererProcessKilled | PageBackForwardCacheNotRestoredReasonRendererProcessCrashed | PageBackForwardCacheNotRestoredReasonSchedulerTrackedFeatureUsed | PageBackForwardCacheNotRestoredReasonConflictingBrowsingInstance | PageBackForwardCacheNotRestoredReasonCacheFlushed | PageBackForwardCacheNotRestoredReasonServiceWorkerVersionActivation | PageBackForwardCacheNotRestoredReasonSessionRestored | PageBackForwardCacheNotRestoredReasonServiceWorkerPostMessage | PageBackForwardCacheNotRestoredReasonEnteredBackForwardCacheBeforeServiceWorkerHostAdded | PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedSameSite | PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedCrossSite | PageBackForwardCacheNotRestoredReasonServiceWorkerClaim | PageBackForwardCacheNotRestoredReasonIgnoreEventAndEvict | PageBackForwardCacheNotRestoredReasonHaveInnerContents | PageBackForwardCacheNotRestoredReasonTimeoutPuttingInCache | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByLowMemory | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByCommandLine | PageBackForwardCacheNotRestoredReasonNetworkRequestDatapipeDrainedAsBytesConsumer | PageBackForwardCacheNotRestoredReasonNetworkRequestRedirected | PageBackForwardCacheNotRestoredReasonNetworkRequestTimeout | PageBackForwardCacheNotRestoredReasonNetworkExceedsBufferLimit | PageBackForwardCacheNotRestoredReasonNavigationCancelledWhileRestoring | PageBackForwardCacheNotRestoredReasonNotMostRecentNavigationEntry | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForPrerender | PageBackForwardCacheNotRestoredReasonUserAgentOverrideDiffers | PageBackForwardCacheNotRestoredReasonForegroundCacheLimit | PageBackForwardCacheNotRestoredReasonBrowsingInstanceNotSwapped | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForDelegate | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInMainFrame | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInSubFrame | PageBackForwardCacheNotRestoredReasonServiceWorkerUnregistration | PageBackForwardCacheNotRestoredReasonCacheControlNoStore | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreCookieModified | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHttpOnlyCookieModified | PageBackForwardCacheNotRestoredReasonNoResponseHead | PageBackForwardCacheNotRestoredReasonUnknown | PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857 | PageBackForwardCacheNotRestoredReasonErrorDocument | PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder | PageBackForwardCacheNotRestoredReasonWebSocket | PageBackForwardCacheNotRestoredReasonWebTransport | PageBackForwardCacheNotRestoredReasonWebRtc | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonContainsPlugins | PageBackForwardCacheNotRestoredReasonDocumentLoaded | PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers | PageBackForwardCacheNotRestoredReasonOutstandingIndexedDbTransaction | PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission | PageBackForwardCacheNotRestoredReasonRequestedMidiPermission | PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors | PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission | PageBackForwardCacheNotRestoredReasonBroadcastChannel | PageBackForwardCacheNotRestoredReasonIndexedDbConnection | PageBackForwardCacheNotRestoredReasonWebXr | PageBackForwardCacheNotRestoredReasonSharedWorker | PageBackForwardCacheNotRestoredReasonWebLocks | PageBackForwardCacheNotRestoredReasonWebHid | PageBackForwardCacheNotRestoredReasonWebShare | PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant | PageBackForwardCacheNotRestoredReasonWebNfc | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXhr | PageBackForwardCacheNotRestoredReasonAppBanner | PageBackForwardCacheNotRestoredReasonPrinting | PageBackForwardCacheNotRestoredReasonWebDatabase | PageBackForwardCacheNotRestoredReasonPictureInPicture | PageBackForwardCacheNotRestoredReasonPortal | PageBackForwardCacheNotRestoredReasonSpeechRecognizer | PageBackForwardCacheNotRestoredReasonIdleManager | PageBackForwardCacheNotRestoredReasonPaymentManager | PageBackForwardCacheNotRestoredReasonSpeechSynthesis | PageBackForwardCacheNotRestoredReasonKeyboardLock | PageBackForwardCacheNotRestoredReasonWebOtpService | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket | PageBackForwardCacheNotRestoredReasonInjectedJavascript | PageBackForwardCacheNotRestoredReasonInjectedStyleSheet | PageBackForwardCacheNotRestoredReasonDummy | PageBackForwardCacheNotRestoredReasonContentSecurityHandler | PageBackForwardCacheNotRestoredReasonContentWebAuthenticationApi | PageBackForwardCacheNotRestoredReasonContentFileChooser | PageBackForwardCacheNotRestoredReasonContentSerial | PageBackForwardCacheNotRestoredReasonContentFileSystemAccess | PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost | PageBackForwardCacheNotRestoredReasonContentWebBluetooth | PageBackForwardCacheNotRestoredReasonContentWebUsb | PageBackForwardCacheNotRestoredReasonContentMediaSessionService | PageBackForwardCacheNotRestoredReasonContentScreenReader | PageBackForwardCacheNotRestoredReasonEmbedderPopupBlockerTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingTriggeredPopupBlocker | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingThreatDetails | PageBackForwardCacheNotRestoredReasonEmbedderAppBannerManager | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerViewerSource | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerSelfDeletingRequestDelegate | PageBackForwardCacheNotRestoredReasonEmbedderOomInterventionTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderOfflinePage | PageBackForwardCacheNotRestoredReasonEmbedderChromePasswordManagerClientBindCredentialManager | PageBackForwardCacheNotRestoredReasonEmbedderPermissionRequestManager | PageBackForwardCacheNotRestoredReasonEmbedderModalDialog | PageBackForwardCacheNotRestoredReasonEmbedderExtensions | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessaging | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessagingForOpenPort | PageBackForwardCacheNotRestoredReasonEmbedderExtensionSentMessageToCachedFrame
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageBackForwardCacheNotRestoredReason where
   parseJSON = A.withText  "PageBackForwardCacheNotRestoredReason"  $ \v -> do
      case v of
         "NotPrimaryMainFrame" -> pure PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame
         "BackForwardCacheDisabled" -> pure PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled
         "RelatedActiveContentsExist" -> pure PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist
         "HTTPStatusNotOK" -> pure PageBackForwardCacheNotRestoredReasonHttpStatusNotOk
         "SchemeNotHTTPOrHTTPS" -> pure PageBackForwardCacheNotRestoredReasonSchemeNotHttpOrHttps
         "Loading" -> pure PageBackForwardCacheNotRestoredReasonLoading
         "WasGrantedMediaAccess" -> pure PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess
         "DisableForRenderFrameHostCalled" -> pure PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled
         "DomainNotAllowed" -> pure PageBackForwardCacheNotRestoredReasonDomainNotAllowed
         "HTTPMethodNotGET" -> pure PageBackForwardCacheNotRestoredReasonHttpMethodNotGet
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
         "RenderFrameHostReused_SameSite" -> pure PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedSameSite
         "RenderFrameHostReused_CrossSite" -> pure PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedCrossSite
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
         "CacheControlNoStoreHTTPOnlyCookieModified" -> pure PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHttpOnlyCookieModified
         "NoResponseHead" -> pure PageBackForwardCacheNotRestoredReasonNoResponseHead
         "Unknown" -> pure PageBackForwardCacheNotRestoredReasonUnknown
         "ActivationNavigationsDisallowedForBug1234857" -> pure PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857
         "ErrorDocument" -> pure PageBackForwardCacheNotRestoredReasonErrorDocument
         "FencedFramesEmbedder" -> pure PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder
         "WebSocket" -> pure PageBackForwardCacheNotRestoredReasonWebSocket
         "WebTransport" -> pure PageBackForwardCacheNotRestoredReasonWebTransport
         "WebRTC" -> pure PageBackForwardCacheNotRestoredReasonWebRtc
         "MainResourceHasCacheControlNoStore" -> pure PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore
         "MainResourceHasCacheControlNoCache" -> pure PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache
         "SubresourceHasCacheControlNoStore" -> pure PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore
         "SubresourceHasCacheControlNoCache" -> pure PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache
         "ContainsPlugins" -> pure PageBackForwardCacheNotRestoredReasonContainsPlugins
         "DocumentLoaded" -> pure PageBackForwardCacheNotRestoredReasonDocumentLoaded
         "DedicatedWorkerOrWorklet" -> pure PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet
         "OutstandingNetworkRequestOthers" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers
         "OutstandingIndexedDBTransaction" -> pure PageBackForwardCacheNotRestoredReasonOutstandingIndexedDbTransaction
         "RequestedNotificationsPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission
         "RequestedMIDIPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedMidiPermission
         "RequestedAudioCapturePermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission
         "RequestedVideoCapturePermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission
         "RequestedBackForwardCacheBlockedSensors" -> pure PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors
         "RequestedBackgroundWorkPermission" -> pure PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission
         "BroadcastChannel" -> pure PageBackForwardCacheNotRestoredReasonBroadcastChannel
         "IndexedDBConnection" -> pure PageBackForwardCacheNotRestoredReasonIndexedDbConnection
         "WebXR" -> pure PageBackForwardCacheNotRestoredReasonWebXr
         "SharedWorker" -> pure PageBackForwardCacheNotRestoredReasonSharedWorker
         "WebLocks" -> pure PageBackForwardCacheNotRestoredReasonWebLocks
         "WebHID" -> pure PageBackForwardCacheNotRestoredReasonWebHid
         "WebShare" -> pure PageBackForwardCacheNotRestoredReasonWebShare
         "RequestedStorageAccessGrant" -> pure PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant
         "WebNfc" -> pure PageBackForwardCacheNotRestoredReasonWebNfc
         "OutstandingNetworkRequestFetch" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch
         "OutstandingNetworkRequestXHR" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXhr
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
         "WebOTPService" -> pure PageBackForwardCacheNotRestoredReasonWebOtpService
         "OutstandingNetworkRequestDirectSocket" -> pure PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket
         "InjectedJavascript" -> pure PageBackForwardCacheNotRestoredReasonInjectedJavascript
         "InjectedStyleSheet" -> pure PageBackForwardCacheNotRestoredReasonInjectedStyleSheet
         "Dummy" -> pure PageBackForwardCacheNotRestoredReasonDummy
         "ContentSecurityHandler" -> pure PageBackForwardCacheNotRestoredReasonContentSecurityHandler
         "ContentWebAuthenticationAPI" -> pure PageBackForwardCacheNotRestoredReasonContentWebAuthenticationApi
         "ContentFileChooser" -> pure PageBackForwardCacheNotRestoredReasonContentFileChooser
         "ContentSerial" -> pure PageBackForwardCacheNotRestoredReasonContentSerial
         "ContentFileSystemAccess" -> pure PageBackForwardCacheNotRestoredReasonContentFileSystemAccess
         "ContentMediaDevicesDispatcherHost" -> pure PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost
         "ContentWebBluetooth" -> pure PageBackForwardCacheNotRestoredReasonContentWebBluetooth
         "ContentWebUSB" -> pure PageBackForwardCacheNotRestoredReasonContentWebUsb
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
         _ -> fail "failed to parse PageBackForwardCacheNotRestoredReason"

instance ToJSON PageBackForwardCacheNotRestoredReason where
   toJSON v = A.String $
      case v of
         PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame -> "NotPrimaryMainFrame"
         PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled -> "BackForwardCacheDisabled"
         PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist -> "RelatedActiveContentsExist"
         PageBackForwardCacheNotRestoredReasonHttpStatusNotOk -> "HTTPStatusNotOK"
         PageBackForwardCacheNotRestoredReasonSchemeNotHttpOrHttps -> "SchemeNotHTTPOrHTTPS"
         PageBackForwardCacheNotRestoredReasonLoading -> "Loading"
         PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess -> "WasGrantedMediaAccess"
         PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled -> "DisableForRenderFrameHostCalled"
         PageBackForwardCacheNotRestoredReasonDomainNotAllowed -> "DomainNotAllowed"
         PageBackForwardCacheNotRestoredReasonHttpMethodNotGet -> "HTTPMethodNotGET"
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
         PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedSameSite -> "RenderFrameHostReused_SameSite"
         PageBackForwardCacheNotRestoredReasonRenderFrameHostReusedCrossSite -> "RenderFrameHostReused_CrossSite"
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
         PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHttpOnlyCookieModified -> "CacheControlNoStoreHTTPOnlyCookieModified"
         PageBackForwardCacheNotRestoredReasonNoResponseHead -> "NoResponseHead"
         PageBackForwardCacheNotRestoredReasonUnknown -> "Unknown"
         PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857 -> "ActivationNavigationsDisallowedForBug1234857"
         PageBackForwardCacheNotRestoredReasonErrorDocument -> "ErrorDocument"
         PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder -> "FencedFramesEmbedder"
         PageBackForwardCacheNotRestoredReasonWebSocket -> "WebSocket"
         PageBackForwardCacheNotRestoredReasonWebTransport -> "WebTransport"
         PageBackForwardCacheNotRestoredReasonWebRtc -> "WebRTC"
         PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore -> "MainResourceHasCacheControlNoStore"
         PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache -> "MainResourceHasCacheControlNoCache"
         PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore -> "SubresourceHasCacheControlNoStore"
         PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache -> "SubresourceHasCacheControlNoCache"
         PageBackForwardCacheNotRestoredReasonContainsPlugins -> "ContainsPlugins"
         PageBackForwardCacheNotRestoredReasonDocumentLoaded -> "DocumentLoaded"
         PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet -> "DedicatedWorkerOrWorklet"
         PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers -> "OutstandingNetworkRequestOthers"
         PageBackForwardCacheNotRestoredReasonOutstandingIndexedDbTransaction -> "OutstandingIndexedDBTransaction"
         PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission -> "RequestedNotificationsPermission"
         PageBackForwardCacheNotRestoredReasonRequestedMidiPermission -> "RequestedMIDIPermission"
         PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission -> "RequestedAudioCapturePermission"
         PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission -> "RequestedVideoCapturePermission"
         PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors -> "RequestedBackForwardCacheBlockedSensors"
         PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission -> "RequestedBackgroundWorkPermission"
         PageBackForwardCacheNotRestoredReasonBroadcastChannel -> "BroadcastChannel"
         PageBackForwardCacheNotRestoredReasonIndexedDbConnection -> "IndexedDBConnection"
         PageBackForwardCacheNotRestoredReasonWebXr -> "WebXR"
         PageBackForwardCacheNotRestoredReasonSharedWorker -> "SharedWorker"
         PageBackForwardCacheNotRestoredReasonWebLocks -> "WebLocks"
         PageBackForwardCacheNotRestoredReasonWebHid -> "WebHID"
         PageBackForwardCacheNotRestoredReasonWebShare -> "WebShare"
         PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant -> "RequestedStorageAccessGrant"
         PageBackForwardCacheNotRestoredReasonWebNfc -> "WebNfc"
         PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch -> "OutstandingNetworkRequestFetch"
         PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXhr -> "OutstandingNetworkRequestXHR"
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
         PageBackForwardCacheNotRestoredReasonWebOtpService -> "WebOTPService"
         PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket -> "OutstandingNetworkRequestDirectSocket"
         PageBackForwardCacheNotRestoredReasonInjectedJavascript -> "InjectedJavascript"
         PageBackForwardCacheNotRestoredReasonInjectedStyleSheet -> "InjectedStyleSheet"
         PageBackForwardCacheNotRestoredReasonDummy -> "Dummy"
         PageBackForwardCacheNotRestoredReasonContentSecurityHandler -> "ContentSecurityHandler"
         PageBackForwardCacheNotRestoredReasonContentWebAuthenticationApi -> "ContentWebAuthenticationAPI"
         PageBackForwardCacheNotRestoredReasonContentFileChooser -> "ContentFileChooser"
         PageBackForwardCacheNotRestoredReasonContentSerial -> "ContentSerial"
         PageBackForwardCacheNotRestoredReasonContentFileSystemAccess -> "ContentFileSystemAccess"
         PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost -> "ContentMediaDevicesDispatcherHost"
         PageBackForwardCacheNotRestoredReasonContentWebBluetooth -> "ContentWebBluetooth"
         PageBackForwardCacheNotRestoredReasonContentWebUsb -> "ContentWebUSB"
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



-- | Types of not restored reasons for back-forward cache.
data PageBackForwardCacheNotRestoredReasonType = PageBackForwardCacheNotRestoredReasonTypeSupportPending | PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded | PageBackForwardCacheNotRestoredReasonTypeCircumstantial
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageBackForwardCacheNotRestoredReasonType where
   parseJSON = A.withText  "PageBackForwardCacheNotRestoredReasonType"  $ \v -> do
      case v of
         "SupportPending" -> pure PageBackForwardCacheNotRestoredReasonTypeSupportPending
         "PageSupportNeeded" -> pure PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded
         "Circumstantial" -> pure PageBackForwardCacheNotRestoredReasonTypeCircumstantial
         _ -> fail "failed to parse PageBackForwardCacheNotRestoredReasonType"

instance ToJSON PageBackForwardCacheNotRestoredReasonType where
   toJSON v = A.String $
      case v of
         PageBackForwardCacheNotRestoredReasonTypeSupportPending -> "SupportPending"
         PageBackForwardCacheNotRestoredReasonTypePageSupportNeeded -> "PageSupportNeeded"
         PageBackForwardCacheNotRestoredReasonTypeCircumstantial -> "Circumstantial"



-- | Type 'Page.BackForwardCacheNotRestoredExplanation' .
data PageBackForwardCacheNotRestoredExplanation = PageBackForwardCacheNotRestoredExplanation {
   pageBackForwardCacheNotRestoredExplanationType :: PageBackForwardCacheNotRestoredExplanationType, -- ^ Type of the reason
   pageBackForwardCacheNotRestoredExplanationReason :: PageBackForwardCacheNotRestoredExplanationReason, -- ^ Not restored reason
   pageBackForwardCacheNotRestoredExplanationContext :: PageBackForwardCacheNotRestoredExplanationContext -- ^ Context associated with the reason. The meaning of this context is
dependent on the reason:
- EmbedderExtensionSentMessageToCachedFrame: the extension ID.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'Page.BackForwardCacheNotRestoredExplanationTree' .
data PageBackForwardCacheNotRestoredExplanationTree = PageBackForwardCacheNotRestoredExplanationTree {
   pageBackForwardCacheNotRestoredExplanationTreeUrl :: PageBackForwardCacheNotRestoredExplanationTreeUrl, -- ^ URL of each frame
   pageBackForwardCacheNotRestoredExplanationTreeExplanations :: PageBackForwardCacheNotRestoredExplanationTreeExplanations, -- ^ Not restored reasons of each frame
   pageBackForwardCacheNotRestoredExplanationTreeChildren :: PageBackForwardCacheNotRestoredExplanationTreeChildren -- ^ Array of children frame
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanationTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanationTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }



-- | List of FinalStatus reasons for Prerender2.
data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
   deriving (Ord, Eq, Show, Read)
instance FromJSON PagePrerenderFinalStatus where
   parseJSON = A.withText  "PagePrerenderFinalStatus"  $ \v -> do
      case v of
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
         "EmbedderTriggeredAndSameOriginRedirected" -> pure PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected
         "EmbedderTriggeredAndCrossOriginRedirected" -> pure PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
         "EmbedderTriggeredAndDestroyed" -> pure PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
         _ -> fail "failed to parse PagePrerenderFinalStatus"

instance ToJSON PagePrerenderFinalStatus where
   toJSON v = A.String $
      case v of
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
         PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected -> "EmbedderTriggeredAndSameOriginRedirected"
         PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected -> "EmbedderTriggeredAndCrossOriginRedirected"
         PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed -> "EmbedderTriggeredAndDestroyed"





-- | Type of the 'Page.domContentEventFired' event.
data PageDomContentEventFired = PageDomContentEventFired {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDomContentEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PageDomContentEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'Page.fileChooserOpened' event.
data PageFileChooserOpenedMode = PageFileChooserOpenedModeSelectSingle | PageFileChooserOpenedModeSelectMultiple
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageFileChooserOpenedMode where
   parseJSON = A.withText  "PageFileChooserOpenedMode"  $ \v -> do
      case v of
         "selectSingle" -> pure PageFileChooserOpenedModeSelectSingle
         "selectMultiple" -> pure PageFileChooserOpenedModeSelectMultiple
         _ -> fail "failed to parse PageFileChooserOpenedMode"

instance ToJSON PageFileChooserOpenedMode where
   toJSON v = A.String $
      case v of
         PageFileChooserOpenedModeSelectSingle -> "selectSingle"
         PageFileChooserOpenedModeSelectMultiple -> "selectMultiple"



data PageFileChooserOpened = PageFileChooserOpened {
   pageFileChooserOpenedFrameId :: PageFileChooserOpenedFrameId, -- ^ Id of the frame containing input node.
   pageFileChooserOpenedBackendNodeId :: PageFileChooserOpenedBackendNodeId, -- ^ Input node id.
   pageFileChooserOpenedMode :: PageFileChooserOpenedMode -- ^ Input mode.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFileChooserOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFileChooserOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'Page.frameAttached' event.
data PageFrameAttached = PageFrameAttached {
   pageFrameAttachedFrameId :: PageFrameAttachedFrameId, -- ^ Id of the frame that has been attached.
   pageFrameAttachedParentFrameId :: PageFrameAttachedParentFrameId, -- ^ Parent frame identifier.
   pageFrameAttachedStack :: PageFrameAttachedStack -- ^ JavaScript stack trace of when frame was attached, only set if frame initiated from script.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameAttached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameAttached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type of the 'Page.frameDetached' event.
data PageFrameDetachedReason = PageFrameDetachedReasonRemove | PageFrameDetachedReasonSwap
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageFrameDetachedReason where
   parseJSON = A.withText  "PageFrameDetachedReason"  $ \v -> do
      case v of
         "remove" -> pure PageFrameDetachedReasonRemove
         "swap" -> pure PageFrameDetachedReasonSwap
         _ -> fail "failed to parse PageFrameDetachedReason"

instance ToJSON PageFrameDetachedReason where
   toJSON v = A.String $
      case v of
         PageFrameDetachedReasonRemove -> "remove"
         PageFrameDetachedReasonSwap -> "swap"



data PageFrameDetached = PageFrameDetached {
   pageFrameDetachedFrameId :: PageFrameDetachedFrameId, -- ^ Id of the frame that has been detached.

} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type of the 'Page.frameNavigated' event.
data PageFrameNavigated = PageFrameNavigated {
   pageFrameNavigatedFrame :: PageFrameNavigatedFrame, -- ^ Frame object.

} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameNavigated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageFrameNavigated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.documentOpened' event.
data PageDocumentOpened = PageDocumentOpened {
   pageDocumentOpenedFrame :: PageDocumentOpenedFrame -- ^ Frame object.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDocumentOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageDocumentOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.frameResized' event.
data PageFrameResized = PageFrameResized
   deriving (Eq, Show, Read)
instance FromJSON PageFrameResized where
   parseJSON = A.withText  "PageFrameResized"  $ \v -> do
      case v of
         "PageFrameResized" -> pure PageFrameResized
         _ -> fail "failed to parse PageFrameResized"



-- | Type of the 'Page.frameRequestedNavigation' event.
data PageFrameRequestedNavigation = PageFrameRequestedNavigation {
   pageFrameRequestedNavigationFrameId :: PageFrameRequestedNavigationFrameId, -- ^ Id of the frame that is being navigated.
   pageFrameRequestedNavigationReason :: PageFrameRequestedNavigationReason, -- ^ The reason for the navigation.
   pageFrameRequestedNavigationUrl :: PageFrameRequestedNavigationUrl, -- ^ The destination URL for the requested navigation.
   pageFrameRequestedNavigationDisposition :: PageFrameRequestedNavigationDisposition -- ^ The disposition for the navigation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameRequestedNavigation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageFrameRequestedNavigation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type of the 'Page.frameStartedLoading' event.
data PageFrameStartedLoading = PageFrameStartedLoading {
   pageFrameStartedLoadingFrameId :: PageFrameStartedLoadingFrameId -- ^ Id of the frame that has started loading.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStartedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStartedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Page.frameStoppedLoading' event.
data PageFrameStoppedLoading = PageFrameStoppedLoading {
   pageFrameStoppedLoadingFrameId :: PageFrameStoppedLoadingFrameId -- ^ Id of the frame that has stopped loading.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStoppedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStoppedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Page.interstitialHidden' event.
data PageInterstitialHidden = PageInterstitialHidden
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
   parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
      case v of
         "PageInterstitialHidden" -> pure PageInterstitialHidden
         _ -> fail "failed to parse PageInterstitialHidden"



-- | Type of the 'Page.interstitialShown' event.
data PageInterstitialShown = PageInterstitialShown
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
   parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
      case v of
         "PageInterstitialShown" -> pure PageInterstitialShown
         _ -> fail "failed to parse PageInterstitialShown"



-- | Type of the 'Page.javascriptDialogClosed' event.
data PageJavascriptDialogClosed = PageJavascriptDialogClosed {
   pageJavascriptDialogClosedResult :: PageJavascriptDialogClosedResult, -- ^ Whether dialog was confirmed.
   pageJavascriptDialogClosedUserInput :: PageJavascriptDialogClosedUserInput -- ^ User input in case of prompt.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Page.javascriptDialogOpening' event.
data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
   pageJavascriptDialogOpeningUrl :: PageJavascriptDialogOpeningUrl, -- ^ Frame url.
   pageJavascriptDialogOpeningMessage :: PageJavascriptDialogOpeningMessage, -- ^ Message that will be displayed by the dialog.
   pageJavascriptDialogOpeningType :: PageJavascriptDialogOpeningType, -- ^ Dialog type.
   pageJavascriptDialogOpeningHasBrowserHandler :: PageJavascriptDialogOpeningHasBrowserHandler, -- ^ True iff browser is capable showing or acting on the given dialog. When browser has no
dialog handler for given target, calling alert while Page domain is engaged will stall
the page execution. Execution can be resumed via calling Page.handleJavaScriptDialog.
   pageJavascriptDialogOpeningDefaultPrompt :: PageJavascriptDialogOpeningDefaultPrompt -- ^ Default dialog prompt.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogOpening  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogOpening where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.lifecycleEvent' event.
data PageLifecycleEvent = PageLifecycleEvent {
   pageLifecycleEventFrameId :: PageLifecycleEventFrameId, -- ^ Id of the frame.
   pageLifecycleEventLoaderId :: PageLifecycleEventLoaderId, -- ^ Loader identifier. Empty string if the request is fetched from worker.


} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLifecycleEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLifecycleEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.backForwardCacheNotUsed' event.
data PageBackForwardCacheNotUsed = PageBackForwardCacheNotUsed {
   pageBackForwardCacheNotUsedLoaderId :: PageBackForwardCacheNotUsedLoaderId, -- ^ The loader id for the associated navgation.
   pageBackForwardCacheNotUsedFrameId :: PageBackForwardCacheNotUsedFrameId, -- ^ The frame id of the associated frame.
   pageBackForwardCacheNotUsedNotRestoredExplanations :: PageBackForwardCacheNotUsedNotRestoredExplanations, -- ^ Array of reasons why the page could not be cached. This must not be empty.
   pageBackForwardCacheNotUsedNotRestoredExplanationsTree :: PageBackForwardCacheNotUsedNotRestoredExplanationsTree -- ^ Tree structure of reasons why the page could not be cached for each frame.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotUsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotUsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.prerenderAttemptCompleted' event.
data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
   pagePrerenderAttemptCompletedInitiatingFrameId :: PagePrerenderAttemptCompletedInitiatingFrameId, -- ^ The frame id of the frame initiating prerendering.


} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePrerenderAttemptCompleted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PagePrerenderAttemptCompleted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Page.loadEventFired' event.
data PageLoadEventFired = PageLoadEventFired {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLoadEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLoadEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.navigatedWithinDocument' event.
data PageNavigatedWithinDocument = PageNavigatedWithinDocument {
   pageNavigatedWithinDocumentFrameId :: PageNavigatedWithinDocumentFrameId, -- ^ Id of the frame.
   pageNavigatedWithinDocumentUrl :: PageNavigatedWithinDocumentUrl -- ^ Frame's new url.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigatedWithinDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageNavigatedWithinDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.screencastFrame' event.
data PageScreencastFrame = PageScreencastFrame {
   pageScreencastFrameData :: PageScreencastFrameData, -- ^ Base64-encoded compressed image. (Encoded as a base64 string when passed over JSON)
   pageScreencastFrameMetadata :: PageScreencastFrameMetadata, -- ^ Screencast frame metadata.
   pageScreencastFrameSessionId :: PageScreencastFrameSessionId -- ^ Frame number.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageScreencastFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Page.screencastVisibilityChanged' event.
data PageScreencastVisibilityChanged = PageScreencastVisibilityChanged {
   pageScreencastVisibilityChangedVisible :: PageScreencastVisibilityChangedVisible -- ^ True if the page is visible.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastVisibilityChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageScreencastVisibilityChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'Page.windowOpen' event.
data PageWindowOpen = PageWindowOpen {
   pageWindowOpenUrl :: PageWindowOpenUrl, -- ^ The URL for the new window.
   pageWindowOpenWindowName :: PageWindowOpenWindowName, -- ^ Window name.
   pageWindowOpenWindowFeatures :: PageWindowOpenWindowFeatures, -- ^ An array of enabled window features.
   pageWindowOpenUserGesture :: PageWindowOpenUserGesture -- ^ Whether or not it was triggered by user gesture.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageWindowOpen  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PageWindowOpen where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type of the 'Page.compilationCacheProduced' event.
data PageCompilationCacheProduced = PageCompilationCacheProduced {

   pageCompilationCacheProducedData :: PageCompilationCacheProducedData -- ^ Base64-encoded data (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheProduced  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheProduced where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }





-- | Parameters of the 'pageAddScriptToEvaluateOnNewDocument' command.
data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {

   pPageAddScriptToEvaluateOnNewDocumentWorldName :: PPageAddScriptToEvaluateOnNewDocumentWorldName, -- ^ If specified, creates an isolated world with the given name and evaluates given script in it.
This world name will be used as the ExecutionContextDescription::name when the corresponding
event is emitted.
   pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineApi :: PPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineApi -- ^ Specifies whether command line API should be available to the script, defaults
to false.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the command 'Page.addScriptToEvaluateOnNewDocument'.
-- Evaluates given script in every frame upon creation (before loading frame's scripts).
-- Parameters: 'PPageAddScriptToEvaluateOnNewDocument'
-- Returns: 'PageAddScriptToEvaluateOnNewDocument'
pageAddScriptToEvaluateOnNewDocument :: Handle ev -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument handle params = sendReceiveCommandResult handle "Page.addScriptToEvaluateOnNewDocument" (Just params)

-- | Return type of the 'pageAddScriptToEvaluateOnNewDocument' command.
data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
   pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier -- ^ Identifier of the added script.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PageAddScriptToEvaluateOnNewDocument where
   commandName _ = "Page.addScriptToEvaluateOnNewDocument"



-- | Function for the command 'Page.bringToFront'.
-- Brings page to front (activates tab).
pageBringToFront :: Handle ev -> IO (Maybe Error)
pageBringToFront handle = sendReceiveCommand handle "Page.bringToFront" (Nothing :: Maybe ())


-- | Parameters of the 'pageCaptureScreenshot' command.
data PPageCaptureScreenshotFormat = PPageCaptureScreenshotFormatJpeg | PPageCaptureScreenshotFormatPng | PPageCaptureScreenshotFormatWebp
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageCaptureScreenshotFormat where
   parseJSON = A.withText  "PPageCaptureScreenshotFormat"  $ \v -> do
      case v of
         "jpeg" -> pure PPageCaptureScreenshotFormatJpeg
         "png" -> pure PPageCaptureScreenshotFormatPng
         "webp" -> pure PPageCaptureScreenshotFormatWebp
         _ -> fail "failed to parse PPageCaptureScreenshotFormat"

instance ToJSON PPageCaptureScreenshotFormat where
   toJSON v = A.String $
      case v of
         PPageCaptureScreenshotFormatJpeg -> "jpeg"
         PPageCaptureScreenshotFormatPng -> "png"
         PPageCaptureScreenshotFormatWebp -> "webp"



data PPageCaptureScreenshot = PPageCaptureScreenshot {
   pPageCaptureScreenshotFormat :: PPageCaptureScreenshotFormat, -- ^ Image compression format (defaults to png).
   pPageCaptureScreenshotQuality :: PPageCaptureScreenshotQuality, -- ^ Compression quality from range [0..100] (jpeg only).
   pPageCaptureScreenshotClip :: PPageCaptureScreenshotClip, -- ^ Capture the screenshot of a given region only.
   pPageCaptureScreenshotFromSurface :: PPageCaptureScreenshotFromSurface, -- ^ Capture the screenshot from the surface, rather than the view. Defaults to true.
   pPageCaptureScreenshotCaptureBeyondViewport :: PPageCaptureScreenshotCaptureBeyondViewport -- ^ Capture the screenshot beyond the viewport. Defaults to false.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureScreenshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Page.captureScreenshot'.
-- Capture page screenshot.
-- Parameters: 'PPageCaptureScreenshot'
-- Returns: 'PageCaptureScreenshot'
pageCaptureScreenshot :: Handle ev -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot handle params = sendReceiveCommandResult handle "Page.captureScreenshot" (Just params)

-- | Return type of the 'pageCaptureScreenshot' command.
data PageCaptureScreenshot = PageCaptureScreenshot {
   pageCaptureScreenshotData :: String -- ^ Base64-encoded image data. (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PageCaptureScreenshot where
   commandName _ = "Page.captureScreenshot"



-- | Parameters of the 'pageCaptureSnapshot' command.
data PPageCaptureSnapshotFormat = PPageCaptureSnapshotFormatMhtml
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageCaptureSnapshotFormat where
   parseJSON = A.withText  "PPageCaptureSnapshotFormat"  $ \v -> do
      case v of
         "mhtml" -> pure PPageCaptureSnapshotFormatMhtml
         _ -> fail "failed to parse PPageCaptureSnapshotFormat"

instance ToJSON PPageCaptureSnapshotFormat where
   toJSON v = A.String $
      case v of
         PPageCaptureSnapshotFormatMhtml -> "mhtml"



data PPageCaptureSnapshot = PPageCaptureSnapshot {
   pPageCaptureSnapshotFormat :: PPageCaptureSnapshotFormat -- ^ Format (defaults to mhtml).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Page.captureSnapshot'.
-- Returns a snapshot of the page as a string. For MHTML format, the serialization includes
-- iframes, shadow DOM, external resources, and element-inline styles.
-- Parameters: 'PPageCaptureSnapshot'
-- Returns: 'PageCaptureSnapshot'
pageCaptureSnapshot :: Handle ev -> PPageCaptureSnapshot -> IO (Either Error PageCaptureSnapshot)
pageCaptureSnapshot handle params = sendReceiveCommandResult handle "Page.captureSnapshot" (Just params)

-- | Return type of the 'pageCaptureSnapshot' command.
data PageCaptureSnapshot = PageCaptureSnapshot {
   pageCaptureSnapshotData :: String -- ^ Serialized page data.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageCaptureSnapshot where
   commandName _ = "Page.captureSnapshot"



-- | Parameters of the 'pageCreateIsolatedWorld' command.
data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
   pPageCreateIsolatedWorldFrameId :: PPageCreateIsolatedWorldFrameId, -- ^ Id of the frame in which the isolated world should be created.
   pPageCreateIsolatedWorldWorldName :: PPageCreateIsolatedWorldWorldName, -- ^ An optional name which is reported in the Execution Context.
   pPageCreateIsolatedWorldGrantUniveralAccess :: PPageCreateIsolatedWorldGrantUniveralAccess -- ^ Whether or not universal access should be granted to the isolated world. This is a powerful
option, use with caution.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCreateIsolatedWorld  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Page.createIsolatedWorld'.
-- Creates an isolated world for the given frame.
-- Parameters: 'PPageCreateIsolatedWorld'
-- Returns: 'PageCreateIsolatedWorld'
pageCreateIsolatedWorld :: Handle ev -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld handle params = sendReceiveCommandResult handle "Page.createIsolatedWorld" (Just params)

-- | Return type of the 'pageCreateIsolatedWorld' command.
data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
   pageCreateIsolatedWorldExecutionContextId :: Runtime.RuntimeExecutionContextId -- ^ Execution context of the isolated world.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PageCreateIsolatedWorld where
   commandName _ = "Page.createIsolatedWorld"



-- | Function for the command 'Page.disable'.
-- Disables page domain notifications.
pageDisable :: Handle ev -> IO (Maybe Error)
pageDisable handle = sendReceiveCommand handle "Page.disable" (Nothing :: Maybe ())


-- | Function for the command 'Page.enable'.
-- Enables page domain notifications.
pageEnable :: Handle ev -> IO (Maybe Error)
pageEnable handle = sendReceiveCommand handle "Page.enable" (Nothing :: Maybe ())


-- | Function for the command 'Page.getAppManifest'.
-- Returns: 'PageGetAppManifest'
pageGetAppManifest :: Handle ev -> IO (Either Error PageGetAppManifest)
pageGetAppManifest handle = sendReceiveCommandResult handle "Page.getAppManifest" (Nothing :: Maybe ())

-- | Return type of the 'pageGetAppManifest' command.
data PageGetAppManifest = PageGetAppManifest {
   pageGetAppManifestUrl :: String, -- ^ Manifest location.

   pageGetAppManifestData :: Maybe String, -- ^ Manifest content.
   pageGetAppManifestParsed :: Maybe PageAppManifestParsedProperties -- ^ Parsed manifest properties
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppManifest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PageGetAppManifest where
   commandName _ = "Page.getAppManifest"



-- | Function for the command 'Page.getInstallabilityErrors'.
-- Returns: 'PageGetInstallabilityErrors'
pageGetInstallabilityErrors :: Handle ev -> IO (Either Error PageGetInstallabilityErrors)
pageGetInstallabilityErrors handle = sendReceiveCommandResult handle "Page.getInstallabilityErrors" (Nothing :: Maybe ())

-- | Return type of the 'pageGetInstallabilityErrors' command.
data PageGetInstallabilityErrors = PageGetInstallabilityErrors {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetInstallabilityErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PageGetInstallabilityErrors where
   commandName _ = "Page.getInstallabilityErrors"



-- | Function for the command 'Page.getManifestIcons'.
-- Returns: 'PageGetManifestIcons'
pageGetManifestIcons :: Handle ev -> IO (Either Error PageGetManifestIcons)
pageGetManifestIcons handle = sendReceiveCommandResult handle "Page.getManifestIcons" (Nothing :: Maybe ())

-- | Return type of the 'pageGetManifestIcons' command.
data PageGetManifestIcons = PageGetManifestIcons {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetManifestIcons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetManifestIcons where
   commandName _ = "Page.getManifestIcons"



-- | Function for the command 'Page.getAppId'.
-- Returns the unique (PWA) app id.
-- Only returns values if the feature flag 'WebAppEnableManifestId' is enabled
-- Returns: 'PageGetAppId'
pageGetAppId :: Handle ev -> IO (Either Error PageGetAppId)
pageGetAppId handle = sendReceiveCommandResult handle "Page.getAppId" (Nothing :: Maybe ())

-- | Return type of the 'pageGetAppId' command.
data PageGetAppId = PageGetAppId {
   pageGetAppIdAppId :: Maybe String, -- ^ App id, either from manifest's id attribute or computed from start_url
   pageGetAppIdRecommendedId :: Maybe String -- ^ Recommendation for manifest's id attribute to match current id computed from start_url
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageGetAppId where
   commandName _ = "Page.getAppId"



-- | Function for the command 'Page.getFrameTree'.
-- Returns present frame tree structure.
-- Returns: 'PageGetFrameTree'
pageGetFrameTree :: Handle ev -> IO (Either Error PageGetFrameTree)
pageGetFrameTree handle = sendReceiveCommandResult handle "Page.getFrameTree" (Nothing :: Maybe ())

-- | Return type of the 'pageGetFrameTree' command.
data PageGetFrameTree = PageGetFrameTree {
   pageGetFrameTreeFrameTree :: PageFrameTree -- ^ Present frame tree structure.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PageGetFrameTree where
   commandName _ = "Page.getFrameTree"



-- | Function for the command 'Page.getLayoutMetrics'.
-- Returns metrics relating to the layouting of the page, such as viewport bounds/scale.
-- Returns: 'PageGetLayoutMetrics'
pageGetLayoutMetrics :: Handle ev -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics handle = sendReceiveCommandResult handle "Page.getLayoutMetrics" (Nothing :: Maybe ())

-- | Return type of the 'pageGetLayoutMetrics' command.
data PageGetLayoutMetrics = PageGetLayoutMetrics {
   pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport, -- ^ Metrics relating to the layout viewport in CSS pixels.
   pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport, -- ^ Metrics relating to the visual viewport in CSS pixels.
   pageGetLayoutMetricsCssContentSize :: DomRect -- ^ Size of scrollable area in CSS pixels.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetLayoutMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetLayoutMetrics where
   commandName _ = "Page.getLayoutMetrics"



-- | Function for the command 'Page.getNavigationHistory'.
-- Returns navigation history for the current page.
-- Returns: 'PageGetNavigationHistory'
pageGetNavigationHistory :: Handle ev -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory handle = sendReceiveCommandResult handle "Page.getNavigationHistory" (Nothing :: Maybe ())

-- | Return type of the 'pageGetNavigationHistory' command.
data PageGetNavigationHistory = PageGetNavigationHistory {
   pageGetNavigationHistoryCurrentIndex :: Int, -- ^ Index of the current navigation history entry.
   pageGetNavigationHistoryEntries :: [PageNavigationEntry] -- ^ Array of navigation history entries.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetNavigationHistory where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PageGetNavigationHistory where
   commandName _ = "Page.getNavigationHistory"



-- | Function for the command 'Page.resetNavigationHistory'.
-- Resets navigation history for the current page.
pageResetNavigationHistory :: Handle ev -> IO (Maybe Error)
pageResetNavigationHistory handle = sendReceiveCommand handle "Page.resetNavigationHistory" (Nothing :: Maybe ())


-- | Parameters of the 'pageGetResourceContent' command.
data PPageGetResourceContent = PPageGetResourceContent {
   pPageGetResourceContentFrameId :: PPageGetResourceContentFrameId, -- ^ Frame id to get resource for.
   pPageGetResourceContentUrl :: PPageGetResourceContentUrl -- ^ URL of the resource to get content for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetResourceContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Page.getResourceContent'.
-- Returns content of the given resource.
-- Parameters: 'PPageGetResourceContent'
-- Returns: 'PageGetResourceContent'
pageGetResourceContent :: Handle ev -> PPageGetResourceContent -> IO (Either Error PageGetResourceContent)
pageGetResourceContent handle params = sendReceiveCommandResult handle "Page.getResourceContent" (Just params)

-- | Return type of the 'pageGetResourceContent' command.
data PageGetResourceContent = PageGetResourceContent {
   pageGetResourceContentContent :: String, -- ^ Resource content.
   pageGetResourceContentBase64Encoded :: Bool -- ^ True, if content was served as base64.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PageGetResourceContent where
   commandName _ = "Page.getResourceContent"



-- | Function for the command 'Page.getResourceTree'.
-- Returns present frame / resource tree structure.
-- Returns: 'PageGetResourceTree'
pageGetResourceTree :: Handle ev -> IO (Either Error PageGetResourceTree)
pageGetResourceTree handle = sendReceiveCommandResult handle "Page.getResourceTree" (Nothing :: Maybe ())

-- | Return type of the 'pageGetResourceTree' command.
data PageGetResourceTree = PageGetResourceTree {
   pageGetResourceTreeFrameTree :: PageFrameResourceTree -- ^ Present frame / resource tree structure.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetResourceTree where
   commandName _ = "Page.getResourceTree"



-- | Parameters of the 'pageHandleJavaScriptDialog' command.
data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
   pPageHandleJavaScriptDialogAccept :: PPageHandleJavaScriptDialogAccept, -- ^ Whether to accept or dismiss the dialog.
   pPageHandleJavaScriptDialogPromptText :: PPageHandleJavaScriptDialogPromptText -- ^ The text to enter into the dialog prompt before accepting. Used only if this is a prompt
dialog.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageHandleJavaScriptDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageHandleJavaScriptDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Page.handleJavaScriptDialog'.
-- Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).
-- Parameters: 'PPageHandleJavaScriptDialog'
pageHandleJavaScriptDialog :: Handle ev -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog handle params = sendReceiveCommand handle "Page.handleJavaScriptDialog" (Just params)


-- | Parameters of the 'pageNavigate' command.
data PPageNavigate = PPageNavigate {
   pPageNavigateUrl :: PPageNavigateUrl, -- ^ URL to navigate the page to.
   pPageNavigateReferrer :: PPageNavigateReferrer, -- ^ Referrer URL.
   pPageNavigateTransitionType :: PPageNavigateTransitionType, -- ^ Intended transition type.
   pPageNavigateFrameId :: PPageNavigateFrameId, -- ^ Frame id to navigate, if not specified navigates the top frame.
   pPageNavigateReferrerPolicy :: PPageNavigateReferrerPolicy -- ^ Referrer-policy used for the navigation.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PPageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


-- | Function for the command 'Page.navigate'.
-- Navigates current page to the given URL.
-- Parameters: 'PPageNavigate'
-- Returns: 'PageNavigate'
pageNavigate :: Handle ev -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate handle params = sendReceiveCommandResult handle "Page.navigate" (Just params)

-- | Return type of the 'pageNavigate' command.
data PageNavigate = PageNavigate {
   pageNavigateFrameId :: PageFrameId, -- ^ Frame id that has navigated (or failed to navigate)
   pageNavigateLoaderId :: Maybe NetworkLoaderId, -- ^ Loader identifier. This is omitted in case of same-document navigation,
as the previously committed loaderId would not change.
   pageNavigateErrorText :: Maybe String -- ^ User friendly error message, present if and only if navigation has failed.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageNavigate where
   commandName _ = "Page.navigate"



-- | Parameters of the 'pageNavigateToHistoryEntry' command.
data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
   pPageNavigateToHistoryEntryEntryId :: PPageNavigateToHistoryEntryEntryId -- ^ Unique id of the entry to navigate to.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigateToHistoryEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageNavigateToHistoryEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Page.navigateToHistoryEntry'.
-- Navigates current page to the given history entry.
-- Parameters: 'PPageNavigateToHistoryEntry'
pageNavigateToHistoryEntry :: Handle ev -> PPageNavigateToHistoryEntry -> IO (Maybe Error)
pageNavigateToHistoryEntry handle params = sendReceiveCommand handle "Page.navigateToHistoryEntry" (Just params)


-- | Parameters of the 'pagePrintToPdf' command.
data PPagePrintToPdfTransferMode = PPagePrintToPdfTransferModeReturnAsBase64 | PPagePrintToPdfTransferModeReturnAsStream
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPagePrintToPdfTransferMode where
   parseJSON = A.withText  "PPagePrintToPdfTransferMode"  $ \v -> do
      case v of
         "ReturnAsBase64" -> pure PPagePrintToPdfTransferModeReturnAsBase64
         "ReturnAsStream" -> pure PPagePrintToPdfTransferModeReturnAsStream
         _ -> fail "failed to parse PPagePrintToPdfTransferMode"

instance ToJSON PPagePrintToPdfTransferMode where
   toJSON v = A.String $
      case v of
         PPagePrintToPdfTransferModeReturnAsBase64 -> "ReturnAsBase64"
         PPagePrintToPdfTransferModeReturnAsStream -> "ReturnAsStream"



data PPagePrintToPdf = PPagePrintToPdf {
   pPagePrintToPdfLandscape :: PPagePrintToPdfLandscape, -- ^ Paper orientation. Defaults to false.
   pPagePrintToPdfDisplayHeaderFooter :: PPagePrintToPdfDisplayHeaderFooter, -- ^ Display header and footer. Defaults to false.
   pPagePrintToPdfPrintBackground :: PPagePrintToPdfPrintBackground, -- ^ Print background graphics. Defaults to false.
   pPagePrintToPdfScale :: PPagePrintToPdfScale, -- ^ Scale of the webpage rendering. Defaults to 1.
   pPagePrintToPdfPaperWidth :: PPagePrintToPdfPaperWidth, -- ^ Paper width in inches. Defaults to 8.5 inches.
   pPagePrintToPdfPaperHeight :: PPagePrintToPdfPaperHeight, -- ^ Paper height in inches. Defaults to 11 inches.
   pPagePrintToPdfMarginTop :: PPagePrintToPdfMarginTop, -- ^ Top margin in inches. Defaults to 1cm (~0.4 inches).
   pPagePrintToPdfMarginBottom :: PPagePrintToPdfMarginBottom, -- ^ Bottom margin in inches. Defaults to 1cm (~0.4 inches).
   pPagePrintToPdfMarginLeft :: PPagePrintToPdfMarginLeft, -- ^ Left margin in inches. Defaults to 1cm (~0.4 inches).
   pPagePrintToPdfMarginRight :: PPagePrintToPdfMarginRight, -- ^ Right margin in inches. Defaults to 1cm (~0.4 inches).
   pPagePrintToPdfPageRanges :: PPagePrintToPdfPageRanges, -- ^ Paper ranges to print, one based, e.g., '1-5, 8, 11-13'. Pages are
printed in the document order, not in the order specified, and no
more than once.
Defaults to empty string, which implies the entire document is printed.
The page numbers are quietly capped to actual page count of the
document, and ranges beyond the end of the document are ignored.
If this results in no pages to print, an error is reported.
It is an error to specify a range with start greater than end.
   pPagePrintToPdfHeaderTemplate :: PPagePrintToPdfHeaderTemplate, -- ^ HTML template for the print header. Should be valid HTML markup with following
classes used to inject printing values into them:
- `date`: formatted print date
- `title`: document title
- `url`: document location
- `pageNumber`: current page number
- `totalPages`: total pages in the document

For example, `<span class=title></span>` would generate span containing the title.
   pPagePrintToPdfFooterTemplate :: PPagePrintToPdfFooterTemplate, -- ^ HTML template for the print footer. Should use the same format as the `headerTemplate`.
   pPagePrintToPdfPreferCssPageSize :: PPagePrintToPdfPreferCssPageSize, -- ^ Whether or not to prefer page size as defined by css. Defaults to false,
in which case the content will be scaled to fit the paper size.
   pPagePrintToPdfTransferMode :: PPagePrintToPdfTransferMode -- ^ return as stream
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPdf  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'Page.printToPDF'.
-- Print page as PDF.
-- Parameters: 'PPagePrintToPdf'
-- Returns: 'PagePrintToPdf'
pagePrintToPdf :: Handle ev -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)

-- | Return type of the 'pagePrintToPdf' command.
data PagePrintToPdf = PagePrintToPdf {
   pagePrintToPdfData :: String, -- ^ Base64-encoded pdf data. Empty if |returnAsStream| is specified. (Encoded as a base64 string when passed over JSON)
   pagePrintToPdfStream :: Maybe IO.IoStreamHandle -- ^ A handle of the stream that holds resulting PDF data.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PagePrintToPdf where
   commandName _ = "Page.printToPDF"



-- | Parameters of the 'pageReload' command.
data PPageReload = PPageReload {
   pPageReloadIgnoreCache :: PPageReloadIgnoreCache, -- ^ If true, browser cache is ignored (as if the user pressed Shift+refresh).
   pPageReloadScriptToEvaluateOnLoad :: PPageReloadScriptToEvaluateOnLoad -- ^ If set, the script will be injected into all frames of the inspected page after reload.
Argument will be ignored if reloading dataURL origin.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageReload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PPageReload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


-- | Function for the command 'Page.reload'.
-- Reloads given page optionally ignoring the cache.
-- Parameters: 'PPageReload'
pageReload :: Handle ev -> PPageReload -> IO (Maybe Error)
pageReload handle params = sendReceiveCommand handle "Page.reload" (Just params)


-- | Parameters of the 'pageRemoveScriptToEvaluateOnNewDocument' command.
data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the command 'Page.removeScriptToEvaluateOnNewDocument'.
-- Removes given script from the list.
-- Parameters: 'PPageRemoveScriptToEvaluateOnNewDocument'
pageRemoveScriptToEvaluateOnNewDocument :: Handle ev -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument handle params = sendReceiveCommand handle "Page.removeScriptToEvaluateOnNewDocument" (Just params)


-- | Parameters of the 'pageScreencastFrameAck' command.
data PPageScreencastFrameAck = PPageScreencastFrameAck {
   pPageScreencastFrameAckSessionId :: PPageScreencastFrameAckSessionId -- ^ Frame number.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageScreencastFrameAck  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageScreencastFrameAck where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Page.screencastFrameAck'.
-- Acknowledges that a screencast frame has been received by the frontend.
-- Parameters: 'PPageScreencastFrameAck'
pageScreencastFrameAck :: Handle ev -> PPageScreencastFrameAck -> IO (Maybe Error)
pageScreencastFrameAck handle params = sendReceiveCommand handle "Page.screencastFrameAck" (Just params)


-- | Parameters of the 'pageSearchInResource' command.
data PPageSearchInResource = PPageSearchInResource {
   pPageSearchInResourceFrameId :: PPageSearchInResourceFrameId, -- ^ Frame id for resource to search in.
   pPageSearchInResourceUrl :: PPageSearchInResourceUrl, -- ^ URL of the resource to search in.
   pPageSearchInResourceQuery :: PPageSearchInResourceQuery, -- ^ String to search for.
   pPageSearchInResourceCaseSensitive :: PPageSearchInResourceCaseSensitive, -- ^ If true, search is case sensitive.
   pPageSearchInResourceIsRegex :: PPageSearchInResourceIsRegex -- ^ If true, treats string parameter as regex.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSearchInResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PPageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Page.searchInResource'.
-- Searches for given string in resource content.
-- Parameters: 'PPageSearchInResource'
-- Returns: 'PageSearchInResource'
pageSearchInResource :: Handle ev -> PPageSearchInResource -> IO (Either Error PageSearchInResource)
pageSearchInResource handle params = sendReceiveCommandResult handle "Page.searchInResource" (Just params)

-- | Return type of the 'pageSearchInResource' command.
data PageSearchInResource = PageSearchInResource {
   pageSearchInResourceResult :: [Debugger.DebuggerSearchMatch] -- ^ List of search matches.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageSearchInResource where
   commandName _ = "Page.searchInResource"



-- | Parameters of the 'pageSetAdBlockingEnabled' command.
data PPageSetAdBlockingEnabled = PPageSetAdBlockingEnabled {
   pPageSetAdBlockingEnabledEnabled :: PPageSetAdBlockingEnabledEnabled -- ^ Whether to block ads.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetAdBlockingEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetAdBlockingEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Page.setAdBlockingEnabled'.
-- Enable Chrome's experimental ad filter on all sites.
-- Parameters: 'PPageSetAdBlockingEnabled'
pageSetAdBlockingEnabled :: Handle ev -> PPageSetAdBlockingEnabled -> IO (Maybe Error)
pageSetAdBlockingEnabled handle params = sendReceiveCommand handle "Page.setAdBlockingEnabled" (Just params)


-- | Parameters of the 'pageSetBypassCsp' command.
data PPageSetBypassCsp = PPageSetBypassCsp {
   pPageSetBypassCspEnabled :: PPageSetBypassCspEnabled -- ^ Whether to bypass page CSP.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetBypassCsp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetBypassCsp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Page.setBypassCSP'.
-- Enable page Content Security Policy by-passing.
-- Parameters: 'PPageSetBypassCsp'
pageSetBypassCsp :: Handle ev -> PPageSetBypassCsp -> IO (Maybe Error)
pageSetBypassCsp handle params = sendReceiveCommand handle "Page.setBypassCSP" (Just params)


-- | Parameters of the 'pageGetPermissionsPolicyState' command.
data PPageGetPermissionsPolicyState = PPageGetPermissionsPolicyState {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetPermissionsPolicyState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Page.getPermissionsPolicyState'.
-- Get Permissions Policy state on given frame.
-- Parameters: 'PPageGetPermissionsPolicyState'
-- Returns: 'PageGetPermissionsPolicyState'
pageGetPermissionsPolicyState :: Handle ev -> PPageGetPermissionsPolicyState -> IO (Either Error PageGetPermissionsPolicyState)
pageGetPermissionsPolicyState handle params = sendReceiveCommandResult handle "Page.getPermissionsPolicyState" (Just params)

-- | Return type of the 'pageGetPermissionsPolicyState' command.
data PageGetPermissionsPolicyState = PageGetPermissionsPolicyState {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PageGetPermissionsPolicyState where
   commandName _ = "Page.getPermissionsPolicyState"



-- | Parameters of the 'pageGetOriginTrials' command.
data PPageGetOriginTrials = PPageGetOriginTrials {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetOriginTrials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Page.getOriginTrials'.
-- Get Origin Trials on given frame.
-- Parameters: 'PPageGetOriginTrials'
-- Returns: 'PageGetOriginTrials'
pageGetOriginTrials :: Handle ev -> PPageGetOriginTrials -> IO (Either Error PageGetOriginTrials)
pageGetOriginTrials handle params = sendReceiveCommandResult handle "Page.getOriginTrials" (Just params)

-- | Return type of the 'pageGetOriginTrials' command.
data PageGetOriginTrials = PageGetOriginTrials {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetOriginTrials where
   commandName _ = "Page.getOriginTrials"



-- | Parameters of the 'pageSetFontFamilies' command.
data PPageSetFontFamilies = PPageSetFontFamilies {
   pPageSetFontFamiliesFontFamilies :: PPageSetFontFamiliesFontFamilies, -- ^ Specifies font families to set. If a font family is not specified, it won't be changed.
   pPageSetFontFamiliesForScripts :: PPageSetFontFamiliesForScripts -- ^ Specifies font families to set for individual scripts.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Page.setFontFamilies'.
-- Set generic font families.
-- Parameters: 'PPageSetFontFamilies'
pageSetFontFamilies :: Handle ev -> PPageSetFontFamilies -> IO (Maybe Error)
pageSetFontFamilies handle params = sendReceiveCommand handle "Page.setFontFamilies" (Just params)


-- | Parameters of the 'pageSetFontSizes' command.
data PPageSetFontSizes = PPageSetFontSizes {
   pPageSetFontSizesFontSizes :: PPageSetFontSizesFontSizes -- ^ Specifies font sizes to set. If a font size is not specified, it won't be changed.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Page.setFontSizes'.
-- Set default font sizes.
-- Parameters: 'PPageSetFontSizes'
pageSetFontSizes :: Handle ev -> PPageSetFontSizes -> IO (Maybe Error)
pageSetFontSizes handle params = sendReceiveCommand handle "Page.setFontSizes" (Just params)


-- | Parameters of the 'pageSetDocumentContent' command.
data PPageSetDocumentContent = PPageSetDocumentContent {
   pPageSetDocumentContentFrameId :: PPageSetDocumentContentFrameId, -- ^ Frame id to set HTML for.
   pPageSetDocumentContentHtml :: PPageSetDocumentContentHtml -- ^ HTML content to set.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetDocumentContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageSetDocumentContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Page.setDocumentContent'.
-- Sets given markup as the document's HTML.
-- Parameters: 'PPageSetDocumentContent'
pageSetDocumentContent :: Handle ev -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent handle params = sendReceiveCommand handle "Page.setDocumentContent" (Just params)


-- | Parameters of the 'pageSetLifecycleEventsEnabled' command.
data PPageSetLifecycleEventsEnabled = PPageSetLifecycleEventsEnabled {
   pPageSetLifecycleEventsEnabledEnabled :: PPageSetLifecycleEventsEnabledEnabled -- ^ If true, starts emitting lifecycle events.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetLifecycleEventsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageSetLifecycleEventsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Page.setLifecycleEventsEnabled'.
-- Controls whether page will emit lifecycle events.
-- Parameters: 'PPageSetLifecycleEventsEnabled'
pageSetLifecycleEventsEnabled :: Handle ev -> PPageSetLifecycleEventsEnabled -> IO (Maybe Error)
pageSetLifecycleEventsEnabled handle params = sendReceiveCommand handle "Page.setLifecycleEventsEnabled" (Just params)


-- | Parameters of the 'pageStartScreencast' command.
data PPageStartScreencastFormat = PPageStartScreencastFormatJpeg | PPageStartScreencastFormatPng
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageStartScreencastFormat where
   parseJSON = A.withText  "PPageStartScreencastFormat"  $ \v -> do
      case v of
         "jpeg" -> pure PPageStartScreencastFormatJpeg
         "png" -> pure PPageStartScreencastFormatPng
         _ -> fail "failed to parse PPageStartScreencastFormat"

instance ToJSON PPageStartScreencastFormat where
   toJSON v = A.String $
      case v of
         PPageStartScreencastFormatJpeg -> "jpeg"
         PPageStartScreencastFormatPng -> "png"



data PPageStartScreencast = PPageStartScreencast {
   pPageStartScreencastFormat :: PPageStartScreencastFormat, -- ^ Image compression format.
   pPageStartScreencastQuality :: PPageStartScreencastQuality, -- ^ Compression quality from range [0..100].
   pPageStartScreencastMaxWidth :: PPageStartScreencastMaxWidth, -- ^ Maximum screenshot width.
   pPageStartScreencastMaxHeight :: PPageStartScreencastMaxHeight, -- ^ Maximum screenshot height.
   pPageStartScreencastEveryNthFrame :: PPageStartScreencastEveryNthFrame -- ^ Send every n-th frame.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageStartScreencast  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageStartScreencast where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Page.startScreencast'.
-- Starts sending each frame using the `screencastFrame` event.
-- Parameters: 'PPageStartScreencast'
pageStartScreencast :: Handle ev -> PPageStartScreencast -> IO (Maybe Error)
pageStartScreencast handle params = sendReceiveCommand handle "Page.startScreencast" (Just params)


-- | Function for the command 'Page.stopLoading'.
-- Force the page stop all navigations and pending resource fetches.
pageStopLoading :: Handle ev -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())


-- | Function for the command 'Page.crash'.
-- Crashes renderer on the IO thread, generates minidumps.
pageCrash :: Handle ev -> IO (Maybe Error)
pageCrash handle = sendReceiveCommand handle "Page.crash" (Nothing :: Maybe ())


-- | Function for the command 'Page.close'.
-- Tries to close page, running its beforeunload hooks, if any.
pageClose :: Handle ev -> IO (Maybe Error)
pageClose handle = sendReceiveCommand handle "Page.close" (Nothing :: Maybe ())


-- | Parameters of the 'pageSetWebLifecycleState' command.
data PPageSetWebLifecycleStateState = PPageSetWebLifecycleStateStateFrozen | PPageSetWebLifecycleStateStateActive
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageSetWebLifecycleStateState where
   parseJSON = A.withText  "PPageSetWebLifecycleStateState"  $ \v -> do
      case v of
         "frozen" -> pure PPageSetWebLifecycleStateStateFrozen
         "active" -> pure PPageSetWebLifecycleStateStateActive
         _ -> fail "failed to parse PPageSetWebLifecycleStateState"

instance ToJSON PPageSetWebLifecycleStateState where
   toJSON v = A.String $
      case v of
         PPageSetWebLifecycleStateStateFrozen -> "frozen"
         PPageSetWebLifecycleStateStateActive -> "active"



data PPageSetWebLifecycleState = PPageSetWebLifecycleState {
   pPageSetWebLifecycleStateState :: PPageSetWebLifecycleStateState -- ^ Target lifecycle state
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetWebLifecycleState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetWebLifecycleState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Page.setWebLifecycleState'.
-- Tries to update the web lifecycle state of the page.
-- It will transition the page to the given state according to:
-- https://github.com/WICG/web-lifecycle/
-- Parameters: 'PPageSetWebLifecycleState'
pageSetWebLifecycleState :: Handle ev -> PPageSetWebLifecycleState -> IO (Maybe Error)
pageSetWebLifecycleState handle params = sendReceiveCommand handle "Page.setWebLifecycleState" (Just params)


-- | Function for the command 'Page.stopScreencast'.
-- Stops sending each frame in the `screencastFrame`.
pageStopScreencast :: Handle ev -> IO (Maybe Error)
pageStopScreencast handle = sendReceiveCommand handle "Page.stopScreencast" (Nothing :: Maybe ())


-- | Parameters of the 'pageProduceCompilationCache' command.
data PPageProduceCompilationCache = PPageProduceCompilationCache {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageProduceCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PPageProduceCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Page.produceCompilationCache'.
-- Requests backend to produce compilation cache for the specified scripts.
-- `scripts` are appeneded to the list of scripts for which the cache
-- would be produced. The list may be reset during page navigation.
-- When script with a matching URL is encountered, the cache is optionally
-- produced upon backend discretion, based on internal heuristics.
-- See also: `Page.compilationCacheProduced`.
-- Parameters: 'PPageProduceCompilationCache'
pageProduceCompilationCache :: Handle ev -> PPageProduceCompilationCache -> IO (Maybe Error)
pageProduceCompilationCache handle params = sendReceiveCommand handle "Page.produceCompilationCache" (Just params)


-- | Parameters of the 'pageAddCompilationCache' command.
data PPageAddCompilationCache = PPageAddCompilationCache {

   pPageAddCompilationCacheData :: PPageAddCompilationCacheData -- ^ Base64-encoded data (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageAddCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Page.addCompilationCache'.
-- Seeds compilation cache for given url. Compilation cache does not survive
-- cross-process navigation.
-- Parameters: 'PPageAddCompilationCache'
pageAddCompilationCache :: Handle ev -> PPageAddCompilationCache -> IO (Maybe Error)
pageAddCompilationCache handle params = sendReceiveCommand handle "Page.addCompilationCache" (Just params)


-- | Function for the command 'Page.clearCompilationCache'.
-- Clears seeded compilation cache.
pageClearCompilationCache :: Handle ev -> IO (Maybe Error)
pageClearCompilationCache handle = sendReceiveCommand handle "Page.clearCompilationCache" (Nothing :: Maybe ())


-- | Parameters of the 'pageSetSpcTransactionMode' command.
data PPageSetSpcTransactionModeMode = PPageSetSpcTransactionModeModeNone | PPageSetSpcTransactionModeModeAutoaccept | PPageSetSpcTransactionModeModeAutoreject
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageSetSpcTransactionModeMode where
   parseJSON = A.withText  "PPageSetSpcTransactionModeMode"  $ \v -> do
      case v of
         "none" -> pure PPageSetSpcTransactionModeModeNone
         "autoaccept" -> pure PPageSetSpcTransactionModeModeAutoaccept
         "autoreject" -> pure PPageSetSpcTransactionModeModeAutoreject
         _ -> fail "failed to parse PPageSetSpcTransactionModeMode"

instance ToJSON PPageSetSpcTransactionModeMode where
   toJSON v = A.String $
      case v of
         PPageSetSpcTransactionModeModeNone -> "none"
         PPageSetSpcTransactionModeModeAutoaccept -> "autoaccept"
         PPageSetSpcTransactionModeModeAutoreject -> "autoreject"



data PPageSetSpcTransactionMode = PPageSetSpcTransactionMode {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetSpcTransactionMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PPageSetSpcTransactionMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Page.setSPCTransactionMode'.
-- Sets the Secure Payment Confirmation transaction mode.
-- https://w3c.github.io/secure-payment-confirmation/#sctn-automation-set-spc-transaction-mode
-- Parameters: 'PPageSetSpcTransactionMode'
pageSetSpcTransactionMode :: Handle ev -> PPageSetSpcTransactionMode -> IO (Maybe Error)
pageSetSpcTransactionMode handle params = sendReceiveCommand handle "Page.setSPCTransactionMode" (Just params)


-- | Parameters of the 'pageGenerateTestReport' command.
data PPageGenerateTestReport = PPageGenerateTestReport {
   pPageGenerateTestReportMessage :: PPageGenerateTestReportMessage, -- ^ Message to be displayed in the report.
   pPageGenerateTestReportGroup :: PPageGenerateTestReportGroup -- ^ Specifies the endpoint group to deliver the report to.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGenerateTestReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGenerateTestReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Page.generateTestReport'.
-- Generates a report for testing.
-- Parameters: 'PPageGenerateTestReport'
pageGenerateTestReport :: Handle ev -> PPageGenerateTestReport -> IO (Maybe Error)
pageGenerateTestReport handle params = sendReceiveCommand handle "Page.generateTestReport" (Just params)


-- | Function for the command 'Page.waitForDebugger'.
-- Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.
pageWaitForDebugger :: Handle ev -> IO (Maybe Error)
pageWaitForDebugger handle = sendReceiveCommand handle "Page.waitForDebugger" (Nothing :: Maybe ())


-- | Parameters of the 'pageSetInterceptFileChooserDialog' command.
data PPageSetInterceptFileChooserDialog = PPageSetInterceptFileChooserDialog {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetInterceptFileChooserDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PPageSetInterceptFileChooserDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Page.setInterceptFileChooserDialog'.
-- Intercept file chooser requests and transfer control to protocol clients.
-- When file chooser interception is enabled, native file chooser dialog is not shown.
-- Instead, a protocol event `Page.fileChooserOpened` is emitted.
-- Parameters: 'PPageSetInterceptFileChooserDialog'
pageSetInterceptFileChooserDialog :: Handle ev -> PPageSetInterceptFileChooserDialog -> IO (Maybe Error)
pageSetInterceptFileChooserDialog handle params = sendReceiveCommand handle "Page.setInterceptFileChooserDialog" (Just params)



-- | An internal certificate ID value.
type SecurityCertificateId = Int

-- | A description of mixed content (HTTP resources on HTTPS pages), as defined by
-- https://www.w3.org/TR/mixed-content/#categories
data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityMixedContentType where
   parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> do
      case v of
         "blockable" -> pure SecurityMixedContentTypeBlockable
         "optionally-blockable" -> pure SecurityMixedContentTypeOptionallyBlockable
         "none" -> pure SecurityMixedContentTypeNone
         _ -> fail "failed to parse SecurityMixedContentType"

instance ToJSON SecurityMixedContentType where
   toJSON v = A.String $
      case v of
         SecurityMixedContentTypeBlockable -> "blockable"
         SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
         SecurityMixedContentTypeNone -> "none"



-- | The security level of a page or resource.
data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecuritySecurityState where
   parseJSON = A.withText  "SecuritySecurityState"  $ \v -> do
      case v of
         "unknown" -> pure SecuritySecurityStateUnknown
         "neutral" -> pure SecuritySecurityStateNeutral
         "insecure" -> pure SecuritySecurityStateInsecure
         "secure" -> pure SecuritySecurityStateSecure
         "info" -> pure SecuritySecurityStateInfo
         "insecure-broken" -> pure SecuritySecurityStateInsecureBroken
         _ -> fail "failed to parse SecuritySecurityState"

instance ToJSON SecuritySecurityState where
   toJSON v = A.String $
      case v of
         SecuritySecurityStateUnknown -> "unknown"
         SecuritySecurityStateNeutral -> "neutral"
         SecuritySecurityStateInsecure -> "insecure"
         SecuritySecurityStateSecure -> "secure"
         SecuritySecurityStateInfo -> "info"
         SecuritySecurityStateInsecureBroken -> "insecure-broken"



-- | Details about the security state of the page certificate.
data SecurityCertificateSecurityState = SecurityCertificateSecurityState {
   securityCertificateSecurityStateProtocol :: SecurityCertificateSecurityStateProtocol, -- ^ Protocol name (e.g. "TLS 1.2" or "QUIC").
   securityCertificateSecurityStateKeyExchange :: SecurityCertificateSecurityStateKeyExchange, -- ^ Key Exchange used by the connection, or the empty string if not applicable.
   securityCertificateSecurityStateKeyExchangeGroup :: SecurityCertificateSecurityStateKeyExchangeGroup, -- ^ (EC)DH group used by the connection, if applicable.
   securityCertificateSecurityStateCipher :: SecurityCertificateSecurityStateCipher, -- ^ Cipher name.
   securityCertificateSecurityStateMac :: SecurityCertificateSecurityStateMac, -- ^ TLS MAC. Note that AEAD ciphers do not have separate MACs.
   securityCertificateSecurityStateCertificate :: SecurityCertificateSecurityStateCertificate, -- ^ Page certificate.
   securityCertificateSecurityStateSubjectName :: SecurityCertificateSecurityStateSubjectName, -- ^ Certificate subject name.
   securityCertificateSecurityStateIssuer :: SecurityCertificateSecurityStateIssuer, -- ^ Name of the issuing CA.
   securityCertificateSecurityStateValidFrom :: SecurityCertificateSecurityStateValidFrom, -- ^ Certificate valid from date.
   securityCertificateSecurityStateValidTo :: SecurityCertificateSecurityStateValidTo, -- ^ Certificate valid to (expiration) date
   securityCertificateSecurityStateCertificateNetworkError :: SecurityCertificateSecurityStateCertificateNetworkError, -- ^ The highest priority network error code, if the certificate has an error.
   securityCertificateSecurityStateCertificateHasWeakSignature :: SecurityCertificateSecurityStateCertificateHasWeakSignature, -- ^ True if the certificate uses a weak signature aglorithm.
   securityCertificateSecurityStateCertificateHasSha1Signature :: SecurityCertificateSecurityStateCertificateHasSha1Signature, -- ^ True if the certificate has a SHA1 signature in the chain.
   securityCertificateSecurityStateModernSsl :: SecurityCertificateSecurityStateModernSsl, -- ^ True if modern SSL
   securityCertificateSecurityStateObsoleteSslProtocol :: SecurityCertificateSecurityStateObsoleteSslProtocol, -- ^ True if the connection is using an obsolete SSL protocol.
   securityCertificateSecurityStateObsoleteSslKeyExchange :: SecurityCertificateSecurityStateObsoleteSslKeyExchange, -- ^ True if the connection is using an obsolete SSL key exchange.
   securityCertificateSecurityStateObsoleteSslCipher :: SecurityCertificateSecurityStateObsoleteSslCipher, -- ^ True if the connection is using an obsolete SSL cipher.
   securityCertificateSecurityStateObsoleteSslSignature :: SecurityCertificateSecurityStateObsoleteSslSignature -- ^ True if the connection is using an obsolete SSL signature.
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityCertificateSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecurityCertificateSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type 'Security.SafetyTipStatus' .
data SecuritySafetyTipStatus = SecuritySafetyTipStatusBadReputation | SecuritySafetyTipStatusLookalike
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecuritySafetyTipStatus where
   parseJSON = A.withText  "SecuritySafetyTipStatus"  $ \v -> do
      case v of
         "badReputation" -> pure SecuritySafetyTipStatusBadReputation
         "lookalike" -> pure SecuritySafetyTipStatusLookalike
         _ -> fail "failed to parse SecuritySafetyTipStatus"

instance ToJSON SecuritySafetyTipStatus where
   toJSON v = A.String $
      case v of
         SecuritySafetyTipStatusBadReputation -> "badReputation"
         SecuritySafetyTipStatusLookalike -> "lookalike"



-- | Type 'Security.SafetyTipInfo' .
data SecuritySafetyTipInfo = SecuritySafetyTipInfo {
   securitySafetyTipInfoSafetyTipStatus :: SecuritySafetyTipInfoSafetyTipStatus, -- ^ Describes whether the page triggers any safety tips or reputation warnings. Default is unknown.
   securitySafetyTipInfoSafeUrl :: SecuritySafetyTipInfoSafeUrl -- ^ The URL the safety tip suggested ("Did you mean?"). Only filled in for lookalike matches.
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySafetyTipInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  SecuritySafetyTipInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Security state information about the page.
data SecurityVisibleSecurityState = SecurityVisibleSecurityState {
   securityVisibleSecurityStateSecurityState :: SecurityVisibleSecurityStateSecurityState, -- ^ The security level of the page.
   securityVisibleSecurityStateCertificateSecurityState :: SecurityVisibleSecurityStateCertificateSecurityState, -- ^ Security state details about the page certificate.
   securityVisibleSecurityStateSafetyTipInfo :: SecurityVisibleSecurityStateSafetyTipInfo, -- ^ The type of Safety Tip triggered on the page. Note that this field will be set even if the Safety Tip UI was not actually shown.
   securityVisibleSecurityStateSecurityStateIssueIds :: SecurityVisibleSecurityStateSecurityStateIssueIds -- ^ Array of security state issues ids.
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | An explanation of an factor contributing to the security state.
data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
   securitySecurityStateExplanationSecurityState :: SecuritySecurityStateExplanationSecurityState, -- ^ Security state representing the severity of the factor being explained.
   securitySecurityStateExplanationTitle :: SecuritySecurityStateExplanationTitle, -- ^ Title describing the type of factor.
   securitySecurityStateExplanationSummary :: SecuritySecurityStateExplanationSummary, -- ^ Short phrase describing the type of factor.
   securitySecurityStateExplanationDescription :: SecuritySecurityStateExplanationDescription, -- ^ Full text explanation of the factor.
   securitySecurityStateExplanationMixedContentType :: SecuritySecurityStateExplanationMixedContentType, -- ^ The type of mixed content described by the explanation.
   securitySecurityStateExplanationCertificate :: SecuritySecurityStateExplanationCertificate, -- ^ Page certificate.
   securitySecurityStateExplanationRecommendations :: SecuritySecurityStateExplanationRecommendations -- ^ Recommendations to fix any issues.
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySecurityStateExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecuritySecurityStateExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | The action to take when a certificate error occurs. continue will continue processing the
-- request and cancel will cancel the request.
data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
   deriving (Ord, Eq, Show, Read)
instance FromJSON SecurityCertificateErrorAction where
   parseJSON = A.withText  "SecurityCertificateErrorAction"  $ \v -> do
      case v of
         "continue" -> pure SecurityCertificateErrorActionContinue
         "cancel" -> pure SecurityCertificateErrorActionCancel
         _ -> fail "failed to parse SecurityCertificateErrorAction"

instance ToJSON SecurityCertificateErrorAction where
   toJSON v = A.String $
      case v of
         SecurityCertificateErrorActionContinue -> "continue"
         SecurityCertificateErrorActionCancel -> "cancel"





-- | Type of the 'Security.visibleSecurityStateChanged' event.
data SecurityVisibleSecurityStateChanged = SecurityVisibleSecurityStateChanged {
   securityVisibleSecurityStateChangedVisibleSecurityState :: SecurityVisibleSecurityStateChangedVisibleSecurityState -- ^ Security state information about the page.
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityStateChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityStateChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }





-- | Function for the command 'Security.disable'.
-- Disables tracking security state changes.
securityDisable :: Handle ev -> IO (Maybe Error)
securityDisable handle = sendReceiveCommand handle "Security.disable" (Nothing :: Maybe ())


-- | Function for the command 'Security.enable'.
-- Enables tracking security state changes.
securityEnable :: Handle ev -> IO (Maybe Error)
securityEnable handle = sendReceiveCommand handle "Security.enable" (Nothing :: Maybe ())


-- | Parameters of the 'securitySetIgnoreCertificateErrors' command.
data PSecuritySetIgnoreCertificateErrors = PSecuritySetIgnoreCertificateErrors {
   pSecuritySetIgnoreCertificateErrorsIgnore :: PSecuritySetIgnoreCertificateErrorsIgnore -- ^ If true, all certificate errors will be ignored.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PSecuritySetIgnoreCertificateErrors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PSecuritySetIgnoreCertificateErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'Security.setIgnoreCertificateErrors'.
-- Enable/disable whether all certificate errors should be ignored.
-- Parameters: 'PSecuritySetIgnoreCertificateErrors'
securitySetIgnoreCertificateErrors :: Handle ev -> PSecuritySetIgnoreCertificateErrors -> IO (Maybe Error)
securitySetIgnoreCertificateErrors handle params = sendReceiveCommand handle "Security.setIgnoreCertificateErrors" (Just params)



