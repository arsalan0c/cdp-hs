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
  -- | `Node`'s nodeType.
  domBackendNodeNodeType :: Int,
  -- | `Node`'s nodeName.
  domBackendNodeNodeName :: String,
  domBackendNodeBackendNodeId :: DomBackendNodeId
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
  -- | Node identifier that is passed into the rest of the DOM messages as the `nodeId`. Backend
  -- will only push node with given `id` once. It is aware of all requested nodes and will only
  -- fire DOM events for nodes known to the client.
  domNodeNodeId :: DomNodeId,
  -- | The id of the parent node if any.
  domNodeParentId :: Maybe DomNodeId,
  -- | The BackendNodeId for this node.
  domNodeBackendNodeId :: DomBackendNodeId,
  -- | `Node`'s nodeType.
  domNodeNodeType :: Int,
  -- | `Node`'s nodeName.
  domNodeNodeName :: String,
  -- | `Node`'s localName.
  domNodeLocalName :: String,
  -- | `Node`'s nodeValue.
  domNodeNodeValue :: String,
  -- | Child count for `Container` nodes.
  domNodeChildNodeCount :: Maybe Int,
  -- | Child nodes of this node when requested with children.
  domNodeChildren :: Maybe [DomNode],
  -- | Attributes of the `Element` node in the form of flat array `[name1, value1, name2, value2]`.
  domNodeAttributes :: Maybe [String],
  -- | Document URL that `Document` or `FrameOwner` node points to.
  domNodeDocumentUrl :: Maybe String,
  -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
  domNodeBaseUrl :: Maybe String,
  -- | `DocumentType`'s publicId.
  domNodePublicId :: Maybe String,
  -- | `DocumentType`'s systemId.
  domNodeSystemId :: Maybe String,
  -- | `DocumentType`'s internalSubset.
  domNodeInternalSubset :: Maybe String,
  -- | `Document`'s XML version in case of XML documents.
  domNodeXmlVersion :: Maybe String,
  -- | `Attr`'s name.
  domNodeName :: Maybe String,
  -- | `Attr`'s value.
  domNodeValue :: Maybe String,
  -- | Pseudo element type for this node.
  domNodePseudoType :: Maybe DomPseudoType,
  -- | Shadow root type.
  domNodeShadowRootType :: Maybe DomShadowRootType,
  -- | Frame ID for frame owner elements.
  domNodeFrameId :: Maybe PageFrameId,
  -- | Content document for frame owner elements.
  domNodeContentDocument :: Maybe DomNode,
  -- | Shadow root list for given element host.
  domNodeShadowRoots :: Maybe [DomNode],
  -- | Content document fragment for template elements.
  domNodeTemplateContent :: Maybe DomNode,
  -- | Pseudo elements associated with this node.
  domNodePseudoElements :: Maybe [DomNode],
  -- | Distributed nodes for given insertion point.
  domNodeDistributedNodes :: Maybe [DomBackendNode],
  -- | Whether the node is SVG.
  domNodeIsSvg :: Maybe Bool,
  domNodeCompatibilityMode :: Maybe DomCompatibilityMode,
  domNodeAssignedSlot :: Maybe DomBackendNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | A structure holding an RGBA color.
data DomRgba = DomRgba {
  -- | The red component, in the [0-255] range.
  domRgbaR :: Int,
  -- | The green component, in the [0-255] range.
  domRgbaG :: Int,
  -- | The blue component, in the [0-255] range.
  domRgbaB :: Int,
  -- | The alpha component, in the [0-1] range (default: 1).
  domRgbaA :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRgba  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRgba where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | An array of quad vertices, x immediately followed by y for each point, points clock-wise.
type DomQuad = [Double]

-- | Box model.
data DomBoxModel = DomBoxModel {
  -- | Content box
  domBoxModelContent :: DomQuad,
  -- | Padding box
  domBoxModelPadding :: DomQuad,
  -- | Border box
  domBoxModelBorder :: DomQuad,
  -- | Margin box
  domBoxModelMargin :: DomQuad,
  -- | Node width
  domBoxModelWidth :: Int,
  -- | Node height
  domBoxModelHeight :: Int,
  -- | Shape outside coordinates
  domBoxModelShapeOutside :: Maybe DomShapeOutsideInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  DomBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | CSS Shape Outside details.
data DomShapeOutsideInfo = DomShapeOutsideInfo {
  -- | Shape bounds
  domShapeOutsideInfoBounds :: DomQuad,
  -- | Shape coordinate details
  domShapeOutsideInfoShape :: [Int],
  -- | Margin shape bounds
  domShapeOutsideInfoMarginShape :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShapeOutsideInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShapeOutsideInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Rectangle.
data DomRect = DomRect {
  -- | X coordinate
  domRectX :: Double,
  -- | Y coordinate
  domRectY :: Double,
  -- | Rectangle width
  domRectWidth :: Double,
  -- | Rectangle height
  domRectHeight :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | Type 'DOM.CSSComputedStyleProperty' .
data DomCssComputedStyleProperty = DomCssComputedStyleProperty {
  -- | Computed style property name.
  domCssComputedStylePropertyName :: String,
  -- | Computed style property value.
  domCssComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Type of the 'DOM.attributeModified' event.
data DomAttributeModified = DomAttributeModified {
  -- | Id of the node that has changed.
  domAttributeModifiedNodeId :: DomNodeId,
  -- | Attribute name.
  domAttributeModifiedName :: String,
  -- | Attribute value.
  domAttributeModifiedValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomAttributeModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'DOM.attributeRemoved' event.
data DomAttributeRemoved = DomAttributeRemoved {
  -- | Id of the node that has changed.
  domAttributeRemovedNodeId :: DomNodeId,
  -- | A ttribute name.
  domAttributeRemovedName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomAttributeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.characterDataModified' event.
data DomCharacterDataModified = DomCharacterDataModified {
  -- | Id of the node that has changed.
  domCharacterDataModifiedNodeId :: DomNodeId,
  -- | New text value.
  domCharacterDataModifiedCharacterData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCharacterDataModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomCharacterDataModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'DOM.childNodeCountUpdated' event.
data DomChildNodeCountUpdated = DomChildNodeCountUpdated {
  -- | Id of the node that has changed.
  domChildNodeCountUpdatedNodeId :: DomNodeId,
  -- | New node count.
  domChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeCountUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeCountUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'DOM.childNodeInserted' event.
data DomChildNodeInserted = DomChildNodeInserted {
  -- | Id of the node that has changed.
  domChildNodeInsertedParentNodeId :: DomNodeId,
  -- | If of the previous siblint.
  domChildNodeInsertedPreviousNodeId :: DomNodeId,
  -- | Inserted node data.
  domChildNodeInsertedNode :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeInserted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeInserted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'DOM.childNodeRemoved' event.
data DomChildNodeRemoved = DomChildNodeRemoved {
  -- | Parent id.
  domChildNodeRemovedParentNodeId :: DomNodeId,
  -- | Id of the node that has been removed.
  domChildNodeRemovedNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.distributedNodesUpdated' event.
data DomDistributedNodesUpdated = DomDistributedNodesUpdated {
  -- | Insertion point where distributed nodes were updated.
  domDistributedNodesUpdatedInsertionPointId :: DomNodeId,
  -- | Distributed nodes for given insertion point.
  domDistributedNodesUpdatedDistributedNodes :: [DomBackendNode]
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
  -- | Ids of the nodes for which the inline styles have been invalidated.
  domInlineStyleInvalidatedNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomInlineStyleInvalidated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomInlineStyleInvalidated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'DOM.pseudoElementAdded' event.
data DomPseudoElementAdded = DomPseudoElementAdded {
  -- | Pseudo element's parent element id.
  domPseudoElementAddedParentId :: DomNodeId,
  -- | The added pseudo element.
  domPseudoElementAddedPseudoElement :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'DOM.pseudoElementRemoved' event.
data DomPseudoElementRemoved = DomPseudoElementRemoved {
  -- | Pseudo element's parent element id.
  domPseudoElementRemovedParentId :: DomNodeId,
  -- | The removed pseudo element id.
  domPseudoElementRemovedPseudoElementId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'DOM.setChildNodes' event.
data DomSetChildNodes = DomSetChildNodes {
  -- | Parent node id to populate with children.
  domSetChildNodesParentId :: DomNodeId,
  -- | Child nodes array.
  domSetChildNodesNodes :: [DomNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DomSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type of the 'DOM.shadowRootPopped' event.
data DomShadowRootPopped = DomShadowRootPopped {
  -- | Host element id.
  domShadowRootPoppedHostId :: DomNodeId,
  -- | Shadow root id.
  domShadowRootPoppedRootId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPopped  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPopped where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'DOM.shadowRootPushed' event.
data DomShadowRootPushed = DomShadowRootPushed {
  -- | Host element id.
  domShadowRootPushedHostId :: DomNodeId,
  -- | Shadow root.
  domShadowRootPushedRoot :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPushed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPushed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Parameters of the 'domCollectClassNamesFromSubtree' command.
data PDomCollectClassNamesFromSubtree = PDomCollectClassNamesFromSubtree {
  -- | Id of the node to collect class names.
  pDomCollectClassNamesFromSubtreeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCollectClassNamesFromSubtree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PDomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'DOM.collectClassNamesFromSubtree' command.
-- Collects class names for the node with given id and all of it's child nodes.
-- Parameters: 'PDomCollectClassNamesFromSubtree'
-- Returns: 'DomCollectClassNamesFromSubtree'
domCollectClassNamesFromSubtree :: Handle ev -> PDomCollectClassNamesFromSubtree -> IO (Either Error DomCollectClassNamesFromSubtree)
domCollectClassNamesFromSubtree handle params = sendReceiveCommandResult handle "DOM.collectClassNamesFromSubtree" (Just params)

-- | Return type of the 'domCollectClassNamesFromSubtree' command.
data DomCollectClassNamesFromSubtree = DomCollectClassNamesFromSubtree {
  -- | Class name list.
  domCollectClassNamesFromSubtreeClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command DomCollectClassNamesFromSubtree where
   commandName _ = "DOM.collectClassNamesFromSubtree"



-- | Parameters of the 'domCopyTo' command.
data PDomCopyTo = PDomCopyTo {
  -- | Id of the node to copy.
  pDomCopyToNodeId :: DomNodeId,
  -- | Id of the element to drop the copy into.
  pDomCopyToTargetNodeId :: DomNodeId,
  -- | Drop the copy before this node (if absent, the copy becomes the last child of
  -- `targetNodeId`).
  pDomCopyToInsertBeforeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCopyTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the 'DOM.copyTo' command.
-- Creates a deep copy of the specified node and places it into the target container before the
-- given anchor.
-- Parameters: 'PDomCopyTo'
-- Returns: 'DomCopyTo'
domCopyTo :: Handle ev -> PDomCopyTo -> IO (Either Error DomCopyTo)
domCopyTo handle params = sendReceiveCommandResult handle "DOM.copyTo" (Just params)

-- | Return type of the 'domCopyTo' command.
data DomCopyTo = DomCopyTo {
  -- | Id of the node clone.
  domCopyToNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomCopyTo where
   commandName _ = "DOM.copyTo"



-- | Parameters of the 'domDescribeNode' command.
data PDomDescribeNode = PDomDescribeNode {
  -- | Identifier of the node.
  pDomDescribeNodeNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomDescribeNodeBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomDescribeNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
  -- entire subtree or provide an integer larger than 0.
  pDomDescribeNodeDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
  -- (default is false).
  pDomDescribeNodePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDescribeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOM.describeNode' command.
-- Describes node given its id, does not require domain to be enabled. Does not start tracking any
-- objects, can be used for automation.
-- Parameters: 'PDomDescribeNode'
-- Returns: 'DomDescribeNode'
domDescribeNode :: Handle ev -> PDomDescribeNode -> IO (Either Error DomDescribeNode)
domDescribeNode handle params = sendReceiveCommandResult handle "DOM.describeNode" (Just params)

-- | Return type of the 'domDescribeNode' command.
data DomDescribeNode = DomDescribeNode {
  -- | Node description.
  domDescribeNodeNode :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomDescribeNode where
   commandName _ = "DOM.describeNode"



-- | Parameters of the 'domScrollIntoViewIfNeeded' command.
data PDomScrollIntoViewIfNeeded = PDomScrollIntoViewIfNeeded {
  -- | Identifier of the node.
  pDomScrollIntoViewIfNeededNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomScrollIntoViewIfNeededBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomScrollIntoViewIfNeededObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | The rect to be scrolled into view, relative to the node's border box, in CSS pixels.
  -- When omitted, center of the node will be used, similar to Element.scrollIntoView.
  pDomScrollIntoViewIfNeededRect :: Maybe DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomScrollIntoViewIfNeeded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PDomScrollIntoViewIfNeeded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'DOM.scrollIntoViewIfNeeded' command.
-- Scrolls the specified rect of the given node into view if not already visible.
-- Note: exactly one between nodeId, backendNodeId and objectId should be passed
-- to identify the node.
-- Parameters: 'PDomScrollIntoViewIfNeeded'
domScrollIntoViewIfNeeded :: Handle ev -> PDomScrollIntoViewIfNeeded -> IO (Maybe Error)
domScrollIntoViewIfNeeded handle params = sendReceiveCommand handle "DOM.scrollIntoViewIfNeeded" (Just params)


-- | Function for the 'DOM.disable' command.
-- Disables DOM agent for the given page.
domDisable :: Handle ev -> IO (Maybe Error)
domDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())


-- | Parameters of the 'domDiscardSearchResults' command.
data PDomDiscardSearchResults = PDomDiscardSearchResults {
  -- | Unique search session identifier.
  pDomDiscardSearchResultsSearchId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDiscardSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDomDiscardSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'DOM.discardSearchResults' command.
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
  -- | Whether to include whitespaces in the children array of returned Nodes.
  pDomEnableIncludeWhitespace :: PDomEnableIncludeWhitespace
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the 'DOM.enable' command.
-- Enables DOM agent for the given page.
-- Parameters: 'PDomEnable'
domEnable :: Handle ev -> PDomEnable -> IO (Maybe Error)
domEnable handle params = sendReceiveCommand handle "DOM.enable" (Just params)


-- | Parameters of the 'domFocus' command.
data PDomFocus = PDomFocus {
  -- | Identifier of the node.
  pDomFocusNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomFocusBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomFocusObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomFocus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PDomFocus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


-- | Function for the 'DOM.focus' command.
-- Focuses the given element.
-- Parameters: 'PDomFocus'
domFocus :: Handle ev -> PDomFocus -> IO (Maybe Error)
domFocus handle params = sendReceiveCommand handle "DOM.focus" (Just params)


-- | Parameters of the 'domGetAttributes' command.
data PDomGetAttributes = PDomGetAttributes {
  -- | Id of the node to retrieve attibutes for.
  pDomGetAttributesNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetAttributes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'DOM.getAttributes' command.
-- Returns attributes for the specified node.
-- Parameters: 'PDomGetAttributes'
-- Returns: 'DomGetAttributes'
domGetAttributes :: Handle ev -> PDomGetAttributes -> IO (Either Error DomGetAttributes)
domGetAttributes handle params = sendReceiveCommandResult handle "DOM.getAttributes" (Just params)

-- | Return type of the 'domGetAttributes' command.
data DomGetAttributes = DomGetAttributes {
  -- | An interleaved array of node attribute names and values.
  domGetAttributesAttributes :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetAttributes where
   commandName _ = "DOM.getAttributes"



-- | Parameters of the 'domGetBoxModel' command.
data PDomGetBoxModel = PDomGetBoxModel {
  -- | Identifier of the node.
  pDomGetBoxModelNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomGetBoxModelBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomGetBoxModelObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.getBoxModel' command.
-- Returns boxes for the given node.
-- Parameters: 'PDomGetBoxModel'
-- Returns: 'DomGetBoxModel'
domGetBoxModel :: Handle ev -> PDomGetBoxModel -> IO (Either Error DomGetBoxModel)
domGetBoxModel handle params = sendReceiveCommandResult handle "DOM.getBoxModel" (Just params)

-- | Return type of the 'domGetBoxModel' command.
data DomGetBoxModel = DomGetBoxModel {
  -- | Box model for the node.
  domGetBoxModelModel :: DomBoxModel
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetBoxModel where
   commandName _ = "DOM.getBoxModel"



-- | Parameters of the 'domGetContentQuads' command.
data PDomGetContentQuads = PDomGetContentQuads {
  -- | Identifier of the node.
  pDomGetContentQuadsNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomGetContentQuadsBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomGetContentQuadsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContentQuads  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'DOM.getContentQuads' command.
-- Returns quads that describe node position on the page. This method
-- might return multiple quads for inline nodes.
-- Parameters: 'PDomGetContentQuads'
-- Returns: 'DomGetContentQuads'
domGetContentQuads :: Handle ev -> PDomGetContentQuads -> IO (Either Error DomGetContentQuads)
domGetContentQuads handle params = sendReceiveCommandResult handle "DOM.getContentQuads" (Just params)

-- | Return type of the 'domGetContentQuads' command.
data DomGetContentQuads = DomGetContentQuads {
  -- | Quads that describe node layout relative to viewport.
  domGetContentQuadsQuads :: [DomQuad]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command DomGetContentQuads where
   commandName _ = "DOM.getContentQuads"



-- | Parameters of the 'domGetDocument' command.
data PDomGetDocument = PDomGetDocument {
  -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
  -- entire subtree or provide an integer larger than 0.
  pDomGetDocumentDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
  -- (default is false).
  pDomGetDocumentPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.getDocument' command.
-- Returns the root DOM node (and optionally the subtree) to the caller.
-- Parameters: 'PDomGetDocument'
-- Returns: 'DomGetDocument'
domGetDocument :: Handle ev -> PDomGetDocument -> IO (Either Error DomGetDocument)
domGetDocument handle params = sendReceiveCommandResult handle "DOM.getDocument" (Just params)

-- | Return type of the 'domGetDocument' command.
data DomGetDocument = DomGetDocument {
  -- | Resulting node.
  domGetDocumentRoot :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetDocument where
   commandName _ = "DOM.getDocument"



-- | Parameters of the 'domGetNodesForSubtreeByStyle' command.
data PDomGetNodesForSubtreeByStyle = PDomGetNodesForSubtreeByStyle {
  -- | Node ID pointing to the root of a subtree.
  pDomGetNodesForSubtreeByStyleNodeId :: DomNodeId,
  -- | The style to filter nodes by (includes nodes if any of properties matches).
  pDomGetNodesForSubtreeByStyleComputedStyles :: [DomCssComputedStyleProperty],
  -- | Whether or not iframes and shadow roots in the same target should be traversed when returning the
  -- results (default is false).
  pDomGetNodesForSubtreeByStylePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodesForSubtreeByStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'DOM.getNodesForSubtreeByStyle' command.
-- Finds nodes with a given computed style in a subtree.
-- Parameters: 'PDomGetNodesForSubtreeByStyle'
-- Returns: 'DomGetNodesForSubtreeByStyle'
domGetNodesForSubtreeByStyle :: Handle ev -> PDomGetNodesForSubtreeByStyle -> IO (Either Error DomGetNodesForSubtreeByStyle)
domGetNodesForSubtreeByStyle handle params = sendReceiveCommandResult handle "DOM.getNodesForSubtreeByStyle" (Just params)

-- | Return type of the 'domGetNodesForSubtreeByStyle' command.
data DomGetNodesForSubtreeByStyle = DomGetNodesForSubtreeByStyle {
  -- | Resulting nodes.
  domGetNodesForSubtreeByStyleNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomGetNodesForSubtreeByStyle where
   commandName _ = "DOM.getNodesForSubtreeByStyle"



-- | Parameters of the 'domGetNodeForLocation' command.
data PDomGetNodeForLocation = PDomGetNodeForLocation {
  -- | X coordinate.
  pDomGetNodeForLocationX :: Int,
  -- | Y coordinate.
  pDomGetNodeForLocationY :: Int,
  -- | False to skip to the nearest non-UA shadow root ancestor (default: false).
  pDomGetNodeForLocationIncludeUserAgentShadowDom :: Maybe Bool,
  -- | Whether to ignore pointer-events: none on elements and hit test them.
  pDomGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeForLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'DOM.getNodeForLocation' command.
-- Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
-- either returned or not.
-- Parameters: 'PDomGetNodeForLocation'
-- Returns: 'DomGetNodeForLocation'
domGetNodeForLocation :: Handle ev -> PDomGetNodeForLocation -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation handle params = sendReceiveCommandResult handle "DOM.getNodeForLocation" (Just params)

-- | Return type of the 'domGetNodeForLocation' command.
data DomGetNodeForLocation = DomGetNodeForLocation {
  -- | Resulting node.
  domGetNodeForLocationBackendNodeId :: DomBackendNodeId,
  -- | Frame this node belongs to.
  domGetNodeForLocationFrameId :: PageFrameId,
  -- | Id of the node at given coordinates, only when enabled and requested document.
  domGetNodeForLocationNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeForLocation where
   commandName _ = "DOM.getNodeForLocation"



-- | Parameters of the 'domGetOuterHtml' command.
data PDomGetOuterHtml = PDomGetOuterHtml {
  -- | Identifier of the node.
  pDomGetOuterHtmlNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomGetOuterHtmlBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomGetOuterHtmlObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOM.getOuterHTML' command.
-- Returns node's HTML markup.
-- Parameters: 'PDomGetOuterHtml'
-- Returns: 'DomGetOuterHtml'
domGetOuterHtml :: Handle ev -> PDomGetOuterHtml -> IO (Either Error DomGetOuterHtml)
domGetOuterHtml handle params = sendReceiveCommandResult handle "DOM.getOuterHTML" (Just params)

-- | Return type of the 'domGetOuterHtml' command.
data DomGetOuterHtml = DomGetOuterHtml {
  -- | Outer HTML markup.
  domGetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomGetOuterHtml where
   commandName _ = "DOM.getOuterHTML"



-- | Parameters of the 'domGetRelayoutBoundary' command.
data PDomGetRelayoutBoundary = PDomGetRelayoutBoundary {
  -- | Id of the node.
  pDomGetRelayoutBoundaryNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetRelayoutBoundary  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'DOM.getRelayoutBoundary' command.
-- Returns the id of the nearest ancestor that is a relayout boundary.
-- Parameters: 'PDomGetRelayoutBoundary'
-- Returns: 'DomGetRelayoutBoundary'
domGetRelayoutBoundary :: Handle ev -> PDomGetRelayoutBoundary -> IO (Either Error DomGetRelayoutBoundary)
domGetRelayoutBoundary handle params = sendReceiveCommandResult handle "DOM.getRelayoutBoundary" (Just params)

-- | Return type of the 'domGetRelayoutBoundary' command.
data DomGetRelayoutBoundary = DomGetRelayoutBoundary {
  -- | Relayout boundary node id for the given node.
  domGetRelayoutBoundaryNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetRelayoutBoundary where
   commandName _ = "DOM.getRelayoutBoundary"



-- | Parameters of the 'domGetSearchResults' command.
data PDomGetSearchResults = PDomGetSearchResults {
  -- | Unique search session identifier.
  pDomGetSearchResultsSearchId :: String,
  -- | Start index of the search result to be returned.
  pDomGetSearchResultsFromIndex :: Int,
  -- | End index of the search result to be returned.
  pDomGetSearchResultsToIndex :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'DOM.getSearchResults' command.
-- Returns search results from given `fromIndex` to given `toIndex` from the search with the given
-- identifier.
-- Parameters: 'PDomGetSearchResults'
-- Returns: 'DomGetSearchResults'
domGetSearchResults :: Handle ev -> PDomGetSearchResults -> IO (Either Error DomGetSearchResults)
domGetSearchResults handle params = sendReceiveCommandResult handle "DOM.getSearchResults" (Just params)

-- | Return type of the 'domGetSearchResults' command.
data DomGetSearchResults = DomGetSearchResults {
  -- | Ids of the search result nodes.
  domGetSearchResultsNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomGetSearchResults where
   commandName _ = "DOM.getSearchResults"



-- | Function for the 'DOM.hideHighlight' command.
-- Hides any highlight.
domHideHighlight :: Handle ev -> IO (Maybe Error)
domHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())


-- | Function for the 'DOM.highlightNode' command.
-- Highlights DOM node.
domHighlightNode :: Handle ev -> IO (Maybe Error)
domHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())


-- | Function for the 'DOM.highlightRect' command.
-- Highlights given rectangle.
domHighlightRect :: Handle ev -> IO (Maybe Error)
domHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())


-- | Function for the 'DOM.markUndoableState' command.
-- Marks last undoable state.
domMarkUndoableState :: Handle ev -> IO (Maybe Error)
domMarkUndoableState handle = sendReceiveCommand handle "DOM.markUndoableState" (Nothing :: Maybe ())


-- | Parameters of the 'domMoveTo' command.
data PDomMoveTo = PDomMoveTo {
  -- | Id of the node to move.
  pDomMoveToNodeId :: DomNodeId,
  -- | Id of the element to drop the moved node into.
  pDomMoveToTargetNodeId :: DomNodeId,
  -- | Drop node before this one (if absent, the moved node becomes the last child of
  -- `targetNodeId`).
  pDomMoveToInsertBeforeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomMoveTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Function for the 'DOM.moveTo' command.
-- Moves node into the new container, places it before the given anchor.
-- Parameters: 'PDomMoveTo'
-- Returns: 'DomMoveTo'
domMoveTo :: Handle ev -> PDomMoveTo -> IO (Either Error DomMoveTo)
domMoveTo handle params = sendReceiveCommandResult handle "DOM.moveTo" (Just params)

-- | Return type of the 'domMoveTo' command.
data DomMoveTo = DomMoveTo {
  -- | New id of the moved node.
  domMoveToNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomMoveTo where
   commandName _ = "DOM.moveTo"



-- | Parameters of the 'domPerformSearch' command.
data PDomPerformSearch = PDomPerformSearch {
  -- | Plain text or query selector or XPath search query.
  pDomPerformSearchQuery :: String,
  -- | True to search in user agent shadow DOM.
  pDomPerformSearchIncludeUserAgentShadowDom :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPerformSearch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'DOM.performSearch' command.
-- Searches for a given string in the DOM tree. Use `getSearchResults` to access search results or
-- `cancelSearch` to end this search session.
-- Parameters: 'PDomPerformSearch'
-- Returns: 'DomPerformSearch'
domPerformSearch :: Handle ev -> PDomPerformSearch -> IO (Either Error DomPerformSearch)
domPerformSearch handle params = sendReceiveCommandResult handle "DOM.performSearch" (Just params)

-- | Return type of the 'domPerformSearch' command.
data DomPerformSearch = DomPerformSearch {
  -- | Unique search session identifier.
  domPerformSearchSearchId :: String,
  -- | Number of search results.
  domPerformSearchResultCount :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomPerformSearch where
   commandName _ = "DOM.performSearch"



-- | Parameters of the 'domPushNodeByPathToFrontend' command.
data PDomPushNodeByPathToFrontend = PDomPushNodeByPathToFrontend {
  -- | Path to node in the proprietary format.
  pDomPushNodeByPathToFrontendPath :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodeByPathToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'DOM.pushNodeByPathToFrontend' command.
-- Requests that the node is sent to the caller given its path. // FIXME, use XPath
-- Parameters: 'PDomPushNodeByPathToFrontend'
-- Returns: 'DomPushNodeByPathToFrontend'
domPushNodeByPathToFrontend :: Handle ev -> PDomPushNodeByPathToFrontend -> IO (Either Error DomPushNodeByPathToFrontend)
domPushNodeByPathToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodeByPathToFrontend" (Just params)

-- | Return type of the 'domPushNodeByPathToFrontend' command.
data DomPushNodeByPathToFrontend = DomPushNodeByPathToFrontend {
  -- | Id of the node for given path.
  domPushNodeByPathToFrontendNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DomPushNodeByPathToFrontend where
   commandName _ = "DOM.pushNodeByPathToFrontend"



-- | Parameters of the 'domPushNodesByBackendIdsToFrontend' command.
data PDomPushNodesByBackendIdsToFrontend = PDomPushNodesByBackendIdsToFrontend {
  -- | The array of backend node ids.
  pDomPushNodesByBackendIdsToFrontendBackendNodeIds :: [DomBackendNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodesByBackendIdsToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'DOM.pushNodesByBackendIdsToFrontend' command.
-- Requests that a batch of nodes is sent to the caller given their backend node ids.
-- Parameters: 'PDomPushNodesByBackendIdsToFrontend'
-- Returns: 'DomPushNodesByBackendIdsToFrontend'
domPushNodesByBackendIdsToFrontend :: Handle ev -> PDomPushNodesByBackendIdsToFrontend -> IO (Either Error DomPushNodesByBackendIdsToFrontend)
domPushNodesByBackendIdsToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodesByBackendIdsToFrontend" (Just params)

-- | Return type of the 'domPushNodesByBackendIdsToFrontend' command.
data DomPushNodesByBackendIdsToFrontend = DomPushNodesByBackendIdsToFrontend {
  -- | The array of ids of pushed nodes that correspond to the backend ids specified in
  -- backendNodeIds.
  domPushNodesByBackendIdsToFrontendNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command DomPushNodesByBackendIdsToFrontend where
   commandName _ = "DOM.pushNodesByBackendIdsToFrontend"



-- | Parameters of the 'domQuerySelector' command.
data PDomQuerySelector = PDomQuerySelector {
  -- | Id of the node to query upon.
  pDomQuerySelectorNodeId :: DomNodeId,
  -- | Selector string.
  pDomQuerySelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'DOM.querySelector' command.
-- Executes `querySelector` on a given node.
-- Parameters: 'PDomQuerySelector'
-- Returns: 'DomQuerySelector'
domQuerySelector :: Handle ev -> PDomQuerySelector -> IO (Either Error DomQuerySelector)
domQuerySelector handle params = sendReceiveCommandResult handle "DOM.querySelector" (Just params)

-- | Return type of the 'domQuerySelector' command.
data DomQuerySelector = DomQuerySelector {
  -- | Query selector result.
  domQuerySelectorNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomQuerySelector where
   commandName _ = "DOM.querySelector"



-- | Parameters of the 'domQuerySelectorAll' command.
data PDomQuerySelectorAll = PDomQuerySelectorAll {
  -- | Id of the node to query upon.
  pDomQuerySelectorAllNodeId :: DomNodeId,
  -- | Selector string.
  pDomQuerySelectorAllSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelectorAll  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'DOM.querySelectorAll' command.
-- Executes `querySelectorAll` on a given node.
-- Parameters: 'PDomQuerySelectorAll'
-- Returns: 'DomQuerySelectorAll'
domQuerySelectorAll :: Handle ev -> PDomQuerySelectorAll -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll handle params = sendReceiveCommandResult handle "DOM.querySelectorAll" (Just params)

-- | Return type of the 'domQuerySelectorAll' command.
data DomQuerySelectorAll = DomQuerySelectorAll {
  -- | Query selector result.
  domQuerySelectorAllNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomQuerySelectorAll where
   commandName _ = "DOM.querySelectorAll"



-- | Function for the 'DOM.redo' command.
-- Re-does the last undone action.
domRedo :: Handle ev -> IO (Maybe Error)
domRedo handle = sendReceiveCommand handle "DOM.redo" (Nothing :: Maybe ())


-- | Parameters of the 'domRemoveAttribute' command.
data PDomRemoveAttribute = PDomRemoveAttribute {
  -- | Id of the element to remove attribute from.
  pDomRemoveAttributeNodeId :: DomNodeId,
  -- | Name of the attribute to remove.
  pDomRemoveAttributeName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveAttribute  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveAttribute where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'DOM.removeAttribute' command.
-- Removes attribute with given name from an element with given id.
-- Parameters: 'PDomRemoveAttribute'
domRemoveAttribute :: Handle ev -> PDomRemoveAttribute -> IO (Maybe Error)
domRemoveAttribute handle params = sendReceiveCommand handle "DOM.removeAttribute" (Just params)


-- | Parameters of the 'domRemoveNode' command.
data PDomRemoveNode = PDomRemoveNode {
  -- | Id of the node to remove.
  pDomRemoveNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the 'DOM.removeNode' command.
-- Removes node with given id.
-- Parameters: 'PDomRemoveNode'
domRemoveNode :: Handle ev -> PDomRemoveNode -> IO (Maybe Error)
domRemoveNode handle params = sendReceiveCommand handle "DOM.removeNode" (Just params)


-- | Parameters of the 'domRequestChildNodes' command.
data PDomRequestChildNodes = PDomRequestChildNodes {
  -- | Id of the node to get children for.
  pDomRequestChildNodesNodeId :: DomNodeId,
  -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
  -- entire subtree or provide an integer larger than 0.
  pDomRequestChildNodesDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the sub-tree
  -- (default is false).
  pDomRequestChildNodesPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomRequestChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'DOM.requestChildNodes' command.
-- Requests that children of the node with given id are returned to the caller in form of
-- `setChildNodes` events where not only immediate children are retrieved, but all children down to
-- the specified depth.
-- Parameters: 'PDomRequestChildNodes'
domRequestChildNodes :: Handle ev -> PDomRequestChildNodes -> IO (Maybe Error)
domRequestChildNodes handle params = sendReceiveCommand handle "DOM.requestChildNodes" (Just params)


-- | Parameters of the 'domRequestNode' command.
data PDomRequestNode = PDomRequestNode {
  -- | JavaScript object id to convert into node.
  pDomRequestNodeObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.requestNode' command.
-- Requests that the node is sent to the caller given the JavaScript node object reference. All
-- nodes that form the path from the node to the root are also sent to the client as a series of
-- `setChildNodes` notifications.
-- Parameters: 'PDomRequestNode'
-- Returns: 'DomRequestNode'
domRequestNode :: Handle ev -> PDomRequestNode -> IO (Either Error DomRequestNode)
domRequestNode handle params = sendReceiveCommandResult handle "DOM.requestNode" (Just params)

-- | Return type of the 'domRequestNode' command.
data DomRequestNode = DomRequestNode {
  -- | Node id for given object.
  domRequestNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomRequestNode where
   commandName _ = "DOM.requestNode"



-- | Parameters of the 'domResolveNode' command.
data PDomResolveNode = PDomResolveNode {
  -- | Id of the node to resolve.
  pDomResolveNodeNodeId :: Maybe DomNodeId,
  -- | Backend identifier of the node to resolve.
  pDomResolveNodeBackendNodeId :: Maybe DomBackendNodeId,
  -- | Symbolic group name that can be used to release multiple objects.
  pDomResolveNodeObjectGroup :: Maybe String,
  -- | Execution context in which to resolve the node.
  pDomResolveNodeExecutionContextId :: Maybe Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomResolveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.resolveNode' command.
-- Resolves the JavaScript node object for a given NodeId or BackendNodeId.
-- Parameters: 'PDomResolveNode'
-- Returns: 'DomResolveNode'
domResolveNode :: Handle ev -> PDomResolveNode -> IO (Either Error DomResolveNode)
domResolveNode handle params = sendReceiveCommandResult handle "DOM.resolveNode" (Just params)

-- | Return type of the 'domResolveNode' command.
data DomResolveNode = DomResolveNode {
  -- | JavaScript object wrapper for given node.
  domResolveNodeObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomResolveNode where
   commandName _ = "DOM.resolveNode"



-- | Parameters of the 'domSetAttributeValue' command.
data PDomSetAttributeValue = PDomSetAttributeValue {
  -- | Id of the element to set attribute for.
  pDomSetAttributeValueNodeId :: DomNodeId,
  -- | Attribute name.
  pDomSetAttributeValueName :: String,
  -- | Attribute value.
  pDomSetAttributeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'DOM.setAttributeValue' command.
-- Sets attribute for an element with given id.
-- Parameters: 'PDomSetAttributeValue'
domSetAttributeValue :: Handle ev -> PDomSetAttributeValue -> IO (Maybe Error)
domSetAttributeValue handle params = sendReceiveCommand handle "DOM.setAttributeValue" (Just params)


-- | Parameters of the 'domSetAttributesAsText' command.
data PDomSetAttributesAsText = PDomSetAttributesAsText {
  -- | Id of the element to set attributes for.
  pDomSetAttributesAsTextNodeId :: DomNodeId,
  -- | Text with a number of attributes. Will parse this text using HTML parser.
  pDomSetAttributesAsTextText :: String,
  -- | Attribute name to replace with new attributes derived from text in case text parsed
  -- successfully.
  pDomSetAttributesAsTextName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributesAsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributesAsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'DOM.setAttributesAsText' command.
-- Sets attributes on element with given id. This method is useful when user edits some existing
-- attribute value and types in several attribute name/value pairs.
-- Parameters: 'PDomSetAttributesAsText'
domSetAttributesAsText :: Handle ev -> PDomSetAttributesAsText -> IO (Maybe Error)
domSetAttributesAsText handle params = sendReceiveCommand handle "DOM.setAttributesAsText" (Just params)


-- | Parameters of the 'domSetFileInputFiles' command.
data PDomSetFileInputFiles = PDomSetFileInputFiles {
  -- | Array of file paths to set.
  pDomSetFileInputFilesFiles :: [String],
  -- | Identifier of the node.
  pDomSetFileInputFilesNodeId :: Maybe DomNodeId,
  -- | Identifier of the backend node.
  pDomSetFileInputFilesBackendNodeId :: Maybe DomBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDomSetFileInputFilesObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetFileInputFiles  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetFileInputFiles where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'DOM.setFileInputFiles' command.
-- Sets files for the given file input element.
-- Parameters: 'PDomSetFileInputFiles'
domSetFileInputFiles :: Handle ev -> PDomSetFileInputFiles -> IO (Maybe Error)
domSetFileInputFiles handle params = sendReceiveCommand handle "DOM.setFileInputFiles" (Just params)


-- | Parameters of the 'domSetNodeStackTracesEnabled' command.
data PDomSetNodeStackTracesEnabled = PDomSetNodeStackTracesEnabled {
  -- | Enable or disable.
  pDomSetNodeStackTracesEnabledEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeStackTracesEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeStackTracesEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'DOM.setNodeStackTracesEnabled' command.
-- Sets if stack traces should be captured for Nodes. See `Node.getNodeStackTraces`. Default is disabled.
-- Parameters: 'PDomSetNodeStackTracesEnabled'
domSetNodeStackTracesEnabled :: Handle ev -> PDomSetNodeStackTracesEnabled -> IO (Maybe Error)
domSetNodeStackTracesEnabled handle params = sendReceiveCommand handle "DOM.setNodeStackTracesEnabled" (Just params)


-- | Parameters of the 'domGetNodeStackTraces' command.
data PDomGetNodeStackTraces = PDomGetNodeStackTraces {
  -- | Id of the node to get stack traces for.
  pDomGetNodeStackTracesNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeStackTraces  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'DOM.getNodeStackTraces' command.
-- Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.
-- Parameters: 'PDomGetNodeStackTraces'
-- Returns: 'DomGetNodeStackTraces'
domGetNodeStackTraces :: Handle ev -> PDomGetNodeStackTraces -> IO (Either Error DomGetNodeStackTraces)
domGetNodeStackTraces handle params = sendReceiveCommandResult handle "DOM.getNodeStackTraces" (Just params)

-- | Return type of the 'domGetNodeStackTraces' command.
data DomGetNodeStackTraces = DomGetNodeStackTraces {
  -- | Creation stack trace, if available.
  domGetNodeStackTracesCreation :: Maybe Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeStackTraces where
   commandName _ = "DOM.getNodeStackTraces"



-- | Parameters of the 'domGetFileInfo' command.
data PDomGetFileInfo = PDomGetFileInfo {
  -- | JavaScript object id of the node wrapper.
  pDomGetFileInfoObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFileInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.getFileInfo' command.
-- Returns file information for the given
-- File wrapper.
-- Parameters: 'PDomGetFileInfo'
-- Returns: 'DomGetFileInfo'
domGetFileInfo :: Handle ev -> PDomGetFileInfo -> IO (Either Error DomGetFileInfo)
domGetFileInfo handle params = sendReceiveCommandResult handle "DOM.getFileInfo" (Just params)

-- | Return type of the 'domGetFileInfo' command.
data DomGetFileInfo = DomGetFileInfo {
  domGetFileInfoPath :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetFileInfo where
   commandName _ = "DOM.getFileInfo"



-- | Parameters of the 'domSetInspectedNode' command.
data PDomSetInspectedNode = PDomSetInspectedNode {
  -- | DOM node id to be accessible by means of $x command line API.
  pDomSetInspectedNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetInspectedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomSetInspectedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'DOM.setInspectedNode' command.
-- Enables console to refer to the node with given id via $x (see Command Line API for more details
-- $x functions).
-- Parameters: 'PDomSetInspectedNode'
domSetInspectedNode :: Handle ev -> PDomSetInspectedNode -> IO (Maybe Error)
domSetInspectedNode handle params = sendReceiveCommand handle "DOM.setInspectedNode" (Just params)


-- | Parameters of the 'domSetNodeName' command.
data PDomSetNodeName = PDomSetNodeName {
  -- | Id of the node to set name for.
  pDomSetNodeNameNodeId :: DomNodeId,
  -- | New node's name.
  pDomSetNodeNameName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeName  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'DOM.setNodeName' command.
-- Sets node name for a node with given id.
-- Parameters: 'PDomSetNodeName'
-- Returns: 'DomSetNodeName'
domSetNodeName :: Handle ev -> PDomSetNodeName -> IO (Either Error DomSetNodeName)
domSetNodeName handle params = sendReceiveCommandResult handle "DOM.setNodeName" (Just params)

-- | Return type of the 'domSetNodeName' command.
data DomSetNodeName = DomSetNodeName {
  -- | New node's id.
  domSetNodeNameNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomSetNodeName where
   commandName _ = "DOM.setNodeName"



-- | Parameters of the 'domSetNodeValue' command.
data PDomSetNodeValue = PDomSetNodeValue {
  -- | Id of the node to set value for.
  pDomSetNodeValueNodeId :: DomNodeId,
  -- | New node's value.
  pDomSetNodeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOM.setNodeValue' command.
-- Sets node value for a node with given id.
-- Parameters: 'PDomSetNodeValue'
domSetNodeValue :: Handle ev -> PDomSetNodeValue -> IO (Maybe Error)
domSetNodeValue handle params = sendReceiveCommand handle "DOM.setNodeValue" (Just params)


-- | Parameters of the 'domSetOuterHtml' command.
data PDomSetOuterHtml = PDomSetOuterHtml {
  -- | Id of the node to set markup for.
  pDomSetOuterHtmlNodeId :: DomNodeId,
  -- | Outer HTML markup to set.
  pDomSetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'DOM.setOuterHTML' command.
-- Sets node HTML markup, returns new node id.
-- Parameters: 'PDomSetOuterHtml'
domSetOuterHtml :: Handle ev -> PDomSetOuterHtml -> IO (Maybe Error)
domSetOuterHtml handle params = sendReceiveCommand handle "DOM.setOuterHTML" (Just params)


-- | Function for the 'DOM.undo' command.
-- Undoes the last performed action.
domUndo :: Handle ev -> IO (Maybe Error)
domUndo handle = sendReceiveCommand handle "DOM.undo" (Nothing :: Maybe ())


-- | Parameters of the 'domGetFrameOwner' command.
data PDomGetFrameOwner = PDomGetFrameOwner {
  pDomGetFrameOwnerFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFrameOwner  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'DOM.getFrameOwner' command.
-- Returns iframe node that owns iframe with the given domain.
-- Parameters: 'PDomGetFrameOwner'
-- Returns: 'DomGetFrameOwner'
domGetFrameOwner :: Handle ev -> PDomGetFrameOwner -> IO (Either Error DomGetFrameOwner)
domGetFrameOwner handle params = sendReceiveCommandResult handle "DOM.getFrameOwner" (Just params)

-- | Return type of the 'domGetFrameOwner' command.
data DomGetFrameOwner = DomGetFrameOwner {
  -- | Resulting node.
  domGetFrameOwnerBackendNodeId :: DomBackendNodeId,
  -- | Id of the node at given coordinates, only when enabled and requested document.
  domGetFrameOwnerNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetFrameOwner where
   commandName _ = "DOM.getFrameOwner"



-- | Parameters of the 'domGetContainerForNode' command.
data PDomGetContainerForNode = PDomGetContainerForNode {
  pDomGetContainerForNodeNodeId :: DomNodeId,
  pDomGetContainerForNodeContainerName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContainerForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'DOM.getContainerForNode' command.
-- Returns the container of the given node based on container query conditions.
-- If containerName is given, it will find the nearest container with a matching name;
-- otherwise it will find the nearest container regardless of its container name.
-- Parameters: 'PDomGetContainerForNode'
-- Returns: 'DomGetContainerForNode'
domGetContainerForNode :: Handle ev -> PDomGetContainerForNode -> IO (Either Error DomGetContainerForNode)
domGetContainerForNode handle params = sendReceiveCommandResult handle "DOM.getContainerForNode" (Just params)

-- | Return type of the 'domGetContainerForNode' command.
data DomGetContainerForNode = DomGetContainerForNode {
  -- | The container node for the given node, or null if not found.
  domGetContainerForNodeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetContainerForNode where
   commandName _ = "DOM.getContainerForNode"



-- | Parameters of the 'domGetQueryingDescendantsForContainer' command.
data PDomGetQueryingDescendantsForContainer = PDomGetQueryingDescendantsForContainer {
  -- | Id of the container node to find querying descendants from.
  pDomGetQueryingDescendantsForContainerNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetQueryingDescendantsForContainer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'DOM.getQueryingDescendantsForContainer' command.
-- Returns the descendants of a container query container that have
-- container queries against this container.
-- Parameters: 'PDomGetQueryingDescendantsForContainer'
-- Returns: 'DomGetQueryingDescendantsForContainer'
domGetQueryingDescendantsForContainer :: Handle ev -> PDomGetQueryingDescendantsForContainer -> IO (Either Error DomGetQueryingDescendantsForContainer)
domGetQueryingDescendantsForContainer handle params = sendReceiveCommandResult handle "DOM.getQueryingDescendantsForContainer" (Just params)

-- | Return type of the 'domGetQueryingDescendantsForContainer' command.
data DomGetQueryingDescendantsForContainer = DomGetQueryingDescendantsForContainer {
  -- | Descendant nodes with container queries against the given container.
  domGetQueryingDescendantsForContainerNodeIds :: [DomNodeId]
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
  -- | Orientation type.
  emulationScreenOrientationType :: EmulationScreenOrientationType,
  -- | Orientation angle.
  emulationScreenOrientationAngle :: Int
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
  -- | Orientation of a display feature in relation to screen
  emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation,
  -- | The offset from the screen origin in either the x (for vertical
  -- orientation) or y (for horizontal orientation) direction.
  emulationDisplayFeatureOffset :: Int,
  -- | A display feature may mask content such that it is not physically
  -- displayed - this length along with the offset describes this area.
  -- A display feature that only splits content will have a 0 mask_length.
  emulationDisplayFeatureMaskLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Emulation.MediaFeature' .
data EmulationMediaFeature = EmulationMediaFeature {
  emulationMediaFeatureName :: String,
  emulationMediaFeatureValue :: String
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
  emulationUserAgentBrandVersionBrand :: String,
  emulationUserAgentBrandVersionVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationUserAgentBrandVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  EmulationUserAgentBrandVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
-- Missing optional values will be filled in by the target with what it would normally use.
data EmulationUserAgentMetadata = EmulationUserAgentMetadata {
  emulationUserAgentMetadataBrands :: Maybe [EmulationUserAgentBrandVersion],
  emulationUserAgentMetadataFullVersionList :: Maybe [EmulationUserAgentBrandVersion],
  emulationUserAgentMetadataPlatform :: String,
  emulationUserAgentMetadataPlatformVersion :: String,
  emulationUserAgentMetadataArchitecture :: String,
  emulationUserAgentMetadataModel :: String,
  emulationUserAgentMetadataMobile :: Bool,
  emulationUserAgentMetadataBitness :: Maybe String,
  emulationUserAgentMetadataWow64 :: Maybe Bool
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





-- | Function for the 'Emulation.canEmulate' command.
-- Tells whether emulation is supported.
-- Returns: 'EmulationCanEmulate'
emulationCanEmulate :: Handle ev -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())

-- | Return type of the 'emulationCanEmulate' command.
data EmulationCanEmulate = EmulationCanEmulate {
  -- | True if emulation is supported.
  emulationCanEmulateResult :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command EmulationCanEmulate where
   commandName _ = "Emulation.canEmulate"



-- | Function for the 'Emulation.clearDeviceMetricsOverride' command.
-- Clears the overridden device metrics.
emulationClearDeviceMetricsOverride :: Handle ev -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())


-- | Function for the 'Emulation.clearGeolocationOverride' command.
-- Clears the overridden Geolocation Position and Error.
emulationClearGeolocationOverride :: Handle ev -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())


-- | Function for the 'Emulation.resetPageScaleFactor' command.
-- Requests that page scale factor is reset to initial values.
emulationResetPageScaleFactor :: Handle ev -> IO (Maybe Error)
emulationResetPageScaleFactor handle = sendReceiveCommand handle "Emulation.resetPageScaleFactor" (Nothing :: Maybe ())


-- | Parameters of the 'emulationSetFocusEmulationEnabled' command.
data PEmulationSetFocusEmulationEnabled = PEmulationSetFocusEmulationEnabled {
  -- | Whether to enable to disable focus emulation.
  pEmulationSetFocusEmulationEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetFocusEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetFocusEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Emulation.setFocusEmulationEnabled' command.
-- Enables or disables simulating a focused and active page.
-- Parameters: 'PEmulationSetFocusEmulationEnabled'
emulationSetFocusEmulationEnabled :: Handle ev -> PEmulationSetFocusEmulationEnabled -> IO (Maybe Error)
emulationSetFocusEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setFocusEmulationEnabled" (Just params)


-- | Parameters of the 'emulationSetAutoDarkModeOverride' command.
data PEmulationSetAutoDarkModeOverride = PEmulationSetAutoDarkModeOverride {
  -- | Whether to enable or disable automatic dark mode.
  -- If not specified, any existing override will be cleared.
  pEmulationSetAutoDarkModeOverrideEnabled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutoDarkModeOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutoDarkModeOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Emulation.setAutoDarkModeOverride' command.
-- Automatically render all web contents using a dark theme.
-- Parameters: 'PEmulationSetAutoDarkModeOverride'
emulationSetAutoDarkModeOverride :: Handle ev -> PEmulationSetAutoDarkModeOverride -> IO (Maybe Error)
emulationSetAutoDarkModeOverride handle params = sendReceiveCommand handle "Emulation.setAutoDarkModeOverride" (Just params)


-- | Parameters of the 'emulationSetCpuThrottlingRate' command.
data PEmulationSetCpuThrottlingRate = PEmulationSetCpuThrottlingRate {
  -- | Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
  pEmulationSetCpuThrottlingRateRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetCpuThrottlingRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetCpuThrottlingRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Emulation.setCPUThrottlingRate' command.
-- Enables CPU throttling to emulate slow CPUs.
-- Parameters: 'PEmulationSetCpuThrottlingRate'
emulationSetCpuThrottlingRate :: Handle ev -> PEmulationSetCpuThrottlingRate -> IO (Maybe Error)
emulationSetCpuThrottlingRate handle params = sendReceiveCommand handle "Emulation.setCPUThrottlingRate" (Just params)


-- | Parameters of the 'emulationSetDefaultBackgroundColorOverride' command.
data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
  -- | RGBA of the default background color. If not specified, any existing override will be
  -- cleared.
  pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


-- | Function for the 'Emulation.setDefaultBackgroundColorOverride' command.
-- Sets or clears an override of the default background color of the frame. This override is used
-- if the content does not specify one.
-- Parameters: 'PEmulationSetDefaultBackgroundColorOverride'
emulationSetDefaultBackgroundColorOverride :: Handle ev -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)


-- | Parameters of the 'emulationSetDeviceMetricsOverride' command.
data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
  -- | Overriding width value in pixels (minimum 0, maximum 10000000). 0 disables the override.
  pEmulationSetDeviceMetricsOverrideWidth :: Int,
  -- | Overriding height value in pixels (minimum 0, maximum 10000000). 0 disables the override.
  pEmulationSetDeviceMetricsOverrideHeight :: Int,
  -- | Overriding device scale factor value. 0 disables the override.
  pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
  -- | Whether to emulate mobile device. This includes viewport meta tag, overlay scrollbars, text
  -- autosizing and more.
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
  -- change is not observed by the page, e.g. viewport-relative elements do not change positions.
  pEmulationSetDeviceMetricsOverrideViewport :: Maybe PageViewport,
  -- | If set, the display feature of a multi-segment screen. If not set, multi-segment support
  -- is turned-off.
  pEmulationSetDeviceMetricsOverrideDisplayFeature :: Maybe EmulationDisplayFeature
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Emulation.setDeviceMetricsOverride' command.
-- Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
-- window.innerWidth, window.innerHeight, and "device-width"/"device-height"-related CSS media
-- query results).
-- Parameters: 'PEmulationSetDeviceMetricsOverride'
emulationSetDeviceMetricsOverride :: Handle ev -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)


-- | Parameters of the 'emulationSetScrollbarsHidden' command.
data PEmulationSetScrollbarsHidden = PEmulationSetScrollbarsHidden {
  -- | Whether scrollbars should be always hidden.
  pEmulationSetScrollbarsHiddenHidden :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScrollbarsHidden  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScrollbarsHidden where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Emulation.setScrollbarsHidden' command.
-- Parameters: 'PEmulationSetScrollbarsHidden'
emulationSetScrollbarsHidden :: Handle ev -> PEmulationSetScrollbarsHidden -> IO (Maybe Error)
emulationSetScrollbarsHidden handle params = sendReceiveCommand handle "Emulation.setScrollbarsHidden" (Just params)


-- | Parameters of the 'emulationSetDocumentCookieDisabled' command.
data PEmulationSetDocumentCookieDisabled = PEmulationSetDocumentCookieDisabled {
  -- | Whether document.coookie API should be disabled.
  pEmulationSetDocumentCookieDisabledDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDocumentCookieDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDocumentCookieDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'Emulation.setDocumentCookieDisabled' command.
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
  -- | Whether touch emulation based on mouse input should be enabled.
  pEmulationSetEmitTouchEventsForMouseEnabled :: Bool,
  -- | Touch/gesture events configuration. Default: current platform.
  pEmulationSetEmitTouchEventsForMouseConfiguration :: PEmulationSetEmitTouchEventsForMouseConfiguration
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmitTouchEventsForMouse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmitTouchEventsForMouse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'Emulation.setEmitTouchEventsForMouse' command.
-- Parameters: 'PEmulationSetEmitTouchEventsForMouse'
emulationSetEmitTouchEventsForMouse :: Handle ev -> PEmulationSetEmitTouchEventsForMouse -> IO (Maybe Error)
emulationSetEmitTouchEventsForMouse handle params = sendReceiveCommand handle "Emulation.setEmitTouchEventsForMouse" (Just params)


-- | Parameters of the 'emulationSetEmulatedMedia' command.
data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
  -- | Media type to emulate. Empty string disables the override.
  pEmulationSetEmulatedMediaMedia :: Maybe String,
  -- | Media features to emulate.
  pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Emulation.setEmulatedMedia' command.
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
  -- | Vision deficiency to emulate.
  pEmulationSetEmulatedVisionDeficiencyType :: PEmulationSetEmulatedVisionDeficiencyType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedVisionDeficiency  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedVisionDeficiency where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the 'Emulation.setEmulatedVisionDeficiency' command.
-- Emulates the given vision deficiency.
-- Parameters: 'PEmulationSetEmulatedVisionDeficiency'
emulationSetEmulatedVisionDeficiency :: Handle ev -> PEmulationSetEmulatedVisionDeficiency -> IO (Maybe Error)
emulationSetEmulatedVisionDeficiency handle params = sendReceiveCommand handle "Emulation.setEmulatedVisionDeficiency" (Just params)


-- | Parameters of the 'emulationSetGeolocationOverride' command.
data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
  -- | Mock latitude
  pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
  -- | Mock longitude
  pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
  -- | Mock accuracy
  pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetGeolocationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetGeolocationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'Emulation.setGeolocationOverride' command.
-- Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
-- unavailable.
-- Parameters: 'PEmulationSetGeolocationOverride'
emulationSetGeolocationOverride :: Handle ev -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)


-- | Parameters of the 'emulationSetIdleOverride' command.
data PEmulationSetIdleOverride = PEmulationSetIdleOverride {
  -- | Mock isUserActive
  pEmulationSetIdleOverrideIsUserActive :: Bool,
  -- | Mock isScreenUnlocked
  pEmulationSetIdleOverrideIsScreenUnlocked :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetIdleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetIdleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Emulation.setIdleOverride' command.
-- Overrides the Idle state.
-- Parameters: 'PEmulationSetIdleOverride'
emulationSetIdleOverride :: Handle ev -> PEmulationSetIdleOverride -> IO (Maybe Error)
emulationSetIdleOverride handle params = sendReceiveCommand handle "Emulation.setIdleOverride" (Just params)


-- | Function for the 'Emulation.clearIdleOverride' command.
-- Clears Idle state overrides.
emulationClearIdleOverride :: Handle ev -> IO (Maybe Error)
emulationClearIdleOverride handle = sendReceiveCommand handle "Emulation.clearIdleOverride" (Nothing :: Maybe ())


-- | Parameters of the 'emulationSetPageScaleFactor' command.
data PEmulationSetPageScaleFactor = PEmulationSetPageScaleFactor {
  -- | Page scale factor.
  pEmulationSetPageScaleFactorPageScaleFactor :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetPageScaleFactor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetPageScaleFactor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Emulation.setPageScaleFactor' command.
-- Sets a specified page scale factor.
-- Parameters: 'PEmulationSetPageScaleFactor'
emulationSetPageScaleFactor :: Handle ev -> PEmulationSetPageScaleFactor -> IO (Maybe Error)
emulationSetPageScaleFactor handle params = sendReceiveCommand handle "Emulation.setPageScaleFactor" (Just params)


-- | Parameters of the 'emulationSetScriptExecutionDisabled' command.
data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
  -- | Whether script execution should be disabled in the page.
  pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'Emulation.setScriptExecutionDisabled' command.
-- Switches script execution in the page.
-- Parameters: 'PEmulationSetScriptExecutionDisabled'
emulationSetScriptExecutionDisabled :: Handle ev -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)


-- | Parameters of the 'emulationSetTouchEmulationEnabled' command.
data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
  -- | Whether the touch event emulation should be enabled.
  pEmulationSetTouchEmulationEnabledEnabled :: Bool,
  -- | Maximum touch points supported. Defaults to one.
  pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTouchEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTouchEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Emulation.setTouchEmulationEnabled' command.
-- Enables touch on platforms which do not support them.
-- Parameters: 'PEmulationSetTouchEmulationEnabled'
emulationSetTouchEmulationEnabled :: Handle ev -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)


-- | Parameters of the 'emulationSetVirtualTimePolicy' command.
data PEmulationSetVirtualTimePolicy = PEmulationSetVirtualTimePolicy {
  pEmulationSetVirtualTimePolicyPolicy :: EmulationVirtualTimePolicy,
  -- | If set, after this many virtual milliseconds have elapsed virtual time will be paused and a
  -- virtualTimeBudgetExpired event is sent.
  pEmulationSetVirtualTimePolicyBudget :: Maybe Double,
  -- | If set this specifies the maximum number of tasks that can be run before virtual is forced
  -- forwards to prevent deadlock.
  pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount :: Maybe Int,
  -- | If set, base::Time::Now will be overridden to initially return this value.
  pEmulationSetVirtualTimePolicyInitialVirtualTime :: Maybe NetworkTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetVirtualTimePolicy  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Emulation.setVirtualTimePolicy' command.
-- Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
-- the current virtual time policy.  Note this supersedes any previous time budget.
-- Parameters: 'PEmulationSetVirtualTimePolicy'
-- Returns: 'EmulationSetVirtualTimePolicy'
emulationSetVirtualTimePolicy :: Handle ev -> PEmulationSetVirtualTimePolicy -> IO (Either Error EmulationSetVirtualTimePolicy)
emulationSetVirtualTimePolicy handle params = sendReceiveCommandResult handle "Emulation.setVirtualTimePolicy" (Just params)

-- | Return type of the 'emulationSetVirtualTimePolicy' command.
data EmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy {
  -- | Absolute timestamp at which virtual time was first enabled (up time in milliseconds).
  emulationSetVirtualTimePolicyVirtualTimeTicksBase :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command EmulationSetVirtualTimePolicy where
   commandName _ = "Emulation.setVirtualTimePolicy"



-- | Parameters of the 'emulationSetLocaleOverride' command.
data PEmulationSetLocaleOverride = PEmulationSetLocaleOverride {
  -- | ICU style C locale (e.g. "en_US"). If not specified or empty, disables the override and
  -- restores default host system locale.
  pEmulationSetLocaleOverrideLocale :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetLocaleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetLocaleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Emulation.setLocaleOverride' command.
-- Overrides default host system locale with the specified one.
-- Parameters: 'PEmulationSetLocaleOverride'
emulationSetLocaleOverride :: Handle ev -> PEmulationSetLocaleOverride -> IO (Maybe Error)
emulationSetLocaleOverride handle params = sendReceiveCommand handle "Emulation.setLocaleOverride" (Just params)


-- | Parameters of the 'emulationSetTimezoneOverride' command.
data PEmulationSetTimezoneOverride = PEmulationSetTimezoneOverride {
  -- | The timezone identifier. If empty, disables the override and
  -- restores default host system timezone.
  pEmulationSetTimezoneOverrideTimezoneId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTimezoneOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTimezoneOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Emulation.setTimezoneOverride' command.
-- Overrides default host system timezone with the specified one.
-- Parameters: 'PEmulationSetTimezoneOverride'
emulationSetTimezoneOverride :: Handle ev -> PEmulationSetTimezoneOverride -> IO (Maybe Error)
emulationSetTimezoneOverride handle params = sendReceiveCommand handle "Emulation.setTimezoneOverride" (Just params)


-- | Parameters of the 'emulationSetDisabledImageTypes' command.
data PEmulationSetDisabledImageTypes = PEmulationSetDisabledImageTypes {
  -- | Image types to disable.
  pEmulationSetDisabledImageTypesImageTypes :: [EmulationDisabledImageType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDisabledImageTypes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDisabledImageTypes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'Emulation.setDisabledImageTypes' command.
-- Parameters: 'PEmulationSetDisabledImageTypes'
emulationSetDisabledImageTypes :: Handle ev -> PEmulationSetDisabledImageTypes -> IO (Maybe Error)
emulationSetDisabledImageTypes handle params = sendReceiveCommand handle "Emulation.setDisabledImageTypes" (Just params)


-- | Parameters of the 'emulationSetHardwareConcurrencyOverride' command.
data PEmulationSetHardwareConcurrencyOverride = PEmulationSetHardwareConcurrencyOverride {
  -- | Hardware concurrency to report
  pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetHardwareConcurrencyOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetHardwareConcurrencyOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the 'Emulation.setHardwareConcurrencyOverride' command.
-- Parameters: 'PEmulationSetHardwareConcurrencyOverride'
emulationSetHardwareConcurrencyOverride :: Handle ev -> PEmulationSetHardwareConcurrencyOverride -> IO (Maybe Error)
emulationSetHardwareConcurrencyOverride handle params = sendReceiveCommand handle "Emulation.setHardwareConcurrencyOverride" (Just params)


-- | Parameters of the 'emulationSetUserAgentOverride' command.
data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
  -- | User agent to use.
  pEmulationSetUserAgentOverrideUserAgent :: String,
  -- | Browser langugage to emulate.
  pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
  -- | The platform navigator.platform should return.
  pEmulationSetUserAgentOverridePlatform :: Maybe String,
  -- | To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
  pEmulationSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Emulation.setUserAgentOverride' command.
-- Allows overriding user agent with the given string.
-- Parameters: 'PEmulationSetUserAgentOverride'
emulationSetUserAgentOverride :: Handle ev -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)


-- | Parameters of the 'emulationSetAutomationOverride' command.
data PEmulationSetAutomationOverride = PEmulationSetAutomationOverride {
  -- | Whether the override should be enabled.
  pEmulationSetAutomationOverrideEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutomationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutomationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'Emulation.setAutomationOverride' command.
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
  -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
  -- milliseconds relatively to this requestTime.
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
  networkPostDataEntryBytes :: Maybe String
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
  -- | Request URL (without fragment).
  networkRequestUrl :: String,
  -- | Fragment of the requested URL starting with hash, if present.
  networkRequestUrlFragment :: Maybe String,
  -- | HTTP request method.
  networkRequestMethod :: String,
  -- | HTTP request headers.
  networkRequestHeaders :: NetworkHeaders,
  -- | HTTP POST request data.
  networkRequestPostData :: Maybe String,
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
  -- passed by the developer (e.g. via "fetch") as understood by the backend.
  networkRequestTrustTokenParams :: Maybe NetworkTrustTokenParams,
  -- | True if this resource request is considered to be the 'same site' as the
  -- request correspondinfg to the main frame.
  networkRequestIsSameSite :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  NetworkRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Details of a signed certificate timestamp (SCT).
data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
  -- | Validation status.
  networkSignedCertificateTimestampStatus :: String,
  -- | Origin.
  networkSignedCertificateTimestampOrigin :: String,
  -- | Log name / description.
  networkSignedCertificateTimestampLogDescription :: String,
  -- | Log ID.
  networkSignedCertificateTimestampLogId :: String,
  -- | Issuance date. Unlike TimeSinceEpoch, this contains the number of
  -- milliseconds since January 1, 1970, UTC, not the number of seconds.
  networkSignedCertificateTimestampTimestamp :: Double,
  -- | Hash algorithm.
  networkSignedCertificateTimestampHashAlgorithm :: String,
  -- | Signature algorithm.
  networkSignedCertificateTimestampSignatureAlgorithm :: String,
  -- | Signature data.
  networkSignedCertificateTimestampSignatureData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedCertificateTimestamp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedCertificateTimestamp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Security details about a request.
data NetworkSecurityDetails = NetworkSecurityDetails {
  -- | Protocol name (e.g. "TLS 1.2" or "QUIC").
  networkSecurityDetailsProtocol :: String,
  -- | Key Exchange used by the connection, or the empty string if not applicable.
  networkSecurityDetailsKeyExchange :: String,
  -- | (EC)DH group used by the connection, if applicable.
  networkSecurityDetailsKeyExchangeGroup :: Maybe String,
  -- | Cipher name.
  networkSecurityDetailsCipher :: String,
  -- | TLS MAC. Note that AEAD ciphers do not have separate MACs.
  networkSecurityDetailsMac :: Maybe String,
  -- | Certificate ID value.
  networkSecurityDetailsCertificateId :: SecurityCertificateId,
  -- | Certificate subject name.
  networkSecurityDetailsSubjectName :: String,
  -- | Subject Alternative Name (SAN) DNS names and IP addresses.
  networkSecurityDetailsSanList :: [String],
  -- | Name of the issuing CA.
  networkSecurityDetailsIssuer :: String,
  -- | Certificate valid from date.
  networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
  -- | Certificate valid to (expiration) date
  networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
  -- | List of signed certificate timestamps (SCTs).
  networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
  -- | Whether the request complied with Certificate Transparency policy
  networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance
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
  networkCorsErrorStatusCorsError :: NetworkCorsError,
  networkCorsErrorStatusFailedParameter :: String
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
  networkTrustTokenParamsType :: NetworkTrustTokenOperationType,
  -- | Only set for "token-redemption" type and determine whether
  -- to request a fresh SRR or use a still valid cached SRR.
  networkTrustTokenParamsRefreshPolicy :: NetworkTrustTokenParamsRefreshPolicy,
  -- | Origins of issuers from whom to request tokens or redemption
  -- records.
  networkTrustTokenParamsIssuers :: Maybe [String]
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
  -- | Response URL. This URL can be different from CachedResource.url in case of redirect.
  networkResponseUrl :: String,
  -- | HTTP response status code.
  networkResponseStatus :: Int,
  -- | HTTP response status text.
  networkResponseStatusText :: String,
  -- | HTTP response headers.
  networkResponseHeaders :: NetworkHeaders,
  -- | Resource mimeType as determined by the browser.
  networkResponseMimeType :: String,
  -- | Refined HTTP request headers that were actually transmitted over the network.
  networkResponseRequestHeaders :: Maybe NetworkHeaders,
  -- | Specifies whether physical connection was actually reused for this request.
  networkResponseConnectionReused :: Bool,
  -- | Physical connection id that was actually used for this request.
  networkResponseConnectionId :: Double,
  -- | Remote IP address.
  networkResponseRemoteIpAddress :: Maybe String,
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
  networkResponseCacheStorageCacheName :: Maybe String,
  -- | Protocol used to fetch this request.
  networkResponseProtocol :: Maybe String,
  -- | Security state of the request resource.
  networkResponseSecurityState :: SecuritySecurityState,
  -- | Security details for the request.
  networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  NetworkResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | WebSocket request data.
data NetworkWebSocketRequest = NetworkWebSocketRequest {
  -- | HTTP request headers.
  networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | WebSocket response data.
data NetworkWebSocketResponse = NetworkWebSocketResponse {
  -- | HTTP response status code.
  networkWebSocketResponseStatus :: Int,
  -- | HTTP response status text.
  networkWebSocketResponseStatusText :: String,
  -- | HTTP response headers.
  networkWebSocketResponseHeaders :: NetworkHeaders,
  -- | HTTP response headers text.
  networkWebSocketResponseHeadersText :: Maybe String,
  -- | HTTP request headers.
  networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
  -- | HTTP request headers text.
  networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | WebSocket message data. This represents an entire WebSocket message, not just a fragmented frame as the name suggests.
data NetworkWebSocketFrame = NetworkWebSocketFrame {
  -- | WebSocket message opcode.
  networkWebSocketFrameOpcode :: Double,
  -- | WebSocket message mask.
  networkWebSocketFrameMask :: Bool,
  -- | WebSocket message payload data.
  -- If the opcode is 1, this is a text message and payloadData is a UTF-8 string.
  -- If the opcode isn't 1, then payloadData is a base64 encoded string representing binary data.
  networkWebSocketFramePayloadData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about the cached resource.
data NetworkCachedResource = NetworkCachedResource {
  -- | Resource URL. This is the url of the original network request.
  networkCachedResourceUrl :: String,
  -- | Type of this resource.
  networkCachedResourceType :: NetworkResourceType,
  -- | Cached response data.
  networkCachedResourceResponse :: Maybe NetworkResponse,
  -- | Cached response body size.
  networkCachedResourceBodySize :: Double
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
  -- | Type of this initiator.
  networkInitiatorType :: NetworkInitiatorType,
  -- | Initiator JavaScript stack trace, set for Script only.
  networkInitiatorStack :: Maybe Runtime.RuntimeStackTrace,
  -- | Initiator URL, set for Parser type or for Script type (when script is importing module) or for SignedExchange type.
  networkInitiatorUrl :: Maybe String,
  -- | Initiator line number, set for Parser type or for Script type (when script is importing
  -- module) (0-based).
  networkInitiatorLineNumber :: Maybe Double,
  -- | Initiator column number, set for Parser type or for Script type (when script is importing
  -- module) (0-based).
  networkInitiatorColumnNumber :: Maybe Double,
  -- | Set if another request triggered this request (e.g. preflight).
  networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkInitiator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  NetworkInitiator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Cookie object
data NetworkCookie = NetworkCookie {
  -- | Cookie name.
  networkCookieName :: String,
  -- | Cookie value.
  networkCookieValue :: String,
  -- | Cookie domain.
  networkCookieDomain :: String,
  -- | Cookie path.
  networkCookiePath :: String,
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
  -- An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  -- This is a temporary ability and it will be removed in the future.
  networkCookieSourcePort :: Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  -- of the request to the endpoint that set the cookie.
  networkCookiePartitionKey :: Maybe String,
  -- | True if cookie partition key is opaque.
  networkCookiePartitionKeyOpaque :: Maybe Bool
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
  -- | The reason(s) this cookie was blocked.
  networkBlockedSetCookieWithReasonBlockedReasons :: [NetworkSetCookieBlockedReason],
  -- | The string representing this individual cookie as it would appear in the header.
  -- This is not the entire "cookie" or "set-cookie" header which could have multiple cookies.
  networkBlockedSetCookieWithReasonCookieLine :: String,
  -- | The cookie object which represents the cookie which was not stored. It is optional because
  -- sometimes complete cookie information is not available, such as in the case of parsing
  -- errors.
  networkBlockedSetCookieWithReasonCookie :: Maybe NetworkCookie
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedSetCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedSetCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | A cookie with was not sent with a request with the corresponding reason.
data NetworkBlockedCookieWithReason = NetworkBlockedCookieWithReason {
  -- | The reason(s) the cookie was blocked.
  networkBlockedCookieWithReasonBlockedReasons :: [NetworkCookieBlockedReason],
  -- | The cookie object representing the cookie which was not sent.
  networkBlockedCookieWithReasonCookie :: NetworkCookie
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Cookie parameter object
data NetworkCookieParam = NetworkCookieParam {
  -- | Cookie name.
  networkCookieParamName :: String,
  -- | Cookie value.
  networkCookieParamValue :: String,
  -- | The request-URI to associate with the setting of the cookie. This value can affect the
  -- default domain, path, source port, and source scheme values of the created cookie.
  networkCookieParamUrl :: Maybe String,
  -- | Cookie domain.
  networkCookieParamDomain :: Maybe String,
  -- | Cookie path.
  networkCookieParamPath :: Maybe String,
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
  -- An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  -- This is a temporary ability and it will be removed in the future.
  networkCookieParamSourcePort :: Maybe Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  -- of the request to the endpoint that set the cookie.
  -- If not set, the cookie will be set as not partitioned.
  networkCookieParamPartitionKey :: Maybe String
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
  -- | Source of the authentication challenge.
  networkAuthChallengeSource :: NetworkAuthChallengeSource,
  -- | Origin of the challenger.
  networkAuthChallengeOrigin :: String,
  -- | The authentication scheme used, such as basic or digest
  networkAuthChallengeScheme :: String,
  -- | The realm of the challenge. May be empty.
  networkAuthChallengeRealm :: String
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
  -- | The decision on what to do in response to the authorization challenge.  Default means
  -- deferring to the default behavior of the net stack, which will likely either the Cancel
  -- authentication or display a popup dialog box.
  networkAuthChallengeResponseResponse :: NetworkAuthChallengeResponseResponse,
  -- | The username to provide, possibly empty. Should only be set if response is
  -- ProvideCredentials.
  networkAuthChallengeResponseUsername :: Maybe String,
  -- | The password to provide, possibly empty. Should only be set if response is
  -- ProvideCredentials.
  networkAuthChallengeResponsePassword :: Maybe String
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
  -- | Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
  -- backslash. Omitting is equivalent to `"*"`.
  networkRequestPatternUrlPattern :: Maybe String,
  -- | If set, only requests for matching resource types will be intercepted.
  networkRequestPatternResourceType :: Maybe NetworkResourceType,
  -- | Stage at which to begin intercepting requests. Default is Request.
  networkRequestPatternInterceptionStage :: Maybe NetworkInterceptionStage
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about a signed exchange signature.
-- https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#rfc.section.3.1
data NetworkSignedExchangeSignature = NetworkSignedExchangeSignature {
  -- | Signed exchange signature label.
  networkSignedExchangeSignatureLabel :: String,
  -- | The hex string of signed exchange signature.
  networkSignedExchangeSignatureSignature :: String,
  -- | Signed exchange signature integrity.
  networkSignedExchangeSignatureIntegrity :: String,
  -- | Signed exchange signature cert Url.
  networkSignedExchangeSignatureCertUrl :: Maybe String,
  -- | The hex string of signed exchange signature cert sha256.
  networkSignedExchangeSignatureCertSha256 :: Maybe String,
  -- | Signed exchange signature validity Url.
  networkSignedExchangeSignatureValidityUrl :: String,
  -- | Signed exchange signature date.
  networkSignedExchangeSignatureDate :: Int,
  -- | Signed exchange signature expires.
  networkSignedExchangeSignatureExpires :: Int,
  -- | The encoded certificates.
  networkSignedExchangeSignatureCertificates :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeSignature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeSignature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Information about a signed exchange header.
-- https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#cbor-representation
data NetworkSignedExchangeHeader = NetworkSignedExchangeHeader {
  -- | Signed exchange request URL.
  networkSignedExchangeHeaderRequestUrl :: String,
  -- | Signed exchange response code.
  networkSignedExchangeHeaderResponseCode :: Int,
  -- | Signed exchange response headers.
  networkSignedExchangeHeaderResponseHeaders :: NetworkHeaders,
  -- | Signed exchange response signature.
  networkSignedExchangeHeaderSignatures :: [NetworkSignedExchangeSignature],
  -- | Signed exchange header integrity hash in the form of "sha256-<base64-hash-value>".
  networkSignedExchangeHeaderHeaderIntegrity :: String
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
  -- | Error message.
  networkSignedExchangeErrorMessage :: String,
  -- | The index of the signature which caused the error.
  networkSignedExchangeErrorSignatureIndex :: Maybe Int,
  -- | The field which caused the error.
  networkSignedExchangeErrorErrorField :: Maybe NetworkSignedExchangeErrorField
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Information about a signed exchange response.
data NetworkSignedExchangeInfo = NetworkSignedExchangeInfo {
  -- | The outer response of signed HTTP exchange which was received from network.
  networkSignedExchangeInfoOuterResponse :: NetworkResponse,
  -- | Information about the signed exchange header.
  networkSignedExchangeInfoHeader :: Maybe NetworkSignedExchangeHeader,
  -- | Security details for the signed exchange header.
  networkSignedExchangeInfoSecurityDetails :: Maybe NetworkSecurityDetails,
  -- | Errors occurred while handling the signed exchagne.
  networkSignedExchangeInfoErrors :: Maybe [NetworkSignedExchangeError]
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
  -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
  -- milliseconds relatively to this requestTime. Matches ResourceTiming's requestTime for
  -- the same request (but not for redirected requests).
  networkConnectTimingRequestTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkConnectTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkConnectTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Network.ClientSecurityState' .
data NetworkClientSecurityState = NetworkClientSecurityState {
  networkClientSecurityStateInitiatorIsSecureContext :: Bool,
  networkClientSecurityStateInitiatorIpAddressSpace :: NetworkIpAddressSpace,
  networkClientSecurityStatePrivateNetworkRequestPolicy :: NetworkPrivateNetworkRequestPolicy
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
  networkCrossOriginOpenerPolicyStatusValue :: NetworkCrossOriginOpenerPolicyValue,
  networkCrossOriginOpenerPolicyStatusReportOnlyValue :: NetworkCrossOriginOpenerPolicyValue,
  networkCrossOriginOpenerPolicyStatusReportingEndpoint :: Maybe String,
  networkCrossOriginOpenerPolicyStatusReportOnlyReportingEndpoint :: Maybe String
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
  networkCrossOriginEmbedderPolicyStatusValue :: NetworkCrossOriginEmbedderPolicyValue,
  networkCrossOriginEmbedderPolicyStatusReportOnlyValue :: NetworkCrossOriginEmbedderPolicyValue,
  networkCrossOriginEmbedderPolicyStatusReportingEndpoint :: Maybe String,
  networkCrossOriginEmbedderPolicyStatusReportOnlyReportingEndpoint :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCrossOriginEmbedderPolicyStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  NetworkCrossOriginEmbedderPolicyStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }



-- | Type 'Network.SecurityIsolationStatus' .
data NetworkSecurityIsolationStatus = NetworkSecurityIsolationStatus {
  networkSecurityIsolationStatusCoop :: Maybe NetworkCrossOriginOpenerPolicyStatus,
  networkSecurityIsolationStatusCoep :: Maybe NetworkCrossOriginEmbedderPolicyStatus
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
  networkReportingApiReportId :: NetworkReportId,
  -- | The URL of the document that triggered the report.
  networkReportingApiReportInitiatorUrl :: String,
  -- | The name of the endpoint group that should be used to deliver the report.
  networkReportingApiReportDestination :: String,
  -- | The type of the report (specifies the set of data that is contained in the report body).
  networkReportingApiReportType :: String,
  -- | When the report was generated.
  networkReportingApiReportTimestamp :: NetworkTimeSinceEpoch,
  -- | How many uploads deep the related request was.
  networkReportingApiReportDepth :: Int,
  -- | The number of delivery attempts made so far, not including an active attempt.
  networkReportingApiReportCompletedAttempts :: Int,
  networkReportingApiReportBody :: [(String, String)],
  networkReportingApiReportStatus :: NetworkReportStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type 'Network.ReportingApiEndpoint' .
data NetworkReportingApiEndpoint = NetworkReportingApiEndpoint {
  -- | The URL of the endpoint to which reports may be delivered.
  networkReportingApiEndpointUrl :: String,
  -- | Name of the endpoint group.
  networkReportingApiEndpointGroupName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | An object providing the result of a network resource load.
data NetworkLoadNetworkResourcePageResult = NetworkLoadNetworkResourcePageResult {
  networkLoadNetworkResourcePageResultSuccess :: Bool,
  -- | Optional values used for error reporting.
  networkLoadNetworkResourcePageResultNetError :: Maybe Double,
  networkLoadNetworkResourcePageResultNetErrorName :: Maybe String,
  networkLoadNetworkResourcePageResultHttpStatusCode :: Maybe Double,
  -- | If successful, one of the following two fields holds the result.
  networkLoadNetworkResourcePageResultStream :: Maybe IO.IoStreamHandle,
  -- | Response headers.
  networkLoadNetworkResourcePageResultHeaders :: Maybe NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourcePageResult  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourcePageResult where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | An options object that may be extended later to better support CORS,
-- CORB and streaming.
data NetworkLoadNetworkResourceOptions = NetworkLoadNetworkResourceOptions {
  networkLoadNetworkResourceOptionsDisableCache :: Bool,
  networkLoadNetworkResourceOptionsIncludeCredentials :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourceOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourceOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }





-- | Type of the 'Network.dataReceived' event.
data NetworkDataReceived = NetworkDataReceived {
  -- | Request identifier.
  networkDataReceivedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkDataReceivedTimestamp :: NetworkMonotonicTime,
  -- | Data chunk length.
  networkDataReceivedDataLength :: Int,
  -- | Actual bytes received (might be less than dataLength for compressed encodings).
  networkDataReceivedEncodedDataLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkDataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  NetworkDataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Network.eventSourceMessageReceived' event.
data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
  -- | Request identifier.
  networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
  -- | Message type.
  networkEventSourceMessageReceivedEventName :: String,
  -- | Message identifier.
  networkEventSourceMessageReceivedEventId :: String,
  -- | Message content.
  networkEventSourceMessageReceivedData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkEventSourceMessageReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkEventSourceMessageReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type of the 'Network.loadingFailed' event.
data NetworkLoadingFailed = NetworkLoadingFailed {
  -- | Request identifier.
  networkLoadingFailedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkLoadingFailedTimestamp :: NetworkMonotonicTime,
  -- | Resource type.
  networkLoadingFailedType :: NetworkResourceType,
  -- | User friendly error message.
  networkLoadingFailedErrorText :: String,
  -- | True if loading was canceled.
  networkLoadingFailedCanceled :: Maybe Bool,
  -- | The reason why loading was blocked, if any.
  networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
  -- | The reason why loading was blocked by CORS, if any.
  networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFailed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFailed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'Network.loadingFinished' event.
data NetworkLoadingFinished = NetworkLoadingFinished {
  -- | Request identifier.
  networkLoadingFinishedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
  -- | Total number of bytes received for this request.
  networkLoadingFinishedEncodedDataLength :: Double,
  -- | Set when 1) response was blocked by Cross-Origin Read Blocking and also
  -- 2) this needs to be reported to the DevTools console.
  networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Network.requestServedFromCache' event.
data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
  -- | Request identifier.
  networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestServedFromCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestServedFromCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.requestWillBeSent' event.
data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
  -- | Request identifier.
  networkRequestWillBeSentRequestId :: NetworkRequestId,
  -- | Loader identifier. Empty string if the request is fetched from worker.
  networkRequestWillBeSentLoaderId :: NetworkLoaderId,
  -- | URL of the document this request is loaded for.
  networkRequestWillBeSentDocumentUrl :: String,
  -- | Request data.
  networkRequestWillBeSentRequest :: NetworkRequest,
  -- | Timestamp.
  networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
  -- | Timestamp.
  networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
  -- | Request initiator.
  networkRequestWillBeSentInitiator :: NetworkInitiator,
  -- | In the case that redirectResponse is populated, this flag indicates whether
  -- requestWillBeSentExtraInfo and responseReceivedExtraInfo events will be or were emitted
  -- for the request which was just redirected.
  networkRequestWillBeSentRedirectHasExtraInfo :: Bool,
  -- | Redirect response data.
  networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
  -- | Type of this resource.
  networkRequestWillBeSentType :: Maybe NetworkResourceType,
  -- | Frame identifier.
  networkRequestWillBeSentFrameId :: Maybe PageFrameId,
  -- | Whether the request is initiated by a user gesture. Defaults to false.
  networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'Network.resourceChangedPriority' event.
data NetworkResourceChangedPriority = NetworkResourceChangedPriority {
  -- | Request identifier.
  networkResourceChangedPriorityRequestId :: NetworkRequestId,
  -- | New priority
  networkResourceChangedPriorityNewPriority :: NetworkResourcePriority,
  -- | Timestamp.
  networkResourceChangedPriorityTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceChangedPriority  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceChangedPriority where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.signedExchangeReceived' event.
data NetworkSignedExchangeReceived = NetworkSignedExchangeReceived {
  -- | Request identifier.
  networkSignedExchangeReceivedRequestId :: NetworkRequestId,
  -- | Information about the signed exchange response.
  networkSignedExchangeReceivedInfo :: NetworkSignedExchangeInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.responseReceived' event.
data NetworkResponseReceived = NetworkResponseReceived {
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
  -- or were emitted for this request.
  networkResponseReceivedHasExtraInfo :: Bool,
  -- | Frame identifier.
  networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Network.webSocketClosed' event.
data NetworkWebSocketClosed = NetworkWebSocketClosed {
  -- | Request identifier.
  networkWebSocketClosedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Network.webSocketCreated' event.
data NetworkWebSocketCreated = NetworkWebSocketCreated {
  -- | Request identifier.
  networkWebSocketCreatedRequestId :: NetworkRequestId,
  -- | WebSocket request URL.
  networkWebSocketCreatedUrl :: String,
  -- | Request initiator.
  networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Network.webSocketFrameError' event.
data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
  -- | Request identifier.
  networkWebSocketFrameErrorRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
  -- | WebSocket error message.
  networkWebSocketFrameErrorErrorMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Network.webSocketFrameReceived' event.
data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
  -- | Request identifier.
  networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
  -- | WebSocket response data.
  networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Network.webSocketFrameSent' event.
data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
  -- | Request identifier.
  networkWebSocketFrameSentRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
  -- | WebSocket response data.
  networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Network.webSocketHandshakeResponseReceived' event.
data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
  -- | Request identifier.
  networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
  -- | WebSocket response data.
  networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | Type of the 'Network.webSocketWillSendHandshakeRequest' event.
data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
  -- | Request identifier.
  networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
  -- | UTC Timestamp.
  networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
  -- | WebSocket request data.
  networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.webTransportCreated' event.
data NetworkWebTransportCreated = NetworkWebTransportCreated {
  -- | WebTransport identifier.
  networkWebTransportCreatedTransportId :: NetworkRequestId,
  -- | WebTransport request URL.
  networkWebTransportCreatedUrl :: String,
  -- | Timestamp.
  networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
  -- | Request initiator.
  networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Network.webTransportConnectionEstablished' event.
data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
  -- | WebTransport identifier.
  networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
  -- | Timestamp.
  networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportConnectionEstablished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportConnectionEstablished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.webTransportClosed' event.
data NetworkWebTransportClosed = NetworkWebTransportClosed {
  -- | WebTransport identifier.
  networkWebTransportClosedTransportId :: NetworkRequestId,
  -- | Timestamp.
  networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Network.requestWillBeSentExtraInfo' event.
data NetworkRequestWillBeSentExtraInfo = NetworkRequestWillBeSentExtraInfo {
  -- | Request identifier. Used to match this information to an existing requestWillBeSent event.
  networkRequestWillBeSentExtraInfoRequestId :: NetworkRequestId,
  -- | A list of cookies potentially associated to the requested URL. This includes both cookies sent with
  -- the request and the ones not sent; the latter are distinguished by having blockedReason field set.
  networkRequestWillBeSentExtraInfoAssociatedCookies :: [NetworkBlockedCookieWithReason],
  -- | Raw request headers as they will be sent over the wire.
  networkRequestWillBeSentExtraInfoHeaders :: NetworkHeaders,
  -- | Connection timing information for the request.
  networkRequestWillBeSentExtraInfoConnectTiming :: NetworkConnectTiming,
  -- | The client security state set for the request.
  networkRequestWillBeSentExtraInfoClientSecurityState :: Maybe NetworkClientSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSentExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSentExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type of the 'Network.responseReceivedExtraInfo' event.
data NetworkResponseReceivedExtraInfo = NetworkResponseReceivedExtraInfo {
  -- | Request identifier. Used to match this information to another responseReceived event.
  networkResponseReceivedExtraInfoRequestId :: NetworkRequestId,
  -- | A list of cookies which were not stored from the response along with the corresponding
  -- reasons for blocking. The cookies here may not be valid due to syntax errors, which
  -- are represented by the invalid cookie line string instead of a proper cookie.
  networkResponseReceivedExtraInfoBlockedCookies :: [NetworkBlockedSetCookieWithReason],
  -- | Raw response headers as they were received over the wire.
  networkResponseReceivedExtraInfoHeaders :: NetworkHeaders,
  -- | The IP address space of the resource. The address space can only be determined once the transport
  -- established the connection, so we can't send it in `requestWillBeSentExtraInfo`.
  networkResponseReceivedExtraInfoResourceIpAddressSpace :: NetworkIpAddressSpace,
  -- | The status code of the response. This is useful in cases the request failed and no responseReceived
  -- event is triggered, which is the case for, e.g., CORS errors. This is also the correct status code
  -- for cached requests, where the status in responseReceived is a 200 and this will be 304.
  networkResponseReceivedExtraInfoStatusCode :: Int,
  -- | Raw response header text as it was received over the wire. The raw text may not always be
  -- available, such as in the case of HTTP/2 or QUIC.
  networkResponseReceivedExtraInfoHeadersText :: Maybe String
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
  -- | Detailed success or error status of the operation.
  -- 'AlreadyExists' also signifies a successful operation, as the result
  -- of the operation already exists und thus, the operation was abort
  -- preemptively (e.g. a cache hit).
  networkTrustTokenOperationDoneStatus :: NetworkTrustTokenOperationDoneStatus,
  networkTrustTokenOperationDoneType :: NetworkTrustTokenOperationType,
  networkTrustTokenOperationDoneRequestId :: NetworkRequestId,
  -- | Top level origin. The context in which the operation was attempted.
  networkTrustTokenOperationDoneTopLevelOrigin :: Maybe String,
  -- | Origin of the issuer in case of a "Issuance" or "Redemption" operation.
  networkTrustTokenOperationDoneIssuerOrigin :: Maybe String,
  -- | The number of obtained Trust Tokens on a successful "Issuance" operation.
  networkTrustTokenOperationDoneIssuedTokenCount :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenOperationDone  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenOperationDone where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.subresourceWebBundleMetadataReceived' event.
data NetworkSubresourceWebBundleMetadataReceived = NetworkSubresourceWebBundleMetadataReceived {
  -- | Request identifier. Used to match this information to another event.
  networkSubresourceWebBundleMetadataReceivedRequestId :: NetworkRequestId,
  -- | A list of URLs of resources in the subresource Web Bundle.
  networkSubresourceWebBundleMetadataReceivedUrls :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }



-- | Type of the 'Network.subresourceWebBundleMetadataError' event.
data NetworkSubresourceWebBundleMetadataError = NetworkSubresourceWebBundleMetadataError {
  -- | Request identifier. Used to match this information to another event.
  networkSubresourceWebBundleMetadataErrorRequestId :: NetworkRequestId,
  -- | Error message
  networkSubresourceWebBundleMetadataErrorErrorMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



-- | Type of the 'Network.subresourceWebBundleInnerResponseParsed' event.
data NetworkSubresourceWebBundleInnerResponseParsed = NetworkSubresourceWebBundleInnerResponseParsed {
  -- | Request identifier of the subresource request
  networkSubresourceWebBundleInnerResponseParsedInnerRequestId :: NetworkRequestId,
  -- | URL of the subresource resource.
  networkSubresourceWebBundleInnerResponseParsedInnerRequestUrl :: String,
  -- | Bundle request identifier. Used to match this information to another event.
  -- This made be absent in case when the instrumentation was enabled only
  -- after webbundle was parsed.
  networkSubresourceWebBundleInnerResponseParsedBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }



-- | Type of the 'Network.subresourceWebBundleInnerResponseError' event.
data NetworkSubresourceWebBundleInnerResponseError = NetworkSubresourceWebBundleInnerResponseError {
  -- | Request identifier of the subresource request
  networkSubresourceWebBundleInnerResponseErrorInnerRequestId :: NetworkRequestId,
  -- | URL of the subresource resource.
  networkSubresourceWebBundleInnerResponseErrorInnerRequestUrl :: String,
  -- | Error message
  networkSubresourceWebBundleInnerResponseErrorErrorMessage :: String,
  -- | Bundle request identifier. Used to match this information to another event.
  -- This made be absent in case when the instrumentation was enabled only
  -- after webbundle was parsed.
  networkSubresourceWebBundleInnerResponseErrorBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



-- | Type of the 'Network.reportingApiReportAdded' event.
data NetworkReportingApiReportAdded = NetworkReportingApiReportAdded {
  networkReportingApiReportAddedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Network.reportingApiReportUpdated' event.
data NetworkReportingApiReportUpdated = NetworkReportingApiReportUpdated {
  networkReportingApiReportUpdatedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type of the 'Network.reportingApiEndpointsChangedForOrigin' event.
data NetworkReportingApiEndpointsChangedForOrigin = NetworkReportingApiEndpointsChangedForOrigin {
  -- | Origin of the document(s) which configured the endpoints.
  networkReportingApiEndpointsChangedForOriginOrigin :: String,
  networkReportingApiEndpointsChangedForOriginEndpoints :: [NetworkReportingApiEndpoint]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpointsChangedForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpointsChangedForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }





-- | Parameters of the 'networkSetAcceptedEncodings' command.
data PNetworkSetAcceptedEncodings = PNetworkSetAcceptedEncodings {
  -- | List of accepted content encodings.
  pNetworkSetAcceptedEncodingsEncodings :: [NetworkContentEncoding]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAcceptedEncodings  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAcceptedEncodings where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Network.setAcceptedEncodings' command.
-- Sets a list of content encodings that will be accepted. Empty list means no encoding is accepted.
-- Parameters: 'PNetworkSetAcceptedEncodings'
networkSetAcceptedEncodings :: Handle ev -> PNetworkSetAcceptedEncodings -> IO (Maybe Error)
networkSetAcceptedEncodings handle params = sendReceiveCommand handle "Network.setAcceptedEncodings" (Just params)


-- | Function for the 'Network.clearAcceptedEncodingsOverride' command.
-- Clears accepted encodings set by setAcceptedEncodings
networkClearAcceptedEncodingsOverride :: Handle ev -> IO (Maybe Error)
networkClearAcceptedEncodingsOverride handle = sendReceiveCommand handle "Network.clearAcceptedEncodingsOverride" (Nothing :: Maybe ())


-- | Function for the 'Network.clearBrowserCache' command.
-- Clears browser cache.
networkClearBrowserCache :: Handle ev -> IO (Maybe Error)
networkClearBrowserCache handle = sendReceiveCommand handle "Network.clearBrowserCache" (Nothing :: Maybe ())


-- | Function for the 'Network.clearBrowserCookies' command.
-- Clears browser cookies.
networkClearBrowserCookies :: Handle ev -> IO (Maybe Error)
networkClearBrowserCookies handle = sendReceiveCommand handle "Network.clearBrowserCookies" (Nothing :: Maybe ())


-- | Parameters of the 'networkDeleteCookies' command.
data PNetworkDeleteCookies = PNetworkDeleteCookies {
  -- | Name of the cookies to remove.
  pNetworkDeleteCookiesName :: String,
  -- | If specified, deletes all the cookies with the given name where domain and path match
  -- provided URL.
  pNetworkDeleteCookiesUrl :: Maybe String,
  -- | If specified, deletes only cookies with the exact domain.
  pNetworkDeleteCookiesDomain :: Maybe String,
  -- | If specified, deletes only cookies with the exact path.
  pNetworkDeleteCookiesPath :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkDeleteCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PNetworkDeleteCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Network.deleteCookies' command.
-- Deletes browser cookies with matching name and url or domain/path pair.
-- Parameters: 'PNetworkDeleteCookies'
networkDeleteCookies :: Handle ev -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies handle params = sendReceiveCommand handle "Network.deleteCookies" (Just params)


-- | Function for the 'Network.disable' command.
-- Disables network tracking, prevents network events from being sent to the client.
networkDisable :: Handle ev -> IO (Maybe Error)
networkDisable handle = sendReceiveCommand handle "Network.disable" (Nothing :: Maybe ())


-- | Parameters of the 'networkEmulateNetworkConditions' command.
data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEmulateNetworkConditions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PNetworkEmulateNetworkConditions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'Network.emulateNetworkConditions' command.
-- Activates emulation of network conditions.
-- Parameters: 'PNetworkEmulateNetworkConditions'
networkEmulateNetworkConditions :: Handle ev -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions handle params = sendReceiveCommand handle "Network.emulateNetworkConditions" (Just params)


-- | Parameters of the 'networkEnable' command.
data PNetworkEnable = PNetworkEnable {
  -- | Buffer size in bytes to use when preserving network payloads (XHRs, etc).
  pNetworkEnableMaxTotalBufferSize :: Maybe Int,
  -- | Per-resource buffer size in bytes to use when preserving network payloads (XHRs, etc).
  pNetworkEnableMaxResourceBufferSize :: Maybe Int,
  -- | Longest post body size (in bytes) that would be included in requestWillBeSent notification
  pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


-- | Function for the 'Network.enable' command.
-- Enables network tracking, network events will now be delivered to the client.
-- Parameters: 'PNetworkEnable'
networkEnable :: Handle ev -> PNetworkEnable -> IO (Maybe Error)
networkEnable handle params = sendReceiveCommand handle "Network.enable" (Just params)


-- | Function for the 'Network.getAllCookies' command.
-- Returns all browser cookies. Depending on the backend support, will return detailed cookie
-- information in the `cookies` field.
-- Returns: 'NetworkGetAllCookies'
networkGetAllCookies :: Handle ev -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies handle = sendReceiveCommandResult handle "Network.getAllCookies" (Nothing :: Maybe ())

-- | Return type of the 'networkGetAllCookies' command.
data NetworkGetAllCookies = NetworkGetAllCookies {
  -- | Array of cookie objects.
  networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetAllCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command NetworkGetAllCookies where
   commandName _ = "Network.getAllCookies"



-- | Parameters of the 'networkGetCertificate' command.
data PNetworkGetCertificate = PNetworkGetCertificate {
  -- | Origin to get certificate for.
  pNetworkGetCertificateOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCertificate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Network.getCertificate' command.
-- Returns the DER-encoded certificate.
-- Parameters: 'PNetworkGetCertificate'
-- Returns: 'NetworkGetCertificate'
networkGetCertificate :: Handle ev -> PNetworkGetCertificate -> IO (Either Error NetworkGetCertificate)
networkGetCertificate handle params = sendReceiveCommandResult handle "Network.getCertificate" (Just params)

-- | Return type of the 'networkGetCertificate' command.
data NetworkGetCertificate = NetworkGetCertificate {
  networkGetCertificateTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command NetworkGetCertificate where
   commandName _ = "Network.getCertificate"



-- | Parameters of the 'networkGetCookies' command.
data PNetworkGetCookies = PNetworkGetCookies {
  -- | The list of URLs for which applicable cookies will be fetched.
  -- If not specified, it's assumed to be set to the list containing
  -- the URLs of the page and all of its subframes.
  pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'Network.getCookies' command.
-- Returns all browser cookies for the current URL. Depending on the backend support, will return
-- detailed cookie information in the `cookies` field.
-- Parameters: 'PNetworkGetCookies'
-- Returns: 'NetworkGetCookies'
networkGetCookies :: Handle ev -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies handle params = sendReceiveCommandResult handle "Network.getCookies" (Just params)

-- | Return type of the 'networkGetCookies' command.
data NetworkGetCookies = NetworkGetCookies {
  -- | Array of cookie objects.
  networkGetCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command NetworkGetCookies where
   commandName _ = "Network.getCookies"



-- | Parameters of the 'networkGetResponseBody' command.
data PNetworkGetResponseBody = PNetworkGetResponseBody {
  -- | Identifier of the network request to get content for.
  pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Network.getResponseBody' command.
-- Returns content served for the given request.
-- Parameters: 'PNetworkGetResponseBody'
-- Returns: 'NetworkGetResponseBody'
networkGetResponseBody :: Handle ev -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody handle params = sendReceiveCommandResult handle "Network.getResponseBody" (Just params)

-- | Return type of the 'networkGetResponseBody' command.
data NetworkGetResponseBody = NetworkGetResponseBody {
  -- | Response body.
  networkGetResponseBodyBody :: String,
  -- | True, if content was sent as base64.
  networkGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command NetworkGetResponseBody where
   commandName _ = "Network.getResponseBody"



-- | Parameters of the 'networkGetRequestPostData' command.
data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
  -- | Identifier of the network request to get content for.
  pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetRequestPostData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Network.getRequestPostData' command.
-- Returns post data sent with the request. Returns an error when no data was sent with the request.
-- Parameters: 'PNetworkGetRequestPostData'
-- Returns: 'NetworkGetRequestPostData'
networkGetRequestPostData :: Handle ev -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData handle params = sendReceiveCommandResult handle "Network.getRequestPostData" (Just params)

-- | Return type of the 'networkGetRequestPostData' command.
data NetworkGetRequestPostData = NetworkGetRequestPostData {
  -- | Request body string, omitting files from multipart requests
  networkGetRequestPostDataPostData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command NetworkGetRequestPostData where
   commandName _ = "Network.getRequestPostData"



-- | Parameters of the 'networkGetResponseBodyForInterception' command.
data PNetworkGetResponseBodyForInterception = PNetworkGetResponseBodyForInterception {
  -- | Identifier for the intercepted request to get body for.
  pNetworkGetResponseBodyForInterceptionInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBodyForInterception  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'Network.getResponseBodyForInterception' command.
-- Returns content served for the given currently intercepted request.
-- Parameters: 'PNetworkGetResponseBodyForInterception'
-- Returns: 'NetworkGetResponseBodyForInterception'
networkGetResponseBodyForInterception :: Handle ev -> PNetworkGetResponseBodyForInterception -> IO (Either Error NetworkGetResponseBodyForInterception)
networkGetResponseBodyForInterception handle params = sendReceiveCommandResult handle "Network.getResponseBodyForInterception" (Just params)

-- | Return type of the 'networkGetResponseBodyForInterception' command.
data NetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception {
  -- | Response body.
  networkGetResponseBodyForInterceptionBody :: String,
  -- | True, if content was sent as base64.
  networkGetResponseBodyForInterceptionBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command NetworkGetResponseBodyForInterception where
   commandName _ = "Network.getResponseBodyForInterception"



-- | Parameters of the 'networkTakeResponseBodyForInterceptionAsStream' command.
data PNetworkTakeResponseBodyForInterceptionAsStream = PNetworkTakeResponseBodyForInterceptionAsStream {
  pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkTakeResponseBodyForInterceptionAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  PNetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }


-- | Function for the 'Network.takeResponseBodyForInterceptionAsStream' command.
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
  networkTakeResponseBodyForInterceptionAsStreamStream :: IO.IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }

instance Command NetworkTakeResponseBodyForInterceptionAsStream where
   commandName _ = "Network.takeResponseBodyForInterceptionAsStream"



-- | Parameters of the 'networkReplayXhr' command.
data PNetworkReplayXhr = PNetworkReplayXhr {
  -- | Identifier of XHR to replay.
  pNetworkReplayXhrRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkReplayXhr  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkReplayXhr where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'Network.replayXHR' command.
-- This method sends a new XMLHttpRequest which is identical to the original one. The following
-- parameters should be identical: method, url, async, request body, extra headers, withCredentials
-- attribute, user, password.
-- Parameters: 'PNetworkReplayXhr'
networkReplayXhr :: Handle ev -> PNetworkReplayXhr -> IO (Maybe Error)
networkReplayXhr handle params = sendReceiveCommand handle "Network.replayXHR" (Just params)


-- | Parameters of the 'networkSearchInResponseBody' command.
data PNetworkSearchInResponseBody = PNetworkSearchInResponseBody {
  -- | Identifier of the network response to search.
  pNetworkSearchInResponseBodyRequestId :: NetworkRequestId,
  -- | String to search for.
  pNetworkSearchInResponseBodyQuery :: String,
  -- | If true, search is case sensitive.
  pNetworkSearchInResponseBodyCaseSensitive :: Maybe Bool,
  -- | If true, treats string parameter as regex.
  pNetworkSearchInResponseBodyIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSearchInResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Network.searchInResponseBody' command.
-- Searches for given string in response content.
-- Parameters: 'PNetworkSearchInResponseBody'
-- Returns: 'NetworkSearchInResponseBody'
networkSearchInResponseBody :: Handle ev -> PNetworkSearchInResponseBody -> IO (Either Error NetworkSearchInResponseBody)
networkSearchInResponseBody handle params = sendReceiveCommandResult handle "Network.searchInResponseBody" (Just params)

-- | Return type of the 'networkSearchInResponseBody' command.
data NetworkSearchInResponseBody = NetworkSearchInResponseBody {
  -- | List of search matches.
  networkSearchInResponseBodyResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command NetworkSearchInResponseBody where
   commandName _ = "Network.searchInResponseBody"



-- | Parameters of the 'networkSetBlockedUrLs' command.
data PNetworkSetBlockedUrLs = PNetworkSetBlockedUrLs {
  -- | URL patterns to block. Wildcards ('*') are allowed.
  pNetworkSetBlockedUrLsUrls :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBlockedUrLs  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBlockedUrLs where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Network.setBlockedURLs' command.
-- Blocks URLs from loading.
-- Parameters: 'PNetworkSetBlockedUrLs'
networkSetBlockedUrLs :: Handle ev -> PNetworkSetBlockedUrLs -> IO (Maybe Error)
networkSetBlockedUrLs handle params = sendReceiveCommand handle "Network.setBlockedURLs" (Just params)


-- | Parameters of the 'networkSetBypassServiceWorker' command.
data PNetworkSetBypassServiceWorker = PNetworkSetBypassServiceWorker {
  -- | Bypass service worker and load from network.
  pNetworkSetBypassServiceWorkerBypass :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBypassServiceWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBypassServiceWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Network.setBypassServiceWorker' command.
-- Toggles ignoring of service worker for each request.
-- Parameters: 'PNetworkSetBypassServiceWorker'
networkSetBypassServiceWorker :: Handle ev -> PNetworkSetBypassServiceWorker -> IO (Maybe Error)
networkSetBypassServiceWorker handle params = sendReceiveCommand handle "Network.setBypassServiceWorker" (Just params)


-- | Parameters of the 'networkSetCacheDisabled' command.
data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
  -- | Cache disabled state.
  pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCacheDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCacheDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Network.setCacheDisabled' command.
-- Toggles ignoring cache for each request. If `true`, cache will not be used.
-- Parameters: 'PNetworkSetCacheDisabled'
networkSetCacheDisabled :: Handle ev -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled handle params = sendReceiveCommand handle "Network.setCacheDisabled" (Just params)


-- | Parameters of the 'networkSetCookie' command.
data PNetworkSetCookie = PNetworkSetCookie {
  -- | Cookie name.
  pNetworkSetCookieName :: String,
  -- | Cookie value.
  pNetworkSetCookieValue :: String,
  -- | The request-URI to associate with the setting of the cookie. This value can affect the
  -- default domain, path, source port, and source scheme values of the created cookie.
  pNetworkSetCookieUrl :: Maybe String,
  -- | Cookie domain.
  pNetworkSetCookieDomain :: Maybe String,
  -- | Cookie path.
  pNetworkSetCookiePath :: Maybe String,
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
  -- An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  -- This is a temporary ability and it will be removed in the future.
  pNetworkSetCookieSourcePort :: Maybe Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  -- of the request to the endpoint that set the cookie.
  -- If not set, the cookie will be set as not partitioned.
  pNetworkSetCookiePartitionKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'Network.setCookie' command.
-- Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.
-- Parameters: 'PNetworkSetCookie'
networkSetCookie :: Handle ev -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie handle params = sendReceiveCommand handle "Network.setCookie" (Just params)


-- | Parameters of the 'networkSetCookies' command.
data PNetworkSetCookies = PNetworkSetCookies {
  -- | Cookies to be set.
  pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'Network.setCookies' command.
-- Sets given cookies.
-- Parameters: 'PNetworkSetCookies'
networkSetCookies :: Handle ev -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies handle params = sendReceiveCommand handle "Network.setCookies" (Just params)


-- | Parameters of the 'networkSetExtraHttpHeaders' command.
data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
  -- | Map with extra HTTP headers.
  pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetExtraHttpHeaders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetExtraHttpHeaders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Network.setExtraHTTPHeaders' command.
-- Specifies whether to always send extra HTTP headers with the requests from this page.
-- Parameters: 'PNetworkSetExtraHttpHeaders'
networkSetExtraHttpHeaders :: Handle ev -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders handle params = sendReceiveCommand handle "Network.setExtraHTTPHeaders" (Just params)


-- | Parameters of the 'networkSetAttachDebugStack' command.
data PNetworkSetAttachDebugStack = PNetworkSetAttachDebugStack {
  -- | Whether to attach a page script stack for debugging purpose.
  pNetworkSetAttachDebugStackEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAttachDebugStack  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAttachDebugStack where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Network.setAttachDebugStack' command.
-- Specifies whether to attach a page script stack id in requests
-- Parameters: 'PNetworkSetAttachDebugStack'
networkSetAttachDebugStack :: Handle ev -> PNetworkSetAttachDebugStack -> IO (Maybe Error)
networkSetAttachDebugStack handle params = sendReceiveCommand handle "Network.setAttachDebugStack" (Just params)


-- | Parameters of the 'networkSetUserAgentOverride' command.
data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
  -- | User agent to use.
  pNetworkSetUserAgentOverrideUserAgent :: String,
  -- | Browser langugage to emulate.
  pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
  -- | The platform navigator.platform should return.
  pNetworkSetUserAgentOverridePlatform :: Maybe String,
  -- | To be sent in Sec-CH-UA-* headers and returned in navigator.userAgentData
  pNetworkSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Network.setUserAgentOverride' command.
-- Allows overriding user agent with the given string.
-- Parameters: 'PNetworkSetUserAgentOverride'
networkSetUserAgentOverride :: Handle ev -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)


-- | Parameters of the 'networkGetSecurityIsolationStatus' command.
data PNetworkGetSecurityIsolationStatus = PNetworkGetSecurityIsolationStatus {
  -- | If no frameId is provided, the status of the target is provided.
  pNetworkGetSecurityIsolationStatusFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Network.getSecurityIsolationStatus' command.
-- Returns information about the COEP/COOP isolation status.
-- Parameters: 'PNetworkGetSecurityIsolationStatus'
-- Returns: 'NetworkGetSecurityIsolationStatus'
networkGetSecurityIsolationStatus :: Handle ev -> PNetworkGetSecurityIsolationStatus -> IO (Either Error NetworkGetSecurityIsolationStatus)
networkGetSecurityIsolationStatus handle params = sendReceiveCommandResult handle "Network.getSecurityIsolationStatus" (Just params)

-- | Return type of the 'networkGetSecurityIsolationStatus' command.
data NetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus {
  networkGetSecurityIsolationStatusStatus :: NetworkSecurityIsolationStatus
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command NetworkGetSecurityIsolationStatus where
   commandName _ = "Network.getSecurityIsolationStatus"



-- | Parameters of the 'networkEnableReportingApi' command.
data PNetworkEnableReportingApi = PNetworkEnableReportingApi {
  -- | Whether to enable or disable events for the Reporting API
  pNetworkEnableReportingApiEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnableReportingApi  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnableReportingApi where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Network.enableReportingApi' command.
-- Enables tracking for the Reporting API, events generated by the Reporting API will now be delivered to the client.
-- Enabling triggers 'reportingApiReportAdded' for all existing reports.
-- Parameters: 'PNetworkEnableReportingApi'
networkEnableReportingApi :: Handle ev -> PNetworkEnableReportingApi -> IO (Maybe Error)
networkEnableReportingApi handle params = sendReceiveCommand handle "Network.enableReportingApi" (Just params)


-- | Parameters of the 'networkLoadNetworkResource' command.
data PNetworkLoadNetworkResource = PNetworkLoadNetworkResource {
  -- | Frame id to get the resource for. Mandatory for frame targets, and
  -- should be omitted for worker targets.
  pNetworkLoadNetworkResourceFrameId :: Maybe PageFrameId,
  -- | URL of the resource to get content for.
  pNetworkLoadNetworkResourceUrl :: String,
  -- | Options for the request.
  pNetworkLoadNetworkResourceOptions :: NetworkLoadNetworkResourceOptions
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkLoadNetworkResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Network.loadNetworkResource' command.
-- Fetches the resource and returns the content.
-- Parameters: 'PNetworkLoadNetworkResource'
-- Returns: 'NetworkLoadNetworkResource'
networkLoadNetworkResource :: Handle ev -> PNetworkLoadNetworkResource -> IO (Either Error NetworkLoadNetworkResource)
networkLoadNetworkResource handle params = sendReceiveCommandResult handle "Network.loadNetworkResource" (Just params)

-- | Return type of the 'networkLoadNetworkResource' command.
data NetworkLoadNetworkResource = NetworkLoadNetworkResource {
  networkLoadNetworkResourceResource :: NetworkLoadNetworkResourcePageResult
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
  pageAdFrameStatusAdFrameType :: PageAdFrameType,
  pageAdFrameStatusExplanations :: Maybe [PageAdFrameExplanation]
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
  pagePermissionsPolicyBlockLocatorFrameId :: PageFrameId,
  pagePermissionsPolicyBlockLocatorBlockReason :: PagePermissionsPolicyBlockReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyBlockLocator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyBlockLocator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Page.PermissionsPolicyFeatureState' .
data PagePermissionsPolicyFeatureState = PagePermissionsPolicyFeatureState {
  pagePermissionsPolicyFeatureStateFeature :: PagePermissionsPolicyFeature,
  pagePermissionsPolicyFeatureStateAllowed :: Bool,
  pagePermissionsPolicyFeatureStateLocator :: Maybe PagePermissionsPolicyBlockLocator
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
  pageOriginTrialTokenOrigin :: String,
  pageOriginTrialTokenMatchSubDomains :: Bool,
  pageOriginTrialTokenTrialName :: String,
  pageOriginTrialTokenExpiryTime :: NetworkTimeSinceEpoch,
  pageOriginTrialTokenIsThirdParty :: Bool,
  pageOriginTrialTokenUsageRestriction :: PageOriginTrialUsageRestriction
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialToken  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialToken where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Page.OriginTrialTokenWithStatus' .
data PageOriginTrialTokenWithStatus = PageOriginTrialTokenWithStatus {
  pageOriginTrialTokenWithStatusRawTokenText :: String,
  -- | `parsedToken` is present only when the token is extractable and
  -- parsable.
  pageOriginTrialTokenWithStatusParsedToken :: Maybe PageOriginTrialToken,
  pageOriginTrialTokenWithStatusStatus :: PageOriginTrialTokenStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialTokenWithStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialTokenWithStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Page.OriginTrial' .
data PageOriginTrial = PageOriginTrial {
  pageOriginTrialTrialName :: String,
  pageOriginTrialStatus :: PageOriginTrialStatus,
  pageOriginTrialTokensWithStatus :: [PageOriginTrialTokenWithStatus]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrial  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrial where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Information about the Frame on the page.
data PageFrame = PageFrame {
  -- | Frame unique identifier.
  pageFrameId :: PageFrameId,
  -- | Parent frame identifier.
  pageFrameParentId :: Maybe PageFrameId,
  -- | Identifier of the loader associated with this frame.
  pageFrameLoaderId :: NetworkLoaderId,
  -- | Frame's name as specified in the tag.
  pageFrameName :: Maybe String,
  -- | Frame document's URL without fragment.
  pageFrameUrl :: String,
  -- | Frame document's URL fragment including the '#'.
  pageFrameUrlFragment :: Maybe String,
  -- | Frame document's registered domain, taking the public suffixes list into account.
  -- Extracted from the Frame's url.
  -- Example URLs: http://www.google.com/file.html -> "google.com"
  --               http://a.b.co.uk/file.html      -> "b.co.uk"
  pageFrameDomainAndRegistry :: String,
  -- | Frame document's security origin.
  pageFrameSecurityOrigin :: String,
  -- | Frame document's mimeType as determined by the browser.
  pageFrameMimeType :: String,
  -- | If the frame failed to load, this contains the URL that could not be loaded. Note that unlike url above, this URL may contain a fragment.
  pageFrameUnreachableUrl :: Maybe String,
  -- | Indicates whether this frame was tagged as an ad and why.
  pageFrameAdFrameStatus :: Maybe PageAdFrameStatus,
  -- | Indicates whether the main document is a secure context and explains why that is the case.
  pageFrameSecureContextType :: PageSecureContextType,
  -- | Indicates whether this is a cross origin isolated context.
  pageFrameCrossOriginIsolatedContextType :: PageCrossOriginIsolatedContextType,
  -- | Indicated which gated APIs / features are available.
  pageFrameGatedApiFeatures :: [PageGatedApiFeatures]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



-- | Information about the Resource on the page.
data PageFrameResource = PageFrameResource {
  -- | Resource URL.
  pageFrameResourceUrl :: String,
  -- | Type of this resource.
  pageFrameResourceType :: NetworkResourceType,
  -- | Resource mimeType as determined by the browser.
  pageFrameResourceMimeType :: String,
  -- | last-modified timestamp as reported by server.
  pageFrameResourceLastModified :: Maybe NetworkTimeSinceEpoch,
  -- | Resource content size.
  pageFrameResourceContentSize :: Maybe Double,
  -- | True if the resource failed to load.
  pageFrameResourceFailed :: Maybe Bool,
  -- | True if the resource was canceled during loading.
  pageFrameResourceCanceled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Information about the Frame hierarchy along with their cached resources.
data PageFrameResourceTree = PageFrameResourceTree {
  -- | Frame information for this tree item.
  pageFrameResourceTreeFrame :: PageFrame,
  -- | Child frames.
  pageFrameResourceTreeChildFrames :: Maybe [PageFrameResourceTree],
  -- | Information about frame resources.
  pageFrameResourceTreeResources :: [PageFrameResource]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResourceTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFrameResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Information about the Frame hierarchy.
data PageFrameTree = PageFrameTree {
  -- | Frame information for this tree item.
  pageFrameTreeFrame :: PageFrame,
  -- | Child frames.
  pageFrameTreeChildFrames :: Maybe [PageFrameTree]
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
  -- | Unique id of the navigation history entry.
  pageNavigationEntryId :: Int,
  -- | URL of the navigation history entry.
  pageNavigationEntryUrl :: String,
  -- | URL that the user typed in the url bar.
  pageNavigationEntryUserTypedUrl :: String,
  -- | Title of the navigation history entry.
  pageNavigationEntryTitle :: String,
  -- | Transition type.
  pageNavigationEntryTransitionType :: PageTransitionType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigationEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageNavigationEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Screencast frame metadata.
data PageScreencastFrameMetadata = PageScreencastFrameMetadata {
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
  -- | Error message.
  pageAppManifestErrorMessage :: String,
  -- | If criticial, this is a non-recoverable parse error.
  pageAppManifestErrorCritical :: Int,
  -- | Error line.
  pageAppManifestErrorLine :: Int,
  -- | Error column.
  pageAppManifestErrorColumn :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Parsed app manifest properties.
data PageAppManifestParsedProperties = PageAppManifestParsedProperties {
  -- | Computed scope value
  pageAppManifestParsedPropertiesScope :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestParsedProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestParsedProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Layout viewport position and dimensions.
data PageLayoutViewport = PageLayoutViewport {
  -- | Horizontal offset relative to the document (CSS pixels).
  pageLayoutViewportPageX :: Int,
  -- | Vertical offset relative to the document (CSS pixels).
  pageLayoutViewportPageY :: Int,
  -- | Width (CSS pixels), excludes scrollbar if present.
  pageLayoutViewportClientWidth :: Int,
  -- | Height (CSS pixels), excludes scrollbar if present.
  pageLayoutViewportClientHeight :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLayoutViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLayoutViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Visual viewport position, dimensions, and scale.
data PageVisualViewport = PageVisualViewport {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageVisualViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageVisualViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Viewport for capturing screenshot.
data PageViewport = PageViewport {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PageViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Generic font families collection.
data PageFontFamilies = PageFontFamilies {
  -- | The standard font-family.
  pageFontFamiliesStandard :: Maybe String,
  -- | The fixed font-family.
  pageFontFamiliesFixed :: Maybe String,
  -- | The serif font-family.
  pageFontFamiliesSerif :: Maybe String,
  -- | The sansSerif font-family.
  pageFontFamiliesSansSerif :: Maybe String,
  -- | The cursive font-family.
  pageFontFamiliesCursive :: Maybe String,
  -- | The fantasy font-family.
  pageFontFamiliesFantasy :: Maybe String,
  -- | The math font-family.
  pageFontFamiliesMath :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PageFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Font families collection for a script.
data PageScriptFontFamilies = PageScriptFontFamilies {
  -- | Name of the script which these font families are defined for.
  pageScriptFontFamiliesScript :: String,
  -- | Generic font families collection for the script.
  pageScriptFontFamiliesFontFamilies :: PageFontFamilies
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScriptFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PageScriptFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Default font sizes.
data PageFontSizes = PageFontSizes {
  -- | Default standard font size.
  pageFontSizesStandard :: Maybe Int,
  -- | Default fixed font size.
  pageFontSizesFixed :: Maybe Int
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
  -- | Argument name (e.g. name:'minimum-icon-size-in-pixels').
  pageInstallabilityErrorArgumentName :: String,
  -- | Argument value (e.g. value:'64').
  pageInstallabilityErrorArgumentValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageInstallabilityErrorArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageInstallabilityErrorArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | The installability error
data PageInstallabilityError = PageInstallabilityError {
  -- | The error id (e.g. 'manifest-missing-suitable-icon').
  pageInstallabilityErrorErrorId :: String,
  -- | The list of error arguments (e.g. {name:'minimum-icon-size-in-pixels', value:'64'}).
  pageInstallabilityErrorErrorArguments :: [PageInstallabilityErrorArgument]
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
  -- | The URL of the script to produce a compilation cache entry for.
  pageCompilationCacheParamsUrl :: String,
  -- | A hint to the backend whether eager compilation is recommended.
  -- (the actual compilation mode used is upon backend discretion).
  pageCompilationCacheParamsEager :: Maybe Bool
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
  -- | Type of the reason
  pageBackForwardCacheNotRestoredExplanationType :: PageBackForwardCacheNotRestoredReasonType,
  -- | Not restored reason
  pageBackForwardCacheNotRestoredExplanationReason :: PageBackForwardCacheNotRestoredReason,
  -- | Context associated with the reason. The meaning of this context is
  -- dependent on the reason:
  -- - EmbedderExtensionSentMessageToCachedFrame: the extension ID.
  pageBackForwardCacheNotRestoredExplanationContext :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'Page.BackForwardCacheNotRestoredExplanationTree' .
data PageBackForwardCacheNotRestoredExplanationTree = PageBackForwardCacheNotRestoredExplanationTree {
  -- | URL of each frame
  pageBackForwardCacheNotRestoredExplanationTreeUrl :: String,
  -- | Not restored reasons of each frame
  pageBackForwardCacheNotRestoredExplanationTreeExplanations :: [PageBackForwardCacheNotRestoredExplanation],
  -- | Array of children frame
  pageBackForwardCacheNotRestoredExplanationTreeChildren :: [PageBackForwardCacheNotRestoredExplanationTree]
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
  pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
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
  -- | Id of the frame containing input node.
  pageFileChooserOpenedFrameId :: PageFrameId,
  -- | Input node id.
  pageFileChooserOpenedBackendNodeId :: DomBackendNodeId,
  -- | Input mode.
  pageFileChooserOpenedMode :: PageFileChooserOpenedMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFileChooserOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFileChooserOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'Page.frameAttached' event.
data PageFrameAttached = PageFrameAttached {
  -- | Id of the frame that has been attached.
  pageFrameAttachedFrameId :: PageFrameId,
  -- | Parent frame identifier.
  pageFrameAttachedParentFrameId :: PageFrameId,
  -- | JavaScript stack trace of when frame was attached, only set if frame initiated from script.
  pageFrameAttachedStack :: Maybe Runtime.RuntimeStackTrace
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
  -- | Id of the frame that has been detached.
  pageFrameDetachedFrameId :: PageFrameId,
  pageFrameDetachedReason :: PageFrameDetachedReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type of the 'Page.frameNavigated' event.
data PageFrameNavigated = PageFrameNavigated {
  -- | Frame object.
  pageFrameNavigatedFrame :: PageFrame,
  pageFrameNavigatedType :: PageNavigationType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameNavigated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageFrameNavigated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.documentOpened' event.
data PageDocumentOpened = PageDocumentOpened {
  -- | Frame object.
  pageDocumentOpenedFrame :: PageFrame
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
  -- | Id of the frame that is being navigated.
  pageFrameRequestedNavigationFrameId :: PageFrameId,
  -- | The reason for the navigation.
  pageFrameRequestedNavigationReason :: PageClientNavigationReason,
  -- | The destination URL for the requested navigation.
  pageFrameRequestedNavigationUrl :: String,
  -- | The disposition for the navigation.
  pageFrameRequestedNavigationDisposition :: PageClientNavigationDisposition
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameRequestedNavigation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageFrameRequestedNavigation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type of the 'Page.frameStartedLoading' event.
data PageFrameStartedLoading = PageFrameStartedLoading {
  -- | Id of the frame that has started loading.
  pageFrameStartedLoadingFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStartedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStartedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Page.frameStoppedLoading' event.
data PageFrameStoppedLoading = PageFrameStoppedLoading {
  -- | Id of the frame that has stopped loading.
  pageFrameStoppedLoadingFrameId :: PageFrameId
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
  -- | Whether dialog was confirmed.
  pageJavascriptDialogClosedResult :: Bool,
  -- | User input in case of prompt.
  pageJavascriptDialogClosedUserInput :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Page.javascriptDialogOpening' event.
data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
  -- | Frame url.
  pageJavascriptDialogOpeningUrl :: String,
  -- | Message that will be displayed by the dialog.
  pageJavascriptDialogOpeningMessage :: String,
  -- | Dialog type.
  pageJavascriptDialogOpeningType :: PageDialogType,
  -- | True iff browser is capable showing or acting on the given dialog. When browser has no
  -- dialog handler for given target, calling alert while Page domain is engaged will stall
  -- the page execution. Execution can be resumed via calling Page.handleJavaScriptDialog.
  pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
  -- | Default dialog prompt.
  pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogOpening  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogOpening where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.lifecycleEvent' event.
data PageLifecycleEvent = PageLifecycleEvent {
  -- | Id of the frame.
  pageLifecycleEventFrameId :: PageFrameId,
  -- | Loader identifier. Empty string if the request is fetched from worker.
  pageLifecycleEventLoaderId :: NetworkLoaderId,
  pageLifecycleEventName :: String,
  pageLifecycleEventTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLifecycleEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLifecycleEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.backForwardCacheNotUsed' event.
data PageBackForwardCacheNotUsed = PageBackForwardCacheNotUsed {
  -- | The loader id for the associated navgation.
  pageBackForwardCacheNotUsedLoaderId :: NetworkLoaderId,
  -- | The frame id of the associated frame.
  pageBackForwardCacheNotUsedFrameId :: PageFrameId,
  -- | Array of reasons why the page could not be cached. This must not be empty.
  pageBackForwardCacheNotUsedNotRestoredExplanations :: [PageBackForwardCacheNotRestoredExplanation],
  -- | Tree structure of reasons why the page could not be cached for each frame.
  pageBackForwardCacheNotUsedNotRestoredExplanationsTree :: Maybe PageBackForwardCacheNotRestoredExplanationTree
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotUsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotUsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.prerenderAttemptCompleted' event.
data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
  -- | The frame id of the frame initiating prerendering.
  pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
  pagePrerenderAttemptCompletedPrerenderingUrl :: String,
  pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePrerenderAttemptCompleted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PagePrerenderAttemptCompleted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Page.loadEventFired' event.
data PageLoadEventFired = PageLoadEventFired {
  pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLoadEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLoadEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'Page.navigatedWithinDocument' event.
data PageNavigatedWithinDocument = PageNavigatedWithinDocument {
  -- | Id of the frame.
  pageNavigatedWithinDocumentFrameId :: PageFrameId,
  -- | Frame's new url.
  pageNavigatedWithinDocumentUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigatedWithinDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageNavigatedWithinDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Page.screencastFrame' event.
data PageScreencastFrame = PageScreencastFrame {
  -- | Base64-encoded compressed image. (Encoded as a base64 string when passed over JSON)
  pageScreencastFrameData :: String,
  -- | Screencast frame metadata.
  pageScreencastFrameMetadata :: PageScreencastFrameMetadata,
  -- | Frame number.
  pageScreencastFrameSessionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageScreencastFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Page.screencastVisibilityChanged' event.
data PageScreencastVisibilityChanged = PageScreencastVisibilityChanged {
  -- | True if the page is visible.
  pageScreencastVisibilityChangedVisible :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastVisibilityChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageScreencastVisibilityChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'Page.windowOpen' event.
data PageWindowOpen = PageWindowOpen {
  -- | The URL for the new window.
  pageWindowOpenUrl :: String,
  -- | Window name.
  pageWindowOpenWindowName :: String,
  -- | An array of enabled window features.
  pageWindowOpenWindowFeatures :: [String],
  -- | Whether or not it was triggered by user gesture.
  pageWindowOpenUserGesture :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageWindowOpen  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PageWindowOpen where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type of the 'Page.compilationCacheProduced' event.
data PageCompilationCacheProduced = PageCompilationCacheProduced {
  pageCompilationCacheProducedUrl :: String,
  -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
  pageCompilationCacheProducedData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheProduced  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheProduced where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }





-- | Parameters of the 'pageAddScriptToEvaluateOnNewDocument' command.
data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
  pPageAddScriptToEvaluateOnNewDocumentSource :: String,
  -- | If specified, creates an isolated world with the given name and evaluates given script in it.
  -- This world name will be used as the ExecutionContextDescription::name when the corresponding
  -- event is emitted.
  pPageAddScriptToEvaluateOnNewDocumentWorldName :: Maybe String,
  -- | Specifies whether command line API should be available to the script, defaults
  -- to false.
  pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineApi :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the 'Page.addScriptToEvaluateOnNewDocument' command.
-- Evaluates given script in every frame upon creation (before loading frame's scripts).
-- Parameters: 'PPageAddScriptToEvaluateOnNewDocument'
-- Returns: 'PageAddScriptToEvaluateOnNewDocument'
pageAddScriptToEvaluateOnNewDocument :: Handle ev -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument handle params = sendReceiveCommandResult handle "Page.addScriptToEvaluateOnNewDocument" (Just params)

-- | Return type of the 'pageAddScriptToEvaluateOnNewDocument' command.
data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
  -- | Identifier of the added script.
  pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PageAddScriptToEvaluateOnNewDocument where
   commandName _ = "Page.addScriptToEvaluateOnNewDocument"



-- | Function for the 'Page.bringToFront' command.
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
  -- | Image compression format (defaults to png).
  pPageCaptureScreenshotFormat :: PPageCaptureScreenshotFormat,
  -- | Compression quality from range [0..100] (jpeg only).
  pPageCaptureScreenshotQuality :: Maybe Int,
  -- | Capture the screenshot of a given region only.
  pPageCaptureScreenshotClip :: Maybe PageViewport,
  -- | Capture the screenshot from the surface, rather than the view. Defaults to true.
  pPageCaptureScreenshotFromSurface :: Maybe Bool,
  -- | Capture the screenshot beyond the viewport. Defaults to false.
  pPageCaptureScreenshotCaptureBeyondViewport :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureScreenshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Page.captureScreenshot' command.
-- Capture page screenshot.
-- Parameters: 'PPageCaptureScreenshot'
-- Returns: 'PageCaptureScreenshot'
pageCaptureScreenshot :: Handle ev -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot handle params = sendReceiveCommandResult handle "Page.captureScreenshot" (Just params)

-- | Return type of the 'pageCaptureScreenshot' command.
data PageCaptureScreenshot = PageCaptureScreenshot {
  -- | Base64-encoded image data. (Encoded as a base64 string when passed over JSON)
  pageCaptureScreenshotData :: String
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
  -- | Format (defaults to mhtml).
  pPageCaptureSnapshotFormat :: PPageCaptureSnapshotFormat
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Page.captureSnapshot' command.
-- Returns a snapshot of the page as a string. For MHTML format, the serialization includes
-- iframes, shadow DOM, external resources, and element-inline styles.
-- Parameters: 'PPageCaptureSnapshot'
-- Returns: 'PageCaptureSnapshot'
pageCaptureSnapshot :: Handle ev -> PPageCaptureSnapshot -> IO (Either Error PageCaptureSnapshot)
pageCaptureSnapshot handle params = sendReceiveCommandResult handle "Page.captureSnapshot" (Just params)

-- | Return type of the 'pageCaptureSnapshot' command.
data PageCaptureSnapshot = PageCaptureSnapshot {
  -- | Serialized page data.
  pageCaptureSnapshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageCaptureSnapshot where
   commandName _ = "Page.captureSnapshot"



-- | Parameters of the 'pageCreateIsolatedWorld' command.
data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
  -- | Id of the frame in which the isolated world should be created.
  pPageCreateIsolatedWorldFrameId :: PageFrameId,
  -- | An optional name which is reported in the Execution Context.
  pPageCreateIsolatedWorldWorldName :: Maybe String,
  -- | Whether or not universal access should be granted to the isolated world. This is a powerful
  -- option, use with caution.
  pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCreateIsolatedWorld  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Page.createIsolatedWorld' command.
-- Creates an isolated world for the given frame.
-- Parameters: 'PPageCreateIsolatedWorld'
-- Returns: 'PageCreateIsolatedWorld'
pageCreateIsolatedWorld :: Handle ev -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld handle params = sendReceiveCommandResult handle "Page.createIsolatedWorld" (Just params)

-- | Return type of the 'pageCreateIsolatedWorld' command.
data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
  -- | Execution context of the isolated world.
  pageCreateIsolatedWorldExecutionContextId :: Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PageCreateIsolatedWorld where
   commandName _ = "Page.createIsolatedWorld"



-- | Function for the 'Page.disable' command.
-- Disables page domain notifications.
pageDisable :: Handle ev -> IO (Maybe Error)
pageDisable handle = sendReceiveCommand handle "Page.disable" (Nothing :: Maybe ())


-- | Function for the 'Page.enable' command.
-- Enables page domain notifications.
pageEnable :: Handle ev -> IO (Maybe Error)
pageEnable handle = sendReceiveCommand handle "Page.enable" (Nothing :: Maybe ())


-- | Function for the 'Page.getAppManifest' command.
-- Returns: 'PageGetAppManifest'
pageGetAppManifest :: Handle ev -> IO (Either Error PageGetAppManifest)
pageGetAppManifest handle = sendReceiveCommandResult handle "Page.getAppManifest" (Nothing :: Maybe ())

-- | Return type of the 'pageGetAppManifest' command.
data PageGetAppManifest = PageGetAppManifest {
  -- | Manifest location.
  pageGetAppManifestUrl :: String,
  pageGetAppManifestErrors :: [PageAppManifestError],
  -- | Manifest content.
  pageGetAppManifestData :: Maybe String,
  -- | Parsed manifest properties
  pageGetAppManifestParsed :: Maybe PageAppManifestParsedProperties
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppManifest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PageGetAppManifest where
   commandName _ = "Page.getAppManifest"



-- | Function for the 'Page.getInstallabilityErrors' command.
-- Returns: 'PageGetInstallabilityErrors'
pageGetInstallabilityErrors :: Handle ev -> IO (Either Error PageGetInstallabilityErrors)
pageGetInstallabilityErrors handle = sendReceiveCommandResult handle "Page.getInstallabilityErrors" (Nothing :: Maybe ())

-- | Return type of the 'pageGetInstallabilityErrors' command.
data PageGetInstallabilityErrors = PageGetInstallabilityErrors {
  pageGetInstallabilityErrorsInstallabilityErrors :: [PageInstallabilityError]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetInstallabilityErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PageGetInstallabilityErrors where
   commandName _ = "Page.getInstallabilityErrors"



-- | Function for the 'Page.getManifestIcons' command.
-- Returns: 'PageGetManifestIcons'
pageGetManifestIcons :: Handle ev -> IO (Either Error PageGetManifestIcons)
pageGetManifestIcons handle = sendReceiveCommandResult handle "Page.getManifestIcons" (Nothing :: Maybe ())

-- | Return type of the 'pageGetManifestIcons' command.
data PageGetManifestIcons = PageGetManifestIcons {
  pageGetManifestIconsPrimaryIcon :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetManifestIcons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetManifestIcons where
   commandName _ = "Page.getManifestIcons"



-- | Function for the 'Page.getAppId' command.
-- Returns the unique (PWA) app id.
-- Only returns values if the feature flag 'WebAppEnableManifestId' is enabled
-- Returns: 'PageGetAppId'
pageGetAppId :: Handle ev -> IO (Either Error PageGetAppId)
pageGetAppId handle = sendReceiveCommandResult handle "Page.getAppId" (Nothing :: Maybe ())

-- | Return type of the 'pageGetAppId' command.
data PageGetAppId = PageGetAppId {
  -- | App id, either from manifest's id attribute or computed from start_url
  pageGetAppIdAppId :: Maybe String,
  -- | Recommendation for manifest's id attribute to match current id computed from start_url
  pageGetAppIdRecommendedId :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageGetAppId where
   commandName _ = "Page.getAppId"



-- | Function for the 'Page.getFrameTree' command.
-- Returns present frame tree structure.
-- Returns: 'PageGetFrameTree'
pageGetFrameTree :: Handle ev -> IO (Either Error PageGetFrameTree)
pageGetFrameTree handle = sendReceiveCommandResult handle "Page.getFrameTree" (Nothing :: Maybe ())

-- | Return type of the 'pageGetFrameTree' command.
data PageGetFrameTree = PageGetFrameTree {
  -- | Present frame tree structure.
  pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PageGetFrameTree where
   commandName _ = "Page.getFrameTree"



-- | Function for the 'Page.getLayoutMetrics' command.
-- Returns metrics relating to the layouting of the page, such as viewport bounds/scale.
-- Returns: 'PageGetLayoutMetrics'
pageGetLayoutMetrics :: Handle ev -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics handle = sendReceiveCommandResult handle "Page.getLayoutMetrics" (Nothing :: Maybe ())

-- | Return type of the 'pageGetLayoutMetrics' command.
data PageGetLayoutMetrics = PageGetLayoutMetrics {
  -- | Metrics relating to the layout viewport in CSS pixels.
  pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
  -- | Metrics relating to the visual viewport in CSS pixels.
  pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
  -- | Size of scrollable area in CSS pixels.
  pageGetLayoutMetricsCssContentSize :: DomRect
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetLayoutMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetLayoutMetrics where
   commandName _ = "Page.getLayoutMetrics"



-- | Function for the 'Page.getNavigationHistory' command.
-- Returns navigation history for the current page.
-- Returns: 'PageGetNavigationHistory'
pageGetNavigationHistory :: Handle ev -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory handle = sendReceiveCommandResult handle "Page.getNavigationHistory" (Nothing :: Maybe ())

-- | Return type of the 'pageGetNavigationHistory' command.
data PageGetNavigationHistory = PageGetNavigationHistory {
  -- | Index of the current navigation history entry.
  pageGetNavigationHistoryCurrentIndex :: Int,
  -- | Array of navigation history entries.
  pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetNavigationHistory where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PageGetNavigationHistory where
   commandName _ = "Page.getNavigationHistory"



-- | Function for the 'Page.resetNavigationHistory' command.
-- Resets navigation history for the current page.
pageResetNavigationHistory :: Handle ev -> IO (Maybe Error)
pageResetNavigationHistory handle = sendReceiveCommand handle "Page.resetNavigationHistory" (Nothing :: Maybe ())


-- | Parameters of the 'pageGetResourceContent' command.
data PPageGetResourceContent = PPageGetResourceContent {
  -- | Frame id to get resource for.
  pPageGetResourceContentFrameId :: PageFrameId,
  -- | URL of the resource to get content for.
  pPageGetResourceContentUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetResourceContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Page.getResourceContent' command.
-- Returns content of the given resource.
-- Parameters: 'PPageGetResourceContent'
-- Returns: 'PageGetResourceContent'
pageGetResourceContent :: Handle ev -> PPageGetResourceContent -> IO (Either Error PageGetResourceContent)
pageGetResourceContent handle params = sendReceiveCommandResult handle "Page.getResourceContent" (Just params)

-- | Return type of the 'pageGetResourceContent' command.
data PageGetResourceContent = PageGetResourceContent {
  -- | Resource content.
  pageGetResourceContentContent :: String,
  -- | True, if content was served as base64.
  pageGetResourceContentBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PageGetResourceContent where
   commandName _ = "Page.getResourceContent"



-- | Function for the 'Page.getResourceTree' command.
-- Returns present frame / resource tree structure.
-- Returns: 'PageGetResourceTree'
pageGetResourceTree :: Handle ev -> IO (Either Error PageGetResourceTree)
pageGetResourceTree handle = sendReceiveCommandResult handle "Page.getResourceTree" (Nothing :: Maybe ())

-- | Return type of the 'pageGetResourceTree' command.
data PageGetResourceTree = PageGetResourceTree {
  -- | Present frame / resource tree structure.
  pageGetResourceTreeFrameTree :: PageFrameResourceTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetResourceTree where
   commandName _ = "Page.getResourceTree"



-- | Parameters of the 'pageHandleJavaScriptDialog' command.
data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
  -- | Whether to accept or dismiss the dialog.
  pPageHandleJavaScriptDialogAccept :: Bool,
  -- | The text to enter into the dialog prompt before accepting. Used only if this is a prompt
  -- dialog.
  pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageHandleJavaScriptDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageHandleJavaScriptDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Page.handleJavaScriptDialog' command.
-- Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).
-- Parameters: 'PPageHandleJavaScriptDialog'
pageHandleJavaScriptDialog :: Handle ev -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog handle params = sendReceiveCommand handle "Page.handleJavaScriptDialog" (Just params)


-- | Parameters of the 'pageNavigate' command.
data PPageNavigate = PPageNavigate {
  -- | URL to navigate the page to.
  pPageNavigateUrl :: String,
  -- | Referrer URL.
  pPageNavigateReferrer :: Maybe String,
  -- | Intended transition type.
  pPageNavigateTransitionType :: Maybe PageTransitionType,
  -- | Frame id to navigate, if not specified navigates the top frame.
  pPageNavigateFrameId :: Maybe PageFrameId,
  -- | Referrer-policy used for the navigation.
  pPageNavigateReferrerPolicy :: Maybe PageReferrerPolicy
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PPageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


-- | Function for the 'Page.navigate' command.
-- Navigates current page to the given URL.
-- Parameters: 'PPageNavigate'
-- Returns: 'PageNavigate'
pageNavigate :: Handle ev -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate handle params = sendReceiveCommandResult handle "Page.navigate" (Just params)

-- | Return type of the 'pageNavigate' command.
data PageNavigate = PageNavigate {
  -- | Frame id that has navigated (or failed to navigate)
  pageNavigateFrameId :: PageFrameId,
  -- | Loader identifier. This is omitted in case of same-document navigation,
  -- as the previously committed loaderId would not change.
  pageNavigateLoaderId :: Maybe NetworkLoaderId,
  -- | User friendly error message, present if and only if navigation has failed.
  pageNavigateErrorText :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageNavigate where
   commandName _ = "Page.navigate"



-- | Parameters of the 'pageNavigateToHistoryEntry' command.
data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
  -- | Unique id of the entry to navigate to.
  pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigateToHistoryEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageNavigateToHistoryEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Page.navigateToHistoryEntry' command.
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
  -- | Paper orientation. Defaults to false.
  pPagePrintToPdfLandscape :: Maybe Bool,
  -- | Display header and footer. Defaults to false.
  pPagePrintToPdfDisplayHeaderFooter :: Maybe Bool,
  -- | Print background graphics. Defaults to false.
  pPagePrintToPdfPrintBackground :: Maybe Bool,
  -- | Scale of the webpage rendering. Defaults to 1.
  pPagePrintToPdfScale :: Maybe Double,
  -- | Paper width in inches. Defaults to 8.5 inches.
  pPagePrintToPdfPaperWidth :: Maybe Double,
  -- | Paper height in inches. Defaults to 11 inches.
  pPagePrintToPdfPaperHeight :: Maybe Double,
  -- | Top margin in inches. Defaults to 1cm (~0.4 inches).
  pPagePrintToPdfMarginTop :: Maybe Double,
  -- | Bottom margin in inches. Defaults to 1cm (~0.4 inches).
  pPagePrintToPdfMarginBottom :: Maybe Double,
  -- | Left margin in inches. Defaults to 1cm (~0.4 inches).
  pPagePrintToPdfMarginLeft :: Maybe Double,
  -- | Right margin in inches. Defaults to 1cm (~0.4 inches).
  pPagePrintToPdfMarginRight :: Maybe Double,
  -- | Paper ranges to print, one based, e.g., '1-5, 8, 11-13'. Pages are
  -- printed in the document order, not in the order specified, and no
  -- more than once.
  -- Defaults to empty string, which implies the entire document is printed.
  -- The page numbers are quietly capped to actual page count of the
  -- document, and ranges beyond the end of the document are ignored.
  -- If this results in no pages to print, an error is reported.
  -- It is an error to specify a range with start greater than end.
  pPagePrintToPdfPageRanges :: Maybe String,
  -- | HTML template for the print header. Should be valid HTML markup with following
  -- classes used to inject printing values into them:
  -- - `date`: formatted print date
  -- - `title`: document title
  -- - `url`: document location
  -- - `pageNumber`: current page number
  -- - `totalPages`: total pages in the document
  -- 
  -- For example, `<span class=title></span>` would generate span containing the title.
  pPagePrintToPdfHeaderTemplate :: Maybe String,
  -- | HTML template for the print footer. Should use the same format as the `headerTemplate`.
  pPagePrintToPdfFooterTemplate :: Maybe String,
  -- | Whether or not to prefer page size as defined by css. Defaults to false,
  -- in which case the content will be scaled to fit the paper size.
  pPagePrintToPdfPreferCssPageSize :: Maybe Bool,
  -- | return as stream
  pPagePrintToPdfTransferMode :: PPagePrintToPdfTransferMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPdf  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the 'Page.printToPDF' command.
-- Print page as PDF.
-- Parameters: 'PPagePrintToPdf'
-- Returns: 'PagePrintToPdf'
pagePrintToPdf :: Handle ev -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)

-- | Return type of the 'pagePrintToPdf' command.
data PagePrintToPdf = PagePrintToPdf {
  -- | Base64-encoded pdf data. Empty if |returnAsStream| is specified. (Encoded as a base64 string when passed over JSON)
  pagePrintToPdfData :: String,
  -- | A handle of the stream that holds resulting PDF data.
  pagePrintToPdfStream :: Maybe IO.IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PagePrintToPdf where
   commandName _ = "Page.printToPDF"



-- | Parameters of the 'pageReload' command.
data PPageReload = PPageReload {
  -- | If true, browser cache is ignored (as if the user pressed Shift+refresh).
  pPageReloadIgnoreCache :: Maybe Bool,
  -- | If set, the script will be injected into all frames of the inspected page after reload.
  -- Argument will be ignored if reloading dataURL origin.
  pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageReload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PPageReload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


-- | Function for the 'Page.reload' command.
-- Reloads given page optionally ignoring the cache.
-- Parameters: 'PPageReload'
pageReload :: Handle ev -> PPageReload -> IO (Maybe Error)
pageReload handle params = sendReceiveCommand handle "Page.reload" (Just params)


-- | Parameters of the 'pageRemoveScriptToEvaluateOnNewDocument' command.
data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
  pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


-- | Function for the 'Page.removeScriptToEvaluateOnNewDocument' command.
-- Removes given script from the list.
-- Parameters: 'PPageRemoveScriptToEvaluateOnNewDocument'
pageRemoveScriptToEvaluateOnNewDocument :: Handle ev -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument handle params = sendReceiveCommand handle "Page.removeScriptToEvaluateOnNewDocument" (Just params)


-- | Parameters of the 'pageScreencastFrameAck' command.
data PPageScreencastFrameAck = PPageScreencastFrameAck {
  -- | Frame number.
  pPageScreencastFrameAckSessionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageScreencastFrameAck  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageScreencastFrameAck where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Page.screencastFrameAck' command.
-- Acknowledges that a screencast frame has been received by the frontend.
-- Parameters: 'PPageScreencastFrameAck'
pageScreencastFrameAck :: Handle ev -> PPageScreencastFrameAck -> IO (Maybe Error)
pageScreencastFrameAck handle params = sendReceiveCommand handle "Page.screencastFrameAck" (Just params)


-- | Parameters of the 'pageSearchInResource' command.
data PPageSearchInResource = PPageSearchInResource {
  -- | Frame id for resource to search in.
  pPageSearchInResourceFrameId :: PageFrameId,
  -- | URL of the resource to search in.
  pPageSearchInResourceUrl :: String,
  -- | String to search for.
  pPageSearchInResourceQuery :: String,
  -- | If true, search is case sensitive.
  pPageSearchInResourceCaseSensitive :: Maybe Bool,
  -- | If true, treats string parameter as regex.
  pPageSearchInResourceIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSearchInResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PPageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Page.searchInResource' command.
-- Searches for given string in resource content.
-- Parameters: 'PPageSearchInResource'
-- Returns: 'PageSearchInResource'
pageSearchInResource :: Handle ev -> PPageSearchInResource -> IO (Either Error PageSearchInResource)
pageSearchInResource handle params = sendReceiveCommandResult handle "Page.searchInResource" (Just params)

-- | Return type of the 'pageSearchInResource' command.
data PageSearchInResource = PageSearchInResource {
  -- | List of search matches.
  pageSearchInResourceResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageSearchInResource where
   commandName _ = "Page.searchInResource"



-- | Parameters of the 'pageSetAdBlockingEnabled' command.
data PPageSetAdBlockingEnabled = PPageSetAdBlockingEnabled {
  -- | Whether to block ads.
  pPageSetAdBlockingEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetAdBlockingEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetAdBlockingEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Page.setAdBlockingEnabled' command.
-- Enable Chrome's experimental ad filter on all sites.
-- Parameters: 'PPageSetAdBlockingEnabled'
pageSetAdBlockingEnabled :: Handle ev -> PPageSetAdBlockingEnabled -> IO (Maybe Error)
pageSetAdBlockingEnabled handle params = sendReceiveCommand handle "Page.setAdBlockingEnabled" (Just params)


-- | Parameters of the 'pageSetBypassCsp' command.
data PPageSetBypassCsp = PPageSetBypassCsp {
  -- | Whether to bypass page CSP.
  pPageSetBypassCspEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetBypassCsp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetBypassCsp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'Page.setBypassCSP' command.
-- Enable page Content Security Policy by-passing.
-- Parameters: 'PPageSetBypassCsp'
pageSetBypassCsp :: Handle ev -> PPageSetBypassCsp -> IO (Maybe Error)
pageSetBypassCsp handle params = sendReceiveCommand handle "Page.setBypassCSP" (Just params)


-- | Parameters of the 'pageGetPermissionsPolicyState' command.
data PPageGetPermissionsPolicyState = PPageGetPermissionsPolicyState {
  pPageGetPermissionsPolicyStateFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetPermissionsPolicyState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Page.getPermissionsPolicyState' command.
-- Get Permissions Policy state on given frame.
-- Parameters: 'PPageGetPermissionsPolicyState'
-- Returns: 'PageGetPermissionsPolicyState'
pageGetPermissionsPolicyState :: Handle ev -> PPageGetPermissionsPolicyState -> IO (Either Error PageGetPermissionsPolicyState)
pageGetPermissionsPolicyState handle params = sendReceiveCommandResult handle "Page.getPermissionsPolicyState" (Just params)

-- | Return type of the 'pageGetPermissionsPolicyState' command.
data PageGetPermissionsPolicyState = PageGetPermissionsPolicyState {
  pageGetPermissionsPolicyStateStates :: [PagePermissionsPolicyFeatureState]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PageGetPermissionsPolicyState where
   commandName _ = "Page.getPermissionsPolicyState"



-- | Parameters of the 'pageGetOriginTrials' command.
data PPageGetOriginTrials = PPageGetOriginTrials {
  pPageGetOriginTrialsFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetOriginTrials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Page.getOriginTrials' command.
-- Get Origin Trials on given frame.
-- Parameters: 'PPageGetOriginTrials'
-- Returns: 'PageGetOriginTrials'
pageGetOriginTrials :: Handle ev -> PPageGetOriginTrials -> IO (Either Error PageGetOriginTrials)
pageGetOriginTrials handle params = sendReceiveCommandResult handle "Page.getOriginTrials" (Just params)

-- | Return type of the 'pageGetOriginTrials' command.
data PageGetOriginTrials = PageGetOriginTrials {
  pageGetOriginTrialsOriginTrials :: [PageOriginTrial]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetOriginTrials where
   commandName _ = "Page.getOriginTrials"



-- | Parameters of the 'pageSetFontFamilies' command.
data PPageSetFontFamilies = PPageSetFontFamilies {
  -- | Specifies font families to set. If a font family is not specified, it won't be changed.
  pPageSetFontFamiliesFontFamilies :: PageFontFamilies,
  -- | Specifies font families to set for individual scripts.
  pPageSetFontFamiliesForScripts :: Maybe [PageScriptFontFamilies]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Page.setFontFamilies' command.
-- Set generic font families.
-- Parameters: 'PPageSetFontFamilies'
pageSetFontFamilies :: Handle ev -> PPageSetFontFamilies -> IO (Maybe Error)
pageSetFontFamilies handle params = sendReceiveCommand handle "Page.setFontFamilies" (Just params)


-- | Parameters of the 'pageSetFontSizes' command.
data PPageSetFontSizes = PPageSetFontSizes {
  -- | Specifies font sizes to set. If a font size is not specified, it won't be changed.
  pPageSetFontSizesFontSizes :: PageFontSizes
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'Page.setFontSizes' command.
-- Set default font sizes.
-- Parameters: 'PPageSetFontSizes'
pageSetFontSizes :: Handle ev -> PPageSetFontSizes -> IO (Maybe Error)
pageSetFontSizes handle params = sendReceiveCommand handle "Page.setFontSizes" (Just params)


-- | Parameters of the 'pageSetDocumentContent' command.
data PPageSetDocumentContent = PPageSetDocumentContent {
  -- | Frame id to set HTML for.
  pPageSetDocumentContentFrameId :: PageFrameId,
  -- | HTML content to set.
  pPageSetDocumentContentHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetDocumentContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageSetDocumentContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Page.setDocumentContent' command.
-- Sets given markup as the document's HTML.
-- Parameters: 'PPageSetDocumentContent'
pageSetDocumentContent :: Handle ev -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent handle params = sendReceiveCommand handle "Page.setDocumentContent" (Just params)


-- | Parameters of the 'pageSetLifecycleEventsEnabled' command.
data PPageSetLifecycleEventsEnabled = PPageSetLifecycleEventsEnabled {
  -- | If true, starts emitting lifecycle events.
  pPageSetLifecycleEventsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetLifecycleEventsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageSetLifecycleEventsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Page.setLifecycleEventsEnabled' command.
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
  -- | Image compression format.
  pPageStartScreencastFormat :: PPageStartScreencastFormat,
  -- | Compression quality from range [0..100].
  pPageStartScreencastQuality :: Maybe Int,
  -- | Maximum screenshot width.
  pPageStartScreencastMaxWidth :: Maybe Int,
  -- | Maximum screenshot height.
  pPageStartScreencastMaxHeight :: Maybe Int,
  -- | Send every n-th frame.
  pPageStartScreencastEveryNthFrame :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageStartScreencast  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageStartScreencast where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Page.startScreencast' command.
-- Starts sending each frame using the `screencastFrame` event.
-- Parameters: 'PPageStartScreencast'
pageStartScreencast :: Handle ev -> PPageStartScreencast -> IO (Maybe Error)
pageStartScreencast handle params = sendReceiveCommand handle "Page.startScreencast" (Just params)


-- | Function for the 'Page.stopLoading' command.
-- Force the page stop all navigations and pending resource fetches.
pageStopLoading :: Handle ev -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())


-- | Function for the 'Page.crash' command.
-- Crashes renderer on the IO thread, generates minidumps.
pageCrash :: Handle ev -> IO (Maybe Error)
pageCrash handle = sendReceiveCommand handle "Page.crash" (Nothing :: Maybe ())


-- | Function for the 'Page.close' command.
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
  -- | Target lifecycle state
  pPageSetWebLifecycleStateState :: PPageSetWebLifecycleStateState
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetWebLifecycleState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetWebLifecycleState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Page.setWebLifecycleState' command.
-- Tries to update the web lifecycle state of the page.
-- It will transition the page to the given state according to:
-- https://github.com/WICG/web-lifecycle/
-- Parameters: 'PPageSetWebLifecycleState'
pageSetWebLifecycleState :: Handle ev -> PPageSetWebLifecycleState -> IO (Maybe Error)
pageSetWebLifecycleState handle params = sendReceiveCommand handle "Page.setWebLifecycleState" (Just params)


-- | Function for the 'Page.stopScreencast' command.
-- Stops sending each frame in the `screencastFrame`.
pageStopScreencast :: Handle ev -> IO (Maybe Error)
pageStopScreencast handle = sendReceiveCommand handle "Page.stopScreencast" (Nothing :: Maybe ())


-- | Parameters of the 'pageProduceCompilationCache' command.
data PPageProduceCompilationCache = PPageProduceCompilationCache {
  pPageProduceCompilationCacheScripts :: [PageCompilationCacheParams]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageProduceCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PPageProduceCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Page.produceCompilationCache' command.
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
  pPageAddCompilationCacheUrl :: String,
  -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
  pPageAddCompilationCacheData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageAddCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Page.addCompilationCache' command.
-- Seeds compilation cache for given url. Compilation cache does not survive
-- cross-process navigation.
-- Parameters: 'PPageAddCompilationCache'
pageAddCompilationCache :: Handle ev -> PPageAddCompilationCache -> IO (Maybe Error)
pageAddCompilationCache handle params = sendReceiveCommand handle "Page.addCompilationCache" (Just params)


-- | Function for the 'Page.clearCompilationCache' command.
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
  pPageSetSpcTransactionModeMode :: PPageSetSpcTransactionModeMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetSpcTransactionMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PPageSetSpcTransactionMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Page.setSPCTransactionMode' command.
-- Sets the Secure Payment Confirmation transaction mode.
-- https://w3c.github.io/secure-payment-confirmation/#sctn-automation-set-spc-transaction-mode
-- Parameters: 'PPageSetSpcTransactionMode'
pageSetSpcTransactionMode :: Handle ev -> PPageSetSpcTransactionMode -> IO (Maybe Error)
pageSetSpcTransactionMode handle params = sendReceiveCommand handle "Page.setSPCTransactionMode" (Just params)


-- | Parameters of the 'pageGenerateTestReport' command.
data PPageGenerateTestReport = PPageGenerateTestReport {
  -- | Message to be displayed in the report.
  pPageGenerateTestReportMessage :: String,
  -- | Specifies the endpoint group to deliver the report to.
  pPageGenerateTestReportGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGenerateTestReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGenerateTestReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Page.generateTestReport' command.
-- Generates a report for testing.
-- Parameters: 'PPageGenerateTestReport'
pageGenerateTestReport :: Handle ev -> PPageGenerateTestReport -> IO (Maybe Error)
pageGenerateTestReport handle params = sendReceiveCommand handle "Page.generateTestReport" (Just params)


-- | Function for the 'Page.waitForDebugger' command.
-- Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.
pageWaitForDebugger :: Handle ev -> IO (Maybe Error)
pageWaitForDebugger handle = sendReceiveCommand handle "Page.waitForDebugger" (Nothing :: Maybe ())


-- | Parameters of the 'pageSetInterceptFileChooserDialog' command.
data PPageSetInterceptFileChooserDialog = PPageSetInterceptFileChooserDialog {
  pPageSetInterceptFileChooserDialogEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetInterceptFileChooserDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PPageSetInterceptFileChooserDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Page.setInterceptFileChooserDialog' command.
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
  -- | Protocol name (e.g. "TLS 1.2" or "QUIC").
  securityCertificateSecurityStateProtocol :: String,
  -- | Key Exchange used by the connection, or the empty string if not applicable.
  securityCertificateSecurityStateKeyExchange :: String,
  -- | (EC)DH group used by the connection, if applicable.
  securityCertificateSecurityStateKeyExchangeGroup :: Maybe String,
  -- | Cipher name.
  securityCertificateSecurityStateCipher :: String,
  -- | TLS MAC. Note that AEAD ciphers do not have separate MACs.
  securityCertificateSecurityStateMac :: Maybe String,
  -- | Page certificate.
  securityCertificateSecurityStateCertificate :: [String],
  -- | Certificate subject name.
  securityCertificateSecurityStateSubjectName :: String,
  -- | Name of the issuing CA.
  securityCertificateSecurityStateIssuer :: String,
  -- | Certificate valid from date.
  securityCertificateSecurityStateValidFrom :: NetworkTimeSinceEpoch,
  -- | Certificate valid to (expiration) date
  securityCertificateSecurityStateValidTo :: NetworkTimeSinceEpoch,
  -- | The highest priority network error code, if the certificate has an error.
  securityCertificateSecurityStateCertificateNetworkError :: Maybe String,
  -- | True if the certificate uses a weak signature aglorithm.
  securityCertificateSecurityStateCertificateHasWeakSignature :: Bool,
  -- | True if the certificate has a SHA1 signature in the chain.
  securityCertificateSecurityStateCertificateHasSha1Signature :: Bool,
  -- | True if modern SSL
  securityCertificateSecurityStateModernSsl :: Bool,
  -- | True if the connection is using an obsolete SSL protocol.
  securityCertificateSecurityStateObsoleteSslProtocol :: Bool,
  -- | True if the connection is using an obsolete SSL key exchange.
  securityCertificateSecurityStateObsoleteSslKeyExchange :: Bool,
  -- | True if the connection is using an obsolete SSL cipher.
  securityCertificateSecurityStateObsoleteSslCipher :: Bool,
  -- | True if the connection is using an obsolete SSL signature.
  securityCertificateSecurityStateObsoleteSslSignature :: Bool
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
  -- | Describes whether the page triggers any safety tips or reputation warnings. Default is unknown.
  securitySafetyTipInfoSafetyTipStatus :: SecuritySafetyTipStatus,
  -- | The URL the safety tip suggested ("Did you mean?"). Only filled in for lookalike matches.
  securitySafetyTipInfoSafeUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySafetyTipInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  SecuritySafetyTipInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Security state information about the page.
data SecurityVisibleSecurityState = SecurityVisibleSecurityState {
  -- | The security level of the page.
  securityVisibleSecurityStateSecurityState :: SecuritySecurityState,
  -- | Security state details about the page certificate.
  securityVisibleSecurityStateCertificateSecurityState :: Maybe SecurityCertificateSecurityState,
  -- | The type of Safety Tip triggered on the page. Note that this field will be set even if the Safety Tip UI was not actually shown.
  securityVisibleSecurityStateSafetyTipInfo :: Maybe SecuritySafetyTipInfo,
  -- | Array of security state issues ids.
  securityVisibleSecurityStateSecurityStateIssueIds :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | An explanation of an factor contributing to the security state.
data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
  -- | Security state representing the severity of the factor being explained.
  securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
  -- | Title describing the type of factor.
  securitySecurityStateExplanationTitle :: String,
  -- | Short phrase describing the type of factor.
  securitySecurityStateExplanationSummary :: String,
  -- | Full text explanation of the factor.
  securitySecurityStateExplanationDescription :: String,
  -- | The type of mixed content described by the explanation.
  securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
  -- | Page certificate.
  securitySecurityStateExplanationCertificate :: [String],
  -- | Recommendations to fix any issues.
  securitySecurityStateExplanationRecommendations :: Maybe [String]
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
  -- | Security state information about the page.
  securityVisibleSecurityStateChangedVisibleSecurityState :: SecurityVisibleSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityStateChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityStateChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }





-- | Function for the 'Security.disable' command.
-- Disables tracking security state changes.
securityDisable :: Handle ev -> IO (Maybe Error)
securityDisable handle = sendReceiveCommand handle "Security.disable" (Nothing :: Maybe ())


-- | Function for the 'Security.enable' command.
-- Enables tracking security state changes.
securityEnable :: Handle ev -> IO (Maybe Error)
securityEnable handle = sendReceiveCommand handle "Security.enable" (Nothing :: Maybe ())


-- | Parameters of the 'securitySetIgnoreCertificateErrors' command.
data PSecuritySetIgnoreCertificateErrors = PSecuritySetIgnoreCertificateErrors {
  -- | If true, all certificate errors will be ignored.
  pSecuritySetIgnoreCertificateErrorsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PSecuritySetIgnoreCertificateErrors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PSecuritySetIgnoreCertificateErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'Security.setIgnoreCertificateErrors' command.
-- Enable/disable whether all certificate errors should be ignored.
-- Parameters: 'PSecuritySetIgnoreCertificateErrors'
securitySetIgnoreCertificateErrors :: Handle ev -> PSecuritySetIgnoreCertificateErrors -> IO (Maybe Error)
securitySetIgnoreCertificateErrors handle params = sendReceiveCommand handle "Security.setIgnoreCertificateErrors" (Just params)



