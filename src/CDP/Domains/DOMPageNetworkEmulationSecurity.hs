{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


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
data DOMBackendNode = DOMBackendNode {
  -- | `Node`'s nodeType.
  dOMBackendNodeNodeType :: Int,
  -- | `Node`'s nodeName.
  dOMBackendNodeNodeName :: String,
  dOMBackendNodeBackendNodeId :: DOMBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMBackendNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DOMBackendNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'DOM.PseudoType'.
--   Pseudo element type.
data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
   deriving (Ord, Eq, Show, Read)
instance FromJSON DOMPseudoType where
   parseJSON = A.withText  "DOMPseudoType"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse DOMPseudoType"

instance ToJSON DOMPseudoType where
   toJSON v = A.String $
      case v of
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
   parseJSON = A.withText  "DOMShadowRootType"  $ \v -> do
      case v of
         "user-agent" -> pure DOMShadowRootTypeUserAgent
         "open" -> pure DOMShadowRootTypeOpen
         "closed" -> pure DOMShadowRootTypeClosed
         _ -> fail "failed to parse DOMShadowRootType"

instance ToJSON DOMShadowRootType where
   toJSON v = A.String $
      case v of
         DOMShadowRootTypeUserAgent -> "user-agent"
         DOMShadowRootTypeOpen -> "open"
         DOMShadowRootTypeClosed -> "closed"



-- | Type 'DOM.CompatibilityMode'.
--   Document compatibility mode.
data DOMCompatibilityMode = DOMCompatibilityModeQuirksMode | DOMCompatibilityModeLimitedQuirksMode | DOMCompatibilityModeNoQuirksMode
   deriving (Ord, Eq, Show, Read)
instance FromJSON DOMCompatibilityMode where
   parseJSON = A.withText  "DOMCompatibilityMode"  $ \v -> do
      case v of
         "QuirksMode" -> pure DOMCompatibilityModeQuirksMode
         "LimitedQuirksMode" -> pure DOMCompatibilityModeLimitedQuirksMode
         "NoQuirksMode" -> pure DOMCompatibilityModeNoQuirksMode
         _ -> fail "failed to parse DOMCompatibilityMode"

instance ToJSON DOMCompatibilityMode where
   toJSON v = A.String $
      case v of
         DOMCompatibilityModeQuirksMode -> "QuirksMode"
         DOMCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
         DOMCompatibilityModeNoQuirksMode -> "NoQuirksMode"



-- | Type 'DOM.Node'.
--   DOM interaction is implemented in terms of mirror objects that represent the actual DOM nodes.
--   DOMNode is a base node mirror type.
data DOMNode = DOMNode {
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
  dOMNodeNodeName :: String,
  -- | `Node`'s localName.
  dOMNodeLocalName :: String,
  -- | `Node`'s nodeValue.
  dOMNodeNodeValue :: String,
  -- | Child count for `Container` nodes.
  dOMNodeChildNodeCount :: Maybe Int,
  -- | Child nodes of this node when requested with children.
  dOMNodeChildren :: Maybe [DOMNode],
  -- | Attributes of the `Element` node in the form of flat array `[name1, value1, name2, value2]`.
  dOMNodeAttributes :: Maybe [String],
  -- | Document URL that `Document` or `FrameOwner` node points to.
  dOMNodeDocumentURL :: Maybe String,
  -- | Base URL that `Document` or `FrameOwner` node uses for URL completion.
  dOMNodeBaseURL :: Maybe String,
  -- | `DocumentType`'s publicId.
  dOMNodePublicId :: Maybe String,
  -- | `DocumentType`'s systemId.
  dOMNodeSystemId :: Maybe String,
  -- | `DocumentType`'s internalSubset.
  dOMNodeInternalSubset :: Maybe String,
  -- | `Document`'s XML version in case of XML documents.
  dOMNodeXmlVersion :: Maybe String,
  -- | `Attr`'s name.
  dOMNodeName :: Maybe String,
  -- | `Attr`'s value.
  dOMNodeValue :: Maybe String,
  -- | Pseudo element type for this node.
  dOMNodePseudoType :: Maybe DOMPseudoType,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DOMNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | Type 'DOM.RGBA'.
--   A structure holding an RGBA color.
data DOMRGBA = DOMRGBA {
  -- | The red component, in the [0-255] range.
  dOMRGBAR :: Int,
  -- | The green component, in the [0-255] range.
  dOMRGBAG :: Int,
  -- | The blue component, in the [0-255] range.
  dOMRGBAB :: Int,
  -- | The alpha component, in the [0-1] range (default: 1).
  dOMRGBAA :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMRGBA  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DOMRGBA where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | Type 'DOM.Quad'.
--   An array of quad vertices, x immediately followed by y for each point, points clock-wise.
type DOMQuad = [Double]

-- | Type 'DOM.BoxModel'.
--   Box model.
data DOMBoxModel = DOMBoxModel {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  DOMBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'DOM.ShapeOutsideInfo'.
--   CSS Shape Outside details.
data DOMShapeOutsideInfo = DOMShapeOutsideInfo {
  -- | Shape bounds
  dOMShapeOutsideInfoBounds :: DOMQuad,
  -- | Shape coordinate details
  dOMShapeOutsideInfoShape :: [Int],
  -- | Margin shape bounds
  dOMShapeOutsideInfoMarginShape :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMShapeOutsideInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMShapeOutsideInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'DOM.Rect'.
--   Rectangle.
data DOMRect = DOMRect {
  -- | X coordinate
  dOMRectX :: Double,
  -- | Y coordinate
  dOMRectY :: Double,
  -- | Rectangle width
  dOMRectWidth :: Double,
  -- | Rectangle height
  dOMRectHeight :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DOMRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



-- | Type 'DOM.CSSComputedStyleProperty'.
data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty {
  -- | Computed style property name.
  dOMCSSComputedStylePropertyName :: String,
  -- | Computed style property value.
  dOMCSSComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMCSSComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DOMCSSComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Type of the 'DOM.attributeModified' event.
data DOMAttributeModified = DOMAttributeModified {
  -- | Id of the node that has changed.
  dOMAttributeModifiedNodeId :: DOMNodeId,
  -- | Attribute name.
  dOMAttributeModifiedName :: String,
  -- | Attribute value.
  dOMAttributeModifiedValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMAttributeModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DOMAttributeModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Event DOMAttributeModified where
    eventName _ = "DOM.attributeModified"

-- | Type of the 'DOM.attributeRemoved' event.
data DOMAttributeRemoved = DOMAttributeRemoved {
  -- | Id of the node that has changed.
  dOMAttributeRemovedNodeId :: DOMNodeId,
  -- | A ttribute name.
  dOMAttributeRemovedName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMAttributeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMAttributeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event DOMAttributeRemoved where
    eventName _ = "DOM.attributeRemoved"

-- | Type of the 'DOM.characterDataModified' event.
data DOMCharacterDataModified = DOMCharacterDataModified {
  -- | Id of the node that has changed.
  dOMCharacterDataModifiedNodeId :: DOMNodeId,
  -- | New text value.
  dOMCharacterDataModifiedCharacterData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMCharacterDataModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DOMCharacterDataModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event DOMCharacterDataModified where
    eventName _ = "DOM.characterDataModified"

-- | Type of the 'DOM.childNodeCountUpdated' event.
data DOMChildNodeCountUpdated = DOMChildNodeCountUpdated {
  -- | Id of the node that has changed.
  dOMChildNodeCountUpdatedNodeId :: DOMNodeId,
  -- | New node count.
  dOMChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMChildNodeCountUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DOMChildNodeCountUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event DOMChildNodeCountUpdated where
    eventName _ = "DOM.childNodeCountUpdated"

-- | Type of the 'DOM.childNodeInserted' event.
data DOMChildNodeInserted = DOMChildNodeInserted {
  -- | Id of the node that has changed.
  dOMChildNodeInsertedParentNodeId :: DOMNodeId,
  -- | If of the previous siblint.
  dOMChildNodeInsertedPreviousNodeId :: DOMNodeId,
  -- | Inserted node data.
  dOMChildNodeInsertedNode :: DOMNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMChildNodeInserted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DOMChildNodeInserted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Event DOMChildNodeInserted where
    eventName _ = "DOM.childNodeInserted"

-- | Type of the 'DOM.childNodeRemoved' event.
data DOMChildNodeRemoved = DOMChildNodeRemoved {
  -- | Parent id.
  dOMChildNodeRemovedParentNodeId :: DOMNodeId,
  -- | Id of the node that has been removed.
  dOMChildNodeRemovedNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMChildNodeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMChildNodeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event DOMChildNodeRemoved where
    eventName _ = "DOM.childNodeRemoved"

-- | Type of the 'DOM.distributedNodesUpdated' event.
data DOMDistributedNodesUpdated = DOMDistributedNodesUpdated {
  -- | Insertion point where distributed nodes were updated.
  dOMDistributedNodesUpdatedInsertionPointId :: DOMNodeId,
  -- | Distributed nodes for given insertion point.
  dOMDistributedNodesUpdatedDistributedNodes :: [DOMBackendNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMDistributedNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DOMDistributedNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Event DOMDistributedNodesUpdated where
    eventName _ = "DOM.distributedNodesUpdated"

-- | Type of the 'DOM.documentUpdated' event.
data DOMDocumentUpdated = DOMDocumentUpdated
   deriving (Eq, Show, Read)
instance FromJSON DOMDocumentUpdated where
   parseJSON = A.withText  "DOMDocumentUpdated"  $ \v -> do
      case v of
         "DOMDocumentUpdated" -> pure DOMDocumentUpdated
         _ -> fail "failed to parse DOMDocumentUpdated"


instance Event DOMDocumentUpdated where
    eventName _ = "DOM.documentUpdated"

-- | Type of the 'DOM.inlineStyleInvalidated' event.
data DOMInlineStyleInvalidated = DOMInlineStyleInvalidated {
  -- | Ids of the nodes for which the inline styles have been invalidated.
  dOMInlineStyleInvalidatedNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMInlineStyleInvalidated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DOMInlineStyleInvalidated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event DOMInlineStyleInvalidated where
    eventName _ = "DOM.inlineStyleInvalidated"

-- | Type of the 'DOM.pseudoElementAdded' event.
data DOMPseudoElementAdded = DOMPseudoElementAdded {
  -- | Pseudo element's parent element id.
  dOMPseudoElementAddedParentId :: DOMNodeId,
  -- | The added pseudo element.
  dOMPseudoElementAddedPseudoElement :: DOMNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMPseudoElementAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DOMPseudoElementAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Event DOMPseudoElementAdded where
    eventName _ = "DOM.pseudoElementAdded"

-- | Type of the 'DOM.pseudoElementRemoved' event.
data DOMPseudoElementRemoved = DOMPseudoElementRemoved {
  -- | Pseudo element's parent element id.
  dOMPseudoElementRemovedParentId :: DOMNodeId,
  -- | The removed pseudo element id.
  dOMPseudoElementRemovedPseudoElementId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMPseudoElementRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  DOMPseudoElementRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event DOMPseudoElementRemoved where
    eventName _ = "DOM.pseudoElementRemoved"

-- | Type of the 'DOM.setChildNodes' event.
data DOMSetChildNodes = DOMSetChildNodes {
  -- | Parent node id to populate with children.
  dOMSetChildNodesParentId :: DOMNodeId,
  -- | Child nodes array.
  dOMSetChildNodesNodes :: [DOMNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DOMSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Event DOMSetChildNodes where
    eventName _ = "DOM.setChildNodes"

-- | Type of the 'DOM.shadowRootPopped' event.
data DOMShadowRootPopped = DOMShadowRootPopped {
  -- | Host element id.
  dOMShadowRootPoppedHostId :: DOMNodeId,
  -- | Shadow root id.
  dOMShadowRootPoppedRootId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMShadowRootPopped  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMShadowRootPopped where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event DOMShadowRootPopped where
    eventName _ = "DOM.shadowRootPopped"

-- | Type of the 'DOM.shadowRootPushed' event.
data DOMShadowRootPushed = DOMShadowRootPushed {
  -- | Host element id.
  dOMShadowRootPushedHostId :: DOMNodeId,
  -- | Shadow root.
  dOMShadowRootPushedRoot :: DOMNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DOMShadowRootPushed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DOMShadowRootPushed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event DOMShadowRootPushed where
    eventName _ = "DOM.shadowRootPushed"



-- | DOM.collectClassNamesFromSubtree
--   Collects class names for the node with given id and all of it's child nodes.

-- | Parameters of the 'DOM.collectClassNamesFromSubtree' command.
data PDOMCollectClassNamesFromSubtree = PDOMCollectClassNamesFromSubtree {
  -- | Id of the node to collect class names.
  pDOMCollectClassNamesFromSubtreeNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMCollectClassNamesFromSubtree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PDOMCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Return type of the 'DOM.collectClassNamesFromSubtree' command.
data DOMCollectClassNamesFromSubtree = DOMCollectClassNamesFromSubtree {
  -- | Class name list.
  dOMCollectClassNamesFromSubtreeClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PDOMCollectClassNamesFromSubtree where
   type CommandResponse PDOMCollectClassNamesFromSubtree = DOMCollectClassNamesFromSubtree
   commandName _ = "DOM.collectClassNamesFromSubtree"



-- | DOM.copyTo
--   Creates a deep copy of the specified node and places it into the target container before the
--   given anchor.

-- | Parameters of the 'DOM.copyTo' command.
data PDOMCopyTo = PDOMCopyTo {
  -- | Id of the node to copy.
  pDOMCopyToNodeId :: DOMNodeId,
  -- | Id of the element to drop the copy into.
  pDOMCopyToTargetNodeId :: DOMNodeId,
  -- | Drop the copy before this node (if absent, the copy becomes the last child of
  --   `targetNodeId`).
  pDOMCopyToInsertBeforeNodeId :: Maybe DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMCopyTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDOMCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Return type of the 'DOM.copyTo' command.
data DOMCopyTo = DOMCopyTo {
  -- | Id of the node clone.
  dOMCopyToNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command PDOMCopyTo where
   type CommandResponse PDOMCopyTo = DOMCopyTo
   commandName _ = "DOM.copyTo"



-- | DOM.describeNode
--   Describes node given its id, does not require domain to be enabled. Does not start tracking any
--   objects, can be used for automation.

-- | Parameters of the 'DOM.describeNode' command.
data PDOMDescribeNode = PDOMDescribeNode {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDescribeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Return type of the 'DOM.describeNode' command.
data DOMDescribeNode = DOMDescribeNode {
  -- | Node description.
  dOMDescribeNodeNode :: DOMNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command PDOMDescribeNode where
   type CommandResponse PDOMDescribeNode = DOMDescribeNode
   commandName _ = "DOM.describeNode"



-- | DOM.scrollIntoViewIfNeeded
--   Scrolls the specified rect of the given node into view if not already visible.
--   Note: exactly one between nodeId, backendNodeId and objectId should be passed
--   to identify the node.

-- | Parameters of the 'DOM.scrollIntoViewIfNeeded' command.
data PDOMScrollIntoViewIfNeeded = PDOMScrollIntoViewIfNeeded {
  -- | Identifier of the node.
  pDOMScrollIntoViewIfNeededNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMScrollIntoViewIfNeededBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMScrollIntoViewIfNeededObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | The rect to be scrolled into view, relative to the node's border box, in CSS pixels.
  --   When omitted, center of the node will be used, similar to Element.scrollIntoView.
  pDOMScrollIntoViewIfNeededRect :: Maybe DOMRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMScrollIntoViewIfNeeded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PDOMScrollIntoViewIfNeeded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PDOMScrollIntoViewIfNeeded where
   type CommandResponse PDOMScrollIntoViewIfNeeded = ()
   commandName _ = "DOM.scrollIntoViewIfNeeded"
   fromJSON = const . A.Success . const ()


-- | DOM.disable
--   Disables DOM agent for the given page.

-- | Parameters of the 'DOM.disable' command.
data PDOMDisable = PDOMDisable
instance ToJSON PDOMDisable where toJSON _ = A.Null

instance Command PDOMDisable where
   type CommandResponse PDOMDisable = ()
   commandName _ = "DOM.disable"
   fromJSON = const . A.Success . const ()


-- | DOM.discardSearchResults
--   Discards search results from the session with the given id. `getSearchResults` should no longer
--   be called for that search.

-- | Parameters of the 'DOM.discardSearchResults' command.
data PDOMDiscardSearchResults = PDOMDiscardSearchResults {
  -- | Unique search session identifier.
  pDOMDiscardSearchResultsSearchId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMDiscardSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDOMDiscardSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PDOMDiscardSearchResults where
   type CommandResponse PDOMDiscardSearchResults = ()
   commandName _ = "DOM.discardSearchResults"
   fromJSON = const . A.Success . const ()


-- | DOM.enable
--   Enables DOM agent for the given page.

-- | Parameters of the 'DOM.enable' command.
data PDOMEnableIncludeWhitespace = PDOMEnableIncludeWhitespaceNone | PDOMEnableIncludeWhitespaceAll
   deriving (Ord, Eq, Show, Read)
instance FromJSON PDOMEnableIncludeWhitespace where
   parseJSON = A.withText  "PDOMEnableIncludeWhitespace"  $ \v -> do
      case v of
         "none" -> pure PDOMEnableIncludeWhitespaceNone
         "all" -> pure PDOMEnableIncludeWhitespaceAll
         _ -> fail "failed to parse PDOMEnableIncludeWhitespace"

instance ToJSON PDOMEnableIncludeWhitespace where
   toJSON v = A.String $
      case v of
         PDOMEnableIncludeWhitespaceNone -> "none"
         PDOMEnableIncludeWhitespaceAll -> "all"



data PDOMEnable = PDOMEnable {
  -- | Whether to include whitespaces in the children array of returned Nodes.
  pDOMEnableIncludeWhitespace :: PDOMEnableIncludeWhitespace
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDOMEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


instance Command PDOMEnable where
   type CommandResponse PDOMEnable = ()
   commandName _ = "DOM.enable"
   fromJSON = const . A.Success . const ()


-- | DOM.focus
--   Focuses the given element.

-- | Parameters of the 'DOM.focus' command.
data PDOMFocus = PDOMFocus {
  -- | Identifier of the node.
  pDOMFocusNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMFocusBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMFocusObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMFocus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PDOMFocus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


instance Command PDOMFocus where
   type CommandResponse PDOMFocus = ()
   commandName _ = "DOM.focus"
   fromJSON = const . A.Success . const ()


-- | DOM.getAttributes
--   Returns attributes for the specified node.

-- | Parameters of the 'DOM.getAttributes' command.
data PDOMGetAttributes = PDOMGetAttributes {
  -- | Id of the node to retrieve attibutes for.
  pDOMGetAttributesNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetAttributes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDOMGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Return type of the 'DOM.getAttributes' command.
data DOMGetAttributes = DOMGetAttributes {
  -- | An interleaved array of node attribute names and values.
  dOMGetAttributesAttributes :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PDOMGetAttributes where
   type CommandResponse PDOMGetAttributes = DOMGetAttributes
   commandName _ = "DOM.getAttributes"



-- | DOM.getBoxModel
--   Returns boxes for the given node.

-- | Parameters of the 'DOM.getBoxModel' command.
data PDOMGetBoxModel = PDOMGetBoxModel {
  -- | Identifier of the node.
  pDOMGetBoxModelNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMGetBoxModelBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMGetBoxModelObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.getBoxModel' command.
data DOMGetBoxModel = DOMGetBoxModel {
  -- | Box model for the node.
  dOMGetBoxModelModel :: DOMBoxModel
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMGetBoxModel where
   type CommandResponse PDOMGetBoxModel = DOMGetBoxModel
   commandName _ = "DOM.getBoxModel"



-- | DOM.getContentQuads
--   Returns quads that describe node position on the page. This method
--   might return multiple quads for inline nodes.

-- | Parameters of the 'DOM.getContentQuads' command.
data PDOMGetContentQuads = PDOMGetContentQuads {
  -- | Identifier of the node.
  pDOMGetContentQuadsNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMGetContentQuadsBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMGetContentQuadsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetContentQuads  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDOMGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Return type of the 'DOM.getContentQuads' command.
data DOMGetContentQuads = DOMGetContentQuads {
  -- | Quads that describe node layout relative to viewport.
  dOMGetContentQuadsQuads :: [DOMQuad]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PDOMGetContentQuads where
   type CommandResponse PDOMGetContentQuads = DOMGetContentQuads
   commandName _ = "DOM.getContentQuads"



-- | DOM.getDocument
--   Returns the root DOM node (and optionally the subtree) to the caller.

-- | Parameters of the 'DOM.getDocument' command.
data PDOMGetDocument = PDOMGetDocument {
  -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
  --   entire subtree or provide an integer larger than 0.
  pDOMGetDocumentDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the subtree
  --   (default is false).
  pDOMGetDocumentPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.getDocument' command.
data DOMGetDocument = DOMGetDocument {
  -- | Resulting node.
  dOMGetDocumentRoot :: DOMNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMGetDocument where
   type CommandResponse PDOMGetDocument = DOMGetDocument
   commandName _ = "DOM.getDocument"



-- | DOM.getNodesForSubtreeByStyle
--   Finds nodes with a given computed style in a subtree.

-- | Parameters of the 'DOM.getNodesForSubtreeByStyle' command.
data PDOMGetNodesForSubtreeByStyle = PDOMGetNodesForSubtreeByStyle {
  -- | Node ID pointing to the root of a subtree.
  pDOMGetNodesForSubtreeByStyleNodeId :: DOMNodeId,
  -- | The style to filter nodes by (includes nodes if any of properties matches).
  pDOMGetNodesForSubtreeByStyleComputedStyles :: [DOMCSSComputedStyleProperty],
  -- | Whether or not iframes and shadow roots in the same target should be traversed when returning the
  --   results (default is false).
  pDOMGetNodesForSubtreeByStylePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetNodesForSubtreeByStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDOMGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Return type of the 'DOM.getNodesForSubtreeByStyle' command.
data DOMGetNodesForSubtreeByStyle = DOMGetNodesForSubtreeByStyle {
  -- | Resulting nodes.
  dOMGetNodesForSubtreeByStyleNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command PDOMGetNodesForSubtreeByStyle where
   type CommandResponse PDOMGetNodesForSubtreeByStyle = DOMGetNodesForSubtreeByStyle
   commandName _ = "DOM.getNodesForSubtreeByStyle"



-- | DOM.getNodeForLocation
--   Returns node id at given location. Depending on whether DOM domain is enabled, nodeId is
--   either returned or not.

-- | Parameters of the 'DOM.getNodeForLocation' command.
data PDOMGetNodeForLocation = PDOMGetNodeForLocation {
  -- | X coordinate.
  pDOMGetNodeForLocationX :: Int,
  -- | Y coordinate.
  pDOMGetNodeForLocationY :: Int,
  -- | False to skip to the nearest non-UA shadow root ancestor (default: false).
  pDOMGetNodeForLocationIncludeUserAgentShadowDOM :: Maybe Bool,
  -- | Whether to ignore pointer-events: none on elements and hit test them.
  pDOMGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetNodeForLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDOMGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'DOM.getNodeForLocation' command.
data DOMGetNodeForLocation = DOMGetNodeForLocation {
  -- | Resulting node.
  dOMGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
  -- | Frame this node belongs to.
  dOMGetNodeForLocationFrameId :: PageFrameId,
  -- | Id of the node at given coordinates, only when enabled and requested document.
  dOMGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PDOMGetNodeForLocation where
   type CommandResponse PDOMGetNodeForLocation = DOMGetNodeForLocation
   commandName _ = "DOM.getNodeForLocation"



-- | DOM.getOuterHTML
--   Returns node's HTML markup.

-- | Parameters of the 'DOM.getOuterHTML' command.
data PDOMGetOuterHTML = PDOMGetOuterHTML {
  -- | Identifier of the node.
  pDOMGetOuterHTMLNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMGetOuterHTMLBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMGetOuterHTMLObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetOuterHTML  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMGetOuterHTML where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Return type of the 'DOM.getOuterHTML' command.
data DOMGetOuterHTML = DOMGetOuterHTML {
  -- | Outer HTML markup.
  dOMGetOuterHTMLOuterHTML :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetOuterHTML where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command PDOMGetOuterHTML where
   type CommandResponse PDOMGetOuterHTML = DOMGetOuterHTML
   commandName _ = "DOM.getOuterHTML"



-- | DOM.getRelayoutBoundary
--   Returns the id of the nearest ancestor that is a relayout boundary.

-- | Parameters of the 'DOM.getRelayoutBoundary' command.
data PDOMGetRelayoutBoundary = PDOMGetRelayoutBoundary {
  -- | Id of the node.
  pDOMGetRelayoutBoundaryNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetRelayoutBoundary  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDOMGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Return type of the 'DOM.getRelayoutBoundary' command.
data DOMGetRelayoutBoundary = DOMGetRelayoutBoundary {
  -- | Relayout boundary node id for the given node.
  dOMGetRelayoutBoundaryNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PDOMGetRelayoutBoundary where
   type CommandResponse PDOMGetRelayoutBoundary = DOMGetRelayoutBoundary
   commandName _ = "DOM.getRelayoutBoundary"



-- | DOM.getSearchResults
--   Returns search results from given `fromIndex` to given `toIndex` from the search with the given
--   identifier.

-- | Parameters of the 'DOM.getSearchResults' command.
data PDOMGetSearchResults = PDOMGetSearchResults {
  -- | Unique search session identifier.
  pDOMGetSearchResultsSearchId :: String,
  -- | Start index of the search result to be returned.
  pDOMGetSearchResultsFromIndex :: Int,
  -- | End index of the search result to be returned.
  pDOMGetSearchResultsToIndex :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDOMGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Return type of the 'DOM.getSearchResults' command.
data DOMGetSearchResults = DOMGetSearchResults {
  -- | Ids of the search result nodes.
  dOMGetSearchResultsNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PDOMGetSearchResults where
   type CommandResponse PDOMGetSearchResults = DOMGetSearchResults
   commandName _ = "DOM.getSearchResults"



-- | DOM.hideHighlight
--   Hides any highlight.

-- | Parameters of the 'DOM.hideHighlight' command.
data PDOMHideHighlight = PDOMHideHighlight
instance ToJSON PDOMHideHighlight where toJSON _ = A.Null

instance Command PDOMHideHighlight where
   type CommandResponse PDOMHideHighlight = ()
   commandName _ = "DOM.hideHighlight"
   fromJSON = const . A.Success . const ()


-- | DOM.highlightNode
--   Highlights DOM node.

-- | Parameters of the 'DOM.highlightNode' command.
data PDOMHighlightNode = PDOMHighlightNode
instance ToJSON PDOMHighlightNode where toJSON _ = A.Null

instance Command PDOMHighlightNode where
   type CommandResponse PDOMHighlightNode = ()
   commandName _ = "DOM.highlightNode"
   fromJSON = const . A.Success . const ()


-- | DOM.highlightRect
--   Highlights given rectangle.

-- | Parameters of the 'DOM.highlightRect' command.
data PDOMHighlightRect = PDOMHighlightRect
instance ToJSON PDOMHighlightRect where toJSON _ = A.Null

instance Command PDOMHighlightRect where
   type CommandResponse PDOMHighlightRect = ()
   commandName _ = "DOM.highlightRect"
   fromJSON = const . A.Success . const ()


-- | DOM.markUndoableState
--   Marks last undoable state.

-- | Parameters of the 'DOM.markUndoableState' command.
data PDOMMarkUndoableState = PDOMMarkUndoableState
instance ToJSON PDOMMarkUndoableState where toJSON _ = A.Null

instance Command PDOMMarkUndoableState where
   type CommandResponse PDOMMarkUndoableState = ()
   commandName _ = "DOM.markUndoableState"
   fromJSON = const . A.Success . const ()


-- | DOM.moveTo
--   Moves node into the new container, places it before the given anchor.

-- | Parameters of the 'DOM.moveTo' command.
data PDOMMoveTo = PDOMMoveTo {
  -- | Id of the node to move.
  pDOMMoveToNodeId :: DOMNodeId,
  -- | Id of the element to drop the moved node into.
  pDOMMoveToTargetNodeId :: DOMNodeId,
  -- | Drop node before this one (if absent, the moved node becomes the last child of
  --   `targetNodeId`).
  pDOMMoveToInsertBeforeNodeId :: Maybe DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMMoveTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDOMMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


-- | Return type of the 'DOM.moveTo' command.
data DOMMoveTo = DOMMoveTo {
  -- | New id of the moved node.
  dOMMoveToNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command PDOMMoveTo where
   type CommandResponse PDOMMoveTo = DOMMoveTo
   commandName _ = "DOM.moveTo"



-- | DOM.performSearch
--   Searches for a given string in the DOM tree. Use `getSearchResults` to access search results or
--   `cancelSearch` to end this search session.

-- | Parameters of the 'DOM.performSearch' command.
data PDOMPerformSearch = PDOMPerformSearch {
  -- | Plain text or query selector or XPath search query.
  pDOMPerformSearchQuery :: String,
  -- | True to search in user agent shadow DOM.
  pDOMPerformSearchIncludeUserAgentShadowDOM :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMPerformSearch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDOMPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Return type of the 'DOM.performSearch' command.
data DOMPerformSearch = DOMPerformSearch {
  -- | Unique search session identifier.
  dOMPerformSearchSearchId :: String,
  -- | Number of search results.
  dOMPerformSearchResultCount :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PDOMPerformSearch where
   type CommandResponse PDOMPerformSearch = DOMPerformSearch
   commandName _ = "DOM.performSearch"



-- | DOM.pushNodeByPathToFrontend
--   Requests that the node is sent to the caller given its path. // FIXME, use XPath

-- | Parameters of the 'DOM.pushNodeByPathToFrontend' command.
data PDOMPushNodeByPathToFrontend = PDOMPushNodeByPathToFrontend {
  -- | Path to node in the proprietary format.
  pDOMPushNodeByPathToFrontendPath :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMPushNodeByPathToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDOMPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Return type of the 'DOM.pushNodeByPathToFrontend' command.
data DOMPushNodeByPathToFrontend = DOMPushNodeByPathToFrontend {
  -- | Id of the node for given path.
  dOMPushNodeByPathToFrontendNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PDOMPushNodeByPathToFrontend where
   type CommandResponse PDOMPushNodeByPathToFrontend = DOMPushNodeByPathToFrontend
   commandName _ = "DOM.pushNodeByPathToFrontend"



-- | DOM.pushNodesByBackendIdsToFrontend
--   Requests that a batch of nodes is sent to the caller given their backend node ids.

-- | Parameters of the 'DOM.pushNodesByBackendIdsToFrontend' command.
data PDOMPushNodesByBackendIdsToFrontend = PDOMPushNodesByBackendIdsToFrontend {
  -- | The array of backend node ids.
  pDOMPushNodesByBackendIdsToFrontendBackendNodeIds :: [DOMBackendNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMPushNodesByBackendIdsToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PDOMPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Return type of the 'DOM.pushNodesByBackendIdsToFrontend' command.
data DOMPushNodesByBackendIdsToFrontend = DOMPushNodesByBackendIdsToFrontend {
  -- | The array of ids of pushed nodes that correspond to the backend ids specified in
  --   backendNodeIds.
  dOMPushNodesByBackendIdsToFrontendNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command PDOMPushNodesByBackendIdsToFrontend where
   type CommandResponse PDOMPushNodesByBackendIdsToFrontend = DOMPushNodesByBackendIdsToFrontend
   commandName _ = "DOM.pushNodesByBackendIdsToFrontend"



-- | DOM.querySelector
--   Executes `querySelector` on a given node.

-- | Parameters of the 'DOM.querySelector' command.
data PDOMQuerySelector = PDOMQuerySelector {
  -- | Id of the node to query upon.
  pDOMQuerySelectorNodeId :: DOMNodeId,
  -- | Selector string.
  pDOMQuerySelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMQuerySelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDOMQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Return type of the 'DOM.querySelector' command.
data DOMQuerySelector = DOMQuerySelector {
  -- | Query selector result.
  dOMQuerySelectorNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PDOMQuerySelector where
   type CommandResponse PDOMQuerySelector = DOMQuerySelector
   commandName _ = "DOM.querySelector"



-- | DOM.querySelectorAll
--   Executes `querySelectorAll` on a given node.

-- | Parameters of the 'DOM.querySelectorAll' command.
data PDOMQuerySelectorAll = PDOMQuerySelectorAll {
  -- | Id of the node to query upon.
  pDOMQuerySelectorAllNodeId :: DOMNodeId,
  -- | Selector string.
  pDOMQuerySelectorAllSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMQuerySelectorAll  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDOMQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Return type of the 'DOM.querySelectorAll' command.
data DOMQuerySelectorAll = DOMQuerySelectorAll {
  -- | Query selector result.
  dOMQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PDOMQuerySelectorAll where
   type CommandResponse PDOMQuerySelectorAll = DOMQuerySelectorAll
   commandName _ = "DOM.querySelectorAll"



-- | DOM.redo
--   Re-does the last undone action.

-- | Parameters of the 'DOM.redo' command.
data PDOMRedo = PDOMRedo
instance ToJSON PDOMRedo where toJSON _ = A.Null

instance Command PDOMRedo where
   type CommandResponse PDOMRedo = ()
   commandName _ = "DOM.redo"
   fromJSON = const . A.Success . const ()


-- | DOM.removeAttribute
--   Removes attribute with given name from an element with given id.

-- | Parameters of the 'DOM.removeAttribute' command.
data PDOMRemoveAttribute = PDOMRemoveAttribute {
  -- | Id of the element to remove attribute from.
  pDOMRemoveAttributeNodeId :: DOMNodeId,
  -- | Name of the attribute to remove.
  pDOMRemoveAttributeName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMRemoveAttribute  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDOMRemoveAttribute where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Command PDOMRemoveAttribute where
   type CommandResponse PDOMRemoveAttribute = ()
   commandName _ = "DOM.removeAttribute"
   fromJSON = const . A.Success . const ()


-- | DOM.removeNode
--   Removes node with given id.

-- | Parameters of the 'DOM.removeNode' command.
data PDOMRemoveNode = PDOMRemoveNode {
  -- | Id of the node to remove.
  pDOMRemoveNodeNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMRemoveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PDOMRemoveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


instance Command PDOMRemoveNode where
   type CommandResponse PDOMRemoveNode = ()
   commandName _ = "DOM.removeNode"
   fromJSON = const . A.Success . const ()


-- | DOM.requestChildNodes
--   Requests that children of the node with given id are returned to the caller in form of
--   `setChildNodes` events where not only immediate children are retrieved, but all children down to
--   the specified depth.

-- | Parameters of the 'DOM.requestChildNodes' command.
data PDOMRequestChildNodes = PDOMRequestChildNodes {
  -- | Id of the node to get children for.
  pDOMRequestChildNodesNodeId :: DOMNodeId,
  -- | The maximum depth at which children should be retrieved, defaults to 1. Use -1 for the
  --   entire subtree or provide an integer larger than 0.
  pDOMRequestChildNodesDepth :: Maybe Int,
  -- | Whether or not iframes and shadow roots should be traversed when returning the sub-tree
  --   (default is false).
  pDOMRequestChildNodesPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMRequestChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDOMRequestChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command PDOMRequestChildNodes where
   type CommandResponse PDOMRequestChildNodes = ()
   commandName _ = "DOM.requestChildNodes"
   fromJSON = const . A.Success . const ()


-- | DOM.requestNode
--   Requests that the node is sent to the caller given the JavaScript node object reference. All
--   nodes that form the path from the node to the root are also sent to the client as a series of
--   `setChildNodes` notifications.

-- | Parameters of the 'DOM.requestNode' command.
data PDOMRequestNode = PDOMRequestNode {
  -- | JavaScript object id to convert into node.
  pDOMRequestNodeObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMRequestNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.requestNode' command.
data DOMRequestNode = DOMRequestNode {
  -- | Node id for given object.
  dOMRequestNodeNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMRequestNode where
   type CommandResponse PDOMRequestNode = DOMRequestNode
   commandName _ = "DOM.requestNode"



-- | DOM.resolveNode
--   Resolves the JavaScript node object for a given NodeId or BackendNodeId.

-- | Parameters of the 'DOM.resolveNode' command.
data PDOMResolveNode = PDOMResolveNode {
  -- | Id of the node to resolve.
  pDOMResolveNodeNodeId :: Maybe DOMNodeId,
  -- | Backend identifier of the node to resolve.
  pDOMResolveNodeBackendNodeId :: Maybe DOMBackendNodeId,
  -- | Symbolic group name that can be used to release multiple objects.
  pDOMResolveNodeObjectGroup :: Maybe String,
  -- | Execution context in which to resolve the node.
  pDOMResolveNodeExecutionContextId :: Maybe Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMResolveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.resolveNode' command.
data DOMResolveNode = DOMResolveNode {
  -- | JavaScript object wrapper for given node.
  dOMResolveNodeObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMResolveNode where
   type CommandResponse PDOMResolveNode = DOMResolveNode
   commandName _ = "DOM.resolveNode"



-- | DOM.setAttributeValue
--   Sets attribute for an element with given id.

-- | Parameters of the 'DOM.setAttributeValue' command.
data PDOMSetAttributeValue = PDOMSetAttributeValue {
  -- | Id of the element to set attribute for.
  pDOMSetAttributeValueNodeId :: DOMNodeId,
  -- | Attribute name.
  pDOMSetAttributeValueName :: String,
  -- | Attribute value.
  pDOMSetAttributeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetAttributeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDOMSetAttributeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command PDOMSetAttributeValue where
   type CommandResponse PDOMSetAttributeValue = ()
   commandName _ = "DOM.setAttributeValue"
   fromJSON = const . A.Success . const ()


-- | DOM.setAttributesAsText
--   Sets attributes on element with given id. This method is useful when user edits some existing
--   attribute value and types in several attribute name/value pairs.

-- | Parameters of the 'DOM.setAttributesAsText' command.
data PDOMSetAttributesAsText = PDOMSetAttributesAsText {
  -- | Id of the element to set attributes for.
  pDOMSetAttributesAsTextNodeId :: DOMNodeId,
  -- | Text with a number of attributes. Will parse this text using HTML parser.
  pDOMSetAttributesAsTextText :: String,
  -- | Attribute name to replace with new attributes derived from text in case text parsed
  --   successfully.
  pDOMSetAttributesAsTextName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetAttributesAsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDOMSetAttributesAsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command PDOMSetAttributesAsText where
   type CommandResponse PDOMSetAttributesAsText = ()
   commandName _ = "DOM.setAttributesAsText"
   fromJSON = const . A.Success . const ()


-- | DOM.setFileInputFiles
--   Sets files for the given file input element.

-- | Parameters of the 'DOM.setFileInputFiles' command.
data PDOMSetFileInputFiles = PDOMSetFileInputFiles {
  -- | Array of file paths to set.
  pDOMSetFileInputFilesFiles :: [String],
  -- | Identifier of the node.
  pDOMSetFileInputFilesNodeId :: Maybe DOMNodeId,
  -- | Identifier of the backend node.
  pDOMSetFileInputFilesBackendNodeId :: Maybe DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper.
  pDOMSetFileInputFilesObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetFileInputFiles  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDOMSetFileInputFiles where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Command PDOMSetFileInputFiles where
   type CommandResponse PDOMSetFileInputFiles = ()
   commandName _ = "DOM.setFileInputFiles"
   fromJSON = const . A.Success . const ()


-- | DOM.setNodeStackTracesEnabled
--   Sets if stack traces should be captured for Nodes. See `Node.getNodeStackTraces`. Default is disabled.

-- | Parameters of the 'DOM.setNodeStackTracesEnabled' command.
data PDOMSetNodeStackTracesEnabled = PDOMSetNodeStackTracesEnabled {
  -- | Enable or disable.
  pDOMSetNodeStackTracesEnabledEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetNodeStackTracesEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDOMSetNodeStackTracesEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PDOMSetNodeStackTracesEnabled where
   type CommandResponse PDOMSetNodeStackTracesEnabled = ()
   commandName _ = "DOM.setNodeStackTracesEnabled"
   fromJSON = const . A.Success . const ()


-- | DOM.getNodeStackTraces
--   Gets stack traces associated with a Node. As of now, only provides stack trace for Node creation.

-- | Parameters of the 'DOM.getNodeStackTraces' command.
data PDOMGetNodeStackTraces = PDOMGetNodeStackTraces {
  -- | Id of the node to get stack traces for.
  pDOMGetNodeStackTracesNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetNodeStackTraces  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDOMGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'DOM.getNodeStackTraces' command.
data DOMGetNodeStackTraces = DOMGetNodeStackTraces {
  -- | Creation stack trace, if available.
  dOMGetNodeStackTracesCreation :: Maybe Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PDOMGetNodeStackTraces where
   type CommandResponse PDOMGetNodeStackTraces = DOMGetNodeStackTraces
   commandName _ = "DOM.getNodeStackTraces"



-- | DOM.getFileInfo
--   Returns file information for the given
--   File wrapper.

-- | Parameters of the 'DOM.getFileInfo' command.
data PDOMGetFileInfo = PDOMGetFileInfo {
  -- | JavaScript object id of the node wrapper.
  pDOMGetFileInfoObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetFileInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.getFileInfo' command.
data DOMGetFileInfo = DOMGetFileInfo {
  dOMGetFileInfoPath :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMGetFileInfo where
   type CommandResponse PDOMGetFileInfo = DOMGetFileInfo
   commandName _ = "DOM.getFileInfo"



-- | DOM.setInspectedNode
--   Enables console to refer to the node with given id via $x (see Command Line API for more details
--   $x functions).

-- | Parameters of the 'DOM.setInspectedNode' command.
data PDOMSetInspectedNode = PDOMSetInspectedNode {
  -- | DOM node id to be accessible by means of $x command line API.
  pDOMSetInspectedNodeNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetInspectedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDOMSetInspectedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command PDOMSetInspectedNode where
   type CommandResponse PDOMSetInspectedNode = ()
   commandName _ = "DOM.setInspectedNode"
   fromJSON = const . A.Success . const ()


-- | DOM.setNodeName
--   Sets node name for a node with given id.

-- | Parameters of the 'DOM.setNodeName' command.
data PDOMSetNodeName = PDOMSetNodeName {
  -- | Id of the node to set name for.
  pDOMSetNodeNameNodeId :: DOMNodeId,
  -- | New node's name.
  pDOMSetNodeNameName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetNodeName  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDOMSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'DOM.setNodeName' command.
data DOMSetNodeName = DOMSetNodeName {
  -- | New node's id.
  dOMSetNodeNameNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PDOMSetNodeName where
   type CommandResponse PDOMSetNodeName = DOMSetNodeName
   commandName _ = "DOM.setNodeName"



-- | DOM.setNodeValue
--   Sets node value for a node with given id.

-- | Parameters of the 'DOM.setNodeValue' command.
data PDOMSetNodeValue = PDOMSetNodeValue {
  -- | Id of the node to set value for.
  pDOMSetNodeValueNodeId :: DOMNodeId,
  -- | New node's value.
  pDOMSetNodeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetNodeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMSetNodeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command PDOMSetNodeValue where
   type CommandResponse PDOMSetNodeValue = ()
   commandName _ = "DOM.setNodeValue"
   fromJSON = const . A.Success . const ()


-- | DOM.setOuterHTML
--   Sets node HTML markup, returns new node id.

-- | Parameters of the 'DOM.setOuterHTML' command.
data PDOMSetOuterHTML = PDOMSetOuterHTML {
  -- | Id of the node to set markup for.
  pDOMSetOuterHTMLNodeId :: DOMNodeId,
  -- | Outer HTML markup to set.
  pDOMSetOuterHTMLOuterHTML :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMSetOuterHTML  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDOMSetOuterHTML where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command PDOMSetOuterHTML where
   type CommandResponse PDOMSetOuterHTML = ()
   commandName _ = "DOM.setOuterHTML"
   fromJSON = const . A.Success . const ()


-- | DOM.undo
--   Undoes the last performed action.

-- | Parameters of the 'DOM.undo' command.
data PDOMUndo = PDOMUndo
instance ToJSON PDOMUndo where toJSON _ = A.Null

instance Command PDOMUndo where
   type CommandResponse PDOMUndo = ()
   commandName _ = "DOM.undo"
   fromJSON = const . A.Success . const ()


-- | DOM.getFrameOwner
--   Returns iframe node that owns iframe with the given domain.

-- | Parameters of the 'DOM.getFrameOwner' command.
data PDOMGetFrameOwner = PDOMGetFrameOwner {
  pDOMGetFrameOwnerFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetFrameOwner  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDOMGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Return type of the 'DOM.getFrameOwner' command.
data DOMGetFrameOwner = DOMGetFrameOwner {
  -- | Resulting node.
  dOMGetFrameOwnerBackendNodeId :: DOMBackendNodeId,
  -- | Id of the node at given coordinates, only when enabled and requested document.
  dOMGetFrameOwnerNodeId :: Maybe DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PDOMGetFrameOwner where
   type CommandResponse PDOMGetFrameOwner = DOMGetFrameOwner
   commandName _ = "DOM.getFrameOwner"



-- | DOM.getContainerForNode
--   Returns the container of the given node based on container query conditions.
--   If containerName is given, it will find the nearest container with a matching name;
--   otherwise it will find the nearest container regardless of its container name.

-- | Parameters of the 'DOM.getContainerForNode' command.
data PDOMGetContainerForNode = PDOMGetContainerForNode {
  pDOMGetContainerForNodeNodeId :: DOMNodeId,
  pDOMGetContainerForNodeContainerName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetContainerForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDOMGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Return type of the 'DOM.getContainerForNode' command.
data DOMGetContainerForNode = DOMGetContainerForNode {
  -- | The container node for the given node, or null if not found.
  dOMGetContainerForNodeNodeId :: Maybe DOMNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PDOMGetContainerForNode where
   type CommandResponse PDOMGetContainerForNode = DOMGetContainerForNode
   commandName _ = "DOM.getContainerForNode"



-- | DOM.getQueryingDescendantsForContainer
--   Returns the descendants of a container query container that have
--   container queries against this container.

-- | Parameters of the 'DOM.getQueryingDescendantsForContainer' command.
data PDOMGetQueryingDescendantsForContainer = PDOMGetQueryingDescendantsForContainer {
  -- | Id of the container node to find querying descendants from.
  pDOMGetQueryingDescendantsForContainerNodeId :: DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDOMGetQueryingDescendantsForContainer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDOMGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Return type of the 'DOM.getQueryingDescendantsForContainer' command.
data DOMGetQueryingDescendantsForContainer = DOMGetQueryingDescendantsForContainer {
  -- | Descendant nodes with container queries against the given container.
  dOMGetQueryingDescendantsForContainerNodeIds :: [DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DOMGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command PDOMGetQueryingDescendantsForContainer where
   type CommandResponse PDOMGetQueryingDescendantsForContainer = DOMGetQueryingDescendantsForContainer
   commandName _ = "DOM.getQueryingDescendantsForContainer"




-- | Type 'Emulation.ScreenOrientation'.
--   Screen orientation.
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



-- | Type 'Emulation.DisplayFeature'.
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
  --   orientation) or y (for horizontal orientation) direction.
  emulationDisplayFeatureOffset :: Int,
  -- | A display feature may mask content such that it is not physically
  --   displayed - this length along with the offset describes this area.
  --   A display feature that only splits content will have a 0 mask_length.
  emulationDisplayFeatureMaskLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Emulation.MediaFeature'.
data EmulationMediaFeature = EmulationMediaFeature {
  emulationMediaFeatureName :: String,
  emulationMediaFeatureValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationMediaFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  EmulationMediaFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Emulation.VirtualTimePolicy'.
--   advance: If the scheduler runs out of immediate work, the virtual time base may fast forward to
--   allow the next delayed task (if any) to run; pause: The virtual time base may not advance;
--   pauseIfNetworkFetchesPending: The virtual time base may not advance if there are any pending
--   resource fetches.
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



-- | Type 'Emulation.UserAgentBrandVersion'.
--   Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
data EmulationUserAgentBrandVersion = EmulationUserAgentBrandVersion {
  emulationUserAgentBrandVersionBrand :: String,
  emulationUserAgentBrandVersionVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationUserAgentBrandVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  EmulationUserAgentBrandVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Emulation.UserAgentMetadata'.
--   Used to specify User Agent Cient Hints to emulate. See https://wicg.github.io/ua-client-hints
--   Missing optional values will be filled in by the target with what it would normally use.
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



-- | Type 'Emulation.DisabledImageType'.
--   Enum of image types that can be disabled.
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


instance Event EmulationVirtualTimeBudgetExpired where
    eventName _ = "Emulation.virtualTimeBudgetExpired"



-- | Emulation.canEmulate
--   Tells whether emulation is supported.

-- | Parameters of the 'Emulation.canEmulate' command.
data PEmulationCanEmulate = PEmulationCanEmulate
instance ToJSON PEmulationCanEmulate where toJSON _ = A.Null

-- | Return type of the 'Emulation.canEmulate' command.
data EmulationCanEmulate = EmulationCanEmulate {
  -- | True if emulation is supported.
  emulationCanEmulateResult :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PEmulationCanEmulate where
   type CommandResponse PEmulationCanEmulate = EmulationCanEmulate
   commandName _ = "Emulation.canEmulate"



-- | Emulation.clearDeviceMetricsOverride
--   Clears the overridden device metrics.

-- | Parameters of the 'Emulation.clearDeviceMetricsOverride' command.
data PEmulationClearDeviceMetricsOverride = PEmulationClearDeviceMetricsOverride
instance ToJSON PEmulationClearDeviceMetricsOverride where toJSON _ = A.Null

instance Command PEmulationClearDeviceMetricsOverride where
   type CommandResponse PEmulationClearDeviceMetricsOverride = ()
   commandName _ = "Emulation.clearDeviceMetricsOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.clearGeolocationOverride
--   Clears the overridden Geolocation Position and Error.

-- | Parameters of the 'Emulation.clearGeolocationOverride' command.
data PEmulationClearGeolocationOverride = PEmulationClearGeolocationOverride
instance ToJSON PEmulationClearGeolocationOverride where toJSON _ = A.Null

instance Command PEmulationClearGeolocationOverride where
   type CommandResponse PEmulationClearGeolocationOverride = ()
   commandName _ = "Emulation.clearGeolocationOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.resetPageScaleFactor
--   Requests that page scale factor is reset to initial values.

-- | Parameters of the 'Emulation.resetPageScaleFactor' command.
data PEmulationResetPageScaleFactor = PEmulationResetPageScaleFactor
instance ToJSON PEmulationResetPageScaleFactor where toJSON _ = A.Null

instance Command PEmulationResetPageScaleFactor where
   type CommandResponse PEmulationResetPageScaleFactor = ()
   commandName _ = "Emulation.resetPageScaleFactor"
   fromJSON = const . A.Success . const ()


-- | Emulation.setFocusEmulationEnabled
--   Enables or disables simulating a focused and active page.

-- | Parameters of the 'Emulation.setFocusEmulationEnabled' command.
data PEmulationSetFocusEmulationEnabled = PEmulationSetFocusEmulationEnabled {
  -- | Whether to enable to disable focus emulation.
  pEmulationSetFocusEmulationEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetFocusEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetFocusEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Command PEmulationSetFocusEmulationEnabled where
   type CommandResponse PEmulationSetFocusEmulationEnabled = ()
   commandName _ = "Emulation.setFocusEmulationEnabled"
   fromJSON = const . A.Success . const ()


-- | Emulation.setAutoDarkModeOverride
--   Automatically render all web contents using a dark theme.

-- | Parameters of the 'Emulation.setAutoDarkModeOverride' command.
data PEmulationSetAutoDarkModeOverride = PEmulationSetAutoDarkModeOverride {
  -- | Whether to enable or disable automatic dark mode.
  --   If not specified, any existing override will be cleared.
  pEmulationSetAutoDarkModeOverrideEnabled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutoDarkModeOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutoDarkModeOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Command PEmulationSetAutoDarkModeOverride where
   type CommandResponse PEmulationSetAutoDarkModeOverride = ()
   commandName _ = "Emulation.setAutoDarkModeOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setCPUThrottlingRate
--   Enables CPU throttling to emulate slow CPUs.

-- | Parameters of the 'Emulation.setCPUThrottlingRate' command.
data PEmulationSetCPUThrottlingRate = PEmulationSetCPUThrottlingRate {
  -- | Throttling rate as a slowdown factor (1 is no throttle, 2 is 2x slowdown, etc).
  pEmulationSetCPUThrottlingRateRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetCPUThrottlingRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetCPUThrottlingRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PEmulationSetCPUThrottlingRate where
   type CommandResponse PEmulationSetCPUThrottlingRate = ()
   commandName _ = "Emulation.setCPUThrottlingRate"
   fromJSON = const . A.Success . const ()


-- | Emulation.setDefaultBackgroundColorOverride
--   Sets or clears an override of the default background color of the frame. This override is used
--   if the content does not specify one.

-- | Parameters of the 'Emulation.setDefaultBackgroundColorOverride' command.
data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
  -- | RGBA of the default background color. If not specified, any existing override will be
  --   cleared.
  pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DOMRGBA
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


instance Command PEmulationSetDefaultBackgroundColorOverride where
   type CommandResponse PEmulationSetDefaultBackgroundColorOverride = ()
   commandName _ = "Emulation.setDefaultBackgroundColorOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setDeviceMetricsOverride
--   Overrides the values of device screen dimensions (window.screen.width, window.screen.height,
--   window.innerWidth, window.innerHeight, and "device-width"/"device-height"-related CSS media
--   query results).

-- | Parameters of the 'Emulation.setDeviceMetricsOverride' command.
data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


instance Command PEmulationSetDeviceMetricsOverride where
   type CommandResponse PEmulationSetDeviceMetricsOverride = ()
   commandName _ = "Emulation.setDeviceMetricsOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setScrollbarsHidden

-- | Parameters of the 'Emulation.setScrollbarsHidden' command.
data PEmulationSetScrollbarsHidden = PEmulationSetScrollbarsHidden {
  -- | Whether scrollbars should be always hidden.
  pEmulationSetScrollbarsHiddenHidden :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScrollbarsHidden  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScrollbarsHidden where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PEmulationSetScrollbarsHidden where
   type CommandResponse PEmulationSetScrollbarsHidden = ()
   commandName _ = "Emulation.setScrollbarsHidden"
   fromJSON = const . A.Success . const ()


-- | Emulation.setDocumentCookieDisabled

-- | Parameters of the 'Emulation.setDocumentCookieDisabled' command.
data PEmulationSetDocumentCookieDisabled = PEmulationSetDocumentCookieDisabled {
  -- | Whether document.coookie API should be disabled.
  pEmulationSetDocumentCookieDisabledDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDocumentCookieDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDocumentCookieDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PEmulationSetDocumentCookieDisabled where
   type CommandResponse PEmulationSetDocumentCookieDisabled = ()
   commandName _ = "Emulation.setDocumentCookieDisabled"
   fromJSON = const . A.Success . const ()


-- | Emulation.setEmitTouchEventsForMouse

-- | Parameters of the 'Emulation.setEmitTouchEventsForMouse' command.
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


instance Command PEmulationSetEmitTouchEventsForMouse where
   type CommandResponse PEmulationSetEmitTouchEventsForMouse = ()
   commandName _ = "Emulation.setEmitTouchEventsForMouse"
   fromJSON = const . A.Success . const ()


-- | Emulation.setEmulatedMedia
--   Emulates the given media type or media feature for CSS media queries.

-- | Parameters of the 'Emulation.setEmulatedMedia' command.
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


instance Command PEmulationSetEmulatedMedia where
   type CommandResponse PEmulationSetEmulatedMedia = ()
   commandName _ = "Emulation.setEmulatedMedia"
   fromJSON = const . A.Success . const ()


-- | Emulation.setEmulatedVisionDeficiency
--   Emulates the given vision deficiency.

-- | Parameters of the 'Emulation.setEmulatedVisionDeficiency' command.
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


instance Command PEmulationSetEmulatedVisionDeficiency where
   type CommandResponse PEmulationSetEmulatedVisionDeficiency = ()
   commandName _ = "Emulation.setEmulatedVisionDeficiency"
   fromJSON = const . A.Success . const ()


-- | Emulation.setGeolocationOverride
--   Overrides the Geolocation Position or Error. Omitting any of the parameters emulates position
--   unavailable.

-- | Parameters of the 'Emulation.setGeolocationOverride' command.
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


instance Command PEmulationSetGeolocationOverride where
   type CommandResponse PEmulationSetGeolocationOverride = ()
   commandName _ = "Emulation.setGeolocationOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setIdleOverride
--   Overrides the Idle state.

-- | Parameters of the 'Emulation.setIdleOverride' command.
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


instance Command PEmulationSetIdleOverride where
   type CommandResponse PEmulationSetIdleOverride = ()
   commandName _ = "Emulation.setIdleOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.clearIdleOverride
--   Clears Idle state overrides.

-- | Parameters of the 'Emulation.clearIdleOverride' command.
data PEmulationClearIdleOverride = PEmulationClearIdleOverride
instance ToJSON PEmulationClearIdleOverride where toJSON _ = A.Null

instance Command PEmulationClearIdleOverride where
   type CommandResponse PEmulationClearIdleOverride = ()
   commandName _ = "Emulation.clearIdleOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setPageScaleFactor
--   Sets a specified page scale factor.

-- | Parameters of the 'Emulation.setPageScaleFactor' command.
data PEmulationSetPageScaleFactor = PEmulationSetPageScaleFactor {
  -- | Page scale factor.
  pEmulationSetPageScaleFactorPageScaleFactor :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetPageScaleFactor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetPageScaleFactor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PEmulationSetPageScaleFactor where
   type CommandResponse PEmulationSetPageScaleFactor = ()
   commandName _ = "Emulation.setPageScaleFactor"
   fromJSON = const . A.Success . const ()


-- | Emulation.setScriptExecutionDisabled
--   Switches script execution in the page.

-- | Parameters of the 'Emulation.setScriptExecutionDisabled' command.
data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
  -- | Whether script execution should be disabled in the page.
  pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Command PEmulationSetScriptExecutionDisabled where
   type CommandResponse PEmulationSetScriptExecutionDisabled = ()
   commandName _ = "Emulation.setScriptExecutionDisabled"
   fromJSON = const . A.Success . const ()


-- | Emulation.setTouchEmulationEnabled
--   Enables touch on platforms which do not support them.

-- | Parameters of the 'Emulation.setTouchEmulationEnabled' command.
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


instance Command PEmulationSetTouchEmulationEnabled where
   type CommandResponse PEmulationSetTouchEmulationEnabled = ()
   commandName _ = "Emulation.setTouchEmulationEnabled"
   fromJSON = const . A.Success . const ()


-- | Emulation.setVirtualTimePolicy
--   Turns on virtual time for all frames (replacing real-time with a synthetic time source) and sets
--   the current virtual time policy.  Note this supersedes any previous time budget.

-- | Parameters of the 'Emulation.setVirtualTimePolicy' command.
data PEmulationSetVirtualTimePolicy = PEmulationSetVirtualTimePolicy {
  pEmulationSetVirtualTimePolicyPolicy :: EmulationVirtualTimePolicy,
  -- | If set, after this many virtual milliseconds have elapsed virtual time will be paused and a
  --   virtualTimeBudgetExpired event is sent.
  pEmulationSetVirtualTimePolicyBudget :: Maybe Double,
  -- | If set this specifies the maximum number of tasks that can be run before virtual is forced
  --   forwards to prevent deadlock.
  pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount :: Maybe Int,
  -- | If set, base::Time::Now will be overridden to initially return this value.
  pEmulationSetVirtualTimePolicyInitialVirtualTime :: Maybe NetworkTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetVirtualTimePolicy  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Return type of the 'Emulation.setVirtualTimePolicy' command.
data EmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy {
  -- | Absolute timestamp at which virtual time was first enabled (up time in milliseconds).
  emulationSetVirtualTimePolicyVirtualTimeTicksBase :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PEmulationSetVirtualTimePolicy where
   type CommandResponse PEmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy
   commandName _ = "Emulation.setVirtualTimePolicy"



-- | Emulation.setLocaleOverride
--   Overrides default host system locale with the specified one.

-- | Parameters of the 'Emulation.setLocaleOverride' command.
data PEmulationSetLocaleOverride = PEmulationSetLocaleOverride {
  -- | ICU style C locale (e.g. "en_US"). If not specified or empty, disables the override and
  --   restores default host system locale.
  pEmulationSetLocaleOverrideLocale :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetLocaleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetLocaleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PEmulationSetLocaleOverride where
   type CommandResponse PEmulationSetLocaleOverride = ()
   commandName _ = "Emulation.setLocaleOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setTimezoneOverride
--   Overrides default host system timezone with the specified one.

-- | Parameters of the 'Emulation.setTimezoneOverride' command.
data PEmulationSetTimezoneOverride = PEmulationSetTimezoneOverride {
  -- | The timezone identifier. If empty, disables the override and
  --   restores default host system timezone.
  pEmulationSetTimezoneOverrideTimezoneId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTimezoneOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTimezoneOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PEmulationSetTimezoneOverride where
   type CommandResponse PEmulationSetTimezoneOverride = ()
   commandName _ = "Emulation.setTimezoneOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setDisabledImageTypes

-- | Parameters of the 'Emulation.setDisabledImageTypes' command.
data PEmulationSetDisabledImageTypes = PEmulationSetDisabledImageTypes {
  -- | Image types to disable.
  pEmulationSetDisabledImageTypesImageTypes :: [EmulationDisabledImageType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDisabledImageTypes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDisabledImageTypes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PEmulationSetDisabledImageTypes where
   type CommandResponse PEmulationSetDisabledImageTypes = ()
   commandName _ = "Emulation.setDisabledImageTypes"
   fromJSON = const . A.Success . const ()


-- | Emulation.setHardwareConcurrencyOverride

-- | Parameters of the 'Emulation.setHardwareConcurrencyOverride' command.
data PEmulationSetHardwareConcurrencyOverride = PEmulationSetHardwareConcurrencyOverride {
  -- | Hardware concurrency to report
  pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetHardwareConcurrencyOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetHardwareConcurrencyOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


instance Command PEmulationSetHardwareConcurrencyOverride where
   type CommandResponse PEmulationSetHardwareConcurrencyOverride = ()
   commandName _ = "Emulation.setHardwareConcurrencyOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setUserAgentOverride
--   Allows overriding user agent with the given string.

-- | Parameters of the 'Emulation.setUserAgentOverride' command.
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


instance Command PEmulationSetUserAgentOverride where
   type CommandResponse PEmulationSetUserAgentOverride = ()
   commandName _ = "Emulation.setUserAgentOverride"
   fromJSON = const . A.Success . const ()


-- | Emulation.setAutomationOverride
--   Allows overriding the automation flag.

-- | Parameters of the 'Emulation.setAutomationOverride' command.
data PEmulationSetAutomationOverride = PEmulationSetAutomationOverride {
  -- | Whether the override should be enabled.
  pEmulationSetAutomationOverrideEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutomationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutomationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Command PEmulationSetAutomationOverride where
   type CommandResponse PEmulationSetAutomationOverride = ()
   commandName _ = "Emulation.setAutomationOverride"
   fromJSON = const . A.Success . const ()



-- | Type 'Network.ResourceType'.
--   Resource type as it was perceived by the rendering engine.
data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXHR | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCSPViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
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
         "XHR" -> pure NetworkResourceTypeXHR
         "Fetch" -> pure NetworkResourceTypeFetch
         "EventSource" -> pure NetworkResourceTypeEventSource
         "WebSocket" -> pure NetworkResourceTypeWebSocket
         "Manifest" -> pure NetworkResourceTypeManifest
         "SignedExchange" -> pure NetworkResourceTypeSignedExchange
         "Ping" -> pure NetworkResourceTypePing
         "CSPViolationReport" -> pure NetworkResourceTypeCSPViolationReport
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
         NetworkResourceTypeXHR -> "XHR"
         NetworkResourceTypeFetch -> "Fetch"
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
type NetworkLoaderId = String

-- | Type 'Network.RequestId'.
--   Unique request identifier.
type NetworkRequestId = String

-- | Type 'Network.InterceptionId'.
--   Unique intercepted request identifier.
type NetworkInterceptionId = String

-- | Type 'Network.ErrorReason'.
--   Network level fetch failure reason.
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



-- | Type 'Network.TimeSinceEpoch'.
--   UTC time in seconds, counted from January 1, 1970.
type NetworkTimeSinceEpoch = Double

-- | Type 'Network.MonotonicTime'.
--   Monotonically increasing time in seconds since an arbitrary point in the past.
type NetworkMonotonicTime = Double

-- | Type 'Network.Headers'.
--   Request / response headers as keys / values of JSON object.
type NetworkHeaders = [(String, String)]

-- | Type 'Network.ConnectionType'.
--   The underlying connection technology that the browser is supposedly using.
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



-- | Type 'Network.CookieSameSite'.
--   Represents the cookie's 'SameSite' status:
--   https://tools.ietf.org/html/draft-west-first-party-cookies
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



-- | Type 'Network.CookiePriority'.
--   Represents the cookie's 'Priority' status:
--   https://tools.ietf.org/html/draft-west-cookie-priority-00
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



-- | Type 'Network.CookieSourceScheme'.
--   Represents the source scheme of the origin that originally set the cookie.
--   A value of "Unset" allows protocol clients to emulate legacy cookie scope for the scheme.
--   This is a temporary ability and it will be removed in the future.
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



-- | Type 'Network.ResourceTiming'.
--   Timing information for the request.
data NetworkResourceTiming = NetworkResourceTiming {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Network.ResourcePriority'.
--   Loading priority of a resource request.
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



-- | Type 'Network.PostDataEntry'.
--   Post data entry for HTTP request
data NetworkPostDataEntry = NetworkPostDataEntry {
  networkPostDataEntryBytes :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkPostDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkPostDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Network.Request'.
--   HTTP request data.
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
  --   passed by the developer (e.g. via "fetch") as understood by the backend.
  networkRequestTrustTokenParams :: Maybe NetworkTrustTokenParams,
  -- | True if this resource request is considered to be the 'same site' as the
  --   request correspondinfg to the main frame.
  networkRequestIsSameSite :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  NetworkRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'Network.SignedCertificateTimestamp'.
--   Details of a signed certificate timestamp (SCT).
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
  --   milliseconds since January 1, 1970, UTC, not the number of seconds.
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



-- | Type 'Network.SecurityDetails'.
--   Security details about a request.
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



-- | Type 'Network.CertificateTransparencyCompliance'.
--   Whether the request complied with Certificate Transparency policy.
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



-- | Type 'Network.BlockedReason'.
--   The reason why request was blocked.
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



-- | Type 'Network.CorsError'.
--   The reason why request was blocked.
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



-- | Type 'Network.CorsErrorStatus'.
data NetworkCorsErrorStatus = NetworkCorsErrorStatus {
  networkCorsErrorStatusCorsError :: NetworkCorsError,
  networkCorsErrorStatusFailedParameter :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCorsErrorStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkCorsErrorStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Network.ServiceWorkerResponseSource'.
--   Source of serviceworker response.
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



-- | Type 'Network.TrustTokenParams'.
--   Determines what type of Trust Token operation is executed and
--   depending on the type, some additional parameters. The values
--   are specified in third_party/blink/renderer/core/fetch/trust_token.idl.
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
  --   to request a fresh SRR or use a still valid cached SRR.
  networkTrustTokenParamsRefreshPolicy :: NetworkTrustTokenParamsRefreshPolicy,
  -- | Origins of issuers from whom to request tokens or redemption
  --   records.
  networkTrustTokenParamsIssuers :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Network.TrustTokenOperationType'.
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



-- | Type 'Network.Response'.
--   HTTP response data.
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
  networkResponseRemoteIPAddress :: Maybe String,
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



-- | Type 'Network.WebSocketRequest'.
--   WebSocket request data.
data NetworkWebSocketRequest = NetworkWebSocketRequest {
  -- | HTTP request headers.
  networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Network.WebSocketResponse'.
--   WebSocket response data.
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



-- | Type 'Network.WebSocketFrame'.
--   WebSocket message data. This represents an entire WebSocket message, not just a fragmented frame as the name suggests.
data NetworkWebSocketFrame = NetworkWebSocketFrame {
  -- | WebSocket message opcode.
  networkWebSocketFrameOpcode :: Double,
  -- | WebSocket message mask.
  networkWebSocketFrameMask :: Bool,
  -- | WebSocket message payload data.
  --   If the opcode is 1, this is a text message and payloadData is a UTF-8 string.
  --   If the opcode isn't 1, then payloadData is a base64 encoded string representing binary data.
  networkWebSocketFramePayloadData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type 'Network.CachedResource'.
--   Information about the cached resource.
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



-- | Type 'Network.Initiator'.
--   Information about the request initiator.
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
  --   module) (0-based).
  networkInitiatorLineNumber :: Maybe Double,
  -- | Initiator column number, set for Parser type or for Script type (when script is importing
  --   module) (0-based).
  networkInitiatorColumnNumber :: Maybe Double,
  -- | Set if another request triggered this request (e.g. preflight).
  networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkInitiator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  NetworkInitiator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Network.Cookie'.
--   Cookie object
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
  --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  --   This is a temporary ability and it will be removed in the future.
  networkCookieSourcePort :: Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  --   of the request to the endpoint that set the cookie.
  networkCookiePartitionKey :: Maybe String,
  -- | True if cookie partition key is opaque.
  networkCookiePartitionKeyOpaque :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  NetworkCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Network.SetCookieBlockedReason'.
--   Types of reasons why a cookie may not be stored from a response.
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



-- | Type 'Network.CookieBlockedReason'.
--   Types of reasons why a cookie may not be sent with a request.
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



-- | Type 'Network.BlockedSetCookieWithReason'.
--   A cookie which was not stored from a response with the corresponding reason.
data NetworkBlockedSetCookieWithReason = NetworkBlockedSetCookieWithReason {
  -- | The reason(s) this cookie was blocked.
  networkBlockedSetCookieWithReasonBlockedReasons :: [NetworkSetCookieBlockedReason],
  -- | The string representing this individual cookie as it would appear in the header.
  --   This is not the entire "cookie" or "set-cookie" header which could have multiple cookies.
  networkBlockedSetCookieWithReasonCookieLine :: String,
  -- | The cookie object which represents the cookie which was not stored. It is optional because
  --   sometimes complete cookie information is not available, such as in the case of parsing
  --   errors.
  networkBlockedSetCookieWithReasonCookie :: Maybe NetworkCookie
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedSetCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedSetCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Network.BlockedCookieWithReason'.
--   A cookie with was not sent with a request with the corresponding reason.
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



-- | Type 'Network.CookieParam'.
--   Cookie parameter object
data NetworkCookieParam = NetworkCookieParam {
  -- | Cookie name.
  networkCookieParamName :: String,
  -- | Cookie value.
  networkCookieParamValue :: String,
  -- | The request-URI to associate with the setting of the cookie. This value can affect the
  --   default domain, path, source port, and source scheme values of the created cookie.
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
  --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  --   This is a temporary ability and it will be removed in the future.
  networkCookieParamSourcePort :: Maybe Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  --   of the request to the endpoint that set the cookie.
  --   If not set, the cookie will be set as not partitioned.
  networkCookieParamPartitionKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookieParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  NetworkCookieParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Network.AuthChallenge'.
--   Authorization challenge for HTTP status code 401 or 407.
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



-- | Type 'Network.AuthChallengeResponse'.
--   Response to an AuthChallenge.
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
  --   deferring to the default behavior of the net stack, which will likely either the Cancel
  --   authentication or display a popup dialog box.
  networkAuthChallengeResponseResponse :: NetworkAuthChallengeResponseResponse,
  -- | The username to provide, possibly empty. Should only be set if response is
  --   ProvideCredentials.
  networkAuthChallengeResponseUsername :: Maybe String,
  -- | The password to provide, possibly empty. Should only be set if response is
  --   ProvideCredentials.
  networkAuthChallengeResponsePassword :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  NetworkAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type 'Network.InterceptionStage'.
--   Stages of the interception to begin intercepting. Request will intercept before the request is
--   sent. Response will intercept after the response is received.
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



-- | Type 'Network.RequestPattern'.
--   Request pattern for interception.
data NetworkRequestPattern = NetworkRequestPattern {
  -- | Wildcards (`'*'` -> zero or more, `'?'` -> exactly one) are allowed. Escape character is
  --   backslash. Omitting is equivalent to `"*"`.
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



-- | Type 'Network.SignedExchangeSignature'.
--   Information about a signed exchange signature.
--   https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#rfc.section.3.1
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



-- | Type 'Network.SignedExchangeHeader'.
--   Information about a signed exchange header.
--   https://wicg.github.io/webpackage/draft-yasskin-httpbis-origin-signed-exchanges-impl.html#cbor-representation
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



-- | Type 'Network.SignedExchangeErrorField'.
--   Field type for a signed exchange related error.
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



-- | Type 'Network.SignedExchangeError'.
--   Information about a signed exchange response.
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



-- | Type 'Network.SignedExchangeInfo'.
--   Information about a signed exchange response.
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



-- | Type 'Network.ContentEncoding'.
--   List of content encodings supported by the backend.
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



-- | Type 'Network.PrivateNetworkRequestPolicy'.
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



-- | Type 'Network.IPAddressSpace'.
data NetworkIPAddressSpace = NetworkIPAddressSpaceLocal | NetworkIPAddressSpacePrivate | NetworkIPAddressSpacePublic | NetworkIPAddressSpaceUnknown
   deriving (Ord, Eq, Show, Read)
instance FromJSON NetworkIPAddressSpace where
   parseJSON = A.withText  "NetworkIPAddressSpace"  $ \v -> do
      case v of
         "Local" -> pure NetworkIPAddressSpaceLocal
         "Private" -> pure NetworkIPAddressSpacePrivate
         "Public" -> pure NetworkIPAddressSpacePublic
         "Unknown" -> pure NetworkIPAddressSpaceUnknown
         _ -> fail "failed to parse NetworkIPAddressSpace"

instance ToJSON NetworkIPAddressSpace where
   toJSON v = A.String $
      case v of
         NetworkIPAddressSpaceLocal -> "Local"
         NetworkIPAddressSpacePrivate -> "Private"
         NetworkIPAddressSpacePublic -> "Public"
         NetworkIPAddressSpaceUnknown -> "Unknown"



-- | Type 'Network.ConnectTiming'.
data NetworkConnectTiming = NetworkConnectTiming {
  -- | Timing's requestTime is a baseline in seconds, while the other numbers are ticks in
  --   milliseconds relatively to this requestTime. Matches ResourceTiming's requestTime for
  --   the same request (but not for redirected requests).
  networkConnectTimingRequestTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkConnectTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkConnectTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Network.ClientSecurityState'.
data NetworkClientSecurityState = NetworkClientSecurityState {
  networkClientSecurityStateInitiatorIsSecureContext :: Bool,
  networkClientSecurityStateInitiatorIPAddressSpace :: NetworkIPAddressSpace,
  networkClientSecurityStatePrivateNetworkRequestPolicy :: NetworkPrivateNetworkRequestPolicy
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkClientSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkClientSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Network.CrossOriginOpenerPolicyValue'.
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



-- | Type 'Network.CrossOriginOpenerPolicyStatus'.
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



-- | Type 'Network.CrossOriginEmbedderPolicyValue'.
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



-- | Type 'Network.CrossOriginEmbedderPolicyStatus'.
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



-- | Type 'Network.SecurityIsolationStatus'.
data NetworkSecurityIsolationStatus = NetworkSecurityIsolationStatus {
  networkSecurityIsolationStatusCoop :: Maybe NetworkCrossOriginOpenerPolicyStatus,
  networkSecurityIsolationStatusCoep :: Maybe NetworkCrossOriginEmbedderPolicyStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Network.ReportStatus'.
--   The status of a Reporting API report.
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



-- | Type 'Network.ReportId'.
type NetworkReportId = String

-- | Type 'Network.ReportingApiReport'.
--   An object representing a report generated by the Reporting API.
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



-- | Type 'Network.ReportingApiEndpoint'.
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



-- | Type 'Network.LoadNetworkResourcePageResult'.
--   An object providing the result of a network resource load.
data NetworkLoadNetworkResourcePageResult = NetworkLoadNetworkResourcePageResult {
  networkLoadNetworkResourcePageResultSuccess :: Bool,
  -- | Optional values used for error reporting.
  networkLoadNetworkResourcePageResultNetError :: Maybe Double,
  networkLoadNetworkResourcePageResultNetErrorName :: Maybe String,
  networkLoadNetworkResourcePageResultHttpStatusCode :: Maybe Double,
  -- | If successful, one of the following two fields holds the result.
  networkLoadNetworkResourcePageResultStream :: Maybe IO.IOStreamHandle,
  -- | Response headers.
  networkLoadNetworkResourcePageResultHeaders :: Maybe NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourcePageResult  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourcePageResult where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Network.LoadNetworkResourceOptions'.
--   An options object that may be extended later to better support CORS,
--   CORB and streaming.
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


instance Event NetworkDataReceived where
    eventName _ = "Network.dataReceived"

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


instance Event NetworkEventSourceMessageReceived where
    eventName _ = "Network.eventSourceMessageReceived"

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


instance Event NetworkLoadingFailed where
    eventName _ = "Network.loadingFailed"

-- | Type of the 'Network.loadingFinished' event.
data NetworkLoadingFinished = NetworkLoadingFinished {
  -- | Request identifier.
  networkLoadingFinishedRequestId :: NetworkRequestId,
  -- | Timestamp.
  networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
  -- | Total number of bytes received for this request.
  networkLoadingFinishedEncodedDataLength :: Double,
  -- | Set when 1) response was blocked by Cross-Origin Read Blocking and also
  --   2) this needs to be reported to the DevTools console.
  networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Event NetworkLoadingFinished where
    eventName _ = "Network.loadingFinished"

-- | Type of the 'Network.requestServedFromCache' event.
data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
  -- | Request identifier.
  networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestServedFromCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestServedFromCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Event NetworkRequestServedFromCache where
    eventName _ = "Network.requestServedFromCache"

-- | Type of the 'Network.requestWillBeSent' event.
data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
  -- | Request identifier.
  networkRequestWillBeSentRequestId :: NetworkRequestId,
  -- | Loader identifier. Empty string if the request is fetched from worker.
  networkRequestWillBeSentLoaderId :: NetworkLoaderId,
  -- | URL of the document this request is loaded for.
  networkRequestWillBeSentDocumentURL :: String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event NetworkRequestWillBeSent where
    eventName _ = "Network.requestWillBeSent"

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


instance Event NetworkResourceChangedPriority where
    eventName _ = "Network.resourceChangedPriority"

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


instance Event NetworkSignedExchangeReceived where
    eventName _ = "Network.signedExchangeReceived"

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
  --   or were emitted for this request.
  networkResponseReceivedHasExtraInfo :: Bool,
  -- | Frame identifier.
  networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event NetworkResponseReceived where
    eventName _ = "Network.responseReceived"

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


instance Event NetworkWebSocketClosed where
    eventName _ = "Network.webSocketClosed"

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


instance Event NetworkWebSocketCreated where
    eventName _ = "Network.webSocketCreated"

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


instance Event NetworkWebSocketFrameError where
    eventName _ = "Network.webSocketFrameError"

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


instance Event NetworkWebSocketFrameReceived where
    eventName _ = "Network.webSocketFrameReceived"

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


instance Event NetworkWebSocketFrameSent where
    eventName _ = "Network.webSocketFrameSent"

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


instance Event NetworkWebSocketHandshakeResponseReceived where
    eventName _ = "Network.webSocketHandshakeResponseReceived"

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


instance Event NetworkWebSocketWillSendHandshakeRequest where
    eventName _ = "Network.webSocketWillSendHandshakeRequest"

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


instance Event NetworkWebTransportCreated where
    eventName _ = "Network.webTransportCreated"

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


instance Event NetworkWebTransportConnectionEstablished where
    eventName _ = "Network.webTransportConnectionEstablished"

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


instance Event NetworkWebTransportClosed where
    eventName _ = "Network.webTransportClosed"

-- | Type of the 'Network.requestWillBeSentExtraInfo' event.
data NetworkRequestWillBeSentExtraInfo = NetworkRequestWillBeSentExtraInfo {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSentExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSentExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


instance Event NetworkRequestWillBeSentExtraInfo where
    eventName _ = "Network.requestWillBeSentExtraInfo"

-- | Type of the 'Network.responseReceivedExtraInfo' event.
data NetworkResponseReceivedExtraInfo = NetworkResponseReceivedExtraInfo {
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
  networkResponseReceivedExtraInfoHeadersText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceivedExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceivedExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event NetworkResponseReceivedExtraInfo where
    eventName _ = "Network.responseReceivedExtraInfo"

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
  --   'AlreadyExists' also signifies a successful operation, as the result
  --   of the operation already exists und thus, the operation was abort
  --   preemptively (e.g. a cache hit).
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


instance Event NetworkTrustTokenOperationDone where
    eventName _ = "Network.trustTokenOperationDone"

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


instance Event NetworkSubresourceWebBundleMetadataReceived where
    eventName _ = "Network.subresourceWebBundleMetadataReceived"

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


instance Event NetworkSubresourceWebBundleMetadataError where
    eventName _ = "Network.subresourceWebBundleMetadataError"

-- | Type of the 'Network.subresourceWebBundleInnerResponseParsed' event.
data NetworkSubresourceWebBundleInnerResponseParsed = NetworkSubresourceWebBundleInnerResponseParsed {
  -- | Request identifier of the subresource request
  networkSubresourceWebBundleInnerResponseParsedInnerRequestId :: NetworkRequestId,
  -- | URL of the subresource resource.
  networkSubresourceWebBundleInnerResponseParsedInnerRequestURL :: String,
  -- | Bundle request identifier. Used to match this information to another event.
  --   This made be absent in case when the instrumentation was enabled only
  --   after webbundle was parsed.
  networkSubresourceWebBundleInnerResponseParsedBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }


instance Event NetworkSubresourceWebBundleInnerResponseParsed where
    eventName _ = "Network.subresourceWebBundleInnerResponseParsed"

-- | Type of the 'Network.subresourceWebBundleInnerResponseError' event.
data NetworkSubresourceWebBundleInnerResponseError = NetworkSubresourceWebBundleInnerResponseError {
  -- | Request identifier of the subresource request
  networkSubresourceWebBundleInnerResponseErrorInnerRequestId :: NetworkRequestId,
  -- | URL of the subresource resource.
  networkSubresourceWebBundleInnerResponseErrorInnerRequestURL :: String,
  -- | Error message
  networkSubresourceWebBundleInnerResponseErrorErrorMessage :: String,
  -- | Bundle request identifier. Used to match this information to another event.
  --   This made be absent in case when the instrumentation was enabled only
  --   after webbundle was parsed.
  networkSubresourceWebBundleInnerResponseErrorBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }


instance Event NetworkSubresourceWebBundleInnerResponseError where
    eventName _ = "Network.subresourceWebBundleInnerResponseError"

-- | Type of the 'Network.reportingApiReportAdded' event.
data NetworkReportingApiReportAdded = NetworkReportingApiReportAdded {
  networkReportingApiReportAddedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Event NetworkReportingApiReportAdded where
    eventName _ = "Network.reportingApiReportAdded"

-- | Type of the 'Network.reportingApiReportUpdated' event.
data NetworkReportingApiReportUpdated = NetworkReportingApiReportUpdated {
  networkReportingApiReportUpdatedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


instance Event NetworkReportingApiReportUpdated where
    eventName _ = "Network.reportingApiReportUpdated"

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


instance Event NetworkReportingApiEndpointsChangedForOrigin where
    eventName _ = "Network.reportingApiEndpointsChangedForOrigin"



-- | Network.setAcceptedEncodings
--   Sets a list of content encodings that will be accepted. Empty list means no encoding is accepted.

-- | Parameters of the 'Network.setAcceptedEncodings' command.
data PNetworkSetAcceptedEncodings = PNetworkSetAcceptedEncodings {
  -- | List of accepted content encodings.
  pNetworkSetAcceptedEncodingsEncodings :: [NetworkContentEncoding]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAcceptedEncodings  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAcceptedEncodings where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PNetworkSetAcceptedEncodings where
   type CommandResponse PNetworkSetAcceptedEncodings = ()
   commandName _ = "Network.setAcceptedEncodings"
   fromJSON = const . A.Success . const ()


-- | Network.clearAcceptedEncodingsOverride
--   Clears accepted encodings set by setAcceptedEncodings

-- | Parameters of the 'Network.clearAcceptedEncodingsOverride' command.
data PNetworkClearAcceptedEncodingsOverride = PNetworkClearAcceptedEncodingsOverride
instance ToJSON PNetworkClearAcceptedEncodingsOverride where toJSON _ = A.Null

instance Command PNetworkClearAcceptedEncodingsOverride where
   type CommandResponse PNetworkClearAcceptedEncodingsOverride = ()
   commandName _ = "Network.clearAcceptedEncodingsOverride"
   fromJSON = const . A.Success . const ()


-- | Network.clearBrowserCache
--   Clears browser cache.

-- | Parameters of the 'Network.clearBrowserCache' command.
data PNetworkClearBrowserCache = PNetworkClearBrowserCache
instance ToJSON PNetworkClearBrowserCache where toJSON _ = A.Null

instance Command PNetworkClearBrowserCache where
   type CommandResponse PNetworkClearBrowserCache = ()
   commandName _ = "Network.clearBrowserCache"
   fromJSON = const . A.Success . const ()


-- | Network.clearBrowserCookies
--   Clears browser cookies.

-- | Parameters of the 'Network.clearBrowserCookies' command.
data PNetworkClearBrowserCookies = PNetworkClearBrowserCookies
instance ToJSON PNetworkClearBrowserCookies where toJSON _ = A.Null

instance Command PNetworkClearBrowserCookies where
   type CommandResponse PNetworkClearBrowserCookies = ()
   commandName _ = "Network.clearBrowserCookies"
   fromJSON = const . A.Success . const ()


-- | Network.deleteCookies
--   Deletes browser cookies with matching name and url or domain/path pair.

-- | Parameters of the 'Network.deleteCookies' command.
data PNetworkDeleteCookies = PNetworkDeleteCookies {
  -- | Name of the cookies to remove.
  pNetworkDeleteCookiesName :: String,
  -- | If specified, deletes all the cookies with the given name where domain and path match
  --   provided URL.
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


instance Command PNetworkDeleteCookies where
   type CommandResponse PNetworkDeleteCookies = ()
   commandName _ = "Network.deleteCookies"
   fromJSON = const . A.Success . const ()


-- | Network.disable
--   Disables network tracking, prevents network events from being sent to the client.

-- | Parameters of the 'Network.disable' command.
data PNetworkDisable = PNetworkDisable
instance ToJSON PNetworkDisable where toJSON _ = A.Null

instance Command PNetworkDisable where
   type CommandResponse PNetworkDisable = ()
   commandName _ = "Network.disable"
   fromJSON = const . A.Success . const ()


-- | Network.emulateNetworkConditions
--   Activates emulation of network conditions.

-- | Parameters of the 'Network.emulateNetworkConditions' command.
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


instance Command PNetworkEmulateNetworkConditions where
   type CommandResponse PNetworkEmulateNetworkConditions = ()
   commandName _ = "Network.emulateNetworkConditions"
   fromJSON = const . A.Success . const ()


-- | Network.enable
--   Enables network tracking, network events will now be delivered to the client.

-- | Parameters of the 'Network.enable' command.
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


instance Command PNetworkEnable where
   type CommandResponse PNetworkEnable = ()
   commandName _ = "Network.enable"
   fromJSON = const . A.Success . const ()


-- | Network.getAllCookies
--   Returns all browser cookies. Depending on the backend support, will return detailed cookie
--   information in the `cookies` field.

-- | Parameters of the 'Network.getAllCookies' command.
data PNetworkGetAllCookies = PNetworkGetAllCookies
instance ToJSON PNetworkGetAllCookies where toJSON _ = A.Null

-- | Return type of the 'Network.getAllCookies' command.
data NetworkGetAllCookies = NetworkGetAllCookies {
  -- | Array of cookie objects.
  networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetAllCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PNetworkGetAllCookies where
   type CommandResponse PNetworkGetAllCookies = NetworkGetAllCookies
   commandName _ = "Network.getAllCookies"



-- | Network.getCertificate
--   Returns the DER-encoded certificate.

-- | Parameters of the 'Network.getCertificate' command.
data PNetworkGetCertificate = PNetworkGetCertificate {
  -- | Origin to get certificate for.
  pNetworkGetCertificateOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCertificate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'Network.getCertificate' command.
data NetworkGetCertificate = NetworkGetCertificate {
  networkGetCertificateTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PNetworkGetCertificate where
   type CommandResponse PNetworkGetCertificate = NetworkGetCertificate
   commandName _ = "Network.getCertificate"



-- | Network.getCookies
--   Returns all browser cookies for the current URL. Depending on the backend support, will return
--   detailed cookie information in the `cookies` field.

-- | Parameters of the 'Network.getCookies' command.
data PNetworkGetCookies = PNetworkGetCookies {
  -- | The list of URLs for which applicable cookies will be fetched.
  --   If not specified, it's assumed to be set to the list containing
  --   the URLs of the page and all of its subframes.
  pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Return type of the 'Network.getCookies' command.
data NetworkGetCookies = NetworkGetCookies {
  -- | Array of cookie objects.
  networkGetCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command PNetworkGetCookies where
   type CommandResponse PNetworkGetCookies = NetworkGetCookies
   commandName _ = "Network.getCookies"



-- | Network.getResponseBody
--   Returns content served for the given request.

-- | Parameters of the 'Network.getResponseBody' command.
data PNetworkGetResponseBody = PNetworkGetResponseBody {
  -- | Identifier of the network request to get content for.
  pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Return type of the 'Network.getResponseBody' command.
data NetworkGetResponseBody = NetworkGetResponseBody {
  -- | Response body.
  networkGetResponseBodyBody :: String,
  -- | True, if content was sent as base64.
  networkGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PNetworkGetResponseBody where
   type CommandResponse PNetworkGetResponseBody = NetworkGetResponseBody
   commandName _ = "Network.getResponseBody"



-- | Network.getRequestPostData
--   Returns post data sent with the request. Returns an error when no data was sent with the request.

-- | Parameters of the 'Network.getRequestPostData' command.
data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
  -- | Identifier of the network request to get content for.
  pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetRequestPostData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Return type of the 'Network.getRequestPostData' command.
data NetworkGetRequestPostData = NetworkGetRequestPostData {
  -- | Request body string, omitting files from multipart requests
  networkGetRequestPostDataPostData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command PNetworkGetRequestPostData where
   type CommandResponse PNetworkGetRequestPostData = NetworkGetRequestPostData
   commandName _ = "Network.getRequestPostData"



-- | Network.getResponseBodyForInterception
--   Returns content served for the given currently intercepted request.

-- | Parameters of the 'Network.getResponseBodyForInterception' command.
data PNetworkGetResponseBodyForInterception = PNetworkGetResponseBodyForInterception {
  -- | Identifier for the intercepted request to get body for.
  pNetworkGetResponseBodyForInterceptionInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBodyForInterception  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Return type of the 'Network.getResponseBodyForInterception' command.
data NetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception {
  -- | Response body.
  networkGetResponseBodyForInterceptionBody :: String,
  -- | True, if content was sent as base64.
  networkGetResponseBodyForInterceptionBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command PNetworkGetResponseBodyForInterception where
   type CommandResponse PNetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception
   commandName _ = "Network.getResponseBodyForInterception"



-- | Network.takeResponseBodyForInterceptionAsStream
--   Returns a handle to the stream representing the response body. Note that after this command,
--   the intercepted request can't be continued as is -- you either need to cancel it or to provide
--   the response body. The stream only supports sequential read, IO.read will fail if the position
--   is specified.

-- | Parameters of the 'Network.takeResponseBodyForInterceptionAsStream' command.
data PNetworkTakeResponseBodyForInterceptionAsStream = PNetworkTakeResponseBodyForInterceptionAsStream {
  pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkTakeResponseBodyForInterceptionAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  PNetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }


-- | Return type of the 'Network.takeResponseBodyForInterceptionAsStream' command.
data NetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream {
  networkTakeResponseBodyForInterceptionAsStreamStream :: IO.IOStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }

instance Command PNetworkTakeResponseBodyForInterceptionAsStream where
   type CommandResponse PNetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream
   commandName _ = "Network.takeResponseBodyForInterceptionAsStream"



-- | Network.replayXHR
--   This method sends a new XMLHttpRequest which is identical to the original one. The following
--   parameters should be identical: method, url, async, request body, extra headers, withCredentials
--   attribute, user, password.

-- | Parameters of the 'Network.replayXHR' command.
data PNetworkReplayXHR = PNetworkReplayXHR {
  -- | Identifier of XHR to replay.
  pNetworkReplayXHRRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkReplayXHR  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkReplayXHR where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PNetworkReplayXHR where
   type CommandResponse PNetworkReplayXHR = ()
   commandName _ = "Network.replayXHR"
   fromJSON = const . A.Success . const ()


-- | Network.searchInResponseBody
--   Searches for given string in response content.

-- | Parameters of the 'Network.searchInResponseBody' command.
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


-- | Return type of the 'Network.searchInResponseBody' command.
data NetworkSearchInResponseBody = NetworkSearchInResponseBody {
  -- | List of search matches.
  networkSearchInResponseBodyResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PNetworkSearchInResponseBody where
   type CommandResponse PNetworkSearchInResponseBody = NetworkSearchInResponseBody
   commandName _ = "Network.searchInResponseBody"



-- | Network.setBlockedURLs
--   Blocks URLs from loading.

-- | Parameters of the 'Network.setBlockedURLs' command.
data PNetworkSetBlockedURLs = PNetworkSetBlockedURLs {
  -- | URL patterns to block. Wildcards ('*') are allowed.
  pNetworkSetBlockedURLsUrls :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBlockedURLs  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBlockedURLs where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Command PNetworkSetBlockedURLs where
   type CommandResponse PNetworkSetBlockedURLs = ()
   commandName _ = "Network.setBlockedURLs"
   fromJSON = const . A.Success . const ()


-- | Network.setBypassServiceWorker
--   Toggles ignoring of service worker for each request.

-- | Parameters of the 'Network.setBypassServiceWorker' command.
data PNetworkSetBypassServiceWorker = PNetworkSetBypassServiceWorker {
  -- | Bypass service worker and load from network.
  pNetworkSetBypassServiceWorkerBypass :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBypassServiceWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBypassServiceWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PNetworkSetBypassServiceWorker where
   type CommandResponse PNetworkSetBypassServiceWorker = ()
   commandName _ = "Network.setBypassServiceWorker"
   fromJSON = const . A.Success . const ()


-- | Network.setCacheDisabled
--   Toggles ignoring cache for each request. If `true`, cache will not be used.

-- | Parameters of the 'Network.setCacheDisabled' command.
data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
  -- | Cache disabled state.
  pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCacheDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCacheDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PNetworkSetCacheDisabled where
   type CommandResponse PNetworkSetCacheDisabled = ()
   commandName _ = "Network.setCacheDisabled"
   fromJSON = const . A.Success . const ()


-- | Network.setCookie
--   Sets a cookie with the given cookie data; may overwrite equivalent cookies if they exist.

-- | Parameters of the 'Network.setCookie' command.
data PNetworkSetCookie = PNetworkSetCookie {
  -- | Cookie name.
  pNetworkSetCookieName :: String,
  -- | Cookie value.
  pNetworkSetCookieValue :: String,
  -- | The request-URI to associate with the setting of the cookie. This value can affect the
  --   default domain, path, source port, and source scheme values of the created cookie.
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
  --   An unspecified port value allows protocol clients to emulate legacy cookie scope for the port.
  --   This is a temporary ability and it will be removed in the future.
  pNetworkSetCookieSourcePort :: Maybe Int,
  -- | Cookie partition key. The site of the top-level URL the browser was visiting at the start
  --   of the request to the endpoint that set the cookie.
  --   If not set, the cookie will be set as not partitioned.
  pNetworkSetCookiePartitionKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PNetworkSetCookie where
   type CommandResponse PNetworkSetCookie = ()
   commandName _ = "Network.setCookie"
   fromJSON = const . A.Success . const ()


-- | Network.setCookies
--   Sets given cookies.

-- | Parameters of the 'Network.setCookies' command.
data PNetworkSetCookies = PNetworkSetCookies {
  -- | Cookies to be set.
  pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Command PNetworkSetCookies where
   type CommandResponse PNetworkSetCookies = ()
   commandName _ = "Network.setCookies"
   fromJSON = const . A.Success . const ()


-- | Network.setExtraHTTPHeaders
--   Specifies whether to always send extra HTTP headers with the requests from this page.

-- | Parameters of the 'Network.setExtraHTTPHeaders' command.
data PNetworkSetExtraHTTPHeaders = PNetworkSetExtraHTTPHeaders {
  -- | Map with extra HTTP headers.
  pNetworkSetExtraHTTPHeadersHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetExtraHTTPHeaders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetExtraHTTPHeaders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PNetworkSetExtraHTTPHeaders where
   type CommandResponse PNetworkSetExtraHTTPHeaders = ()
   commandName _ = "Network.setExtraHTTPHeaders"
   fromJSON = const . A.Success . const ()


-- | Network.setAttachDebugStack
--   Specifies whether to attach a page script stack id in requests

-- | Parameters of the 'Network.setAttachDebugStack' command.
data PNetworkSetAttachDebugStack = PNetworkSetAttachDebugStack {
  -- | Whether to attach a page script stack for debugging purpose.
  pNetworkSetAttachDebugStackEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAttachDebugStack  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAttachDebugStack where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PNetworkSetAttachDebugStack where
   type CommandResponse PNetworkSetAttachDebugStack = ()
   commandName _ = "Network.setAttachDebugStack"
   fromJSON = const . A.Success . const ()


-- | Network.setUserAgentOverride
--   Allows overriding user agent with the given string.

-- | Parameters of the 'Network.setUserAgentOverride' command.
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


instance Command PNetworkSetUserAgentOverride where
   type CommandResponse PNetworkSetUserAgentOverride = ()
   commandName _ = "Network.setUserAgentOverride"
   fromJSON = const . A.Success . const ()


-- | Network.getSecurityIsolationStatus
--   Returns information about the COEP/COOP isolation status.

-- | Parameters of the 'Network.getSecurityIsolationStatus' command.
data PNetworkGetSecurityIsolationStatus = PNetworkGetSecurityIsolationStatus {
  -- | If no frameId is provided, the status of the target is provided.
  pNetworkGetSecurityIsolationStatusFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Return type of the 'Network.getSecurityIsolationStatus' command.
data NetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus {
  networkGetSecurityIsolationStatusStatus :: NetworkSecurityIsolationStatus
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command PNetworkGetSecurityIsolationStatus where
   type CommandResponse PNetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus
   commandName _ = "Network.getSecurityIsolationStatus"



-- | Network.enableReportingApi
--   Enables tracking for the Reporting API, events generated by the Reporting API will now be delivered to the client.
--   Enabling triggers 'reportingApiReportAdded' for all existing reports.

-- | Parameters of the 'Network.enableReportingApi' command.
data PNetworkEnableReportingApi = PNetworkEnableReportingApi {
  -- | Whether to enable or disable events for the Reporting API
  pNetworkEnableReportingApiEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnableReportingApi  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnableReportingApi where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PNetworkEnableReportingApi where
   type CommandResponse PNetworkEnableReportingApi = ()
   commandName _ = "Network.enableReportingApi"
   fromJSON = const . A.Success . const ()


-- | Network.loadNetworkResource
--   Fetches the resource and returns the content.

-- | Parameters of the 'Network.loadNetworkResource' command.
data PNetworkLoadNetworkResource = PNetworkLoadNetworkResource {
  -- | Frame id to get the resource for. Mandatory for frame targets, and
  --   should be omitted for worker targets.
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


-- | Return type of the 'Network.loadNetworkResource' command.
data NetworkLoadNetworkResource = NetworkLoadNetworkResource {
  networkLoadNetworkResourceResource :: NetworkLoadNetworkResourcePageResult
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command PNetworkLoadNetworkResource where
   type CommandResponse PNetworkLoadNetworkResource = NetworkLoadNetworkResource
   commandName _ = "Network.loadNetworkResource"




-- | Type 'Page.FrameId'.
--   Unique frame identifier.
type PageFrameId = String

-- | Type 'Page.AdFrameType'.
--   Indicates whether a frame has been identified as an ad.
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



-- | Type 'Page.AdFrameExplanation'.
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



-- | Type 'Page.AdFrameStatus'.
--   Indicates whether a frame has been identified as an ad and why.
data PageAdFrameStatus = PageAdFrameStatus {
  pageAdFrameStatusAdFrameType :: PageAdFrameType,
  pageAdFrameStatusExplanations :: Maybe [PageAdFrameExplanation]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAdFrameStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageAdFrameStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'Page.SecureContextType'.
--   Indicates whether the frame is a secure context and why it is the case.
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



-- | Type 'Page.CrossOriginIsolatedContextType'.
--   Indicates whether the frame is cross-origin isolated and why it is the case.
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



-- | Type 'Page.GatedAPIFeatures'.
data PageGatedAPIFeatures = PageGatedAPIFeaturesSharedArrayBuffers | PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed | PageGatedAPIFeaturesPerformanceMeasureMemory | PageGatedAPIFeaturesPerformanceProfile
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageGatedAPIFeatures where
   parseJSON = A.withText  "PageGatedAPIFeatures"  $ \v -> do
      case v of
         "SharedArrayBuffers" -> pure PageGatedAPIFeaturesSharedArrayBuffers
         "SharedArrayBuffersTransferAllowed" -> pure PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed
         "PerformanceMeasureMemory" -> pure PageGatedAPIFeaturesPerformanceMeasureMemory
         "PerformanceProfile" -> pure PageGatedAPIFeaturesPerformanceProfile
         _ -> fail "failed to parse PageGatedAPIFeatures"

instance ToJSON PageGatedAPIFeatures where
   toJSON v = A.String $
      case v of
         PageGatedAPIFeaturesSharedArrayBuffers -> "SharedArrayBuffers"
         PageGatedAPIFeaturesSharedArrayBuffersTransferAllowed -> "SharedArrayBuffersTransferAllowed"
         PageGatedAPIFeaturesPerformanceMeasureMemory -> "PerformanceMeasureMemory"
         PageGatedAPIFeaturesPerformanceProfile -> "PerformanceProfile"



-- | Type 'Page.PermissionsPolicyFeature'.
--   All Permissions Policy features. This enum should match the one defined
--   in third_party/blink/renderer/core/permissions_policy/permissions_policy_features.json5.
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



-- | Type 'Page.PermissionsPolicyBlockReason'.
--   Reason for a permissions policy feature to be disabled.
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



-- | Type 'Page.PermissionsPolicyBlockLocator'.
data PagePermissionsPolicyBlockLocator = PagePermissionsPolicyBlockLocator {
  pagePermissionsPolicyBlockLocatorFrameId :: PageFrameId,
  pagePermissionsPolicyBlockLocatorBlockReason :: PagePermissionsPolicyBlockReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyBlockLocator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyBlockLocator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Page.PermissionsPolicyFeatureState'.
data PagePermissionsPolicyFeatureState = PagePermissionsPolicyFeatureState {
  pagePermissionsPolicyFeatureStateFeature :: PagePermissionsPolicyFeature,
  pagePermissionsPolicyFeatureStateAllowed :: Bool,
  pagePermissionsPolicyFeatureStateLocator :: Maybe PagePermissionsPolicyBlockLocator
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyFeatureState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyFeatureState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'Page.OriginTrialTokenStatus'.
--   Origin Trial(https://www.chromium.org/blink/origin-trials) support.
--   Status for an Origin Trial token.
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



-- | Type 'Page.OriginTrialStatus'.
--   Status for an Origin Trial.
data PageOriginTrialStatus = PageOriginTrialStatusEnabled | PageOriginTrialStatusValidTokenNotProvided | PageOriginTrialStatusOSNotSupported | PageOriginTrialStatusTrialNotAllowed
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageOriginTrialStatus where
   parseJSON = A.withText  "PageOriginTrialStatus"  $ \v -> do
      case v of
         "Enabled" -> pure PageOriginTrialStatusEnabled
         "ValidTokenNotProvided" -> pure PageOriginTrialStatusValidTokenNotProvided
         "OSNotSupported" -> pure PageOriginTrialStatusOSNotSupported
         "TrialNotAllowed" -> pure PageOriginTrialStatusTrialNotAllowed
         _ -> fail "failed to parse PageOriginTrialStatus"

instance ToJSON PageOriginTrialStatus where
   toJSON v = A.String $
      case v of
         PageOriginTrialStatusEnabled -> "Enabled"
         PageOriginTrialStatusValidTokenNotProvided -> "ValidTokenNotProvided"
         PageOriginTrialStatusOSNotSupported -> "OSNotSupported"
         PageOriginTrialStatusTrialNotAllowed -> "TrialNotAllowed"



-- | Type 'Page.OriginTrialUsageRestriction'.
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



-- | Type 'Page.OriginTrialToken'.
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



-- | Type 'Page.OriginTrialTokenWithStatus'.
data PageOriginTrialTokenWithStatus = PageOriginTrialTokenWithStatus {
  pageOriginTrialTokenWithStatusRawTokenText :: String,
  -- | `parsedToken` is present only when the token is extractable and
  --   parsable.
  pageOriginTrialTokenWithStatusParsedToken :: Maybe PageOriginTrialToken,
  pageOriginTrialTokenWithStatusStatus :: PageOriginTrialTokenStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialTokenWithStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialTokenWithStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Page.OriginTrial'.
data PageOriginTrial = PageOriginTrial {
  pageOriginTrialTrialName :: String,
  pageOriginTrialStatus :: PageOriginTrialStatus,
  pageOriginTrialTokensWithStatus :: [PageOriginTrialTokenWithStatus]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrial  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrial where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'Page.Frame'.
--   Information about the Frame on the page.
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
  --   Extracted from the Frame's url.
  --   Example URLs: http://www.google.com/file.html -> "google.com"
  --                 http://a.b.co.uk/file.html      -> "b.co.uk"
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
  pageFrameGatedAPIFeatures :: [PageGatedAPIFeatures]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



-- | Type 'Page.FrameResource'.
--   Information about the Resource on the page.
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



-- | Type 'Page.FrameResourceTree'.
--   Information about the Frame hierarchy along with their cached resources.
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



-- | Type 'Page.FrameTree'.
--   Information about the Frame hierarchy.
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



-- | Type 'Page.ScriptIdentifier'.
--   Unique script identifier.
type PageScriptIdentifier = String

-- | Type 'Page.TransitionType'.
--   Transition type.
data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddress_bar | PageTransitionTypeAuto_bookmark | PageTransitionTypeAuto_subframe | PageTransitionTypeManual_subframe | PageTransitionTypeGenerated | PageTransitionTypeAuto_toplevel | PageTransitionTypeForm_submit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeyword_generated | PageTransitionTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageTransitionType where
   parseJSON = A.withText  "PageTransitionType"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse PageTransitionType"

instance ToJSON PageTransitionType where
   toJSON v = A.String $
      case v of
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
data PageNavigationEntry = PageNavigationEntry {
  -- | Unique id of the navigation history entry.
  pageNavigationEntryId :: Int,
  -- | URL of the navigation history entry.
  pageNavigationEntryUrl :: String,
  -- | URL that the user typed in the url bar.
  pageNavigationEntryUserTypedURL :: String,
  -- | Title of the navigation history entry.
  pageNavigationEntryTitle :: String,
  -- | Transition type.
  pageNavigationEntryTransitionType :: PageTransitionType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigationEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageNavigationEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Page.ScreencastFrameMetadata'.
--   Screencast frame metadata.
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



-- | Type 'Page.DialogType'.
--   Javascript dialog type.
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



-- | Type 'Page.AppManifestError'.
--   Error while paring app manifest.
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



-- | Type 'Page.AppManifestParsedProperties'.
--   Parsed app manifest properties.
data PageAppManifestParsedProperties = PageAppManifestParsedProperties {
  -- | Computed scope value
  pageAppManifestParsedPropertiesScope :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestParsedProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestParsedProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type 'Page.LayoutViewport'.
--   Layout viewport position and dimensions.
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



-- | Type 'Page.VisualViewport'.
--   Visual viewport position, dimensions, and scale.
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



-- | Type 'Page.Viewport'.
--   Viewport for capturing screenshot.
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



-- | Type 'Page.FontFamilies'.
--   Generic font families collection.
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



-- | Type 'Page.ScriptFontFamilies'.
--   Font families collection for a script.
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



-- | Type 'Page.FontSizes'.
--   Default font sizes.
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



-- | Type 'Page.ClientNavigationReason'.
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



-- | Type 'Page.ClientNavigationDisposition'.
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



-- | Type 'Page.InstallabilityErrorArgument'.
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



-- | Type 'Page.InstallabilityError'.
--   The installability error
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



-- | Type 'Page.ReferrerPolicy'.
--   The referring-policy used for the navigation.
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



-- | Type 'Page.CompilationCacheParams'.
--   Per-script compilation cache parameters for `Page.produceCompilationCache`
data PageCompilationCacheParams = PageCompilationCacheParams {
  -- | The URL of the script to produce a compilation cache entry for.
  pageCompilationCacheParamsUrl :: String,
  -- | A hint to the backend whether eager compilation is recommended.
  --   (the actual compilation mode used is upon backend discretion).
  pageCompilationCacheParamsEager :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Page.NavigationType'.
--   The type of a frameNavigated event.
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



-- | Type 'Page.BackForwardCacheNotRestoredReason'.
--   List of not restored reasons for back-forward cache.
data PageBackForwardCacheNotRestoredReason = PageBackForwardCacheNotRestoredReasonNotPrimaryMainFrame | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabled | PageBackForwardCacheNotRestoredReasonRelatedActiveContentsExist | PageBackForwardCacheNotRestoredReasonHTTPStatusNotOK | PageBackForwardCacheNotRestoredReasonSchemeNotHTTPOrHTTPS | PageBackForwardCacheNotRestoredReasonLoading | PageBackForwardCacheNotRestoredReasonWasGrantedMediaAccess | PageBackForwardCacheNotRestoredReasonDisableForRenderFrameHostCalled | PageBackForwardCacheNotRestoredReasonDomainNotAllowed | PageBackForwardCacheNotRestoredReasonHTTPMethodNotGET | PageBackForwardCacheNotRestoredReasonSubframeIsNavigating | PageBackForwardCacheNotRestoredReasonTimeout | PageBackForwardCacheNotRestoredReasonCacheLimit | PageBackForwardCacheNotRestoredReasonJavaScriptExecution | PageBackForwardCacheNotRestoredReasonRendererProcessKilled | PageBackForwardCacheNotRestoredReasonRendererProcessCrashed | PageBackForwardCacheNotRestoredReasonSchedulerTrackedFeatureUsed | PageBackForwardCacheNotRestoredReasonConflictingBrowsingInstance | PageBackForwardCacheNotRestoredReasonCacheFlushed | PageBackForwardCacheNotRestoredReasonServiceWorkerVersionActivation | PageBackForwardCacheNotRestoredReasonSessionRestored | PageBackForwardCacheNotRestoredReasonServiceWorkerPostMessage | PageBackForwardCacheNotRestoredReasonEnteredBackForwardCacheBeforeServiceWorkerHostAdded | PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_SameSite | PageBackForwardCacheNotRestoredReasonRenderFrameHostReused_CrossSite | PageBackForwardCacheNotRestoredReasonServiceWorkerClaim | PageBackForwardCacheNotRestoredReasonIgnoreEventAndEvict | PageBackForwardCacheNotRestoredReasonHaveInnerContents | PageBackForwardCacheNotRestoredReasonTimeoutPuttingInCache | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByLowMemory | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledByCommandLine | PageBackForwardCacheNotRestoredReasonNetworkRequestDatapipeDrainedAsBytesConsumer | PageBackForwardCacheNotRestoredReasonNetworkRequestRedirected | PageBackForwardCacheNotRestoredReasonNetworkRequestTimeout | PageBackForwardCacheNotRestoredReasonNetworkExceedsBufferLimit | PageBackForwardCacheNotRestoredReasonNavigationCancelledWhileRestoring | PageBackForwardCacheNotRestoredReasonNotMostRecentNavigationEntry | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForPrerender | PageBackForwardCacheNotRestoredReasonUserAgentOverrideDiffers | PageBackForwardCacheNotRestoredReasonForegroundCacheLimit | PageBackForwardCacheNotRestoredReasonBrowsingInstanceNotSwapped | PageBackForwardCacheNotRestoredReasonBackForwardCacheDisabledForDelegate | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInMainFrame | PageBackForwardCacheNotRestoredReasonUnloadHandlerExistsInSubFrame | PageBackForwardCacheNotRestoredReasonServiceWorkerUnregistration | PageBackForwardCacheNotRestoredReasonCacheControlNoStore | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreCookieModified | PageBackForwardCacheNotRestoredReasonCacheControlNoStoreHTTPOnlyCookieModified | PageBackForwardCacheNotRestoredReasonNoResponseHead | PageBackForwardCacheNotRestoredReasonUnknown | PageBackForwardCacheNotRestoredReasonActivationNavigationsDisallowedForBug1234857 | PageBackForwardCacheNotRestoredReasonErrorDocument | PageBackForwardCacheNotRestoredReasonFencedFramesEmbedder | PageBackForwardCacheNotRestoredReasonWebSocket | PageBackForwardCacheNotRestoredReasonWebTransport | PageBackForwardCacheNotRestoredReasonWebRTC | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonMainResourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoStore | PageBackForwardCacheNotRestoredReasonSubresourceHasCacheControlNoCache | PageBackForwardCacheNotRestoredReasonContainsPlugins | PageBackForwardCacheNotRestoredReasonDocumentLoaded | PageBackForwardCacheNotRestoredReasonDedicatedWorkerOrWorklet | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestOthers | PageBackForwardCacheNotRestoredReasonOutstandingIndexedDBTransaction | PageBackForwardCacheNotRestoredReasonRequestedNotificationsPermission | PageBackForwardCacheNotRestoredReasonRequestedMIDIPermission | PageBackForwardCacheNotRestoredReasonRequestedAudioCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedVideoCapturePermission | PageBackForwardCacheNotRestoredReasonRequestedBackForwardCacheBlockedSensors | PageBackForwardCacheNotRestoredReasonRequestedBackgroundWorkPermission | PageBackForwardCacheNotRestoredReasonBroadcastChannel | PageBackForwardCacheNotRestoredReasonIndexedDBConnection | PageBackForwardCacheNotRestoredReasonWebXR | PageBackForwardCacheNotRestoredReasonSharedWorker | PageBackForwardCacheNotRestoredReasonWebLocks | PageBackForwardCacheNotRestoredReasonWebHID | PageBackForwardCacheNotRestoredReasonWebShare | PageBackForwardCacheNotRestoredReasonRequestedStorageAccessGrant | PageBackForwardCacheNotRestoredReasonWebNfc | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestFetch | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestXHR | PageBackForwardCacheNotRestoredReasonAppBanner | PageBackForwardCacheNotRestoredReasonPrinting | PageBackForwardCacheNotRestoredReasonWebDatabase | PageBackForwardCacheNotRestoredReasonPictureInPicture | PageBackForwardCacheNotRestoredReasonPortal | PageBackForwardCacheNotRestoredReasonSpeechRecognizer | PageBackForwardCacheNotRestoredReasonIdleManager | PageBackForwardCacheNotRestoredReasonPaymentManager | PageBackForwardCacheNotRestoredReasonSpeechSynthesis | PageBackForwardCacheNotRestoredReasonKeyboardLock | PageBackForwardCacheNotRestoredReasonWebOTPService | PageBackForwardCacheNotRestoredReasonOutstandingNetworkRequestDirectSocket | PageBackForwardCacheNotRestoredReasonInjectedJavascript | PageBackForwardCacheNotRestoredReasonInjectedStyleSheet | PageBackForwardCacheNotRestoredReasonDummy | PageBackForwardCacheNotRestoredReasonContentSecurityHandler | PageBackForwardCacheNotRestoredReasonContentWebAuthenticationAPI | PageBackForwardCacheNotRestoredReasonContentFileChooser | PageBackForwardCacheNotRestoredReasonContentSerial | PageBackForwardCacheNotRestoredReasonContentFileSystemAccess | PageBackForwardCacheNotRestoredReasonContentMediaDevicesDispatcherHost | PageBackForwardCacheNotRestoredReasonContentWebBluetooth | PageBackForwardCacheNotRestoredReasonContentWebUSB | PageBackForwardCacheNotRestoredReasonContentMediaSessionService | PageBackForwardCacheNotRestoredReasonContentScreenReader | PageBackForwardCacheNotRestoredReasonEmbedderPopupBlockerTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingTriggeredPopupBlocker | PageBackForwardCacheNotRestoredReasonEmbedderSafeBrowsingThreatDetails | PageBackForwardCacheNotRestoredReasonEmbedderAppBannerManager | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerViewerSource | PageBackForwardCacheNotRestoredReasonEmbedderDomDistillerSelfDeletingRequestDelegate | PageBackForwardCacheNotRestoredReasonEmbedderOomInterventionTabHelper | PageBackForwardCacheNotRestoredReasonEmbedderOfflinePage | PageBackForwardCacheNotRestoredReasonEmbedderChromePasswordManagerClientBindCredentialManager | PageBackForwardCacheNotRestoredReasonEmbedderPermissionRequestManager | PageBackForwardCacheNotRestoredReasonEmbedderModalDialog | PageBackForwardCacheNotRestoredReasonEmbedderExtensions | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessaging | PageBackForwardCacheNotRestoredReasonEmbedderExtensionMessagingForOpenPort | PageBackForwardCacheNotRestoredReasonEmbedderExtensionSentMessageToCachedFrame
   deriving (Ord, Eq, Show, Read)
instance FromJSON PageBackForwardCacheNotRestoredReason where
   parseJSON = A.withText  "PageBackForwardCacheNotRestoredReason"  $ \v -> do
      case v of
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
         _ -> fail "failed to parse PageBackForwardCacheNotRestoredReason"

instance ToJSON PageBackForwardCacheNotRestoredReason where
   toJSON v = A.String $
      case v of
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



-- | Type 'Page.BackForwardCacheNotRestoredExplanation'.
data PageBackForwardCacheNotRestoredExplanation = PageBackForwardCacheNotRestoredExplanation {
  -- | Type of the reason
  pageBackForwardCacheNotRestoredExplanationType :: PageBackForwardCacheNotRestoredReasonType,
  -- | Not restored reason
  pageBackForwardCacheNotRestoredExplanationReason :: PageBackForwardCacheNotRestoredReason,
  -- | Context associated with the reason. The meaning of this context is
  --   dependent on the reason:
  --   - EmbedderExtensionSentMessageToCachedFrame: the extension ID.
  pageBackForwardCacheNotRestoredExplanationContext :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



-- | Type 'Page.BackForwardCacheNotRestoredExplanationTree'.
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



-- | Type 'Page.PrerenderFinalStatus'.
--   List of FinalStatus reasons for Prerender2.
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


instance Event PageDomContentEventFired where
    eventName _ = "Page.domContentEventFired"

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
  pageFileChooserOpenedBackendNodeId :: DOMBackendNodeId,
  -- | Input mode.
  pageFileChooserOpenedMode :: PageFileChooserOpenedMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFileChooserOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFileChooserOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Event PageFileChooserOpened where
    eventName _ = "Page.fileChooserOpened"

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


instance Event PageFrameAttached where
    eventName _ = "Page.frameAttached"

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


instance Event PageFrameDetached where
    eventName _ = "Page.frameDetached"

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


instance Event PageFrameNavigated where
    eventName _ = "Page.frameNavigated"

-- | Type of the 'Page.documentOpened' event.
data PageDocumentOpened = PageDocumentOpened {
  -- | Frame object.
  pageDocumentOpenedFrame :: PageFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDocumentOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageDocumentOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Event PageDocumentOpened where
    eventName _ = "Page.documentOpened"

-- | Type of the 'Page.frameResized' event.
data PageFrameResized = PageFrameResized
   deriving (Eq, Show, Read)
instance FromJSON PageFrameResized where
   parseJSON = A.withText  "PageFrameResized"  $ \v -> do
      case v of
         "PageFrameResized" -> pure PageFrameResized
         _ -> fail "failed to parse PageFrameResized"


instance Event PageFrameResized where
    eventName _ = "Page.frameResized"

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


instance Event PageFrameRequestedNavigation where
    eventName _ = "Page.frameRequestedNavigation"

-- | Type of the 'Page.frameStartedLoading' event.
data PageFrameStartedLoading = PageFrameStartedLoading {
  -- | Id of the frame that has started loading.
  pageFrameStartedLoadingFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStartedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStartedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event PageFrameStartedLoading where
    eventName _ = "Page.frameStartedLoading"

-- | Type of the 'Page.frameStoppedLoading' event.
data PageFrameStoppedLoading = PageFrameStoppedLoading {
  -- | Id of the frame that has stopped loading.
  pageFrameStoppedLoadingFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStoppedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStoppedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event PageFrameStoppedLoading where
    eventName _ = "Page.frameStoppedLoading"

-- | Type of the 'Page.interstitialHidden' event.
data PageInterstitialHidden = PageInterstitialHidden
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
   parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
      case v of
         "PageInterstitialHidden" -> pure PageInterstitialHidden
         _ -> fail "failed to parse PageInterstitialHidden"


instance Event PageInterstitialHidden where
    eventName _ = "Page.interstitialHidden"

-- | Type of the 'Page.interstitialShown' event.
data PageInterstitialShown = PageInterstitialShown
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
   parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
      case v of
         "PageInterstitialShown" -> pure PageInterstitialShown
         _ -> fail "failed to parse PageInterstitialShown"


instance Event PageInterstitialShown where
    eventName _ = "Page.interstitialShown"

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


instance Event PageJavascriptDialogClosed where
    eventName _ = "Page.javascriptDialogClosed"

-- | Type of the 'Page.javascriptDialogOpening' event.
data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
  -- | Frame url.
  pageJavascriptDialogOpeningUrl :: String,
  -- | Message that will be displayed by the dialog.
  pageJavascriptDialogOpeningMessage :: String,
  -- | Dialog type.
  pageJavascriptDialogOpeningType :: PageDialogType,
  -- | True iff browser is capable showing or acting on the given dialog. When browser has no
  --   dialog handler for given target, calling alert while Page domain is engaged will stall
  --   the page execution. Execution can be resumed via calling Page.handleJavaScriptDialog.
  pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
  -- | Default dialog prompt.
  pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogOpening  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogOpening where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Event PageJavascriptDialogOpening where
    eventName _ = "Page.javascriptDialogOpening"

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


instance Event PageLifecycleEvent where
    eventName _ = "Page.lifecycleEvent"

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


instance Event PageBackForwardCacheNotUsed where
    eventName _ = "Page.backForwardCacheNotUsed"

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


instance Event PagePrerenderAttemptCompleted where
    eventName _ = "Page.prerenderAttemptCompleted"

-- | Type of the 'Page.loadEventFired' event.
data PageLoadEventFired = PageLoadEventFired {
  pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLoadEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLoadEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Event PageLoadEventFired where
    eventName _ = "Page.loadEventFired"

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


instance Event PageNavigatedWithinDocument where
    eventName _ = "Page.navigatedWithinDocument"

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


instance Event PageScreencastFrame where
    eventName _ = "Page.screencastFrame"

-- | Type of the 'Page.screencastVisibilityChanged' event.
data PageScreencastVisibilityChanged = PageScreencastVisibilityChanged {
  -- | True if the page is visible.
  pageScreencastVisibilityChangedVisible :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastVisibilityChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageScreencastVisibilityChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Event PageScreencastVisibilityChanged where
    eventName _ = "Page.screencastVisibilityChanged"

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


instance Event PageWindowOpen where
    eventName _ = "Page.windowOpen"

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


instance Event PageCompilationCacheProduced where
    eventName _ = "Page.compilationCacheProduced"



-- | Page.addScriptToEvaluateOnNewDocument
--   Evaluates given script in every frame upon creation (before loading frame's scripts).

-- | Parameters of the 'Page.addScriptToEvaluateOnNewDocument' command.
data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
  pPageAddScriptToEvaluateOnNewDocumentSource :: String,
  -- | If specified, creates an isolated world with the given name and evaluates given script in it.
  --   This world name will be used as the ExecutionContextDescription::name when the corresponding
  --   event is emitted.
  pPageAddScriptToEvaluateOnNewDocumentWorldName :: Maybe String,
  -- | Specifies whether command line API should be available to the script, defaults
  --   to false.
  pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineAPI :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Return type of the 'Page.addScriptToEvaluateOnNewDocument' command.
data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
  -- | Identifier of the added script.
  pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PPageAddScriptToEvaluateOnNewDocument where
   type CommandResponse PPageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument
   commandName _ = "Page.addScriptToEvaluateOnNewDocument"



-- | Page.bringToFront
--   Brings page to front (activates tab).

-- | Parameters of the 'Page.bringToFront' command.
data PPageBringToFront = PPageBringToFront
instance ToJSON PPageBringToFront where toJSON _ = A.Null

instance Command PPageBringToFront where
   type CommandResponse PPageBringToFront = ()
   commandName _ = "Page.bringToFront"
   fromJSON = const . A.Success . const ()


-- | Page.captureScreenshot
--   Capture page screenshot.

-- | Parameters of the 'Page.captureScreenshot' command.
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


-- | Return type of the 'Page.captureScreenshot' command.
data PageCaptureScreenshot = PageCaptureScreenshot {
  -- | Base64-encoded image data. (Encoded as a base64 string when passed over JSON)
  pageCaptureScreenshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PPageCaptureScreenshot where
   type CommandResponse PPageCaptureScreenshot = PageCaptureScreenshot
   commandName _ = "Page.captureScreenshot"



-- | Page.captureSnapshot
--   Returns a snapshot of the page as a string. For MHTML format, the serialization includes
--   iframes, shadow DOM, external resources, and element-inline styles.

-- | Parameters of the 'Page.captureSnapshot' command.
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


-- | Return type of the 'Page.captureSnapshot' command.
data PageCaptureSnapshot = PageCaptureSnapshot {
  -- | Serialized page data.
  pageCaptureSnapshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PPageCaptureSnapshot where
   type CommandResponse PPageCaptureSnapshot = PageCaptureSnapshot
   commandName _ = "Page.captureSnapshot"



-- | Page.createIsolatedWorld
--   Creates an isolated world for the given frame.

-- | Parameters of the 'Page.createIsolatedWorld' command.
data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
  -- | Id of the frame in which the isolated world should be created.
  pPageCreateIsolatedWorldFrameId :: PageFrameId,
  -- | An optional name which is reported in the Execution Context.
  pPageCreateIsolatedWorldWorldName :: Maybe String,
  -- | Whether or not universal access should be granted to the isolated world. This is a powerful
  --   option, use with caution.
  pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCreateIsolatedWorld  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Return type of the 'Page.createIsolatedWorld' command.
data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
  -- | Execution context of the isolated world.
  pageCreateIsolatedWorldExecutionContextId :: Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PPageCreateIsolatedWorld where
   type CommandResponse PPageCreateIsolatedWorld = PageCreateIsolatedWorld
   commandName _ = "Page.createIsolatedWorld"



-- | Page.disable
--   Disables page domain notifications.

-- | Parameters of the 'Page.disable' command.
data PPageDisable = PPageDisable
instance ToJSON PPageDisable where toJSON _ = A.Null

instance Command PPageDisable where
   type CommandResponse PPageDisable = ()
   commandName _ = "Page.disable"
   fromJSON = const . A.Success . const ()


-- | Page.enable
--   Enables page domain notifications.

-- | Parameters of the 'Page.enable' command.
data PPageEnable = PPageEnable
instance ToJSON PPageEnable where toJSON _ = A.Null

instance Command PPageEnable where
   type CommandResponse PPageEnable = ()
   commandName _ = "Page.enable"
   fromJSON = const . A.Success . const ()


-- | Page.getAppManifest

-- | Parameters of the 'Page.getAppManifest' command.
data PPageGetAppManifest = PPageGetAppManifest
instance ToJSON PPageGetAppManifest where toJSON _ = A.Null

-- | Return type of the 'Page.getAppManifest' command.
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

instance Command PPageGetAppManifest where
   type CommandResponse PPageGetAppManifest = PageGetAppManifest
   commandName _ = "Page.getAppManifest"



-- | Page.getInstallabilityErrors

-- | Parameters of the 'Page.getInstallabilityErrors' command.
data PPageGetInstallabilityErrors = PPageGetInstallabilityErrors
instance ToJSON PPageGetInstallabilityErrors where toJSON _ = A.Null

-- | Return type of the 'Page.getInstallabilityErrors' command.
data PageGetInstallabilityErrors = PageGetInstallabilityErrors {
  pageGetInstallabilityErrorsInstallabilityErrors :: [PageInstallabilityError]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetInstallabilityErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PPageGetInstallabilityErrors where
   type CommandResponse PPageGetInstallabilityErrors = PageGetInstallabilityErrors
   commandName _ = "Page.getInstallabilityErrors"



-- | Page.getManifestIcons

-- | Parameters of the 'Page.getManifestIcons' command.
data PPageGetManifestIcons = PPageGetManifestIcons
instance ToJSON PPageGetManifestIcons where toJSON _ = A.Null

-- | Return type of the 'Page.getManifestIcons' command.
data PageGetManifestIcons = PageGetManifestIcons {
  pageGetManifestIconsPrimaryIcon :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetManifestIcons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PPageGetManifestIcons where
   type CommandResponse PPageGetManifestIcons = PageGetManifestIcons
   commandName _ = "Page.getManifestIcons"



-- | Page.getAppId
--   Returns the unique (PWA) app id.
--   Only returns values if the feature flag 'WebAppEnableManifestId' is enabled

-- | Parameters of the 'Page.getAppId' command.
data PPageGetAppId = PPageGetAppId
instance ToJSON PPageGetAppId where toJSON _ = A.Null

-- | Return type of the 'Page.getAppId' command.
data PageGetAppId = PageGetAppId {
  -- | App id, either from manifest's id attribute or computed from start_url
  pageGetAppIdAppId :: Maybe String,
  -- | Recommendation for manifest's id attribute to match current id computed from start_url
  pageGetAppIdRecommendedId :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PPageGetAppId where
   type CommandResponse PPageGetAppId = PageGetAppId
   commandName _ = "Page.getAppId"



-- | Page.getFrameTree
--   Returns present frame tree structure.

-- | Parameters of the 'Page.getFrameTree' command.
data PPageGetFrameTree = PPageGetFrameTree
instance ToJSON PPageGetFrameTree where toJSON _ = A.Null

-- | Return type of the 'Page.getFrameTree' command.
data PageGetFrameTree = PageGetFrameTree {
  -- | Present frame tree structure.
  pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PPageGetFrameTree where
   type CommandResponse PPageGetFrameTree = PageGetFrameTree
   commandName _ = "Page.getFrameTree"



-- | Page.getLayoutMetrics
--   Returns metrics relating to the layouting of the page, such as viewport bounds/scale.

-- | Parameters of the 'Page.getLayoutMetrics' command.
data PPageGetLayoutMetrics = PPageGetLayoutMetrics
instance ToJSON PPageGetLayoutMetrics where toJSON _ = A.Null

-- | Return type of the 'Page.getLayoutMetrics' command.
data PageGetLayoutMetrics = PageGetLayoutMetrics {
  -- | Metrics relating to the layout viewport in CSS pixels.
  pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
  -- | Metrics relating to the visual viewport in CSS pixels.
  pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
  -- | Size of scrollable area in CSS pixels.
  pageGetLayoutMetricsCssContentSize :: DOMRect
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetLayoutMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PPageGetLayoutMetrics where
   type CommandResponse PPageGetLayoutMetrics = PageGetLayoutMetrics
   commandName _ = "Page.getLayoutMetrics"



-- | Page.getNavigationHistory
--   Returns navigation history for the current page.

-- | Parameters of the 'Page.getNavigationHistory' command.
data PPageGetNavigationHistory = PPageGetNavigationHistory
instance ToJSON PPageGetNavigationHistory where toJSON _ = A.Null

-- | Return type of the 'Page.getNavigationHistory' command.
data PageGetNavigationHistory = PageGetNavigationHistory {
  -- | Index of the current navigation history entry.
  pageGetNavigationHistoryCurrentIndex :: Int,
  -- | Array of navigation history entries.
  pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetNavigationHistory where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PPageGetNavigationHistory where
   type CommandResponse PPageGetNavigationHistory = PageGetNavigationHistory
   commandName _ = "Page.getNavigationHistory"



-- | Page.resetNavigationHistory
--   Resets navigation history for the current page.

-- | Parameters of the 'Page.resetNavigationHistory' command.
data PPageResetNavigationHistory = PPageResetNavigationHistory
instance ToJSON PPageResetNavigationHistory where toJSON _ = A.Null

instance Command PPageResetNavigationHistory where
   type CommandResponse PPageResetNavigationHistory = ()
   commandName _ = "Page.resetNavigationHistory"
   fromJSON = const . A.Success . const ()


-- | Page.getResourceContent
--   Returns content of the given resource.

-- | Parameters of the 'Page.getResourceContent' command.
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


-- | Return type of the 'Page.getResourceContent' command.
data PageGetResourceContent = PageGetResourceContent {
  -- | Resource content.
  pageGetResourceContentContent :: String,
  -- | True, if content was served as base64.
  pageGetResourceContentBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PPageGetResourceContent where
   type CommandResponse PPageGetResourceContent = PageGetResourceContent
   commandName _ = "Page.getResourceContent"



-- | Page.getResourceTree
--   Returns present frame / resource tree structure.

-- | Parameters of the 'Page.getResourceTree' command.
data PPageGetResourceTree = PPageGetResourceTree
instance ToJSON PPageGetResourceTree where toJSON _ = A.Null

-- | Return type of the 'Page.getResourceTree' command.
data PageGetResourceTree = PageGetResourceTree {
  -- | Present frame / resource tree structure.
  pageGetResourceTreeFrameTree :: PageFrameResourceTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PPageGetResourceTree where
   type CommandResponse PPageGetResourceTree = PageGetResourceTree
   commandName _ = "Page.getResourceTree"



-- | Page.handleJavaScriptDialog
--   Accepts or dismisses a JavaScript initiated dialog (alert, confirm, prompt, or onbeforeunload).

-- | Parameters of the 'Page.handleJavaScriptDialog' command.
data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
  -- | Whether to accept or dismiss the dialog.
  pPageHandleJavaScriptDialogAccept :: Bool,
  -- | The text to enter into the dialog prompt before accepting. Used only if this is a prompt
  --   dialog.
  pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageHandleJavaScriptDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageHandleJavaScriptDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PPageHandleJavaScriptDialog where
   type CommandResponse PPageHandleJavaScriptDialog = ()
   commandName _ = "Page.handleJavaScriptDialog"
   fromJSON = const . A.Success . const ()


-- | Page.navigate
--   Navigates current page to the given URL.

-- | Parameters of the 'Page.navigate' command.
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


-- | Return type of the 'Page.navigate' command.
data PageNavigate = PageNavigate {
  -- | Frame id that has navigated (or failed to navigate)
  pageNavigateFrameId :: PageFrameId,
  -- | Loader identifier. This is omitted in case of same-document navigation,
  --   as the previously committed loaderId would not change.
  pageNavigateLoaderId :: Maybe NetworkLoaderId,
  -- | User friendly error message, present if and only if navigation has failed.
  pageNavigateErrorText :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PPageNavigate where
   type CommandResponse PPageNavigate = PageNavigate
   commandName _ = "Page.navigate"



-- | Page.navigateToHistoryEntry
--   Navigates current page to the given history entry.

-- | Parameters of the 'Page.navigateToHistoryEntry' command.
data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
  -- | Unique id of the entry to navigate to.
  pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigateToHistoryEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageNavigateToHistoryEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Command PPageNavigateToHistoryEntry where
   type CommandResponse PPageNavigateToHistoryEntry = ()
   commandName _ = "Page.navigateToHistoryEntry"
   fromJSON = const . A.Success . const ()


-- | Page.printToPDF
--   Print page as PDF.

-- | Parameters of the 'Page.printToPDF' command.
data PPagePrintToPDFTransferMode = PPagePrintToPDFTransferModeReturnAsBase64 | PPagePrintToPDFTransferModeReturnAsStream
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPagePrintToPDFTransferMode where
   parseJSON = A.withText  "PPagePrintToPDFTransferMode"  $ \v -> do
      case v of
         "ReturnAsBase64" -> pure PPagePrintToPDFTransferModeReturnAsBase64
         "ReturnAsStream" -> pure PPagePrintToPDFTransferModeReturnAsStream
         _ -> fail "failed to parse PPagePrintToPDFTransferMode"

instance ToJSON PPagePrintToPDFTransferMode where
   toJSON v = A.String $
      case v of
         PPagePrintToPDFTransferModeReturnAsBase64 -> "ReturnAsBase64"
         PPagePrintToPDFTransferModeReturnAsStream -> "ReturnAsStream"



data PPagePrintToPDF = PPagePrintToPDF {
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
  pPagePrintToPDFPageRanges :: Maybe String,
  -- | HTML template for the print header. Should be valid HTML markup with following
  --   classes used to inject printing values into them:
  --   - `date`: formatted print date
  --   - `title`: document title
  --   - `url`: document location
  --   - `pageNumber`: current page number
  --   - `totalPages`: total pages in the document
  --   
  --   For example, `<span class=title></span>` would generate span containing the title.
  pPagePrintToPDFHeaderTemplate :: Maybe String,
  -- | HTML template for the print footer. Should use the same format as the `headerTemplate`.
  pPagePrintToPDFFooterTemplate :: Maybe String,
  -- | Whether or not to prefer page size as defined by css. Defaults to false,
  --   in which case the content will be scaled to fit the paper size.
  pPagePrintToPDFPreferCSSPageSize :: Maybe Bool,
  -- | return as stream
  pPagePrintToPDFTransferMode :: PPagePrintToPDFTransferMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPDF  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPDF where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Return type of the 'Page.printToPDF' command.
data PagePrintToPDF = PagePrintToPDF {
  -- | Base64-encoded pdf data. Empty if |returnAsStream| is specified. (Encoded as a base64 string when passed over JSON)
  pagePrintToPDFData :: String,
  -- | A handle of the stream that holds resulting PDF data.
  pagePrintToPDFStream :: Maybe IO.IOStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PagePrintToPDF where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PPagePrintToPDF where
   type CommandResponse PPagePrintToPDF = PagePrintToPDF
   commandName _ = "Page.printToPDF"



-- | Page.reload
--   Reloads given page optionally ignoring the cache.

-- | Parameters of the 'Page.reload' command.
data PPageReload = PPageReload {
  -- | If true, browser cache is ignored (as if the user pressed Shift+refresh).
  pPageReloadIgnoreCache :: Maybe Bool,
  -- | If set, the script will be injected into all frames of the inspected page after reload.
  --   Argument will be ignored if reloading dataURL origin.
  pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageReload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PPageReload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


instance Command PPageReload where
   type CommandResponse PPageReload = ()
   commandName _ = "Page.reload"
   fromJSON = const . A.Success . const ()


-- | Page.removeScriptToEvaluateOnNewDocument
--   Removes given script from the list.

-- | Parameters of the 'Page.removeScriptToEvaluateOnNewDocument' command.
data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
  pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


instance Command PPageRemoveScriptToEvaluateOnNewDocument where
   type CommandResponse PPageRemoveScriptToEvaluateOnNewDocument = ()
   commandName _ = "Page.removeScriptToEvaluateOnNewDocument"
   fromJSON = const . A.Success . const ()


-- | Page.screencastFrameAck
--   Acknowledges that a screencast frame has been received by the frontend.

-- | Parameters of the 'Page.screencastFrameAck' command.
data PPageScreencastFrameAck = PPageScreencastFrameAck {
  -- | Frame number.
  pPageScreencastFrameAckSessionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageScreencastFrameAck  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageScreencastFrameAck where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Command PPageScreencastFrameAck where
   type CommandResponse PPageScreencastFrameAck = ()
   commandName _ = "Page.screencastFrameAck"
   fromJSON = const . A.Success . const ()


-- | Page.searchInResource
--   Searches for given string in resource content.

-- | Parameters of the 'Page.searchInResource' command.
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


-- | Return type of the 'Page.searchInResource' command.
data PageSearchInResource = PageSearchInResource {
  -- | List of search matches.
  pageSearchInResourceResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PPageSearchInResource where
   type CommandResponse PPageSearchInResource = PageSearchInResource
   commandName _ = "Page.searchInResource"



-- | Page.setAdBlockingEnabled
--   Enable Chrome's experimental ad filter on all sites.

-- | Parameters of the 'Page.setAdBlockingEnabled' command.
data PPageSetAdBlockingEnabled = PPageSetAdBlockingEnabled {
  -- | Whether to block ads.
  pPageSetAdBlockingEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetAdBlockingEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetAdBlockingEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PPageSetAdBlockingEnabled where
   type CommandResponse PPageSetAdBlockingEnabled = ()
   commandName _ = "Page.setAdBlockingEnabled"
   fromJSON = const . A.Success . const ()


-- | Page.setBypassCSP
--   Enable page Content Security Policy by-passing.

-- | Parameters of the 'Page.setBypassCSP' command.
data PPageSetBypassCSP = PPageSetBypassCSP {
  -- | Whether to bypass page CSP.
  pPageSetBypassCSPEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetBypassCSP  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetBypassCSP where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PPageSetBypassCSP where
   type CommandResponse PPageSetBypassCSP = ()
   commandName _ = "Page.setBypassCSP"
   fromJSON = const . A.Success . const ()


-- | Page.getPermissionsPolicyState
--   Get Permissions Policy state on given frame.

-- | Parameters of the 'Page.getPermissionsPolicyState' command.
data PPageGetPermissionsPolicyState = PPageGetPermissionsPolicyState {
  pPageGetPermissionsPolicyStateFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetPermissionsPolicyState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Return type of the 'Page.getPermissionsPolicyState' command.
data PageGetPermissionsPolicyState = PageGetPermissionsPolicyState {
  pageGetPermissionsPolicyStateStates :: [PagePermissionsPolicyFeatureState]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PPageGetPermissionsPolicyState where
   type CommandResponse PPageGetPermissionsPolicyState = PageGetPermissionsPolicyState
   commandName _ = "Page.getPermissionsPolicyState"



-- | Page.getOriginTrials
--   Get Origin Trials on given frame.

-- | Parameters of the 'Page.getOriginTrials' command.
data PPageGetOriginTrials = PPageGetOriginTrials {
  pPageGetOriginTrialsFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetOriginTrials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Return type of the 'Page.getOriginTrials' command.
data PageGetOriginTrials = PageGetOriginTrials {
  pageGetOriginTrialsOriginTrials :: [PageOriginTrial]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PPageGetOriginTrials where
   type CommandResponse PPageGetOriginTrials = PageGetOriginTrials
   commandName _ = "Page.getOriginTrials"



-- | Page.setFontFamilies
--   Set generic font families.

-- | Parameters of the 'Page.setFontFamilies' command.
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


instance Command PPageSetFontFamilies where
   type CommandResponse PPageSetFontFamilies = ()
   commandName _ = "Page.setFontFamilies"
   fromJSON = const . A.Success . const ()


-- | Page.setFontSizes
--   Set default font sizes.

-- | Parameters of the 'Page.setFontSizes' command.
data PPageSetFontSizes = PPageSetFontSizes {
  -- | Specifies font sizes to set. If a font size is not specified, it won't be changed.
  pPageSetFontSizesFontSizes :: PageFontSizes
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PPageSetFontSizes where
   type CommandResponse PPageSetFontSizes = ()
   commandName _ = "Page.setFontSizes"
   fromJSON = const . A.Success . const ()


-- | Page.setDocumentContent
--   Sets given markup as the document's HTML.

-- | Parameters of the 'Page.setDocumentContent' command.
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


instance Command PPageSetDocumentContent where
   type CommandResponse PPageSetDocumentContent = ()
   commandName _ = "Page.setDocumentContent"
   fromJSON = const . A.Success . const ()


-- | Page.setLifecycleEventsEnabled
--   Controls whether page will emit lifecycle events.

-- | Parameters of the 'Page.setLifecycleEventsEnabled' command.
data PPageSetLifecycleEventsEnabled = PPageSetLifecycleEventsEnabled {
  -- | If true, starts emitting lifecycle events.
  pPageSetLifecycleEventsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetLifecycleEventsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageSetLifecycleEventsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


instance Command PPageSetLifecycleEventsEnabled where
   type CommandResponse PPageSetLifecycleEventsEnabled = ()
   commandName _ = "Page.setLifecycleEventsEnabled"
   fromJSON = const . A.Success . const ()


-- | Page.startScreencast
--   Starts sending each frame using the `screencastFrame` event.

-- | Parameters of the 'Page.startScreencast' command.
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


instance Command PPageStartScreencast where
   type CommandResponse PPageStartScreencast = ()
   commandName _ = "Page.startScreencast"
   fromJSON = const . A.Success . const ()


-- | Page.stopLoading
--   Force the page stop all navigations and pending resource fetches.

-- | Parameters of the 'Page.stopLoading' command.
data PPageStopLoading = PPageStopLoading
instance ToJSON PPageStopLoading where toJSON _ = A.Null

instance Command PPageStopLoading where
   type CommandResponse PPageStopLoading = ()
   commandName _ = "Page.stopLoading"
   fromJSON = const . A.Success . const ()


-- | Page.crash
--   Crashes renderer on the IO thread, generates minidumps.

-- | Parameters of the 'Page.crash' command.
data PPageCrash = PPageCrash
instance ToJSON PPageCrash where toJSON _ = A.Null

instance Command PPageCrash where
   type CommandResponse PPageCrash = ()
   commandName _ = "Page.crash"
   fromJSON = const . A.Success . const ()


-- | Page.close
--   Tries to close page, running its beforeunload hooks, if any.

-- | Parameters of the 'Page.close' command.
data PPageClose = PPageClose
instance ToJSON PPageClose where toJSON _ = A.Null

instance Command PPageClose where
   type CommandResponse PPageClose = ()
   commandName _ = "Page.close"
   fromJSON = const . A.Success . const ()


-- | Page.setWebLifecycleState
--   Tries to update the web lifecycle state of the page.
--   It will transition the page to the given state according to:
--   https://github.com/WICG/web-lifecycle/

-- | Parameters of the 'Page.setWebLifecycleState' command.
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


instance Command PPageSetWebLifecycleState where
   type CommandResponse PPageSetWebLifecycleState = ()
   commandName _ = "Page.setWebLifecycleState"
   fromJSON = const . A.Success . const ()


-- | Page.stopScreencast
--   Stops sending each frame in the `screencastFrame`.

-- | Parameters of the 'Page.stopScreencast' command.
data PPageStopScreencast = PPageStopScreencast
instance ToJSON PPageStopScreencast where toJSON _ = A.Null

instance Command PPageStopScreencast where
   type CommandResponse PPageStopScreencast = ()
   commandName _ = "Page.stopScreencast"
   fromJSON = const . A.Success . const ()


-- | Page.produceCompilationCache
--   Requests backend to produce compilation cache for the specified scripts.
--   `scripts` are appeneded to the list of scripts for which the cache
--   would be produced. The list may be reset during page navigation.
--   When script with a matching URL is encountered, the cache is optionally
--   produced upon backend discretion, based on internal heuristics.
--   See also: `Page.compilationCacheProduced`.

-- | Parameters of the 'Page.produceCompilationCache' command.
data PPageProduceCompilationCache = PPageProduceCompilationCache {
  pPageProduceCompilationCacheScripts :: [PageCompilationCacheParams]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageProduceCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PPageProduceCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


instance Command PPageProduceCompilationCache where
   type CommandResponse PPageProduceCompilationCache = ()
   commandName _ = "Page.produceCompilationCache"
   fromJSON = const . A.Success . const ()


-- | Page.addCompilationCache
--   Seeds compilation cache for given url. Compilation cache does not survive
--   cross-process navigation.

-- | Parameters of the 'Page.addCompilationCache' command.
data PPageAddCompilationCache = PPageAddCompilationCache {
  pPageAddCompilationCacheUrl :: String,
  -- | Base64-encoded data (Encoded as a base64 string when passed over JSON)
  pPageAddCompilationCacheData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageAddCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PPageAddCompilationCache where
   type CommandResponse PPageAddCompilationCache = ()
   commandName _ = "Page.addCompilationCache"
   fromJSON = const . A.Success . const ()


-- | Page.clearCompilationCache
--   Clears seeded compilation cache.

-- | Parameters of the 'Page.clearCompilationCache' command.
data PPageClearCompilationCache = PPageClearCompilationCache
instance ToJSON PPageClearCompilationCache where toJSON _ = A.Null

instance Command PPageClearCompilationCache where
   type CommandResponse PPageClearCompilationCache = ()
   commandName _ = "Page.clearCompilationCache"
   fromJSON = const . A.Success . const ()


-- | Page.setSPCTransactionMode
--   Sets the Secure Payment Confirmation transaction mode.
--   https://w3c.github.io/secure-payment-confirmation/#sctn-automation-set-spc-transaction-mode

-- | Parameters of the 'Page.setSPCTransactionMode' command.
data PPageSetSPCTransactionModeMode = PPageSetSPCTransactionModeModeNone | PPageSetSPCTransactionModeModeAutoaccept | PPageSetSPCTransactionModeModeAutoreject
   deriving (Ord, Eq, Show, Read)
instance FromJSON PPageSetSPCTransactionModeMode where
   parseJSON = A.withText  "PPageSetSPCTransactionModeMode"  $ \v -> do
      case v of
         "none" -> pure PPageSetSPCTransactionModeModeNone
         "autoaccept" -> pure PPageSetSPCTransactionModeModeAutoaccept
         "autoreject" -> pure PPageSetSPCTransactionModeModeAutoreject
         _ -> fail "failed to parse PPageSetSPCTransactionModeMode"

instance ToJSON PPageSetSPCTransactionModeMode where
   toJSON v = A.String $
      case v of
         PPageSetSPCTransactionModeModeNone -> "none"
         PPageSetSPCTransactionModeModeAutoaccept -> "autoaccept"
         PPageSetSPCTransactionModeModeAutoreject -> "autoreject"



data PPageSetSPCTransactionMode = PPageSetSPCTransactionMode {
  pPageSetSPCTransactionModeMode :: PPageSetSPCTransactionModeMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetSPCTransactionMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PPageSetSPCTransactionMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PPageSetSPCTransactionMode where
   type CommandResponse PPageSetSPCTransactionMode = ()
   commandName _ = "Page.setSPCTransactionMode"
   fromJSON = const . A.Success . const ()


-- | Page.generateTestReport
--   Generates a report for testing.

-- | Parameters of the 'Page.generateTestReport' command.
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


instance Command PPageGenerateTestReport where
   type CommandResponse PPageGenerateTestReport = ()
   commandName _ = "Page.generateTestReport"
   fromJSON = const . A.Success . const ()


-- | Page.waitForDebugger
--   Pauses page execution. Can be resumed using generic Runtime.runIfWaitingForDebugger.

-- | Parameters of the 'Page.waitForDebugger' command.
data PPageWaitForDebugger = PPageWaitForDebugger
instance ToJSON PPageWaitForDebugger where toJSON _ = A.Null

instance Command PPageWaitForDebugger where
   type CommandResponse PPageWaitForDebugger = ()
   commandName _ = "Page.waitForDebugger"
   fromJSON = const . A.Success . const ()


-- | Page.setInterceptFileChooserDialog
--   Intercept file chooser requests and transfer control to protocol clients.
--   When file chooser interception is enabled, native file chooser dialog is not shown.
--   Instead, a protocol event `Page.fileChooserOpened` is emitted.

-- | Parameters of the 'Page.setInterceptFileChooserDialog' command.
data PPageSetInterceptFileChooserDialog = PPageSetInterceptFileChooserDialog {
  pPageSetInterceptFileChooserDialogEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetInterceptFileChooserDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PPageSetInterceptFileChooserDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


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



-- | Type 'Security.SecurityState'.
--   The security level of a page or resource.
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



-- | Type 'Security.CertificateSecurityState'.
--   Details about the security state of the page certificate.
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
  securityCertificateSecurityStateModernSSL :: Bool,
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



-- | Type 'Security.SafetyTipStatus'.
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



-- | Type 'Security.SafetyTipInfo'.
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



-- | Type 'Security.VisibleSecurityState'.
--   Security state information about the page.
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



-- | Type 'Security.SecurityStateExplanation'.
--   An explanation of an factor contributing to the security state.
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



-- | Type 'Security.CertificateErrorAction'.
--   The action to take when a certificate error occurs. continue will continue processing the
--   request and cancel will cancel the request.
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


instance Event SecurityVisibleSecurityStateChanged where
    eventName _ = "Security.visibleSecurityStateChanged"



-- | Security.disable
--   Disables tracking security state changes.

-- | Parameters of the 'Security.disable' command.
data PSecurityDisable = PSecurityDisable
instance ToJSON PSecurityDisable where toJSON _ = A.Null

instance Command PSecurityDisable where
   type CommandResponse PSecurityDisable = ()
   commandName _ = "Security.disable"
   fromJSON = const . A.Success . const ()


-- | Security.enable
--   Enables tracking security state changes.

-- | Parameters of the 'Security.enable' command.
data PSecurityEnable = PSecurityEnable
instance ToJSON PSecurityEnable where toJSON _ = A.Null

instance Command PSecurityEnable where
   type CommandResponse PSecurityEnable = ()
   commandName _ = "Security.enable"
   fromJSON = const . A.Success . const ()


-- | Security.setIgnoreCertificateErrors
--   Enable/disable whether all certificate errors should be ignored.

-- | Parameters of the 'Security.setIgnoreCertificateErrors' command.
data PSecuritySetIgnoreCertificateErrors = PSecuritySetIgnoreCertificateErrors {
  -- | If true, all certificate errors will be ignored.
  pSecuritySetIgnoreCertificateErrorsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PSecuritySetIgnoreCertificateErrors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PSecuritySetIgnoreCertificateErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PSecuritySetIgnoreCertificateErrors where
   type CommandResponse PSecuritySetIgnoreCertificateErrors = ()
   commandName _ = "Security.setIgnoreCertificateErrors"
   fromJSON = const . A.Success . const ()



