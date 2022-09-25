{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type DomNodeId = Int
type DomBackendNodeId = Int

data DomBackendNode = DomBackendNode {
   domBackendNodeNodeType :: Int,
   domBackendNodeNodeName :: String,
   domBackendNodeBackendNodeId :: DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBackendNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  DomBackendNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


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



data DomNode = DomNode {
   domNodeNodeId :: DomNodeId,
   domNodeParentId :: Maybe DomNodeId,
   domNodeBackendNodeId :: DomBackendNodeId,
   domNodeNodeType :: Int,
   domNodeNodeName :: String,
   domNodeLocalName :: String,
   domNodeNodeValue :: String,
   domNodeChildNodeCount :: Maybe Int,
   domNodeChildren :: Maybe [DomNode],
   domNodeAttributes :: Maybe [String],
   domNodeDocumentUrl :: Maybe String,
   domNodeBaseUrl :: Maybe String,
   domNodePublicId :: Maybe String,
   domNodeSystemId :: Maybe String,
   domNodeInternalSubset :: Maybe String,
   domNodeXmlVersion :: Maybe String,
   domNodeName :: Maybe String,
   domNodeValue :: Maybe String,
   domNodePseudoType :: Maybe DomPseudoType,
   domNodeShadowRootType :: Maybe DomShadowRootType,
   domNodeFrameId :: Maybe PageFrameId,
   domNodeContentDocument :: Maybe DomNode,
   domNodeShadowRoots :: Maybe [DomNode],
   domNodeTemplateContent :: Maybe DomNode,
   domNodePseudoElements :: Maybe [DomNode],
   domNodeDistributedNodes :: Maybe [DomBackendNode],
   domNodeIsSvg :: Maybe Bool,
   domNodeCompatibilityMode :: Maybe DomCompatibilityMode,
   domNodeAssignedSlot :: Maybe DomBackendNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



data DomRgba = DomRgba {
   domRgbaR :: Int,
   domRgbaG :: Int,
   domRgbaB :: Int,
   domRgbaA :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRgba  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRgba where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }


type DomQuad = [Double]

data DomBoxModel = DomBoxModel {
   domBoxModelContent :: DomQuad,
   domBoxModelPadding :: DomQuad,
   domBoxModelBorder :: DomQuad,
   domBoxModelMargin :: DomQuad,
   domBoxModelWidth :: Int,
   domBoxModelHeight :: Int,
   domBoxModelShapeOutside :: Maybe DomShapeOutsideInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  DomBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data DomShapeOutsideInfo = DomShapeOutsideInfo {
   domShapeOutsideInfoBounds :: DomQuad,
   domShapeOutsideInfoShape :: [Int],
   domShapeOutsideInfoMarginShape :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShapeOutsideInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShapeOutsideInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data DomRect = DomRect {
   domRectX :: Double,
   domRectY :: Double,
   domRectWidth :: Double,
   domRectHeight :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 , A.omitNothingFields = True}

instance FromJSON  DomRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 7 }



data DomCssComputedStyleProperty = DomCssComputedStyleProperty {
   domCssComputedStylePropertyName :: String,
   domCssComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  DomCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





data DomAttributeModified = DomAttributeModified {
   domAttributeModifiedNodeId :: DomNodeId,
   domAttributeModifiedName :: String,
   domAttributeModifiedValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomAttributeModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data DomAttributeRemoved = DomAttributeRemoved {
   domAttributeRemovedNodeId :: DomNodeId,
   domAttributeRemovedName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomAttributeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomAttributeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data DomCharacterDataModified = DomCharacterDataModified {
   domCharacterDataModifiedNodeId :: DomNodeId,
   domCharacterDataModifiedCharacterData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomCharacterDataModified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomCharacterDataModified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data DomChildNodeCountUpdated = DomChildNodeCountUpdated {
   domChildNodeCountUpdatedNodeId :: DomNodeId,
   domChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeCountUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeCountUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data DomChildNodeInserted = DomChildNodeInserted {
   domChildNodeInsertedParentNodeId :: DomNodeId,
   domChildNodeInsertedPreviousNodeId :: DomNodeId,
   domChildNodeInsertedNode :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeInserted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeInserted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data DomChildNodeRemoved = DomChildNodeRemoved {
   domChildNodeRemovedParentNodeId :: DomNodeId,
   domChildNodeRemovedNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomChildNodeRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomChildNodeRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data DomDistributedNodesUpdated = DomDistributedNodesUpdated {
   domDistributedNodesUpdatedInsertionPointId :: DomNodeId,
   domDistributedNodesUpdatedDistributedNodes :: [DomBackendNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomDistributedNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  DomDistributedNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


data DomDocumentUpdated = DomDocumentUpdated
   deriving (Eq, Show, Read)
instance FromJSON DomDocumentUpdated where
   parseJSON = A.withText  "DomDocumentUpdated"  $ \v -> do
      case v of
         "DomDocumentUpdated" -> pure DomDocumentUpdated
         _ -> fail "failed to parse DomDocumentUpdated"



data DomInlineStyleInvalidated = DomInlineStyleInvalidated {
   domInlineStyleInvalidatedNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomInlineStyleInvalidated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  DomInlineStyleInvalidated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data DomPseudoElementAdded = DomPseudoElementAdded {
   domPseudoElementAddedParentId :: DomNodeId,
   domPseudoElementAddedPseudoElement :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data DomPseudoElementRemoved = DomPseudoElementRemoved {
   domPseudoElementRemovedParentId :: DomNodeId,
   domPseudoElementRemovedPseudoElementId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomPseudoElementRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  DomPseudoElementRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data DomSetChildNodes = DomSetChildNodes {
   domSetChildNodesParentId :: DomNodeId,
   domSetChildNodesNodes :: [DomNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DomSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data DomShadowRootPopped = DomShadowRootPopped {
   domShadowRootPoppedHostId :: DomNodeId,
   domShadowRootPoppedRootId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPopped  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPopped where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data DomShadowRootPushed = DomShadowRootPushed {
   domShadowRootPushedHostId :: DomNodeId,
   domShadowRootPushedRoot :: DomNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomShadowRootPushed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  DomShadowRootPushed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





data PDomCollectClassNamesFromSubtree = PDomCollectClassNamesFromSubtree {
   pDomCollectClassNamesFromSubtreeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCollectClassNamesFromSubtree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PDomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


domCollectClassNamesFromSubtree :: Handle ev -> PDomCollectClassNamesFromSubtree -> IO (Either Error DomCollectClassNamesFromSubtree)
domCollectClassNamesFromSubtree handle params = sendReceiveCommandResult handle "DOM.collectClassNamesFromSubtree" (Just params)

data DomCollectClassNamesFromSubtree = DomCollectClassNamesFromSubtree {
   domCollectClassNamesFromSubtreeClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCollectClassNamesFromSubtree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command DomCollectClassNamesFromSubtree where
   commandName _ = "DOM.collectClassNamesFromSubtree"




data PDomCopyTo = PDomCopyTo {
   pDomCopyToNodeId :: DomNodeId,
   pDomCopyToTargetNodeId :: DomNodeId,
   pDomCopyToInsertBeforeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomCopyTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


domCopyTo :: Handle ev -> PDomCopyTo -> IO (Either Error DomCopyTo)
domCopyTo handle params = sendReceiveCommandResult handle "DOM.copyTo" (Just params)

data DomCopyTo = DomCopyTo {
   domCopyToNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomCopyTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomCopyTo where
   commandName _ = "DOM.copyTo"




data PDomDescribeNode = PDomDescribeNode {
   pDomDescribeNodeNodeId :: Maybe DomNodeId,
   pDomDescribeNodeBackendNodeId :: Maybe DomBackendNodeId,
   pDomDescribeNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
   pDomDescribeNodeDepth :: Maybe Int,
   pDomDescribeNodePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDescribeNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domDescribeNode :: Handle ev -> PDomDescribeNode -> IO (Either Error DomDescribeNode)
domDescribeNode handle params = sendReceiveCommandResult handle "DOM.describeNode" (Just params)

data DomDescribeNode = DomDescribeNode {
   domDescribeNodeNode :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomDescribeNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomDescribeNode where
   commandName _ = "DOM.describeNode"




data PDomScrollIntoViewIfNeeded = PDomScrollIntoViewIfNeeded {
   pDomScrollIntoViewIfNeededNodeId :: Maybe DomNodeId,
   pDomScrollIntoViewIfNeededBackendNodeId :: Maybe DomBackendNodeId,
   pDomScrollIntoViewIfNeededObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
   pDomScrollIntoViewIfNeededRect :: Maybe DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomScrollIntoViewIfNeeded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PDomScrollIntoViewIfNeeded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


domScrollIntoViewIfNeeded :: Handle ev -> PDomScrollIntoViewIfNeeded -> IO (Maybe Error)
domScrollIntoViewIfNeeded handle params = sendReceiveCommand handle "DOM.scrollIntoViewIfNeeded" (Just params)


domDisable :: Handle ev -> IO (Maybe Error)
domDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())



data PDomDiscardSearchResults = PDomDiscardSearchResults {
   pDomDiscardSearchResultsSearchId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomDiscardSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PDomDiscardSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


domDiscardSearchResults :: Handle ev -> PDomDiscardSearchResults -> IO (Maybe Error)
domDiscardSearchResults handle params = sendReceiveCommand handle "DOM.discardSearchResults" (Just params)


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
   pDomEnableIncludeWhitespace :: PDomEnableIncludeWhitespace
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


domEnable :: Handle ev -> PDomEnable -> IO (Maybe Error)
domEnable handle params = sendReceiveCommand handle "DOM.enable" (Just params)



data PDomFocus = PDomFocus {
   pDomFocusNodeId :: Maybe DomNodeId,
   pDomFocusBackendNodeId :: Maybe DomBackendNodeId,
   pDomFocusObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomFocus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PDomFocus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }


domFocus :: Handle ev -> PDomFocus -> IO (Maybe Error)
domFocus handle params = sendReceiveCommand handle "DOM.focus" (Just params)



data PDomGetAttributes = PDomGetAttributes {
   pDomGetAttributesNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetAttributes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domGetAttributes :: Handle ev -> PDomGetAttributes -> IO (Either Error DomGetAttributes)
domGetAttributes handle params = sendReceiveCommandResult handle "DOM.getAttributes" (Just params)

data DomGetAttributes = DomGetAttributes {
   domGetAttributesAttributes :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetAttributes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetAttributes where
   commandName _ = "DOM.getAttributes"




data PDomGetBoxModel = PDomGetBoxModel {
   pDomGetBoxModelNodeId :: Maybe DomNodeId,
   pDomGetBoxModelBackendNodeId :: Maybe DomBackendNodeId,
   pDomGetBoxModelObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetBoxModel  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domGetBoxModel :: Handle ev -> PDomGetBoxModel -> IO (Either Error DomGetBoxModel)
domGetBoxModel handle params = sendReceiveCommandResult handle "DOM.getBoxModel" (Just params)

data DomGetBoxModel = DomGetBoxModel {
   domGetBoxModelModel :: DomBoxModel
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetBoxModel where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetBoxModel where
   commandName _ = "DOM.getBoxModel"




data PDomGetContentQuads = PDomGetContentQuads {
   pDomGetContentQuadsNodeId :: Maybe DomNodeId,
   pDomGetContentQuadsBackendNodeId :: Maybe DomBackendNodeId,
   pDomGetContentQuadsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContentQuads  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


domGetContentQuads :: Handle ev -> PDomGetContentQuads -> IO (Either Error DomGetContentQuads)
domGetContentQuads handle params = sendReceiveCommandResult handle "DOM.getContentQuads" (Just params)

data DomGetContentQuads = DomGetContentQuads {
   domGetContentQuadsQuads :: [DomQuad]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContentQuads where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command DomGetContentQuads where
   commandName _ = "DOM.getContentQuads"




data PDomGetDocument = PDomGetDocument {
   pDomGetDocumentDepth :: Maybe Int,
   pDomGetDocumentPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domGetDocument :: Handle ev -> PDomGetDocument -> IO (Either Error DomGetDocument)
domGetDocument handle params = sendReceiveCommandResult handle "DOM.getDocument" (Just params)

data DomGetDocument = DomGetDocument {
   domGetDocumentRoot :: DomNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetDocument where
   commandName _ = "DOM.getDocument"




data PDomGetNodesForSubtreeByStyle = PDomGetNodesForSubtreeByStyle {
   pDomGetNodesForSubtreeByStyleNodeId :: DomNodeId,
   pDomGetNodesForSubtreeByStyleComputedStyles :: [DomCssComputedStyleProperty],
   pDomGetNodesForSubtreeByStylePierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodesForSubtreeByStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


domGetNodesForSubtreeByStyle :: Handle ev -> PDomGetNodesForSubtreeByStyle -> IO (Either Error DomGetNodesForSubtreeByStyle)
domGetNodesForSubtreeByStyle handle params = sendReceiveCommandResult handle "DOM.getNodesForSubtreeByStyle" (Just params)

data DomGetNodesForSubtreeByStyle = DomGetNodesForSubtreeByStyle {
   domGetNodesForSubtreeByStyleNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodesForSubtreeByStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command DomGetNodesForSubtreeByStyle where
   commandName _ = "DOM.getNodesForSubtreeByStyle"




data PDomGetNodeForLocation = PDomGetNodeForLocation {
   pDomGetNodeForLocationX :: Int,
   pDomGetNodeForLocationY :: Int,
   pDomGetNodeForLocationIncludeUserAgentShadowDom :: Maybe Bool,
   pDomGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeForLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


domGetNodeForLocation :: Handle ev -> PDomGetNodeForLocation -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation handle params = sendReceiveCommandResult handle "DOM.getNodeForLocation" (Just params)

data DomGetNodeForLocation = DomGetNodeForLocation {
   domGetNodeForLocationBackendNodeId :: DomBackendNodeId,
   domGetNodeForLocationFrameId :: PageFrameId,
   domGetNodeForLocationNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeForLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeForLocation where
   commandName _ = "DOM.getNodeForLocation"




data PDomGetOuterHtml = PDomGetOuterHtml {
   pDomGetOuterHtmlNodeId :: Maybe DomNodeId,
   pDomGetOuterHtmlBackendNodeId :: Maybe DomBackendNodeId,
   pDomGetOuterHtmlObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domGetOuterHtml :: Handle ev -> PDomGetOuterHtml -> IO (Either Error DomGetOuterHtml)
domGetOuterHtml handle params = sendReceiveCommandResult handle "DOM.getOuterHTML" (Just params)

data DomGetOuterHtml = DomGetOuterHtml {
   domGetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command DomGetOuterHtml where
   commandName _ = "DOM.getOuterHTML"




data PDomGetRelayoutBoundary = PDomGetRelayoutBoundary {
   pDomGetRelayoutBoundaryNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetRelayoutBoundary  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


domGetRelayoutBoundary :: Handle ev -> PDomGetRelayoutBoundary -> IO (Either Error DomGetRelayoutBoundary)
domGetRelayoutBoundary handle params = sendReceiveCommandResult handle "DOM.getRelayoutBoundary" (Just params)

data DomGetRelayoutBoundary = DomGetRelayoutBoundary {
   domGetRelayoutBoundaryNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetRelayoutBoundary where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetRelayoutBoundary where
   commandName _ = "DOM.getRelayoutBoundary"




data PDomGetSearchResults = PDomGetSearchResults {
   pDomGetSearchResultsSearchId :: String,
   pDomGetSearchResultsFromIndex :: Int,
   pDomGetSearchResultsToIndex :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetSearchResults  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


domGetSearchResults :: Handle ev -> PDomGetSearchResults -> IO (Either Error DomGetSearchResults)
domGetSearchResults handle params = sendReceiveCommandResult handle "DOM.getSearchResults" (Just params)

data DomGetSearchResults = DomGetSearchResults {
   domGetSearchResultsNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetSearchResults where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomGetSearchResults where
   commandName _ = "DOM.getSearchResults"



domHideHighlight :: Handle ev -> IO (Maybe Error)
domHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())


domHighlightNode :: Handle ev -> IO (Maybe Error)
domHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())


domHighlightRect :: Handle ev -> IO (Maybe Error)
domHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())


domMarkUndoableState :: Handle ev -> IO (Maybe Error)
domMarkUndoableState handle = sendReceiveCommand handle "DOM.markUndoableState" (Nothing :: Maybe ())



data PDomMoveTo = PDomMoveTo {
   pDomMoveToNodeId :: DomNodeId,
   pDomMoveToTargetNodeId :: DomNodeId,
   pDomMoveToInsertBeforeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomMoveTo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  PDomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }


domMoveTo :: Handle ev -> PDomMoveTo -> IO (Either Error DomMoveTo)
domMoveTo handle params = sendReceiveCommandResult handle "DOM.moveTo" (Just params)

data DomMoveTo = DomMoveTo {
   domMoveToNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomMoveTo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }

instance Command DomMoveTo where
   commandName _ = "DOM.moveTo"




data PDomPerformSearch = PDomPerformSearch {
   pDomPerformSearchQuery :: String,
   pDomPerformSearchIncludeUserAgentShadowDom :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPerformSearch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domPerformSearch :: Handle ev -> PDomPerformSearch -> IO (Either Error DomPerformSearch)
domPerformSearch handle params = sendReceiveCommandResult handle "DOM.performSearch" (Just params)

data DomPerformSearch = DomPerformSearch {
   domPerformSearchSearchId :: String,
   domPerformSearchResultCount :: Int
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPerformSearch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomPerformSearch where
   commandName _ = "DOM.performSearch"




data PDomPushNodeByPathToFrontend = PDomPushNodeByPathToFrontend {
   pDomPushNodeByPathToFrontendPath :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodeByPathToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


domPushNodeByPathToFrontend :: Handle ev -> PDomPushNodeByPathToFrontend -> IO (Either Error DomPushNodeByPathToFrontend)
domPushNodeByPathToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodeByPathToFrontend" (Just params)

data DomPushNodeByPathToFrontend = DomPushNodeByPathToFrontend {
   domPushNodeByPathToFrontendNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodeByPathToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command DomPushNodeByPathToFrontend where
   commandName _ = "DOM.pushNodeByPathToFrontend"




data PDomPushNodesByBackendIdsToFrontend = PDomPushNodesByBackendIdsToFrontend {
   pDomPushNodesByBackendIdsToFrontendBackendNodeIds :: [DomBackendNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomPushNodesByBackendIdsToFrontend  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PDomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


domPushNodesByBackendIdsToFrontend :: Handle ev -> PDomPushNodesByBackendIdsToFrontend -> IO (Either Error DomPushNodesByBackendIdsToFrontend)
domPushNodesByBackendIdsToFrontend handle params = sendReceiveCommandResult handle "DOM.pushNodesByBackendIdsToFrontend" (Just params)

data DomPushNodesByBackendIdsToFrontend = DomPushNodesByBackendIdsToFrontend {
   domPushNodesByBackendIdsToFrontendNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomPushNodesByBackendIdsToFrontend where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command DomPushNodesByBackendIdsToFrontend where
   commandName _ = "DOM.pushNodesByBackendIdsToFrontend"




data PDomQuerySelector = PDomQuerySelector {
   pDomQuerySelectorNodeId :: DomNodeId,
   pDomQuerySelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domQuerySelector :: Handle ev -> PDomQuerySelector -> IO (Either Error DomQuerySelector)
domQuerySelector handle params = sendReceiveCommandResult handle "DOM.querySelector" (Just params)

data DomQuerySelector = DomQuerySelector {
   domQuerySelectorNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomQuerySelector where
   commandName _ = "DOM.querySelector"




data PDomQuerySelectorAll = PDomQuerySelectorAll {
   pDomQuerySelectorAllNodeId :: DomNodeId,
   pDomQuerySelectorAllSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomQuerySelectorAll  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


domQuerySelectorAll :: Handle ev -> PDomQuerySelectorAll -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll handle params = sendReceiveCommandResult handle "DOM.querySelectorAll" (Just params)

data DomQuerySelectorAll = DomQuerySelectorAll {
   domQuerySelectorAllNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomQuerySelectorAll where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command DomQuerySelectorAll where
   commandName _ = "DOM.querySelectorAll"



domRedo :: Handle ev -> IO (Maybe Error)
domRedo handle = sendReceiveCommand handle "DOM.redo" (Nothing :: Maybe ())



data PDomRemoveAttribute = PDomRemoveAttribute {
   pDomRemoveAttributeNodeId :: DomNodeId,
   pDomRemoveAttributeName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveAttribute  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveAttribute where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


domRemoveAttribute :: Handle ev -> PDomRemoveAttribute -> IO (Maybe Error)
domRemoveAttribute handle params = sendReceiveCommand handle "DOM.removeAttribute" (Just params)



data PDomRemoveNode = PDomRemoveNode {
   pDomRemoveNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRemoveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PDomRemoveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


domRemoveNode :: Handle ev -> PDomRemoveNode -> IO (Maybe Error)
domRemoveNode handle params = sendReceiveCommand handle "DOM.removeNode" (Just params)



data PDomRequestChildNodes = PDomRequestChildNodes {
   pDomRequestChildNodesNodeId :: DomNodeId,
   pDomRequestChildNodesDepth :: Maybe Int,
   pDomRequestChildNodesPierce :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomRequestChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domRequestChildNodes :: Handle ev -> PDomRequestChildNodes -> IO (Maybe Error)
domRequestChildNodes handle params = sendReceiveCommand handle "DOM.requestChildNodes" (Just params)



data PDomRequestNode = PDomRequestNode {
   pDomRequestNodeObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomRequestNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domRequestNode :: Handle ev -> PDomRequestNode -> IO (Either Error DomRequestNode)
domRequestNode handle params = sendReceiveCommandResult handle "DOM.requestNode" (Just params)

data DomRequestNode = DomRequestNode {
   domRequestNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomRequestNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomRequestNode where
   commandName _ = "DOM.requestNode"




data PDomResolveNode = PDomResolveNode {
   pDomResolveNodeNodeId :: Maybe DomNodeId,
   pDomResolveNodeBackendNodeId :: Maybe DomBackendNodeId,
   pDomResolveNodeObjectGroup :: Maybe String,
   pDomResolveNodeExecutionContextId :: Maybe Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomResolveNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domResolveNode :: Handle ev -> PDomResolveNode -> IO (Either Error DomResolveNode)
domResolveNode handle params = sendReceiveCommandResult handle "DOM.resolveNode" (Just params)

data DomResolveNode = DomResolveNode {
   domResolveNodeObject :: Runtime.RuntimeRemoteObject
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomResolveNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomResolveNode where
   commandName _ = "DOM.resolveNode"




data PDomSetAttributeValue = PDomSetAttributeValue {
   pDomSetAttributeValueNodeId :: DomNodeId,
   pDomSetAttributeValueName :: String,
   pDomSetAttributeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domSetAttributeValue :: Handle ev -> PDomSetAttributeValue -> IO (Maybe Error)
domSetAttributeValue handle params = sendReceiveCommand handle "DOM.setAttributeValue" (Just params)



data PDomSetAttributesAsText = PDomSetAttributesAsText {
   pDomSetAttributesAsTextNodeId :: DomNodeId,
   pDomSetAttributesAsTextText :: String,
   pDomSetAttributesAsTextName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetAttributesAsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomSetAttributesAsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


domSetAttributesAsText :: Handle ev -> PDomSetAttributesAsText -> IO (Maybe Error)
domSetAttributesAsText handle params = sendReceiveCommand handle "DOM.setAttributesAsText" (Just params)



data PDomSetFileInputFiles = PDomSetFileInputFiles {
   pDomSetFileInputFilesFiles :: [String],
   pDomSetFileInputFilesNodeId :: Maybe DomNodeId,
   pDomSetFileInputFilesBackendNodeId :: Maybe DomBackendNodeId,
   pDomSetFileInputFilesObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetFileInputFiles  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PDomSetFileInputFiles where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


domSetFileInputFiles :: Handle ev -> PDomSetFileInputFiles -> IO (Maybe Error)
domSetFileInputFiles handle params = sendReceiveCommand handle "DOM.setFileInputFiles" (Just params)



data PDomSetNodeStackTracesEnabled = PDomSetNodeStackTracesEnabled {
   pDomSetNodeStackTracesEnabledEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeStackTracesEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeStackTracesEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


domSetNodeStackTracesEnabled :: Handle ev -> PDomSetNodeStackTracesEnabled -> IO (Maybe Error)
domSetNodeStackTracesEnabled handle params = sendReceiveCommand handle "DOM.setNodeStackTracesEnabled" (Just params)



data PDomGetNodeStackTraces = PDomGetNodeStackTraces {
   pDomGetNodeStackTracesNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetNodeStackTraces  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PDomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


domGetNodeStackTraces :: Handle ev -> PDomGetNodeStackTraces -> IO (Either Error DomGetNodeStackTraces)
domGetNodeStackTraces handle params = sendReceiveCommandResult handle "DOM.getNodeStackTraces" (Just params)

data DomGetNodeStackTraces = DomGetNodeStackTraces {
   domGetNodeStackTracesCreation :: Maybe Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetNodeStackTraces where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command DomGetNodeStackTraces where
   commandName _ = "DOM.getNodeStackTraces"




data PDomGetFileInfo = PDomGetFileInfo {
   pDomGetFileInfoObjectId :: Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFileInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domGetFileInfo :: Handle ev -> PDomGetFileInfo -> IO (Either Error DomGetFileInfo)
domGetFileInfo handle params = sendReceiveCommandResult handle "DOM.getFileInfo" (Just params)

data DomGetFileInfo = DomGetFileInfo {
   domGetFileInfoPath :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFileInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomGetFileInfo where
   commandName _ = "DOM.getFileInfo"




data PDomSetInspectedNode = PDomSetInspectedNode {
   pDomSetInspectedNodeNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetInspectedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PDomSetInspectedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


domSetInspectedNode :: Handle ev -> PDomSetInspectedNode -> IO (Maybe Error)
domSetInspectedNode handle params = sendReceiveCommand handle "DOM.setInspectedNode" (Just params)



data PDomSetNodeName = PDomSetNodeName {
   pDomSetNodeNameNodeId :: DomNodeId,
   pDomSetNodeNameName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeName  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


domSetNodeName :: Handle ev -> PDomSetNodeName -> IO (Either Error DomSetNodeName)
domSetNodeName handle params = sendReceiveCommandResult handle "DOM.setNodeName" (Just params)

data DomSetNodeName = DomSetNodeName {
   domSetNodeNameNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomSetNodeName where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command DomSetNodeName where
   commandName _ = "DOM.setNodeName"




data PDomSetNodeValue = PDomSetNodeValue {
   pDomSetNodeValueNodeId :: DomNodeId,
   pDomSetNodeValueValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetNodeValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetNodeValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domSetNodeValue :: Handle ev -> PDomSetNodeValue -> IO (Maybe Error)
domSetNodeValue handle params = sendReceiveCommand handle "DOM.setNodeValue" (Just params)



data PDomSetOuterHtml = PDomSetOuterHtml {
   pDomSetOuterHtmlNodeId :: DomNodeId,
   pDomSetOuterHtmlOuterHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomSetOuterHtml  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PDomSetOuterHtml where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


domSetOuterHtml :: Handle ev -> PDomSetOuterHtml -> IO (Maybe Error)
domSetOuterHtml handle params = sendReceiveCommand handle "DOM.setOuterHTML" (Just params)


domUndo :: Handle ev -> IO (Maybe Error)
domUndo handle = sendReceiveCommand handle "DOM.undo" (Nothing :: Maybe ())



data PDomGetFrameOwner = PDomGetFrameOwner {
   pDomGetFrameOwnerFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetFrameOwner  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PDomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


domGetFrameOwner :: Handle ev -> PDomGetFrameOwner -> IO (Either Error DomGetFrameOwner)
domGetFrameOwner handle params = sendReceiveCommandResult handle "DOM.getFrameOwner" (Just params)

data DomGetFrameOwner = DomGetFrameOwner {
   domGetFrameOwnerBackendNodeId :: DomBackendNodeId,
   domGetFrameOwnerNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetFrameOwner where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command DomGetFrameOwner where
   commandName _ = "DOM.getFrameOwner"




data PDomGetContainerForNode = PDomGetContainerForNode {
   pDomGetContainerForNodeNodeId :: DomNodeId,
   pDomGetContainerForNodeContainerName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetContainerForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PDomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


domGetContainerForNode :: Handle ev -> PDomGetContainerForNode -> IO (Either Error DomGetContainerForNode)
domGetContainerForNode handle params = sendReceiveCommandResult handle "DOM.getContainerForNode" (Just params)

data DomGetContainerForNode = DomGetContainerForNode {
   domGetContainerForNodeNodeId :: Maybe DomNodeId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetContainerForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command DomGetContainerForNode where
   commandName _ = "DOM.getContainerForNode"




data PDomGetQueryingDescendantsForContainer = PDomGetQueryingDescendantsForContainer {
   pDomGetQueryingDescendantsForContainerNodeId :: DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PDomGetQueryingDescendantsForContainer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PDomGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


domGetQueryingDescendantsForContainer :: Handle ev -> PDomGetQueryingDescendantsForContainer -> IO (Either Error DomGetQueryingDescendantsForContainer)
domGetQueryingDescendantsForContainer handle params = sendReceiveCommandResult handle "DOM.getQueryingDescendantsForContainer" (Just params)

data DomGetQueryingDescendantsForContainer = DomGetQueryingDescendantsForContainer {
   domGetQueryingDescendantsForContainerNodeIds :: [DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  DomGetQueryingDescendantsForContainer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command DomGetQueryingDescendantsForContainer where
   commandName _ = "DOM.getQueryingDescendantsForContainer"




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
   emulationScreenOrientationType :: EmulationScreenOrientationType,
   emulationScreenOrientationAngle :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationScreenOrientation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  EmulationScreenOrientation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


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
   emulationDisplayFeatureOrientation :: EmulationDisplayFeatureOrientation,
   emulationDisplayFeatureOffset :: Int,
   emulationDisplayFeatureMaskLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationDisplayFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  EmulationDisplayFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data EmulationMediaFeature = EmulationMediaFeature {
   emulationMediaFeatureName :: String,
   emulationMediaFeatureValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationMediaFeature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  EmulationMediaFeature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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



data EmulationUserAgentBrandVersion = EmulationUserAgentBrandVersion {
   emulationUserAgentBrandVersionBrand :: String,
   emulationUserAgentBrandVersionVersion :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON EmulationUserAgentBrandVersion  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  EmulationUserAgentBrandVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



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




data EmulationVirtualTimeBudgetExpired = EmulationVirtualTimeBudgetExpired
   deriving (Eq, Show, Read)
instance FromJSON EmulationVirtualTimeBudgetExpired where
   parseJSON = A.withText  "EmulationVirtualTimeBudgetExpired"  $ \v -> do
      case v of
         "EmulationVirtualTimeBudgetExpired" -> pure EmulationVirtualTimeBudgetExpired
         _ -> fail "failed to parse EmulationVirtualTimeBudgetExpired"




emulationCanEmulate :: Handle ev -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate handle = sendReceiveCommandResult handle "Emulation.canEmulate" (Nothing :: Maybe ())

data EmulationCanEmulate = EmulationCanEmulate {
   emulationCanEmulateResult :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationCanEmulate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command EmulationCanEmulate where
   commandName _ = "Emulation.canEmulate"



emulationClearDeviceMetricsOverride :: Handle ev -> IO (Maybe Error)
emulationClearDeviceMetricsOverride handle = sendReceiveCommand handle "Emulation.clearDeviceMetricsOverride" (Nothing :: Maybe ())


emulationClearGeolocationOverride :: Handle ev -> IO (Maybe Error)
emulationClearGeolocationOverride handle = sendReceiveCommand handle "Emulation.clearGeolocationOverride" (Nothing :: Maybe ())


emulationResetPageScaleFactor :: Handle ev -> IO (Maybe Error)
emulationResetPageScaleFactor handle = sendReceiveCommand handle "Emulation.resetPageScaleFactor" (Nothing :: Maybe ())



data PEmulationSetFocusEmulationEnabled = PEmulationSetFocusEmulationEnabled {
   pEmulationSetFocusEmulationEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetFocusEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetFocusEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetFocusEmulationEnabled :: Handle ev -> PEmulationSetFocusEmulationEnabled -> IO (Maybe Error)
emulationSetFocusEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setFocusEmulationEnabled" (Just params)



data PEmulationSetAutoDarkModeOverride = PEmulationSetAutoDarkModeOverride {
   pEmulationSetAutoDarkModeOverrideEnabled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutoDarkModeOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutoDarkModeOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


emulationSetAutoDarkModeOverride :: Handle ev -> PEmulationSetAutoDarkModeOverride -> IO (Maybe Error)
emulationSetAutoDarkModeOverride handle params = sendReceiveCommand handle "Emulation.setAutoDarkModeOverride" (Just params)



data PEmulationSetCpuThrottlingRate = PEmulationSetCpuThrottlingRate {
   pEmulationSetCpuThrottlingRateRate :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetCpuThrottlingRate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetCpuThrottlingRate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


emulationSetCpuThrottlingRate :: Handle ev -> PEmulationSetCpuThrottlingRate -> IO (Maybe Error)
emulationSetCpuThrottlingRate handle params = sendReceiveCommand handle "Emulation.setCPUThrottlingRate" (Just params)



data PEmulationSetDefaultBackgroundColorOverride = PEmulationSetDefaultBackgroundColorOverride {
   pEmulationSetDefaultBackgroundColorOverrideColor :: Maybe DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDefaultBackgroundColorOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDefaultBackgroundColorOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }


emulationSetDefaultBackgroundColorOverride :: Handle ev -> PEmulationSetDefaultBackgroundColorOverride -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride handle params = sendReceiveCommand handle "Emulation.setDefaultBackgroundColorOverride" (Just params)



data PEmulationSetDeviceMetricsOverride = PEmulationSetDeviceMetricsOverride {
   pEmulationSetDeviceMetricsOverrideWidth :: Int,
   pEmulationSetDeviceMetricsOverrideHeight :: Int,
   pEmulationSetDeviceMetricsOverrideDeviceScaleFactor :: Double,
   pEmulationSetDeviceMetricsOverrideMobile :: Bool,
   pEmulationSetDeviceMetricsOverrideScale :: Maybe Double,
   pEmulationSetDeviceMetricsOverrideScreenWidth :: Maybe Int,
   pEmulationSetDeviceMetricsOverrideScreenHeight :: Maybe Int,
   pEmulationSetDeviceMetricsOverridePositionX :: Maybe Int,
   pEmulationSetDeviceMetricsOverridePositionY :: Maybe Int,
   pEmulationSetDeviceMetricsOverrideDontSetVisibleSize :: Maybe Bool,
   pEmulationSetDeviceMetricsOverrideScreenOrientation :: Maybe EmulationScreenOrientation,
   pEmulationSetDeviceMetricsOverrideViewport :: Maybe PageViewport,
   pEmulationSetDeviceMetricsOverrideDisplayFeature :: Maybe EmulationDisplayFeature
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDeviceMetricsOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDeviceMetricsOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetDeviceMetricsOverride :: Handle ev -> PEmulationSetDeviceMetricsOverride -> IO (Maybe Error)
emulationSetDeviceMetricsOverride handle params = sendReceiveCommand handle "Emulation.setDeviceMetricsOverride" (Just params)



data PEmulationSetScrollbarsHidden = PEmulationSetScrollbarsHidden {
   pEmulationSetScrollbarsHiddenHidden :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScrollbarsHidden  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScrollbarsHidden where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


emulationSetScrollbarsHidden :: Handle ev -> PEmulationSetScrollbarsHidden -> IO (Maybe Error)
emulationSetScrollbarsHidden handle params = sendReceiveCommand handle "Emulation.setScrollbarsHidden" (Just params)



data PEmulationSetDocumentCookieDisabled = PEmulationSetDocumentCookieDisabled {
   pEmulationSetDocumentCookieDisabledDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDocumentCookieDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDocumentCookieDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


emulationSetDocumentCookieDisabled :: Handle ev -> PEmulationSetDocumentCookieDisabled -> IO (Maybe Error)
emulationSetDocumentCookieDisabled handle params = sendReceiveCommand handle "Emulation.setDocumentCookieDisabled" (Just params)


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
   pEmulationSetEmitTouchEventsForMouseEnabled :: Bool,
   pEmulationSetEmitTouchEventsForMouseConfiguration :: PEmulationSetEmitTouchEventsForMouseConfiguration
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmitTouchEventsForMouse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmitTouchEventsForMouse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


emulationSetEmitTouchEventsForMouse :: Handle ev -> PEmulationSetEmitTouchEventsForMouse -> IO (Maybe Error)
emulationSetEmitTouchEventsForMouse handle params = sendReceiveCommand handle "Emulation.setEmitTouchEventsForMouse" (Just params)



data PEmulationSetEmulatedMedia = PEmulationSetEmulatedMedia {
   pEmulationSetEmulatedMediaMedia :: Maybe String,
   pEmulationSetEmulatedMediaFeatures :: Maybe [EmulationMediaFeature]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


emulationSetEmulatedMedia :: Handle ev -> PEmulationSetEmulatedMedia -> IO (Maybe Error)
emulationSetEmulatedMedia handle params = sendReceiveCommand handle "Emulation.setEmulatedMedia" (Just params)


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
   pEmulationSetEmulatedVisionDeficiencyType :: PEmulationSetEmulatedVisionDeficiencyType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetEmulatedVisionDeficiency  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetEmulatedVisionDeficiency where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


emulationSetEmulatedVisionDeficiency :: Handle ev -> PEmulationSetEmulatedVisionDeficiency -> IO (Maybe Error)
emulationSetEmulatedVisionDeficiency handle params = sendReceiveCommand handle "Emulation.setEmulatedVisionDeficiency" (Just params)



data PEmulationSetGeolocationOverride = PEmulationSetGeolocationOverride {
   pEmulationSetGeolocationOverrideLatitude :: Maybe Double,
   pEmulationSetGeolocationOverrideLongitude :: Maybe Double,
   pEmulationSetGeolocationOverrideAccuracy :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetGeolocationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetGeolocationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


emulationSetGeolocationOverride :: Handle ev -> PEmulationSetGeolocationOverride -> IO (Maybe Error)
emulationSetGeolocationOverride handle params = sendReceiveCommand handle "Emulation.setGeolocationOverride" (Just params)



data PEmulationSetIdleOverride = PEmulationSetIdleOverride {
   pEmulationSetIdleOverrideIsUserActive :: Bool,
   pEmulationSetIdleOverrideIsScreenUnlocked :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetIdleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetIdleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


emulationSetIdleOverride :: Handle ev -> PEmulationSetIdleOverride -> IO (Maybe Error)
emulationSetIdleOverride handle params = sendReceiveCommand handle "Emulation.setIdleOverride" (Just params)


emulationClearIdleOverride :: Handle ev -> IO (Maybe Error)
emulationClearIdleOverride handle = sendReceiveCommand handle "Emulation.clearIdleOverride" (Nothing :: Maybe ())



data PEmulationSetPageScaleFactor = PEmulationSetPageScaleFactor {
   pEmulationSetPageScaleFactorPageScaleFactor :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetPageScaleFactor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetPageScaleFactor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


emulationSetPageScaleFactor :: Handle ev -> PEmulationSetPageScaleFactor -> IO (Maybe Error)
emulationSetPageScaleFactor handle params = sendReceiveCommand handle "Emulation.setPageScaleFactor" (Just params)



data PEmulationSetScriptExecutionDisabled = PEmulationSetScriptExecutionDisabled {
   pEmulationSetScriptExecutionDisabledValue :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetScriptExecutionDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetScriptExecutionDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


emulationSetScriptExecutionDisabled :: Handle ev -> PEmulationSetScriptExecutionDisabled -> IO (Maybe Error)
emulationSetScriptExecutionDisabled handle params = sendReceiveCommand handle "Emulation.setScriptExecutionDisabled" (Just params)



data PEmulationSetTouchEmulationEnabled = PEmulationSetTouchEmulationEnabled {
   pEmulationSetTouchEmulationEnabledEnabled :: Bool,
   pEmulationSetTouchEmulationEnabledMaxTouchPoints :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTouchEmulationEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTouchEmulationEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


emulationSetTouchEmulationEnabled :: Handle ev -> PEmulationSetTouchEmulationEnabled -> IO (Maybe Error)
emulationSetTouchEmulationEnabled handle params = sendReceiveCommand handle "Emulation.setTouchEmulationEnabled" (Just params)



data PEmulationSetVirtualTimePolicy = PEmulationSetVirtualTimePolicy {
   pEmulationSetVirtualTimePolicyPolicy :: EmulationVirtualTimePolicy,
   pEmulationSetVirtualTimePolicyBudget :: Maybe Double,
   pEmulationSetVirtualTimePolicyMaxVirtualTimeTaskStarvationCount :: Maybe Int,
   pEmulationSetVirtualTimePolicyInitialVirtualTime :: Maybe NetworkTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetVirtualTimePolicy  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


emulationSetVirtualTimePolicy :: Handle ev -> PEmulationSetVirtualTimePolicy -> IO (Either Error EmulationSetVirtualTimePolicy)
emulationSetVirtualTimePolicy handle params = sendReceiveCommandResult handle "Emulation.setVirtualTimePolicy" (Just params)

data EmulationSetVirtualTimePolicy = EmulationSetVirtualTimePolicy {
   emulationSetVirtualTimePolicyVirtualTimeTicksBase :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  EmulationSetVirtualTimePolicy where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command EmulationSetVirtualTimePolicy where
   commandName _ = "Emulation.setVirtualTimePolicy"




data PEmulationSetLocaleOverride = PEmulationSetLocaleOverride {
   pEmulationSetLocaleOverrideLocale :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetLocaleOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetLocaleOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


emulationSetLocaleOverride :: Handle ev -> PEmulationSetLocaleOverride -> IO (Maybe Error)
emulationSetLocaleOverride handle params = sendReceiveCommand handle "Emulation.setLocaleOverride" (Just params)



data PEmulationSetTimezoneOverride = PEmulationSetTimezoneOverride {
   pEmulationSetTimezoneOverrideTimezoneId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetTimezoneOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetTimezoneOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


emulationSetTimezoneOverride :: Handle ev -> PEmulationSetTimezoneOverride -> IO (Maybe Error)
emulationSetTimezoneOverride handle params = sendReceiveCommand handle "Emulation.setTimezoneOverride" (Just params)



data PEmulationSetDisabledImageTypes = PEmulationSetDisabledImageTypes {
   pEmulationSetDisabledImageTypesImageTypes :: [EmulationDisabledImageType]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetDisabledImageTypes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetDisabledImageTypes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


emulationSetDisabledImageTypes :: Handle ev -> PEmulationSetDisabledImageTypes -> IO (Maybe Error)
emulationSetDisabledImageTypes handle params = sendReceiveCommand handle "Emulation.setDisabledImageTypes" (Just params)



data PEmulationSetHardwareConcurrencyOverride = PEmulationSetHardwareConcurrencyOverride {
   pEmulationSetHardwareConcurrencyOverrideHardwareConcurrency :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetHardwareConcurrencyOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetHardwareConcurrencyOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


emulationSetHardwareConcurrencyOverride :: Handle ev -> PEmulationSetHardwareConcurrencyOverride -> IO (Maybe Error)
emulationSetHardwareConcurrencyOverride handle params = sendReceiveCommand handle "Emulation.setHardwareConcurrencyOverride" (Just params)



data PEmulationSetUserAgentOverride = PEmulationSetUserAgentOverride {
   pEmulationSetUserAgentOverrideUserAgent :: String,
   pEmulationSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pEmulationSetUserAgentOverridePlatform :: Maybe String,
   pEmulationSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


emulationSetUserAgentOverride :: Handle ev -> PEmulationSetUserAgentOverride -> IO (Maybe Error)
emulationSetUserAgentOverride handle params = sendReceiveCommand handle "Emulation.setUserAgentOverride" (Just params)



data PEmulationSetAutomationOverride = PEmulationSetAutomationOverride {
   pEmulationSetAutomationOverrideEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEmulationSetAutomationOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PEmulationSetAutomationOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


emulationSetAutomationOverride :: Handle ev -> PEmulationSetAutomationOverride -> IO (Maybe Error)
emulationSetAutomationOverride handle params = sendReceiveCommand handle "Emulation.setAutomationOverride" (Just params)



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


type NetworkLoaderId = String
type NetworkRequestId = String
type NetworkInterceptionId = String
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


type NetworkTimeSinceEpoch = Double
type NetworkMonotonicTime = Double
type NetworkHeaders = [(String, String)]
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



data NetworkResourceTiming = NetworkResourceTiming {
   networkResourceTimingRequestTime :: Double,
   networkResourceTimingProxyStart :: Double,
   networkResourceTimingProxyEnd :: Double,
   networkResourceTimingDnsStart :: Double,
   networkResourceTimingDnsEnd :: Double,
   networkResourceTimingConnectStart :: Double,
   networkResourceTimingConnectEnd :: Double,
   networkResourceTimingSslStart :: Double,
   networkResourceTimingSslEnd :: Double,
   networkResourceTimingWorkerStart :: Double,
   networkResourceTimingWorkerReady :: Double,
   networkResourceTimingWorkerFetchStart :: Double,
   networkResourceTimingWorkerRespondWithSettled :: Double,
   networkResourceTimingSendStart :: Double,
   networkResourceTimingSendEnd :: Double,
   networkResourceTimingPushStart :: Double,
   networkResourceTimingPushEnd :: Double,
   networkResourceTimingReceiveHeadersEnd :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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



data NetworkPostDataEntry = NetworkPostDataEntry {
   networkPostDataEntryBytes :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkPostDataEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkPostDataEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


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
   networkRequestUrl :: String,
   networkRequestUrlFragment :: Maybe String,
   networkRequestMethod :: String,
   networkRequestHeaders :: NetworkHeaders,
   networkRequestPostData :: Maybe String,
   networkRequestHasPostData :: Maybe Bool,
   networkRequestPostDataEntries :: Maybe [NetworkPostDataEntry],
   networkRequestMixedContentType :: Maybe SecurityMixedContentType,
   networkRequestInitialPriority :: NetworkResourcePriority,
   networkRequestReferrerPolicy :: NetworkRequestReferrerPolicy,
   networkRequestIsLinkPreload :: Maybe Bool,
   networkRequestTrustTokenParams :: Maybe NetworkTrustTokenParams,
   networkRequestIsSameSite :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  NetworkRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
   networkSignedCertificateTimestampStatus :: String,
   networkSignedCertificateTimestampOrigin :: String,
   networkSignedCertificateTimestampLogDescription :: String,
   networkSignedCertificateTimestampLogId :: String,
   networkSignedCertificateTimestampTimestamp :: Double,
   networkSignedCertificateTimestampHashAlgorithm :: String,
   networkSignedCertificateTimestampSignatureAlgorithm :: String,
   networkSignedCertificateTimestampSignatureData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedCertificateTimestamp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedCertificateTimestamp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data NetworkSecurityDetails = NetworkSecurityDetails {
   networkSecurityDetailsProtocol :: String,
   networkSecurityDetailsKeyExchange :: String,
   networkSecurityDetailsKeyExchangeGroup :: Maybe String,
   networkSecurityDetailsCipher :: String,
   networkSecurityDetailsMac :: Maybe String,
   networkSecurityDetailsCertificateId :: SecurityCertificateId,
   networkSecurityDetailsSubjectName :: String,
   networkSecurityDetailsSanList :: [String],
   networkSecurityDetailsIssuer :: String,
   networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
   networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
   networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
   networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


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



data NetworkCorsErrorStatus = NetworkCorsErrorStatus {
   networkCorsErrorStatusCorsError :: NetworkCorsError,
   networkCorsErrorStatusFailedParameter :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCorsErrorStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkCorsErrorStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


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
   networkTrustTokenParamsRefreshPolicy :: NetworkTrustTokenParamsRefreshPolicy,
   networkTrustTokenParamsIssuers :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


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



data NetworkResponse = NetworkResponse {
   networkResponseUrl :: String,
   networkResponseStatus :: Int,
   networkResponseStatusText :: String,
   networkResponseHeaders :: NetworkHeaders,
   networkResponseMimeType :: String,
   networkResponseRequestHeaders :: Maybe NetworkHeaders,
   networkResponseConnectionReused :: Bool,
   networkResponseConnectionId :: Double,
   networkResponseRemoteIpAddress :: Maybe String,
   networkResponseRemotePort :: Maybe Int,
   networkResponseFromDiskCache :: Maybe Bool,
   networkResponseFromServiceWorker :: Maybe Bool,
   networkResponseFromPrefetchCache :: Maybe Bool,
   networkResponseEncodedDataLength :: Double,
   networkResponseTiming :: Maybe NetworkResourceTiming,
   networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
   networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
   networkResponseCacheStorageCacheName :: Maybe String,
   networkResponseProtocol :: Maybe String,
   networkResponseSecurityState :: SecuritySecurityState,
   networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  NetworkResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data NetworkWebSocketRequest = NetworkWebSocketRequest {
   networkWebSocketRequestHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data NetworkWebSocketResponse = NetworkWebSocketResponse {
   networkWebSocketResponseStatus :: Int,
   networkWebSocketResponseStatusText :: String,
   networkWebSocketResponseHeaders :: NetworkHeaders,
   networkWebSocketResponseHeadersText :: Maybe String,
   networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
   networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data NetworkWebSocketFrame = NetworkWebSocketFrame {
   networkWebSocketFrameOpcode :: Double,
   networkWebSocketFrameMask :: Bool,
   networkWebSocketFramePayloadData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data NetworkCachedResource = NetworkCachedResource {
   networkCachedResourceUrl :: String,
   networkCachedResourceType :: NetworkResourceType,
   networkCachedResourceResponse :: Maybe NetworkResponse,
   networkCachedResourceBodySize :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCachedResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkCachedResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


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
   networkInitiatorType :: NetworkInitiatorType,
   networkInitiatorStack :: Maybe Runtime.RuntimeStackTrace,
   networkInitiatorUrl :: Maybe String,
   networkInitiatorLineNumber :: Maybe Double,
   networkInitiatorColumnNumber :: Maybe Double,
   networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkInitiator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  NetworkInitiator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data NetworkCookie = NetworkCookie {
   networkCookieName :: String,
   networkCookieValue :: String,
   networkCookieDomain :: String,
   networkCookiePath :: String,
   networkCookieExpires :: Double,
   networkCookieSize :: Int,
   networkCookieHttpOnly :: Bool,
   networkCookieSecure :: Bool,
   networkCookieSession :: Bool,
   networkCookieSameSite :: Maybe NetworkCookieSameSite,
   networkCookiePriority :: NetworkCookiePriority,
   networkCookieSameParty :: Bool,
   networkCookieSourceScheme :: NetworkCookieSourceScheme,
   networkCookieSourcePort :: Int,
   networkCookiePartitionKey :: Maybe String,
   networkCookiePartitionKeyOpaque :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  NetworkCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


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



data NetworkBlockedSetCookieWithReason = NetworkBlockedSetCookieWithReason {
   networkBlockedSetCookieWithReasonBlockedReasons :: [NetworkSetCookieBlockedReason],
   networkBlockedSetCookieWithReasonCookieLine :: String,
   networkBlockedSetCookieWithReasonCookie :: Maybe NetworkCookie
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedSetCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedSetCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data NetworkBlockedCookieWithReason = NetworkBlockedCookieWithReason {
   networkBlockedCookieWithReasonBlockedReasons :: [NetworkCookieBlockedReason],
   networkBlockedCookieWithReasonCookie :: NetworkCookie
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkBlockedCookieWithReason  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkBlockedCookieWithReason where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data NetworkCookieParam = NetworkCookieParam {
   networkCookieParamName :: String,
   networkCookieParamValue :: String,
   networkCookieParamUrl :: Maybe String,
   networkCookieParamDomain :: Maybe String,
   networkCookieParamPath :: Maybe String,
   networkCookieParamSecure :: Maybe Bool,
   networkCookieParamHttpOnly :: Maybe Bool,
   networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
   networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch,
   networkCookieParamPriority :: Maybe NetworkCookiePriority,
   networkCookieParamSameParty :: Maybe Bool,
   networkCookieParamSourceScheme :: Maybe NetworkCookieSourceScheme,
   networkCookieParamSourcePort :: Maybe Int,
   networkCookieParamPartitionKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookieParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  NetworkCookieParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


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
   networkAuthChallengeSource :: NetworkAuthChallengeSource,
   networkAuthChallengeOrigin :: String,
   networkAuthChallengeScheme :: String,
   networkAuthChallengeRealm :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkAuthChallenge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkAuthChallenge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


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
   networkAuthChallengeResponseResponse :: NetworkAuthChallengeResponseResponse,
   networkAuthChallengeResponseUsername :: Maybe String,
   networkAuthChallengeResponsePassword :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkAuthChallengeResponse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  NetworkAuthChallengeResponse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


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



data NetworkRequestPattern = NetworkRequestPattern {
   networkRequestPatternUrlPattern :: Maybe String,
   networkRequestPatternResourceType :: Maybe NetworkResourceType,
   networkRequestPatternInterceptionStage :: Maybe NetworkInterceptionStage
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestPattern  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestPattern where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data NetworkSignedExchangeSignature = NetworkSignedExchangeSignature {
   networkSignedExchangeSignatureLabel :: String,
   networkSignedExchangeSignatureSignature :: String,
   networkSignedExchangeSignatureIntegrity :: String,
   networkSignedExchangeSignatureCertUrl :: Maybe String,
   networkSignedExchangeSignatureCertSha256 :: Maybe String,
   networkSignedExchangeSignatureValidityUrl :: String,
   networkSignedExchangeSignatureDate :: Int,
   networkSignedExchangeSignatureExpires :: Int,
   networkSignedExchangeSignatureCertificates :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeSignature  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeSignature where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data NetworkSignedExchangeHeader = NetworkSignedExchangeHeader {
   networkSignedExchangeHeaderRequestUrl :: String,
   networkSignedExchangeHeaderResponseCode :: Int,
   networkSignedExchangeHeaderResponseHeaders :: NetworkHeaders,
   networkSignedExchangeHeaderSignatures :: [NetworkSignedExchangeSignature],
   networkSignedExchangeHeaderHeaderIntegrity :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


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



data NetworkSignedExchangeError = NetworkSignedExchangeError {
   networkSignedExchangeErrorMessage :: String,
   networkSignedExchangeErrorSignatureIndex :: Maybe Int,
   networkSignedExchangeErrorErrorField :: Maybe NetworkSignedExchangeErrorField
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data NetworkSignedExchangeInfo = NetworkSignedExchangeInfo {
   networkSignedExchangeInfoOuterResponse :: NetworkResponse,
   networkSignedExchangeInfoHeader :: Maybe NetworkSignedExchangeHeader,
   networkSignedExchangeInfoSecurityDetails :: Maybe NetworkSecurityDetails,
   networkSignedExchangeInfoErrors :: Maybe [NetworkSignedExchangeError]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


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



data NetworkConnectTiming = NetworkConnectTiming {
   networkConnectTimingRequestTime :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkConnectTiming  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkConnectTiming where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data NetworkClientSecurityState = NetworkClientSecurityState {
   networkClientSecurityStateInitiatorIsSecureContext :: Bool,
   networkClientSecurityStateInitiatorIpAddressSpace :: NetworkIpAddressSpace,
   networkClientSecurityStatePrivateNetworkRequestPolicy :: NetworkPrivateNetworkRequestPolicy
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkClientSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkClientSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


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



data NetworkSecurityIsolationStatus = NetworkSecurityIsolationStatus {
   networkSecurityIsolationStatusCoop :: Maybe NetworkCrossOriginOpenerPolicyStatus,
   networkSecurityIsolationStatusCoep :: Maybe NetworkCrossOriginEmbedderPolicyStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


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


type NetworkReportId = String

data NetworkReportingApiReport = NetworkReportingApiReport {
   networkReportingApiReportId :: NetworkReportId,
   networkReportingApiReportInitiatorUrl :: String,
   networkReportingApiReportDestination :: String,
   networkReportingApiReportType :: String,
   networkReportingApiReportTimestamp :: NetworkTimeSinceEpoch,
   networkReportingApiReportDepth :: Int,
   networkReportingApiReportCompletedAttempts :: Int,
   networkReportingApiReportBody :: [(String, String)],
   networkReportingApiReportStatus :: NetworkReportStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data NetworkReportingApiEndpoint = NetworkReportingApiEndpoint {
   networkReportingApiEndpointUrl :: String,
   networkReportingApiEndpointGroupName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data NetworkLoadNetworkResourcePageResult = NetworkLoadNetworkResourcePageResult {
   networkLoadNetworkResourcePageResultSuccess :: Bool,
   networkLoadNetworkResourcePageResultNetError :: Maybe Double,
   networkLoadNetworkResourcePageResultNetErrorName :: Maybe String,
   networkLoadNetworkResourcePageResultHttpStatusCode :: Maybe Double,
   networkLoadNetworkResourcePageResultStream :: Maybe IO.IoStreamHandle,
   networkLoadNetworkResourcePageResultHeaders :: Maybe NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourcePageResult  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourcePageResult where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



data NetworkLoadNetworkResourceOptions = NetworkLoadNetworkResourceOptions {
   networkLoadNetworkResourceOptionsDisableCache :: Bool,
   networkLoadNetworkResourceOptionsIncludeCredentials :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadNetworkResourceOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadNetworkResourceOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }





data NetworkDataReceived = NetworkDataReceived {
   networkDataReceivedRequestId :: NetworkRequestId,
   networkDataReceivedTimestamp :: NetworkMonotonicTime,
   networkDataReceivedDataLength :: Int,
   networkDataReceivedEncodedDataLength :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkDataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  NetworkDataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data NetworkEventSourceMessageReceived = NetworkEventSourceMessageReceived {
   networkEventSourceMessageReceivedRequestId :: NetworkRequestId,
   networkEventSourceMessageReceivedTimestamp :: NetworkMonotonicTime,
   networkEventSourceMessageReceivedEventName :: String,
   networkEventSourceMessageReceivedEventId :: String,
   networkEventSourceMessageReceivedData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkEventSourceMessageReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkEventSourceMessageReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data NetworkLoadingFailed = NetworkLoadingFailed {
   networkLoadingFailedRequestId :: NetworkRequestId,
   networkLoadingFailedTimestamp :: NetworkMonotonicTime,
   networkLoadingFailedType :: NetworkResourceType,
   networkLoadingFailedErrorText :: String,
   networkLoadingFailedCanceled :: Maybe Bool,
   networkLoadingFailedBlockedReason :: Maybe NetworkBlockedReason,
   networkLoadingFailedCorsErrorStatus :: Maybe NetworkCorsErrorStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFailed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFailed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data NetworkLoadingFinished = NetworkLoadingFinished {
   networkLoadingFinishedRequestId :: NetworkRequestId,
   networkLoadingFinishedTimestamp :: NetworkMonotonicTime,
   networkLoadingFinishedEncodedDataLength :: Double,
   networkLoadingFinishedShouldReportCorbBlocking :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkLoadingFinished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkLoadingFinished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data NetworkRequestServedFromCache = NetworkRequestServedFromCache {
   networkRequestServedFromCacheRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestServedFromCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestServedFromCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data NetworkRequestWillBeSent = NetworkRequestWillBeSent {
   networkRequestWillBeSentRequestId :: NetworkRequestId,
   networkRequestWillBeSentLoaderId :: NetworkLoaderId,
   networkRequestWillBeSentDocumentUrl :: String,
   networkRequestWillBeSentRequest :: NetworkRequest,
   networkRequestWillBeSentTimestamp :: NetworkMonotonicTime,
   networkRequestWillBeSentWallTime :: NetworkTimeSinceEpoch,
   networkRequestWillBeSentInitiator :: NetworkInitiator,
   networkRequestWillBeSentRedirectHasExtraInfo :: Bool,
   networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
   networkRequestWillBeSentType :: Maybe NetworkResourceType,
   networkRequestWillBeSentFrameId :: Maybe PageFrameId,
   networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data NetworkResourceChangedPriority = NetworkResourceChangedPriority {
   networkResourceChangedPriorityRequestId :: NetworkRequestId,
   networkResourceChangedPriorityNewPriority :: NetworkResourcePriority,
   networkResourceChangedPriorityTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResourceChangedPriority  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkResourceChangedPriority where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data NetworkSignedExchangeReceived = NetworkSignedExchangeReceived {
   networkSignedExchangeReceivedRequestId :: NetworkRequestId,
   networkSignedExchangeReceivedInfo :: NetworkSignedExchangeInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSignedExchangeReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkSignedExchangeReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data NetworkResponseReceived = NetworkResponseReceived {
   networkResponseReceivedRequestId :: NetworkRequestId,
   networkResponseReceivedLoaderId :: NetworkLoaderId,
   networkResponseReceivedTimestamp :: NetworkMonotonicTime,
   networkResponseReceivedType :: NetworkResourceType,
   networkResponseReceivedResponse :: NetworkResponse,
   networkResponseReceivedHasExtraInfo :: Bool,
   networkResponseReceivedFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data NetworkWebSocketClosed = NetworkWebSocketClosed {
   networkWebSocketClosedRequestId :: NetworkRequestId,
   networkWebSocketClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data NetworkWebSocketCreated = NetworkWebSocketCreated {
   networkWebSocketCreatedRequestId :: NetworkRequestId,
   networkWebSocketCreatedUrl :: String,
   networkWebSocketCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data NetworkWebSocketFrameError = NetworkWebSocketFrameError {
   networkWebSocketFrameErrorRequestId :: NetworkRequestId,
   networkWebSocketFrameErrorTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameErrorErrorMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data NetworkWebSocketFrameReceived = NetworkWebSocketFrameReceived {
   networkWebSocketFrameReceivedRequestId :: NetworkRequestId,
   networkWebSocketFrameReceivedTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameReceivedResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data NetworkWebSocketFrameSent = NetworkWebSocketFrameSent {
   networkWebSocketFrameSentRequestId :: NetworkRequestId,
   networkWebSocketFrameSentTimestamp :: NetworkMonotonicTime,
   networkWebSocketFrameSentResponse :: NetworkWebSocketFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketFrameSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketFrameSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data NetworkWebSocketHandshakeResponseReceived = NetworkWebSocketHandshakeResponseReceived {
   networkWebSocketHandshakeResponseReceivedRequestId :: NetworkRequestId,
   networkWebSocketHandshakeResponseReceivedTimestamp :: NetworkMonotonicTime,
   networkWebSocketHandshakeResponseReceivedResponse :: NetworkWebSocketResponse
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketHandshakeResponseReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketHandshakeResponseReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



data NetworkWebSocketWillSendHandshakeRequest = NetworkWebSocketWillSendHandshakeRequest {
   networkWebSocketWillSendHandshakeRequestRequestId :: NetworkRequestId,
   networkWebSocketWillSendHandshakeRequestTimestamp :: NetworkMonotonicTime,
   networkWebSocketWillSendHandshakeRequestWallTime :: NetworkTimeSinceEpoch,
   networkWebSocketWillSendHandshakeRequestRequest :: NetworkWebSocketRequest
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebSocketWillSendHandshakeRequest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebSocketWillSendHandshakeRequest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



data NetworkWebTransportCreated = NetworkWebTransportCreated {
   networkWebTransportCreatedTransportId :: NetworkRequestId,
   networkWebTransportCreatedUrl :: String,
   networkWebTransportCreatedTimestamp :: NetworkMonotonicTime,
   networkWebTransportCreatedInitiator :: Maybe NetworkInitiator
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data NetworkWebTransportConnectionEstablished = NetworkWebTransportConnectionEstablished {
   networkWebTransportConnectionEstablishedTransportId :: NetworkRequestId,
   networkWebTransportConnectionEstablishedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportConnectionEstablished  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportConnectionEstablished where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



data NetworkWebTransportClosed = NetworkWebTransportClosed {
   networkWebTransportClosedTransportId :: NetworkRequestId,
   networkWebTransportClosedTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkWebTransportClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  NetworkWebTransportClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data NetworkRequestWillBeSentExtraInfo = NetworkRequestWillBeSentExtraInfo {
   networkRequestWillBeSentExtraInfoRequestId :: NetworkRequestId,
   networkRequestWillBeSentExtraInfoAssociatedCookies :: [NetworkBlockedCookieWithReason],
   networkRequestWillBeSentExtraInfoHeaders :: NetworkHeaders,
   networkRequestWillBeSentExtraInfoConnectTiming :: NetworkConnectTiming,
   networkRequestWillBeSentExtraInfoClientSecurityState :: Maybe NetworkClientSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSentExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSentExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data NetworkResponseReceivedExtraInfo = NetworkResponseReceivedExtraInfo {
   networkResponseReceivedExtraInfoRequestId :: NetworkRequestId,
   networkResponseReceivedExtraInfoBlockedCookies :: [NetworkBlockedSetCookieWithReason],
   networkResponseReceivedExtraInfoHeaders :: NetworkHeaders,
   networkResponseReceivedExtraInfoResourceIpAddressSpace :: NetworkIpAddressSpace,
   networkResponseReceivedExtraInfoStatusCode :: Int,
   networkResponseReceivedExtraInfoHeadersText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkResponseReceivedExtraInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkResponseReceivedExtraInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


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
   networkTrustTokenOperationDoneStatus :: NetworkTrustTokenOperationDoneStatus,
   networkTrustTokenOperationDoneType :: NetworkTrustTokenOperationType,
   networkTrustTokenOperationDoneRequestId :: NetworkRequestId,
   networkTrustTokenOperationDoneTopLevelOrigin :: Maybe String,
   networkTrustTokenOperationDoneIssuerOrigin :: Maybe String,
   networkTrustTokenOperationDoneIssuedTokenCount :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkTrustTokenOperationDone  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkTrustTokenOperationDone where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data NetworkSubresourceWebBundleMetadataReceived = NetworkSubresourceWebBundleMetadataReceived {
   networkSubresourceWebBundleMetadataReceivedRequestId :: NetworkRequestId,
   networkSubresourceWebBundleMetadataReceivedUrls :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataReceived  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataReceived where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }



data NetworkSubresourceWebBundleMetadataError = NetworkSubresourceWebBundleMetadataError {
   networkSubresourceWebBundleMetadataErrorRequestId :: NetworkRequestId,
   networkSubresourceWebBundleMetadataErrorErrorMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleMetadataError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleMetadataError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }



data NetworkSubresourceWebBundleInnerResponseParsed = NetworkSubresourceWebBundleInnerResponseParsed {
   networkSubresourceWebBundleInnerResponseParsedInnerRequestId :: NetworkRequestId,
   networkSubresourceWebBundleInnerResponseParsedInnerRequestUrl :: String,
   networkSubresourceWebBundleInnerResponseParsedBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseParsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseParsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }



data NetworkSubresourceWebBundleInnerResponseError = NetworkSubresourceWebBundleInnerResponseError {
   networkSubresourceWebBundleInnerResponseErrorInnerRequestId :: NetworkRequestId,
   networkSubresourceWebBundleInnerResponseErrorInnerRequestUrl :: String,
   networkSubresourceWebBundleInnerResponseErrorErrorMessage :: String,
   networkSubresourceWebBundleInnerResponseErrorBundleRequestId :: Maybe NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkSubresourceWebBundleInnerResponseError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  NetworkSubresourceWebBundleInnerResponseError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



data NetworkReportingApiReportAdded = NetworkReportingApiReportAdded {
   networkReportingApiReportAddedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data NetworkReportingApiReportUpdated = NetworkReportingApiReportUpdated {
   networkReportingApiReportUpdatedReport :: NetworkReportingApiReport
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiReportUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiReportUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



data NetworkReportingApiEndpointsChangedForOrigin = NetworkReportingApiEndpointsChangedForOrigin {
   networkReportingApiEndpointsChangedForOriginOrigin :: String,
   networkReportingApiEndpointsChangedForOriginEndpoints :: [NetworkReportingApiEndpoint]
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkReportingApiEndpointsChangedForOrigin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  NetworkReportingApiEndpointsChangedForOrigin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }





data PNetworkSetAcceptedEncodings = PNetworkSetAcceptedEncodings {
   pNetworkSetAcceptedEncodingsEncodings :: [NetworkContentEncoding]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAcceptedEncodings  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAcceptedEncodings where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


networkSetAcceptedEncodings :: Handle ev -> PNetworkSetAcceptedEncodings -> IO (Maybe Error)
networkSetAcceptedEncodings handle params = sendReceiveCommand handle "Network.setAcceptedEncodings" (Just params)


networkClearAcceptedEncodingsOverride :: Handle ev -> IO (Maybe Error)
networkClearAcceptedEncodingsOverride handle = sendReceiveCommand handle "Network.clearAcceptedEncodingsOverride" (Nothing :: Maybe ())


networkClearBrowserCache :: Handle ev -> IO (Maybe Error)
networkClearBrowserCache handle = sendReceiveCommand handle "Network.clearBrowserCache" (Nothing :: Maybe ())


networkClearBrowserCookies :: Handle ev -> IO (Maybe Error)
networkClearBrowserCookies handle = sendReceiveCommand handle "Network.clearBrowserCookies" (Nothing :: Maybe ())



data PNetworkDeleteCookies = PNetworkDeleteCookies {
   pNetworkDeleteCookiesName :: String,
   pNetworkDeleteCookiesUrl :: Maybe String,
   pNetworkDeleteCookiesDomain :: Maybe String,
   pNetworkDeleteCookiesPath :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkDeleteCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PNetworkDeleteCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


networkDeleteCookies :: Handle ev -> PNetworkDeleteCookies -> IO (Maybe Error)
networkDeleteCookies handle params = sendReceiveCommand handle "Network.deleteCookies" (Just params)


networkDisable :: Handle ev -> IO (Maybe Error)
networkDisable handle = sendReceiveCommand handle "Network.disable" (Nothing :: Maybe ())



data PNetworkEmulateNetworkConditions = PNetworkEmulateNetworkConditions {
   pNetworkEmulateNetworkConditionsOffline :: Bool,
   pNetworkEmulateNetworkConditionsLatency :: Double,
   pNetworkEmulateNetworkConditionsDownloadThroughput :: Double,
   pNetworkEmulateNetworkConditionsUploadThroughput :: Double,
   pNetworkEmulateNetworkConditionsConnectionType :: Maybe NetworkConnectionType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEmulateNetworkConditions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PNetworkEmulateNetworkConditions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


networkEmulateNetworkConditions :: Handle ev -> PNetworkEmulateNetworkConditions -> IO (Maybe Error)
networkEmulateNetworkConditions handle params = sendReceiveCommand handle "Network.emulateNetworkConditions" (Just params)



data PNetworkEnable = PNetworkEnable {
   pNetworkEnableMaxTotalBufferSize :: Maybe Int,
   pNetworkEnableMaxResourceBufferSize :: Maybe Int,
   pNetworkEnableMaxPostDataSize :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


networkEnable :: Handle ev -> PNetworkEnable -> IO (Maybe Error)
networkEnable handle params = sendReceiveCommand handle "Network.enable" (Just params)


networkGetAllCookies :: Handle ev -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies handle = sendReceiveCommandResult handle "Network.getAllCookies" (Nothing :: Maybe ())

data NetworkGetAllCookies = NetworkGetAllCookies {
   networkGetAllCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetAllCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command NetworkGetAllCookies where
   commandName _ = "Network.getAllCookies"




data PNetworkGetCertificate = PNetworkGetCertificate {
   pNetworkGetCertificateOrigin :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCertificate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


networkGetCertificate :: Handle ev -> PNetworkGetCertificate -> IO (Either Error NetworkGetCertificate)
networkGetCertificate handle params = sendReceiveCommandResult handle "Network.getCertificate" (Just params)

data NetworkGetCertificate = NetworkGetCertificate {
   networkGetCertificateTableNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCertificate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command NetworkGetCertificate where
   commandName _ = "Network.getCertificate"




data PNetworkGetCookies = PNetworkGetCookies {
   pNetworkGetCookiesUrls :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


networkGetCookies :: Handle ev -> PNetworkGetCookies -> IO (Either Error NetworkGetCookies)
networkGetCookies handle params = sendReceiveCommandResult handle "Network.getCookies" (Just params)

data NetworkGetCookies = NetworkGetCookies {
   networkGetCookiesCookies :: [NetworkCookie]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command NetworkGetCookies where
   commandName _ = "Network.getCookies"




data PNetworkGetResponseBody = PNetworkGetResponseBody {
   pNetworkGetResponseBodyRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


networkGetResponseBody :: Handle ev -> PNetworkGetResponseBody -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody handle params = sendReceiveCommandResult handle "Network.getResponseBody" (Just params)

data NetworkGetResponseBody = NetworkGetResponseBody {
   networkGetResponseBodyBody :: String,
   networkGetResponseBodyBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command NetworkGetResponseBody where
   commandName _ = "Network.getResponseBody"




data PNetworkGetRequestPostData = PNetworkGetRequestPostData {
   pNetworkGetRequestPostDataRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetRequestPostData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


networkGetRequestPostData :: Handle ev -> PNetworkGetRequestPostData -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData handle params = sendReceiveCommandResult handle "Network.getRequestPostData" (Just params)

data NetworkGetRequestPostData = NetworkGetRequestPostData {
   networkGetRequestPostDataPostData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetRequestPostData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command NetworkGetRequestPostData where
   commandName _ = "Network.getRequestPostData"




data PNetworkGetResponseBodyForInterception = PNetworkGetResponseBodyForInterception {
   pNetworkGetResponseBodyForInterceptionInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetResponseBodyForInterception  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


networkGetResponseBodyForInterception :: Handle ev -> PNetworkGetResponseBodyForInterception -> IO (Either Error NetworkGetResponseBodyForInterception)
networkGetResponseBodyForInterception handle params = sendReceiveCommandResult handle "Network.getResponseBodyForInterception" (Just params)

data NetworkGetResponseBodyForInterception = NetworkGetResponseBodyForInterception {
   networkGetResponseBodyForInterceptionBody :: String,
   networkGetResponseBodyForInterceptionBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetResponseBodyForInterception where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command NetworkGetResponseBodyForInterception where
   commandName _ = "Network.getResponseBodyForInterception"




data PNetworkTakeResponseBodyForInterceptionAsStream = PNetworkTakeResponseBodyForInterceptionAsStream {
   pNetworkTakeResponseBodyForInterceptionAsStreamInterceptionId :: NetworkInterceptionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkTakeResponseBodyForInterceptionAsStream  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 , A.omitNothingFields = True}

instance FromJSON  PNetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 47 }


networkTakeResponseBodyForInterceptionAsStream :: Handle ev -> PNetworkTakeResponseBodyForInterceptionAsStream -> IO (Either Error NetworkTakeResponseBodyForInterceptionAsStream)
networkTakeResponseBodyForInterceptionAsStream handle params = sendReceiveCommandResult handle "Network.takeResponseBodyForInterceptionAsStream" (Just params)

data NetworkTakeResponseBodyForInterceptionAsStream = NetworkTakeResponseBodyForInterceptionAsStream {
   networkTakeResponseBodyForInterceptionAsStreamStream :: IO.IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkTakeResponseBodyForInterceptionAsStream where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }

instance Command NetworkTakeResponseBodyForInterceptionAsStream where
   commandName _ = "Network.takeResponseBodyForInterceptionAsStream"




data PNetworkReplayXhr = PNetworkReplayXhr {
   pNetworkReplayXhrRequestId :: NetworkRequestId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkReplayXhr  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkReplayXhr where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


networkReplayXhr :: Handle ev -> PNetworkReplayXhr -> IO (Maybe Error)
networkReplayXhr handle params = sendReceiveCommand handle "Network.replayXHR" (Just params)



data PNetworkSearchInResponseBody = PNetworkSearchInResponseBody {
   pNetworkSearchInResponseBodyRequestId :: NetworkRequestId,
   pNetworkSearchInResponseBodyQuery :: String,
   pNetworkSearchInResponseBodyCaseSensitive :: Maybe Bool,
   pNetworkSearchInResponseBodyIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSearchInResponseBody  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


networkSearchInResponseBody :: Handle ev -> PNetworkSearchInResponseBody -> IO (Either Error NetworkSearchInResponseBody)
networkSearchInResponseBody handle params = sendReceiveCommandResult handle "Network.searchInResponseBody" (Just params)

data NetworkSearchInResponseBody = NetworkSearchInResponseBody {
   networkSearchInResponseBodyResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkSearchInResponseBody where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command NetworkSearchInResponseBody where
   commandName _ = "Network.searchInResponseBody"




data PNetworkSetBlockedUrLs = PNetworkSetBlockedUrLs {
   pNetworkSetBlockedUrLsUrls :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBlockedUrLs  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBlockedUrLs where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


networkSetBlockedUrLs :: Handle ev -> PNetworkSetBlockedUrLs -> IO (Maybe Error)
networkSetBlockedUrLs handle params = sendReceiveCommand handle "Network.setBlockedURLs" (Just params)



data PNetworkSetBypassServiceWorker = PNetworkSetBypassServiceWorker {
   pNetworkSetBypassServiceWorkerBypass :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetBypassServiceWorker  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetBypassServiceWorker where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


networkSetBypassServiceWorker :: Handle ev -> PNetworkSetBypassServiceWorker -> IO (Maybe Error)
networkSetBypassServiceWorker handle params = sendReceiveCommand handle "Network.setBypassServiceWorker" (Just params)



data PNetworkSetCacheDisabled = PNetworkSetCacheDisabled {
   pNetworkSetCacheDisabledCacheDisabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCacheDisabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCacheDisabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


networkSetCacheDisabled :: Handle ev -> PNetworkSetCacheDisabled -> IO (Maybe Error)
networkSetCacheDisabled handle params = sendReceiveCommand handle "Network.setCacheDisabled" (Just params)



data PNetworkSetCookie = PNetworkSetCookie {
   pNetworkSetCookieName :: String,
   pNetworkSetCookieValue :: String,
   pNetworkSetCookieUrl :: Maybe String,
   pNetworkSetCookieDomain :: Maybe String,
   pNetworkSetCookiePath :: Maybe String,
   pNetworkSetCookieSecure :: Maybe Bool,
   pNetworkSetCookieHttpOnly :: Maybe Bool,
   pNetworkSetCookieSameSite :: Maybe NetworkCookieSameSite,
   pNetworkSetCookieExpires :: Maybe NetworkTimeSinceEpoch,
   pNetworkSetCookiePriority :: Maybe NetworkCookiePriority,
   pNetworkSetCookieSameParty :: Maybe Bool,
   pNetworkSetCookieSourceScheme :: Maybe NetworkCookieSourceScheme,
   pNetworkSetCookieSourcePort :: Maybe Int,
   pNetworkSetCookiePartitionKey :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


networkSetCookie :: Handle ev -> PNetworkSetCookie -> IO (Maybe Error)
networkSetCookie handle params = sendReceiveCommand handle "Network.setCookie" (Just params)



data PNetworkSetCookies = PNetworkSetCookies {
   pNetworkSetCookiesCookies :: [NetworkCookieParam]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetCookies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetCookies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


networkSetCookies :: Handle ev -> PNetworkSetCookies -> IO (Maybe Error)
networkSetCookies handle params = sendReceiveCommand handle "Network.setCookies" (Just params)



data PNetworkSetExtraHttpHeaders = PNetworkSetExtraHttpHeaders {
   pNetworkSetExtraHttpHeadersHeaders :: NetworkHeaders
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetExtraHttpHeaders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetExtraHttpHeaders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


networkSetExtraHttpHeaders :: Handle ev -> PNetworkSetExtraHttpHeaders -> IO (Maybe Error)
networkSetExtraHttpHeaders handle params = sendReceiveCommand handle "Network.setExtraHTTPHeaders" (Just params)



data PNetworkSetAttachDebugStack = PNetworkSetAttachDebugStack {
   pNetworkSetAttachDebugStackEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetAttachDebugStack  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetAttachDebugStack where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


networkSetAttachDebugStack :: Handle ev -> PNetworkSetAttachDebugStack -> IO (Maybe Error)
networkSetAttachDebugStack handle params = sendReceiveCommand handle "Network.setAttachDebugStack" (Just params)



data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
   pNetworkSetUserAgentOverrideUserAgent :: String,
   pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pNetworkSetUserAgentOverridePlatform :: Maybe String,
   pNetworkSetUserAgentOverrideUserAgentMetadata :: Maybe EmulationUserAgentMetadata
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


networkSetUserAgentOverride :: Handle ev -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)



data PNetworkGetSecurityIsolationStatus = PNetworkGetSecurityIsolationStatus {
   pNetworkGetSecurityIsolationStatusFrameId :: Maybe PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkGetSecurityIsolationStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PNetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


networkGetSecurityIsolationStatus :: Handle ev -> PNetworkGetSecurityIsolationStatus -> IO (Either Error NetworkGetSecurityIsolationStatus)
networkGetSecurityIsolationStatus handle params = sendReceiveCommandResult handle "Network.getSecurityIsolationStatus" (Just params)

data NetworkGetSecurityIsolationStatus = NetworkGetSecurityIsolationStatus {
   networkGetSecurityIsolationStatusStatus :: NetworkSecurityIsolationStatus
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkGetSecurityIsolationStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }

instance Command NetworkGetSecurityIsolationStatus where
   commandName _ = "Network.getSecurityIsolationStatus"




data PNetworkEnableReportingApi = PNetworkEnableReportingApi {
   pNetworkEnableReportingApiEnable :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkEnableReportingApi  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PNetworkEnableReportingApi where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


networkEnableReportingApi :: Handle ev -> PNetworkEnableReportingApi -> IO (Maybe Error)
networkEnableReportingApi handle params = sendReceiveCommand handle "Network.enableReportingApi" (Just params)



data PNetworkLoadNetworkResource = PNetworkLoadNetworkResource {
   pNetworkLoadNetworkResourceFrameId :: Maybe PageFrameId,
   pNetworkLoadNetworkResourceUrl :: String,
   pNetworkLoadNetworkResourceOptions :: NetworkLoadNetworkResourceOptions
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkLoadNetworkResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PNetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


networkLoadNetworkResource :: Handle ev -> PNetworkLoadNetworkResource -> IO (Either Error NetworkLoadNetworkResource)
networkLoadNetworkResource handle params = sendReceiveCommandResult handle "Network.loadNetworkResource" (Just params)

data NetworkLoadNetworkResource = NetworkLoadNetworkResource {
   networkLoadNetworkResourceResource :: NetworkLoadNetworkResourcePageResult
} deriving (Generic, Eq, Show, Read)

instance FromJSON  NetworkLoadNetworkResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command NetworkLoadNetworkResource where
   commandName _ = "Network.loadNetworkResource"




type PageFrameId = String
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



data PageAdFrameStatus = PageAdFrameStatus {
   pageAdFrameStatusAdFrameType :: PageAdFrameType,
   pageAdFrameStatusExplanations :: Maybe [PageAdFrameExplanation]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAdFrameStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageAdFrameStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


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



data PagePermissionsPolicyBlockLocator = PagePermissionsPolicyBlockLocator {
   pagePermissionsPolicyBlockLocatorFrameId :: PageFrameId,
   pagePermissionsPolicyBlockLocatorBlockReason :: PagePermissionsPolicyBlockReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyBlockLocator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyBlockLocator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data PagePermissionsPolicyFeatureState = PagePermissionsPolicyFeatureState {
   pagePermissionsPolicyFeatureStateFeature :: PagePermissionsPolicyFeature,
   pagePermissionsPolicyFeatureStateAllowed :: Bool,
   pagePermissionsPolicyFeatureStateLocator :: Maybe PagePermissionsPolicyBlockLocator
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePermissionsPolicyFeatureState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  PagePermissionsPolicyFeatureState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


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



data PageOriginTrialTokenWithStatus = PageOriginTrialTokenWithStatus {
   pageOriginTrialTokenWithStatusRawTokenText :: String,
   pageOriginTrialTokenWithStatusParsedToken :: Maybe PageOriginTrialToken,
   pageOriginTrialTokenWithStatusStatus :: PageOriginTrialTokenStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrialTokenWithStatus  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrialTokenWithStatus where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data PageOriginTrial = PageOriginTrial {
   pageOriginTrialTrialName :: String,
   pageOriginTrialStatus :: PageOriginTrialStatus,
   pageOriginTrialTokensWithStatus :: [PageOriginTrialTokenWithStatus]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageOriginTrial  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PageOriginTrial where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data PageFrame = PageFrame {
   pageFrameId :: PageFrameId,
   pageFrameParentId :: Maybe PageFrameId,
   pageFrameLoaderId :: NetworkLoaderId,
   pageFrameName :: Maybe String,
   pageFrameUrl :: String,
   pageFrameUrlFragment :: Maybe String,
   pageFrameDomainAndRegistry :: String,
   pageFrameSecurityOrigin :: String,
   pageFrameMimeType :: String,
   pageFrameUnreachableUrl :: Maybe String,
   pageFrameAdFrameStatus :: Maybe PageAdFrameStatus,
   pageFrameSecureContextType :: PageSecureContextType,
   pageFrameCrossOriginIsolatedContextType :: PageCrossOriginIsolatedContextType,
   pageFrameGatedApiFeatures :: [PageGatedApiFeatures]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



data PageFrameResource = PageFrameResource {
   pageFrameResourceUrl :: String,
   pageFrameResourceType :: NetworkResourceType,
   pageFrameResourceMimeType :: String,
   pageFrameResourceLastModified :: Maybe NetworkTimeSinceEpoch,
   pageFrameResourceContentSize :: Maybe Double,
   pageFrameResourceFailed :: Maybe Bool,
   pageFrameResourceCanceled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data PageFrameResourceTree = PageFrameResourceTree {
   pageFrameResourceTreeFrame :: PageFrame,
   pageFrameResourceTreeChildFrames :: Maybe [PageFrameResourceTree],
   pageFrameResourceTreeResources :: [PageFrameResource]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameResourceTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFrameResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data PageFrameTree = PageFrameTree {
   pageFrameTreeFrame :: PageFrame,
   pageFrameTreeChildFrames :: Maybe [PageFrameTree]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PageFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


type PageScriptIdentifier = String
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



data PageNavigationEntry = PageNavigationEntry {
   pageNavigationEntryId :: Int,
   pageNavigationEntryUrl :: String,
   pageNavigationEntryUserTypedUrl :: String,
   pageNavigationEntryTitle :: String,
   pageNavigationEntryTransitionType :: PageTransitionType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigationEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageNavigationEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data PageScreencastFrameMetadata = PageScreencastFrameMetadata {
   pageScreencastFrameMetadataOffsetTop :: Double,
   pageScreencastFrameMetadataPageScaleFactor :: Double,
   pageScreencastFrameMetadataDeviceWidth :: Double,
   pageScreencastFrameMetadataDeviceHeight :: Double,
   pageScreencastFrameMetadataScrollOffsetX :: Double,
   pageScreencastFrameMetadataScrollOffsetY :: Double,
   pageScreencastFrameMetadataTimestamp :: Maybe NetworkTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastFrameMetadata  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageScreencastFrameMetadata where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


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



data PageAppManifestError = PageAppManifestError {
   pageAppManifestErrorMessage :: String,
   pageAppManifestErrorCritical :: Int,
   pageAppManifestErrorLine :: Int,
   pageAppManifestErrorColumn :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data PageAppManifestParsedProperties = PageAppManifestParsedProperties {
   pageAppManifestParsedPropertiesScope :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageAppManifestParsedProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageAppManifestParsedProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data PageLayoutViewport = PageLayoutViewport {
   pageLayoutViewportPageX :: Int,
   pageLayoutViewportPageY :: Int,
   pageLayoutViewportClientWidth :: Int,
   pageLayoutViewportClientHeight :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLayoutViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLayoutViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageVisualViewport = PageVisualViewport {
   pageVisualViewportOffsetX :: Double,
   pageVisualViewportOffsetY :: Double,
   pageVisualViewportPageX :: Double,
   pageVisualViewportPageY :: Double,
   pageVisualViewportClientWidth :: Double,
   pageVisualViewportClientHeight :: Double,
   pageVisualViewportScale :: Double,
   pageVisualViewportZoom :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageVisualViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageVisualViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageViewport = PageViewport {
   pageViewportX :: Double,
   pageViewportY :: Double,
   pageViewportWidth :: Double,
   pageViewportHeight :: Double,
   pageViewportScale :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageViewport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  PageViewport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



data PageFontFamilies = PageFontFamilies {
   pageFontFamiliesStandard :: Maybe String,
   pageFontFamiliesFixed :: Maybe String,
   pageFontFamiliesSerif :: Maybe String,
   pageFontFamiliesSansSerif :: Maybe String,
   pageFontFamiliesCursive :: Maybe String,
   pageFontFamiliesFantasy :: Maybe String,
   pageFontFamiliesMath :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PageFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data PageScriptFontFamilies = PageScriptFontFamilies {
   pageScriptFontFamiliesScript :: String,
   pageScriptFontFamiliesFontFamilies :: PageFontFamilies
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScriptFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PageScriptFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data PageFontSizes = PageFontSizes {
   pageFontSizesStandard :: Maybe Int,
   pageFontSizesFixed :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PageFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


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



data PageInstallabilityErrorArgument = PageInstallabilityErrorArgument {
   pageInstallabilityErrorArgumentName :: String,
   pageInstallabilityErrorArgumentValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageInstallabilityErrorArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageInstallabilityErrorArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data PageInstallabilityError = PageInstallabilityError {
   pageInstallabilityErrorErrorId :: String,
   pageInstallabilityErrorErrorArguments :: [PageInstallabilityErrorArgument]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageInstallabilityError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageInstallabilityError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


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



data PageCompilationCacheParams = PageCompilationCacheParams {
   pageCompilationCacheParamsUrl :: String,
   pageCompilationCacheParamsEager :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheParams  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheParams where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


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



data PageBackForwardCacheNotRestoredExplanation = PageBackForwardCacheNotRestoredExplanation {
   pageBackForwardCacheNotRestoredExplanationType :: PageBackForwardCacheNotRestoredReasonType,
   pageBackForwardCacheNotRestoredExplanationReason :: PageBackForwardCacheNotRestoredReason,
   pageBackForwardCacheNotRestoredExplanationContext :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 42 }



data PageBackForwardCacheNotRestoredExplanationTree = PageBackForwardCacheNotRestoredExplanationTree {
   pageBackForwardCacheNotRestoredExplanationTreeUrl :: String,
   pageBackForwardCacheNotRestoredExplanationTreeExplanations :: [PageBackForwardCacheNotRestoredExplanation],
   pageBackForwardCacheNotRestoredExplanationTreeChildren :: [PageBackForwardCacheNotRestoredExplanationTree]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotRestoredExplanationTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotRestoredExplanationTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 46 }


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





data PageDomContentEventFired = PageDomContentEventFired {
   pageDomContentEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDomContentEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PageDomContentEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


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
   pageFileChooserOpenedFrameId :: PageFrameId,
   pageFileChooserOpenedBackendNodeId :: DomBackendNodeId,
   pageFileChooserOpenedMode :: PageFileChooserOpenedMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFileChooserOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PageFileChooserOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data PageFrameAttached = PageFrameAttached {
   pageFrameAttachedFrameId :: PageFrameId,
   pageFrameAttachedParentFrameId :: PageFrameId,
   pageFrameAttachedStack :: Maybe Runtime.RuntimeStackTrace
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameAttached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameAttached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


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
   pageFrameDetachedFrameId :: PageFrameId,
   pageFrameDetachedReason :: PageFrameDetachedReason
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data PageFrameNavigated = PageFrameNavigated {
   pageFrameNavigatedFrame :: PageFrame,
   pageFrameNavigatedType :: PageNavigationType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameNavigated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageFrameNavigated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageDocumentOpened = PageDocumentOpened {
   pageDocumentOpenedFrame :: PageFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageDocumentOpened  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageDocumentOpened where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


data PageFrameResized = PageFrameResized
   deriving (Eq, Show, Read)
instance FromJSON PageFrameResized where
   parseJSON = A.withText  "PageFrameResized"  $ \v -> do
      case v of
         "PageFrameResized" -> pure PageFrameResized
         _ -> fail "failed to parse PageFrameResized"



data PageFrameRequestedNavigation = PageFrameRequestedNavigation {
   pageFrameRequestedNavigationFrameId :: PageFrameId,
   pageFrameRequestedNavigationReason :: PageClientNavigationReason,
   pageFrameRequestedNavigationUrl :: String,
   pageFrameRequestedNavigationDisposition :: PageClientNavigationDisposition
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameRequestedNavigation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageFrameRequestedNavigation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data PageFrameStartedLoading = PageFrameStartedLoading {
   pageFrameStartedLoadingFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStartedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStartedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data PageFrameStoppedLoading = PageFrameStoppedLoading {
   pageFrameStoppedLoadingFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameStoppedLoading  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PageFrameStoppedLoading where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


data PageInterstitialHidden = PageInterstitialHidden
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialHidden where
   parseJSON = A.withText  "PageInterstitialHidden"  $ \v -> do
      case v of
         "PageInterstitialHidden" -> pure PageInterstitialHidden
         _ -> fail "failed to parse PageInterstitialHidden"


data PageInterstitialShown = PageInterstitialShown
   deriving (Eq, Show, Read)
instance FromJSON PageInterstitialShown where
   parseJSON = A.withText  "PageInterstitialShown"  $ \v -> do
      case v of
         "PageInterstitialShown" -> pure PageInterstitialShown
         _ -> fail "failed to parse PageInterstitialShown"



data PageJavascriptDialogClosed = PageJavascriptDialogClosed {
   pageJavascriptDialogClosedResult :: Bool,
   pageJavascriptDialogClosedUserInput :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogClosed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogClosed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data PageJavascriptDialogOpening = PageJavascriptDialogOpening {
   pageJavascriptDialogOpeningUrl :: String,
   pageJavascriptDialogOpeningMessage :: String,
   pageJavascriptDialogOpeningType :: PageDialogType,
   pageJavascriptDialogOpeningHasBrowserHandler :: Bool,
   pageJavascriptDialogOpeningDefaultPrompt :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageJavascriptDialogOpening  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageJavascriptDialogOpening where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data PageLifecycleEvent = PageLifecycleEvent {
   pageLifecycleEventFrameId :: PageFrameId,
   pageLifecycleEventLoaderId :: NetworkLoaderId,
   pageLifecycleEventName :: String,
   pageLifecycleEventTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLifecycleEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLifecycleEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageBackForwardCacheNotUsed = PageBackForwardCacheNotUsed {
   pageBackForwardCacheNotUsedLoaderId :: NetworkLoaderId,
   pageBackForwardCacheNotUsedFrameId :: PageFrameId,
   pageBackForwardCacheNotUsedNotRestoredExplanations :: [PageBackForwardCacheNotRestoredExplanation],
   pageBackForwardCacheNotUsedNotRestoredExplanationsTree :: Maybe PageBackForwardCacheNotRestoredExplanationTree
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageBackForwardCacheNotUsed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageBackForwardCacheNotUsed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data PagePrerenderAttemptCompleted = PagePrerenderAttemptCompleted {
   pagePrerenderAttemptCompletedInitiatingFrameId :: PageFrameId,
   pagePrerenderAttemptCompletedPrerenderingUrl :: String,
   pagePrerenderAttemptCompletedFinalStatus :: PagePrerenderFinalStatus
} deriving (Generic, Eq, Show, Read)
instance ToJSON PagePrerenderAttemptCompleted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PagePrerenderAttemptCompleted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data PageLoadEventFired = PageLoadEventFired {
   pageLoadEventFiredTimestamp :: NetworkMonotonicTime
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageLoadEventFired  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageLoadEventFired where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data PageNavigatedWithinDocument = PageNavigatedWithinDocument {
   pageNavigatedWithinDocumentFrameId :: PageFrameId,
   pageNavigatedWithinDocumentUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageNavigatedWithinDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PageNavigatedWithinDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data PageScreencastFrame = PageScreencastFrame {
   pageScreencastFrameData :: String,
   pageScreencastFrameMetadata :: PageScreencastFrameMetadata,
   pageScreencastFrameSessionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PageScreencastFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data PageScreencastVisibilityChanged = PageScreencastVisibilityChanged {
   pageScreencastVisibilityChangedVisible :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageScreencastVisibilityChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PageScreencastVisibilityChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data PageWindowOpen = PageWindowOpen {
   pageWindowOpenUrl :: String,
   pageWindowOpenWindowName :: String,
   pageWindowOpenWindowFeatures :: [String],
   pageWindowOpenUserGesture :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageWindowOpen  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  PageWindowOpen where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data PageCompilationCacheProduced = PageCompilationCacheProduced {
   pageCompilationCacheProducedUrl :: String,
   pageCompilationCacheProducedData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageCompilationCacheProduced  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PageCompilationCacheProduced where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }





data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
   pPageAddScriptToEvaluateOnNewDocumentSource :: String,
   pPageAddScriptToEvaluateOnNewDocumentWorldName :: Maybe String,
   pPageAddScriptToEvaluateOnNewDocumentIncludeCommandLineApi :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  PPageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


pageAddScriptToEvaluateOnNewDocument :: Handle ev -> PPageAddScriptToEvaluateOnNewDocument -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument handle params = sendReceiveCommandResult handle "Page.addScriptToEvaluateOnNewDocument" (Just params)

data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
   pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }

instance Command PageAddScriptToEvaluateOnNewDocument where
   commandName _ = "Page.addScriptToEvaluateOnNewDocument"



pageBringToFront :: Handle ev -> IO (Maybe Error)
pageBringToFront handle = sendReceiveCommand handle "Page.bringToFront" (Nothing :: Maybe ())


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
   pPageCaptureScreenshotFormat :: PPageCaptureScreenshotFormat,
   pPageCaptureScreenshotQuality :: Maybe Int,
   pPageCaptureScreenshotClip :: Maybe PageViewport,
   pPageCaptureScreenshotFromSurface :: Maybe Bool,
   pPageCaptureScreenshotCaptureBeyondViewport :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureScreenshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


pageCaptureScreenshot :: Handle ev -> PPageCaptureScreenshot -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot handle params = sendReceiveCommandResult handle "Page.captureScreenshot" (Just params)

data PageCaptureScreenshot = PageCaptureScreenshot {
   pageCaptureScreenshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureScreenshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PageCaptureScreenshot where
   commandName _ = "Page.captureScreenshot"



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
   pPageCaptureSnapshotFormat :: PPageCaptureSnapshotFormat
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCaptureSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


pageCaptureSnapshot :: Handle ev -> PPageCaptureSnapshot -> IO (Either Error PageCaptureSnapshot)
pageCaptureSnapshot handle params = sendReceiveCommandResult handle "Page.captureSnapshot" (Just params)

data PageCaptureSnapshot = PageCaptureSnapshot {
   pageCaptureSnapshotData :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCaptureSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageCaptureSnapshot where
   commandName _ = "Page.captureSnapshot"




data PPageCreateIsolatedWorld = PPageCreateIsolatedWorld {
   pPageCreateIsolatedWorldFrameId :: PageFrameId,
   pPageCreateIsolatedWorldWorldName :: Maybe String,
   pPageCreateIsolatedWorldGrantUniveralAccess :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageCreateIsolatedWorld  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


pageCreateIsolatedWorld :: Handle ev -> PPageCreateIsolatedWorld -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld handle params = sendReceiveCommandResult handle "Page.createIsolatedWorld" (Just params)

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
   pageCreateIsolatedWorldExecutionContextId :: Runtime.RuntimeExecutionContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageCreateIsolatedWorld where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command PageCreateIsolatedWorld where
   commandName _ = "Page.createIsolatedWorld"



pageDisable :: Handle ev -> IO (Maybe Error)
pageDisable handle = sendReceiveCommand handle "Page.disable" (Nothing :: Maybe ())


pageEnable :: Handle ev -> IO (Maybe Error)
pageEnable handle = sendReceiveCommand handle "Page.enable" (Nothing :: Maybe ())


pageGetAppManifest :: Handle ev -> IO (Either Error PageGetAppManifest)
pageGetAppManifest handle = sendReceiveCommandResult handle "Page.getAppManifest" (Nothing :: Maybe ())

data PageGetAppManifest = PageGetAppManifest {
   pageGetAppManifestUrl :: String,
   pageGetAppManifestErrors :: [PageAppManifestError],
   pageGetAppManifestData :: Maybe String,
   pageGetAppManifestParsed :: Maybe PageAppManifestParsedProperties
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppManifest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PageGetAppManifest where
   commandName _ = "Page.getAppManifest"



pageGetInstallabilityErrors :: Handle ev -> IO (Either Error PageGetInstallabilityErrors)
pageGetInstallabilityErrors handle = sendReceiveCommandResult handle "Page.getInstallabilityErrors" (Nothing :: Maybe ())

data PageGetInstallabilityErrors = PageGetInstallabilityErrors {
   pageGetInstallabilityErrorsInstallabilityErrors :: [PageInstallabilityError]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetInstallabilityErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PageGetInstallabilityErrors where
   commandName _ = "Page.getInstallabilityErrors"



pageGetManifestIcons :: Handle ev -> IO (Either Error PageGetManifestIcons)
pageGetManifestIcons handle = sendReceiveCommandResult handle "Page.getManifestIcons" (Nothing :: Maybe ())

data PageGetManifestIcons = PageGetManifestIcons {
   pageGetManifestIconsPrimaryIcon :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetManifestIcons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetManifestIcons where
   commandName _ = "Page.getManifestIcons"



pageGetAppId :: Handle ev -> IO (Either Error PageGetAppId)
pageGetAppId handle = sendReceiveCommandResult handle "Page.getAppId" (Nothing :: Maybe ())

data PageGetAppId = PageGetAppId {
   pageGetAppIdAppId :: Maybe String,
   pageGetAppIdRecommendedId :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageGetAppId where
   commandName _ = "Page.getAppId"



pageGetFrameTree :: Handle ev -> IO (Either Error PageGetFrameTree)
pageGetFrameTree handle = sendReceiveCommandResult handle "Page.getFrameTree" (Nothing :: Maybe ())

data PageGetFrameTree = PageGetFrameTree {
   pageGetFrameTreeFrameTree :: PageFrameTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetFrameTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PageGetFrameTree where
   commandName _ = "Page.getFrameTree"



pageGetLayoutMetrics :: Handle ev -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics handle = sendReceiveCommandResult handle "Page.getLayoutMetrics" (Nothing :: Maybe ())

data PageGetLayoutMetrics = PageGetLayoutMetrics {
   pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
   pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
   pageGetLayoutMetricsCssContentSize :: DomRect
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetLayoutMetrics where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageGetLayoutMetrics where
   commandName _ = "Page.getLayoutMetrics"



pageGetNavigationHistory :: Handle ev -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory handle = sendReceiveCommandResult handle "Page.getNavigationHistory" (Nothing :: Maybe ())

data PageGetNavigationHistory = PageGetNavigationHistory {
   pageGetNavigationHistoryCurrentIndex :: Int,
   pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetNavigationHistory where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PageGetNavigationHistory where
   commandName _ = "Page.getNavigationHistory"



pageResetNavigationHistory :: Handle ev -> IO (Maybe Error)
pageResetNavigationHistory handle = sendReceiveCommand handle "Page.resetNavigationHistory" (Nothing :: Maybe ())



data PPageGetResourceContent = PPageGetResourceContent {
   pPageGetResourceContentFrameId :: PageFrameId,
   pPageGetResourceContentUrl :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetResourceContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


pageGetResourceContent :: Handle ev -> PPageGetResourceContent -> IO (Either Error PageGetResourceContent)
pageGetResourceContent handle params = sendReceiveCommandResult handle "Page.getResourceContent" (Just params)

data PageGetResourceContent = PageGetResourceContent {
   pageGetResourceContentContent :: String,
   pageGetResourceContentBase64Encoded :: Bool
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PageGetResourceContent where
   commandName _ = "Page.getResourceContent"



pageGetResourceTree :: Handle ev -> IO (Either Error PageGetResourceTree)
pageGetResourceTree handle = sendReceiveCommandResult handle "Page.getResourceTree" (Nothing :: Maybe ())

data PageGetResourceTree = PageGetResourceTree {
   pageGetResourceTreeFrameTree :: PageFrameResourceTree
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetResourceTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetResourceTree where
   commandName _ = "Page.getResourceTree"




data PPageHandleJavaScriptDialog = PPageHandleJavaScriptDialog {
   pPageHandleJavaScriptDialogAccept :: Bool,
   pPageHandleJavaScriptDialogPromptText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageHandleJavaScriptDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageHandleJavaScriptDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


pageHandleJavaScriptDialog :: Handle ev -> PPageHandleJavaScriptDialog -> IO (Maybe Error)
pageHandleJavaScriptDialog handle params = sendReceiveCommand handle "Page.handleJavaScriptDialog" (Just params)



data PPageNavigate = PPageNavigate {
   pPageNavigateUrl :: String,
   pPageNavigateReferrer :: Maybe String,
   pPageNavigateTransitionType :: Maybe PageTransitionType,
   pPageNavigateFrameId :: Maybe PageFrameId,
   pPageNavigateReferrerPolicy :: Maybe PageReferrerPolicy
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  PPageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


pageNavigate :: Handle ev -> PPageNavigate -> IO (Either Error PageNavigate)
pageNavigate handle params = sendReceiveCommandResult handle "Page.navigate" (Just params)

data PageNavigate = PageNavigate {
   pageNavigateFrameId :: PageFrameId,
   pageNavigateLoaderId :: Maybe NetworkLoaderId,
   pageNavigateErrorText :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageNavigate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }

instance Command PageNavigate where
   commandName _ = "Page.navigate"




data PPageNavigateToHistoryEntry = PPageNavigateToHistoryEntry {
   pPageNavigateToHistoryEntryEntryId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageNavigateToHistoryEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PPageNavigateToHistoryEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


pageNavigateToHistoryEntry :: Handle ev -> PPageNavigateToHistoryEntry -> IO (Maybe Error)
pageNavigateToHistoryEntry handle params = sendReceiveCommand handle "Page.navigateToHistoryEntry" (Just params)


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
   pPagePrintToPdfLandscape :: Maybe Bool,
   pPagePrintToPdfDisplayHeaderFooter :: Maybe Bool,
   pPagePrintToPdfPrintBackground :: Maybe Bool,
   pPagePrintToPdfScale :: Maybe Double,
   pPagePrintToPdfPaperWidth :: Maybe Double,
   pPagePrintToPdfPaperHeight :: Maybe Double,
   pPagePrintToPdfMarginTop :: Maybe Double,
   pPagePrintToPdfMarginBottom :: Maybe Double,
   pPagePrintToPdfMarginLeft :: Maybe Double,
   pPagePrintToPdfMarginRight :: Maybe Double,
   pPagePrintToPdfPageRanges :: Maybe String,
   pPagePrintToPdfHeaderTemplate :: Maybe String,
   pPagePrintToPdfFooterTemplate :: Maybe String,
   pPagePrintToPdfPreferCssPageSize :: Maybe Bool,
   pPagePrintToPdfTransferMode :: PPagePrintToPdfTransferMode
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPdf  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


pagePrintToPdf :: Handle ev -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)

data PagePrintToPdf = PagePrintToPdf {
   pagePrintToPdfData :: String,
   pagePrintToPdfStream :: Maybe IO.IoStreamHandle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }

instance Command PagePrintToPdf where
   commandName _ = "Page.printToPDF"




data PPageReload = PPageReload {
   pPageReloadIgnoreCache :: Maybe Bool,
   pPageReloadScriptToEvaluateOnLoad :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageReload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PPageReload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


pageReload :: Handle ev -> PPageReload -> IO (Maybe Error)
pageReload handle params = sendReceiveCommand handle "Page.reload" (Just params)



data PPageRemoveScriptToEvaluateOnNewDocument = PPageRemoveScriptToEvaluateOnNewDocument {
   pPageRemoveScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageRemoveScriptToEvaluateOnNewDocument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 , A.omitNothingFields = True}

instance FromJSON  PPageRemoveScriptToEvaluateOnNewDocument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 40 }


pageRemoveScriptToEvaluateOnNewDocument :: Handle ev -> PPageRemoveScriptToEvaluateOnNewDocument -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument handle params = sendReceiveCommand handle "Page.removeScriptToEvaluateOnNewDocument" (Just params)



data PPageScreencastFrameAck = PPageScreencastFrameAck {
   pPageScreencastFrameAckSessionId :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageScreencastFrameAck  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageScreencastFrameAck where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


pageScreencastFrameAck :: Handle ev -> PPageScreencastFrameAck -> IO (Maybe Error)
pageScreencastFrameAck handle params = sendReceiveCommand handle "Page.screencastFrameAck" (Just params)



data PPageSearchInResource = PPageSearchInResource {
   pPageSearchInResourceFrameId :: PageFrameId,
   pPageSearchInResourceUrl :: String,
   pPageSearchInResourceQuery :: String,
   pPageSearchInResourceCaseSensitive :: Maybe Bool,
   pPageSearchInResourceIsRegex :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSearchInResource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PPageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


pageSearchInResource :: Handle ev -> PPageSearchInResource -> IO (Either Error PageSearchInResource)
pageSearchInResource handle params = sendReceiveCommandResult handle "Page.searchInResource" (Just params)

data PageSearchInResource = PageSearchInResource {
   pageSearchInResourceResult :: [Debugger.DebuggerSearchMatch]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageSearchInResource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PageSearchInResource where
   commandName _ = "Page.searchInResource"




data PPageSetAdBlockingEnabled = PPageSetAdBlockingEnabled {
   pPageSetAdBlockingEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetAdBlockingEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetAdBlockingEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


pageSetAdBlockingEnabled :: Handle ev -> PPageSetAdBlockingEnabled -> IO (Maybe Error)
pageSetAdBlockingEnabled handle params = sendReceiveCommand handle "Page.setAdBlockingEnabled" (Just params)



data PPageSetBypassCsp = PPageSetBypassCsp {
   pPageSetBypassCspEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetBypassCsp  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetBypassCsp where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


pageSetBypassCsp :: Handle ev -> PPageSetBypassCsp -> IO (Maybe Error)
pageSetBypassCsp handle params = sendReceiveCommand handle "Page.setBypassCSP" (Just params)



data PPageGetPermissionsPolicyState = PPageGetPermissionsPolicyState {
   pPageGetPermissionsPolicyStateFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetPermissionsPolicyState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


pageGetPermissionsPolicyState :: Handle ev -> PPageGetPermissionsPolicyState -> IO (Either Error PageGetPermissionsPolicyState)
pageGetPermissionsPolicyState handle params = sendReceiveCommandResult handle "Page.getPermissionsPolicyState" (Just params)

data PageGetPermissionsPolicyState = PageGetPermissionsPolicyState {
   pageGetPermissionsPolicyStateStates :: [PagePermissionsPolicyFeatureState]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetPermissionsPolicyState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command PageGetPermissionsPolicyState where
   commandName _ = "Page.getPermissionsPolicyState"




data PPageGetOriginTrials = PPageGetOriginTrials {
   pPageGetOriginTrialsFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGetOriginTrials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


pageGetOriginTrials :: Handle ev -> PPageGetOriginTrials -> IO (Either Error PageGetOriginTrials)
pageGetOriginTrials handle params = sendReceiveCommandResult handle "Page.getOriginTrials" (Just params)

data PageGetOriginTrials = PageGetOriginTrials {
   pageGetOriginTrialsOriginTrials :: [PageOriginTrial]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetOriginTrials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PageGetOriginTrials where
   commandName _ = "Page.getOriginTrials"




data PPageSetFontFamilies = PPageSetFontFamilies {
   pPageSetFontFamiliesFontFamilies :: PageFontFamilies,
   pPageSetFontFamiliesForScripts :: Maybe [PageScriptFontFamilies]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontFamilies  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontFamilies where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


pageSetFontFamilies :: Handle ev -> PPageSetFontFamilies -> IO (Maybe Error)
pageSetFontFamilies handle params = sendReceiveCommand handle "Page.setFontFamilies" (Just params)



data PPageSetFontSizes = PPageSetFontSizes {
   pPageSetFontSizesFontSizes :: PageFontSizes
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetFontSizes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PPageSetFontSizes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


pageSetFontSizes :: Handle ev -> PPageSetFontSizes -> IO (Maybe Error)
pageSetFontSizes handle params = sendReceiveCommand handle "Page.setFontSizes" (Just params)



data PPageSetDocumentContent = PPageSetDocumentContent {
   pPageSetDocumentContentFrameId :: PageFrameId,
   pPageSetDocumentContentHtml :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetDocumentContent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageSetDocumentContent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


pageSetDocumentContent :: Handle ev -> PPageSetDocumentContent -> IO (Maybe Error)
pageSetDocumentContent handle params = sendReceiveCommand handle "Page.setDocumentContent" (Just params)



data PPageSetLifecycleEventsEnabled = PPageSetLifecycleEventsEnabled {
   pPageSetLifecycleEventsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetLifecycleEventsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PPageSetLifecycleEventsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


pageSetLifecycleEventsEnabled :: Handle ev -> PPageSetLifecycleEventsEnabled -> IO (Maybe Error)
pageSetLifecycleEventsEnabled handle params = sendReceiveCommand handle "Page.setLifecycleEventsEnabled" (Just params)


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
   pPageStartScreencastFormat :: PPageStartScreencastFormat,
   pPageStartScreencastQuality :: Maybe Int,
   pPageStartScreencastMaxWidth :: Maybe Int,
   pPageStartScreencastMaxHeight :: Maybe Int,
   pPageStartScreencastEveryNthFrame :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageStartScreencast  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PPageStartScreencast where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


pageStartScreencast :: Handle ev -> PPageStartScreencast -> IO (Maybe Error)
pageStartScreencast handle params = sendReceiveCommand handle "Page.startScreencast" (Just params)


pageStopLoading :: Handle ev -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())


pageCrash :: Handle ev -> IO (Maybe Error)
pageCrash handle = sendReceiveCommand handle "Page.crash" (Nothing :: Maybe ())


pageClose :: Handle ev -> IO (Maybe Error)
pageClose handle = sendReceiveCommand handle "Page.close" (Nothing :: Maybe ())


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
   pPageSetWebLifecycleStateState :: PPageSetWebLifecycleStateState
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetWebLifecycleState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PPageSetWebLifecycleState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


pageSetWebLifecycleState :: Handle ev -> PPageSetWebLifecycleState -> IO (Maybe Error)
pageSetWebLifecycleState handle params = sendReceiveCommand handle "Page.setWebLifecycleState" (Just params)


pageStopScreencast :: Handle ev -> IO (Maybe Error)
pageStopScreencast handle = sendReceiveCommand handle "Page.stopScreencast" (Nothing :: Maybe ())



data PPageProduceCompilationCache = PPageProduceCompilationCache {
   pPageProduceCompilationCacheScripts :: [PageCompilationCacheParams]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageProduceCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PPageProduceCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


pageProduceCompilationCache :: Handle ev -> PPageProduceCompilationCache -> IO (Maybe Error)
pageProduceCompilationCache handle params = sendReceiveCommand handle "Page.produceCompilationCache" (Just params)



data PPageAddCompilationCache = PPageAddCompilationCache {
   pPageAddCompilationCacheUrl :: String,
   pPageAddCompilationCacheData :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageAddCompilationCache  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PPageAddCompilationCache where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


pageAddCompilationCache :: Handle ev -> PPageAddCompilationCache -> IO (Maybe Error)
pageAddCompilationCache handle params = sendReceiveCommand handle "Page.addCompilationCache" (Just params)


pageClearCompilationCache :: Handle ev -> IO (Maybe Error)
pageClearCompilationCache handle = sendReceiveCommand handle "Page.clearCompilationCache" (Nothing :: Maybe ())


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


pageSetSpcTransactionMode :: Handle ev -> PPageSetSpcTransactionMode -> IO (Maybe Error)
pageSetSpcTransactionMode handle params = sendReceiveCommand handle "Page.setSPCTransactionMode" (Just params)



data PPageGenerateTestReport = PPageGenerateTestReport {
   pPageGenerateTestReportMessage :: String,
   pPageGenerateTestReportGroup :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageGenerateTestReport  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PPageGenerateTestReport where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


pageGenerateTestReport :: Handle ev -> PPageGenerateTestReport -> IO (Maybe Error)
pageGenerateTestReport handle params = sendReceiveCommand handle "Page.generateTestReport" (Just params)


pageWaitForDebugger :: Handle ev -> IO (Maybe Error)
pageWaitForDebugger handle = sendReceiveCommand handle "Page.waitForDebugger" (Nothing :: Maybe ())



data PPageSetInterceptFileChooserDialog = PPageSetInterceptFileChooserDialog {
   pPageSetInterceptFileChooserDialogEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPageSetInterceptFileChooserDialog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  PPageSetInterceptFileChooserDialog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


pageSetInterceptFileChooserDialog :: Handle ev -> PPageSetInterceptFileChooserDialog -> IO (Maybe Error)
pageSetInterceptFileChooserDialog handle params = sendReceiveCommand handle "Page.setInterceptFileChooserDialog" (Just params)



type SecurityCertificateId = Int
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



data SecurityCertificateSecurityState = SecurityCertificateSecurityState {
   securityCertificateSecurityStateProtocol :: String,
   securityCertificateSecurityStateKeyExchange :: String,
   securityCertificateSecurityStateKeyExchangeGroup :: Maybe String,
   securityCertificateSecurityStateCipher :: String,
   securityCertificateSecurityStateMac :: Maybe String,
   securityCertificateSecurityStateCertificate :: [String],
   securityCertificateSecurityStateSubjectName :: String,
   securityCertificateSecurityStateIssuer :: String,
   securityCertificateSecurityStateValidFrom :: NetworkTimeSinceEpoch,
   securityCertificateSecurityStateValidTo :: NetworkTimeSinceEpoch,
   securityCertificateSecurityStateCertificateNetworkError :: Maybe String,
   securityCertificateSecurityStateCertificateHasWeakSignature :: Bool,
   securityCertificateSecurityStateCertificateHasSha1Signature :: Bool,
   securityCertificateSecurityStateModernSsl :: Bool,
   securityCertificateSecurityStateObsoleteSslProtocol :: Bool,
   securityCertificateSecurityStateObsoleteSslKeyExchange :: Bool,
   securityCertificateSecurityStateObsoleteSslCipher :: Bool,
   securityCertificateSecurityStateObsoleteSslSignature :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityCertificateSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecurityCertificateSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


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



data SecuritySafetyTipInfo = SecuritySafetyTipInfo {
   securitySafetyTipInfoSafetyTipStatus :: SecuritySafetyTipStatus,
   securitySafetyTipInfoSafeUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySafetyTipInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  SecuritySafetyTipInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data SecurityVisibleSecurityState = SecurityVisibleSecurityState {
   securityVisibleSecurityStateSecurityState :: SecuritySecurityState,
   securityVisibleSecurityStateCertificateSecurityState :: Maybe SecurityCertificateSecurityState,
   securityVisibleSecurityStateSafetyTipInfo :: Maybe SecuritySafetyTipInfo,
   securityVisibleSecurityStateSecurityStateIssueIds :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
   securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
   securitySecurityStateExplanationTitle :: String,
   securitySecurityStateExplanationSummary :: String,
   securitySecurityStateExplanationDescription :: String,
   securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
   securitySecurityStateExplanationCertificate :: [String],
   securitySecurityStateExplanationRecommendations :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecuritySecurityStateExplanation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  SecuritySecurityStateExplanation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


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





data SecurityVisibleSecurityStateChanged = SecurityVisibleSecurityStateChanged {
   securityVisibleSecurityStateChangedVisibleSecurityState :: SecurityVisibleSecurityState
} deriving (Generic, Eq, Show, Read)
instance ToJSON SecurityVisibleSecurityStateChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  SecurityVisibleSecurityStateChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }




securityDisable :: Handle ev -> IO (Maybe Error)
securityDisable handle = sendReceiveCommand handle "Security.disable" (Nothing :: Maybe ())


securityEnable :: Handle ev -> IO (Maybe Error)
securityEnable handle = sendReceiveCommand handle "Security.enable" (Nothing :: Maybe ())



data PSecuritySetIgnoreCertificateErrors = PSecuritySetIgnoreCertificateErrors {
   pSecuritySetIgnoreCertificateErrorsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PSecuritySetIgnoreCertificateErrors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PSecuritySetIgnoreCertificateErrors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


securitySetIgnoreCertificateErrors :: Handle ev -> PSecuritySetIgnoreCertificateErrors -> IO (Maybe Error)
securitySetIgnoreCertificateErrors handle params = sendReceiveCommand handle "Security.setIgnoreCertificateErrors" (Just params)



