{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.DOMPageNetwork (module CDP.Domains.DOMPageNetwork) where

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

import CDP.Domains.Runtime as Runtime
import CDP.Domains.Security as Security


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


data DomDocumentUpdated = DomDocumentUpdated
   deriving (Eq, Show, Read)
instance FromJSON DomDocumentUpdated where
   parseJSON = A.withText  "DomDocumentUpdated"  $ \v -> do
      case v of
         "DomDocumentUpdated" -> pure DomDocumentUpdated
         _ -> fail "failed to parse DomDocumentUpdated"



data DomSetChildNodes = DomSetChildNodes {
   domSetChildNodesParentId :: DomNodeId,
   domSetChildNodesNodes :: [DomNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON DomSetChildNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  DomSetChildNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





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



domDisable :: Handle ev -> IO (Maybe Error)
domDisable handle = sendReceiveCommand handle "DOM.disable" (Nothing :: Maybe ())


domEnable :: Handle ev -> IO (Maybe Error)
domEnable handle = sendReceiveCommand handle "DOM.enable" (Nothing :: Maybe ())



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



domHideHighlight :: Handle ev -> IO (Maybe Error)
domHideHighlight handle = sendReceiveCommand handle "DOM.hideHighlight" (Nothing :: Maybe ())


domHighlightNode :: Handle ev -> IO (Maybe Error)
domHighlightNode handle = sendReceiveCommand handle "DOM.highlightNode" (Nothing :: Maybe ())


domHighlightRect :: Handle ev -> IO (Maybe Error)
domHighlightRect handle = sendReceiveCommand handle "DOM.highlightRect" (Nothing :: Maybe ())



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
   networkResourceTimingSendStart :: Double,
   networkResourceTimingSendEnd :: Double,
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
   networkRequestMixedContentType :: Maybe Security.SecurityMixedContentType,
   networkRequestInitialPriority :: NetworkResourcePriority,
   networkRequestReferrerPolicy :: NetworkRequestReferrerPolicy,
   networkRequestIsLinkPreload :: Maybe Bool
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
   networkSecurityDetailsCertificateId :: Security.SecurityCertificateId,
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
   networkResponseSecurityState :: Security.SecuritySecurityState,
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
   networkCookieSameSite :: Maybe NetworkCookieSameSite
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookie  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  NetworkCookie where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



data NetworkCookieParam = NetworkCookieParam {
   networkCookieParamName :: String,
   networkCookieParamValue :: String,
   networkCookieParamUrl :: Maybe String,
   networkCookieParamDomain :: Maybe String,
   networkCookieParamPath :: Maybe String,
   networkCookieParamSecure :: Maybe Bool,
   networkCookieParamHttpOnly :: Maybe Bool,
   networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
   networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkCookieParam  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  NetworkCookieParam where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }





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
   networkRequestWillBeSentRedirectResponse :: Maybe NetworkResponse,
   networkRequestWillBeSentType :: Maybe NetworkResourceType,
   networkRequestWillBeSentFrameId :: Maybe PageFrameId,
   networkRequestWillBeSentHasUserGesture :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON NetworkRequestWillBeSent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  NetworkRequestWillBeSent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data NetworkResponseReceived = NetworkResponseReceived {
   networkResponseReceivedRequestId :: NetworkRequestId,
   networkResponseReceivedLoaderId :: NetworkLoaderId,
   networkResponseReceivedTimestamp :: NetworkMonotonicTime,
   networkResponseReceivedType :: NetworkResourceType,
   networkResponseReceivedResponse :: NetworkResponse,
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
   pNetworkSetCookieExpires :: Maybe NetworkTimeSinceEpoch
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



data PNetworkSetUserAgentOverride = PNetworkSetUserAgentOverride {
   pNetworkSetUserAgentOverrideUserAgent :: String,
   pNetworkSetUserAgentOverrideAcceptLanguage :: Maybe String,
   pNetworkSetUserAgentOverridePlatform :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PNetworkSetUserAgentOverride  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PNetworkSetUserAgentOverride where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


networkSetUserAgentOverride :: Handle ev -> PNetworkSetUserAgentOverride -> IO (Maybe Error)
networkSetUserAgentOverride handle params = sendReceiveCommand handle "Network.setUserAgentOverride" (Just params)



type PageFrameId = String

data PageFrame = PageFrame {
   pageFrameId :: PageFrameId,
   pageFrameParentId :: Maybe PageFrameId,
   pageFrameLoaderId :: NetworkLoaderId,
   pageFrameName :: Maybe String,
   pageFrameUrl :: String,
   pageFrameSecurityOrigin :: String,
   pageFrameMimeType :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 , A.omitNothingFields = True}

instance FromJSON  PageFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 9 }



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



data PageFrameDetached = PageFrameDetached {
   pageFrameDetachedFrameId :: PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PageFrameDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data PageFrameNavigated = PageFrameNavigated {
   pageFrameNavigatedFrame :: PageFrame
} deriving (Generic, Eq, Show, Read)
instance ToJSON PageFrameNavigated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PageFrameNavigated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


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





data PPageAddScriptToEvaluateOnNewDocument = PPageAddScriptToEvaluateOnNewDocument {
   pPageAddScriptToEvaluateOnNewDocumentSource :: String
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
   pPageCaptureScreenshotClip :: Maybe PageViewport
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
   pageGetAppManifestData :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  PageGetAppManifest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PageGetAppManifest where
   commandName _ = "Page.getAppManifest"



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
   pPageNavigateFrameId :: Maybe PageFrameId
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
   pPagePrintToPdfPreferCssPageSize :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PPagePrintToPdf  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PPagePrintToPdf where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


pagePrintToPdf :: Handle ev -> PPagePrintToPdf -> IO (Either Error PagePrintToPdf)
pagePrintToPdf handle params = sendReceiveCommandResult handle "Page.printToPDF" (Just params)

data PagePrintToPdf = PagePrintToPdf {
   pagePrintToPdfData :: String
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


pageStopLoading :: Handle ev -> IO (Maybe Error)
pageStopLoading handle = sendReceiveCommand handle "Page.stopLoading" (Nothing :: Maybe ())



