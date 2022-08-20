{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.DOM (module Domains.DOM) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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

import qualified Domains.Runtime as Runtime


import Utils

data AttributeModified = AttributeModified {
    attributeModifiedNodeId :: NodeId,
    attributeModifiedName :: String,
    attributeModifiedValue :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AttributeModified where
    parseJSON = A.withObject "AttributeModified" $ \v ->
         AttributeModified <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"


instance ToJSON AttributeModified  where
    toJSON v = A.object
        [ "nodeId" .= attributeModifiedNodeId v
        , "name" .= attributeModifiedName v
        , "value" .= attributeModifiedValue v
        ]


data AttributeRemoved = AttributeRemoved {
    attributeRemovedNodeId :: NodeId,
    attributeRemovedName :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AttributeRemoved where
    parseJSON = A.withObject "AttributeRemoved" $ \v ->
         AttributeRemoved <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON AttributeRemoved  where
    toJSON v = A.object
        [ "nodeId" .= attributeRemovedNodeId v
        , "name" .= attributeRemovedName v
        ]


data CharacterDataModified = CharacterDataModified {
    characterDataModifiedNodeId :: NodeId,
    characterDataModifiedCharacterData :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CharacterDataModified where
    parseJSON = A.withObject "CharacterDataModified" $ \v ->
         CharacterDataModified <$> v .:  "nodeId"
            <*> v  .:  "characterData"


instance ToJSON CharacterDataModified  where
    toJSON v = A.object
        [ "nodeId" .= characterDataModifiedNodeId v
        , "characterData" .= characterDataModifiedCharacterData v
        ]


data ChildNodeCountUpdated = ChildNodeCountUpdated {
    childNodeCountUpdatedNodeId :: NodeId,
    childNodeCountUpdatedChildNodeCount :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ChildNodeCountUpdated where
    parseJSON = A.withObject "ChildNodeCountUpdated" $ \v ->
         ChildNodeCountUpdated <$> v .:  "nodeId"
            <*> v  .:  "childNodeCount"


instance ToJSON ChildNodeCountUpdated  where
    toJSON v = A.object
        [ "nodeId" .= childNodeCountUpdatedNodeId v
        , "childNodeCount" .= childNodeCountUpdatedChildNodeCount v
        ]


data ChildNodeInserted = ChildNodeInserted {
    childNodeInsertedParentNodeId :: NodeId,
    childNodeInsertedPreviousNodeId :: NodeId,
    childNodeInsertedNode :: Node
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ChildNodeInserted where
    parseJSON = A.withObject "ChildNodeInserted" $ \v ->
         ChildNodeInserted <$> v .:  "parentNodeId"
            <*> v  .:  "previousNodeId"
            <*> v  .:  "node"


instance ToJSON ChildNodeInserted  where
    toJSON v = A.object
        [ "parentNodeId" .= childNodeInsertedParentNodeId v
        , "previousNodeId" .= childNodeInsertedPreviousNodeId v
        , "node" .= childNodeInsertedNode v
        ]


data ChildNodeRemoved = ChildNodeRemoved {
    childNodeRemovedParentNodeId :: NodeId,
    childNodeRemovedNodeId :: NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ChildNodeRemoved where
    parseJSON = A.withObject "ChildNodeRemoved" $ \v ->
         ChildNodeRemoved <$> v .:  "parentNodeId"
            <*> v  .:  "nodeId"


instance ToJSON ChildNodeRemoved  where
    toJSON v = A.object
        [ "parentNodeId" .= childNodeRemovedParentNodeId v
        , "nodeId" .= childNodeRemovedNodeId v
        ]


data DocumentUpdated = DocumentUpdated
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON DocumentUpdated where
    parseJSON = A.withText  "DocumentUpdated"  $ \v -> do
        pure $ case v of
                "DocumentUpdated" -> DocumentUpdated
                _ -> error "failed to parse DocumentUpdated"

data SetChildNodes = SetChildNodes {
    setChildNodesParentId :: NodeId,
    setChildNodesNodes :: [Node]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetChildNodes where
    parseJSON = A.withObject "SetChildNodes" $ \v ->
         SetChildNodes <$> v .:  "parentId"
            <*> v  .:  "nodes"


instance ToJSON SetChildNodes  where
    toJSON v = A.object
        [ "parentId" .= setChildNodesParentId v
        , "nodes" .= setChildNodesNodes v
        ]



type NodeId = Int

type BackendNodeId = Int

data BackendNode = BackendNode {
    backendNodeNodeType :: Int,
    backendNodeNodeName :: String,
    backendNodeBackendNodeId :: BackendNodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  BackendNode where
    parseJSON = A.withObject "BackendNode" $ \v ->
         BackendNode <$> v .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "backendNodeId"


instance ToJSON BackendNode  where
    toJSON v = A.object
        [ "nodeType" .= backendNodeNodeType v
        , "nodeName" .= backendNodeNodeName v
        , "backendNodeId" .= backendNodeBackendNodeId v
        ]



data PseudoType = PseudoTypeFirstLine | PseudoTypeFirstLetter | PseudoTypeBefore | PseudoTypeAfter | PseudoTypeMarker | PseudoTypeBackdrop | PseudoTypeSelection | PseudoTypeTargetText | PseudoTypeSpellingError | PseudoTypeGrammarError | PseudoTypeHighlight | PseudoTypeFirstLineInherited | PseudoTypeScrollbar | PseudoTypeScrollbarThumb | PseudoTypeScrollbarButton | PseudoTypeScrollbarTrack | PseudoTypeScrollbarTrackPiece | PseudoTypeScrollbarCorner | PseudoTypeResizer | PseudoTypeInputListButton | PseudoTypePageTransition | PseudoTypePageTransitionContainer | PseudoTypePageTransitionImageWrapper | PseudoTypePageTransitionOutgoingImage | PseudoTypePageTransitionIncomingImage
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON PseudoType where
    parseJSON = A.withText  "PseudoType"  $ \v -> do
        pure $ case v of
                "first-line" -> PseudoTypeFirstLine
                "first-letter" -> PseudoTypeFirstLetter
                "before" -> PseudoTypeBefore
                "after" -> PseudoTypeAfter
                "marker" -> PseudoTypeMarker
                "backdrop" -> PseudoTypeBackdrop
                "selection" -> PseudoTypeSelection
                "target-text" -> PseudoTypeTargetText
                "spelling-error" -> PseudoTypeSpellingError
                "grammar-error" -> PseudoTypeGrammarError
                "highlight" -> PseudoTypeHighlight
                "first-line-inherited" -> PseudoTypeFirstLineInherited
                "scrollbar" -> PseudoTypeScrollbar
                "scrollbar-thumb" -> PseudoTypeScrollbarThumb
                "scrollbar-button" -> PseudoTypeScrollbarButton
                "scrollbar-track" -> PseudoTypeScrollbarTrack
                "scrollbar-track-piece" -> PseudoTypeScrollbarTrackPiece
                "scrollbar-corner" -> PseudoTypeScrollbarCorner
                "resizer" -> PseudoTypeResizer
                "input-list-button" -> PseudoTypeInputListButton
                "page-transition" -> PseudoTypePageTransition
                "page-transition-container" -> PseudoTypePageTransitionContainer
                "page-transition-image-wrapper" -> PseudoTypePageTransitionImageWrapper
                "page-transition-outgoing-image" -> PseudoTypePageTransitionOutgoingImage
                "page-transition-incoming-image" -> PseudoTypePageTransitionIncomingImage
                _ -> error "failed to parse PseudoType"

instance ToJSON PseudoType where
    toJSON v = A.String $
        case v of
                PseudoTypeFirstLine -> "first-line"
                PseudoTypeFirstLetter -> "first-letter"
                PseudoTypeBefore -> "before"
                PseudoTypeAfter -> "after"
                PseudoTypeMarker -> "marker"
                PseudoTypeBackdrop -> "backdrop"
                PseudoTypeSelection -> "selection"
                PseudoTypeTargetText -> "target-text"
                PseudoTypeSpellingError -> "spelling-error"
                PseudoTypeGrammarError -> "grammar-error"
                PseudoTypeHighlight -> "highlight"
                PseudoTypeFirstLineInherited -> "first-line-inherited"
                PseudoTypeScrollbar -> "scrollbar"
                PseudoTypeScrollbarThumb -> "scrollbar-thumb"
                PseudoTypeScrollbarButton -> "scrollbar-button"
                PseudoTypeScrollbarTrack -> "scrollbar-track"
                PseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
                PseudoTypeScrollbarCorner -> "scrollbar-corner"
                PseudoTypeResizer -> "resizer"
                PseudoTypeInputListButton -> "input-list-button"
                PseudoTypePageTransition -> "page-transition"
                PseudoTypePageTransitionContainer -> "page-transition-container"
                PseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
                PseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
                PseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"



data ShadowRootType = ShadowRootTypeUserAgent | ShadowRootTypeOpen | ShadowRootTypeClosed
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON ShadowRootType where
    parseJSON = A.withText  "ShadowRootType"  $ \v -> do
        pure $ case v of
                "user-agent" -> ShadowRootTypeUserAgent
                "open" -> ShadowRootTypeOpen
                "closed" -> ShadowRootTypeClosed
                _ -> error "failed to parse ShadowRootType"

instance ToJSON ShadowRootType where
    toJSON v = A.String $
        case v of
                ShadowRootTypeUserAgent -> "user-agent"
                ShadowRootTypeOpen -> "open"
                ShadowRootTypeClosed -> "closed"



data CompatibilityMode = CompatibilityModeQuirksMode | CompatibilityModeLimitedQuirksMode | CompatibilityModeNoQuirksMode
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON CompatibilityMode where
    parseJSON = A.withText  "CompatibilityMode"  $ \v -> do
        pure $ case v of
                "QuirksMode" -> CompatibilityModeQuirksMode
                "LimitedQuirksMode" -> CompatibilityModeLimitedQuirksMode
                "NoQuirksMode" -> CompatibilityModeNoQuirksMode
                _ -> error "failed to parse CompatibilityMode"

instance ToJSON CompatibilityMode where
    toJSON v = A.String $
        case v of
                CompatibilityModeQuirksMode -> "QuirksMode"
                CompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
                CompatibilityModeNoQuirksMode -> "NoQuirksMode"



data Node = Node {
    nodeNodeId :: NodeId,
    nodeBackendNodeId :: BackendNodeId,
    nodeNodeType :: Int,
    nodeNodeName :: String,
    nodeLocalName :: String,
    nodeNodeValue :: String,
    nodeParentId :: Maybe NodeId,
    nodeChildNodeCount :: Maybe Int,
    nodeChildren :: Maybe [Node],
    nodeAttributes :: Maybe [String],
    nodeDocumentUrl :: Maybe String,
    nodeBaseUrl :: Maybe String,
    nodePublicId :: Maybe String,
    nodeSystemId :: Maybe String,
    nodeInternalSubset :: Maybe String,
    nodeXmlVersion :: Maybe String,
    nodeName :: Maybe String,
    nodeValue :: Maybe String,
    nodePseudoType :: Maybe PseudoType,
    nodeShadowRootType :: Maybe ShadowRootType,
    nodeFrameId :: Maybe Page.FrameId,
    nodeContentDocument :: Maybe Node,
    nodeShadowRoots :: Maybe [Node],
    nodeTemplateContent :: Maybe Node,
    nodePseudoElements :: Maybe [Node],
    nodeDistributedNodes :: Maybe [BackendNode],
    nodeIsSvg :: Maybe Bool,
    nodeCompatibilityMode :: Maybe CompatibilityMode,
    nodeAssignedSlot :: Maybe BackendNode
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Node where
    parseJSON = A.withObject "Node" $ \v ->
         Node <$> v .:  "nodeId"
            <*> v  .:  "backendNodeId"
            <*> v  .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "localName"
            <*> v  .:  "nodeValue"
            <*> v  .:?  "parentId"
            <*> v  .:?  "childNodeCount"
            <*> v  .:?  "children"
            <*> v  .:?  "attributes"
            <*> v  .:?  "documentURL"
            <*> v  .:?  "baseURL"
            <*> v  .:?  "publicId"
            <*> v  .:?  "systemId"
            <*> v  .:?  "internalSubset"
            <*> v  .:?  "xmlVersion"
            <*> v  .:?  "name"
            <*> v  .:?  "value"
            <*> v  .:?  "pseudoType"
            <*> v  .:?  "shadowRootType"
            <*> v  .:?  "frameId"
            <*> v  .:?  "contentDocument"
            <*> v  .:?  "shadowRoots"
            <*> v  .:?  "templateContent"
            <*> v  .:?  "pseudoElements"
            <*> v  .:?  "distributedNodes"
            <*> v  .:?  "isSVG"
            <*> v  .:?  "compatibilityMode"
            <*> v  .:?  "assignedSlot"


instance ToJSON Node  where
    toJSON v = A.object
        [ "nodeId" .= nodeNodeId v
        , "backendNodeId" .= nodeBackendNodeId v
        , "nodeType" .= nodeNodeType v
        , "nodeName" .= nodeNodeName v
        , "localName" .= nodeLocalName v
        , "nodeValue" .= nodeNodeValue v
        , "parentId" .= nodeParentId v
        , "childNodeCount" .= nodeChildNodeCount v
        , "children" .= nodeChildren v
        , "attributes" .= nodeAttributes v
        , "documentURL" .= nodeDocumentUrl v
        , "baseURL" .= nodeBaseUrl v
        , "publicId" .= nodePublicId v
        , "systemId" .= nodeSystemId v
        , "internalSubset" .= nodeInternalSubset v
        , "xmlVersion" .= nodeXmlVersion v
        , "name" .= nodeName v
        , "value" .= nodeValue v
        , "pseudoType" .= nodePseudoType v
        , "shadowRootType" .= nodeShadowRootType v
        , "frameId" .= nodeFrameId v
        , "contentDocument" .= nodeContentDocument v
        , "shadowRoots" .= nodeShadowRoots v
        , "templateContent" .= nodeTemplateContent v
        , "pseudoElements" .= nodePseudoElements v
        , "distributedNodes" .= nodeDistributedNodes v
        , "isSVG" .= nodeIsSvg v
        , "compatibilityMode" .= nodeCompatibilityMode v
        , "assignedSlot" .= nodeAssignedSlot v
        ]



data RGBA = RGBA {
    rgbaR :: Int,
    rgbaG :: Int,
    rgbaB :: Int,
    rgbaA :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RGBA where
    parseJSON = A.withObject "RGBA" $ \v ->
         RGBA <$> v .:  "r"
            <*> v  .:  "g"
            <*> v  .:  "b"
            <*> v  .:?  "a"


instance ToJSON RGBA  where
    toJSON v = A.object
        [ "r" .= rgbaR v
        , "g" .= rgbaG v
        , "b" .= rgbaB v
        , "a" .= rgbaA v
        ]



type Quad = [Int]

data BoxModel = BoxModel {
    boxModelContent :: Quad,
    boxModelPadding :: Quad,
    boxModelBorder :: Quad,
    boxModelMargin :: Quad,
    boxModelWidth :: Int,
    boxModelHeight :: Int,
    boxModelShapeOutside :: Maybe ShapeOutsideInfo
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  BoxModel where
    parseJSON = A.withObject "BoxModel" $ \v ->
         BoxModel <$> v .:  "content"
            <*> v  .:  "padding"
            <*> v  .:  "border"
            <*> v  .:  "margin"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:?  "shapeOutside"


instance ToJSON BoxModel  where
    toJSON v = A.object
        [ "content" .= boxModelContent v
        , "padding" .= boxModelPadding v
        , "border" .= boxModelBorder v
        , "margin" .= boxModelMargin v
        , "width" .= boxModelWidth v
        , "height" .= boxModelHeight v
        , "shapeOutside" .= boxModelShapeOutside v
        ]



data ShapeOutsideInfo = ShapeOutsideInfo {
    shapeOutsideInfoBounds :: Quad,
    shapeOutsideInfoShape :: [Int],
    shapeOutsideInfoMarginShape :: [Int]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ShapeOutsideInfo where
    parseJSON = A.withObject "ShapeOutsideInfo" $ \v ->
         ShapeOutsideInfo <$> v .:  "bounds"
            <*> v  .:  "shape"
            <*> v  .:  "marginShape"


instance ToJSON ShapeOutsideInfo  where
    toJSON v = A.object
        [ "bounds" .= shapeOutsideInfoBounds v
        , "shape" .= shapeOutsideInfoShape v
        , "marginShape" .= shapeOutsideInfoMarginShape v
        ]



data Rect = Rect {
    rectX :: Int,
    rectY :: Int,
    rectWidth :: Int,
    rectHeight :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  Rect where
    parseJSON = A.withObject "Rect" $ \v ->
         Rect <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"


instance ToJSON Rect  where
    toJSON v = A.object
        [ "x" .= rectX v
        , "y" .= rectY v
        , "width" .= rectWidth v
        , "height" .= rectHeight v
        ]



data CSSComputedStyleProperty = CSSComputedStyleProperty {
    cssComputedStylePropertyName :: String,
    cssComputedStylePropertyValue :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CSSComputedStyleProperty where
    parseJSON = A.withObject "CSSComputedStyleProperty" $ \v ->
         CSSComputedStyleProperty <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON CSSComputedStyleProperty  where
    toJSON v = A.object
        [ "name" .= cssComputedStylePropertyName v
        , "value" .= cssComputedStylePropertyValue v
        ]


data DescribeNode = DescribeNode {
    describeNodeNode :: Node
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  DescribeNode where
    parseJSON = A.withObject "DescribeNode" $ \v ->
         DescribeNode <$> v .:  "node"



describeNode :: Session a -> Maybe NodeId -> Maybe BackendNodeId -> Maybe Runtime.RemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error DescribeNode)
describeNode session describeNodeNodeId describeNodeBackendNodeId describeNodeObjectId describeNodeDepth describeNodePierce = sendReceiveCommandResult (conn session) ("DOM","describeNode") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) describeNodeNodeId, fmap (("backendNodeId",) . ToJSONEx) describeNodeBackendNodeId, fmap (("objectId",) . ToJSONEx) describeNodeObjectId, fmap (("depth",) . ToJSONEx) describeNodeDepth, fmap (("pierce",) . ToJSONEx) describeNodePierce]))


disable :: Session a -> IO (Maybe Error)
disable session  = sendReceiveCommand (conn session) ("DOM","disable") ([] ++ (catMaybes []))


enable :: Session a -> IO (Maybe Error)
enable session  = sendReceiveCommand (conn session) ("DOM","enable") ([] ++ (catMaybes []))


focus :: Session a -> Maybe NodeId -> Maybe BackendNodeId -> Maybe Runtime.RemoteObjectId -> IO (Maybe Error)
focus session focusNodeId focusBackendNodeId focusObjectId = sendReceiveCommand (conn session) ("DOM","focus") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) focusNodeId, fmap (("backendNodeId",) . ToJSONEx) focusBackendNodeId, fmap (("objectId",) . ToJSONEx) focusObjectId]))

data GetAttributes = GetAttributes {
    getAttributesAttributes :: [String]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetAttributes where
    parseJSON = A.withObject "GetAttributes" $ \v ->
         GetAttributes <$> v .:  "attributes"



getAttributes :: Session a -> NodeId -> IO (Either Error GetAttributes)
getAttributes session getAttributesNodeId = sendReceiveCommandResult (conn session) ("DOM","getAttributes") ([("nodeId", ToJSONEx getAttributesNodeId)] ++ (catMaybes []))

data GetBoxModel = GetBoxModel {
    getBoxModelModel :: BoxModel
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetBoxModel where
    parseJSON = A.withObject "GetBoxModel" $ \v ->
         GetBoxModel <$> v .:  "model"



getBoxModel :: Session a -> Maybe NodeId -> Maybe BackendNodeId -> Maybe Runtime.RemoteObjectId -> IO (Either Error GetBoxModel)
getBoxModel session getBoxModelNodeId getBoxModelBackendNodeId getBoxModelObjectId = sendReceiveCommandResult (conn session) ("DOM","getBoxModel") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) getBoxModelNodeId, fmap (("backendNodeId",) . ToJSONEx) getBoxModelBackendNodeId, fmap (("objectId",) . ToJSONEx) getBoxModelObjectId]))

data GetDocument = GetDocument {
    getDocumentRoot :: Node
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetDocument where
    parseJSON = A.withObject "GetDocument" $ \v ->
         GetDocument <$> v .:  "root"



getDocument :: Session a -> Maybe Int -> Maybe Bool -> IO (Either Error GetDocument)
getDocument session getDocumentDepth getDocumentPierce = sendReceiveCommandResult (conn session) ("DOM","getDocument") ([] ++ (catMaybes [fmap (("depth",) . ToJSONEx) getDocumentDepth, fmap (("pierce",) . ToJSONEx) getDocumentPierce]))

data GetNodeForLocation = GetNodeForLocation {
    getNodeForLocationBackendNodeId :: BackendNodeId,
    getNodeForLocationFrameId :: Page.FrameId,
    getNodeForLocationNodeId :: Maybe NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetNodeForLocation where
    parseJSON = A.withObject "GetNodeForLocation" $ \v ->
         GetNodeForLocation <$> v .:  "backendNodeId"
            <*> v  .:  "frameId"
            <*> v  .:?  "nodeId"



getNodeForLocation :: Session a -> Int -> Int -> Maybe Bool -> Maybe Bool -> IO (Either Error GetNodeForLocation)
getNodeForLocation session getNodeForLocationX getNodeForLocationY getNodeForLocationIncludeUserAgentShadowDom getNodeForLocationIgnorePointerEventsNone = sendReceiveCommandResult (conn session) ("DOM","getNodeForLocation") ([("x", ToJSONEx getNodeForLocationX), ("y", ToJSONEx getNodeForLocationY)] ++ (catMaybes [fmap (("includeUserAgentShadowDOM",) . ToJSONEx) getNodeForLocationIncludeUserAgentShadowDom, fmap (("ignorePointerEventsNone",) . ToJSONEx) getNodeForLocationIgnorePointerEventsNone]))

data GetOuterHTML = GetOuterHTML {
    getOuterHTMLOuterHtml :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetOuterHTML where
    parseJSON = A.withObject "GetOuterHTML" $ \v ->
         GetOuterHTML <$> v .:  "outerHTML"



getOuterHTML :: Session a -> Maybe NodeId -> Maybe BackendNodeId -> Maybe Runtime.RemoteObjectId -> IO (Either Error GetOuterHTML)
getOuterHTML session getOuterHtmlNodeId getOuterHtmlBackendNodeId getOuterHtmlObjectId = sendReceiveCommandResult (conn session) ("DOM","getOuterHTML") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) getOuterHtmlNodeId, fmap (("backendNodeId",) . ToJSONEx) getOuterHtmlBackendNodeId, fmap (("objectId",) . ToJSONEx) getOuterHtmlObjectId]))


hideHighlight :: Session a -> IO (Maybe Error)
hideHighlight session  = sendReceiveCommand (conn session) ("DOM","hideHighlight") ([] ++ (catMaybes []))


highlightNode :: Session a -> IO (Maybe Error)
highlightNode session  = sendReceiveCommand (conn session) ("DOM","highlightNode") ([] ++ (catMaybes []))


highlightRect :: Session a -> IO (Maybe Error)
highlightRect session  = sendReceiveCommand (conn session) ("DOM","highlightRect") ([] ++ (catMaybes []))

data MoveTo = MoveTo {
    moveToNodeId :: NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  MoveTo where
    parseJSON = A.withObject "MoveTo" $ \v ->
         MoveTo <$> v .:  "nodeId"



moveTo :: Session a -> NodeId -> NodeId -> Maybe NodeId -> IO (Either Error MoveTo)
moveTo session moveToNodeId moveToTargetNodeId moveToInsertBeforeNodeId = sendReceiveCommandResult (conn session) ("DOM","moveTo") ([("nodeId", ToJSONEx moveToNodeId), ("targetNodeId", ToJSONEx moveToTargetNodeId)] ++ (catMaybes [fmap (("insertBeforeNodeId",) . ToJSONEx) moveToInsertBeforeNodeId]))

data QuerySelector = QuerySelector {
    querySelectorNodeId :: NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  QuerySelector where
    parseJSON = A.withObject "QuerySelector" $ \v ->
         QuerySelector <$> v .:  "nodeId"



querySelector :: Session a -> NodeId -> String -> IO (Either Error QuerySelector)
querySelector session querySelectorNodeId querySelectorSelector = sendReceiveCommandResult (conn session) ("DOM","querySelector") ([("nodeId", ToJSONEx querySelectorNodeId), ("selector", ToJSONEx querySelectorSelector)] ++ (catMaybes []))

data QuerySelectorAll = QuerySelectorAll {
    querySelectorAllNodeIds :: [NodeId]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  QuerySelectorAll where
    parseJSON = A.withObject "QuerySelectorAll" $ \v ->
         QuerySelectorAll <$> v .:  "nodeIds"



querySelectorAll :: Session a -> NodeId -> String -> IO (Either Error QuerySelectorAll)
querySelectorAll session querySelectorAllNodeId querySelectorAllSelector = sendReceiveCommandResult (conn session) ("DOM","querySelectorAll") ([("nodeId", ToJSONEx querySelectorAllNodeId), ("selector", ToJSONEx querySelectorAllSelector)] ++ (catMaybes []))


removeAttribute :: Session a -> NodeId -> String -> IO (Maybe Error)
removeAttribute session removeAttributeNodeId removeAttributeName = sendReceiveCommand (conn session) ("DOM","removeAttribute") ([("nodeId", ToJSONEx removeAttributeNodeId), ("name", ToJSONEx removeAttributeName)] ++ (catMaybes []))


removeNode :: Session a -> NodeId -> IO (Maybe Error)
removeNode session removeNodeNodeId = sendReceiveCommand (conn session) ("DOM","removeNode") ([("nodeId", ToJSONEx removeNodeNodeId)] ++ (catMaybes []))


requestChildNodes :: Session a -> NodeId -> Maybe Int -> Maybe Bool -> IO (Maybe Error)
requestChildNodes session requestChildNodesNodeId requestChildNodesDepth requestChildNodesPierce = sendReceiveCommand (conn session) ("DOM","requestChildNodes") ([("nodeId", ToJSONEx requestChildNodesNodeId)] ++ (catMaybes [fmap (("depth",) . ToJSONEx) requestChildNodesDepth, fmap (("pierce",) . ToJSONEx) requestChildNodesPierce]))

data RequestNode = RequestNode {
    requestNodeNodeId :: NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  RequestNode where
    parseJSON = A.withObject "RequestNode" $ \v ->
         RequestNode <$> v .:  "nodeId"



requestNode :: Session a -> Runtime.RemoteObjectId -> IO (Either Error RequestNode)
requestNode session requestNodeObjectId = sendReceiveCommandResult (conn session) ("DOM","requestNode") ([("objectId", ToJSONEx requestNodeObjectId)] ++ (catMaybes []))

data ResolveNode = ResolveNode {
    resolveNodeObject :: Runtime.RemoteObject
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ResolveNode where
    parseJSON = A.withObject "ResolveNode" $ \v ->
         ResolveNode <$> v .:  "object"



resolveNode :: Session a -> Maybe NodeId -> Maybe DOM.BackendNodeId -> Maybe String -> Maybe Runtime.ExecutionContextId -> IO (Either Error ResolveNode)
resolveNode session resolveNodeNodeId resolveNodeBackendNodeId resolveNodeObjectGroup resolveNodeExecutionContextId = sendReceiveCommandResult (conn session) ("DOM","resolveNode") ([] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) resolveNodeNodeId, fmap (("backendNodeId",) . ToJSONEx) resolveNodeBackendNodeId, fmap (("objectGroup",) . ToJSONEx) resolveNodeObjectGroup, fmap (("executionContextId",) . ToJSONEx) resolveNodeExecutionContextId]))


setAttributeValue :: Session a -> NodeId -> String -> String -> IO (Maybe Error)
setAttributeValue session setAttributeValueNodeId setAttributeValueName setAttributeValueValue = sendReceiveCommand (conn session) ("DOM","setAttributeValue") ([("nodeId", ToJSONEx setAttributeValueNodeId), ("name", ToJSONEx setAttributeValueName), ("value", ToJSONEx setAttributeValueValue)] ++ (catMaybes []))


setAttributesAsText :: Session a -> NodeId -> String -> Maybe String -> IO (Maybe Error)
setAttributesAsText session setAttributesAsTextNodeId setAttributesAsTextText setAttributesAsTextName = sendReceiveCommand (conn session) ("DOM","setAttributesAsText") ([("nodeId", ToJSONEx setAttributesAsTextNodeId), ("text", ToJSONEx setAttributesAsTextText)] ++ (catMaybes [fmap (("name",) . ToJSONEx) setAttributesAsTextName]))


setFileInputFiles :: Session a -> [String] -> Maybe NodeId -> Maybe BackendNodeId -> Maybe Runtime.RemoteObjectId -> IO (Maybe Error)
setFileInputFiles session setFileInputFilesFiles setFileInputFilesNodeId setFileInputFilesBackendNodeId setFileInputFilesObjectId = sendReceiveCommand (conn session) ("DOM","setFileInputFiles") ([("files", ToJSONEx setFileInputFilesFiles)] ++ (catMaybes [fmap (("nodeId",) . ToJSONEx) setFileInputFilesNodeId, fmap (("backendNodeId",) . ToJSONEx) setFileInputFilesBackendNodeId, fmap (("objectId",) . ToJSONEx) setFileInputFilesObjectId]))

data SetNodeName = SetNodeName {
    setNodeNameNodeId :: NodeId
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  SetNodeName where
    parseJSON = A.withObject "SetNodeName" $ \v ->
         SetNodeName <$> v .:  "nodeId"



setNodeName :: Session a -> NodeId -> String -> IO (Either Error SetNodeName)
setNodeName session setNodeNameNodeId setNodeNameName = sendReceiveCommandResult (conn session) ("DOM","setNodeName") ([("nodeId", ToJSONEx setNodeNameNodeId), ("name", ToJSONEx setNodeNameName)] ++ (catMaybes []))


setNodeValue :: Session a -> NodeId -> String -> IO (Maybe Error)
setNodeValue session setNodeValueNodeId setNodeValueValue = sendReceiveCommand (conn session) ("DOM","setNodeValue") ([("nodeId", ToJSONEx setNodeValueNodeId), ("value", ToJSONEx setNodeValueValue)] ++ (catMaybes []))


setOuterHTML :: Session a -> NodeId -> String -> IO (Maybe Error)
setOuterHTML session setOuterHtmlNodeId setOuterHtmlOuterHtml = sendReceiveCommand (conn session) ("DOM","setOuterHTML") ([("nodeId", ToJSONEx setOuterHtmlNodeId), ("outerHTML", ToJSONEx setOuterHtmlOuterHtml)] ++ (catMaybes []))


