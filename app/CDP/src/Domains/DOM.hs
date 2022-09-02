{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.DOM (module Domains.DOM) where

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

import Utils

import qualified Domains.Runtime as Runtime


data DOMEvent = EVDOMAttributeModified DOMAttributeModified | EVDOMAttributeRemoved DOMAttributeRemoved | EVDOMCharacterDataModified DOMCharacterDataModified | EVDOMChildNodeCountUpdated DOMChildNodeCountUpdated | EVDOMChildNodeInserted DOMChildNodeInserted | EVDOMChildNodeRemoved DOMChildNodeRemoved | EVDOMDocumentUpdated DOMDocumentUpdated | EVDOMSetChildNodes DOMSetChildNodes
    deriving (Eq, Show, Read)

data DOMAttributeModified = DOMAttributeModified {
    domAttributeModifiedNodeId :: DOMNodeId,
    domAttributeModifiedName :: String,
    domAttributeModifiedValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMAttributeModified where
    parseJSON = A.withObject "DOMAttributeModified" $ \v ->
         DOMAttributeModified <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"


instance ToJSON DOMAttributeModified  where
    toJSON v = A.object
        [ "nodeId" .= domAttributeModifiedNodeId v
        , "name" .= domAttributeModifiedName v
        , "value" .= domAttributeModifiedValue v
        ]


instance FromEvent DOMEvent DOMAttributeModified where
    eventName  _ _    =  "DOM.attributeModified"
    fromEvent ev =  case ev of EVDOMAttributeModified v -> Just v; _ -> Nothing


data DOMAttributeRemoved = DOMAttributeRemoved {
    domAttributeRemovedNodeId :: DOMNodeId,
    domAttributeRemovedName :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMAttributeRemoved where
    parseJSON = A.withObject "DOMAttributeRemoved" $ \v ->
         DOMAttributeRemoved <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON DOMAttributeRemoved  where
    toJSON v = A.object
        [ "nodeId" .= domAttributeRemovedNodeId v
        , "name" .= domAttributeRemovedName v
        ]


instance FromEvent DOMEvent DOMAttributeRemoved where
    eventName  _ _    =  "DOM.attributeRemoved"
    fromEvent ev =  case ev of EVDOMAttributeRemoved v -> Just v; _ -> Nothing


data DOMCharacterDataModified = DOMCharacterDataModified {
    domCharacterDataModifiedNodeId :: DOMNodeId,
    domCharacterDataModifiedCharacterData :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMCharacterDataModified where
    parseJSON = A.withObject "DOMCharacterDataModified" $ \v ->
         DOMCharacterDataModified <$> v .:  "nodeId"
            <*> v  .:  "characterData"


instance ToJSON DOMCharacterDataModified  where
    toJSON v = A.object
        [ "nodeId" .= domCharacterDataModifiedNodeId v
        , "characterData" .= domCharacterDataModifiedCharacterData v
        ]


instance FromEvent DOMEvent DOMCharacterDataModified where
    eventName  _ _    =  "DOM.characterDataModified"
    fromEvent ev =  case ev of EVDOMCharacterDataModified v -> Just v; _ -> Nothing


data DOMChildNodeCountUpdated = DOMChildNodeCountUpdated {
    domChildNodeCountUpdatedNodeId :: DOMNodeId,
    domChildNodeCountUpdatedChildNodeCount :: Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeCountUpdated where
    parseJSON = A.withObject "DOMChildNodeCountUpdated" $ \v ->
         DOMChildNodeCountUpdated <$> v .:  "nodeId"
            <*> v  .:  "childNodeCount"


instance ToJSON DOMChildNodeCountUpdated  where
    toJSON v = A.object
        [ "nodeId" .= domChildNodeCountUpdatedNodeId v
        , "childNodeCount" .= domChildNodeCountUpdatedChildNodeCount v
        ]


instance FromEvent DOMEvent DOMChildNodeCountUpdated where
    eventName  _ _    =  "DOM.childNodeCountUpdated"
    fromEvent ev =  case ev of EVDOMChildNodeCountUpdated v -> Just v; _ -> Nothing


data DOMChildNodeInserted = DOMChildNodeInserted {
    domChildNodeInsertedParentNodeId :: DOMNodeId,
    domChildNodeInsertedPreviousNodeId :: DOMNodeId,
    domChildNodeInsertedNode :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeInserted where
    parseJSON = A.withObject "DOMChildNodeInserted" $ \v ->
         DOMChildNodeInserted <$> v .:  "parentNodeId"
            <*> v  .:  "previousNodeId"
            <*> v  .:  "node"


instance ToJSON DOMChildNodeInserted  where
    toJSON v = A.object
        [ "parentNodeId" .= domChildNodeInsertedParentNodeId v
        , "previousNodeId" .= domChildNodeInsertedPreviousNodeId v
        , "node" .= domChildNodeInsertedNode v
        ]


instance FromEvent DOMEvent DOMChildNodeInserted where
    eventName  _ _    =  "DOM.childNodeInserted"
    fromEvent ev =  case ev of EVDOMChildNodeInserted v -> Just v; _ -> Nothing


data DOMChildNodeRemoved = DOMChildNodeRemoved {
    domChildNodeRemovedParentNodeId :: DOMNodeId,
    domChildNodeRemovedNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMChildNodeRemoved where
    parseJSON = A.withObject "DOMChildNodeRemoved" $ \v ->
         DOMChildNodeRemoved <$> v .:  "parentNodeId"
            <*> v  .:  "nodeId"


instance ToJSON DOMChildNodeRemoved  where
    toJSON v = A.object
        [ "parentNodeId" .= domChildNodeRemovedParentNodeId v
        , "nodeId" .= domChildNodeRemovedNodeId v
        ]


instance FromEvent DOMEvent DOMChildNodeRemoved where
    eventName  _ _    =  "DOM.childNodeRemoved"
    fromEvent ev =  case ev of EVDOMChildNodeRemoved v -> Just v; _ -> Nothing


data DOMDocumentUpdated = DOMDocumentUpdated
    deriving (Eq, Show, Read)
instance FromJSON DOMDocumentUpdated where
    parseJSON = A.withText  "DOMDocumentUpdated"  $ \v -> do
        case v of
                "DOMDocumentUpdated" -> pure $ DOMDocumentUpdated
                _ -> fail "failed to parse DOMDocumentUpdated"

instance FromEvent DOMEvent DOMDocumentUpdated where
    eventName  _ _    =  "DOM.documentUpdated"
    fromEvent ev =  case ev of EVDOMDocumentUpdated v -> Just v; _ -> Nothing


data DOMSetChildNodes = DOMSetChildNodes {
    domSetChildNodesParentId :: DOMNodeId,
    domSetChildNodesNodes :: [DOMNode]
} deriving (Eq, Show, Read)
instance FromJSON  DOMSetChildNodes where
    parseJSON = A.withObject "DOMSetChildNodes" $ \v ->
         DOMSetChildNodes <$> v .:  "parentId"
            <*> v  .:  "nodes"


instance ToJSON DOMSetChildNodes  where
    toJSON v = A.object
        [ "parentId" .= domSetChildNodesParentId v
        , "nodes" .= domSetChildNodesNodes v
        ]


instance FromEvent DOMEvent DOMSetChildNodes where
    eventName  _ _    =  "DOM.setChildNodes"
    fromEvent ev =  case ev of EVDOMSetChildNodes v -> Just v; _ -> Nothing




subscribe :: forall a. FromEvent DOMEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy DOMEvent
    pa       = Proxy :: Proxy a


type DOMNodeId = Int

type DOMBackendNodeId = Int

data DOMBackendNode = DOMBackendNode {
    domBackendNodeNodeType :: Int,
    domBackendNodeNodeName :: String,
    domBackendNodeBackendNodeId :: DOMBackendNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMBackendNode where
    parseJSON = A.withObject "DOMBackendNode" $ \v ->
         DOMBackendNode <$> v .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "backendNodeId"


instance ToJSON DOMBackendNode  where
    toJSON v = A.object
        [ "nodeType" .= domBackendNodeNodeType v
        , "nodeName" .= domBackendNodeNodeName v
        , "backendNodeId" .= domBackendNodeBackendNodeId v
        ]



data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
    deriving (Eq, Show, Read)
instance FromJSON DOMPseudoType where
    parseJSON = A.withText  "DOMPseudoType"  $ \v -> do
        case v of
                "first-line" -> pure $ DOMPseudoTypeFirstLine
                "first-letter" -> pure $ DOMPseudoTypeFirstLetter
                "before" -> pure $ DOMPseudoTypeBefore
                "after" -> pure $ DOMPseudoTypeAfter
                "marker" -> pure $ DOMPseudoTypeMarker
                "backdrop" -> pure $ DOMPseudoTypeBackdrop
                "selection" -> pure $ DOMPseudoTypeSelection
                "target-text" -> pure $ DOMPseudoTypeTargetText
                "spelling-error" -> pure $ DOMPseudoTypeSpellingError
                "grammar-error" -> pure $ DOMPseudoTypeGrammarError
                "highlight" -> pure $ DOMPseudoTypeHighlight
                "first-line-inherited" -> pure $ DOMPseudoTypeFirstLineInherited
                "scrollbar" -> pure $ DOMPseudoTypeScrollbar
                "scrollbar-thumb" -> pure $ DOMPseudoTypeScrollbarThumb
                "scrollbar-button" -> pure $ DOMPseudoTypeScrollbarButton
                "scrollbar-track" -> pure $ DOMPseudoTypeScrollbarTrack
                "scrollbar-track-piece" -> pure $ DOMPseudoTypeScrollbarTrackPiece
                "scrollbar-corner" -> pure $ DOMPseudoTypeScrollbarCorner
                "resizer" -> pure $ DOMPseudoTypeResizer
                "input-list-button" -> pure $ DOMPseudoTypeInputListButton
                "page-transition" -> pure $ DOMPseudoTypePageTransition
                "page-transition-container" -> pure $ DOMPseudoTypePageTransitionContainer
                "page-transition-image-wrapper" -> pure $ DOMPseudoTypePageTransitionImageWrapper
                "page-transition-outgoing-image" -> pure $ DOMPseudoTypePageTransitionOutgoingImage
                "page-transition-incoming-image" -> pure $ DOMPseudoTypePageTransitionIncomingImage
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



data DOMShadowRootType = DOMShadowRootTypeUserAgent | DOMShadowRootTypeOpen | DOMShadowRootTypeClosed
    deriving (Eq, Show, Read)
instance FromJSON DOMShadowRootType where
    parseJSON = A.withText  "DOMShadowRootType"  $ \v -> do
        case v of
                "user-agent" -> pure $ DOMShadowRootTypeUserAgent
                "open" -> pure $ DOMShadowRootTypeOpen
                "closed" -> pure $ DOMShadowRootTypeClosed
                _ -> fail "failed to parse DOMShadowRootType"

instance ToJSON DOMShadowRootType where
    toJSON v = A.String $
        case v of
                DOMShadowRootTypeUserAgent -> "user-agent"
                DOMShadowRootTypeOpen -> "open"
                DOMShadowRootTypeClosed -> "closed"



data DOMCompatibilityMode = DOMCompatibilityModeQuirksMode | DOMCompatibilityModeLimitedQuirksMode | DOMCompatibilityModeNoQuirksMode
    deriving (Eq, Show, Read)
instance FromJSON DOMCompatibilityMode where
    parseJSON = A.withText  "DOMCompatibilityMode"  $ \v -> do
        case v of
                "QuirksMode" -> pure $ DOMCompatibilityModeQuirksMode
                "LimitedQuirksMode" -> pure $ DOMCompatibilityModeLimitedQuirksMode
                "NoQuirksMode" -> pure $ DOMCompatibilityModeNoQuirksMode
                _ -> fail "failed to parse DOMCompatibilityMode"

instance ToJSON DOMCompatibilityMode where
    toJSON v = A.String $
        case v of
                DOMCompatibilityModeQuirksMode -> "QuirksMode"
                DOMCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
                DOMCompatibilityModeNoQuirksMode -> "NoQuirksMode"



data DOMNode = DOMNode {
    domNodeNodeId :: DOMNodeId,
    domNodeBackendNodeId :: DOMBackendNodeId,
    domNodeNodeType :: Int,
    domNodeNodeName :: String,
    domNodeLocalName :: String,
    domNodeNodeValue :: String,
    domNodeParentId :: Maybe DOMNodeId,
    domNodeChildNodeCount :: Maybe Int,
    domNodeChildren :: Maybe [DOMNode],
    domNodeAttributes :: Maybe [String],
    domNodeDocumentUrl :: Maybe String,
    domNodeBaseUrl :: Maybe String,
    domNodePublicId :: Maybe String,
    domNodeSystemId :: Maybe String,
    domNodeInternalSubset :: Maybe String,
    domNodeXmlVersion :: Maybe String,
    domNodeName :: Maybe String,
    domNodeValue :: Maybe String,
    domNodePseudoType :: Maybe DOMPseudoType,
    domNodeShadowRootType :: Maybe DOMShadowRootType,
    domNodeFrameId :: Maybe PageFrameId,
    domNodeContentDocument :: Maybe DOMNode,
    domNodeShadowRoots :: Maybe [DOMNode],
    domNodeTemplateContent :: Maybe DOMNode,
    domNodePseudoElements :: Maybe [DOMNode],
    domNodeDistributedNodes :: Maybe [DOMBackendNode],
    domNodeIsSvg :: Maybe Bool,
    domNodeCompatibilityMode :: Maybe DOMCompatibilityMode,
    domNodeAssignedSlot :: Maybe DOMBackendNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMNode where
    parseJSON = A.withObject "DOMNode" $ \v ->
         DOMNode <$> v .:  "nodeId"
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


instance ToJSON DOMNode  where
    toJSON v = A.object
        [ "nodeId" .= domNodeNodeId v
        , "backendNodeId" .= domNodeBackendNodeId v
        , "nodeType" .= domNodeNodeType v
        , "nodeName" .= domNodeNodeName v
        , "localName" .= domNodeLocalName v
        , "nodeValue" .= domNodeNodeValue v
        , "parentId" .= domNodeParentId v
        , "childNodeCount" .= domNodeChildNodeCount v
        , "children" .= domNodeChildren v
        , "attributes" .= domNodeAttributes v
        , "documentURL" .= domNodeDocumentUrl v
        , "baseURL" .= domNodeBaseUrl v
        , "publicId" .= domNodePublicId v
        , "systemId" .= domNodeSystemId v
        , "internalSubset" .= domNodeInternalSubset v
        , "xmlVersion" .= domNodeXmlVersion v
        , "name" .= domNodeName v
        , "value" .= domNodeValue v
        , "pseudoType" .= domNodePseudoType v
        , "shadowRootType" .= domNodeShadowRootType v
        , "frameId" .= domNodeFrameId v
        , "contentDocument" .= domNodeContentDocument v
        , "shadowRoots" .= domNodeShadowRoots v
        , "templateContent" .= domNodeTemplateContent v
        , "pseudoElements" .= domNodePseudoElements v
        , "distributedNodes" .= domNodeDistributedNodes v
        , "isSVG" .= domNodeIsSvg v
        , "compatibilityMode" .= domNodeCompatibilityMode v
        , "assignedSlot" .= domNodeAssignedSlot v
        ]



data DOMRGBA = DOMRGBA {
    domrgbar :: Int,
    domrgbag :: Int,
    domrgbab :: Int,
    domrgbaa :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMRGBA where
    parseJSON = A.withObject "DOMRGBA" $ \v ->
         DOMRGBA <$> v .:  "r"
            <*> v  .:  "g"
            <*> v  .:  "b"
            <*> v  .:?  "a"


instance ToJSON DOMRGBA  where
    toJSON v = A.object
        [ "r" .= domrgbar v
        , "g" .= domrgbag v
        , "b" .= domrgbab v
        , "a" .= domrgbaa v
        ]



type DOMQuad = [Int]

data DOMBoxModel = DOMBoxModel {
    domBoxModelContent :: DOMQuad,
    domBoxModelPadding :: DOMQuad,
    domBoxModelBorder :: DOMQuad,
    domBoxModelMargin :: DOMQuad,
    domBoxModelWidth :: Int,
    domBoxModelHeight :: Int,
    domBoxModelShapeOutside :: Maybe DOMShapeOutsideInfo
} deriving (Eq, Show, Read)
instance FromJSON  DOMBoxModel where
    parseJSON = A.withObject "DOMBoxModel" $ \v ->
         DOMBoxModel <$> v .:  "content"
            <*> v  .:  "padding"
            <*> v  .:  "border"
            <*> v  .:  "margin"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:?  "shapeOutside"


instance ToJSON DOMBoxModel  where
    toJSON v = A.object
        [ "content" .= domBoxModelContent v
        , "padding" .= domBoxModelPadding v
        , "border" .= domBoxModelBorder v
        , "margin" .= domBoxModelMargin v
        , "width" .= domBoxModelWidth v
        , "height" .= domBoxModelHeight v
        , "shapeOutside" .= domBoxModelShapeOutside v
        ]



data DOMShapeOutsideInfo = DOMShapeOutsideInfo {
    domShapeOutsideInfoBounds :: DOMQuad,
    domShapeOutsideInfoShape :: [Int],
    domShapeOutsideInfoMarginShape :: [Int]
} deriving (Eq, Show, Read)
instance FromJSON  DOMShapeOutsideInfo where
    parseJSON = A.withObject "DOMShapeOutsideInfo" $ \v ->
         DOMShapeOutsideInfo <$> v .:  "bounds"
            <*> v  .:  "shape"
            <*> v  .:  "marginShape"


instance ToJSON DOMShapeOutsideInfo  where
    toJSON v = A.object
        [ "bounds" .= domShapeOutsideInfoBounds v
        , "shape" .= domShapeOutsideInfoShape v
        , "marginShape" .= domShapeOutsideInfoMarginShape v
        ]



data DOMRect = DOMRect {
    domRectX :: Int,
    domRectY :: Int,
    domRectWidth :: Int,
    domRectHeight :: Int
} deriving (Eq, Show, Read)
instance FromJSON  DOMRect where
    parseJSON = A.withObject "DOMRect" $ \v ->
         DOMRect <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"


instance ToJSON DOMRect  where
    toJSON v = A.object
        [ "x" .= domRectX v
        , "y" .= domRectY v
        , "width" .= domRectWidth v
        , "height" .= domRectHeight v
        ]



data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty {
    domcssComputedStylePropertyName :: String,
    domcssComputedStylePropertyValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMCSSComputedStyleProperty where
    parseJSON = A.withObject "DOMCSSComputedStyleProperty" $ \v ->
         DOMCSSComputedStyleProperty <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON DOMCSSComputedStyleProperty  where
    toJSON v = A.object
        [ "name" .= domcssComputedStylePropertyName v
        , "value" .= domcssComputedStylePropertyValue v
        ]



data DOMDescribeNode = DOMDescribeNode {
    dOMDescribeNodeNode :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMDescribeNode where
    parseJSON = A.withObject "DOMDescribeNode" $ \v ->
         DOMDescribeNode <$> v .:  "node"



instance Command  DOMDescribeNode where
    commandName _ = "DOM.describeNode"

data PDOMDescribeNode = PDOMDescribeNode {
    pdomDescribeNodeNodeId :: Maybe DOMNodeId,
    pdomDescribeNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pdomDescribeNodeObjectId :: Maybe RuntimeRemoteObjectId,
    pdomDescribeNodeDepth :: Maybe Int,
    pdomDescribeNodePierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMDescribeNode where
    parseJSON = A.withObject "PDOMDescribeNode" $ \v ->
         PDOMDescribeNode <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMDescribeNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomDescribeNodeNodeId v
        , "backendNodeId" .= pdomDescribeNodeBackendNodeId v
        , "objectId" .= pdomDescribeNodeObjectId v
        , "depth" .= pdomDescribeNodeDepth v
        , "pierce" .= pdomDescribeNodePierce v
        ]


dOMDescribeNode :: Session -> PDOMDescribeNode -> IO (Either Error DOMDescribeNode)
dOMDescribeNode session params = sendReceiveCommandResult session "DOM.describeNode" (Just params)




dOMDisable :: Session -> IO (Maybe Error)
dOMDisable session = sendReceiveCommand session "DOM.disable" (Nothing :: Maybe ())




dOMEnable :: Session -> IO (Maybe Error)
dOMEnable session = sendReceiveCommand session "DOM.enable" (Nothing :: Maybe ())



data PDOMFocus = PDOMFocus {
    pdomFocusNodeId :: Maybe DOMNodeId,
    pdomFocusBackendNodeId :: Maybe DOMBackendNodeId,
    pdomFocusObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMFocus where
    parseJSON = A.withObject "PDOMFocus" $ \v ->
         PDOMFocus <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMFocus  where
    toJSON v = A.object
        [ "nodeId" .= pdomFocusNodeId v
        , "backendNodeId" .= pdomFocusBackendNodeId v
        , "objectId" .= pdomFocusObjectId v
        ]


dOMFocus :: Session -> PDOMFocus -> IO (Maybe Error)
dOMFocus session params = sendReceiveCommand session "DOM.focus" (Just params)

data DOMGetAttributes = DOMGetAttributes {
    dOMGetAttributesAttributes :: [String]
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetAttributes where
    parseJSON = A.withObject "DOMGetAttributes" $ \v ->
         DOMGetAttributes <$> v .:  "attributes"



instance Command  DOMGetAttributes where
    commandName _ = "DOM.getAttributes"

data PDOMGetAttributes = PDOMGetAttributes {
    pdomGetAttributesNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetAttributes where
    parseJSON = A.withObject "PDOMGetAttributes" $ \v ->
         PDOMGetAttributes <$> v .:  "nodeId"


instance ToJSON PDOMGetAttributes  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetAttributesNodeId v
        ]


dOMGetAttributes :: Session -> PDOMGetAttributes -> IO (Either Error DOMGetAttributes)
dOMGetAttributes session params = sendReceiveCommandResult session "DOM.getAttributes" (Just params)

data DOMGetBoxModel = DOMGetBoxModel {
    dOMGetBoxModelModel :: DOMBoxModel
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetBoxModel where
    parseJSON = A.withObject "DOMGetBoxModel" $ \v ->
         DOMGetBoxModel <$> v .:  "model"



instance Command  DOMGetBoxModel where
    commandName _ = "DOM.getBoxModel"

data PDOMGetBoxModel = PDOMGetBoxModel {
    pdomGetBoxModelNodeId :: Maybe DOMNodeId,
    pdomGetBoxModelBackendNodeId :: Maybe DOMBackendNodeId,
    pdomGetBoxModelObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetBoxModel where
    parseJSON = A.withObject "PDOMGetBoxModel" $ \v ->
         PDOMGetBoxModel <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMGetBoxModel  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetBoxModelNodeId v
        , "backendNodeId" .= pdomGetBoxModelBackendNodeId v
        , "objectId" .= pdomGetBoxModelObjectId v
        ]


dOMGetBoxModel :: Session -> PDOMGetBoxModel -> IO (Either Error DOMGetBoxModel)
dOMGetBoxModel session params = sendReceiveCommandResult session "DOM.getBoxModel" (Just params)

data DOMGetDocument = DOMGetDocument {
    dOMGetDocumentRoot :: DOMNode
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetDocument where
    parseJSON = A.withObject "DOMGetDocument" $ \v ->
         DOMGetDocument <$> v .:  "root"



instance Command  DOMGetDocument where
    commandName _ = "DOM.getDocument"

data PDOMGetDocument = PDOMGetDocument {
    pdomGetDocumentDepth :: Maybe Int,
    pdomGetDocumentPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetDocument where
    parseJSON = A.withObject "PDOMGetDocument" $ \v ->
         PDOMGetDocument <$> v .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMGetDocument  where
    toJSON v = A.object
        [ "depth" .= pdomGetDocumentDepth v
        , "pierce" .= pdomGetDocumentPierce v
        ]


dOMGetDocument :: Session -> PDOMGetDocument -> IO (Either Error DOMGetDocument)
dOMGetDocument session params = sendReceiveCommandResult session "DOM.getDocument" (Just params)

data DOMGetNodeForLocation = DOMGetNodeForLocation {
    dOMGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    dOMGetNodeForLocationFrameId :: PageFrameId,
    dOMGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetNodeForLocation where
    parseJSON = A.withObject "DOMGetNodeForLocation" $ \v ->
         DOMGetNodeForLocation <$> v .:  "backendNodeId"
            <*> v  .:  "frameId"
            <*> v  .:?  "nodeId"



instance Command  DOMGetNodeForLocation where
    commandName _ = "DOM.getNodeForLocation"

data PDOMGetNodeForLocation = PDOMGetNodeForLocation {
    pdomGetNodeForLocationX :: Int,
    pdomGetNodeForLocationY :: Int,
    pdomGetNodeForLocationIncludeUserAgentShadowDom :: Maybe Bool,
    pdomGetNodeForLocationIgnorePointerEventsNone :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetNodeForLocation where
    parseJSON = A.withObject "PDOMGetNodeForLocation" $ \v ->
         PDOMGetNodeForLocation <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "includeUserAgentShadowDOM"
            <*> v  .:?  "ignorePointerEventsNone"


instance ToJSON PDOMGetNodeForLocation  where
    toJSON v = A.object
        [ "x" .= pdomGetNodeForLocationX v
        , "y" .= pdomGetNodeForLocationY v
        , "includeUserAgentShadowDOM" .= pdomGetNodeForLocationIncludeUserAgentShadowDom v
        , "ignorePointerEventsNone" .= pdomGetNodeForLocationIgnorePointerEventsNone v
        ]


dOMGetNodeForLocation :: Session -> PDOMGetNodeForLocation -> IO (Either Error DOMGetNodeForLocation)
dOMGetNodeForLocation session params = sendReceiveCommandResult session "DOM.getNodeForLocation" (Just params)

data DOMGetOuterHtml = DOMGetOuterHtml {
    dOMGetOuterHtmlOuterHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  DOMGetOuterHtml where
    parseJSON = A.withObject "DOMGetOuterHtml" $ \v ->
         DOMGetOuterHtml <$> v .:  "outerHTML"



instance Command  DOMGetOuterHtml where
    commandName _ = "DOM.getOuterHTML"

data PDOMGetOuterHtml = PDOMGetOuterHtml {
    pdomGetOuterHtmlNodeId :: Maybe DOMNodeId,
    pdomGetOuterHtmlBackendNodeId :: Maybe DOMBackendNodeId,
    pdomGetOuterHtmlObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMGetOuterHtml where
    parseJSON = A.withObject "PDOMGetOuterHtml" $ \v ->
         PDOMGetOuterHtml <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMGetOuterHtml  where
    toJSON v = A.object
        [ "nodeId" .= pdomGetOuterHtmlNodeId v
        , "backendNodeId" .= pdomGetOuterHtmlBackendNodeId v
        , "objectId" .= pdomGetOuterHtmlObjectId v
        ]


dOMGetOuterHtml :: Session -> PDOMGetOuterHtml -> IO (Either Error DOMGetOuterHtml)
dOMGetOuterHtml session params = sendReceiveCommandResult session "DOM.getOuterHTML" (Just params)




dOMHideHighlight :: Session -> IO (Maybe Error)
dOMHideHighlight session = sendReceiveCommand session "DOM.hideHighlight" (Nothing :: Maybe ())




dOMHighlightNode :: Session -> IO (Maybe Error)
dOMHighlightNode session = sendReceiveCommand session "DOM.highlightNode" (Nothing :: Maybe ())




dOMHighlightRect :: Session -> IO (Maybe Error)
dOMHighlightRect session = sendReceiveCommand session "DOM.highlightRect" (Nothing :: Maybe ())

data DOMMoveTo = DOMMoveTo {
    dOMMoveToNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMMoveTo where
    parseJSON = A.withObject "DOMMoveTo" $ \v ->
         DOMMoveTo <$> v .:  "nodeId"



instance Command  DOMMoveTo where
    commandName _ = "DOM.moveTo"

data PDOMMoveTo = PDOMMoveTo {
    pdomMoveToNodeId :: DOMNodeId,
    pdomMoveToTargetNodeId :: DOMNodeId,
    pdomMoveToInsertBeforeNodeId :: Maybe DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMMoveTo where
    parseJSON = A.withObject "PDOMMoveTo" $ \v ->
         PDOMMoveTo <$> v .:  "nodeId"
            <*> v  .:  "targetNodeId"
            <*> v  .:?  "insertBeforeNodeId"


instance ToJSON PDOMMoveTo  where
    toJSON v = A.object
        [ "nodeId" .= pdomMoveToNodeId v
        , "targetNodeId" .= pdomMoveToTargetNodeId v
        , "insertBeforeNodeId" .= pdomMoveToInsertBeforeNodeId v
        ]


dOMMoveTo :: Session -> PDOMMoveTo -> IO (Either Error DOMMoveTo)
dOMMoveTo session params = sendReceiveCommandResult session "DOM.moveTo" (Just params)

data DOMQuerySelector = DOMQuerySelector {
    dOMQuerySelectorNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMQuerySelector where
    parseJSON = A.withObject "DOMQuerySelector" $ \v ->
         DOMQuerySelector <$> v .:  "nodeId"



instance Command  DOMQuerySelector where
    commandName _ = "DOM.querySelector"

data PDOMQuerySelector = PDOMQuerySelector {
    pdomQuerySelectorNodeId :: DOMNodeId,
    pdomQuerySelectorSelector :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMQuerySelector where
    parseJSON = A.withObject "PDOMQuerySelector" $ \v ->
         PDOMQuerySelector <$> v .:  "nodeId"
            <*> v  .:  "selector"


instance ToJSON PDOMQuerySelector  where
    toJSON v = A.object
        [ "nodeId" .= pdomQuerySelectorNodeId v
        , "selector" .= pdomQuerySelectorSelector v
        ]


dOMQuerySelector :: Session -> PDOMQuerySelector -> IO (Either Error DOMQuerySelector)
dOMQuerySelector session params = sendReceiveCommandResult session "DOM.querySelector" (Just params)

data DOMQuerySelectorAll = DOMQuerySelectorAll {
    dOMQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving (Eq, Show, Read)
instance FromJSON  DOMQuerySelectorAll where
    parseJSON = A.withObject "DOMQuerySelectorAll" $ \v ->
         DOMQuerySelectorAll <$> v .:  "nodeIds"



instance Command  DOMQuerySelectorAll where
    commandName _ = "DOM.querySelectorAll"

data PDOMQuerySelectorAll = PDOMQuerySelectorAll {
    pdomQuerySelectorAllNodeId :: DOMNodeId,
    pdomQuerySelectorAllSelector :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMQuerySelectorAll where
    parseJSON = A.withObject "PDOMQuerySelectorAll" $ \v ->
         PDOMQuerySelectorAll <$> v .:  "nodeId"
            <*> v  .:  "selector"


instance ToJSON PDOMQuerySelectorAll  where
    toJSON v = A.object
        [ "nodeId" .= pdomQuerySelectorAllNodeId v
        , "selector" .= pdomQuerySelectorAllSelector v
        ]


dOMQuerySelectorAll :: Session -> PDOMQuerySelectorAll -> IO (Either Error DOMQuerySelectorAll)
dOMQuerySelectorAll session params = sendReceiveCommandResult session "DOM.querySelectorAll" (Just params)



data PDOMRemoveAttribute = PDOMRemoveAttribute {
    pdomRemoveAttributeNodeId :: DOMNodeId,
    pdomRemoveAttributeName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRemoveAttribute where
    parseJSON = A.withObject "PDOMRemoveAttribute" $ \v ->
         PDOMRemoveAttribute <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON PDOMRemoveAttribute  where
    toJSON v = A.object
        [ "nodeId" .= pdomRemoveAttributeNodeId v
        , "name" .= pdomRemoveAttributeName v
        ]


dOMRemoveAttribute :: Session -> PDOMRemoveAttribute -> IO (Maybe Error)
dOMRemoveAttribute session params = sendReceiveCommand session "DOM.removeAttribute" (Just params)



data PDOMRemoveNode = PDOMRemoveNode {
    pdomRemoveNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRemoveNode where
    parseJSON = A.withObject "PDOMRemoveNode" $ \v ->
         PDOMRemoveNode <$> v .:  "nodeId"


instance ToJSON PDOMRemoveNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomRemoveNodeNodeId v
        ]


dOMRemoveNode :: Session -> PDOMRemoveNode -> IO (Maybe Error)
dOMRemoveNode session params = sendReceiveCommand session "DOM.removeNode" (Just params)



data PDOMRequestChildNodes = PDOMRequestChildNodes {
    pdomRequestChildNodesNodeId :: DOMNodeId,
    pdomRequestChildNodesDepth :: Maybe Int,
    pdomRequestChildNodesPierce :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRequestChildNodes where
    parseJSON = A.withObject "PDOMRequestChildNodes" $ \v ->
         PDOMRequestChildNodes <$> v .:  "nodeId"
            <*> v  .:?  "depth"
            <*> v  .:?  "pierce"


instance ToJSON PDOMRequestChildNodes  where
    toJSON v = A.object
        [ "nodeId" .= pdomRequestChildNodesNodeId v
        , "depth" .= pdomRequestChildNodesDepth v
        , "pierce" .= pdomRequestChildNodesPierce v
        ]


dOMRequestChildNodes :: Session -> PDOMRequestChildNodes -> IO (Maybe Error)
dOMRequestChildNodes session params = sendReceiveCommand session "DOM.requestChildNodes" (Just params)

data DOMRequestNode = DOMRequestNode {
    dOMRequestNodeNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMRequestNode where
    parseJSON = A.withObject "DOMRequestNode" $ \v ->
         DOMRequestNode <$> v .:  "nodeId"



instance Command  DOMRequestNode where
    commandName _ = "DOM.requestNode"

data PDOMRequestNode = PDOMRequestNode {
    pdomRequestNodeObjectId :: RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMRequestNode where
    parseJSON = A.withObject "PDOMRequestNode" $ \v ->
         PDOMRequestNode <$> v .:  "objectId"


instance ToJSON PDOMRequestNode  where
    toJSON v = A.object
        [ "objectId" .= pdomRequestNodeObjectId v
        ]


dOMRequestNode :: Session -> PDOMRequestNode -> IO (Either Error DOMRequestNode)
dOMRequestNode session params = sendReceiveCommandResult session "DOM.requestNode" (Just params)

data DOMResolveNode = DOMResolveNode {
    dOMResolveNodeObject :: RuntimeRemoteObject
} deriving (Eq, Show, Read)
instance FromJSON  DOMResolveNode where
    parseJSON = A.withObject "DOMResolveNode" $ \v ->
         DOMResolveNode <$> v .:  "object"



instance Command  DOMResolveNode where
    commandName _ = "DOM.resolveNode"

data PDOMResolveNode = PDOMResolveNode {
    pdomResolveNodeNodeId :: Maybe DOMNodeId,
    pdomResolveNodeBackendNodeId :: Maybe DOMBackendNodeId,
    pdomResolveNodeObjectGroup :: Maybe String,
    pdomResolveNodeExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMResolveNode where
    parseJSON = A.withObject "PDOMResolveNode" $ \v ->
         PDOMResolveNode <$> v .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectGroup"
            <*> v  .:?  "executionContextId"


instance ToJSON PDOMResolveNode  where
    toJSON v = A.object
        [ "nodeId" .= pdomResolveNodeNodeId v
        , "backendNodeId" .= pdomResolveNodeBackendNodeId v
        , "objectGroup" .= pdomResolveNodeObjectGroup v
        , "executionContextId" .= pdomResolveNodeExecutionContextId v
        ]


dOMResolveNode :: Session -> PDOMResolveNode -> IO (Either Error DOMResolveNode)
dOMResolveNode session params = sendReceiveCommandResult session "DOM.resolveNode" (Just params)



data PDOMSetAttributeValue = PDOMSetAttributeValue {
    pdomSetAttributeValueNodeId :: DOMNodeId,
    pdomSetAttributeValueName :: String,
    pdomSetAttributeValueValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetAttributeValue where
    parseJSON = A.withObject "PDOMSetAttributeValue" $ \v ->
         PDOMSetAttributeValue <$> v .:  "nodeId"
            <*> v  .:  "name"
            <*> v  .:  "value"


instance ToJSON PDOMSetAttributeValue  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetAttributeValueNodeId v
        , "name" .= pdomSetAttributeValueName v
        , "value" .= pdomSetAttributeValueValue v
        ]


dOMSetAttributeValue :: Session -> PDOMSetAttributeValue -> IO (Maybe Error)
dOMSetAttributeValue session params = sendReceiveCommand session "DOM.setAttributeValue" (Just params)



data PDOMSetAttributesAsText = PDOMSetAttributesAsText {
    pdomSetAttributesAsTextNodeId :: DOMNodeId,
    pdomSetAttributesAsTextText :: String,
    pdomSetAttributesAsTextName :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetAttributesAsText where
    parseJSON = A.withObject "PDOMSetAttributesAsText" $ \v ->
         PDOMSetAttributesAsText <$> v .:  "nodeId"
            <*> v  .:  "text"
            <*> v  .:?  "name"


instance ToJSON PDOMSetAttributesAsText  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetAttributesAsTextNodeId v
        , "text" .= pdomSetAttributesAsTextText v
        , "name" .= pdomSetAttributesAsTextName v
        ]


dOMSetAttributesAsText :: Session -> PDOMSetAttributesAsText -> IO (Maybe Error)
dOMSetAttributesAsText session params = sendReceiveCommand session "DOM.setAttributesAsText" (Just params)



data PDOMSetFileInputFiles = PDOMSetFileInputFiles {
    pdomSetFileInputFilesFiles :: [String],
    pdomSetFileInputFilesNodeId :: Maybe DOMNodeId,
    pdomSetFileInputFilesBackendNodeId :: Maybe DOMBackendNodeId,
    pdomSetFileInputFilesObjectId :: Maybe RuntimeRemoteObjectId
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetFileInputFiles where
    parseJSON = A.withObject "PDOMSetFileInputFiles" $ \v ->
         PDOMSetFileInputFiles <$> v .:  "files"
            <*> v  .:?  "nodeId"
            <*> v  .:?  "backendNodeId"
            <*> v  .:?  "objectId"


instance ToJSON PDOMSetFileInputFiles  where
    toJSON v = A.object
        [ "files" .= pdomSetFileInputFilesFiles v
        , "nodeId" .= pdomSetFileInputFilesNodeId v
        , "backendNodeId" .= pdomSetFileInputFilesBackendNodeId v
        , "objectId" .= pdomSetFileInputFilesObjectId v
        ]


dOMSetFileInputFiles :: Session -> PDOMSetFileInputFiles -> IO (Maybe Error)
dOMSetFileInputFiles session params = sendReceiveCommand session "DOM.setFileInputFiles" (Just params)

data DOMSetNodeName = DOMSetNodeName {
    dOMSetNodeNameNodeId :: DOMNodeId
} deriving (Eq, Show, Read)
instance FromJSON  DOMSetNodeName where
    parseJSON = A.withObject "DOMSetNodeName" $ \v ->
         DOMSetNodeName <$> v .:  "nodeId"



instance Command  DOMSetNodeName where
    commandName _ = "DOM.setNodeName"

data PDOMSetNodeName = PDOMSetNodeName {
    pdomSetNodeNameNodeId :: DOMNodeId,
    pdomSetNodeNameName :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetNodeName where
    parseJSON = A.withObject "PDOMSetNodeName" $ \v ->
         PDOMSetNodeName <$> v .:  "nodeId"
            <*> v  .:  "name"


instance ToJSON PDOMSetNodeName  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetNodeNameNodeId v
        , "name" .= pdomSetNodeNameName v
        ]


dOMSetNodeName :: Session -> PDOMSetNodeName -> IO (Either Error DOMSetNodeName)
dOMSetNodeName session params = sendReceiveCommandResult session "DOM.setNodeName" (Just params)



data PDOMSetNodeValue = PDOMSetNodeValue {
    pdomSetNodeValueNodeId :: DOMNodeId,
    pdomSetNodeValueValue :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetNodeValue where
    parseJSON = A.withObject "PDOMSetNodeValue" $ \v ->
         PDOMSetNodeValue <$> v .:  "nodeId"
            <*> v  .:  "value"


instance ToJSON PDOMSetNodeValue  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetNodeValueNodeId v
        , "value" .= pdomSetNodeValueValue v
        ]


dOMSetNodeValue :: Session -> PDOMSetNodeValue -> IO (Maybe Error)
dOMSetNodeValue session params = sendReceiveCommand session "DOM.setNodeValue" (Just params)



data PDOMSetOuterHtml = PDOMSetOuterHtml {
    pdomSetOuterHtmlNodeId :: DOMNodeId,
    pdomSetOuterHtmlOuterHtml :: String
} deriving (Eq, Show, Read)
instance FromJSON  PDOMSetOuterHtml where
    parseJSON = A.withObject "PDOMSetOuterHtml" $ \v ->
         PDOMSetOuterHtml <$> v .:  "nodeId"
            <*> v  .:  "outerHTML"


instance ToJSON PDOMSetOuterHtml  where
    toJSON v = A.object
        [ "nodeId" .= pdomSetOuterHtmlNodeId v
        , "outerHTML" .= pdomSetOuterHtmlOuterHtml v
        ]


dOMSetOuterHtml :: Session -> PDOMSetOuterHtml -> IO (Maybe Error)
dOMSetOuterHtml session params = sendReceiveCommand session "DOM.setOuterHTML" (Just params)

