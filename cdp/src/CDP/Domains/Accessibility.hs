{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Accessibility 
-}


module CDP.Domains.Accessibility (module CDP.Domains.Accessibility) where

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Unique accessibility node identifier.
type AccessibilityAxNodeId = String

-- | Enum of possible property types.
data AccessibilityAxValueType = AccessibilityAxValueTypeBoolean | AccessibilityAxValueTypeTristate | AccessibilityAxValueTypeBooleanOrUndefined | AccessibilityAxValueTypeIdref | AccessibilityAxValueTypeIdrefList | AccessibilityAxValueTypeInteger | AccessibilityAxValueTypeNode | AccessibilityAxValueTypeNodeList | AccessibilityAxValueTypeNumber | AccessibilityAxValueTypeString | AccessibilityAxValueTypeComputedString | AccessibilityAxValueTypeToken | AccessibilityAxValueTypeTokenList | AccessibilityAxValueTypeDomRelation | AccessibilityAxValueTypeRole | AccessibilityAxValueTypeInternalRole | AccessibilityAxValueTypeValueUndefined
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAxValueType where
   parseJSON = A.withText  "AccessibilityAxValueType"  $ \v -> do
      case v of
         "boolean" -> pure AccessibilityAxValueTypeBoolean
         "tristate" -> pure AccessibilityAxValueTypeTristate
         "booleanOrUndefined" -> pure AccessibilityAxValueTypeBooleanOrUndefined
         "idref" -> pure AccessibilityAxValueTypeIdref
         "idrefList" -> pure AccessibilityAxValueTypeIdrefList
         "integer" -> pure AccessibilityAxValueTypeInteger
         "node" -> pure AccessibilityAxValueTypeNode
         "nodeList" -> pure AccessibilityAxValueTypeNodeList
         "number" -> pure AccessibilityAxValueTypeNumber
         "string" -> pure AccessibilityAxValueTypeString
         "computedString" -> pure AccessibilityAxValueTypeComputedString
         "token" -> pure AccessibilityAxValueTypeToken
         "tokenList" -> pure AccessibilityAxValueTypeTokenList
         "domRelation" -> pure AccessibilityAxValueTypeDomRelation
         "role" -> pure AccessibilityAxValueTypeRole
         "internalRole" -> pure AccessibilityAxValueTypeInternalRole
         "valueUndefined" -> pure AccessibilityAxValueTypeValueUndefined
         _ -> fail "failed to parse AccessibilityAxValueType"

instance ToJSON AccessibilityAxValueType where
   toJSON v = A.String $
      case v of
         AccessibilityAxValueTypeBoolean -> "boolean"
         AccessibilityAxValueTypeTristate -> "tristate"
         AccessibilityAxValueTypeBooleanOrUndefined -> "booleanOrUndefined"
         AccessibilityAxValueTypeIdref -> "idref"
         AccessibilityAxValueTypeIdrefList -> "idrefList"
         AccessibilityAxValueTypeInteger -> "integer"
         AccessibilityAxValueTypeNode -> "node"
         AccessibilityAxValueTypeNodeList -> "nodeList"
         AccessibilityAxValueTypeNumber -> "number"
         AccessibilityAxValueTypeString -> "string"
         AccessibilityAxValueTypeComputedString -> "computedString"
         AccessibilityAxValueTypeToken -> "token"
         AccessibilityAxValueTypeTokenList -> "tokenList"
         AccessibilityAxValueTypeDomRelation -> "domRelation"
         AccessibilityAxValueTypeRole -> "role"
         AccessibilityAxValueTypeInternalRole -> "internalRole"
         AccessibilityAxValueTypeValueUndefined -> "valueUndefined"



-- | Enum of possible property sources.
data AccessibilityAxValueSourceType = AccessibilityAxValueSourceTypeAttribute | AccessibilityAxValueSourceTypeImplicit | AccessibilityAxValueSourceTypeStyle | AccessibilityAxValueSourceTypeContents | AccessibilityAxValueSourceTypePlaceholder | AccessibilityAxValueSourceTypeRelatedElement
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAxValueSourceType where
   parseJSON = A.withText  "AccessibilityAxValueSourceType"  $ \v -> do
      case v of
         "attribute" -> pure AccessibilityAxValueSourceTypeAttribute
         "implicit" -> pure AccessibilityAxValueSourceTypeImplicit
         "style" -> pure AccessibilityAxValueSourceTypeStyle
         "contents" -> pure AccessibilityAxValueSourceTypeContents
         "placeholder" -> pure AccessibilityAxValueSourceTypePlaceholder
         "relatedElement" -> pure AccessibilityAxValueSourceTypeRelatedElement
         _ -> fail "failed to parse AccessibilityAxValueSourceType"

instance ToJSON AccessibilityAxValueSourceType where
   toJSON v = A.String $
      case v of
         AccessibilityAxValueSourceTypeAttribute -> "attribute"
         AccessibilityAxValueSourceTypeImplicit -> "implicit"
         AccessibilityAxValueSourceTypeStyle -> "style"
         AccessibilityAxValueSourceTypeContents -> "contents"
         AccessibilityAxValueSourceTypePlaceholder -> "placeholder"
         AccessibilityAxValueSourceTypeRelatedElement -> "relatedElement"



-- | Enum of possible native property sources (as a subtype of a particular AXValueSourceType).
data AccessibilityAxValueNativeSourceType = AccessibilityAxValueNativeSourceTypeDescription | AccessibilityAxValueNativeSourceTypeFigcaption | AccessibilityAxValueNativeSourceTypeLabel | AccessibilityAxValueNativeSourceTypeLabelfor | AccessibilityAxValueNativeSourceTypeLabelwrapped | AccessibilityAxValueNativeSourceTypeLegend | AccessibilityAxValueNativeSourceTypeRubyannotation | AccessibilityAxValueNativeSourceTypeTablecaption | AccessibilityAxValueNativeSourceTypeTitle | AccessibilityAxValueNativeSourceTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAxValueNativeSourceType where
   parseJSON = A.withText  "AccessibilityAxValueNativeSourceType"  $ \v -> do
      case v of
         "description" -> pure AccessibilityAxValueNativeSourceTypeDescription
         "figcaption" -> pure AccessibilityAxValueNativeSourceTypeFigcaption
         "label" -> pure AccessibilityAxValueNativeSourceTypeLabel
         "labelfor" -> pure AccessibilityAxValueNativeSourceTypeLabelfor
         "labelwrapped" -> pure AccessibilityAxValueNativeSourceTypeLabelwrapped
         "legend" -> pure AccessibilityAxValueNativeSourceTypeLegend
         "rubyannotation" -> pure AccessibilityAxValueNativeSourceTypeRubyannotation
         "tablecaption" -> pure AccessibilityAxValueNativeSourceTypeTablecaption
         "title" -> pure AccessibilityAxValueNativeSourceTypeTitle
         "other" -> pure AccessibilityAxValueNativeSourceTypeOther
         _ -> fail "failed to parse AccessibilityAxValueNativeSourceType"

instance ToJSON AccessibilityAxValueNativeSourceType where
   toJSON v = A.String $
      case v of
         AccessibilityAxValueNativeSourceTypeDescription -> "description"
         AccessibilityAxValueNativeSourceTypeFigcaption -> "figcaption"
         AccessibilityAxValueNativeSourceTypeLabel -> "label"
         AccessibilityAxValueNativeSourceTypeLabelfor -> "labelfor"
         AccessibilityAxValueNativeSourceTypeLabelwrapped -> "labelwrapped"
         AccessibilityAxValueNativeSourceTypeLegend -> "legend"
         AccessibilityAxValueNativeSourceTypeRubyannotation -> "rubyannotation"
         AccessibilityAxValueNativeSourceTypeTablecaption -> "tablecaption"
         AccessibilityAxValueNativeSourceTypeTitle -> "title"
         AccessibilityAxValueNativeSourceTypeOther -> "other"



-- | A single source for a computed AX property.
data AccessibilityAxValueSource = AccessibilityAxValueSource {
   accessibilityAxValueSourceType :: AccessibilityAxValueSourceType, -- ^ What type of source this is.
   accessibilityAxValueSourceValue :: AccessibilityAxValueSourceValue, -- ^ The value of this property source.
   accessibilityAxValueSourceAttribute :: AccessibilityAxValueSourceAttribute, -- ^ The name of the relevant attribute, if any.
   accessibilityAxValueSourceAttributeValue :: AccessibilityAxValueSourceAttributeValue, -- ^ The value of the relevant attribute, if any.
   accessibilityAxValueSourceSuperseded :: AccessibilityAxValueSourceSuperseded, -- ^ Whether this source is superseded by a higher priority source.
   accessibilityAxValueSourceNativeSource :: AccessibilityAxValueSourceNativeSource, -- ^ The native markup source for this value, e.g. a <label> element.
   accessibilityAxValueSourceNativeSourceValue :: AccessibilityAxValueSourceNativeSourceValue, -- ^ The value, such as a node or node list, of the native source.
   accessibilityAxValueSourceInvalid :: AccessibilityAxValueSourceInvalid, -- ^ Whether the value for this property is invalid.
   accessibilityAxValueSourceInvalidReason :: AccessibilityAxValueSourceInvalidReason -- ^ Reason for the value being invalid, if it is.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxValueSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxValueSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Accessibility.AXRelatedNode' .
data AccessibilityAxRelatedNode = AccessibilityAxRelatedNode {
   accessibilityAxRelatedNodeBackendDomNodeId :: AccessibilityAxRelatedNodeBackendDomNodeId, -- ^ The BackendNodeId of the related DOM node.
   accessibilityAxRelatedNodeIdref :: AccessibilityAxRelatedNodeIdref, -- ^ The IDRef value provided, if any.
   accessibilityAxRelatedNodeText :: AccessibilityAxRelatedNodeText -- ^ The text alternative of this node in the current context.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxRelatedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxRelatedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Accessibility.AXProperty' .
data AccessibilityAxProperty = AccessibilityAxProperty {
   accessibilityAxPropertyName :: AccessibilityAxPropertyName, -- ^ The name of this property.
   accessibilityAxPropertyValue :: AccessibilityAxPropertyValue -- ^ The value of this property.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | A single computed AX property.
data AccessibilityAxValue = AccessibilityAxValue {
   accessibilityAxValueType :: AccessibilityAxValueType, -- ^ The type of this value.
   accessibilityAxValueValue :: AccessibilityAxValueValue, -- ^ The computed value of this property.
   accessibilityAxValueRelatedNodes :: AccessibilityAxValueRelatedNodes, -- ^ One or more related nodes, if applicable.
   accessibilityAxValueSources :: AccessibilityAxValueSources -- ^ The sources which contributed to the computation of this property.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Values of AXProperty name:
-- - from 'busy' to 'roledescription': states which apply to every AX node
-- - from 'live' to 'root': attributes which apply to nodes in live regions
-- - from 'autocomplete' to 'valuetext': attributes which apply to widgets
-- - from 'checked' to 'selected': states which apply to widgets
-- - from 'activedescendant' to 'owns' - relationships between elements other than parent/child/sibling.
data AccessibilityAxPropertyName = AccessibilityAxPropertyNameBusy | AccessibilityAxPropertyNameDisabled | AccessibilityAxPropertyNameEditable | AccessibilityAxPropertyNameFocusable | AccessibilityAxPropertyNameFocused | AccessibilityAxPropertyNameHidden | AccessibilityAxPropertyNameHiddenRoot | AccessibilityAxPropertyNameInvalid | AccessibilityAxPropertyNameKeyshortcuts | AccessibilityAxPropertyNameSettable | AccessibilityAxPropertyNameRoledescription | AccessibilityAxPropertyNameLive | AccessibilityAxPropertyNameAtomic | AccessibilityAxPropertyNameRelevant | AccessibilityAxPropertyNameRoot | AccessibilityAxPropertyNameAutocomplete | AccessibilityAxPropertyNameHasPopup | AccessibilityAxPropertyNameLevel | AccessibilityAxPropertyNameMultiselectable | AccessibilityAxPropertyNameOrientation | AccessibilityAxPropertyNameMultiline | AccessibilityAxPropertyNameReadonly | AccessibilityAxPropertyNameRequired | AccessibilityAxPropertyNameValuemin | AccessibilityAxPropertyNameValuemax | AccessibilityAxPropertyNameValuetext | AccessibilityAxPropertyNameChecked | AccessibilityAxPropertyNameExpanded | AccessibilityAxPropertyNameModal | AccessibilityAxPropertyNamePressed | AccessibilityAxPropertyNameSelected | AccessibilityAxPropertyNameActivedescendant | AccessibilityAxPropertyNameControls | AccessibilityAxPropertyNameDescribedby | AccessibilityAxPropertyNameDetails | AccessibilityAxPropertyNameErrormessage | AccessibilityAxPropertyNameFlowto | AccessibilityAxPropertyNameLabelledby | AccessibilityAxPropertyNameOwns
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAxPropertyName where
   parseJSON = A.withText  "AccessibilityAxPropertyName"  $ \v -> do
      case v of
         "busy" -> pure AccessibilityAxPropertyNameBusy
         "disabled" -> pure AccessibilityAxPropertyNameDisabled
         "editable" -> pure AccessibilityAxPropertyNameEditable
         "focusable" -> pure AccessibilityAxPropertyNameFocusable
         "focused" -> pure AccessibilityAxPropertyNameFocused
         "hidden" -> pure AccessibilityAxPropertyNameHidden
         "hiddenRoot" -> pure AccessibilityAxPropertyNameHiddenRoot
         "invalid" -> pure AccessibilityAxPropertyNameInvalid
         "keyshortcuts" -> pure AccessibilityAxPropertyNameKeyshortcuts
         "settable" -> pure AccessibilityAxPropertyNameSettable
         "roledescription" -> pure AccessibilityAxPropertyNameRoledescription
         "live" -> pure AccessibilityAxPropertyNameLive
         "atomic" -> pure AccessibilityAxPropertyNameAtomic
         "relevant" -> pure AccessibilityAxPropertyNameRelevant
         "root" -> pure AccessibilityAxPropertyNameRoot
         "autocomplete" -> pure AccessibilityAxPropertyNameAutocomplete
         "hasPopup" -> pure AccessibilityAxPropertyNameHasPopup
         "level" -> pure AccessibilityAxPropertyNameLevel
         "multiselectable" -> pure AccessibilityAxPropertyNameMultiselectable
         "orientation" -> pure AccessibilityAxPropertyNameOrientation
         "multiline" -> pure AccessibilityAxPropertyNameMultiline
         "readonly" -> pure AccessibilityAxPropertyNameReadonly
         "required" -> pure AccessibilityAxPropertyNameRequired
         "valuemin" -> pure AccessibilityAxPropertyNameValuemin
         "valuemax" -> pure AccessibilityAxPropertyNameValuemax
         "valuetext" -> pure AccessibilityAxPropertyNameValuetext
         "checked" -> pure AccessibilityAxPropertyNameChecked
         "expanded" -> pure AccessibilityAxPropertyNameExpanded
         "modal" -> pure AccessibilityAxPropertyNameModal
         "pressed" -> pure AccessibilityAxPropertyNamePressed
         "selected" -> pure AccessibilityAxPropertyNameSelected
         "activedescendant" -> pure AccessibilityAxPropertyNameActivedescendant
         "controls" -> pure AccessibilityAxPropertyNameControls
         "describedby" -> pure AccessibilityAxPropertyNameDescribedby
         "details" -> pure AccessibilityAxPropertyNameDetails
         "errormessage" -> pure AccessibilityAxPropertyNameErrormessage
         "flowto" -> pure AccessibilityAxPropertyNameFlowto
         "labelledby" -> pure AccessibilityAxPropertyNameLabelledby
         "owns" -> pure AccessibilityAxPropertyNameOwns
         _ -> fail "failed to parse AccessibilityAxPropertyName"

instance ToJSON AccessibilityAxPropertyName where
   toJSON v = A.String $
      case v of
         AccessibilityAxPropertyNameBusy -> "busy"
         AccessibilityAxPropertyNameDisabled -> "disabled"
         AccessibilityAxPropertyNameEditable -> "editable"
         AccessibilityAxPropertyNameFocusable -> "focusable"
         AccessibilityAxPropertyNameFocused -> "focused"
         AccessibilityAxPropertyNameHidden -> "hidden"
         AccessibilityAxPropertyNameHiddenRoot -> "hiddenRoot"
         AccessibilityAxPropertyNameInvalid -> "invalid"
         AccessibilityAxPropertyNameKeyshortcuts -> "keyshortcuts"
         AccessibilityAxPropertyNameSettable -> "settable"
         AccessibilityAxPropertyNameRoledescription -> "roledescription"
         AccessibilityAxPropertyNameLive -> "live"
         AccessibilityAxPropertyNameAtomic -> "atomic"
         AccessibilityAxPropertyNameRelevant -> "relevant"
         AccessibilityAxPropertyNameRoot -> "root"
         AccessibilityAxPropertyNameAutocomplete -> "autocomplete"
         AccessibilityAxPropertyNameHasPopup -> "hasPopup"
         AccessibilityAxPropertyNameLevel -> "level"
         AccessibilityAxPropertyNameMultiselectable -> "multiselectable"
         AccessibilityAxPropertyNameOrientation -> "orientation"
         AccessibilityAxPropertyNameMultiline -> "multiline"
         AccessibilityAxPropertyNameReadonly -> "readonly"
         AccessibilityAxPropertyNameRequired -> "required"
         AccessibilityAxPropertyNameValuemin -> "valuemin"
         AccessibilityAxPropertyNameValuemax -> "valuemax"
         AccessibilityAxPropertyNameValuetext -> "valuetext"
         AccessibilityAxPropertyNameChecked -> "checked"
         AccessibilityAxPropertyNameExpanded -> "expanded"
         AccessibilityAxPropertyNameModal -> "modal"
         AccessibilityAxPropertyNamePressed -> "pressed"
         AccessibilityAxPropertyNameSelected -> "selected"
         AccessibilityAxPropertyNameActivedescendant -> "activedescendant"
         AccessibilityAxPropertyNameControls -> "controls"
         AccessibilityAxPropertyNameDescribedby -> "describedby"
         AccessibilityAxPropertyNameDetails -> "details"
         AccessibilityAxPropertyNameErrormessage -> "errormessage"
         AccessibilityAxPropertyNameFlowto -> "flowto"
         AccessibilityAxPropertyNameLabelledby -> "labelledby"
         AccessibilityAxPropertyNameOwns -> "owns"



-- | A node in the accessibility tree.
data AccessibilityAxNode = AccessibilityAxNode {
   accessibilityAxNodeNodeId :: AccessibilityAxNodeNodeId, -- ^ Unique identifier for this node.
   accessibilityAxNodeIgnored :: AccessibilityAxNodeIgnored, -- ^ Whether this node is ignored for accessibility
   accessibilityAxNodeIgnoredReasons :: AccessibilityAxNodeIgnoredReasons, -- ^ Collection of reasons why this node is hidden.
   accessibilityAxNodeRole :: AccessibilityAxNodeRole, -- ^ This `Node`'s role, whether explicit or implicit.
   accessibilityAxNodeName :: AccessibilityAxNodeName, -- ^ The accessible name for this `Node`.
   accessibilityAxNodeDescription :: AccessibilityAxNodeDescription, -- ^ The accessible description for this `Node`.
   accessibilityAxNodeValue :: AccessibilityAxNodeValue, -- ^ The value for this `Node`.
   accessibilityAxNodeProperties :: AccessibilityAxNodeProperties, -- ^ All other properties
   accessibilityAxNodeParentId :: AccessibilityAxNodeParentId, -- ^ ID for this node's parent.
   accessibilityAxNodeChildIds :: AccessibilityAxNodeChildIds, -- ^ IDs for each of this node's child nodes.
   accessibilityAxNodeBackendDomNodeId :: AccessibilityAxNodeBackendDomNodeId, -- ^ The backend ID for the associated DOM node, if any.
   accessibilityAxNodeFrameId :: AccessibilityAxNodeFrameId -- ^ The frame ID for the frame associated with this nodes document.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Type of the 'Accessibility.loadComplete' event.
data AccessibilityLoadComplete = AccessibilityLoadComplete {
   accessibilityLoadCompleteRoot :: AccessibilityLoadCompleteRoot -- ^ New document root node.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityLoadComplete  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityLoadComplete where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Accessibility.nodesUpdated' event.
data AccessibilityNodesUpdated = AccessibilityNodesUpdated {
   accessibilityNodesUpdatedNodes :: AccessibilityNodesUpdatedNodes -- ^ Updated node data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }





-- | Function for the command 'Accessibility.disable'.
-- Disables the accessibility domain.
accessibilityDisable :: Handle ev -> IO (Maybe Error)
accessibilityDisable handle = sendReceiveCommand handle "Accessibility.disable" (Nothing :: Maybe ())


-- | Function for the command 'Accessibility.enable'.
-- Enables the accessibility domain which causes `AXNodeId`s to remain consistent between method calls.
-- This turns on accessibility for the page, which can impact performance until accessibility is disabled.
accessibilityEnable :: Handle ev -> IO (Maybe Error)
accessibilityEnable handle = sendReceiveCommand handle "Accessibility.enable" (Nothing :: Maybe ())


-- | Parameters of the 'accessibilityGetPartialAxTree' command.
data PAccessibilityGetPartialAxTree = PAccessibilityGetPartialAxTree {
   pAccessibilityGetPartialAxTreeNodeId :: PAccessibilityGetPartialAxTreeNodeId, -- ^ Identifier of the node to get the partial accessibility tree for.
   pAccessibilityGetPartialAxTreeBackendNodeId :: PAccessibilityGetPartialAxTreeBackendNodeId, -- ^ Identifier of the backend node to get the partial accessibility tree for.
   pAccessibilityGetPartialAxTreeObjectId :: PAccessibilityGetPartialAxTreeObjectId, -- ^ JavaScript object id of the node wrapper to get the partial accessibility tree for.
   pAccessibilityGetPartialAxTreeFetchRelatives :: PAccessibilityGetPartialAxTreeFetchRelatives -- ^ Whether to fetch this nodes ancestors, siblings and children. Defaults to true.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetPartialAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetPartialAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Accessibility.getPartialAXTree'.
-- Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.
-- Parameters: 'PAccessibilityGetPartialAxTree'
-- Returns: 'AccessibilityGetPartialAxTree'
accessibilityGetPartialAxTree :: Handle ev -> PAccessibilityGetPartialAxTree -> IO (Either Error AccessibilityGetPartialAxTree)
accessibilityGetPartialAxTree handle params = sendReceiveCommandResult handle "Accessibility.getPartialAXTree" (Just params)

-- | Return type of the 'accessibilityGetPartialAxTree' command.
data AccessibilityGetPartialAxTree = AccessibilityGetPartialAxTree {
   accessibilityGetPartialAxTreeNodes :: [AccessibilityAxNode] -- ^ The `Accessibility.AXNode` for this DOM node, if it exists, plus its ancestors, siblings and
children, if requested.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetPartialAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command AccessibilityGetPartialAxTree where
   commandName _ = "Accessibility.getPartialAXTree"



-- | Parameters of the 'accessibilityGetFullAxTree' command.
data PAccessibilityGetFullAxTree = PAccessibilityGetFullAxTree {
   pAccessibilityGetFullAxTreeDepth :: PAccessibilityGetFullAxTreeDepth, -- ^ The maximum depth at which descendants of the root node should be retrieved.
If omitted, the full tree is returned.
   pAccessibilityGetFullAxTreeFrameId :: PAccessibilityGetFullAxTreeFrameId -- ^ The frame for whose document the AX tree should be retrieved.
If omited, the root frame is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetFullAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetFullAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Accessibility.getFullAXTree'.
-- Fetches the entire accessibility tree for the root Document
-- Parameters: 'PAccessibilityGetFullAxTree'
-- Returns: 'AccessibilityGetFullAxTree'
accessibilityGetFullAxTree :: Handle ev -> PAccessibilityGetFullAxTree -> IO (Either Error AccessibilityGetFullAxTree)
accessibilityGetFullAxTree handle params = sendReceiveCommandResult handle "Accessibility.getFullAXTree" (Just params)

-- | Return type of the 'accessibilityGetFullAxTree' command.
data AccessibilityGetFullAxTree = AccessibilityGetFullAxTree {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetFullAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetFullAxTree where
   commandName _ = "Accessibility.getFullAXTree"



-- | Parameters of the 'accessibilityGetRootAxNode' command.
data PAccessibilityGetRootAxNode = PAccessibilityGetRootAxNode {
   pAccessibilityGetRootAxNodeFrameId :: PAccessibilityGetRootAxNodeFrameId -- ^ The frame in whose document the node resides.
If omitted, the root frame is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetRootAxNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetRootAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Accessibility.getRootAXNode'.
-- Fetches the root node.
-- Requires `enable()` to have been called previously.
-- Parameters: 'PAccessibilityGetRootAxNode'
-- Returns: 'AccessibilityGetRootAxNode'
accessibilityGetRootAxNode :: Handle ev -> PAccessibilityGetRootAxNode -> IO (Either Error AccessibilityGetRootAxNode)
accessibilityGetRootAxNode handle params = sendReceiveCommandResult handle "Accessibility.getRootAXNode" (Just params)

-- | Return type of the 'accessibilityGetRootAxNode' command.
data AccessibilityGetRootAxNode = AccessibilityGetRootAxNode {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetRootAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetRootAxNode where
   commandName _ = "Accessibility.getRootAXNode"



-- | Parameters of the 'accessibilityGetAxNodeAndAncestors' command.
data PAccessibilityGetAxNodeAndAncestors = PAccessibilityGetAxNodeAndAncestors {
   pAccessibilityGetAxNodeAndAncestorsNodeId :: PAccessibilityGetAxNodeAndAncestorsNodeId, -- ^ Identifier of the node to get.
   pAccessibilityGetAxNodeAndAncestorsBackendNodeId :: PAccessibilityGetAxNodeAndAncestorsBackendNodeId, -- ^ Identifier of the backend node to get.
   pAccessibilityGetAxNodeAndAncestorsObjectId :: PAccessibilityGetAxNodeAndAncestorsObjectId -- ^ JavaScript object id of the node wrapper to get.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetAxNodeAndAncestors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetAxNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'Accessibility.getAXNodeAndAncestors'.
-- Fetches a node and all ancestors up to and including the root.
-- Requires `enable()` to have been called previously.
-- Parameters: 'PAccessibilityGetAxNodeAndAncestors'
-- Returns: 'AccessibilityGetAxNodeAndAncestors'
accessibilityGetAxNodeAndAncestors :: Handle ev -> PAccessibilityGetAxNodeAndAncestors -> IO (Either Error AccessibilityGetAxNodeAndAncestors)
accessibilityGetAxNodeAndAncestors handle params = sendReceiveCommandResult handle "Accessibility.getAXNodeAndAncestors" (Just params)

-- | Return type of the 'accessibilityGetAxNodeAndAncestors' command.
data AccessibilityGetAxNodeAndAncestors = AccessibilityGetAxNodeAndAncestors {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetAxNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command AccessibilityGetAxNodeAndAncestors where
   commandName _ = "Accessibility.getAXNodeAndAncestors"



-- | Parameters of the 'accessibilityGetChildAxNodes' command.
data PAccessibilityGetChildAxNodes = PAccessibilityGetChildAxNodes {

   pAccessibilityGetChildAxNodesFrameId :: PAccessibilityGetChildAxNodesFrameId -- ^ The frame in whose document the node resides.
If omitted, the root frame is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetChildAxNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetChildAxNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Accessibility.getChildAXNodes'.
-- Fetches a particular accessibility node by AXNodeId.
-- Requires `enable()` to have been called previously.
-- Parameters: 'PAccessibilityGetChildAxNodes'
-- Returns: 'AccessibilityGetChildAxNodes'
accessibilityGetChildAxNodes :: Handle ev -> PAccessibilityGetChildAxNodes -> IO (Either Error AccessibilityGetChildAxNodes)
accessibilityGetChildAxNodes handle params = sendReceiveCommandResult handle "Accessibility.getChildAXNodes" (Just params)

-- | Return type of the 'accessibilityGetChildAxNodes' command.
data AccessibilityGetChildAxNodes = AccessibilityGetChildAxNodes {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetChildAxNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command AccessibilityGetChildAxNodes where
   commandName _ = "Accessibility.getChildAXNodes"



-- | Parameters of the 'accessibilityQueryAxTree' command.
data PAccessibilityQueryAxTree = PAccessibilityQueryAxTree {
   pAccessibilityQueryAxTreeNodeId :: PAccessibilityQueryAxTreeNodeId, -- ^ Identifier of the node for the root to query.
   pAccessibilityQueryAxTreeBackendNodeId :: PAccessibilityQueryAxTreeBackendNodeId, -- ^ Identifier of the backend node for the root to query.
   pAccessibilityQueryAxTreeObjectId :: PAccessibilityQueryAxTreeObjectId, -- ^ JavaScript object id of the node wrapper for the root to query.
   pAccessibilityQueryAxTreeAccessibleName :: PAccessibilityQueryAxTreeAccessibleName, -- ^ Find nodes with this computed name.
   pAccessibilityQueryAxTreeRole :: PAccessibilityQueryAxTreeRole -- ^ Find nodes with this computed role.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityQueryAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityQueryAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Accessibility.queryAXTree'.
-- Query a DOM node's accessibility subtree for accessible name and role.
-- This command computes the name and role for all nodes in the subtree, including those that are
-- ignored for accessibility, and returns those that mactch the specified name and role. If no DOM
-- node is specified, or the DOM node does not exist, the command returns an error. If neither
-- `accessibleName` or `role` is specified, it returns all the accessibility nodes in the subtree.
-- Parameters: 'PAccessibilityQueryAxTree'
-- Returns: 'AccessibilityQueryAxTree'
accessibilityQueryAxTree :: Handle ev -> PAccessibilityQueryAxTree -> IO (Either Error AccessibilityQueryAxTree)
accessibilityQueryAxTree handle params = sendReceiveCommandResult handle "Accessibility.queryAXTree" (Just params)

-- | Return type of the 'accessibilityQueryAxTree' command.
data AccessibilityQueryAxTree = AccessibilityQueryAxTree {
   accessibilityQueryAxTreeNodes :: [AccessibilityAxNode] -- ^ A list of `Accessibility.AXNode` matching the specified attributes,
including nodes that are ignored for accessibility.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityQueryAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AccessibilityQueryAxTree where
   commandName _ = "Accessibility.queryAXTree"




