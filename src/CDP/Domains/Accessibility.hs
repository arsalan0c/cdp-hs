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


-- | Type 'Accessibility.AXNodeId'.
--   Unique accessibility node identifier.
type AccessibilityAXNodeId = String

-- | Type 'Accessibility.AXValueType'.
--   Enum of possible property types.
data AccessibilityAXValueType = AccessibilityAXValueTypeBoolean | AccessibilityAXValueTypeTristate | AccessibilityAXValueTypeBooleanOrUndefined | AccessibilityAXValueTypeIdref | AccessibilityAXValueTypeIdrefList | AccessibilityAXValueTypeInteger | AccessibilityAXValueTypeNode | AccessibilityAXValueTypeNodeList | AccessibilityAXValueTypeNumber | AccessibilityAXValueTypeString | AccessibilityAXValueTypeComputedString | AccessibilityAXValueTypeToken | AccessibilityAXValueTypeTokenList | AccessibilityAXValueTypeDomRelation | AccessibilityAXValueTypeRole | AccessibilityAXValueTypeInternalRole | AccessibilityAXValueTypeValueUndefined
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAXValueType where
   parseJSON = A.withText  "AccessibilityAXValueType"  $ \v -> do
      case v of
         "boolean" -> pure AccessibilityAXValueTypeBoolean
         "tristate" -> pure AccessibilityAXValueTypeTristate
         "booleanOrUndefined" -> pure AccessibilityAXValueTypeBooleanOrUndefined
         "idref" -> pure AccessibilityAXValueTypeIdref
         "idrefList" -> pure AccessibilityAXValueTypeIdrefList
         "integer" -> pure AccessibilityAXValueTypeInteger
         "node" -> pure AccessibilityAXValueTypeNode
         "nodeList" -> pure AccessibilityAXValueTypeNodeList
         "number" -> pure AccessibilityAXValueTypeNumber
         "string" -> pure AccessibilityAXValueTypeString
         "computedString" -> pure AccessibilityAXValueTypeComputedString
         "token" -> pure AccessibilityAXValueTypeToken
         "tokenList" -> pure AccessibilityAXValueTypeTokenList
         "domRelation" -> pure AccessibilityAXValueTypeDomRelation
         "role" -> pure AccessibilityAXValueTypeRole
         "internalRole" -> pure AccessibilityAXValueTypeInternalRole
         "valueUndefined" -> pure AccessibilityAXValueTypeValueUndefined
         _ -> fail "failed to parse AccessibilityAXValueType"

instance ToJSON AccessibilityAXValueType where
   toJSON v = A.String $
      case v of
         AccessibilityAXValueTypeBoolean -> "boolean"
         AccessibilityAXValueTypeTristate -> "tristate"
         AccessibilityAXValueTypeBooleanOrUndefined -> "booleanOrUndefined"
         AccessibilityAXValueTypeIdref -> "idref"
         AccessibilityAXValueTypeIdrefList -> "idrefList"
         AccessibilityAXValueTypeInteger -> "integer"
         AccessibilityAXValueTypeNode -> "node"
         AccessibilityAXValueTypeNodeList -> "nodeList"
         AccessibilityAXValueTypeNumber -> "number"
         AccessibilityAXValueTypeString -> "string"
         AccessibilityAXValueTypeComputedString -> "computedString"
         AccessibilityAXValueTypeToken -> "token"
         AccessibilityAXValueTypeTokenList -> "tokenList"
         AccessibilityAXValueTypeDomRelation -> "domRelation"
         AccessibilityAXValueTypeRole -> "role"
         AccessibilityAXValueTypeInternalRole -> "internalRole"
         AccessibilityAXValueTypeValueUndefined -> "valueUndefined"



-- | Type 'Accessibility.AXValueSourceType'.
--   Enum of possible property sources.
data AccessibilityAXValueSourceType = AccessibilityAXValueSourceTypeAttribute | AccessibilityAXValueSourceTypeImplicit | AccessibilityAXValueSourceTypeStyle | AccessibilityAXValueSourceTypeContents | AccessibilityAXValueSourceTypePlaceholder | AccessibilityAXValueSourceTypeRelatedElement
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAXValueSourceType where
   parseJSON = A.withText  "AccessibilityAXValueSourceType"  $ \v -> do
      case v of
         "attribute" -> pure AccessibilityAXValueSourceTypeAttribute
         "implicit" -> pure AccessibilityAXValueSourceTypeImplicit
         "style" -> pure AccessibilityAXValueSourceTypeStyle
         "contents" -> pure AccessibilityAXValueSourceTypeContents
         "placeholder" -> pure AccessibilityAXValueSourceTypePlaceholder
         "relatedElement" -> pure AccessibilityAXValueSourceTypeRelatedElement
         _ -> fail "failed to parse AccessibilityAXValueSourceType"

instance ToJSON AccessibilityAXValueSourceType where
   toJSON v = A.String $
      case v of
         AccessibilityAXValueSourceTypeAttribute -> "attribute"
         AccessibilityAXValueSourceTypeImplicit -> "implicit"
         AccessibilityAXValueSourceTypeStyle -> "style"
         AccessibilityAXValueSourceTypeContents -> "contents"
         AccessibilityAXValueSourceTypePlaceholder -> "placeholder"
         AccessibilityAXValueSourceTypeRelatedElement -> "relatedElement"



-- | Type 'Accessibility.AXValueNativeSourceType'.
--   Enum of possible native property sources (as a subtype of a particular AXValueSourceType).
data AccessibilityAXValueNativeSourceType = AccessibilityAXValueNativeSourceTypeDescription | AccessibilityAXValueNativeSourceTypeFigcaption | AccessibilityAXValueNativeSourceTypeLabel | AccessibilityAXValueNativeSourceTypeLabelfor | AccessibilityAXValueNativeSourceTypeLabelwrapped | AccessibilityAXValueNativeSourceTypeLegend | AccessibilityAXValueNativeSourceTypeRubyannotation | AccessibilityAXValueNativeSourceTypeTablecaption | AccessibilityAXValueNativeSourceTypeTitle | AccessibilityAXValueNativeSourceTypeOther
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAXValueNativeSourceType where
   parseJSON = A.withText  "AccessibilityAXValueNativeSourceType"  $ \v -> do
      case v of
         "description" -> pure AccessibilityAXValueNativeSourceTypeDescription
         "figcaption" -> pure AccessibilityAXValueNativeSourceTypeFigcaption
         "label" -> pure AccessibilityAXValueNativeSourceTypeLabel
         "labelfor" -> pure AccessibilityAXValueNativeSourceTypeLabelfor
         "labelwrapped" -> pure AccessibilityAXValueNativeSourceTypeLabelwrapped
         "legend" -> pure AccessibilityAXValueNativeSourceTypeLegend
         "rubyannotation" -> pure AccessibilityAXValueNativeSourceTypeRubyannotation
         "tablecaption" -> pure AccessibilityAXValueNativeSourceTypeTablecaption
         "title" -> pure AccessibilityAXValueNativeSourceTypeTitle
         "other" -> pure AccessibilityAXValueNativeSourceTypeOther
         _ -> fail "failed to parse AccessibilityAXValueNativeSourceType"

instance ToJSON AccessibilityAXValueNativeSourceType where
   toJSON v = A.String $
      case v of
         AccessibilityAXValueNativeSourceTypeDescription -> "description"
         AccessibilityAXValueNativeSourceTypeFigcaption -> "figcaption"
         AccessibilityAXValueNativeSourceTypeLabel -> "label"
         AccessibilityAXValueNativeSourceTypeLabelfor -> "labelfor"
         AccessibilityAXValueNativeSourceTypeLabelwrapped -> "labelwrapped"
         AccessibilityAXValueNativeSourceTypeLegend -> "legend"
         AccessibilityAXValueNativeSourceTypeRubyannotation -> "rubyannotation"
         AccessibilityAXValueNativeSourceTypeTablecaption -> "tablecaption"
         AccessibilityAXValueNativeSourceTypeTitle -> "title"
         AccessibilityAXValueNativeSourceTypeOther -> "other"



-- | Type 'Accessibility.AXValueSource'.
--   A single source for a computed AX property.
data AccessibilityAXValueSource = AccessibilityAXValueSource {
  -- | What type of source this is.
  accessibilityAXValueSourceType :: AccessibilityAXValueSourceType,
  -- | The value of this property source.
  accessibilityAXValueSourceValue :: Maybe AccessibilityAXValue,
  -- | The name of the relevant attribute, if any.
  accessibilityAXValueSourceAttribute :: Maybe String,
  -- | The value of the relevant attribute, if any.
  accessibilityAXValueSourceAttributeValue :: Maybe AccessibilityAXValue,
  -- | Whether this source is superseded by a higher priority source.
  accessibilityAXValueSourceSuperseded :: Maybe Bool,
  -- | The native markup source for this value, e.g. a <label> element.
  accessibilityAXValueSourceNativeSource :: Maybe AccessibilityAXValueNativeSourceType,
  -- | The value, such as a node or node list, of the native source.
  accessibilityAXValueSourceNativeSourceValue :: Maybe AccessibilityAXValue,
  -- | Whether the value for this property is invalid.
  accessibilityAXValueSourceInvalid :: Maybe Bool,
  -- | Reason for the value being invalid, if it is.
  accessibilityAXValueSourceInvalidReason :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAXValueSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAXValueSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Accessibility.AXRelatedNode'.
data AccessibilityAXRelatedNode = AccessibilityAXRelatedNode {
  -- | The BackendNodeId of the related DOM node.
  accessibilityAXRelatedNodeBackendDOMNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | The IDRef value provided, if any.
  accessibilityAXRelatedNodeIdref :: Maybe String,
  -- | The text alternative of this node in the current context.
  accessibilityAXRelatedNodeText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAXRelatedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAXRelatedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Accessibility.AXProperty'.
data AccessibilityAXProperty = AccessibilityAXProperty {
  -- | The name of this property.
  accessibilityAXPropertyName :: AccessibilityAXPropertyName,
  -- | The value of this property.
  accessibilityAXPropertyValue :: AccessibilityAXValue
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAXProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAXProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'Accessibility.AXValue'.
--   A single computed AX property.
data AccessibilityAXValue = AccessibilityAXValue {
  -- | The type of this value.
  accessibilityAXValueType :: AccessibilityAXValueType,
  -- | The computed value of this property.
  accessibilityAXValueValue :: Maybe Int,
  -- | One or more related nodes, if applicable.
  accessibilityAXValueRelatedNodes :: Maybe [AccessibilityAXRelatedNode],
  -- | The sources which contributed to the computation of this property.
  accessibilityAXValueSources :: Maybe [AccessibilityAXValueSource]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAXValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAXValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Accessibility.AXPropertyName'.
--   Values of AXProperty name:
--   - from 'busy' to 'roledescription': states which apply to every AX node
--   - from 'live' to 'root': attributes which apply to nodes in live regions
--   - from 'autocomplete' to 'valuetext': attributes which apply to widgets
--   - from 'checked' to 'selected': states which apply to widgets
--   - from 'activedescendant' to 'owns' - relationships between elements other than parent/child/sibling.
data AccessibilityAXPropertyName = AccessibilityAXPropertyNameBusy | AccessibilityAXPropertyNameDisabled | AccessibilityAXPropertyNameEditable | AccessibilityAXPropertyNameFocusable | AccessibilityAXPropertyNameFocused | AccessibilityAXPropertyNameHidden | AccessibilityAXPropertyNameHiddenRoot | AccessibilityAXPropertyNameInvalid | AccessibilityAXPropertyNameKeyshortcuts | AccessibilityAXPropertyNameSettable | AccessibilityAXPropertyNameRoledescription | AccessibilityAXPropertyNameLive | AccessibilityAXPropertyNameAtomic | AccessibilityAXPropertyNameRelevant | AccessibilityAXPropertyNameRoot | AccessibilityAXPropertyNameAutocomplete | AccessibilityAXPropertyNameHasPopup | AccessibilityAXPropertyNameLevel | AccessibilityAXPropertyNameMultiselectable | AccessibilityAXPropertyNameOrientation | AccessibilityAXPropertyNameMultiline | AccessibilityAXPropertyNameReadonly | AccessibilityAXPropertyNameRequired | AccessibilityAXPropertyNameValuemin | AccessibilityAXPropertyNameValuemax | AccessibilityAXPropertyNameValuetext | AccessibilityAXPropertyNameChecked | AccessibilityAXPropertyNameExpanded | AccessibilityAXPropertyNameModal | AccessibilityAXPropertyNamePressed | AccessibilityAXPropertyNameSelected | AccessibilityAXPropertyNameActivedescendant | AccessibilityAXPropertyNameControls | AccessibilityAXPropertyNameDescribedby | AccessibilityAXPropertyNameDetails | AccessibilityAXPropertyNameErrormessage | AccessibilityAXPropertyNameFlowto | AccessibilityAXPropertyNameLabelledby | AccessibilityAXPropertyNameOwns
   deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAXPropertyName where
   parseJSON = A.withText  "AccessibilityAXPropertyName"  $ \v -> do
      case v of
         "busy" -> pure AccessibilityAXPropertyNameBusy
         "disabled" -> pure AccessibilityAXPropertyNameDisabled
         "editable" -> pure AccessibilityAXPropertyNameEditable
         "focusable" -> pure AccessibilityAXPropertyNameFocusable
         "focused" -> pure AccessibilityAXPropertyNameFocused
         "hidden" -> pure AccessibilityAXPropertyNameHidden
         "hiddenRoot" -> pure AccessibilityAXPropertyNameHiddenRoot
         "invalid" -> pure AccessibilityAXPropertyNameInvalid
         "keyshortcuts" -> pure AccessibilityAXPropertyNameKeyshortcuts
         "settable" -> pure AccessibilityAXPropertyNameSettable
         "roledescription" -> pure AccessibilityAXPropertyNameRoledescription
         "live" -> pure AccessibilityAXPropertyNameLive
         "atomic" -> pure AccessibilityAXPropertyNameAtomic
         "relevant" -> pure AccessibilityAXPropertyNameRelevant
         "root" -> pure AccessibilityAXPropertyNameRoot
         "autocomplete" -> pure AccessibilityAXPropertyNameAutocomplete
         "hasPopup" -> pure AccessibilityAXPropertyNameHasPopup
         "level" -> pure AccessibilityAXPropertyNameLevel
         "multiselectable" -> pure AccessibilityAXPropertyNameMultiselectable
         "orientation" -> pure AccessibilityAXPropertyNameOrientation
         "multiline" -> pure AccessibilityAXPropertyNameMultiline
         "readonly" -> pure AccessibilityAXPropertyNameReadonly
         "required" -> pure AccessibilityAXPropertyNameRequired
         "valuemin" -> pure AccessibilityAXPropertyNameValuemin
         "valuemax" -> pure AccessibilityAXPropertyNameValuemax
         "valuetext" -> pure AccessibilityAXPropertyNameValuetext
         "checked" -> pure AccessibilityAXPropertyNameChecked
         "expanded" -> pure AccessibilityAXPropertyNameExpanded
         "modal" -> pure AccessibilityAXPropertyNameModal
         "pressed" -> pure AccessibilityAXPropertyNamePressed
         "selected" -> pure AccessibilityAXPropertyNameSelected
         "activedescendant" -> pure AccessibilityAXPropertyNameActivedescendant
         "controls" -> pure AccessibilityAXPropertyNameControls
         "describedby" -> pure AccessibilityAXPropertyNameDescribedby
         "details" -> pure AccessibilityAXPropertyNameDetails
         "errormessage" -> pure AccessibilityAXPropertyNameErrormessage
         "flowto" -> pure AccessibilityAXPropertyNameFlowto
         "labelledby" -> pure AccessibilityAXPropertyNameLabelledby
         "owns" -> pure AccessibilityAXPropertyNameOwns
         _ -> fail "failed to parse AccessibilityAXPropertyName"

instance ToJSON AccessibilityAXPropertyName where
   toJSON v = A.String $
      case v of
         AccessibilityAXPropertyNameBusy -> "busy"
         AccessibilityAXPropertyNameDisabled -> "disabled"
         AccessibilityAXPropertyNameEditable -> "editable"
         AccessibilityAXPropertyNameFocusable -> "focusable"
         AccessibilityAXPropertyNameFocused -> "focused"
         AccessibilityAXPropertyNameHidden -> "hidden"
         AccessibilityAXPropertyNameHiddenRoot -> "hiddenRoot"
         AccessibilityAXPropertyNameInvalid -> "invalid"
         AccessibilityAXPropertyNameKeyshortcuts -> "keyshortcuts"
         AccessibilityAXPropertyNameSettable -> "settable"
         AccessibilityAXPropertyNameRoledescription -> "roledescription"
         AccessibilityAXPropertyNameLive -> "live"
         AccessibilityAXPropertyNameAtomic -> "atomic"
         AccessibilityAXPropertyNameRelevant -> "relevant"
         AccessibilityAXPropertyNameRoot -> "root"
         AccessibilityAXPropertyNameAutocomplete -> "autocomplete"
         AccessibilityAXPropertyNameHasPopup -> "hasPopup"
         AccessibilityAXPropertyNameLevel -> "level"
         AccessibilityAXPropertyNameMultiselectable -> "multiselectable"
         AccessibilityAXPropertyNameOrientation -> "orientation"
         AccessibilityAXPropertyNameMultiline -> "multiline"
         AccessibilityAXPropertyNameReadonly -> "readonly"
         AccessibilityAXPropertyNameRequired -> "required"
         AccessibilityAXPropertyNameValuemin -> "valuemin"
         AccessibilityAXPropertyNameValuemax -> "valuemax"
         AccessibilityAXPropertyNameValuetext -> "valuetext"
         AccessibilityAXPropertyNameChecked -> "checked"
         AccessibilityAXPropertyNameExpanded -> "expanded"
         AccessibilityAXPropertyNameModal -> "modal"
         AccessibilityAXPropertyNamePressed -> "pressed"
         AccessibilityAXPropertyNameSelected -> "selected"
         AccessibilityAXPropertyNameActivedescendant -> "activedescendant"
         AccessibilityAXPropertyNameControls -> "controls"
         AccessibilityAXPropertyNameDescribedby -> "describedby"
         AccessibilityAXPropertyNameDetails -> "details"
         AccessibilityAXPropertyNameErrormessage -> "errormessage"
         AccessibilityAXPropertyNameFlowto -> "flowto"
         AccessibilityAXPropertyNameLabelledby -> "labelledby"
         AccessibilityAXPropertyNameOwns -> "owns"



-- | Type 'Accessibility.AXNode'.
--   A node in the accessibility tree.
data AccessibilityAXNode = AccessibilityAXNode {
  -- | Unique identifier for this node.
  accessibilityAXNodeNodeId :: AccessibilityAXNodeId,
  -- | Whether this node is ignored for accessibility
  accessibilityAXNodeIgnored :: Bool,
  -- | Collection of reasons why this node is hidden.
  accessibilityAXNodeIgnoredReasons :: Maybe [AccessibilityAXProperty],
  -- | This `Node`'s role, whether explicit or implicit.
  accessibilityAXNodeRole :: Maybe AccessibilityAXValue,
  -- | The accessible name for this `Node`.
  accessibilityAXNodeName :: Maybe AccessibilityAXValue,
  -- | The accessible description for this `Node`.
  accessibilityAXNodeDescription :: Maybe AccessibilityAXValue,
  -- | The value for this `Node`.
  accessibilityAXNodeValue :: Maybe AccessibilityAXValue,
  -- | All other properties
  accessibilityAXNodeProperties :: Maybe [AccessibilityAXProperty],
  -- | ID for this node's parent.
  accessibilityAXNodeParentId :: Maybe AccessibilityAXNodeId,
  -- | IDs for each of this node's child nodes.
  accessibilityAXNodeChildIds :: Maybe [AccessibilityAXNodeId],
  -- | The backend ID for the associated DOM node, if any.
  accessibilityAXNodeBackendDOMNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | The frame ID for the frame associated with this nodes document.
  accessibilityAXNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAXNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAXNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Type of the 'Accessibility.loadComplete' event.
data AccessibilityLoadComplete = AccessibilityLoadComplete {
  -- | New document root node.
  accessibilityLoadCompleteRoot :: AccessibilityAXNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityLoadComplete  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityLoadComplete where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event AccessibilityLoadComplete where
    eventName _ = "Accessibility.loadComplete"

-- | Type of the 'Accessibility.nodesUpdated' event.
data AccessibilityNodesUpdated = AccessibilityNodesUpdated {
  -- | Updated node data.
  accessibilityNodesUpdatedNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Event AccessibilityNodesUpdated where
    eventName _ = "Accessibility.nodesUpdated"



-- | Function for the 'Accessibility.disable' command.
--   Disables the accessibility domain.
accessibilityDisable :: Handle ev -> IO ()
accessibilityDisable handle = sendReceiveCommand handle "Accessibility.disable" (Nothing :: Maybe ())


-- | Function for the 'Accessibility.enable' command.
--   Enables the accessibility domain which causes `AXNodeId`s to remain consistent between method calls.
--   This turns on accessibility for the page, which can impact performance until accessibility is disabled.
accessibilityEnable :: Handle ev -> IO ()
accessibilityEnable handle = sendReceiveCommand handle "Accessibility.enable" (Nothing :: Maybe ())


-- | Parameters of the 'accessibilityGetPartialAXTree' command.
data PAccessibilityGetPartialAXTree = PAccessibilityGetPartialAXTree {
  -- | Identifier of the node to get the partial accessibility tree for.
  pAccessibilityGetPartialAXTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Identifier of the backend node to get the partial accessibility tree for.
  pAccessibilityGetPartialAXTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper to get the partial accessibility tree for.
  pAccessibilityGetPartialAXTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | Whether to fetch this nodes ancestors, siblings and children. Defaults to true.
  pAccessibilityGetPartialAXTreeFetchRelatives :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetPartialAXTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetPartialAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the 'Accessibility.getPartialAXTree' command.
--   Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.
--   Parameters: 'PAccessibilityGetPartialAXTree'
--   Returns: 'AccessibilityGetPartialAXTree'
accessibilityGetPartialAXTree :: Handle ev -> PAccessibilityGetPartialAXTree -> IO AccessibilityGetPartialAXTree
accessibilityGetPartialAXTree handle params = sendReceiveCommandResult handle "Accessibility.getPartialAXTree" (Just params)

-- | Return type of the 'accessibilityGetPartialAXTree' command.
data AccessibilityGetPartialAXTree = AccessibilityGetPartialAXTree {
  -- | The `Accessibility.AXNode` for this DOM node, if it exists, plus its ancestors, siblings and
  --   children, if requested.
  accessibilityGetPartialAXTreeNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetPartialAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command AccessibilityGetPartialAXTree where
   commandName _ = "Accessibility.getPartialAXTree"



-- | Parameters of the 'accessibilityGetFullAXTree' command.
data PAccessibilityGetFullAXTree = PAccessibilityGetFullAXTree {
  -- | The maximum depth at which descendants of the root node should be retrieved.
  --   If omitted, the full tree is returned.
  pAccessibilityGetFullAXTreeDepth :: Maybe Int,
  -- | The frame for whose document the AX tree should be retrieved.
  --   If omited, the root frame is used.
  pAccessibilityGetFullAXTreeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetFullAXTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetFullAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Accessibility.getFullAXTree' command.
--   Fetches the entire accessibility tree for the root Document
--   Parameters: 'PAccessibilityGetFullAXTree'
--   Returns: 'AccessibilityGetFullAXTree'
accessibilityGetFullAXTree :: Handle ev -> PAccessibilityGetFullAXTree -> IO AccessibilityGetFullAXTree
accessibilityGetFullAXTree handle params = sendReceiveCommandResult handle "Accessibility.getFullAXTree" (Just params)

-- | Return type of the 'accessibilityGetFullAXTree' command.
data AccessibilityGetFullAXTree = AccessibilityGetFullAXTree {
  accessibilityGetFullAXTreeNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetFullAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetFullAXTree where
   commandName _ = "Accessibility.getFullAXTree"



-- | Parameters of the 'accessibilityGetRootAXNode' command.
data PAccessibilityGetRootAXNode = PAccessibilityGetRootAXNode {
  -- | The frame in whose document the node resides.
  --   If omitted, the root frame is used.
  pAccessibilityGetRootAXNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetRootAXNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetRootAXNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Accessibility.getRootAXNode' command.
--   Fetches the root node.
--   Requires `enable()` to have been called previously.
--   Parameters: 'PAccessibilityGetRootAXNode'
--   Returns: 'AccessibilityGetRootAXNode'
accessibilityGetRootAXNode :: Handle ev -> PAccessibilityGetRootAXNode -> IO AccessibilityGetRootAXNode
accessibilityGetRootAXNode handle params = sendReceiveCommandResult handle "Accessibility.getRootAXNode" (Just params)

-- | Return type of the 'accessibilityGetRootAXNode' command.
data AccessibilityGetRootAXNode = AccessibilityGetRootAXNode {
  accessibilityGetRootAXNodeNode :: AccessibilityAXNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetRootAXNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetRootAXNode where
   commandName _ = "Accessibility.getRootAXNode"



-- | Parameters of the 'accessibilityGetAXNodeAndAncestors' command.
data PAccessibilityGetAXNodeAndAncestors = PAccessibilityGetAXNodeAndAncestors {
  -- | Identifier of the node to get.
  pAccessibilityGetAXNodeAndAncestorsNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Identifier of the backend node to get.
  pAccessibilityGetAXNodeAndAncestorsBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper to get.
  pAccessibilityGetAXNodeAndAncestorsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetAXNodeAndAncestors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetAXNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'Accessibility.getAXNodeAndAncestors' command.
--   Fetches a node and all ancestors up to and including the root.
--   Requires `enable()` to have been called previously.
--   Parameters: 'PAccessibilityGetAXNodeAndAncestors'
--   Returns: 'AccessibilityGetAXNodeAndAncestors'
accessibilityGetAXNodeAndAncestors :: Handle ev -> PAccessibilityGetAXNodeAndAncestors -> IO AccessibilityGetAXNodeAndAncestors
accessibilityGetAXNodeAndAncestors handle params = sendReceiveCommandResult handle "Accessibility.getAXNodeAndAncestors" (Just params)

-- | Return type of the 'accessibilityGetAXNodeAndAncestors' command.
data AccessibilityGetAXNodeAndAncestors = AccessibilityGetAXNodeAndAncestors {
  accessibilityGetAXNodeAndAncestorsNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetAXNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command AccessibilityGetAXNodeAndAncestors where
   commandName _ = "Accessibility.getAXNodeAndAncestors"



-- | Parameters of the 'accessibilityGetChildAXNodes' command.
data PAccessibilityGetChildAXNodes = PAccessibilityGetChildAXNodes {
  pAccessibilityGetChildAXNodesId :: AccessibilityAXNodeId,
  -- | The frame in whose document the node resides.
  --   If omitted, the root frame is used.
  pAccessibilityGetChildAXNodesFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetChildAXNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetChildAXNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Accessibility.getChildAXNodes' command.
--   Fetches a particular accessibility node by AXNodeId.
--   Requires `enable()` to have been called previously.
--   Parameters: 'PAccessibilityGetChildAXNodes'
--   Returns: 'AccessibilityGetChildAXNodes'
accessibilityGetChildAXNodes :: Handle ev -> PAccessibilityGetChildAXNodes -> IO AccessibilityGetChildAXNodes
accessibilityGetChildAXNodes handle params = sendReceiveCommandResult handle "Accessibility.getChildAXNodes" (Just params)

-- | Return type of the 'accessibilityGetChildAXNodes' command.
data AccessibilityGetChildAXNodes = AccessibilityGetChildAXNodes {
  accessibilityGetChildAXNodesNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetChildAXNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command AccessibilityGetChildAXNodes where
   commandName _ = "Accessibility.getChildAXNodes"



-- | Parameters of the 'accessibilityQueryAXTree' command.
data PAccessibilityQueryAXTree = PAccessibilityQueryAXTree {
  -- | Identifier of the node for the root to query.
  pAccessibilityQueryAXTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Identifier of the backend node for the root to query.
  pAccessibilityQueryAXTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
  -- | JavaScript object id of the node wrapper for the root to query.
  pAccessibilityQueryAXTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | Find nodes with this computed name.
  pAccessibilityQueryAXTreeAccessibleName :: Maybe String,
  -- | Find nodes with this computed role.
  pAccessibilityQueryAXTreeRole :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityQueryAXTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityQueryAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Accessibility.queryAXTree' command.
--   Query a DOM node's accessibility subtree for accessible name and role.
--   This command computes the name and role for all nodes in the subtree, including those that are
--   ignored for accessibility, and returns those that mactch the specified name and role. If no DOM
--   node is specified, or the DOM node does not exist, the command returns an error. If neither
--   `accessibleName` or `role` is specified, it returns all the accessibility nodes in the subtree.
--   Parameters: 'PAccessibilityQueryAXTree'
--   Returns: 'AccessibilityQueryAXTree'
accessibilityQueryAXTree :: Handle ev -> PAccessibilityQueryAXTree -> IO AccessibilityQueryAXTree
accessibilityQueryAXTree handle params = sendReceiveCommandResult handle "Accessibility.queryAXTree" (Just params)

-- | Return type of the 'accessibilityQueryAXTree' command.
data AccessibilityQueryAXTree = AccessibilityQueryAXTree {
  -- | A list of `Accessibility.AXNode` matching the specified attributes,
  --   including nodes that are ignored for accessibility.
  accessibilityQueryAXTreeNodes :: [AccessibilityAXNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityQueryAXTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AccessibilityQueryAXTree where
   commandName _ = "Accessibility.queryAXTree"




