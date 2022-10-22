{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Accessibility

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

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'Accessibility.AXNodeId'.
--   Unique accessibility node identifier.
type AccessibilityAXNodeId = T.Text

-- | Type 'Accessibility.AXValueType'.
--   Enum of possible property types.
data AccessibilityAXValueType = AccessibilityAXValueTypeBoolean | AccessibilityAXValueTypeTristate | AccessibilityAXValueTypeBooleanOrUndefined | AccessibilityAXValueTypeIdref | AccessibilityAXValueTypeIdrefList | AccessibilityAXValueTypeInteger | AccessibilityAXValueTypeNode | AccessibilityAXValueTypeNodeList | AccessibilityAXValueTypeNumber | AccessibilityAXValueTypeString | AccessibilityAXValueTypeComputedString | AccessibilityAXValueTypeToken | AccessibilityAXValueTypeTokenList | AccessibilityAXValueTypeDomRelation | AccessibilityAXValueTypeRole | AccessibilityAXValueTypeInternalRole | AccessibilityAXValueTypeValueUndefined
  deriving (Ord, Eq, Show, Read)
instance FromJSON AccessibilityAXValueType where
  parseJSON = A.withText "AccessibilityAXValueType" $ \v -> case v of
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
    "_" -> fail "failed to parse AccessibilityAXValueType"
instance ToJSON AccessibilityAXValueType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "AccessibilityAXValueSourceType" $ \v -> case v of
    "attribute" -> pure AccessibilityAXValueSourceTypeAttribute
    "implicit" -> pure AccessibilityAXValueSourceTypeImplicit
    "style" -> pure AccessibilityAXValueSourceTypeStyle
    "contents" -> pure AccessibilityAXValueSourceTypeContents
    "placeholder" -> pure AccessibilityAXValueSourceTypePlaceholder
    "relatedElement" -> pure AccessibilityAXValueSourceTypeRelatedElement
    "_" -> fail "failed to parse AccessibilityAXValueSourceType"
instance ToJSON AccessibilityAXValueSourceType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "AccessibilityAXValueNativeSourceType" $ \v -> case v of
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
    "_" -> fail "failed to parse AccessibilityAXValueNativeSourceType"
instance ToJSON AccessibilityAXValueNativeSourceType where
  toJSON v = A.String $ case v of
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
data AccessibilityAXValueSource = AccessibilityAXValueSource
  {
    -- | What type of source this is.
    accessibilityAXValueSourceType :: AccessibilityAXValueSourceType,
    -- | The value of this property source.
    accessibilityAXValueSourceValue :: Maybe AccessibilityAXValue,
    -- | The name of the relevant attribute, if any.
    accessibilityAXValueSourceAttribute :: Maybe T.Text,
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
    accessibilityAXValueSourceInvalidReason :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityAXValueSource where
  parseJSON = A.withObject "AccessibilityAXValueSource" $ \o -> AccessibilityAXValueSource
    <$> o A..: "type"
    <*> o A..:? "value"
    <*> o A..:? "attribute"
    <*> o A..:? "attributeValue"
    <*> o A..:? "superseded"
    <*> o A..:? "nativeSource"
    <*> o A..:? "nativeSourceValue"
    <*> o A..:? "invalid"
    <*> o A..:? "invalidReason"
instance ToJSON AccessibilityAXValueSource where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (accessibilityAXValueSourceType p),
    ("value" A..=) <$> (accessibilityAXValueSourceValue p),
    ("attribute" A..=) <$> (accessibilityAXValueSourceAttribute p),
    ("attributeValue" A..=) <$> (accessibilityAXValueSourceAttributeValue p),
    ("superseded" A..=) <$> (accessibilityAXValueSourceSuperseded p),
    ("nativeSource" A..=) <$> (accessibilityAXValueSourceNativeSource p),
    ("nativeSourceValue" A..=) <$> (accessibilityAXValueSourceNativeSourceValue p),
    ("invalid" A..=) <$> (accessibilityAXValueSourceInvalid p),
    ("invalidReason" A..=) <$> (accessibilityAXValueSourceInvalidReason p)
    ]

-- | Type 'Accessibility.AXRelatedNode'.
data AccessibilityAXRelatedNode = AccessibilityAXRelatedNode
  {
    -- | The BackendNodeId of the related DOM node.
    accessibilityAXRelatedNodeBackendDOMNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | The IDRef value provided, if any.
    accessibilityAXRelatedNodeIdref :: Maybe T.Text,
    -- | The text alternative of this node in the current context.
    accessibilityAXRelatedNodeText :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityAXRelatedNode where
  parseJSON = A.withObject "AccessibilityAXRelatedNode" $ \o -> AccessibilityAXRelatedNode
    <$> o A..: "backendDOMNodeId"
    <*> o A..:? "idref"
    <*> o A..:? "text"
instance ToJSON AccessibilityAXRelatedNode where
  toJSON p = A.object $ catMaybes [
    ("backendDOMNodeId" A..=) <$> Just (accessibilityAXRelatedNodeBackendDOMNodeId p),
    ("idref" A..=) <$> (accessibilityAXRelatedNodeIdref p),
    ("text" A..=) <$> (accessibilityAXRelatedNodeText p)
    ]

-- | Type 'Accessibility.AXProperty'.
data AccessibilityAXProperty = AccessibilityAXProperty
  {
    -- | The name of this property.
    accessibilityAXPropertyName :: AccessibilityAXPropertyName,
    -- | The value of this property.
    accessibilityAXPropertyValue :: AccessibilityAXValue
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityAXProperty where
  parseJSON = A.withObject "AccessibilityAXProperty" $ \o -> AccessibilityAXProperty
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON AccessibilityAXProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (accessibilityAXPropertyName p),
    ("value" A..=) <$> Just (accessibilityAXPropertyValue p)
    ]

-- | Type 'Accessibility.AXValue'.
--   A single computed AX property.
data AccessibilityAXValue = AccessibilityAXValue
  {
    -- | The type of this value.
    accessibilityAXValueType :: AccessibilityAXValueType,
    -- | The computed value of this property.
    accessibilityAXValueValue :: Maybe A.Value,
    -- | One or more related nodes, if applicable.
    accessibilityAXValueRelatedNodes :: Maybe [AccessibilityAXRelatedNode],
    -- | The sources which contributed to the computation of this property.
    accessibilityAXValueSources :: Maybe [AccessibilityAXValueSource]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityAXValue where
  parseJSON = A.withObject "AccessibilityAXValue" $ \o -> AccessibilityAXValue
    <$> o A..: "type"
    <*> o A..:? "value"
    <*> o A..:? "relatedNodes"
    <*> o A..:? "sources"
instance ToJSON AccessibilityAXValue where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (accessibilityAXValueType p),
    ("value" A..=) <$> (accessibilityAXValueValue p),
    ("relatedNodes" A..=) <$> (accessibilityAXValueRelatedNodes p),
    ("sources" A..=) <$> (accessibilityAXValueSources p)
    ]

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
  parseJSON = A.withText "AccessibilityAXPropertyName" $ \v -> case v of
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
    "_" -> fail "failed to parse AccessibilityAXPropertyName"
instance ToJSON AccessibilityAXPropertyName where
  toJSON v = A.String $ case v of
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
data AccessibilityAXNode = AccessibilityAXNode
  {
    -- | Unique identifier for this node.
    accessibilityAXNodeNodeId :: AccessibilityAXNodeId,
    -- | Whether this node is ignored for accessibility
    accessibilityAXNodeIgnored :: Bool,
    -- | Collection of reasons why this node is hidden.
    accessibilityAXNodeIgnoredReasons :: Maybe [AccessibilityAXProperty],
    -- | This `Node`'s role, whether explicit or implicit.
    accessibilityAXNodeRole :: Maybe AccessibilityAXValue,
    -- | This `Node`'s Chrome raw role.
    accessibilityAXNodeChromeRole :: Maybe AccessibilityAXValue,
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
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityAXNode where
  parseJSON = A.withObject "AccessibilityAXNode" $ \o -> AccessibilityAXNode
    <$> o A..: "nodeId"
    <*> o A..: "ignored"
    <*> o A..:? "ignoredReasons"
    <*> o A..:? "role"
    <*> o A..:? "chromeRole"
    <*> o A..:? "name"
    <*> o A..:? "description"
    <*> o A..:? "value"
    <*> o A..:? "properties"
    <*> o A..:? "parentId"
    <*> o A..:? "childIds"
    <*> o A..:? "backendDOMNodeId"
    <*> o A..:? "frameId"
instance ToJSON AccessibilityAXNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (accessibilityAXNodeNodeId p),
    ("ignored" A..=) <$> Just (accessibilityAXNodeIgnored p),
    ("ignoredReasons" A..=) <$> (accessibilityAXNodeIgnoredReasons p),
    ("role" A..=) <$> (accessibilityAXNodeRole p),
    ("chromeRole" A..=) <$> (accessibilityAXNodeChromeRole p),
    ("name" A..=) <$> (accessibilityAXNodeName p),
    ("description" A..=) <$> (accessibilityAXNodeDescription p),
    ("value" A..=) <$> (accessibilityAXNodeValue p),
    ("properties" A..=) <$> (accessibilityAXNodeProperties p),
    ("parentId" A..=) <$> (accessibilityAXNodeParentId p),
    ("childIds" A..=) <$> (accessibilityAXNodeChildIds p),
    ("backendDOMNodeId" A..=) <$> (accessibilityAXNodeBackendDOMNodeId p),
    ("frameId" A..=) <$> (accessibilityAXNodeFrameId p)
    ]

-- | Type of the 'Accessibility.loadComplete' event.
data AccessibilityLoadComplete = AccessibilityLoadComplete
  {
    -- | New document root node.
    accessibilityLoadCompleteRoot :: AccessibilityAXNode
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityLoadComplete where
  parseJSON = A.withObject "AccessibilityLoadComplete" $ \o -> AccessibilityLoadComplete
    <$> o A..: "root"
instance Event AccessibilityLoadComplete where
  eventName _ = "Accessibility.loadComplete"

-- | Type of the 'Accessibility.nodesUpdated' event.
data AccessibilityNodesUpdated = AccessibilityNodesUpdated
  {
    -- | Updated node data.
    accessibilityNodesUpdatedNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityNodesUpdated where
  parseJSON = A.withObject "AccessibilityNodesUpdated" $ \o -> AccessibilityNodesUpdated
    <$> o A..: "nodes"
instance Event AccessibilityNodesUpdated where
  eventName _ = "Accessibility.nodesUpdated"

-- | Disables the accessibility domain.

-- | Parameters of the 'Accessibility.disable' command.
data PAccessibilityDisable = PAccessibilityDisable
  deriving (Eq, Show)
pAccessibilityDisable
  :: PAccessibilityDisable
pAccessibilityDisable
  = PAccessibilityDisable
instance ToJSON PAccessibilityDisable where
  toJSON _ = A.Null
instance Command PAccessibilityDisable where
  type CommandResponse PAccessibilityDisable = ()
  commandName _ = "Accessibility.disable"
  fromJSON = const . A.Success . const ()

-- | Enables the accessibility domain which causes `AXNodeId`s to remain consistent between method calls.
--   This turns on accessibility for the page, which can impact performance until accessibility is disabled.

-- | Parameters of the 'Accessibility.enable' command.
data PAccessibilityEnable = PAccessibilityEnable
  deriving (Eq, Show)
pAccessibilityEnable
  :: PAccessibilityEnable
pAccessibilityEnable
  = PAccessibilityEnable
instance ToJSON PAccessibilityEnable where
  toJSON _ = A.Null
instance Command PAccessibilityEnable where
  type CommandResponse PAccessibilityEnable = ()
  commandName _ = "Accessibility.enable"
  fromJSON = const . A.Success . const ()

-- | Fetches the accessibility node and partial accessibility tree for this DOM node, if it exists.

-- | Parameters of the 'Accessibility.getPartialAXTree' command.
data PAccessibilityGetPartialAXTree = PAccessibilityGetPartialAXTree
  {
    -- | Identifier of the node to get the partial accessibility tree for.
    pAccessibilityGetPartialAXTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Identifier of the backend node to get the partial accessibility tree for.
    pAccessibilityGetPartialAXTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper to get the partial accessibility tree for.
    pAccessibilityGetPartialAXTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
    -- | Whether to fetch this nodes ancestors, siblings and children. Defaults to true.
    pAccessibilityGetPartialAXTreeFetchRelatives :: Maybe Bool
  }
  deriving (Eq, Show)
pAccessibilityGetPartialAXTree
  :: PAccessibilityGetPartialAXTree
pAccessibilityGetPartialAXTree
  = PAccessibilityGetPartialAXTree
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PAccessibilityGetPartialAXTree where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pAccessibilityGetPartialAXTreeNodeId p),
    ("backendNodeId" A..=) <$> (pAccessibilityGetPartialAXTreeBackendNodeId p),
    ("objectId" A..=) <$> (pAccessibilityGetPartialAXTreeObjectId p),
    ("fetchRelatives" A..=) <$> (pAccessibilityGetPartialAXTreeFetchRelatives p)
    ]
data AccessibilityGetPartialAXTree = AccessibilityGetPartialAXTree
  {
    -- | The `Accessibility.AXNode` for this DOM node, if it exists, plus its ancestors, siblings and
    --   children, if requested.
    accessibilityGetPartialAXTreeNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityGetPartialAXTree where
  parseJSON = A.withObject "AccessibilityGetPartialAXTree" $ \o -> AccessibilityGetPartialAXTree
    <$> o A..: "nodes"
instance Command PAccessibilityGetPartialAXTree where
  type CommandResponse PAccessibilityGetPartialAXTree = AccessibilityGetPartialAXTree
  commandName _ = "Accessibility.getPartialAXTree"

-- | Fetches the entire accessibility tree for the root Document

-- | Parameters of the 'Accessibility.getFullAXTree' command.
data PAccessibilityGetFullAXTree = PAccessibilityGetFullAXTree
  {
    -- | The maximum depth at which descendants of the root node should be retrieved.
    --   If omitted, the full tree is returned.
    pAccessibilityGetFullAXTreeDepth :: Maybe Int,
    -- | The frame for whose document the AX tree should be retrieved.
    --   If omited, the root frame is used.
    pAccessibilityGetFullAXTreeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
pAccessibilityGetFullAXTree
  :: PAccessibilityGetFullAXTree
pAccessibilityGetFullAXTree
  = PAccessibilityGetFullAXTree
    Nothing
    Nothing
instance ToJSON PAccessibilityGetFullAXTree where
  toJSON p = A.object $ catMaybes [
    ("depth" A..=) <$> (pAccessibilityGetFullAXTreeDepth p),
    ("frameId" A..=) <$> (pAccessibilityGetFullAXTreeFrameId p)
    ]
data AccessibilityGetFullAXTree = AccessibilityGetFullAXTree
  {
    accessibilityGetFullAXTreeNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityGetFullAXTree where
  parseJSON = A.withObject "AccessibilityGetFullAXTree" $ \o -> AccessibilityGetFullAXTree
    <$> o A..: "nodes"
instance Command PAccessibilityGetFullAXTree where
  type CommandResponse PAccessibilityGetFullAXTree = AccessibilityGetFullAXTree
  commandName _ = "Accessibility.getFullAXTree"

-- | Fetches the root node.
--   Requires `enable()` to have been called previously.

-- | Parameters of the 'Accessibility.getRootAXNode' command.
data PAccessibilityGetRootAXNode = PAccessibilityGetRootAXNode
  {
    -- | The frame in whose document the node resides.
    --   If omitted, the root frame is used.
    pAccessibilityGetRootAXNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
pAccessibilityGetRootAXNode
  :: PAccessibilityGetRootAXNode
pAccessibilityGetRootAXNode
  = PAccessibilityGetRootAXNode
    Nothing
instance ToJSON PAccessibilityGetRootAXNode where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> (pAccessibilityGetRootAXNodeFrameId p)
    ]
data AccessibilityGetRootAXNode = AccessibilityGetRootAXNode
  {
    accessibilityGetRootAXNodeNode :: AccessibilityAXNode
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityGetRootAXNode where
  parseJSON = A.withObject "AccessibilityGetRootAXNode" $ \o -> AccessibilityGetRootAXNode
    <$> o A..: "node"
instance Command PAccessibilityGetRootAXNode where
  type CommandResponse PAccessibilityGetRootAXNode = AccessibilityGetRootAXNode
  commandName _ = "Accessibility.getRootAXNode"

-- | Fetches a node and all ancestors up to and including the root.
--   Requires `enable()` to have been called previously.

-- | Parameters of the 'Accessibility.getAXNodeAndAncestors' command.
data PAccessibilityGetAXNodeAndAncestors = PAccessibilityGetAXNodeAndAncestors
  {
    -- | Identifier of the node to get.
    pAccessibilityGetAXNodeAndAncestorsNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Identifier of the backend node to get.
    pAccessibilityGetAXNodeAndAncestorsBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper to get.
    pAccessibilityGetAXNodeAndAncestorsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pAccessibilityGetAXNodeAndAncestors
  :: PAccessibilityGetAXNodeAndAncestors
pAccessibilityGetAXNodeAndAncestors
  = PAccessibilityGetAXNodeAndAncestors
    Nothing
    Nothing
    Nothing
instance ToJSON PAccessibilityGetAXNodeAndAncestors where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pAccessibilityGetAXNodeAndAncestorsNodeId p),
    ("backendNodeId" A..=) <$> (pAccessibilityGetAXNodeAndAncestorsBackendNodeId p),
    ("objectId" A..=) <$> (pAccessibilityGetAXNodeAndAncestorsObjectId p)
    ]
data AccessibilityGetAXNodeAndAncestors = AccessibilityGetAXNodeAndAncestors
  {
    accessibilityGetAXNodeAndAncestorsNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityGetAXNodeAndAncestors where
  parseJSON = A.withObject "AccessibilityGetAXNodeAndAncestors" $ \o -> AccessibilityGetAXNodeAndAncestors
    <$> o A..: "nodes"
instance Command PAccessibilityGetAXNodeAndAncestors where
  type CommandResponse PAccessibilityGetAXNodeAndAncestors = AccessibilityGetAXNodeAndAncestors
  commandName _ = "Accessibility.getAXNodeAndAncestors"

-- | Fetches a particular accessibility node by AXNodeId.
--   Requires `enable()` to have been called previously.

-- | Parameters of the 'Accessibility.getChildAXNodes' command.
data PAccessibilityGetChildAXNodes = PAccessibilityGetChildAXNodes
  {
    pAccessibilityGetChildAXNodesId :: AccessibilityAXNodeId,
    -- | The frame in whose document the node resides.
    --   If omitted, the root frame is used.
    pAccessibilityGetChildAXNodesFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
pAccessibilityGetChildAXNodes
  :: AccessibilityAXNodeId
  -> PAccessibilityGetChildAXNodes
pAccessibilityGetChildAXNodes
  arg_pAccessibilityGetChildAXNodesId
  = PAccessibilityGetChildAXNodes
    arg_pAccessibilityGetChildAXNodesId
    Nothing
instance ToJSON PAccessibilityGetChildAXNodes where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (pAccessibilityGetChildAXNodesId p),
    ("frameId" A..=) <$> (pAccessibilityGetChildAXNodesFrameId p)
    ]
data AccessibilityGetChildAXNodes = AccessibilityGetChildAXNodes
  {
    accessibilityGetChildAXNodesNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityGetChildAXNodes where
  parseJSON = A.withObject "AccessibilityGetChildAXNodes" $ \o -> AccessibilityGetChildAXNodes
    <$> o A..: "nodes"
instance Command PAccessibilityGetChildAXNodes where
  type CommandResponse PAccessibilityGetChildAXNodes = AccessibilityGetChildAXNodes
  commandName _ = "Accessibility.getChildAXNodes"

-- | Query a DOM node's accessibility subtree for accessible name and role.
--   This command computes the name and role for all nodes in the subtree, including those that are
--   ignored for accessibility, and returns those that mactch the specified name and role. If no DOM
--   node is specified, or the DOM node does not exist, the command returns an error. If neither
--   `accessibleName` or `role` is specified, it returns all the accessibility nodes in the subtree.

-- | Parameters of the 'Accessibility.queryAXTree' command.
data PAccessibilityQueryAXTree = PAccessibilityQueryAXTree
  {
    -- | Identifier of the node for the root to query.
    pAccessibilityQueryAXTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Identifier of the backend node for the root to query.
    pAccessibilityQueryAXTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | JavaScript object id of the node wrapper for the root to query.
    pAccessibilityQueryAXTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
    -- | Find nodes with this computed name.
    pAccessibilityQueryAXTreeAccessibleName :: Maybe T.Text,
    -- | Find nodes with this computed role.
    pAccessibilityQueryAXTreeRole :: Maybe T.Text
  }
  deriving (Eq, Show)
pAccessibilityQueryAXTree
  :: PAccessibilityQueryAXTree
pAccessibilityQueryAXTree
  = PAccessibilityQueryAXTree
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PAccessibilityQueryAXTree where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> (pAccessibilityQueryAXTreeNodeId p),
    ("backendNodeId" A..=) <$> (pAccessibilityQueryAXTreeBackendNodeId p),
    ("objectId" A..=) <$> (pAccessibilityQueryAXTreeObjectId p),
    ("accessibleName" A..=) <$> (pAccessibilityQueryAXTreeAccessibleName p),
    ("role" A..=) <$> (pAccessibilityQueryAXTreeRole p)
    ]
data AccessibilityQueryAXTree = AccessibilityQueryAXTree
  {
    -- | A list of `Accessibility.AXNode` matching the specified attributes,
    --   including nodes that are ignored for accessibility.
    accessibilityQueryAXTreeNodes :: [AccessibilityAXNode]
  }
  deriving (Eq, Show)
instance FromJSON AccessibilityQueryAXTree where
  parseJSON = A.withObject "AccessibilityQueryAXTree" $ \o -> AccessibilityQueryAXTree
    <$> o A..: "nodes"
instance Command PAccessibilityQueryAXTree where
  type CommandResponse PAccessibilityQueryAXTree = AccessibilityQueryAXTree
  commandName _ = "Accessibility.queryAXTree"

