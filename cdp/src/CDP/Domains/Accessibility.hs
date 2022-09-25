{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type AccessibilityAxNodeId = String
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



data AccessibilityAxValueSource = AccessibilityAxValueSource {
   accessibilityAxValueSourceType :: AccessibilityAxValueSourceType,
   accessibilityAxValueSourceValue :: Maybe AccessibilityAxValue,
   accessibilityAxValueSourceAttribute :: Maybe String,
   accessibilityAxValueSourceAttributeValue :: Maybe AccessibilityAxValue,
   accessibilityAxValueSourceSuperseded :: Maybe Bool,
   accessibilityAxValueSourceNativeSource :: Maybe AccessibilityAxValueNativeSourceType,
   accessibilityAxValueSourceNativeSourceValue :: Maybe AccessibilityAxValue,
   accessibilityAxValueSourceInvalid :: Maybe Bool,
   accessibilityAxValueSourceInvalidReason :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxValueSource  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxValueSource where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data AccessibilityAxRelatedNode = AccessibilityAxRelatedNode {
   accessibilityAxRelatedNodeBackendDomNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   accessibilityAxRelatedNodeIdref :: Maybe String,
   accessibilityAxRelatedNodeText :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxRelatedNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxRelatedNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data AccessibilityAxProperty = AccessibilityAxProperty {
   accessibilityAxPropertyName :: AccessibilityAxPropertyName,
   accessibilityAxPropertyValue :: AccessibilityAxValue
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data AccessibilityAxValue = AccessibilityAxValue {
   accessibilityAxValueType :: AccessibilityAxValueType,
   accessibilityAxValueValue :: Maybe Int,
   accessibilityAxValueRelatedNodes :: Maybe [AccessibilityAxRelatedNode],
   accessibilityAxValueSources :: Maybe [AccessibilityAxValueSource]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


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



data AccessibilityAxNode = AccessibilityAxNode {
   accessibilityAxNodeNodeId :: AccessibilityAxNodeId,
   accessibilityAxNodeIgnored :: Bool,
   accessibilityAxNodeIgnoredReasons :: Maybe [AccessibilityAxProperty],
   accessibilityAxNodeRole :: Maybe AccessibilityAxValue,
   accessibilityAxNodeName :: Maybe AccessibilityAxValue,
   accessibilityAxNodeDescription :: Maybe AccessibilityAxValue,
   accessibilityAxNodeValue :: Maybe AccessibilityAxValue,
   accessibilityAxNodeProperties :: Maybe [AccessibilityAxProperty],
   accessibilityAxNodeParentId :: Maybe AccessibilityAxNodeId,
   accessibilityAxNodeChildIds :: Maybe [AccessibilityAxNodeId],
   accessibilityAxNodeBackendDomNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   accessibilityAxNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityAxNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  AccessibilityAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





data AccessibilityLoadComplete = AccessibilityLoadComplete {
   accessibilityLoadCompleteRoot :: AccessibilityAxNode
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityLoadComplete  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityLoadComplete where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data AccessibilityNodesUpdated = AccessibilityNodesUpdated {
   accessibilityNodesUpdatedNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)
instance ToJSON AccessibilityNodesUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  AccessibilityNodesUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }




accessibilityDisable :: Handle ev -> IO (Maybe Error)
accessibilityDisable handle = sendReceiveCommand handle "Accessibility.disable" (Nothing :: Maybe ())


accessibilityEnable :: Handle ev -> IO (Maybe Error)
accessibilityEnable handle = sendReceiveCommand handle "Accessibility.enable" (Nothing :: Maybe ())



data PAccessibilityGetPartialAxTree = PAccessibilityGetPartialAxTree {
   pAccessibilityGetPartialAxTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
   pAccessibilityGetPartialAxTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   pAccessibilityGetPartialAxTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
   pAccessibilityGetPartialAxTreeFetchRelatives :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetPartialAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetPartialAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


accessibilityGetPartialAxTree :: Handle ev -> PAccessibilityGetPartialAxTree -> IO (Either Error AccessibilityGetPartialAxTree)
accessibilityGetPartialAxTree handle params = sendReceiveCommandResult handle "Accessibility.getPartialAXTree" (Just params)

data AccessibilityGetPartialAxTree = AccessibilityGetPartialAxTree {
   accessibilityGetPartialAxTreeNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetPartialAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }

instance Command AccessibilityGetPartialAxTree where
   commandName _ = "Accessibility.getPartialAXTree"




data PAccessibilityGetFullAxTree = PAccessibilityGetFullAxTree {
   pAccessibilityGetFullAxTreeDepth :: Maybe Int,
   pAccessibilityGetFullAxTreeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetFullAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetFullAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


accessibilityGetFullAxTree :: Handle ev -> PAccessibilityGetFullAxTree -> IO (Either Error AccessibilityGetFullAxTree)
accessibilityGetFullAxTree handle params = sendReceiveCommandResult handle "Accessibility.getFullAXTree" (Just params)

data AccessibilityGetFullAxTree = AccessibilityGetFullAxTree {
   accessibilityGetFullAxTreeNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetFullAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetFullAxTree where
   commandName _ = "Accessibility.getFullAXTree"




data PAccessibilityGetRootAxNode = PAccessibilityGetRootAxNode {
   pAccessibilityGetRootAxNodeFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetRootAxNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetRootAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


accessibilityGetRootAxNode :: Handle ev -> PAccessibilityGetRootAxNode -> IO (Either Error AccessibilityGetRootAxNode)
accessibilityGetRootAxNode handle params = sendReceiveCommandResult handle "Accessibility.getRootAXNode" (Just params)

data AccessibilityGetRootAxNode = AccessibilityGetRootAxNode {
   accessibilityGetRootAxNodeNode :: AccessibilityAxNode
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetRootAxNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command AccessibilityGetRootAxNode where
   commandName _ = "Accessibility.getRootAXNode"




data PAccessibilityGetAxNodeAndAncestors = PAccessibilityGetAxNodeAndAncestors {
   pAccessibilityGetAxNodeAndAncestorsNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
   pAccessibilityGetAxNodeAndAncestorsBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   pAccessibilityGetAxNodeAndAncestorsObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetAxNodeAndAncestors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetAxNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


accessibilityGetAxNodeAndAncestors :: Handle ev -> PAccessibilityGetAxNodeAndAncestors -> IO (Either Error AccessibilityGetAxNodeAndAncestors)
accessibilityGetAxNodeAndAncestors handle params = sendReceiveCommandResult handle "Accessibility.getAXNodeAndAncestors" (Just params)

data AccessibilityGetAxNodeAndAncestors = AccessibilityGetAxNodeAndAncestors {
   accessibilityGetAxNodeAndAncestorsNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetAxNodeAndAncestors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }

instance Command AccessibilityGetAxNodeAndAncestors where
   commandName _ = "Accessibility.getAXNodeAndAncestors"




data PAccessibilityGetChildAxNodes = PAccessibilityGetChildAxNodes {
   pAccessibilityGetChildAxNodesId :: AccessibilityAxNodeId,
   pAccessibilityGetChildAxNodesFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityGetChildAxNodes  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityGetChildAxNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


accessibilityGetChildAxNodes :: Handle ev -> PAccessibilityGetChildAxNodes -> IO (Either Error AccessibilityGetChildAxNodes)
accessibilityGetChildAxNodes handle params = sendReceiveCommandResult handle "Accessibility.getChildAXNodes" (Just params)

data AccessibilityGetChildAxNodes = AccessibilityGetChildAxNodes {
   accessibilityGetChildAxNodesNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityGetChildAxNodes where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command AccessibilityGetChildAxNodes where
   commandName _ = "Accessibility.getChildAXNodes"




data PAccessibilityQueryAxTree = PAccessibilityQueryAxTree {
   pAccessibilityQueryAxTreeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
   pAccessibilityQueryAxTreeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   pAccessibilityQueryAxTreeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
   pAccessibilityQueryAxTreeAccessibleName :: Maybe String,
   pAccessibilityQueryAxTreeRole :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PAccessibilityQueryAxTree  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PAccessibilityQueryAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


accessibilityQueryAxTree :: Handle ev -> PAccessibilityQueryAxTree -> IO (Either Error AccessibilityQueryAxTree)
accessibilityQueryAxTree handle params = sendReceiveCommandResult handle "Accessibility.queryAXTree" (Just params)

data AccessibilityQueryAxTree = AccessibilityQueryAxTree {
   accessibilityQueryAxTreeNodes :: [AccessibilityAxNode]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  AccessibilityQueryAxTree where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command AccessibilityQueryAxTree where
   commandName _ = "Accessibility.queryAXTree"




