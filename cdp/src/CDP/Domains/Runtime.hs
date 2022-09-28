{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Runtime :
     Runtime domain exposes JavaScript runtime by means of remote evaluation and mirror objects.
     Evaluation results are returned as mirror object that expose object type, string representation
     and unique identifier that can be used for further object reference. Original objects are
     maintained in memory unless they are either explicitly released or are released along with the
     other objects in their object group.

-}


module CDP.Domains.Runtime (module CDP.Domains.Runtime) where

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



-- | Unique script identifier.
type RuntimeScriptId = String

-- | Represents the value serialiazed by the WebDriver BiDi specification
-- https://w3c.github.io/webdriver-bidi.
data RuntimeWebDriverValueType = RuntimeWebDriverValueTypeUndefined | RuntimeWebDriverValueTypeNull | RuntimeWebDriverValueTypeString | RuntimeWebDriverValueTypeNumber | RuntimeWebDriverValueTypeBoolean | RuntimeWebDriverValueTypeBigint | RuntimeWebDriverValueTypeRegexp | RuntimeWebDriverValueTypeDate | RuntimeWebDriverValueTypeSymbol | RuntimeWebDriverValueTypeArray | RuntimeWebDriverValueTypeObject | RuntimeWebDriverValueTypeFunction | RuntimeWebDriverValueTypeMap | RuntimeWebDriverValueTypeSet | RuntimeWebDriverValueTypeWeakmap | RuntimeWebDriverValueTypeWeakset | RuntimeWebDriverValueTypeError | RuntimeWebDriverValueTypeProxy | RuntimeWebDriverValueTypePromise | RuntimeWebDriverValueTypeTypedarray | RuntimeWebDriverValueTypeArraybuffer | RuntimeWebDriverValueTypeNode | RuntimeWebDriverValueTypeWindow
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeWebDriverValueType where
   parseJSON = A.withText  "RuntimeWebDriverValueType"  $ \v -> do
      case v of
         "undefined" -> pure RuntimeWebDriverValueTypeUndefined
         "null" -> pure RuntimeWebDriverValueTypeNull
         "string" -> pure RuntimeWebDriverValueTypeString
         "number" -> pure RuntimeWebDriverValueTypeNumber
         "boolean" -> pure RuntimeWebDriverValueTypeBoolean
         "bigint" -> pure RuntimeWebDriverValueTypeBigint
         "regexp" -> pure RuntimeWebDriverValueTypeRegexp
         "date" -> pure RuntimeWebDriverValueTypeDate
         "symbol" -> pure RuntimeWebDriverValueTypeSymbol
         "array" -> pure RuntimeWebDriverValueTypeArray
         "object" -> pure RuntimeWebDriverValueTypeObject
         "function" -> pure RuntimeWebDriverValueTypeFunction
         "map" -> pure RuntimeWebDriverValueTypeMap
         "set" -> pure RuntimeWebDriverValueTypeSet
         "weakmap" -> pure RuntimeWebDriverValueTypeWeakmap
         "weakset" -> pure RuntimeWebDriverValueTypeWeakset
         "error" -> pure RuntimeWebDriverValueTypeError
         "proxy" -> pure RuntimeWebDriverValueTypeProxy
         "promise" -> pure RuntimeWebDriverValueTypePromise
         "typedarray" -> pure RuntimeWebDriverValueTypeTypedarray
         "arraybuffer" -> pure RuntimeWebDriverValueTypeArraybuffer
         "node" -> pure RuntimeWebDriverValueTypeNode
         "window" -> pure RuntimeWebDriverValueTypeWindow
         _ -> fail "failed to parse RuntimeWebDriverValueType"

instance ToJSON RuntimeWebDriverValueType where
   toJSON v = A.String $
      case v of
         RuntimeWebDriverValueTypeUndefined -> "undefined"
         RuntimeWebDriverValueTypeNull -> "null"
         RuntimeWebDriverValueTypeString -> "string"
         RuntimeWebDriverValueTypeNumber -> "number"
         RuntimeWebDriverValueTypeBoolean -> "boolean"
         RuntimeWebDriverValueTypeBigint -> "bigint"
         RuntimeWebDriverValueTypeRegexp -> "regexp"
         RuntimeWebDriverValueTypeDate -> "date"
         RuntimeWebDriverValueTypeSymbol -> "symbol"
         RuntimeWebDriverValueTypeArray -> "array"
         RuntimeWebDriverValueTypeObject -> "object"
         RuntimeWebDriverValueTypeFunction -> "function"
         RuntimeWebDriverValueTypeMap -> "map"
         RuntimeWebDriverValueTypeSet -> "set"
         RuntimeWebDriverValueTypeWeakmap -> "weakmap"
         RuntimeWebDriverValueTypeWeakset -> "weakset"
         RuntimeWebDriverValueTypeError -> "error"
         RuntimeWebDriverValueTypeProxy -> "proxy"
         RuntimeWebDriverValueTypePromise -> "promise"
         RuntimeWebDriverValueTypeTypedarray -> "typedarray"
         RuntimeWebDriverValueTypeArraybuffer -> "arraybuffer"
         RuntimeWebDriverValueTypeNode -> "node"
         RuntimeWebDriverValueTypeWindow -> "window"



data RuntimeWebDriverValue = RuntimeWebDriverValue {



} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeWebDriverValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  RuntimeWebDriverValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Unique object identifier.
type RuntimeRemoteObjectId = String

-- | Primitive value which cannot be JSON-stringified. Includes values `-0`, `NaN`, `Infinity`,
-- `-Infinity`, and bigint literals.
type RuntimeUnserializableValue = String

-- | Mirror object referencing original JavaScript object.
data RuntimeRemoteObjectType = RuntimeRemoteObjectTypeObject | RuntimeRemoteObjectTypeFunction | RuntimeRemoteObjectTypeUndefined | RuntimeRemoteObjectTypeString | RuntimeRemoteObjectTypeNumber | RuntimeRemoteObjectTypeBoolean | RuntimeRemoteObjectTypeSymbol | RuntimeRemoteObjectTypeBigint
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectType where
   parseJSON = A.withText  "RuntimeRemoteObjectType"  $ \v -> do
      case v of
         "object" -> pure RuntimeRemoteObjectTypeObject
         "function" -> pure RuntimeRemoteObjectTypeFunction
         "undefined" -> pure RuntimeRemoteObjectTypeUndefined
         "string" -> pure RuntimeRemoteObjectTypeString
         "number" -> pure RuntimeRemoteObjectTypeNumber
         "boolean" -> pure RuntimeRemoteObjectTypeBoolean
         "symbol" -> pure RuntimeRemoteObjectTypeSymbol
         "bigint" -> pure RuntimeRemoteObjectTypeBigint
         _ -> fail "failed to parse RuntimeRemoteObjectType"

instance ToJSON RuntimeRemoteObjectType where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectTypeObject -> "object"
         RuntimeRemoteObjectTypeFunction -> "function"
         RuntimeRemoteObjectTypeUndefined -> "undefined"
         RuntimeRemoteObjectTypeString -> "string"
         RuntimeRemoteObjectTypeNumber -> "number"
         RuntimeRemoteObjectTypeBoolean -> "boolean"
         RuntimeRemoteObjectTypeSymbol -> "symbol"
         RuntimeRemoteObjectTypeBigint -> "bigint"


data RuntimeRemoteObjectSubtype = RuntimeRemoteObjectSubtypeArray | RuntimeRemoteObjectSubtypeNull | RuntimeRemoteObjectSubtypeNode | RuntimeRemoteObjectSubtypeRegexp | RuntimeRemoteObjectSubtypeDate | RuntimeRemoteObjectSubtypeMap | RuntimeRemoteObjectSubtypeSet | RuntimeRemoteObjectSubtypeWeakmap | RuntimeRemoteObjectSubtypeWeakset | RuntimeRemoteObjectSubtypeIterator | RuntimeRemoteObjectSubtypeGenerator | RuntimeRemoteObjectSubtypeError | RuntimeRemoteObjectSubtypeProxy | RuntimeRemoteObjectSubtypePromise | RuntimeRemoteObjectSubtypeTypedarray | RuntimeRemoteObjectSubtypeArraybuffer | RuntimeRemoteObjectSubtypeDataview | RuntimeRemoteObjectSubtypeWebassemblymemory | RuntimeRemoteObjectSubtypeWasmvalue
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectSubtype where
   parseJSON = A.withText  "RuntimeRemoteObjectSubtype"  $ \v -> do
      case v of
         "array" -> pure RuntimeRemoteObjectSubtypeArray
         "null" -> pure RuntimeRemoteObjectSubtypeNull
         "node" -> pure RuntimeRemoteObjectSubtypeNode
         "regexp" -> pure RuntimeRemoteObjectSubtypeRegexp
         "date" -> pure RuntimeRemoteObjectSubtypeDate
         "map" -> pure RuntimeRemoteObjectSubtypeMap
         "set" -> pure RuntimeRemoteObjectSubtypeSet
         "weakmap" -> pure RuntimeRemoteObjectSubtypeWeakmap
         "weakset" -> pure RuntimeRemoteObjectSubtypeWeakset
         "iterator" -> pure RuntimeRemoteObjectSubtypeIterator
         "generator" -> pure RuntimeRemoteObjectSubtypeGenerator
         "error" -> pure RuntimeRemoteObjectSubtypeError
         "proxy" -> pure RuntimeRemoteObjectSubtypeProxy
         "promise" -> pure RuntimeRemoteObjectSubtypePromise
         "typedarray" -> pure RuntimeRemoteObjectSubtypeTypedarray
         "arraybuffer" -> pure RuntimeRemoteObjectSubtypeArraybuffer
         "dataview" -> pure RuntimeRemoteObjectSubtypeDataview
         "webassemblymemory" -> pure RuntimeRemoteObjectSubtypeWebassemblymemory
         "wasmvalue" -> pure RuntimeRemoteObjectSubtypeWasmvalue
         _ -> fail "failed to parse RuntimeRemoteObjectSubtype"

instance ToJSON RuntimeRemoteObjectSubtype where
   toJSON v = A.String $
      case v of
         RuntimeRemoteObjectSubtypeArray -> "array"
         RuntimeRemoteObjectSubtypeNull -> "null"
         RuntimeRemoteObjectSubtypeNode -> "node"
         RuntimeRemoteObjectSubtypeRegexp -> "regexp"
         RuntimeRemoteObjectSubtypeDate -> "date"
         RuntimeRemoteObjectSubtypeMap -> "map"
         RuntimeRemoteObjectSubtypeSet -> "set"
         RuntimeRemoteObjectSubtypeWeakmap -> "weakmap"
         RuntimeRemoteObjectSubtypeWeakset -> "weakset"
         RuntimeRemoteObjectSubtypeIterator -> "iterator"
         RuntimeRemoteObjectSubtypeGenerator -> "generator"
         RuntimeRemoteObjectSubtypeError -> "error"
         RuntimeRemoteObjectSubtypeProxy -> "proxy"
         RuntimeRemoteObjectSubtypePromise -> "promise"
         RuntimeRemoteObjectSubtypeTypedarray -> "typedarray"
         RuntimeRemoteObjectSubtypeArraybuffer -> "arraybuffer"
         RuntimeRemoteObjectSubtypeDataview -> "dataview"
         RuntimeRemoteObjectSubtypeWebassemblymemory -> "webassemblymemory"
         RuntimeRemoteObjectSubtypeWasmvalue -> "wasmvalue"



data RuntimeRemoteObject = RuntimeRemoteObject {
   runtimeRemoteObjectType :: RuntimeRemoteObjectType, -- ^ Object type.
   runtimeRemoteObjectSubtype :: RuntimeRemoteObjectSubtype, -- ^ Object subtype hint. Specified for `object` type values only.
NOTE: If you change anything here, make sure to also update
`subtype` in `ObjectPreview` and `PropertyPreview` below.
   runtimeRemoteObjectClassName :: RuntimeRemoteObjectClassName, -- ^ Object class (constructor) name. Specified for `object` type values only.
   runtimeRemoteObjectValue :: RuntimeRemoteObjectValue, -- ^ Remote object value in case of primitive values or JSON values (if it was requested).
   runtimeRemoteObjectUnserializableValue :: RuntimeRemoteObjectUnserializableValue, -- ^ Primitive value which can not be JSON-stringified does not have `value`, but gets this
property.
   runtimeRemoteObjectDescription :: RuntimeRemoteObjectDescription, -- ^ String representation of the object.
   runtimeRemoteObjectWebDriverValue :: RuntimeRemoteObjectWebDriverValue, -- ^ WebDriver BiDi representation of the value.
   runtimeRemoteObjectObjectId :: RuntimeRemoteObjectObjectId, -- ^ Unique object identifier (for non-primitive values).
   runtimeRemoteObjectPreview :: RuntimeRemoteObjectPreview, -- ^ Preview containing abbreviated property values. Specified for `object` type values only.

} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeRemoteObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeRemoteObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'Runtime.CustomPreview' .
data RuntimeCustomPreview = RuntimeCustomPreview {
   runtimeCustomPreviewHeader :: RuntimeCustomPreviewHeader, -- ^ The JSON-stringified result of formatter.header(object, config) call.
It contains json ML array that represents RemoteObject.
   runtimeCustomPreviewBodyGetterId :: RuntimeCustomPreviewBodyGetterId -- ^ If formatter returns true as a result of formatter.hasBody call then bodyGetterId will
contain RemoteObjectId for the function that returns result of formatter.body(object, config) call.
The result value is json ML array.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCustomPreview  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  RuntimeCustomPreview where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Object containing abbreviated remote object value.
data RuntimeObjectPreviewType = RuntimeObjectPreviewTypeObject | RuntimeObjectPreviewTypeFunction | RuntimeObjectPreviewTypeUndefined | RuntimeObjectPreviewTypeString | RuntimeObjectPreviewTypeNumber | RuntimeObjectPreviewTypeBoolean | RuntimeObjectPreviewTypeSymbol | RuntimeObjectPreviewTypeBigint
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeObjectPreviewType where
   parseJSON = A.withText  "RuntimeObjectPreviewType"  $ \v -> do
      case v of
         "object" -> pure RuntimeObjectPreviewTypeObject
         "function" -> pure RuntimeObjectPreviewTypeFunction
         "undefined" -> pure RuntimeObjectPreviewTypeUndefined
         "string" -> pure RuntimeObjectPreviewTypeString
         "number" -> pure RuntimeObjectPreviewTypeNumber
         "boolean" -> pure RuntimeObjectPreviewTypeBoolean
         "symbol" -> pure RuntimeObjectPreviewTypeSymbol
         "bigint" -> pure RuntimeObjectPreviewTypeBigint
         _ -> fail "failed to parse RuntimeObjectPreviewType"

instance ToJSON RuntimeObjectPreviewType where
   toJSON v = A.String $
      case v of
         RuntimeObjectPreviewTypeObject -> "object"
         RuntimeObjectPreviewTypeFunction -> "function"
         RuntimeObjectPreviewTypeUndefined -> "undefined"
         RuntimeObjectPreviewTypeString -> "string"
         RuntimeObjectPreviewTypeNumber -> "number"
         RuntimeObjectPreviewTypeBoolean -> "boolean"
         RuntimeObjectPreviewTypeSymbol -> "symbol"
         RuntimeObjectPreviewTypeBigint -> "bigint"


data RuntimeObjectPreviewSubtype = RuntimeObjectPreviewSubtypeArray | RuntimeObjectPreviewSubtypeNull | RuntimeObjectPreviewSubtypeNode | RuntimeObjectPreviewSubtypeRegexp | RuntimeObjectPreviewSubtypeDate | RuntimeObjectPreviewSubtypeMap | RuntimeObjectPreviewSubtypeSet | RuntimeObjectPreviewSubtypeWeakmap | RuntimeObjectPreviewSubtypeWeakset | RuntimeObjectPreviewSubtypeIterator | RuntimeObjectPreviewSubtypeGenerator | RuntimeObjectPreviewSubtypeError | RuntimeObjectPreviewSubtypeProxy | RuntimeObjectPreviewSubtypePromise | RuntimeObjectPreviewSubtypeTypedarray | RuntimeObjectPreviewSubtypeArraybuffer | RuntimeObjectPreviewSubtypeDataview | RuntimeObjectPreviewSubtypeWebassemblymemory | RuntimeObjectPreviewSubtypeWasmvalue
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeObjectPreviewSubtype where
   parseJSON = A.withText  "RuntimeObjectPreviewSubtype"  $ \v -> do
      case v of
         "array" -> pure RuntimeObjectPreviewSubtypeArray
         "null" -> pure RuntimeObjectPreviewSubtypeNull
         "node" -> pure RuntimeObjectPreviewSubtypeNode
         "regexp" -> pure RuntimeObjectPreviewSubtypeRegexp
         "date" -> pure RuntimeObjectPreviewSubtypeDate
         "map" -> pure RuntimeObjectPreviewSubtypeMap
         "set" -> pure RuntimeObjectPreviewSubtypeSet
         "weakmap" -> pure RuntimeObjectPreviewSubtypeWeakmap
         "weakset" -> pure RuntimeObjectPreviewSubtypeWeakset
         "iterator" -> pure RuntimeObjectPreviewSubtypeIterator
         "generator" -> pure RuntimeObjectPreviewSubtypeGenerator
         "error" -> pure RuntimeObjectPreviewSubtypeError
         "proxy" -> pure RuntimeObjectPreviewSubtypeProxy
         "promise" -> pure RuntimeObjectPreviewSubtypePromise
         "typedarray" -> pure RuntimeObjectPreviewSubtypeTypedarray
         "arraybuffer" -> pure RuntimeObjectPreviewSubtypeArraybuffer
         "dataview" -> pure RuntimeObjectPreviewSubtypeDataview
         "webassemblymemory" -> pure RuntimeObjectPreviewSubtypeWebassemblymemory
         "wasmvalue" -> pure RuntimeObjectPreviewSubtypeWasmvalue
         _ -> fail "failed to parse RuntimeObjectPreviewSubtype"

instance ToJSON RuntimeObjectPreviewSubtype where
   toJSON v = A.String $
      case v of
         RuntimeObjectPreviewSubtypeArray -> "array"
         RuntimeObjectPreviewSubtypeNull -> "null"
         RuntimeObjectPreviewSubtypeNode -> "node"
         RuntimeObjectPreviewSubtypeRegexp -> "regexp"
         RuntimeObjectPreviewSubtypeDate -> "date"
         RuntimeObjectPreviewSubtypeMap -> "map"
         RuntimeObjectPreviewSubtypeSet -> "set"
         RuntimeObjectPreviewSubtypeWeakmap -> "weakmap"
         RuntimeObjectPreviewSubtypeWeakset -> "weakset"
         RuntimeObjectPreviewSubtypeIterator -> "iterator"
         RuntimeObjectPreviewSubtypeGenerator -> "generator"
         RuntimeObjectPreviewSubtypeError -> "error"
         RuntimeObjectPreviewSubtypeProxy -> "proxy"
         RuntimeObjectPreviewSubtypePromise -> "promise"
         RuntimeObjectPreviewSubtypeTypedarray -> "typedarray"
         RuntimeObjectPreviewSubtypeArraybuffer -> "arraybuffer"
         RuntimeObjectPreviewSubtypeDataview -> "dataview"
         RuntimeObjectPreviewSubtypeWebassemblymemory -> "webassemblymemory"
         RuntimeObjectPreviewSubtypeWasmvalue -> "wasmvalue"



data RuntimeObjectPreview = RuntimeObjectPreview {
   runtimeObjectPreviewType :: RuntimeObjectPreviewType, -- ^ Object type.
   runtimeObjectPreviewSubtype :: RuntimeObjectPreviewSubtype, -- ^ Object subtype hint. Specified for `object` type values only.
   runtimeObjectPreviewDescription :: RuntimeObjectPreviewDescription, -- ^ String representation of the object.
   runtimeObjectPreviewOverflow :: RuntimeObjectPreviewOverflow, -- ^ True iff some of the properties or entries of the original object did not fit.
   runtimeObjectPreviewProperties :: RuntimeObjectPreviewProperties, -- ^ List of the properties.
   runtimeObjectPreviewEntries :: RuntimeObjectPreviewEntries -- ^ List of the entries. Specified for `map` and `set` subtype values only.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeObjectPreview  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  RuntimeObjectPreview where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'Runtime.PropertyPreview' .
data RuntimePropertyPreviewType = RuntimePropertyPreviewTypeObject | RuntimePropertyPreviewTypeFunction | RuntimePropertyPreviewTypeUndefined | RuntimePropertyPreviewTypeString | RuntimePropertyPreviewTypeNumber | RuntimePropertyPreviewTypeBoolean | RuntimePropertyPreviewTypeSymbol | RuntimePropertyPreviewTypeAccessor | RuntimePropertyPreviewTypeBigint
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimePropertyPreviewType where
   parseJSON = A.withText  "RuntimePropertyPreviewType"  $ \v -> do
      case v of
         "object" -> pure RuntimePropertyPreviewTypeObject
         "function" -> pure RuntimePropertyPreviewTypeFunction
         "undefined" -> pure RuntimePropertyPreviewTypeUndefined
         "string" -> pure RuntimePropertyPreviewTypeString
         "number" -> pure RuntimePropertyPreviewTypeNumber
         "boolean" -> pure RuntimePropertyPreviewTypeBoolean
         "symbol" -> pure RuntimePropertyPreviewTypeSymbol
         "accessor" -> pure RuntimePropertyPreviewTypeAccessor
         "bigint" -> pure RuntimePropertyPreviewTypeBigint
         _ -> fail "failed to parse RuntimePropertyPreviewType"

instance ToJSON RuntimePropertyPreviewType where
   toJSON v = A.String $
      case v of
         RuntimePropertyPreviewTypeObject -> "object"
         RuntimePropertyPreviewTypeFunction -> "function"
         RuntimePropertyPreviewTypeUndefined -> "undefined"
         RuntimePropertyPreviewTypeString -> "string"
         RuntimePropertyPreviewTypeNumber -> "number"
         RuntimePropertyPreviewTypeBoolean -> "boolean"
         RuntimePropertyPreviewTypeSymbol -> "symbol"
         RuntimePropertyPreviewTypeAccessor -> "accessor"
         RuntimePropertyPreviewTypeBigint -> "bigint"


data RuntimePropertyPreviewSubtype = RuntimePropertyPreviewSubtypeArray | RuntimePropertyPreviewSubtypeNull | RuntimePropertyPreviewSubtypeNode | RuntimePropertyPreviewSubtypeRegexp | RuntimePropertyPreviewSubtypeDate | RuntimePropertyPreviewSubtypeMap | RuntimePropertyPreviewSubtypeSet | RuntimePropertyPreviewSubtypeWeakmap | RuntimePropertyPreviewSubtypeWeakset | RuntimePropertyPreviewSubtypeIterator | RuntimePropertyPreviewSubtypeGenerator | RuntimePropertyPreviewSubtypeError | RuntimePropertyPreviewSubtypeProxy | RuntimePropertyPreviewSubtypePromise | RuntimePropertyPreviewSubtypeTypedarray | RuntimePropertyPreviewSubtypeArraybuffer | RuntimePropertyPreviewSubtypeDataview | RuntimePropertyPreviewSubtypeWebassemblymemory | RuntimePropertyPreviewSubtypeWasmvalue
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimePropertyPreviewSubtype where
   parseJSON = A.withText  "RuntimePropertyPreviewSubtype"  $ \v -> do
      case v of
         "array" -> pure RuntimePropertyPreviewSubtypeArray
         "null" -> pure RuntimePropertyPreviewSubtypeNull
         "node" -> pure RuntimePropertyPreviewSubtypeNode
         "regexp" -> pure RuntimePropertyPreviewSubtypeRegexp
         "date" -> pure RuntimePropertyPreviewSubtypeDate
         "map" -> pure RuntimePropertyPreviewSubtypeMap
         "set" -> pure RuntimePropertyPreviewSubtypeSet
         "weakmap" -> pure RuntimePropertyPreviewSubtypeWeakmap
         "weakset" -> pure RuntimePropertyPreviewSubtypeWeakset
         "iterator" -> pure RuntimePropertyPreviewSubtypeIterator
         "generator" -> pure RuntimePropertyPreviewSubtypeGenerator
         "error" -> pure RuntimePropertyPreviewSubtypeError
         "proxy" -> pure RuntimePropertyPreviewSubtypeProxy
         "promise" -> pure RuntimePropertyPreviewSubtypePromise
         "typedarray" -> pure RuntimePropertyPreviewSubtypeTypedarray
         "arraybuffer" -> pure RuntimePropertyPreviewSubtypeArraybuffer
         "dataview" -> pure RuntimePropertyPreviewSubtypeDataview
         "webassemblymemory" -> pure RuntimePropertyPreviewSubtypeWebassemblymemory
         "wasmvalue" -> pure RuntimePropertyPreviewSubtypeWasmvalue
         _ -> fail "failed to parse RuntimePropertyPreviewSubtype"

instance ToJSON RuntimePropertyPreviewSubtype where
   toJSON v = A.String $
      case v of
         RuntimePropertyPreviewSubtypeArray -> "array"
         RuntimePropertyPreviewSubtypeNull -> "null"
         RuntimePropertyPreviewSubtypeNode -> "node"
         RuntimePropertyPreviewSubtypeRegexp -> "regexp"
         RuntimePropertyPreviewSubtypeDate -> "date"
         RuntimePropertyPreviewSubtypeMap -> "map"
         RuntimePropertyPreviewSubtypeSet -> "set"
         RuntimePropertyPreviewSubtypeWeakmap -> "weakmap"
         RuntimePropertyPreviewSubtypeWeakset -> "weakset"
         RuntimePropertyPreviewSubtypeIterator -> "iterator"
         RuntimePropertyPreviewSubtypeGenerator -> "generator"
         RuntimePropertyPreviewSubtypeError -> "error"
         RuntimePropertyPreviewSubtypeProxy -> "proxy"
         RuntimePropertyPreviewSubtypePromise -> "promise"
         RuntimePropertyPreviewSubtypeTypedarray -> "typedarray"
         RuntimePropertyPreviewSubtypeArraybuffer -> "arraybuffer"
         RuntimePropertyPreviewSubtypeDataview -> "dataview"
         RuntimePropertyPreviewSubtypeWebassemblymemory -> "webassemblymemory"
         RuntimePropertyPreviewSubtypeWasmvalue -> "wasmvalue"



data RuntimePropertyPreview = RuntimePropertyPreview {
   runtimePropertyPreviewName :: RuntimePropertyPreviewName, -- ^ Property name.
   runtimePropertyPreviewType :: RuntimePropertyPreviewType, -- ^ Object type. Accessor means that the property itself is an accessor property.
   runtimePropertyPreviewValue :: RuntimePropertyPreviewValue, -- ^ User-friendly property value string.
   runtimePropertyPreviewValuePreview :: RuntimePropertyPreviewValuePreview, -- ^ Nested value preview.
   runtimePropertyPreviewSubtype :: RuntimePropertyPreviewSubtype -- ^ Object subtype hint. Specified for `object` type values only.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimePropertyPreview  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  RuntimePropertyPreview where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Runtime.EntryPreview' .
data RuntimeEntryPreview = RuntimeEntryPreview {
   runtimeEntryPreviewKey :: RuntimeEntryPreviewKey, -- ^ Preview of the key. Specified for map-like collection entries.
   runtimeEntryPreviewValue :: RuntimeEntryPreviewValue -- ^ Preview of the value.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeEntryPreview  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeEntryPreview where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Object property descriptor.
data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
   runtimePropertyDescriptorName :: RuntimePropertyDescriptorName, -- ^ Property name or symbol description.
   runtimePropertyDescriptorValue :: RuntimePropertyDescriptorValue, -- ^ The value associated with the property.
   runtimePropertyDescriptorWritable :: RuntimePropertyDescriptorWritable, -- ^ True if the value associated with the property may be changed (data descriptors only).
   runtimePropertyDescriptorGet :: RuntimePropertyDescriptorGet, -- ^ A function which serves as a getter for the property, or `undefined` if there is no getter
(accessor descriptors only).
   runtimePropertyDescriptorSet :: RuntimePropertyDescriptorSet, -- ^ A function which serves as a setter for the property, or `undefined` if there is no setter
(accessor descriptors only).
   runtimePropertyDescriptorConfigurable :: RuntimePropertyDescriptorConfigurable, -- ^ True if the type of this property descriptor may be changed and if the property may be
deleted from the corresponding object.
   runtimePropertyDescriptorEnumerable :: RuntimePropertyDescriptorEnumerable, -- ^ True if this property shows up during enumeration of the properties on the corresponding
object.
   runtimePropertyDescriptorWasThrown :: RuntimePropertyDescriptorWasThrown, -- ^ True if the result was thrown during the evaluation.
   runtimePropertyDescriptorIsOwn :: RuntimePropertyDescriptorIsOwn, -- ^ True if the property is owned for the object.
   runtimePropertyDescriptorSymbol :: RuntimePropertyDescriptorSymbol -- ^ Property symbol object, if the property is of the `symbol` type.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimePropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  RuntimePropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Object internal property descriptor. This property isn't normally visible in JavaScript code.
data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
   runtimeInternalPropertyDescriptorName :: RuntimeInternalPropertyDescriptorName, -- ^ Conventional property name.
   runtimeInternalPropertyDescriptorValue :: RuntimeInternalPropertyDescriptorValue -- ^ The value associated with the property.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInternalPropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  RuntimeInternalPropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Object private field descriptor.
data RuntimePrivatePropertyDescriptor = RuntimePrivatePropertyDescriptor {
   runtimePrivatePropertyDescriptorName :: RuntimePrivatePropertyDescriptorName, -- ^ Private property name.
   runtimePrivatePropertyDescriptorValue :: RuntimePrivatePropertyDescriptorValue, -- ^ The value associated with the private property.
   runtimePrivatePropertyDescriptorGet :: RuntimePrivatePropertyDescriptorGet, -- ^ A function which serves as a getter for the private property,
or `undefined` if there is no getter (accessor descriptors only).
   runtimePrivatePropertyDescriptorSet :: RuntimePrivatePropertyDescriptorSet -- ^ A function which serves as a setter for the private property,
or `undefined` if there is no setter (accessor descriptors only).
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimePrivatePropertyDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  RuntimePrivatePropertyDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Represents function call argument. Either remote object id `objectId`, primitive `value`,
-- unserializable primitive value or neither of (for undefined) them should be specified.
data RuntimeCallArgument = RuntimeCallArgument {
   runtimeCallArgumentValue :: RuntimeCallArgumentValue, -- ^ Primitive value or serializable javascript object.
   runtimeCallArgumentUnserializableValue :: RuntimeCallArgumentUnserializableValue, -- ^ Primitive value which can not be JSON-stringified.
   runtimeCallArgumentObjectId :: RuntimeCallArgumentObjectId -- ^ Remote object handle.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallArgument  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallArgument where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Id of an execution context.
type RuntimeExecutionContextId = Int

-- | Description of an isolated world.
data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
   runtimeExecutionContextDescriptionId :: RuntimeExecutionContextDescriptionId, -- ^ Unique id of the execution context. It can be used to specify in which execution context
script evaluation should be performed.
   runtimeExecutionContextDescriptionOrigin :: RuntimeExecutionContextDescriptionOrigin, -- ^ Execution context origin.
   runtimeExecutionContextDescriptionName :: RuntimeExecutionContextDescriptionName, -- ^ Human readable name describing given context.
   runtimeExecutionContextDescriptionUniqueId :: RuntimeExecutionContextDescriptionUniqueId, -- ^ A system-unique execution context identifier. Unlike the id, this is unique across
multiple processes, so can be reliably used to identify specific context while backend
performs a cross-process navigation.
   runtimeExecutionContextDescriptionAuxData :: RuntimeExecutionContextDescriptionAuxData -- ^ Embedder-specific auxiliary data.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDescription  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDescription where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }



-- | Detailed information about exception (or error) that was thrown during script compilation or
-- execution.
data RuntimeExceptionDetails = RuntimeExceptionDetails {
   runtimeExceptionDetailsExceptionId :: RuntimeExceptionDetailsExceptionId, -- ^ Exception id.
   runtimeExceptionDetailsText :: RuntimeExceptionDetailsText, -- ^ Exception text, which should be used together with exception object when available.
   runtimeExceptionDetailsLineNumber :: RuntimeExceptionDetailsLineNumber, -- ^ Line number of the exception location (0-based).
   runtimeExceptionDetailsColumnNumber :: RuntimeExceptionDetailsColumnNumber, -- ^ Column number of the exception location (0-based).
   runtimeExceptionDetailsScriptId :: RuntimeExceptionDetailsScriptId, -- ^ Script ID of the exception location.
   runtimeExceptionDetailsUrl :: RuntimeExceptionDetailsUrl, -- ^ URL of the exception location, to be used when the script was not reported.
   runtimeExceptionDetailsStackTrace :: RuntimeExceptionDetailsStackTrace, -- ^ JavaScript stack trace if available.
   runtimeExceptionDetailsException :: RuntimeExceptionDetailsException, -- ^ Exception object if available.
   runtimeExceptionDetailsExecutionContextId :: RuntimeExceptionDetailsExecutionContextId, -- ^ Identifier of the context where exception happened.
   runtimeExceptionDetailsExceptionMetaData :: RuntimeExceptionDetailsExceptionMetaData -- ^ Dictionary with entries of meta data that the client associated
with this exception, such as information about associated network
requests, etc.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Number of milliseconds since epoch.
type RuntimeTimestamp = Double

-- | Number of milliseconds.
type RuntimeTimeDelta = Double

-- | Stack entry for runtime errors and assertions.
data RuntimeCallFrame = RuntimeCallFrame {
   runtimeCallFrameFunctionName :: RuntimeCallFrameFunctionName, -- ^ JavaScript function name.
   runtimeCallFrameScriptId :: RuntimeCallFrameScriptId, -- ^ JavaScript script id.
   runtimeCallFrameUrl :: RuntimeCallFrameUrl, -- ^ JavaScript script name or url.
   runtimeCallFrameLineNumber :: RuntimeCallFrameLineNumber, -- ^ JavaScript script line number (0-based).
   runtimeCallFrameColumnNumber :: RuntimeCallFrameColumnNumber -- ^ JavaScript script column number (0-based).
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeCallFrame  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  RuntimeCallFrame where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Call frames for assertions or error messages.
data RuntimeStackTrace = RuntimeStackTrace {
   runtimeStackTraceDescription :: RuntimeStackTraceDescription, -- ^ String label of this stack trace. For async traces this may be a name of the function that
initiated the async call.
   runtimeStackTraceCallFrames :: RuntimeStackTraceCallFrames, -- ^ JavaScript function name.
   runtimeStackTraceParent :: RuntimeStackTraceParent, -- ^ Asynchronous JavaScript stack trace that preceded this stack, if available.
   runtimeStackTraceParentId :: RuntimeStackTraceParentId -- ^ Asynchronous JavaScript stack trace that preceded this stack, if available.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeStackTrace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  RuntimeStackTrace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Unique identifier of current debugger.
type RuntimeUniqueDebuggerId = String

-- | If `debuggerId` is set stack trace comes from another debugger and can be resolved there. This
-- allows to track cross-debugger calls. See `Runtime.StackTrace` and `Debugger.paused` for usages.
data RuntimeStackTraceId = RuntimeStackTraceId {


} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeStackTraceId  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  RuntimeStackTraceId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Type of the 'Runtime.bindingCalled' event.
data RuntimeBindingCalled = RuntimeBindingCalled {


   runtimeBindingCalledExecutionContextId :: RuntimeBindingCalledExecutionContextId -- ^ Identifier of the context where the call was made.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeBindingCalled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  RuntimeBindingCalled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'Runtime.consoleAPICalled' event.
data RuntimeConsoleApiCalledType = RuntimeConsoleApiCalledTypeLog | RuntimeConsoleApiCalledTypeDebug | RuntimeConsoleApiCalledTypeInfo | RuntimeConsoleApiCalledTypeError | RuntimeConsoleApiCalledTypeWarning | RuntimeConsoleApiCalledTypeDir | RuntimeConsoleApiCalledTypeDirxml | RuntimeConsoleApiCalledTypeTable | RuntimeConsoleApiCalledTypeTrace | RuntimeConsoleApiCalledTypeClear | RuntimeConsoleApiCalledTypeStartGroup | RuntimeConsoleApiCalledTypeStartGroupCollapsed | RuntimeConsoleApiCalledTypeEndGroup | RuntimeConsoleApiCalledTypeAssert | RuntimeConsoleApiCalledTypeProfile | RuntimeConsoleApiCalledTypeProfileEnd | RuntimeConsoleApiCalledTypeCount | RuntimeConsoleApiCalledTypeTimeEnd
   deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeConsoleApiCalledType where
   parseJSON = A.withText  "RuntimeConsoleApiCalledType"  $ \v -> do
      case v of
         "log" -> pure RuntimeConsoleApiCalledTypeLog
         "debug" -> pure RuntimeConsoleApiCalledTypeDebug
         "info" -> pure RuntimeConsoleApiCalledTypeInfo
         "error" -> pure RuntimeConsoleApiCalledTypeError
         "warning" -> pure RuntimeConsoleApiCalledTypeWarning
         "dir" -> pure RuntimeConsoleApiCalledTypeDir
         "dirxml" -> pure RuntimeConsoleApiCalledTypeDirxml
         "table" -> pure RuntimeConsoleApiCalledTypeTable
         "trace" -> pure RuntimeConsoleApiCalledTypeTrace
         "clear" -> pure RuntimeConsoleApiCalledTypeClear
         "startGroup" -> pure RuntimeConsoleApiCalledTypeStartGroup
         "startGroupCollapsed" -> pure RuntimeConsoleApiCalledTypeStartGroupCollapsed
         "endGroup" -> pure RuntimeConsoleApiCalledTypeEndGroup
         "assert" -> pure RuntimeConsoleApiCalledTypeAssert
         "profile" -> pure RuntimeConsoleApiCalledTypeProfile
         "profileEnd" -> pure RuntimeConsoleApiCalledTypeProfileEnd
         "count" -> pure RuntimeConsoleApiCalledTypeCount
         "timeEnd" -> pure RuntimeConsoleApiCalledTypeTimeEnd
         _ -> fail "failed to parse RuntimeConsoleApiCalledType"

instance ToJSON RuntimeConsoleApiCalledType where
   toJSON v = A.String $
      case v of
         RuntimeConsoleApiCalledTypeLog -> "log"
         RuntimeConsoleApiCalledTypeDebug -> "debug"
         RuntimeConsoleApiCalledTypeInfo -> "info"
         RuntimeConsoleApiCalledTypeError -> "error"
         RuntimeConsoleApiCalledTypeWarning -> "warning"
         RuntimeConsoleApiCalledTypeDir -> "dir"
         RuntimeConsoleApiCalledTypeDirxml -> "dirxml"
         RuntimeConsoleApiCalledTypeTable -> "table"
         RuntimeConsoleApiCalledTypeTrace -> "trace"
         RuntimeConsoleApiCalledTypeClear -> "clear"
         RuntimeConsoleApiCalledTypeStartGroup -> "startGroup"
         RuntimeConsoleApiCalledTypeStartGroupCollapsed -> "startGroupCollapsed"
         RuntimeConsoleApiCalledTypeEndGroup -> "endGroup"
         RuntimeConsoleApiCalledTypeAssert -> "assert"
         RuntimeConsoleApiCalledTypeProfile -> "profile"
         RuntimeConsoleApiCalledTypeProfileEnd -> "profileEnd"
         RuntimeConsoleApiCalledTypeCount -> "count"
         RuntimeConsoleApiCalledTypeTimeEnd -> "timeEnd"



data RuntimeConsoleApiCalled = RuntimeConsoleApiCalled {
   runtimeConsoleApiCalledType :: RuntimeConsoleApiCalledType, -- ^ Type of the call.
   runtimeConsoleApiCalledArgs :: RuntimeConsoleApiCalledArgs, -- ^ Call arguments.
   runtimeConsoleApiCalledExecutionContextId :: RuntimeConsoleApiCalledExecutionContextId, -- ^ Identifier of the context where the call was made.
   runtimeConsoleApiCalledTimestamp :: RuntimeConsoleApiCalledTimestamp, -- ^ Call timestamp.
   runtimeConsoleApiCalledStackTrace :: RuntimeConsoleApiCalledStackTrace, -- ^ Stack trace captured when the call was made. The async stack chain is automatically reported for
the following call types: `assert`, `error`, `trace`, `warning`. For other types the async call
chain can be retrieved using `Debugger.getStackTrace` and `stackTrace.parentId` field.
   runtimeConsoleApiCalledContext :: RuntimeConsoleApiCalledContext -- ^ Console context descriptor for calls on non-default console context (not console.*):
'anonymous#unique-logger-id' for call on unnamed context, 'name#unique-logger-id' for call
on named context.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeConsoleApiCalled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeConsoleApiCalled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Runtime.exceptionRevoked' event.
data RuntimeExceptionRevoked = RuntimeExceptionRevoked {
   runtimeExceptionRevokedReason :: RuntimeExceptionRevokedReason, -- ^ Reason describing why exception was revoked.
   runtimeExceptionRevokedExceptionId :: RuntimeExceptionRevokedExceptionId -- ^ The id of revoked exception, as reported in `exceptionThrown`.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionRevoked  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionRevoked where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Runtime.exceptionThrown' event.
data RuntimeExceptionThrown = RuntimeExceptionThrown {
   runtimeExceptionThrownTimestamp :: RuntimeExceptionThrownTimestamp, -- ^ Timestamp of the exception.

} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExceptionThrown  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  RuntimeExceptionThrown where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Runtime.executionContextCreated' event.
data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated {
   runtimeExecutionContextCreatedContext :: RuntimeExecutionContextCreatedContext -- ^ A newly created execution context.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type of the 'Runtime.executionContextDestroyed' event.
data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed {
   runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextDestroyedExecutionContextId -- ^ Id of the destroyed context
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeExecutionContextDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  RuntimeExecutionContextDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type of the 'Runtime.executionContextsCleared' event.
data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
   deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
   parseJSON = A.withText  "RuntimeExecutionContextsCleared"  $ \v -> do
      case v of
         "RuntimeExecutionContextsCleared" -> pure RuntimeExecutionContextsCleared
         _ -> fail "failed to parse RuntimeExecutionContextsCleared"



-- | Type of the 'Runtime.inspectRequested' event.
data RuntimeInspectRequested = RuntimeInspectRequested {


   runtimeInspectRequestedExecutionContextId :: RuntimeInspectRequestedExecutionContextId -- ^ Identifier of the context where the call was made.
} deriving (Generic, Eq, Show, Read)
instance ToJSON RuntimeInspectRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  RuntimeInspectRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





-- | Parameters of the 'runtimeAwaitPromise' command.
data PRuntimeAwaitPromise = PRuntimeAwaitPromise {
   pRuntimeAwaitPromisePromiseObjectId :: PRuntimeAwaitPromisePromiseObjectId, -- ^ Identifier of the promise.
   pRuntimeAwaitPromiseReturnByValue :: PRuntimeAwaitPromiseReturnByValue, -- ^ Whether the result is expected to be a JSON object that should be sent by value.
   pRuntimeAwaitPromiseGeneratePreview :: PRuntimeAwaitPromiseGeneratePreview -- ^ Whether preview should be generated for the result.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeAwaitPromise  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Runtime.awaitPromise'.
-- Add handler to promise with given promise object id.
-- Parameters: 'PRuntimeAwaitPromise'
-- Returns: 'RuntimeAwaitPromise'
runtimeAwaitPromise :: Handle ev -> PRuntimeAwaitPromise -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise handle params = sendReceiveCommandResult handle "Runtime.awaitPromise" (Just params)

-- | Return type of the 'runtimeAwaitPromise' command.
data RuntimeAwaitPromise = RuntimeAwaitPromise {
   runtimeAwaitPromiseResult :: RuntimeRemoteObject, -- ^ Promise result. Will contain rejected value if promise was rejected.
   runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details if stack strace is available.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeAwaitPromise where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeAwaitPromise where
   commandName _ = "Runtime.awaitPromise"



-- | Parameters of the 'runtimeCallFunctionOn' command.
data PRuntimeCallFunctionOn = PRuntimeCallFunctionOn {
   pRuntimeCallFunctionOnFunctionDeclaration :: PRuntimeCallFunctionOnFunctionDeclaration, -- ^ Declaration of the function to call.
   pRuntimeCallFunctionOnObjectId :: PRuntimeCallFunctionOnObjectId, -- ^ Identifier of the object to call function on. Either objectId or executionContextId should
be specified.
   pRuntimeCallFunctionOnArguments :: PRuntimeCallFunctionOnArguments, -- ^ Call arguments. All call arguments must belong to the same JavaScript world as the target
object.
   pRuntimeCallFunctionOnSilent :: PRuntimeCallFunctionOnSilent, -- ^ In silent mode exceptions thrown during evaluation are not reported and do not pause
execution. Overrides `setPauseOnException` state.
   pRuntimeCallFunctionOnReturnByValue :: PRuntimeCallFunctionOnReturnByValue, -- ^ Whether the result is expected to be a JSON object which should be sent by value.
   pRuntimeCallFunctionOnGeneratePreview :: PRuntimeCallFunctionOnGeneratePreview, -- ^ Whether preview should be generated for the result.
   pRuntimeCallFunctionOnUserGesture :: PRuntimeCallFunctionOnUserGesture, -- ^ Whether execution should be treated as initiated by user in the UI.
   pRuntimeCallFunctionOnAwaitPromise :: PRuntimeCallFunctionOnAwaitPromise, -- ^ Whether execution should `await` for resulting value and return once awaited promise is
resolved.
   pRuntimeCallFunctionOnExecutionContextId :: PRuntimeCallFunctionOnExecutionContextId, -- ^ Specifies execution context which global object will be used to call function on. Either
executionContextId or objectId should be specified.
   pRuntimeCallFunctionOnObjectGroup :: PRuntimeCallFunctionOnObjectGroup, -- ^ Symbolic group name that can be used to release multiple objects. If objectGroup is not
specified and objectId is, objectGroup will be inherited from object.
   pRuntimeCallFunctionOnThrowOnSideEffect :: PRuntimeCallFunctionOnThrowOnSideEffect, -- ^ Whether to throw an exception if side effect cannot be ruled out during evaluation.
   pRuntimeCallFunctionOnGenerateWebDriverValue :: PRuntimeCallFunctionOnGenerateWebDriverValue -- ^ Whether the result should contain `webDriverValue`, serialized according to
https://w3c.github.io/webdriver-bidi. This is mutually exclusive with `returnByValue`, but
resulting `objectId` is still provided.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCallFunctionOn  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Runtime.callFunctionOn'.
-- Calls function with given declaration on the given object. Object group of the result is
-- inherited from the target object.
-- Parameters: 'PRuntimeCallFunctionOn'
-- Returns: 'RuntimeCallFunctionOn'
runtimeCallFunctionOn :: Handle ev -> PRuntimeCallFunctionOn -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn handle params = sendReceiveCommandResult handle "Runtime.callFunctionOn" (Just params)

-- | Return type of the 'runtimeCallFunctionOn' command.
data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
   runtimeCallFunctionOnResult :: RuntimeRemoteObject, -- ^ Call result.
   runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCallFunctionOn where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command RuntimeCallFunctionOn where
   commandName _ = "Runtime.callFunctionOn"



-- | Parameters of the 'runtimeCompileScript' command.
data PRuntimeCompileScript = PRuntimeCompileScript {
   pRuntimeCompileScriptExpression :: PRuntimeCompileScriptExpression, -- ^ Expression to compile.
   pRuntimeCompileScriptSourceUrl :: PRuntimeCompileScriptSourceUrl, -- ^ Source url to be set for the script.
   pRuntimeCompileScriptPersistScript :: PRuntimeCompileScriptPersistScript, -- ^ Specifies whether the compiled script should be persisted.
   pRuntimeCompileScriptExecutionContextId :: PRuntimeCompileScriptExecutionContextId -- ^ Specifies in which execution context to perform script run. If the parameter is omitted the
evaluation will be performed in the context of the inspected page.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeCompileScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Runtime.compileScript'.
-- Compiles expression.
-- Parameters: 'PRuntimeCompileScript'
-- Returns: 'RuntimeCompileScript'
runtimeCompileScript :: Handle ev -> PRuntimeCompileScript -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript handle params = sendReceiveCommandResult handle "Runtime.compileScript" (Just params)

-- | Return type of the 'runtimeCompileScript' command.
data RuntimeCompileScript = RuntimeCompileScript {
   runtimeCompileScriptScriptId :: Maybe RuntimeScriptId, -- ^ Id of the script.
   runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeCompileScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeCompileScript where
   commandName _ = "Runtime.compileScript"



-- | Function for the command 'Runtime.disable'.
-- Disables reporting of execution contexts creation.
runtimeDisable :: Handle ev -> IO (Maybe Error)
runtimeDisable handle = sendReceiveCommand handle "Runtime.disable" (Nothing :: Maybe ())


-- | Function for the command 'Runtime.discardConsoleEntries'.
-- Discards collected exceptions and console API calls.
runtimeDiscardConsoleEntries :: Handle ev -> IO (Maybe Error)
runtimeDiscardConsoleEntries handle = sendReceiveCommand handle "Runtime.discardConsoleEntries" (Nothing :: Maybe ())


-- | Function for the command 'Runtime.enable'.
-- Enables reporting of execution contexts creation by means of `executionContextCreated` event.
-- When the reporting gets enabled the event will be sent immediately for each existing execution
-- context.
runtimeEnable :: Handle ev -> IO (Maybe Error)
runtimeEnable handle = sendReceiveCommand handle "Runtime.enable" (Nothing :: Maybe ())


-- | Parameters of the 'runtimeEvaluate' command.
data PRuntimeEvaluate = PRuntimeEvaluate {
   pRuntimeEvaluateExpression :: PRuntimeEvaluateExpression, -- ^ Expression to evaluate.
   pRuntimeEvaluateObjectGroup :: PRuntimeEvaluateObjectGroup, -- ^ Symbolic group name that can be used to release multiple objects.
   pRuntimeEvaluateIncludeCommandLineApi :: PRuntimeEvaluateIncludeCommandLineApi, -- ^ Determines whether Command Line API should be available during the evaluation.
   pRuntimeEvaluateSilent :: PRuntimeEvaluateSilent, -- ^ In silent mode exceptions thrown during evaluation are not reported and do not pause
execution. Overrides `setPauseOnException` state.
   pRuntimeEvaluateContextId :: PRuntimeEvaluateContextId, -- ^ Specifies in which execution context to perform evaluation. If the parameter is omitted the
evaluation will be performed in the context of the inspected page.
This is mutually exclusive with `uniqueContextId`, which offers an
alternative way to identify the execution context that is more reliable
in a multi-process environment.
   pRuntimeEvaluateReturnByValue :: PRuntimeEvaluateReturnByValue, -- ^ Whether the result is expected to be a JSON object that should be sent by value.
   pRuntimeEvaluateGeneratePreview :: PRuntimeEvaluateGeneratePreview, -- ^ Whether preview should be generated for the result.
   pRuntimeEvaluateUserGesture :: PRuntimeEvaluateUserGesture, -- ^ Whether execution should be treated as initiated by user in the UI.
   pRuntimeEvaluateAwaitPromise :: PRuntimeEvaluateAwaitPromise, -- ^ Whether execution should `await` for resulting value and return once awaited promise is
resolved.
   pRuntimeEvaluateThrowOnSideEffect :: PRuntimeEvaluateThrowOnSideEffect, -- ^ Whether to throw an exception if side effect cannot be ruled out during evaluation.
This implies `disableBreaks` below.
   pRuntimeEvaluateTimeout :: PRuntimeEvaluateTimeout, -- ^ Terminate execution after timing out (number of milliseconds).
   pRuntimeEvaluateDisableBreaks :: PRuntimeEvaluateDisableBreaks, -- ^ Disable breakpoints during execution.
   pRuntimeEvaluateReplMode :: PRuntimeEvaluateReplMode, -- ^ Setting this flag to true enables `let` re-declaration and top-level `await`.
Note that `let` variables can only be re-declared if they originate from
`replMode` themselves.
   pRuntimeEvaluateAllowUnsafeEvalBlockedByCsp :: PRuntimeEvaluateAllowUnsafeEvalBlockedByCsp, -- ^ The Content Security Policy (CSP) for the target might block 'unsafe-eval'
which includes eval(), Function(), setTimeout() and setInterval()
when called with non-callable arguments. This flag bypasses CSP for this
evaluation and allows unsafe-eval. Defaults to true.
   pRuntimeEvaluateUniqueContextId :: PRuntimeEvaluateUniqueContextId, -- ^ An alternative way to specify the execution context to evaluate in.
Compared to contextId that may be reused across processes, this is guaranteed to be
system-unique, so it can be used to prevent accidental evaluation of the expression
in context different than intended (e.g. as a result of navigation across process
boundaries).
This is mutually exclusive with `contextId`.
   pRuntimeEvaluateGenerateWebDriverValue :: PRuntimeEvaluateGenerateWebDriverValue -- ^ Whether the result should be serialized according to https://w3c.github.io/webdriver-bidi.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeEvaluate  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PRuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the command 'Runtime.evaluate'.
-- Evaluates expression on global object.
-- Parameters: 'PRuntimeEvaluate'
-- Returns: 'RuntimeEvaluate'
runtimeEvaluate :: Handle ev -> PRuntimeEvaluate -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate handle params = sendReceiveCommandResult handle "Runtime.evaluate" (Just params)

-- | Return type of the 'runtimeEvaluate' command.
data RuntimeEvaluate = RuntimeEvaluate {
   runtimeEvaluateResult :: RuntimeRemoteObject, -- ^ Evaluation result.
   runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeEvaluate where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command RuntimeEvaluate where
   commandName _ = "Runtime.evaluate"



-- | Function for the command 'Runtime.getIsolateId'.
-- Returns the isolate id.
-- Returns: 'RuntimeGetIsolateId'
runtimeGetIsolateId :: Handle ev -> IO (Either Error RuntimeGetIsolateId)
runtimeGetIsolateId handle = sendReceiveCommandResult handle "Runtime.getIsolateId" (Nothing :: Maybe ())

-- | Return type of the 'runtimeGetIsolateId' command.
data RuntimeGetIsolateId = RuntimeGetIsolateId {
   runtimeGetIsolateIdId :: String -- ^ The isolate id.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetIsolateId where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeGetIsolateId where
   commandName _ = "Runtime.getIsolateId"



-- | Function for the command 'Runtime.getHeapUsage'.
-- Returns the JavaScript heap usage.
-- It is the total usage of the corresponding isolate not scoped to a particular Runtime.
-- Returns: 'RuntimeGetHeapUsage'
runtimeGetHeapUsage :: Handle ev -> IO (Either Error RuntimeGetHeapUsage)
runtimeGetHeapUsage handle = sendReceiveCommandResult handle "Runtime.getHeapUsage" (Nothing :: Maybe ())

-- | Return type of the 'runtimeGetHeapUsage' command.
data RuntimeGetHeapUsage = RuntimeGetHeapUsage {
   runtimeGetHeapUsageUsedSize :: Double, -- ^ Used heap size in bytes.
   runtimeGetHeapUsageTotalSize :: Double -- ^ Allocated heap size in bytes.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetHeapUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeGetHeapUsage where
   commandName _ = "Runtime.getHeapUsage"



-- | Parameters of the 'runtimeGetProperties' command.
data PRuntimeGetProperties = PRuntimeGetProperties {
   pRuntimeGetPropertiesObjectId :: PRuntimeGetPropertiesObjectId, -- ^ Identifier of the object to return properties for.
   pRuntimeGetPropertiesOwnProperties :: PRuntimeGetPropertiesOwnProperties, -- ^ If true, returns properties belonging only to the element itself, not to its prototype
chain.
   pRuntimeGetPropertiesAccessorPropertiesOnly :: PRuntimeGetPropertiesAccessorPropertiesOnly, -- ^ If true, returns accessor properties (with getter/setter) only; internal properties are not
returned either.
   pRuntimeGetPropertiesGeneratePreview :: PRuntimeGetPropertiesGeneratePreview, -- ^ Whether preview should be generated for the results.
   pRuntimeGetPropertiesNonIndexedPropertiesOnly :: PRuntimeGetPropertiesNonIndexedPropertiesOnly -- ^ If true, returns non-indexed properties only.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGetProperties  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Runtime.getProperties'.
-- Returns properties of a given object. Object group of the result is inherited from the target
-- object.
-- Parameters: 'PRuntimeGetProperties'
-- Returns: 'RuntimeGetProperties'
runtimeGetProperties :: Handle ev -> PRuntimeGetProperties -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties handle params = sendReceiveCommandResult handle "Runtime.getProperties" (Just params)

-- | Return type of the 'runtimeGetProperties' command.
data RuntimeGetProperties = RuntimeGetProperties {
   runtimeGetPropertiesResult :: [RuntimePropertyDescriptor], -- ^ Object properties.
   runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor], -- ^ Internal object properties (only of the element itself).
   runtimeGetPropertiesPrivateProperties :: Maybe [RuntimePrivatePropertyDescriptor], -- ^ Object private properties.
   runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetProperties where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command RuntimeGetProperties where
   commandName _ = "Runtime.getProperties"



-- | Parameters of the 'runtimeGlobalLexicalScopeNames' command.
data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames {
   pRuntimeGlobalLexicalScopeNamesExecutionContextId :: PRuntimeGlobalLexicalScopeNamesExecutionContextId -- ^ Specifies in which execution context to lookup global scope variables.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGlobalLexicalScopeNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Runtime.globalLexicalScopeNames'.
-- Returns all let, const and class variables from global scope.
-- Parameters: 'PRuntimeGlobalLexicalScopeNames'
-- Returns: 'RuntimeGlobalLexicalScopeNames'
runtimeGlobalLexicalScopeNames :: Handle ev -> PRuntimeGlobalLexicalScopeNames -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames handle params = sendReceiveCommandResult handle "Runtime.globalLexicalScopeNames" (Just params)

-- | Return type of the 'runtimeGlobalLexicalScopeNames' command.
data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGlobalLexicalScopeNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }

instance Command RuntimeGlobalLexicalScopeNames where
   commandName _ = "Runtime.globalLexicalScopeNames"



-- | Parameters of the 'runtimeQueryObjects' command.
data PRuntimeQueryObjects = PRuntimeQueryObjects {
   pRuntimeQueryObjectsPrototypeObjectId :: PRuntimeQueryObjectsPrototypeObjectId, -- ^ Identifier of the prototype to return objects for.
   pRuntimeQueryObjectsObjectGroup :: PRuntimeQueryObjectsObjectGroup -- ^ Symbolic group name that can be used to release the results.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeQueryObjects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PRuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Runtime.queryObjects'.
-- Parameters: 'PRuntimeQueryObjects'
-- Returns: 'RuntimeQueryObjects'
runtimeQueryObjects :: Handle ev -> PRuntimeQueryObjects -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects handle params = sendReceiveCommandResult handle "Runtime.queryObjects" (Just params)

-- | Return type of the 'runtimeQueryObjects' command.
data RuntimeQueryObjects = RuntimeQueryObjects {
   runtimeQueryObjectsObjects :: RuntimeRemoteObject -- ^ Array with objects.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeQueryObjects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command RuntimeQueryObjects where
   commandName _ = "Runtime.queryObjects"



-- | Parameters of the 'runtimeReleaseObject' command.
data PRuntimeReleaseObject = PRuntimeReleaseObject {
   pRuntimeReleaseObjectObjectId :: PRuntimeReleaseObjectObjectId -- ^ Identifier of the object to release.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObject  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObject where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Runtime.releaseObject'.
-- Releases remote object with given id.
-- Parameters: 'PRuntimeReleaseObject'
runtimeReleaseObject :: Handle ev -> PRuntimeReleaseObject -> IO (Maybe Error)
runtimeReleaseObject handle params = sendReceiveCommand handle "Runtime.releaseObject" (Just params)


-- | Parameters of the 'runtimeReleaseObjectGroup' command.
data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup {
   pRuntimeReleaseObjectGroupObjectGroup :: PRuntimeReleaseObjectGroupObjectGroup -- ^ Symbolic object group name.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeReleaseObjectGroup  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PRuntimeReleaseObjectGroup where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Runtime.releaseObjectGroup'.
-- Releases all remote objects that belong to a given group.
-- Parameters: 'PRuntimeReleaseObjectGroup'
runtimeReleaseObjectGroup :: Handle ev -> PRuntimeReleaseObjectGroup -> IO (Maybe Error)
runtimeReleaseObjectGroup handle params = sendReceiveCommand handle "Runtime.releaseObjectGroup" (Just params)


-- | Function for the command 'Runtime.runIfWaitingForDebugger'.
-- Tells inspected instance to run if it was waiting for debugger to attach.
runtimeRunIfWaitingForDebugger :: Handle ev -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger handle = sendReceiveCommand handle "Runtime.runIfWaitingForDebugger" (Nothing :: Maybe ())


-- | Parameters of the 'runtimeRunScript' command.
data PRuntimeRunScript = PRuntimeRunScript {
   pRuntimeRunScriptScriptId :: PRuntimeRunScriptScriptId, -- ^ Id of the script to run.
   pRuntimeRunScriptExecutionContextId :: PRuntimeRunScriptExecutionContextId, -- ^ Specifies in which execution context to perform script run. If the parameter is omitted the
evaluation will be performed in the context of the inspected page.
   pRuntimeRunScriptObjectGroup :: PRuntimeRunScriptObjectGroup, -- ^ Symbolic group name that can be used to release multiple objects.
   pRuntimeRunScriptSilent :: PRuntimeRunScriptSilent, -- ^ In silent mode exceptions thrown during evaluation are not reported and do not pause
execution. Overrides `setPauseOnException` state.
   pRuntimeRunScriptIncludeCommandLineApi :: PRuntimeRunScriptIncludeCommandLineApi, -- ^ Determines whether Command Line API should be available during the evaluation.
   pRuntimeRunScriptReturnByValue :: PRuntimeRunScriptReturnByValue, -- ^ Whether the result is expected to be a JSON object which should be sent by value.
   pRuntimeRunScriptGeneratePreview :: PRuntimeRunScriptGeneratePreview, -- ^ Whether preview should be generated for the result.
   pRuntimeRunScriptAwaitPromise :: PRuntimeRunScriptAwaitPromise -- ^ Whether execution should `await` for resulting value and return once awaited promise is
resolved.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeRunScript  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PRuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the command 'Runtime.runScript'.
-- Runs script with given id in a given context.
-- Parameters: 'PRuntimeRunScript'
-- Returns: 'RuntimeRunScript'
runtimeRunScript :: Handle ev -> PRuntimeRunScript -> IO (Either Error RuntimeRunScript)
runtimeRunScript handle params = sendReceiveCommandResult handle "Runtime.runScript" (Just params)

-- | Return type of the 'runtimeRunScript' command.
data RuntimeRunScript = RuntimeRunScript {
   runtimeRunScriptResult :: RuntimeRemoteObject, -- ^ Run result.
   runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails -- ^ Exception details.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeRunScript where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command RuntimeRunScript where
   commandName _ = "Runtime.runScript"



-- | Parameters of the 'runtimeSetAsyncCallStackDepth' command.
data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth {
   pRuntimeSetAsyncCallStackDepthMaxDepth :: PRuntimeSetAsyncCallStackDepthMaxDepth -- ^ Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
call stacks (default).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeSetAsyncCallStackDepth  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  PRuntimeSetAsyncCallStackDepth where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


-- | Function for the command 'Runtime.setAsyncCallStackDepth'.
-- Enables or disables async call stacks tracking.
-- Parameters: 'PRuntimeSetAsyncCallStackDepth'
runtimeSetAsyncCallStackDepth :: Handle ev -> PRuntimeSetAsyncCallStackDepth -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth handle params = sendReceiveCommand handle "Runtime.setAsyncCallStackDepth" (Just params)


-- | Parameters of the 'runtimeSetCustomObjectFormatterEnabled' command.
data PRuntimeSetCustomObjectFormatterEnabled = PRuntimeSetCustomObjectFormatterEnabled {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeSetCustomObjectFormatterEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PRuntimeSetCustomObjectFormatterEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


-- | Function for the command 'Runtime.setCustomObjectFormatterEnabled'.
-- Parameters: 'PRuntimeSetCustomObjectFormatterEnabled'
runtimeSetCustomObjectFormatterEnabled :: Handle ev -> PRuntimeSetCustomObjectFormatterEnabled -> IO (Maybe Error)
runtimeSetCustomObjectFormatterEnabled handle params = sendReceiveCommand handle "Runtime.setCustomObjectFormatterEnabled" (Just params)


-- | Parameters of the 'runtimeSetMaxCallStackSizeToCapture' command.
data PRuntimeSetMaxCallStackSizeToCapture = PRuntimeSetMaxCallStackSizeToCapture {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeSetMaxCallStackSizeToCapture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PRuntimeSetMaxCallStackSizeToCapture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the command 'Runtime.setMaxCallStackSizeToCapture'.
-- Parameters: 'PRuntimeSetMaxCallStackSizeToCapture'
runtimeSetMaxCallStackSizeToCapture :: Handle ev -> PRuntimeSetMaxCallStackSizeToCapture -> IO (Maybe Error)
runtimeSetMaxCallStackSizeToCapture handle params = sendReceiveCommand handle "Runtime.setMaxCallStackSizeToCapture" (Just params)


-- | Function for the command 'Runtime.terminateExecution'.
-- Terminate current or next JavaScript execution.
-- Will cancel the termination when the outer-most script execution ends.
runtimeTerminateExecution :: Handle ev -> IO (Maybe Error)
runtimeTerminateExecution handle = sendReceiveCommand handle "Runtime.terminateExecution" (Nothing :: Maybe ())


-- | Parameters of the 'runtimeAddBinding' command.
data PRuntimeAddBinding = PRuntimeAddBinding {

   pRuntimeAddBindingExecutionContextName :: PRuntimeAddBindingExecutionContextName -- ^ If specified, the binding is exposed to the executionContext with
matching name, even for contexts created after the binding is added.
See also `ExecutionContext.name` and `worldName` parameter to
`Page.addScriptToEvaluateOnNewDocument`.
This parameter is mutually exclusive with `executionContextId`.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeAddBinding  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PRuntimeAddBinding where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the command 'Runtime.addBinding'.
-- If executionContextId is empty, adds binding with the given name on the
-- global objects of all inspected contexts, including those created later,
-- bindings survive reloads.
-- Binding function takes exactly one argument, this argument should be string,
-- in case of any other input, function throws an exception.
-- Each binding function call produces Runtime.bindingCalled notification.
-- Parameters: 'PRuntimeAddBinding'
runtimeAddBinding :: Handle ev -> PRuntimeAddBinding -> IO (Maybe Error)
runtimeAddBinding handle params = sendReceiveCommand handle "Runtime.addBinding" (Just params)


-- | Parameters of the 'runtimeRemoveBinding' command.
data PRuntimeRemoveBinding = PRuntimeRemoveBinding {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeRemoveBinding  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PRuntimeRemoveBinding where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Runtime.removeBinding'.
-- This method does not remove binding function from global object but
-- unsubscribes current runtime agent from Runtime.bindingCalled notifications.
-- Parameters: 'PRuntimeRemoveBinding'
runtimeRemoveBinding :: Handle ev -> PRuntimeRemoveBinding -> IO (Maybe Error)
runtimeRemoveBinding handle params = sendReceiveCommand handle "Runtime.removeBinding" (Just params)


-- | Parameters of the 'runtimeGetExceptionDetails' command.
data PRuntimeGetExceptionDetails = PRuntimeGetExceptionDetails {
   pRuntimeGetExceptionDetailsErrorObjectId :: PRuntimeGetExceptionDetailsErrorObjectId -- ^ The error object for which to resolve the exception details.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PRuntimeGetExceptionDetails  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PRuntimeGetExceptionDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Runtime.getExceptionDetails'.
-- This method tries to lookup and populate exception details for a
-- JavaScript Error object.
-- Note that the stackTrace portion of the resulting exceptionDetails will
-- only be populated if the Runtime domain was enabled at the time when the
-- Error was thrown.
-- Parameters: 'PRuntimeGetExceptionDetails'
-- Returns: 'RuntimeGetExceptionDetails'
runtimeGetExceptionDetails :: Handle ev -> PRuntimeGetExceptionDetails -> IO (Either Error RuntimeGetExceptionDetails)
runtimeGetExceptionDetails handle params = sendReceiveCommandResult handle "Runtime.getExceptionDetails" (Just params)

-- | Return type of the 'runtimeGetExceptionDetails' command.
data RuntimeGetExceptionDetails = RuntimeGetExceptionDetails {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  RuntimeGetExceptionDetails where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command RuntimeGetExceptionDetails where
   commandName _ = "Runtime.getExceptionDetails"




