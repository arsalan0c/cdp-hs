{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Runtime

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'Runtime.ScriptId'.
--   Unique script identifier.
type RuntimeScriptId = T.Text

-- | Type 'Runtime.WebDriverValue'.
--   Represents the value serialiazed by the WebDriver BiDi specification
--   https://w3c.github.io/webdriver-bidi.
data RuntimeWebDriverValueType = RuntimeWebDriverValueTypeUndefined | RuntimeWebDriverValueTypeNull | RuntimeWebDriverValueTypeString | RuntimeWebDriverValueTypeNumber | RuntimeWebDriverValueTypeBoolean | RuntimeWebDriverValueTypeBigint | RuntimeWebDriverValueTypeRegexp | RuntimeWebDriverValueTypeDate | RuntimeWebDriverValueTypeSymbol | RuntimeWebDriverValueTypeArray | RuntimeWebDriverValueTypeObject | RuntimeWebDriverValueTypeFunction | RuntimeWebDriverValueTypeMap | RuntimeWebDriverValueTypeSet | RuntimeWebDriverValueTypeWeakmap | RuntimeWebDriverValueTypeWeakset | RuntimeWebDriverValueTypeError | RuntimeWebDriverValueTypeProxy | RuntimeWebDriverValueTypePromise | RuntimeWebDriverValueTypeTypedarray | RuntimeWebDriverValueTypeArraybuffer | RuntimeWebDriverValueTypeNode | RuntimeWebDriverValueTypeWindow
  deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeWebDriverValueType where
  parseJSON = A.withText "RuntimeWebDriverValueType" $ \v -> case v of
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
    "_" -> fail "failed to parse RuntimeWebDriverValueType"
instance ToJSON RuntimeWebDriverValueType where
  toJSON v = A.String $ case v of
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
data RuntimeWebDriverValue = RuntimeWebDriverValue
  {
    runtimeWebDriverValueType :: RuntimeWebDriverValueType,
    runtimeWebDriverValueValue :: Maybe A.Value,
    runtimeWebDriverValueObjectId :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON RuntimeWebDriverValue where
  parseJSON = A.withObject "RuntimeWebDriverValue" $ \o -> RuntimeWebDriverValue
    <$> o A..: "type"
    <*> o A..:? "value"
    <*> o A..:? "objectId"
instance ToJSON RuntimeWebDriverValue where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (runtimeWebDriverValueType p),
    ("value" A..=) <$> (runtimeWebDriverValueValue p),
    ("objectId" A..=) <$> (runtimeWebDriverValueObjectId p)
    ]

-- | Type 'Runtime.RemoteObjectId'.
--   Unique object identifier.
type RuntimeRemoteObjectId = T.Text

-- | Type 'Runtime.UnserializableValue'.
--   Primitive value which cannot be JSON-stringified. Includes values `-0`, `NaN`, `Infinity`,
--   `-Infinity`, and bigint literals.
type RuntimeUnserializableValue = T.Text

-- | Type 'Runtime.RemoteObject'.
--   Mirror object referencing original JavaScript object.
data RuntimeRemoteObjectType = RuntimeRemoteObjectTypeObject | RuntimeRemoteObjectTypeFunction | RuntimeRemoteObjectTypeUndefined | RuntimeRemoteObjectTypeString | RuntimeRemoteObjectTypeNumber | RuntimeRemoteObjectTypeBoolean | RuntimeRemoteObjectTypeSymbol | RuntimeRemoteObjectTypeBigint
  deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeRemoteObjectType where
  parseJSON = A.withText "RuntimeRemoteObjectType" $ \v -> case v of
    "object" -> pure RuntimeRemoteObjectTypeObject
    "function" -> pure RuntimeRemoteObjectTypeFunction
    "undefined" -> pure RuntimeRemoteObjectTypeUndefined
    "string" -> pure RuntimeRemoteObjectTypeString
    "number" -> pure RuntimeRemoteObjectTypeNumber
    "boolean" -> pure RuntimeRemoteObjectTypeBoolean
    "symbol" -> pure RuntimeRemoteObjectTypeSymbol
    "bigint" -> pure RuntimeRemoteObjectTypeBigint
    "_" -> fail "failed to parse RuntimeRemoteObjectType"
instance ToJSON RuntimeRemoteObjectType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "RuntimeRemoteObjectSubtype" $ \v -> case v of
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
    "_" -> fail "failed to parse RuntimeRemoteObjectSubtype"
instance ToJSON RuntimeRemoteObjectSubtype where
  toJSON v = A.String $ case v of
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
data RuntimeRemoteObject = RuntimeRemoteObject
  {
    -- | Object type.
    runtimeRemoteObjectType :: RuntimeRemoteObjectType,
    -- | Object subtype hint. Specified for `object` type values only.
    --   NOTE: If you change anything here, make sure to also update
    --   `subtype` in `ObjectPreview` and `PropertyPreview` below.
    runtimeRemoteObjectSubtype :: Maybe RuntimeRemoteObjectSubtype,
    -- | Object class (constructor) name. Specified for `object` type values only.
    runtimeRemoteObjectClassName :: Maybe T.Text,
    -- | Remote object value in case of primitive values or JSON values (if it was requested).
    runtimeRemoteObjectValue :: Maybe A.Value,
    -- | Primitive value which can not be JSON-stringified does not have `value`, but gets this
    --   property.
    runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
    -- | String representation of the object.
    runtimeRemoteObjectDescription :: Maybe T.Text,
    -- | WebDriver BiDi representation of the value.
    runtimeRemoteObjectWebDriverValue :: Maybe RuntimeWebDriverValue,
    -- | Unique object identifier (for non-primitive values).
    runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId,
    -- | Preview containing abbreviated property values. Specified for `object` type values only.
    runtimeRemoteObjectPreview :: Maybe RuntimeObjectPreview,
    runtimeRemoteObjectCustomPreview :: Maybe RuntimeCustomPreview
  }
  deriving (Eq, Show)
instance FromJSON RuntimeRemoteObject where
  parseJSON = A.withObject "RuntimeRemoteObject" $ \o -> RuntimeRemoteObject
    <$> o A..: "type"
    <*> o A..:? "subtype"
    <*> o A..:? "className"
    <*> o A..:? "value"
    <*> o A..:? "unserializableValue"
    <*> o A..:? "description"
    <*> o A..:? "webDriverValue"
    <*> o A..:? "objectId"
    <*> o A..:? "preview"
    <*> o A..:? "customPreview"
instance ToJSON RuntimeRemoteObject where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (runtimeRemoteObjectType p),
    ("subtype" A..=) <$> (runtimeRemoteObjectSubtype p),
    ("className" A..=) <$> (runtimeRemoteObjectClassName p),
    ("value" A..=) <$> (runtimeRemoteObjectValue p),
    ("unserializableValue" A..=) <$> (runtimeRemoteObjectUnserializableValue p),
    ("description" A..=) <$> (runtimeRemoteObjectDescription p),
    ("webDriverValue" A..=) <$> (runtimeRemoteObjectWebDriverValue p),
    ("objectId" A..=) <$> (runtimeRemoteObjectObjectId p),
    ("preview" A..=) <$> (runtimeRemoteObjectPreview p),
    ("customPreview" A..=) <$> (runtimeRemoteObjectCustomPreview p)
    ]

-- | Type 'Runtime.CustomPreview'.
data RuntimeCustomPreview = RuntimeCustomPreview
  {
    -- | The JSON-stringified result of formatter.header(object, config) call.
    --   It contains json ML array that represents RemoteObject.
    runtimeCustomPreviewHeader :: T.Text,
    -- | If formatter returns true as a result of formatter.hasBody call then bodyGetterId will
    --   contain RemoteObjectId for the function that returns result of formatter.body(object, config) call.
    --   The result value is json ML array.
    runtimeCustomPreviewBodyGetterId :: Maybe RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeCustomPreview where
  parseJSON = A.withObject "RuntimeCustomPreview" $ \o -> RuntimeCustomPreview
    <$> o A..: "header"
    <*> o A..:? "bodyGetterId"
instance ToJSON RuntimeCustomPreview where
  toJSON p = A.object $ catMaybes [
    ("header" A..=) <$> Just (runtimeCustomPreviewHeader p),
    ("bodyGetterId" A..=) <$> (runtimeCustomPreviewBodyGetterId p)
    ]

-- | Type 'Runtime.ObjectPreview'.
--   Object containing abbreviated remote object value.
data RuntimeObjectPreviewType = RuntimeObjectPreviewTypeObject | RuntimeObjectPreviewTypeFunction | RuntimeObjectPreviewTypeUndefined | RuntimeObjectPreviewTypeString | RuntimeObjectPreviewTypeNumber | RuntimeObjectPreviewTypeBoolean | RuntimeObjectPreviewTypeSymbol | RuntimeObjectPreviewTypeBigint
  deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeObjectPreviewType where
  parseJSON = A.withText "RuntimeObjectPreviewType" $ \v -> case v of
    "object" -> pure RuntimeObjectPreviewTypeObject
    "function" -> pure RuntimeObjectPreviewTypeFunction
    "undefined" -> pure RuntimeObjectPreviewTypeUndefined
    "string" -> pure RuntimeObjectPreviewTypeString
    "number" -> pure RuntimeObjectPreviewTypeNumber
    "boolean" -> pure RuntimeObjectPreviewTypeBoolean
    "symbol" -> pure RuntimeObjectPreviewTypeSymbol
    "bigint" -> pure RuntimeObjectPreviewTypeBigint
    "_" -> fail "failed to parse RuntimeObjectPreviewType"
instance ToJSON RuntimeObjectPreviewType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "RuntimeObjectPreviewSubtype" $ \v -> case v of
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
    "_" -> fail "failed to parse RuntimeObjectPreviewSubtype"
instance ToJSON RuntimeObjectPreviewSubtype where
  toJSON v = A.String $ case v of
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
data RuntimeObjectPreview = RuntimeObjectPreview
  {
    -- | Object type.
    runtimeObjectPreviewType :: RuntimeObjectPreviewType,
    -- | Object subtype hint. Specified for `object` type values only.
    runtimeObjectPreviewSubtype :: Maybe RuntimeObjectPreviewSubtype,
    -- | String representation of the object.
    runtimeObjectPreviewDescription :: Maybe T.Text,
    -- | True iff some of the properties or entries of the original object did not fit.
    runtimeObjectPreviewOverflow :: Bool,
    -- | List of the properties.
    runtimeObjectPreviewProperties :: [RuntimePropertyPreview],
    -- | List of the entries. Specified for `map` and `set` subtype values only.
    runtimeObjectPreviewEntries :: Maybe [RuntimeEntryPreview]
  }
  deriving (Eq, Show)
instance FromJSON RuntimeObjectPreview where
  parseJSON = A.withObject "RuntimeObjectPreview" $ \o -> RuntimeObjectPreview
    <$> o A..: "type"
    <*> o A..:? "subtype"
    <*> o A..:? "description"
    <*> o A..: "overflow"
    <*> o A..: "properties"
    <*> o A..:? "entries"
instance ToJSON RuntimeObjectPreview where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (runtimeObjectPreviewType p),
    ("subtype" A..=) <$> (runtimeObjectPreviewSubtype p),
    ("description" A..=) <$> (runtimeObjectPreviewDescription p),
    ("overflow" A..=) <$> Just (runtimeObjectPreviewOverflow p),
    ("properties" A..=) <$> Just (runtimeObjectPreviewProperties p),
    ("entries" A..=) <$> (runtimeObjectPreviewEntries p)
    ]

-- | Type 'Runtime.PropertyPreview'.
data RuntimePropertyPreviewType = RuntimePropertyPreviewTypeObject | RuntimePropertyPreviewTypeFunction | RuntimePropertyPreviewTypeUndefined | RuntimePropertyPreviewTypeString | RuntimePropertyPreviewTypeNumber | RuntimePropertyPreviewTypeBoolean | RuntimePropertyPreviewTypeSymbol | RuntimePropertyPreviewTypeAccessor | RuntimePropertyPreviewTypeBigint
  deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimePropertyPreviewType where
  parseJSON = A.withText "RuntimePropertyPreviewType" $ \v -> case v of
    "object" -> pure RuntimePropertyPreviewTypeObject
    "function" -> pure RuntimePropertyPreviewTypeFunction
    "undefined" -> pure RuntimePropertyPreviewTypeUndefined
    "string" -> pure RuntimePropertyPreviewTypeString
    "number" -> pure RuntimePropertyPreviewTypeNumber
    "boolean" -> pure RuntimePropertyPreviewTypeBoolean
    "symbol" -> pure RuntimePropertyPreviewTypeSymbol
    "accessor" -> pure RuntimePropertyPreviewTypeAccessor
    "bigint" -> pure RuntimePropertyPreviewTypeBigint
    "_" -> fail "failed to parse RuntimePropertyPreviewType"
instance ToJSON RuntimePropertyPreviewType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "RuntimePropertyPreviewSubtype" $ \v -> case v of
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
    "_" -> fail "failed to parse RuntimePropertyPreviewSubtype"
instance ToJSON RuntimePropertyPreviewSubtype where
  toJSON v = A.String $ case v of
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
data RuntimePropertyPreview = RuntimePropertyPreview
  {
    -- | Property name.
    runtimePropertyPreviewName :: T.Text,
    -- | Object type. Accessor means that the property itself is an accessor property.
    runtimePropertyPreviewType :: RuntimePropertyPreviewType,
    -- | User-friendly property value string.
    runtimePropertyPreviewValue :: Maybe T.Text,
    -- | Nested value preview.
    runtimePropertyPreviewValuePreview :: Maybe RuntimeObjectPreview,
    -- | Object subtype hint. Specified for `object` type values only.
    runtimePropertyPreviewSubtype :: Maybe RuntimePropertyPreviewSubtype
  }
  deriving (Eq, Show)
instance FromJSON RuntimePropertyPreview where
  parseJSON = A.withObject "RuntimePropertyPreview" $ \o -> RuntimePropertyPreview
    <$> o A..: "name"
    <*> o A..: "type"
    <*> o A..:? "value"
    <*> o A..:? "valuePreview"
    <*> o A..:? "subtype"
instance ToJSON RuntimePropertyPreview where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (runtimePropertyPreviewName p),
    ("type" A..=) <$> Just (runtimePropertyPreviewType p),
    ("value" A..=) <$> (runtimePropertyPreviewValue p),
    ("valuePreview" A..=) <$> (runtimePropertyPreviewValuePreview p),
    ("subtype" A..=) <$> (runtimePropertyPreviewSubtype p)
    ]

-- | Type 'Runtime.EntryPreview'.
data RuntimeEntryPreview = RuntimeEntryPreview
  {
    -- | Preview of the key. Specified for map-like collection entries.
    runtimeEntryPreviewKey :: Maybe RuntimeObjectPreview,
    -- | Preview of the value.
    runtimeEntryPreviewValue :: RuntimeObjectPreview
  }
  deriving (Eq, Show)
instance FromJSON RuntimeEntryPreview where
  parseJSON = A.withObject "RuntimeEntryPreview" $ \o -> RuntimeEntryPreview
    <$> o A..:? "key"
    <*> o A..: "value"
instance ToJSON RuntimeEntryPreview where
  toJSON p = A.object $ catMaybes [
    ("key" A..=) <$> (runtimeEntryPreviewKey p),
    ("value" A..=) <$> Just (runtimeEntryPreviewValue p)
    ]

-- | Type 'Runtime.PropertyDescriptor'.
--   Object property descriptor.
data RuntimePropertyDescriptor = RuntimePropertyDescriptor
  {
    -- | Property name or symbol description.
    runtimePropertyDescriptorName :: T.Text,
    -- | The value associated with the property.
    runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
    -- | True if the value associated with the property may be changed (data descriptors only).
    runtimePropertyDescriptorWritable :: Maybe Bool,
    -- | A function which serves as a getter for the property, or `undefined` if there is no getter
    --   (accessor descriptors only).
    runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
    -- | A function which serves as a setter for the property, or `undefined` if there is no setter
    --   (accessor descriptors only).
    runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
    -- | True if the type of this property descriptor may be changed and if the property may be
    --   deleted from the corresponding object.
    runtimePropertyDescriptorConfigurable :: Bool,
    -- | True if this property shows up during enumeration of the properties on the corresponding
    --   object.
    runtimePropertyDescriptorEnumerable :: Bool,
    -- | True if the result was thrown during the evaluation.
    runtimePropertyDescriptorWasThrown :: Maybe Bool,
    -- | True if the property is owned for the object.
    runtimePropertyDescriptorIsOwn :: Maybe Bool,
    -- | Property symbol object, if the property is of the `symbol` type.
    runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON RuntimePropertyDescriptor where
  parseJSON = A.withObject "RuntimePropertyDescriptor" $ \o -> RuntimePropertyDescriptor
    <$> o A..: "name"
    <*> o A..:? "value"
    <*> o A..:? "writable"
    <*> o A..:? "get"
    <*> o A..:? "set"
    <*> o A..: "configurable"
    <*> o A..: "enumerable"
    <*> o A..:? "wasThrown"
    <*> o A..:? "isOwn"
    <*> o A..:? "symbol"
instance ToJSON RuntimePropertyDescriptor where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (runtimePropertyDescriptorName p),
    ("value" A..=) <$> (runtimePropertyDescriptorValue p),
    ("writable" A..=) <$> (runtimePropertyDescriptorWritable p),
    ("get" A..=) <$> (runtimePropertyDescriptorGet p),
    ("set" A..=) <$> (runtimePropertyDescriptorSet p),
    ("configurable" A..=) <$> Just (runtimePropertyDescriptorConfigurable p),
    ("enumerable" A..=) <$> Just (runtimePropertyDescriptorEnumerable p),
    ("wasThrown" A..=) <$> (runtimePropertyDescriptorWasThrown p),
    ("isOwn" A..=) <$> (runtimePropertyDescriptorIsOwn p),
    ("symbol" A..=) <$> (runtimePropertyDescriptorSymbol p)
    ]

-- | Type 'Runtime.InternalPropertyDescriptor'.
--   Object internal property descriptor. This property isn't normally visible in JavaScript code.
data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor
  {
    -- | Conventional property name.
    runtimeInternalPropertyDescriptorName :: T.Text,
    -- | The value associated with the property.
    runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON RuntimeInternalPropertyDescriptor where
  parseJSON = A.withObject "RuntimeInternalPropertyDescriptor" $ \o -> RuntimeInternalPropertyDescriptor
    <$> o A..: "name"
    <*> o A..:? "value"
instance ToJSON RuntimeInternalPropertyDescriptor where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (runtimeInternalPropertyDescriptorName p),
    ("value" A..=) <$> (runtimeInternalPropertyDescriptorValue p)
    ]

-- | Type 'Runtime.PrivatePropertyDescriptor'.
--   Object private field descriptor.
data RuntimePrivatePropertyDescriptor = RuntimePrivatePropertyDescriptor
  {
    -- | Private property name.
    runtimePrivatePropertyDescriptorName :: T.Text,
    -- | The value associated with the private property.
    runtimePrivatePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
    -- | A function which serves as a getter for the private property,
    --   or `undefined` if there is no getter (accessor descriptors only).
    runtimePrivatePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
    -- | A function which serves as a setter for the private property,
    --   or `undefined` if there is no setter (accessor descriptors only).
    runtimePrivatePropertyDescriptorSet :: Maybe RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON RuntimePrivatePropertyDescriptor where
  parseJSON = A.withObject "RuntimePrivatePropertyDescriptor" $ \o -> RuntimePrivatePropertyDescriptor
    <$> o A..: "name"
    <*> o A..:? "value"
    <*> o A..:? "get"
    <*> o A..:? "set"
instance ToJSON RuntimePrivatePropertyDescriptor where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (runtimePrivatePropertyDescriptorName p),
    ("value" A..=) <$> (runtimePrivatePropertyDescriptorValue p),
    ("get" A..=) <$> (runtimePrivatePropertyDescriptorGet p),
    ("set" A..=) <$> (runtimePrivatePropertyDescriptorSet p)
    ]

-- | Type 'Runtime.CallArgument'.
--   Represents function call argument. Either remote object id `objectId`, primitive `value`,
--   unserializable primitive value or neither of (for undefined) them should be specified.
data RuntimeCallArgument = RuntimeCallArgument
  {
    -- | Primitive value or serializable javascript object.
    runtimeCallArgumentValue :: Maybe A.Value,
    -- | Primitive value which can not be JSON-stringified.
    runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
    -- | Remote object handle.
    runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeCallArgument where
  parseJSON = A.withObject "RuntimeCallArgument" $ \o -> RuntimeCallArgument
    <$> o A..:? "value"
    <*> o A..:? "unserializableValue"
    <*> o A..:? "objectId"
instance ToJSON RuntimeCallArgument where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> (runtimeCallArgumentValue p),
    ("unserializableValue" A..=) <$> (runtimeCallArgumentUnserializableValue p),
    ("objectId" A..=) <$> (runtimeCallArgumentObjectId p)
    ]

-- | Type 'Runtime.ExecutionContextId'.
--   Id of an execution context.
type RuntimeExecutionContextId = Int

-- | Type 'Runtime.ExecutionContextDescription'.
--   Description of an isolated world.
data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription
  {
    -- | Unique id of the execution context. It can be used to specify in which execution context
    --   script evaluation should be performed.
    runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
    -- | Execution context origin.
    runtimeExecutionContextDescriptionOrigin :: T.Text,
    -- | Human readable name describing given context.
    runtimeExecutionContextDescriptionName :: T.Text,
    -- | A system-unique execution context identifier. Unlike the id, this is unique across
    --   multiple processes, so can be reliably used to identify specific context while backend
    --   performs a cross-process navigation.
    runtimeExecutionContextDescriptionUniqueId :: T.Text,
    -- | Embedder-specific auxiliary data.
    runtimeExecutionContextDescriptionAuxData :: Maybe [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExecutionContextDescription where
  parseJSON = A.withObject "RuntimeExecutionContextDescription" $ \o -> RuntimeExecutionContextDescription
    <$> o A..: "id"
    <*> o A..: "origin"
    <*> o A..: "name"
    <*> o A..: "uniqueId"
    <*> o A..:? "auxData"
instance ToJSON RuntimeExecutionContextDescription where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (runtimeExecutionContextDescriptionId p),
    ("origin" A..=) <$> Just (runtimeExecutionContextDescriptionOrigin p),
    ("name" A..=) <$> Just (runtimeExecutionContextDescriptionName p),
    ("uniqueId" A..=) <$> Just (runtimeExecutionContextDescriptionUniqueId p),
    ("auxData" A..=) <$> (runtimeExecutionContextDescriptionAuxData p)
    ]

-- | Type 'Runtime.ExceptionDetails'.
--   Detailed information about exception (or error) that was thrown during script compilation or
--   execution.
data RuntimeExceptionDetails = RuntimeExceptionDetails
  {
    -- | Exception id.
    runtimeExceptionDetailsExceptionId :: Int,
    -- | Exception text, which should be used together with exception object when available.
    runtimeExceptionDetailsText :: T.Text,
    -- | Line number of the exception location (0-based).
    runtimeExceptionDetailsLineNumber :: Int,
    -- | Column number of the exception location (0-based).
    runtimeExceptionDetailsColumnNumber :: Int,
    -- | Script ID of the exception location.
    runtimeExceptionDetailsScriptId :: Maybe RuntimeScriptId,
    -- | URL of the exception location, to be used when the script was not reported.
    runtimeExceptionDetailsUrl :: Maybe T.Text,
    -- | JavaScript stack trace if available.
    runtimeExceptionDetailsStackTrace :: Maybe RuntimeStackTrace,
    -- | Exception object if available.
    runtimeExceptionDetailsException :: Maybe RuntimeRemoteObject,
    -- | Identifier of the context where exception happened.
    runtimeExceptionDetailsExecutionContextId :: Maybe RuntimeExecutionContextId,
    -- | Dictionary with entries of meta data that the client associated
    --   with this exception, such as information about associated network
    --   requests, etc.
    runtimeExceptionDetailsExceptionMetaData :: Maybe [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExceptionDetails where
  parseJSON = A.withObject "RuntimeExceptionDetails" $ \o -> RuntimeExceptionDetails
    <$> o A..: "exceptionId"
    <*> o A..: "text"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
    <*> o A..:? "scriptId"
    <*> o A..:? "url"
    <*> o A..:? "stackTrace"
    <*> o A..:? "exception"
    <*> o A..:? "executionContextId"
    <*> o A..:? "exceptionMetaData"
instance ToJSON RuntimeExceptionDetails where
  toJSON p = A.object $ catMaybes [
    ("exceptionId" A..=) <$> Just (runtimeExceptionDetailsExceptionId p),
    ("text" A..=) <$> Just (runtimeExceptionDetailsText p),
    ("lineNumber" A..=) <$> Just (runtimeExceptionDetailsLineNumber p),
    ("columnNumber" A..=) <$> Just (runtimeExceptionDetailsColumnNumber p),
    ("scriptId" A..=) <$> (runtimeExceptionDetailsScriptId p),
    ("url" A..=) <$> (runtimeExceptionDetailsUrl p),
    ("stackTrace" A..=) <$> (runtimeExceptionDetailsStackTrace p),
    ("exception" A..=) <$> (runtimeExceptionDetailsException p),
    ("executionContextId" A..=) <$> (runtimeExceptionDetailsExecutionContextId p),
    ("exceptionMetaData" A..=) <$> (runtimeExceptionDetailsExceptionMetaData p)
    ]

-- | Type 'Runtime.Timestamp'.
--   Number of milliseconds since epoch.
type RuntimeTimestamp = Double

-- | Type 'Runtime.TimeDelta'.
--   Number of milliseconds.
type RuntimeTimeDelta = Double

-- | Type 'Runtime.CallFrame'.
--   Stack entry for runtime errors and assertions.
data RuntimeCallFrame = RuntimeCallFrame
  {
    -- | JavaScript function name.
    runtimeCallFrameFunctionName :: T.Text,
    -- | JavaScript script id.
    runtimeCallFrameScriptId :: RuntimeScriptId,
    -- | JavaScript script name or url.
    runtimeCallFrameUrl :: T.Text,
    -- | JavaScript script line number (0-based).
    runtimeCallFrameLineNumber :: Int,
    -- | JavaScript script column number (0-based).
    runtimeCallFrameColumnNumber :: Int
  }
  deriving (Eq, Show)
instance FromJSON RuntimeCallFrame where
  parseJSON = A.withObject "RuntimeCallFrame" $ \o -> RuntimeCallFrame
    <$> o A..: "functionName"
    <*> o A..: "scriptId"
    <*> o A..: "url"
    <*> o A..: "lineNumber"
    <*> o A..: "columnNumber"
instance ToJSON RuntimeCallFrame where
  toJSON p = A.object $ catMaybes [
    ("functionName" A..=) <$> Just (runtimeCallFrameFunctionName p),
    ("scriptId" A..=) <$> Just (runtimeCallFrameScriptId p),
    ("url" A..=) <$> Just (runtimeCallFrameUrl p),
    ("lineNumber" A..=) <$> Just (runtimeCallFrameLineNumber p),
    ("columnNumber" A..=) <$> Just (runtimeCallFrameColumnNumber p)
    ]

-- | Type 'Runtime.StackTrace'.
--   Call frames for assertions or error messages.
data RuntimeStackTrace = RuntimeStackTrace
  {
    -- | String label of this stack trace. For async traces this may be a name of the function that
    --   initiated the async call.
    runtimeStackTraceDescription :: Maybe T.Text,
    -- | JavaScript function name.
    runtimeStackTraceCallFrames :: [RuntimeCallFrame],
    -- | Asynchronous JavaScript stack trace that preceded this stack, if available.
    runtimeStackTraceParent :: Maybe RuntimeStackTrace,
    -- | Asynchronous JavaScript stack trace that preceded this stack, if available.
    runtimeStackTraceParentId :: Maybe RuntimeStackTraceId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeStackTrace where
  parseJSON = A.withObject "RuntimeStackTrace" $ \o -> RuntimeStackTrace
    <$> o A..:? "description"
    <*> o A..: "callFrames"
    <*> o A..:? "parent"
    <*> o A..:? "parentId"
instance ToJSON RuntimeStackTrace where
  toJSON p = A.object $ catMaybes [
    ("description" A..=) <$> (runtimeStackTraceDescription p),
    ("callFrames" A..=) <$> Just (runtimeStackTraceCallFrames p),
    ("parent" A..=) <$> (runtimeStackTraceParent p),
    ("parentId" A..=) <$> (runtimeStackTraceParentId p)
    ]

-- | Type 'Runtime.UniqueDebuggerId'.
--   Unique identifier of current debugger.
type RuntimeUniqueDebuggerId = T.Text

-- | Type 'Runtime.StackTraceId'.
--   If `debuggerId` is set stack trace comes from another debugger and can be resolved there. This
--   allows to track cross-debugger calls. See `Runtime.StackTrace` and `Debugger.paused` for usages.
data RuntimeStackTraceId = RuntimeStackTraceId
  {
    runtimeStackTraceIdId :: T.Text,
    runtimeStackTraceIdDebuggerId :: Maybe RuntimeUniqueDebuggerId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeStackTraceId where
  parseJSON = A.withObject "RuntimeStackTraceId" $ \o -> RuntimeStackTraceId
    <$> o A..: "id"
    <*> o A..:? "debuggerId"
instance ToJSON RuntimeStackTraceId where
  toJSON p = A.object $ catMaybes [
    ("id" A..=) <$> Just (runtimeStackTraceIdId p),
    ("debuggerId" A..=) <$> (runtimeStackTraceIdDebuggerId p)
    ]

-- | Type of the 'Runtime.bindingCalled' event.
data RuntimeBindingCalled = RuntimeBindingCalled
  {
    runtimeBindingCalledName :: T.Text,
    runtimeBindingCalledPayload :: T.Text,
    -- | Identifier of the context where the call was made.
    runtimeBindingCalledExecutionContextId :: RuntimeExecutionContextId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeBindingCalled where
  parseJSON = A.withObject "RuntimeBindingCalled" $ \o -> RuntimeBindingCalled
    <$> o A..: "name"
    <*> o A..: "payload"
    <*> o A..: "executionContextId"
instance Event RuntimeBindingCalled where
  eventName _ = "Runtime.bindingCalled"

-- | Type of the 'Runtime.consoleAPICalled' event.
data RuntimeConsoleAPICalledType = RuntimeConsoleAPICalledTypeLog | RuntimeConsoleAPICalledTypeDebug | RuntimeConsoleAPICalledTypeInfo | RuntimeConsoleAPICalledTypeError | RuntimeConsoleAPICalledTypeWarning | RuntimeConsoleAPICalledTypeDir | RuntimeConsoleAPICalledTypeDirxml | RuntimeConsoleAPICalledTypeTable | RuntimeConsoleAPICalledTypeTrace | RuntimeConsoleAPICalledTypeClear | RuntimeConsoleAPICalledTypeStartGroup | RuntimeConsoleAPICalledTypeStartGroupCollapsed | RuntimeConsoleAPICalledTypeEndGroup | RuntimeConsoleAPICalledTypeAssert | RuntimeConsoleAPICalledTypeProfile | RuntimeConsoleAPICalledTypeProfileEnd | RuntimeConsoleAPICalledTypeCount | RuntimeConsoleAPICalledTypeTimeEnd
  deriving (Ord, Eq, Show, Read)
instance FromJSON RuntimeConsoleAPICalledType where
  parseJSON = A.withText "RuntimeConsoleAPICalledType" $ \v -> case v of
    "log" -> pure RuntimeConsoleAPICalledTypeLog
    "debug" -> pure RuntimeConsoleAPICalledTypeDebug
    "info" -> pure RuntimeConsoleAPICalledTypeInfo
    "error" -> pure RuntimeConsoleAPICalledTypeError
    "warning" -> pure RuntimeConsoleAPICalledTypeWarning
    "dir" -> pure RuntimeConsoleAPICalledTypeDir
    "dirxml" -> pure RuntimeConsoleAPICalledTypeDirxml
    "table" -> pure RuntimeConsoleAPICalledTypeTable
    "trace" -> pure RuntimeConsoleAPICalledTypeTrace
    "clear" -> pure RuntimeConsoleAPICalledTypeClear
    "startGroup" -> pure RuntimeConsoleAPICalledTypeStartGroup
    "startGroupCollapsed" -> pure RuntimeConsoleAPICalledTypeStartGroupCollapsed
    "endGroup" -> pure RuntimeConsoleAPICalledTypeEndGroup
    "assert" -> pure RuntimeConsoleAPICalledTypeAssert
    "profile" -> pure RuntimeConsoleAPICalledTypeProfile
    "profileEnd" -> pure RuntimeConsoleAPICalledTypeProfileEnd
    "count" -> pure RuntimeConsoleAPICalledTypeCount
    "timeEnd" -> pure RuntimeConsoleAPICalledTypeTimeEnd
    "_" -> fail "failed to parse RuntimeConsoleAPICalledType"
instance ToJSON RuntimeConsoleAPICalledType where
  toJSON v = A.String $ case v of
    RuntimeConsoleAPICalledTypeLog -> "log"
    RuntimeConsoleAPICalledTypeDebug -> "debug"
    RuntimeConsoleAPICalledTypeInfo -> "info"
    RuntimeConsoleAPICalledTypeError -> "error"
    RuntimeConsoleAPICalledTypeWarning -> "warning"
    RuntimeConsoleAPICalledTypeDir -> "dir"
    RuntimeConsoleAPICalledTypeDirxml -> "dirxml"
    RuntimeConsoleAPICalledTypeTable -> "table"
    RuntimeConsoleAPICalledTypeTrace -> "trace"
    RuntimeConsoleAPICalledTypeClear -> "clear"
    RuntimeConsoleAPICalledTypeStartGroup -> "startGroup"
    RuntimeConsoleAPICalledTypeStartGroupCollapsed -> "startGroupCollapsed"
    RuntimeConsoleAPICalledTypeEndGroup -> "endGroup"
    RuntimeConsoleAPICalledTypeAssert -> "assert"
    RuntimeConsoleAPICalledTypeProfile -> "profile"
    RuntimeConsoleAPICalledTypeProfileEnd -> "profileEnd"
    RuntimeConsoleAPICalledTypeCount -> "count"
    RuntimeConsoleAPICalledTypeTimeEnd -> "timeEnd"
data RuntimeConsoleAPICalled = RuntimeConsoleAPICalled
  {
    -- | Type of the call.
    runtimeConsoleAPICalledType :: RuntimeConsoleAPICalledType,
    -- | Call arguments.
    runtimeConsoleAPICalledArgs :: [RuntimeRemoteObject],
    -- | Identifier of the context where the call was made.
    runtimeConsoleAPICalledExecutionContextId :: RuntimeExecutionContextId,
    -- | Call timestamp.
    runtimeConsoleAPICalledTimestamp :: RuntimeTimestamp,
    -- | Stack trace captured when the call was made. The async stack chain is automatically reported for
    --   the following call types: `assert`, `error`, `trace`, `warning`. For other types the async call
    --   chain can be retrieved using `Debugger.getStackTrace` and `stackTrace.parentId` field.
    runtimeConsoleAPICalledStackTrace :: Maybe RuntimeStackTrace,
    -- | Console context descriptor for calls on non-default console context (not console.*):
    --   'anonymous#unique-logger-id' for call on unnamed context, 'name#unique-logger-id' for call
    --   on named context.
    runtimeConsoleAPICalledContext :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON RuntimeConsoleAPICalled where
  parseJSON = A.withObject "RuntimeConsoleAPICalled" $ \o -> RuntimeConsoleAPICalled
    <$> o A..: "type"
    <*> o A..: "args"
    <*> o A..: "executionContextId"
    <*> o A..: "timestamp"
    <*> o A..:? "stackTrace"
    <*> o A..:? "context"
instance Event RuntimeConsoleAPICalled where
  eventName _ = "Runtime.consoleAPICalled"

-- | Type of the 'Runtime.exceptionRevoked' event.
data RuntimeExceptionRevoked = RuntimeExceptionRevoked
  {
    -- | Reason describing why exception was revoked.
    runtimeExceptionRevokedReason :: T.Text,
    -- | The id of revoked exception, as reported in `exceptionThrown`.
    runtimeExceptionRevokedExceptionId :: Int
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExceptionRevoked where
  parseJSON = A.withObject "RuntimeExceptionRevoked" $ \o -> RuntimeExceptionRevoked
    <$> o A..: "reason"
    <*> o A..: "exceptionId"
instance Event RuntimeExceptionRevoked where
  eventName _ = "Runtime.exceptionRevoked"

-- | Type of the 'Runtime.exceptionThrown' event.
data RuntimeExceptionThrown = RuntimeExceptionThrown
  {
    -- | Timestamp of the exception.
    runtimeExceptionThrownTimestamp :: RuntimeTimestamp,
    runtimeExceptionThrownExceptionDetails :: RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExceptionThrown where
  parseJSON = A.withObject "RuntimeExceptionThrown" $ \o -> RuntimeExceptionThrown
    <$> o A..: "timestamp"
    <*> o A..: "exceptionDetails"
instance Event RuntimeExceptionThrown where
  eventName _ = "Runtime.exceptionThrown"

-- | Type of the 'Runtime.executionContextCreated' event.
data RuntimeExecutionContextCreated = RuntimeExecutionContextCreated
  {
    -- | A newly created execution context.
    runtimeExecutionContextCreatedContext :: RuntimeExecutionContextDescription
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExecutionContextCreated where
  parseJSON = A.withObject "RuntimeExecutionContextCreated" $ \o -> RuntimeExecutionContextCreated
    <$> o A..: "context"
instance Event RuntimeExecutionContextCreated where
  eventName _ = "Runtime.executionContextCreated"

-- | Type of the 'Runtime.executionContextDestroyed' event.
data RuntimeExecutionContextDestroyed = RuntimeExecutionContextDestroyed
  {
    -- | Id of the destroyed context
    runtimeExecutionContextDestroyedExecutionContextId :: RuntimeExecutionContextId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeExecutionContextDestroyed where
  parseJSON = A.withObject "RuntimeExecutionContextDestroyed" $ \o -> RuntimeExecutionContextDestroyed
    <$> o A..: "executionContextId"
instance Event RuntimeExecutionContextDestroyed where
  eventName _ = "Runtime.executionContextDestroyed"

-- | Type of the 'Runtime.executionContextsCleared' event.
data RuntimeExecutionContextsCleared = RuntimeExecutionContextsCleared
  deriving (Eq, Show, Read)
instance FromJSON RuntimeExecutionContextsCleared where
  parseJSON _ = pure RuntimeExecutionContextsCleared
instance Event RuntimeExecutionContextsCleared where
  eventName _ = "Runtime.executionContextsCleared"

-- | Type of the 'Runtime.inspectRequested' event.
data RuntimeInspectRequested = RuntimeInspectRequested
  {
    runtimeInspectRequestedObject :: RuntimeRemoteObject,
    runtimeInspectRequestedHints :: [(T.Text, T.Text)],
    -- | Identifier of the context where the call was made.
    runtimeInspectRequestedExecutionContextId :: Maybe RuntimeExecutionContextId
  }
  deriving (Eq, Show)
instance FromJSON RuntimeInspectRequested where
  parseJSON = A.withObject "RuntimeInspectRequested" $ \o -> RuntimeInspectRequested
    <$> o A..: "object"
    <*> o A..: "hints"
    <*> o A..:? "executionContextId"
instance Event RuntimeInspectRequested where
  eventName _ = "Runtime.inspectRequested"

-- | Add handler to promise with given promise object id.

-- | Parameters of the 'Runtime.awaitPromise' command.
data PRuntimeAwaitPromise = PRuntimeAwaitPromise
  {
    -- | Identifier of the promise.
    pRuntimeAwaitPromisePromiseObjectId :: RuntimeRemoteObjectId,
    -- | Whether the result is expected to be a JSON object that should be sent by value.
    pRuntimeAwaitPromiseReturnByValue :: Maybe Bool,
    -- | Whether preview should be generated for the result.
    pRuntimeAwaitPromiseGeneratePreview :: Maybe Bool
  }
  deriving (Eq, Show)
pRuntimeAwaitPromise
  {-
  -- | Identifier of the promise.
  -}
  :: RuntimeRemoteObjectId
  -> PRuntimeAwaitPromise
pRuntimeAwaitPromise
  arg_pRuntimeAwaitPromisePromiseObjectId
  = PRuntimeAwaitPromise
    arg_pRuntimeAwaitPromisePromiseObjectId
    Nothing
    Nothing
instance ToJSON PRuntimeAwaitPromise where
  toJSON p = A.object $ catMaybes [
    ("promiseObjectId" A..=) <$> Just (pRuntimeAwaitPromisePromiseObjectId p),
    ("returnByValue" A..=) <$> (pRuntimeAwaitPromiseReturnByValue p),
    ("generatePreview" A..=) <$> (pRuntimeAwaitPromiseGeneratePreview p)
    ]
data RuntimeAwaitPromise = RuntimeAwaitPromise
  {
    -- | Promise result. Will contain rejected value if promise was rejected.
    runtimeAwaitPromiseResult :: RuntimeRemoteObject,
    -- | Exception details if stack strace is available.
    runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeAwaitPromise where
  parseJSON = A.withObject "RuntimeAwaitPromise" $ \o -> RuntimeAwaitPromise
    <$> o A..: "result"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeAwaitPromise where
  type CommandResponse PRuntimeAwaitPromise = RuntimeAwaitPromise
  commandName _ = "Runtime.awaitPromise"

-- | Calls function with given declaration on the given object. Object group of the result is
--   inherited from the target object.

-- | Parameters of the 'Runtime.callFunctionOn' command.
data PRuntimeCallFunctionOn = PRuntimeCallFunctionOn
  {
    -- | Declaration of the function to call.
    pRuntimeCallFunctionOnFunctionDeclaration :: T.Text,
    -- | Identifier of the object to call function on. Either objectId or executionContextId should
    --   be specified.
    pRuntimeCallFunctionOnObjectId :: Maybe RuntimeRemoteObjectId,
    -- | Call arguments. All call arguments must belong to the same JavaScript world as the target
    --   object.
    pRuntimeCallFunctionOnArguments :: Maybe [RuntimeCallArgument],
    -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
    --   execution. Overrides `setPauseOnException` state.
    pRuntimeCallFunctionOnSilent :: Maybe Bool,
    -- | Whether the result is expected to be a JSON object which should be sent by value.
    pRuntimeCallFunctionOnReturnByValue :: Maybe Bool,
    -- | Whether preview should be generated for the result.
    pRuntimeCallFunctionOnGeneratePreview :: Maybe Bool,
    -- | Whether execution should be treated as initiated by user in the UI.
    pRuntimeCallFunctionOnUserGesture :: Maybe Bool,
    -- | Whether execution should `await` for resulting value and return once awaited promise is
    --   resolved.
    pRuntimeCallFunctionOnAwaitPromise :: Maybe Bool,
    -- | Specifies execution context which global object will be used to call function on. Either
    --   executionContextId or objectId should be specified.
    pRuntimeCallFunctionOnExecutionContextId :: Maybe RuntimeExecutionContextId,
    -- | Symbolic group name that can be used to release multiple objects. If objectGroup is not
    --   specified and objectId is, objectGroup will be inherited from object.
    pRuntimeCallFunctionOnObjectGroup :: Maybe T.Text,
    -- | Whether to throw an exception if side effect cannot be ruled out during evaluation.
    pRuntimeCallFunctionOnThrowOnSideEffect :: Maybe Bool,
    -- | Whether the result should contain `webDriverValue`, serialized according to
    --   https://w3c.github.io/webdriver-bidi. This is mutually exclusive with `returnByValue`, but
    --   resulting `objectId` is still provided.
    pRuntimeCallFunctionOnGenerateWebDriverValue :: Maybe Bool
  }
  deriving (Eq, Show)
pRuntimeCallFunctionOn
  {-
  -- | Declaration of the function to call.
  -}
  :: T.Text
  -> PRuntimeCallFunctionOn
pRuntimeCallFunctionOn
  arg_pRuntimeCallFunctionOnFunctionDeclaration
  = PRuntimeCallFunctionOn
    arg_pRuntimeCallFunctionOnFunctionDeclaration
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PRuntimeCallFunctionOn where
  toJSON p = A.object $ catMaybes [
    ("functionDeclaration" A..=) <$> Just (pRuntimeCallFunctionOnFunctionDeclaration p),
    ("objectId" A..=) <$> (pRuntimeCallFunctionOnObjectId p),
    ("arguments" A..=) <$> (pRuntimeCallFunctionOnArguments p),
    ("silent" A..=) <$> (pRuntimeCallFunctionOnSilent p),
    ("returnByValue" A..=) <$> (pRuntimeCallFunctionOnReturnByValue p),
    ("generatePreview" A..=) <$> (pRuntimeCallFunctionOnGeneratePreview p),
    ("userGesture" A..=) <$> (pRuntimeCallFunctionOnUserGesture p),
    ("awaitPromise" A..=) <$> (pRuntimeCallFunctionOnAwaitPromise p),
    ("executionContextId" A..=) <$> (pRuntimeCallFunctionOnExecutionContextId p),
    ("objectGroup" A..=) <$> (pRuntimeCallFunctionOnObjectGroup p),
    ("throwOnSideEffect" A..=) <$> (pRuntimeCallFunctionOnThrowOnSideEffect p),
    ("generateWebDriverValue" A..=) <$> (pRuntimeCallFunctionOnGenerateWebDriverValue p)
    ]
data RuntimeCallFunctionOn = RuntimeCallFunctionOn
  {
    -- | Call result.
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    -- | Exception details.
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeCallFunctionOn where
  parseJSON = A.withObject "RuntimeCallFunctionOn" $ \o -> RuntimeCallFunctionOn
    <$> o A..: "result"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeCallFunctionOn where
  type CommandResponse PRuntimeCallFunctionOn = RuntimeCallFunctionOn
  commandName _ = "Runtime.callFunctionOn"

-- | Compiles expression.

-- | Parameters of the 'Runtime.compileScript' command.
data PRuntimeCompileScript = PRuntimeCompileScript
  {
    -- | Expression to compile.
    pRuntimeCompileScriptExpression :: T.Text,
    -- | Source url to be set for the script.
    pRuntimeCompileScriptSourceURL :: T.Text,
    -- | Specifies whether the compiled script should be persisted.
    pRuntimeCompileScriptPersistScript :: Bool,
    -- | Specifies in which execution context to perform script run. If the parameter is omitted the
    --   evaluation will be performed in the context of the inspected page.
    pRuntimeCompileScriptExecutionContextId :: Maybe RuntimeExecutionContextId
  }
  deriving (Eq, Show)
pRuntimeCompileScript
  {-
  -- | Expression to compile.
  -}
  :: T.Text
  {-
  -- | Source url to be set for the script.
  -}
  -> T.Text
  {-
  -- | Specifies whether the compiled script should be persisted.
  -}
  -> Bool
  -> PRuntimeCompileScript
pRuntimeCompileScript
  arg_pRuntimeCompileScriptExpression
  arg_pRuntimeCompileScriptSourceURL
  arg_pRuntimeCompileScriptPersistScript
  = PRuntimeCompileScript
    arg_pRuntimeCompileScriptExpression
    arg_pRuntimeCompileScriptSourceURL
    arg_pRuntimeCompileScriptPersistScript
    Nothing
instance ToJSON PRuntimeCompileScript where
  toJSON p = A.object $ catMaybes [
    ("expression" A..=) <$> Just (pRuntimeCompileScriptExpression p),
    ("sourceURL" A..=) <$> Just (pRuntimeCompileScriptSourceURL p),
    ("persistScript" A..=) <$> Just (pRuntimeCompileScriptPersistScript p),
    ("executionContextId" A..=) <$> (pRuntimeCompileScriptExecutionContextId p)
    ]
data RuntimeCompileScript = RuntimeCompileScript
  {
    -- | Id of the script.
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    -- | Exception details.
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeCompileScript where
  parseJSON = A.withObject "RuntimeCompileScript" $ \o -> RuntimeCompileScript
    <$> o A..:? "scriptId"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeCompileScript where
  type CommandResponse PRuntimeCompileScript = RuntimeCompileScript
  commandName _ = "Runtime.compileScript"

-- | Disables reporting of execution contexts creation.

-- | Parameters of the 'Runtime.disable' command.
data PRuntimeDisable = PRuntimeDisable
  deriving (Eq, Show)
pRuntimeDisable
  :: PRuntimeDisable
pRuntimeDisable
  = PRuntimeDisable
instance ToJSON PRuntimeDisable where
  toJSON _ = A.Null
instance Command PRuntimeDisable where
  type CommandResponse PRuntimeDisable = ()
  commandName _ = "Runtime.disable"
  fromJSON = const . A.Success . const ()

-- | Discards collected exceptions and console API calls.

-- | Parameters of the 'Runtime.discardConsoleEntries' command.
data PRuntimeDiscardConsoleEntries = PRuntimeDiscardConsoleEntries
  deriving (Eq, Show)
pRuntimeDiscardConsoleEntries
  :: PRuntimeDiscardConsoleEntries
pRuntimeDiscardConsoleEntries
  = PRuntimeDiscardConsoleEntries
instance ToJSON PRuntimeDiscardConsoleEntries where
  toJSON _ = A.Null
instance Command PRuntimeDiscardConsoleEntries where
  type CommandResponse PRuntimeDiscardConsoleEntries = ()
  commandName _ = "Runtime.discardConsoleEntries"
  fromJSON = const . A.Success . const ()

-- | Enables reporting of execution contexts creation by means of `executionContextCreated` event.
--   When the reporting gets enabled the event will be sent immediately for each existing execution
--   context.

-- | Parameters of the 'Runtime.enable' command.
data PRuntimeEnable = PRuntimeEnable
  deriving (Eq, Show)
pRuntimeEnable
  :: PRuntimeEnable
pRuntimeEnable
  = PRuntimeEnable
instance ToJSON PRuntimeEnable where
  toJSON _ = A.Null
instance Command PRuntimeEnable where
  type CommandResponse PRuntimeEnable = ()
  commandName _ = "Runtime.enable"
  fromJSON = const . A.Success . const ()

-- | Evaluates expression on global object.

-- | Parameters of the 'Runtime.evaluate' command.
data PRuntimeEvaluate = PRuntimeEvaluate
  {
    -- | Expression to evaluate.
    pRuntimeEvaluateExpression :: T.Text,
    -- | Symbolic group name that can be used to release multiple objects.
    pRuntimeEvaluateObjectGroup :: Maybe T.Text,
    -- | Determines whether Command Line API should be available during the evaluation.
    pRuntimeEvaluateIncludeCommandLineAPI :: Maybe Bool,
    -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
    --   execution. Overrides `setPauseOnException` state.
    pRuntimeEvaluateSilent :: Maybe Bool,
    -- | Specifies in which execution context to perform evaluation. If the parameter is omitted the
    --   evaluation will be performed in the context of the inspected page.
    --   This is mutually exclusive with `uniqueContextId`, which offers an
    --   alternative way to identify the execution context that is more reliable
    --   in a multi-process environment.
    pRuntimeEvaluateContextId :: Maybe RuntimeExecutionContextId,
    -- | Whether the result is expected to be a JSON object that should be sent by value.
    pRuntimeEvaluateReturnByValue :: Maybe Bool,
    -- | Whether preview should be generated for the result.
    pRuntimeEvaluateGeneratePreview :: Maybe Bool,
    -- | Whether execution should be treated as initiated by user in the UI.
    pRuntimeEvaluateUserGesture :: Maybe Bool,
    -- | Whether execution should `await` for resulting value and return once awaited promise is
    --   resolved.
    pRuntimeEvaluateAwaitPromise :: Maybe Bool,
    -- | Whether to throw an exception if side effect cannot be ruled out during evaluation.
    --   This implies `disableBreaks` below.
    pRuntimeEvaluateThrowOnSideEffect :: Maybe Bool,
    -- | Terminate execution after timing out (number of milliseconds).
    pRuntimeEvaluateTimeout :: Maybe RuntimeTimeDelta,
    -- | Disable breakpoints during execution.
    pRuntimeEvaluateDisableBreaks :: Maybe Bool,
    -- | Setting this flag to true enables `let` re-declaration and top-level `await`.
    --   Note that `let` variables can only be re-declared if they originate from
    --   `replMode` themselves.
    pRuntimeEvaluateReplMode :: Maybe Bool,
    -- | The Content Security Policy (CSP) for the target might block 'unsafe-eval'
    --   which includes eval(), Function(), setTimeout() and setInterval()
    --   when called with non-callable arguments. This flag bypasses CSP for this
    --   evaluation and allows unsafe-eval. Defaults to true.
    pRuntimeEvaluateAllowUnsafeEvalBlockedByCSP :: Maybe Bool,
    -- | An alternative way to specify the execution context to evaluate in.
    --   Compared to contextId that may be reused across processes, this is guaranteed to be
    --   system-unique, so it can be used to prevent accidental evaluation of the expression
    --   in context different than intended (e.g. as a result of navigation across process
    --   boundaries).
    --   This is mutually exclusive with `contextId`.
    pRuntimeEvaluateUniqueContextId :: Maybe T.Text,
    -- | Whether the result should be serialized according to https://w3c.github.io/webdriver-bidi.
    pRuntimeEvaluateGenerateWebDriverValue :: Maybe Bool
  }
  deriving (Eq, Show)
pRuntimeEvaluate
  {-
  -- | Expression to evaluate.
  -}
  :: T.Text
  -> PRuntimeEvaluate
pRuntimeEvaluate
  arg_pRuntimeEvaluateExpression
  = PRuntimeEvaluate
    arg_pRuntimeEvaluateExpression
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PRuntimeEvaluate where
  toJSON p = A.object $ catMaybes [
    ("expression" A..=) <$> Just (pRuntimeEvaluateExpression p),
    ("objectGroup" A..=) <$> (pRuntimeEvaluateObjectGroup p),
    ("includeCommandLineAPI" A..=) <$> (pRuntimeEvaluateIncludeCommandLineAPI p),
    ("silent" A..=) <$> (pRuntimeEvaluateSilent p),
    ("contextId" A..=) <$> (pRuntimeEvaluateContextId p),
    ("returnByValue" A..=) <$> (pRuntimeEvaluateReturnByValue p),
    ("generatePreview" A..=) <$> (pRuntimeEvaluateGeneratePreview p),
    ("userGesture" A..=) <$> (pRuntimeEvaluateUserGesture p),
    ("awaitPromise" A..=) <$> (pRuntimeEvaluateAwaitPromise p),
    ("throwOnSideEffect" A..=) <$> (pRuntimeEvaluateThrowOnSideEffect p),
    ("timeout" A..=) <$> (pRuntimeEvaluateTimeout p),
    ("disableBreaks" A..=) <$> (pRuntimeEvaluateDisableBreaks p),
    ("replMode" A..=) <$> (pRuntimeEvaluateReplMode p),
    ("allowUnsafeEvalBlockedByCSP" A..=) <$> (pRuntimeEvaluateAllowUnsafeEvalBlockedByCSP p),
    ("uniqueContextId" A..=) <$> (pRuntimeEvaluateUniqueContextId p),
    ("generateWebDriverValue" A..=) <$> (pRuntimeEvaluateGenerateWebDriverValue p)
    ]
data RuntimeEvaluate = RuntimeEvaluate
  {
    -- | Evaluation result.
    runtimeEvaluateResult :: RuntimeRemoteObject,
    -- | Exception details.
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeEvaluate where
  parseJSON = A.withObject "RuntimeEvaluate" $ \o -> RuntimeEvaluate
    <$> o A..: "result"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeEvaluate where
  type CommandResponse PRuntimeEvaluate = RuntimeEvaluate
  commandName _ = "Runtime.evaluate"

-- | Returns the isolate id.

-- | Parameters of the 'Runtime.getIsolateId' command.
data PRuntimeGetIsolateId = PRuntimeGetIsolateId
  deriving (Eq, Show)
pRuntimeGetIsolateId
  :: PRuntimeGetIsolateId
pRuntimeGetIsolateId
  = PRuntimeGetIsolateId
instance ToJSON PRuntimeGetIsolateId where
  toJSON _ = A.Null
data RuntimeGetIsolateId = RuntimeGetIsolateId
  {
    -- | The isolate id.
    runtimeGetIsolateIdId :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON RuntimeGetIsolateId where
  parseJSON = A.withObject "RuntimeGetIsolateId" $ \o -> RuntimeGetIsolateId
    <$> o A..: "id"
instance Command PRuntimeGetIsolateId where
  type CommandResponse PRuntimeGetIsolateId = RuntimeGetIsolateId
  commandName _ = "Runtime.getIsolateId"

-- | Returns the JavaScript heap usage.
--   It is the total usage of the corresponding isolate not scoped to a particular Runtime.

-- | Parameters of the 'Runtime.getHeapUsage' command.
data PRuntimeGetHeapUsage = PRuntimeGetHeapUsage
  deriving (Eq, Show)
pRuntimeGetHeapUsage
  :: PRuntimeGetHeapUsage
pRuntimeGetHeapUsage
  = PRuntimeGetHeapUsage
instance ToJSON PRuntimeGetHeapUsage where
  toJSON _ = A.Null
data RuntimeGetHeapUsage = RuntimeGetHeapUsage
  {
    -- | Used heap size in bytes.
    runtimeGetHeapUsageUsedSize :: Double,
    -- | Allocated heap size in bytes.
    runtimeGetHeapUsageTotalSize :: Double
  }
  deriving (Eq, Show)
instance FromJSON RuntimeGetHeapUsage where
  parseJSON = A.withObject "RuntimeGetHeapUsage" $ \o -> RuntimeGetHeapUsage
    <$> o A..: "usedSize"
    <*> o A..: "totalSize"
instance Command PRuntimeGetHeapUsage where
  type CommandResponse PRuntimeGetHeapUsage = RuntimeGetHeapUsage
  commandName _ = "Runtime.getHeapUsage"

-- | Returns properties of a given object. Object group of the result is inherited from the target
--   object.

-- | Parameters of the 'Runtime.getProperties' command.
data PRuntimeGetProperties = PRuntimeGetProperties
  {
    -- | Identifier of the object to return properties for.
    pRuntimeGetPropertiesObjectId :: RuntimeRemoteObjectId,
    -- | If true, returns properties belonging only to the element itself, not to its prototype
    --   chain.
    pRuntimeGetPropertiesOwnProperties :: Maybe Bool,
    -- | If true, returns accessor properties (with getter/setter) only; internal properties are not
    --   returned either.
    pRuntimeGetPropertiesAccessorPropertiesOnly :: Maybe Bool,
    -- | Whether preview should be generated for the results.
    pRuntimeGetPropertiesGeneratePreview :: Maybe Bool,
    -- | If true, returns non-indexed properties only.
    pRuntimeGetPropertiesNonIndexedPropertiesOnly :: Maybe Bool
  }
  deriving (Eq, Show)
pRuntimeGetProperties
  {-
  -- | Identifier of the object to return properties for.
  -}
  :: RuntimeRemoteObjectId
  -> PRuntimeGetProperties
pRuntimeGetProperties
  arg_pRuntimeGetPropertiesObjectId
  = PRuntimeGetProperties
    arg_pRuntimeGetPropertiesObjectId
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PRuntimeGetProperties where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pRuntimeGetPropertiesObjectId p),
    ("ownProperties" A..=) <$> (pRuntimeGetPropertiesOwnProperties p),
    ("accessorPropertiesOnly" A..=) <$> (pRuntimeGetPropertiesAccessorPropertiesOnly p),
    ("generatePreview" A..=) <$> (pRuntimeGetPropertiesGeneratePreview p),
    ("nonIndexedPropertiesOnly" A..=) <$> (pRuntimeGetPropertiesNonIndexedPropertiesOnly p)
    ]
data RuntimeGetProperties = RuntimeGetProperties
  {
    -- | Object properties.
    runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
    -- | Internal object properties (only of the element itself).
    runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
    -- | Object private properties.
    runtimeGetPropertiesPrivateProperties :: Maybe [RuntimePrivatePropertyDescriptor],
    -- | Exception details.
    runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeGetProperties where
  parseJSON = A.withObject "RuntimeGetProperties" $ \o -> RuntimeGetProperties
    <$> o A..: "result"
    <*> o A..:? "internalProperties"
    <*> o A..:? "privateProperties"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeGetProperties where
  type CommandResponse PRuntimeGetProperties = RuntimeGetProperties
  commandName _ = "Runtime.getProperties"

-- | Returns all let, const and class variables from global scope.

-- | Parameters of the 'Runtime.globalLexicalScopeNames' command.
data PRuntimeGlobalLexicalScopeNames = PRuntimeGlobalLexicalScopeNames
  {
    -- | Specifies in which execution context to lookup global scope variables.
    pRuntimeGlobalLexicalScopeNamesExecutionContextId :: Maybe RuntimeExecutionContextId
  }
  deriving (Eq, Show)
pRuntimeGlobalLexicalScopeNames
  :: PRuntimeGlobalLexicalScopeNames
pRuntimeGlobalLexicalScopeNames
  = PRuntimeGlobalLexicalScopeNames
    Nothing
instance ToJSON PRuntimeGlobalLexicalScopeNames where
  toJSON p = A.object $ catMaybes [
    ("executionContextId" A..=) <$> (pRuntimeGlobalLexicalScopeNamesExecutionContextId p)
    ]
data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames
  {
    runtimeGlobalLexicalScopeNamesNames :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON RuntimeGlobalLexicalScopeNames where
  parseJSON = A.withObject "RuntimeGlobalLexicalScopeNames" $ \o -> RuntimeGlobalLexicalScopeNames
    <$> o A..: "names"
instance Command PRuntimeGlobalLexicalScopeNames where
  type CommandResponse PRuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames
  commandName _ = "Runtime.globalLexicalScopeNames"


-- | Parameters of the 'Runtime.queryObjects' command.
data PRuntimeQueryObjects = PRuntimeQueryObjects
  {
    -- | Identifier of the prototype to return objects for.
    pRuntimeQueryObjectsPrototypeObjectId :: RuntimeRemoteObjectId,
    -- | Symbolic group name that can be used to release the results.
    pRuntimeQueryObjectsObjectGroup :: Maybe T.Text
  }
  deriving (Eq, Show)
pRuntimeQueryObjects
  {-
  -- | Identifier of the prototype to return objects for.
  -}
  :: RuntimeRemoteObjectId
  -> PRuntimeQueryObjects
pRuntimeQueryObjects
  arg_pRuntimeQueryObjectsPrototypeObjectId
  = PRuntimeQueryObjects
    arg_pRuntimeQueryObjectsPrototypeObjectId
    Nothing
instance ToJSON PRuntimeQueryObjects where
  toJSON p = A.object $ catMaybes [
    ("prototypeObjectId" A..=) <$> Just (pRuntimeQueryObjectsPrototypeObjectId p),
    ("objectGroup" A..=) <$> (pRuntimeQueryObjectsObjectGroup p)
    ]
data RuntimeQueryObjects = RuntimeQueryObjects
  {
    -- | Array with objects.
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
  }
  deriving (Eq, Show)
instance FromJSON RuntimeQueryObjects where
  parseJSON = A.withObject "RuntimeQueryObjects" $ \o -> RuntimeQueryObjects
    <$> o A..: "objects"
instance Command PRuntimeQueryObjects where
  type CommandResponse PRuntimeQueryObjects = RuntimeQueryObjects
  commandName _ = "Runtime.queryObjects"

-- | Releases remote object with given id.

-- | Parameters of the 'Runtime.releaseObject' command.
data PRuntimeReleaseObject = PRuntimeReleaseObject
  {
    -- | Identifier of the object to release.
    pRuntimeReleaseObjectObjectId :: RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pRuntimeReleaseObject
  {-
  -- | Identifier of the object to release.
  -}
  :: RuntimeRemoteObjectId
  -> PRuntimeReleaseObject
pRuntimeReleaseObject
  arg_pRuntimeReleaseObjectObjectId
  = PRuntimeReleaseObject
    arg_pRuntimeReleaseObjectObjectId
instance ToJSON PRuntimeReleaseObject where
  toJSON p = A.object $ catMaybes [
    ("objectId" A..=) <$> Just (pRuntimeReleaseObjectObjectId p)
    ]
instance Command PRuntimeReleaseObject where
  type CommandResponse PRuntimeReleaseObject = ()
  commandName _ = "Runtime.releaseObject"
  fromJSON = const . A.Success . const ()

-- | Releases all remote objects that belong to a given group.

-- | Parameters of the 'Runtime.releaseObjectGroup' command.
data PRuntimeReleaseObjectGroup = PRuntimeReleaseObjectGroup
  {
    -- | Symbolic object group name.
    pRuntimeReleaseObjectGroupObjectGroup :: T.Text
  }
  deriving (Eq, Show)
pRuntimeReleaseObjectGroup
  {-
  -- | Symbolic object group name.
  -}
  :: T.Text
  -> PRuntimeReleaseObjectGroup
pRuntimeReleaseObjectGroup
  arg_pRuntimeReleaseObjectGroupObjectGroup
  = PRuntimeReleaseObjectGroup
    arg_pRuntimeReleaseObjectGroupObjectGroup
instance ToJSON PRuntimeReleaseObjectGroup where
  toJSON p = A.object $ catMaybes [
    ("objectGroup" A..=) <$> Just (pRuntimeReleaseObjectGroupObjectGroup p)
    ]
instance Command PRuntimeReleaseObjectGroup where
  type CommandResponse PRuntimeReleaseObjectGroup = ()
  commandName _ = "Runtime.releaseObjectGroup"
  fromJSON = const . A.Success . const ()

-- | Tells inspected instance to run if it was waiting for debugger to attach.

-- | Parameters of the 'Runtime.runIfWaitingForDebugger' command.
data PRuntimeRunIfWaitingForDebugger = PRuntimeRunIfWaitingForDebugger
  deriving (Eq, Show)
pRuntimeRunIfWaitingForDebugger
  :: PRuntimeRunIfWaitingForDebugger
pRuntimeRunIfWaitingForDebugger
  = PRuntimeRunIfWaitingForDebugger
instance ToJSON PRuntimeRunIfWaitingForDebugger where
  toJSON _ = A.Null
instance Command PRuntimeRunIfWaitingForDebugger where
  type CommandResponse PRuntimeRunIfWaitingForDebugger = ()
  commandName _ = "Runtime.runIfWaitingForDebugger"
  fromJSON = const . A.Success . const ()

-- | Runs script with given id in a given context.

-- | Parameters of the 'Runtime.runScript' command.
data PRuntimeRunScript = PRuntimeRunScript
  {
    -- | Id of the script to run.
    pRuntimeRunScriptScriptId :: RuntimeScriptId,
    -- | Specifies in which execution context to perform script run. If the parameter is omitted the
    --   evaluation will be performed in the context of the inspected page.
    pRuntimeRunScriptExecutionContextId :: Maybe RuntimeExecutionContextId,
    -- | Symbolic group name that can be used to release multiple objects.
    pRuntimeRunScriptObjectGroup :: Maybe T.Text,
    -- | In silent mode exceptions thrown during evaluation are not reported and do not pause
    --   execution. Overrides `setPauseOnException` state.
    pRuntimeRunScriptSilent :: Maybe Bool,
    -- | Determines whether Command Line API should be available during the evaluation.
    pRuntimeRunScriptIncludeCommandLineAPI :: Maybe Bool,
    -- | Whether the result is expected to be a JSON object which should be sent by value.
    pRuntimeRunScriptReturnByValue :: Maybe Bool,
    -- | Whether preview should be generated for the result.
    pRuntimeRunScriptGeneratePreview :: Maybe Bool,
    -- | Whether execution should `await` for resulting value and return once awaited promise is
    --   resolved.
    pRuntimeRunScriptAwaitPromise :: Maybe Bool
  }
  deriving (Eq, Show)
pRuntimeRunScript
  {-
  -- | Id of the script to run.
  -}
  :: RuntimeScriptId
  -> PRuntimeRunScript
pRuntimeRunScript
  arg_pRuntimeRunScriptScriptId
  = PRuntimeRunScript
    arg_pRuntimeRunScriptScriptId
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PRuntimeRunScript where
  toJSON p = A.object $ catMaybes [
    ("scriptId" A..=) <$> Just (pRuntimeRunScriptScriptId p),
    ("executionContextId" A..=) <$> (pRuntimeRunScriptExecutionContextId p),
    ("objectGroup" A..=) <$> (pRuntimeRunScriptObjectGroup p),
    ("silent" A..=) <$> (pRuntimeRunScriptSilent p),
    ("includeCommandLineAPI" A..=) <$> (pRuntimeRunScriptIncludeCommandLineAPI p),
    ("returnByValue" A..=) <$> (pRuntimeRunScriptReturnByValue p),
    ("generatePreview" A..=) <$> (pRuntimeRunScriptGeneratePreview p),
    ("awaitPromise" A..=) <$> (pRuntimeRunScriptAwaitPromise p)
    ]
data RuntimeRunScript = RuntimeRunScript
  {
    -- | Run result.
    runtimeRunScriptResult :: RuntimeRemoteObject,
    -- | Exception details.
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeRunScript where
  parseJSON = A.withObject "RuntimeRunScript" $ \o -> RuntimeRunScript
    <$> o A..: "result"
    <*> o A..:? "exceptionDetails"
instance Command PRuntimeRunScript where
  type CommandResponse PRuntimeRunScript = RuntimeRunScript
  commandName _ = "Runtime.runScript"

-- | Enables or disables async call stacks tracking.

-- | Parameters of the 'Runtime.setAsyncCallStackDepth' command.
data PRuntimeSetAsyncCallStackDepth = PRuntimeSetAsyncCallStackDepth
  {
    -- | Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
    --   call stacks (default).
    pRuntimeSetAsyncCallStackDepthMaxDepth :: Int
  }
  deriving (Eq, Show)
pRuntimeSetAsyncCallStackDepth
  {-
  -- | Maximum depth of async call stacks. Setting to `0` will effectively disable collecting async
  --   call stacks (default).
  -}
  :: Int
  -> PRuntimeSetAsyncCallStackDepth
pRuntimeSetAsyncCallStackDepth
  arg_pRuntimeSetAsyncCallStackDepthMaxDepth
  = PRuntimeSetAsyncCallStackDepth
    arg_pRuntimeSetAsyncCallStackDepthMaxDepth
instance ToJSON PRuntimeSetAsyncCallStackDepth where
  toJSON p = A.object $ catMaybes [
    ("maxDepth" A..=) <$> Just (pRuntimeSetAsyncCallStackDepthMaxDepth p)
    ]
instance Command PRuntimeSetAsyncCallStackDepth where
  type CommandResponse PRuntimeSetAsyncCallStackDepth = ()
  commandName _ = "Runtime.setAsyncCallStackDepth"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Runtime.setCustomObjectFormatterEnabled' command.
data PRuntimeSetCustomObjectFormatterEnabled = PRuntimeSetCustomObjectFormatterEnabled
  {
    pRuntimeSetCustomObjectFormatterEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pRuntimeSetCustomObjectFormatterEnabled
  :: Bool
  -> PRuntimeSetCustomObjectFormatterEnabled
pRuntimeSetCustomObjectFormatterEnabled
  arg_pRuntimeSetCustomObjectFormatterEnabledEnabled
  = PRuntimeSetCustomObjectFormatterEnabled
    arg_pRuntimeSetCustomObjectFormatterEnabledEnabled
instance ToJSON PRuntimeSetCustomObjectFormatterEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pRuntimeSetCustomObjectFormatterEnabledEnabled p)
    ]
instance Command PRuntimeSetCustomObjectFormatterEnabled where
  type CommandResponse PRuntimeSetCustomObjectFormatterEnabled = ()
  commandName _ = "Runtime.setCustomObjectFormatterEnabled"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Runtime.setMaxCallStackSizeToCapture' command.
data PRuntimeSetMaxCallStackSizeToCapture = PRuntimeSetMaxCallStackSizeToCapture
  {
    pRuntimeSetMaxCallStackSizeToCaptureSize :: Int
  }
  deriving (Eq, Show)
pRuntimeSetMaxCallStackSizeToCapture
  :: Int
  -> PRuntimeSetMaxCallStackSizeToCapture
pRuntimeSetMaxCallStackSizeToCapture
  arg_pRuntimeSetMaxCallStackSizeToCaptureSize
  = PRuntimeSetMaxCallStackSizeToCapture
    arg_pRuntimeSetMaxCallStackSizeToCaptureSize
instance ToJSON PRuntimeSetMaxCallStackSizeToCapture where
  toJSON p = A.object $ catMaybes [
    ("size" A..=) <$> Just (pRuntimeSetMaxCallStackSizeToCaptureSize p)
    ]
instance Command PRuntimeSetMaxCallStackSizeToCapture where
  type CommandResponse PRuntimeSetMaxCallStackSizeToCapture = ()
  commandName _ = "Runtime.setMaxCallStackSizeToCapture"
  fromJSON = const . A.Success . const ()

-- | Terminate current or next JavaScript execution.
--   Will cancel the termination when the outer-most script execution ends.

-- | Parameters of the 'Runtime.terminateExecution' command.
data PRuntimeTerminateExecution = PRuntimeTerminateExecution
  deriving (Eq, Show)
pRuntimeTerminateExecution
  :: PRuntimeTerminateExecution
pRuntimeTerminateExecution
  = PRuntimeTerminateExecution
instance ToJSON PRuntimeTerminateExecution where
  toJSON _ = A.Null
instance Command PRuntimeTerminateExecution where
  type CommandResponse PRuntimeTerminateExecution = ()
  commandName _ = "Runtime.terminateExecution"
  fromJSON = const . A.Success . const ()

-- | If executionContextId is empty, adds binding with the given name on the
--   global objects of all inspected contexts, including those created later,
--   bindings survive reloads.
--   Binding function takes exactly one argument, this argument should be string,
--   in case of any other input, function throws an exception.
--   Each binding function call produces Runtime.bindingCalled notification.

-- | Parameters of the 'Runtime.addBinding' command.
data PRuntimeAddBinding = PRuntimeAddBinding
  {
    pRuntimeAddBindingName :: T.Text,
    -- | If specified, the binding is exposed to the executionContext with
    --   matching name, even for contexts created after the binding is added.
    --   See also `ExecutionContext.name` and `worldName` parameter to
    --   `Page.addScriptToEvaluateOnNewDocument`.
    --   This parameter is mutually exclusive with `executionContextId`.
    pRuntimeAddBindingExecutionContextName :: Maybe T.Text
  }
  deriving (Eq, Show)
pRuntimeAddBinding
  :: T.Text
  -> PRuntimeAddBinding
pRuntimeAddBinding
  arg_pRuntimeAddBindingName
  = PRuntimeAddBinding
    arg_pRuntimeAddBindingName
    Nothing
instance ToJSON PRuntimeAddBinding where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pRuntimeAddBindingName p),
    ("executionContextName" A..=) <$> (pRuntimeAddBindingExecutionContextName p)
    ]
instance Command PRuntimeAddBinding where
  type CommandResponse PRuntimeAddBinding = ()
  commandName _ = "Runtime.addBinding"
  fromJSON = const . A.Success . const ()

-- | This method does not remove binding function from global object but
--   unsubscribes current runtime agent from Runtime.bindingCalled notifications.

-- | Parameters of the 'Runtime.removeBinding' command.
data PRuntimeRemoveBinding = PRuntimeRemoveBinding
  {
    pRuntimeRemoveBindingName :: T.Text
  }
  deriving (Eq, Show)
pRuntimeRemoveBinding
  :: T.Text
  -> PRuntimeRemoveBinding
pRuntimeRemoveBinding
  arg_pRuntimeRemoveBindingName
  = PRuntimeRemoveBinding
    arg_pRuntimeRemoveBindingName
instance ToJSON PRuntimeRemoveBinding where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pRuntimeRemoveBindingName p)
    ]
instance Command PRuntimeRemoveBinding where
  type CommandResponse PRuntimeRemoveBinding = ()
  commandName _ = "Runtime.removeBinding"
  fromJSON = const . A.Success . const ()

-- | This method tries to lookup and populate exception details for a
--   JavaScript Error object.
--   Note that the stackTrace portion of the resulting exceptionDetails will
--   only be populated if the Runtime domain was enabled at the time when the
--   Error was thrown.

-- | Parameters of the 'Runtime.getExceptionDetails' command.
data PRuntimeGetExceptionDetails = PRuntimeGetExceptionDetails
  {
    -- | The error object for which to resolve the exception details.
    pRuntimeGetExceptionDetailsErrorObjectId :: RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pRuntimeGetExceptionDetails
  {-
  -- | The error object for which to resolve the exception details.
  -}
  :: RuntimeRemoteObjectId
  -> PRuntimeGetExceptionDetails
pRuntimeGetExceptionDetails
  arg_pRuntimeGetExceptionDetailsErrorObjectId
  = PRuntimeGetExceptionDetails
    arg_pRuntimeGetExceptionDetailsErrorObjectId
instance ToJSON PRuntimeGetExceptionDetails where
  toJSON p = A.object $ catMaybes [
    ("errorObjectId" A..=) <$> Just (pRuntimeGetExceptionDetailsErrorObjectId p)
    ]
data RuntimeGetExceptionDetails = RuntimeGetExceptionDetails
  {
    runtimeGetExceptionDetailsExceptionDetails :: Maybe RuntimeExceptionDetails
  }
  deriving (Eq, Show)
instance FromJSON RuntimeGetExceptionDetails where
  parseJSON = A.withObject "RuntimeGetExceptionDetails" $ \o -> RuntimeGetExceptionDetails
    <$> o A..:? "exceptionDetails"
instance Command PRuntimeGetExceptionDetails where
  type CommandResponse PRuntimeGetExceptionDetails = RuntimeGetExceptionDetails
  commandName _ = "Runtime.getExceptionDetails"

