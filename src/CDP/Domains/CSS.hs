{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= CSS

This domain exposes CSS read/write operations. All CSS objects (stylesheets, rules, and styles)
have an associated `id` used in subsequent operations on the related object. Each object type has
a specific `id` structure, and those are not interchangeable between objects of different kinds.
CSS objects can be loaded using the `get*ForNode()` calls (which accept a DOM node id). A client
can also keep track of stylesheets via the `styleSheetAdded`/`styleSheetRemoved` events and
subsequently load the required stylesheet contents using the `getStyleSheet[Text]()` methods.
-}


module CDP.Domains.CSS (module CDP.Domains.CSS) where

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


-- | Type 'CSS.StyleSheetId'.
type CSSStyleSheetId = T.Text

-- | Type 'CSS.StyleSheetOrigin'.
--   Stylesheet type: "injected" for stylesheets injected via extension, "user-agent" for user-agent
--   stylesheets, "inspector" for stylesheets created by the inspector (i.e. those holding the "via
--   inspector" rules), "regular" for regular stylesheets.
data CSSStyleSheetOrigin = CSSStyleSheetOriginInjected | CSSStyleSheetOriginUserAgent | CSSStyleSheetOriginInspector | CSSStyleSheetOriginRegular
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSStyleSheetOrigin where
  parseJSON = A.withText "CSSStyleSheetOrigin" $ \v -> case v of
    "injected" -> pure CSSStyleSheetOriginInjected
    "user-agent" -> pure CSSStyleSheetOriginUserAgent
    "inspector" -> pure CSSStyleSheetOriginInspector
    "regular" -> pure CSSStyleSheetOriginRegular
    "_" -> fail "failed to parse CSSStyleSheetOrigin"
instance ToJSON CSSStyleSheetOrigin where
  toJSON v = A.String $ case v of
    CSSStyleSheetOriginInjected -> "injected"
    CSSStyleSheetOriginUserAgent -> "user-agent"
    CSSStyleSheetOriginInspector -> "inspector"
    CSSStyleSheetOriginRegular -> "regular"

-- | Type 'CSS.PseudoElementMatches'.
--   CSS rule collection for a single pseudo style.
data CSSPseudoElementMatches = CSSPseudoElementMatches
  {
    -- | Pseudo element type.
    cSSPseudoElementMatchesPseudoType :: DOMPageNetworkEmulationSecurity.DOMPseudoType,
    -- | Pseudo element custom ident.
    cSSPseudoElementMatchesPseudoIdentifier :: Maybe T.Text,
    -- | Matches of CSS rules applicable to the pseudo style.
    cSSPseudoElementMatchesMatches :: [CSSRuleMatch]
  }
  deriving (Eq, Show)
instance FromJSON CSSPseudoElementMatches where
  parseJSON = A.withObject "CSSPseudoElementMatches" $ \o -> CSSPseudoElementMatches
    <$> o A..: "pseudoType"
    <*> o A..:? "pseudoIdentifier"
    <*> o A..: "matches"
instance ToJSON CSSPseudoElementMatches where
  toJSON p = A.object $ catMaybes [
    ("pseudoType" A..=) <$> Just (cSSPseudoElementMatchesPseudoType p),
    ("pseudoIdentifier" A..=) <$> (cSSPseudoElementMatchesPseudoIdentifier p),
    ("matches" A..=) <$> Just (cSSPseudoElementMatchesMatches p)
    ]

-- | Type 'CSS.InheritedStyleEntry'.
--   Inherited CSS rule collection from ancestor node.
data CSSInheritedStyleEntry = CSSInheritedStyleEntry
  {
    -- | The ancestor node's inline style, if any, in the style inheritance chain.
    cSSInheritedStyleEntryInlineStyle :: Maybe CSSCSSStyle,
    -- | Matches of CSS rules matching the ancestor node in the style inheritance chain.
    cSSInheritedStyleEntryMatchedCSSRules :: [CSSRuleMatch]
  }
  deriving (Eq, Show)
instance FromJSON CSSInheritedStyleEntry where
  parseJSON = A.withObject "CSSInheritedStyleEntry" $ \o -> CSSInheritedStyleEntry
    <$> o A..:? "inlineStyle"
    <*> o A..: "matchedCSSRules"
instance ToJSON CSSInheritedStyleEntry where
  toJSON p = A.object $ catMaybes [
    ("inlineStyle" A..=) <$> (cSSInheritedStyleEntryInlineStyle p),
    ("matchedCSSRules" A..=) <$> Just (cSSInheritedStyleEntryMatchedCSSRules p)
    ]

-- | Type 'CSS.InheritedPseudoElementMatches'.
--   Inherited pseudo element matches from pseudos of an ancestor node.
data CSSInheritedPseudoElementMatches = CSSInheritedPseudoElementMatches
  {
    -- | Matches of pseudo styles from the pseudos of an ancestor node.
    cSSInheritedPseudoElementMatchesPseudoElements :: [CSSPseudoElementMatches]
  }
  deriving (Eq, Show)
instance FromJSON CSSInheritedPseudoElementMatches where
  parseJSON = A.withObject "CSSInheritedPseudoElementMatches" $ \o -> CSSInheritedPseudoElementMatches
    <$> o A..: "pseudoElements"
instance ToJSON CSSInheritedPseudoElementMatches where
  toJSON p = A.object $ catMaybes [
    ("pseudoElements" A..=) <$> Just (cSSInheritedPseudoElementMatchesPseudoElements p)
    ]

-- | Type 'CSS.RuleMatch'.
--   Match data for a CSS rule.
data CSSRuleMatch = CSSRuleMatch
  {
    -- | CSS rule in the match.
    cSSRuleMatchRule :: CSSCSSRule,
    -- | Matching selector indices in the rule's selectorList selectors (0-based).
    cSSRuleMatchMatchingSelectors :: [Int]
  }
  deriving (Eq, Show)
instance FromJSON CSSRuleMatch where
  parseJSON = A.withObject "CSSRuleMatch" $ \o -> CSSRuleMatch
    <$> o A..: "rule"
    <*> o A..: "matchingSelectors"
instance ToJSON CSSRuleMatch where
  toJSON p = A.object $ catMaybes [
    ("rule" A..=) <$> Just (cSSRuleMatchRule p),
    ("matchingSelectors" A..=) <$> Just (cSSRuleMatchMatchingSelectors p)
    ]

-- | Type 'CSS.Value'.
--   Data for a simple selector (these are delimited by commas in a selector list).
data CSSValue = CSSValue
  {
    -- | Value text.
    cSSValueText :: T.Text,
    -- | Value range in the underlying resource (if available).
    cSSValueRange :: Maybe CSSSourceRange
  }
  deriving (Eq, Show)
instance FromJSON CSSValue where
  parseJSON = A.withObject "CSSValue" $ \o -> CSSValue
    <$> o A..: "text"
    <*> o A..:? "range"
instance ToJSON CSSValue where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSValueText p),
    ("range" A..=) <$> (cSSValueRange p)
    ]

-- | Type 'CSS.SelectorList'.
--   Selector list data.
data CSSSelectorList = CSSSelectorList
  {
    -- | Selectors in the list.
    cSSSelectorListSelectors :: [CSSValue],
    -- | Rule selector text.
    cSSSelectorListText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSSelectorList where
  parseJSON = A.withObject "CSSSelectorList" $ \o -> CSSSelectorList
    <$> o A..: "selectors"
    <*> o A..: "text"
instance ToJSON CSSSelectorList where
  toJSON p = A.object $ catMaybes [
    ("selectors" A..=) <$> Just (cSSSelectorListSelectors p),
    ("text" A..=) <$> Just (cSSSelectorListText p)
    ]

-- | Type 'CSS.CSSStyleSheetHeader'.
--   CSS stylesheet metainformation.
data CSSCSSStyleSheetHeader = CSSCSSStyleSheetHeader
  {
    -- | The stylesheet identifier.
    cSSCSSStyleSheetHeaderStyleSheetId :: CSSStyleSheetId,
    -- | Owner frame identifier.
    cSSCSSStyleSheetHeaderFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | Stylesheet resource URL. Empty if this is a constructed stylesheet created using
    --   new CSSStyleSheet() (but non-empty if this is a constructed sylesheet imported
    --   as a CSS module script).
    cSSCSSStyleSheetHeaderSourceURL :: T.Text,
    -- | URL of source map associated with the stylesheet (if any).
    cSSCSSStyleSheetHeaderSourceMapURL :: Maybe T.Text,
    -- | Stylesheet origin.
    cSSCSSStyleSheetHeaderOrigin :: CSSStyleSheetOrigin,
    -- | Stylesheet title.
    cSSCSSStyleSheetHeaderTitle :: T.Text,
    -- | The backend id for the owner node of the stylesheet.
    cSSCSSStyleSheetHeaderOwnerNode :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | Denotes whether the stylesheet is disabled.
    cSSCSSStyleSheetHeaderDisabled :: Bool,
    -- | Whether the sourceURL field value comes from the sourceURL comment.
    cSSCSSStyleSheetHeaderHasSourceURL :: Maybe Bool,
    -- | Whether this stylesheet is created for STYLE tag by parser. This flag is not set for
    --   document.written STYLE tags.
    cSSCSSStyleSheetHeaderIsInline :: Bool,
    -- | Whether this stylesheet is mutable. Inline stylesheets become mutable
    --   after they have been modified via CSSOM API.
    --   <link> element's stylesheets become mutable only if DevTools modifies them.
    --   Constructed stylesheets (new CSSStyleSheet()) are mutable immediately after creation.
    cSSCSSStyleSheetHeaderIsMutable :: Bool,
    -- | True if this stylesheet is created through new CSSStyleSheet() or imported as a
    --   CSS module script.
    cSSCSSStyleSheetHeaderIsConstructed :: Bool,
    -- | Line offset of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderStartLine :: Double,
    -- | Column offset of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderStartColumn :: Double,
    -- | Size of the content (in characters).
    cSSCSSStyleSheetHeaderLength :: Double,
    -- | Line offset of the end of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderEndLine :: Double,
    -- | Column offset of the end of the stylesheet within the resource (zero based).
    cSSCSSStyleSheetHeaderEndColumn :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSStyleSheetHeader where
  parseJSON = A.withObject "CSSCSSStyleSheetHeader" $ \o -> CSSCSSStyleSheetHeader
    <$> o A..: "styleSheetId"
    <*> o A..: "frameId"
    <*> o A..: "sourceURL"
    <*> o A..:? "sourceMapURL"
    <*> o A..: "origin"
    <*> o A..: "title"
    <*> o A..:? "ownerNode"
    <*> o A..: "disabled"
    <*> o A..:? "hasSourceURL"
    <*> o A..: "isInline"
    <*> o A..: "isMutable"
    <*> o A..: "isConstructed"
    <*> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "length"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
instance ToJSON CSSCSSStyleSheetHeader where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSCSSStyleSheetHeaderStyleSheetId p),
    ("frameId" A..=) <$> Just (cSSCSSStyleSheetHeaderFrameId p),
    ("sourceURL" A..=) <$> Just (cSSCSSStyleSheetHeaderSourceURL p),
    ("sourceMapURL" A..=) <$> (cSSCSSStyleSheetHeaderSourceMapURL p),
    ("origin" A..=) <$> Just (cSSCSSStyleSheetHeaderOrigin p),
    ("title" A..=) <$> Just (cSSCSSStyleSheetHeaderTitle p),
    ("ownerNode" A..=) <$> (cSSCSSStyleSheetHeaderOwnerNode p),
    ("disabled" A..=) <$> Just (cSSCSSStyleSheetHeaderDisabled p),
    ("hasSourceURL" A..=) <$> (cSSCSSStyleSheetHeaderHasSourceURL p),
    ("isInline" A..=) <$> Just (cSSCSSStyleSheetHeaderIsInline p),
    ("isMutable" A..=) <$> Just (cSSCSSStyleSheetHeaderIsMutable p),
    ("isConstructed" A..=) <$> Just (cSSCSSStyleSheetHeaderIsConstructed p),
    ("startLine" A..=) <$> Just (cSSCSSStyleSheetHeaderStartLine p),
    ("startColumn" A..=) <$> Just (cSSCSSStyleSheetHeaderStartColumn p),
    ("length" A..=) <$> Just (cSSCSSStyleSheetHeaderLength p),
    ("endLine" A..=) <$> Just (cSSCSSStyleSheetHeaderEndLine p),
    ("endColumn" A..=) <$> Just (cSSCSSStyleSheetHeaderEndColumn p)
    ]

-- | Type 'CSS.CSSRule'.
--   CSS rule representation.
data CSSCSSRule = CSSCSSRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSRuleStyleSheetId :: Maybe CSSStyleSheetId,
    -- | Rule selector data.
    cSSCSSRuleSelectorList :: CSSSelectorList,
    -- | Parent stylesheet's origin.
    cSSCSSRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated style declaration.
    cSSCSSRuleStyle :: CSSCSSStyle,
    -- | Media list array (for rules involving media queries). The array enumerates media queries
    --   starting with the innermost one, going outwards.
    cSSCSSRuleMedia :: Maybe [CSSCSSMedia],
    -- | Container query list array (for rules involving container queries).
    --   The array enumerates container queries starting with the innermost one, going outwards.
    cSSCSSRuleContainerQueries :: Maybe [CSSCSSContainerQuery],
    -- | @supports CSS at-rule array.
    --   The array enumerates @supports at-rules starting with the innermost one, going outwards.
    cSSCSSRuleSupports :: Maybe [CSSCSSSupports],
    -- | Cascade layer array. Contains the layer hierarchy that this rule belongs to starting
    --   with the innermost layer and going outwards.
    cSSCSSRuleLayers :: Maybe [CSSCSSLayer],
    -- | @scope CSS at-rule array.
    --   The array enumerates @scope at-rules starting with the innermost one, going outwards.
    cSSCSSRuleScopes :: Maybe [CSSCSSScope]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSRule where
  parseJSON = A.withObject "CSSCSSRule" $ \o -> CSSCSSRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "selectorList"
    <*> o A..: "origin"
    <*> o A..: "style"
    <*> o A..:? "media"
    <*> o A..:? "containerQueries"
    <*> o A..:? "supports"
    <*> o A..:? "layers"
    <*> o A..:? "scopes"
instance ToJSON CSSCSSRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSRuleStyleSheetId p),
    ("selectorList" A..=) <$> Just (cSSCSSRuleSelectorList p),
    ("origin" A..=) <$> Just (cSSCSSRuleOrigin p),
    ("style" A..=) <$> Just (cSSCSSRuleStyle p),
    ("media" A..=) <$> (cSSCSSRuleMedia p),
    ("containerQueries" A..=) <$> (cSSCSSRuleContainerQueries p),
    ("supports" A..=) <$> (cSSCSSRuleSupports p),
    ("layers" A..=) <$> (cSSCSSRuleLayers p),
    ("scopes" A..=) <$> (cSSCSSRuleScopes p)
    ]

-- | Type 'CSS.RuleUsage'.
--   CSS coverage information.
data CSSRuleUsage = CSSRuleUsage
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSRuleUsageStyleSheetId :: CSSStyleSheetId,
    -- | Offset of the start of the rule (including selector) from the beginning of the stylesheet.
    cSSRuleUsageStartOffset :: Double,
    -- | Offset of the end of the rule body from the beginning of the stylesheet.
    cSSRuleUsageEndOffset :: Double,
    -- | Indicates whether the rule was actually used by some element in the page.
    cSSRuleUsageUsed :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSRuleUsage where
  parseJSON = A.withObject "CSSRuleUsage" $ \o -> CSSRuleUsage
    <$> o A..: "styleSheetId"
    <*> o A..: "startOffset"
    <*> o A..: "endOffset"
    <*> o A..: "used"
instance ToJSON CSSRuleUsage where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSRuleUsageStyleSheetId p),
    ("startOffset" A..=) <$> Just (cSSRuleUsageStartOffset p),
    ("endOffset" A..=) <$> Just (cSSRuleUsageEndOffset p),
    ("used" A..=) <$> Just (cSSRuleUsageUsed p)
    ]

-- | Type 'CSS.SourceRange'.
--   Text range within a resource. All numbers are zero-based.
data CSSSourceRange = CSSSourceRange
  {
    -- | Start line of range.
    cSSSourceRangeStartLine :: Int,
    -- | Start column of range (inclusive).
    cSSSourceRangeStartColumn :: Int,
    -- | End line of range
    cSSSourceRangeEndLine :: Int,
    -- | End column of range (exclusive).
    cSSSourceRangeEndColumn :: Int
  }
  deriving (Eq, Show)
instance FromJSON CSSSourceRange where
  parseJSON = A.withObject "CSSSourceRange" $ \o -> CSSSourceRange
    <$> o A..: "startLine"
    <*> o A..: "startColumn"
    <*> o A..: "endLine"
    <*> o A..: "endColumn"
instance ToJSON CSSSourceRange where
  toJSON p = A.object $ catMaybes [
    ("startLine" A..=) <$> Just (cSSSourceRangeStartLine p),
    ("startColumn" A..=) <$> Just (cSSSourceRangeStartColumn p),
    ("endLine" A..=) <$> Just (cSSSourceRangeEndLine p),
    ("endColumn" A..=) <$> Just (cSSSourceRangeEndColumn p)
    ]

-- | Type 'CSS.ShorthandEntry'.
data CSSShorthandEntry = CSSShorthandEntry
  {
    -- | Shorthand name.
    cSSShorthandEntryName :: T.Text,
    -- | Shorthand value.
    cSSShorthandEntryValue :: T.Text,
    -- | Whether the property has "!important" annotation (implies `false` if absent).
    cSSShorthandEntryImportant :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSShorthandEntry where
  parseJSON = A.withObject "CSSShorthandEntry" $ \o -> CSSShorthandEntry
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..:? "important"
instance ToJSON CSSShorthandEntry where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSShorthandEntryName p),
    ("value" A..=) <$> Just (cSSShorthandEntryValue p),
    ("important" A..=) <$> (cSSShorthandEntryImportant p)
    ]

-- | Type 'CSS.CSSComputedStyleProperty'.
data CSSCSSComputedStyleProperty = CSSCSSComputedStyleProperty
  {
    -- | Computed style property name.
    cSSCSSComputedStylePropertyName :: T.Text,
    -- | Computed style property value.
    cSSCSSComputedStylePropertyValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSComputedStyleProperty where
  parseJSON = A.withObject "CSSCSSComputedStyleProperty" $ \o -> CSSCSSComputedStyleProperty
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON CSSCSSComputedStyleProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSComputedStylePropertyName p),
    ("value" A..=) <$> Just (cSSCSSComputedStylePropertyValue p)
    ]

-- | Type 'CSS.CSSStyle'.
--   CSS style representation.
data CSSCSSStyle = CSSCSSStyle
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSStyleStyleSheetId :: Maybe CSSStyleSheetId,
    -- | CSS properties in the style.
    cSSCSSStyleCssProperties :: [CSSCSSProperty],
    -- | Computed values for all shorthands found in the style.
    cSSCSSStyleShorthandEntries :: [CSSShorthandEntry],
    -- | Style declaration text (if available).
    cSSCSSStyleCssText :: Maybe T.Text,
    -- | Style declaration range in the enclosing stylesheet (if available).
    cSSCSSStyleRange :: Maybe CSSSourceRange
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSStyle where
  parseJSON = A.withObject "CSSCSSStyle" $ \o -> CSSCSSStyle
    <$> o A..:? "styleSheetId"
    <*> o A..: "cssProperties"
    <*> o A..: "shorthandEntries"
    <*> o A..:? "cssText"
    <*> o A..:? "range"
instance ToJSON CSSCSSStyle where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSStyleStyleSheetId p),
    ("cssProperties" A..=) <$> Just (cSSCSSStyleCssProperties p),
    ("shorthandEntries" A..=) <$> Just (cSSCSSStyleShorthandEntries p),
    ("cssText" A..=) <$> (cSSCSSStyleCssText p),
    ("range" A..=) <$> (cSSCSSStyleRange p)
    ]

-- | Type 'CSS.CSSProperty'.
--   CSS property declaration data.
data CSSCSSProperty = CSSCSSProperty
  {
    -- | The property name.
    cSSCSSPropertyName :: T.Text,
    -- | The property value.
    cSSCSSPropertyValue :: T.Text,
    -- | Whether the property has "!important" annotation (implies `false` if absent).
    cSSCSSPropertyImportant :: Maybe Bool,
    -- | Whether the property is implicit (implies `false` if absent).
    cSSCSSPropertyImplicit :: Maybe Bool,
    -- | The full property text as specified in the style.
    cSSCSSPropertyText :: Maybe T.Text,
    -- | Whether the property is understood by the browser (implies `true` if absent).
    cSSCSSPropertyParsedOk :: Maybe Bool,
    -- | Whether the property is disabled by the user (present for source-based properties only).
    cSSCSSPropertyDisabled :: Maybe Bool,
    -- | The entire property range in the enclosing style declaration (if available).
    cSSCSSPropertyRange :: Maybe CSSSourceRange,
    -- | Parsed longhand components of this property if it is a shorthand.
    --   This field will be empty if the given property is not a shorthand.
    cSSCSSPropertyLonghandProperties :: Maybe [CSSCSSProperty]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSProperty where
  parseJSON = A.withObject "CSSCSSProperty" $ \o -> CSSCSSProperty
    <$> o A..: "name"
    <*> o A..: "value"
    <*> o A..:? "important"
    <*> o A..:? "implicit"
    <*> o A..:? "text"
    <*> o A..:? "parsedOk"
    <*> o A..:? "disabled"
    <*> o A..:? "range"
    <*> o A..:? "longhandProperties"
instance ToJSON CSSCSSProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSPropertyName p),
    ("value" A..=) <$> Just (cSSCSSPropertyValue p),
    ("important" A..=) <$> (cSSCSSPropertyImportant p),
    ("implicit" A..=) <$> (cSSCSSPropertyImplicit p),
    ("text" A..=) <$> (cSSCSSPropertyText p),
    ("parsedOk" A..=) <$> (cSSCSSPropertyParsedOk p),
    ("disabled" A..=) <$> (cSSCSSPropertyDisabled p),
    ("range" A..=) <$> (cSSCSSPropertyRange p),
    ("longhandProperties" A..=) <$> (cSSCSSPropertyLonghandProperties p)
    ]

-- | Type 'CSS.CSSMedia'.
--   CSS media rule descriptor.
data CSSCSSMediaSource = CSSCSSMediaSourceMediaRule | CSSCSSMediaSourceImportRule | CSSCSSMediaSourceLinkedSheet | CSSCSSMediaSourceInlineSheet
  deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSMediaSource where
  parseJSON = A.withText "CSSCSSMediaSource" $ \v -> case v of
    "mediaRule" -> pure CSSCSSMediaSourceMediaRule
    "importRule" -> pure CSSCSSMediaSourceImportRule
    "linkedSheet" -> pure CSSCSSMediaSourceLinkedSheet
    "inlineSheet" -> pure CSSCSSMediaSourceInlineSheet
    "_" -> fail "failed to parse CSSCSSMediaSource"
instance ToJSON CSSCSSMediaSource where
  toJSON v = A.String $ case v of
    CSSCSSMediaSourceMediaRule -> "mediaRule"
    CSSCSSMediaSourceImportRule -> "importRule"
    CSSCSSMediaSourceLinkedSheet -> "linkedSheet"
    CSSCSSMediaSourceInlineSheet -> "inlineSheet"
data CSSCSSMedia = CSSCSSMedia
  {
    -- | Media query text.
    cSSCSSMediaText :: T.Text,
    -- | Source of the media query: "mediaRule" if specified by a @media rule, "importRule" if
    --   specified by an @import rule, "linkedSheet" if specified by a "media" attribute in a linked
    --   stylesheet's LINK tag, "inlineSheet" if specified by a "media" attribute in an inline
    --   stylesheet's STYLE tag.
    cSSCSSMediaSource :: CSSCSSMediaSource,
    -- | URL of the document containing the media query description.
    cSSCSSMediaSourceURL :: Maybe T.Text,
    -- | The associated rule (@media or @import) header range in the enclosing stylesheet (if
    --   available).
    cSSCSSMediaRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSMediaStyleSheetId :: Maybe CSSStyleSheetId,
    -- | Array of media queries.
    cSSCSSMediaMediaList :: Maybe [CSSMediaQuery]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSMedia where
  parseJSON = A.withObject "CSSCSSMedia" $ \o -> CSSCSSMedia
    <$> o A..: "text"
    <*> o A..: "source"
    <*> o A..:? "sourceURL"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
    <*> o A..:? "mediaList"
instance ToJSON CSSCSSMedia where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSMediaText p),
    ("source" A..=) <$> Just (cSSCSSMediaSource p),
    ("sourceURL" A..=) <$> (cSSCSSMediaSourceURL p),
    ("range" A..=) <$> (cSSCSSMediaRange p),
    ("styleSheetId" A..=) <$> (cSSCSSMediaStyleSheetId p),
    ("mediaList" A..=) <$> (cSSCSSMediaMediaList p)
    ]

-- | Type 'CSS.MediaQuery'.
--   Media query descriptor.
data CSSMediaQuery = CSSMediaQuery
  {
    -- | Array of media query expressions.
    cSSMediaQueryExpressions :: [CSSMediaQueryExpression],
    -- | Whether the media query condition is satisfied.
    cSSMediaQueryActive :: Bool
  }
  deriving (Eq, Show)
instance FromJSON CSSMediaQuery where
  parseJSON = A.withObject "CSSMediaQuery" $ \o -> CSSMediaQuery
    <$> o A..: "expressions"
    <*> o A..: "active"
instance ToJSON CSSMediaQuery where
  toJSON p = A.object $ catMaybes [
    ("expressions" A..=) <$> Just (cSSMediaQueryExpressions p),
    ("active" A..=) <$> Just (cSSMediaQueryActive p)
    ]

-- | Type 'CSS.MediaQueryExpression'.
--   Media query expression descriptor.
data CSSMediaQueryExpression = CSSMediaQueryExpression
  {
    -- | Media query expression value.
    cSSMediaQueryExpressionValue :: Double,
    -- | Media query expression units.
    cSSMediaQueryExpressionUnit :: T.Text,
    -- | Media query expression feature.
    cSSMediaQueryExpressionFeature :: T.Text,
    -- | The associated range of the value text in the enclosing stylesheet (if available).
    cSSMediaQueryExpressionValueRange :: Maybe CSSSourceRange,
    -- | Computed length of media query expression (if applicable).
    cSSMediaQueryExpressionComputedLength :: Maybe Double
  }
  deriving (Eq, Show)
instance FromJSON CSSMediaQueryExpression where
  parseJSON = A.withObject "CSSMediaQueryExpression" $ \o -> CSSMediaQueryExpression
    <$> o A..: "value"
    <*> o A..: "unit"
    <*> o A..: "feature"
    <*> o A..:? "valueRange"
    <*> o A..:? "computedLength"
instance ToJSON CSSMediaQueryExpression where
  toJSON p = A.object $ catMaybes [
    ("value" A..=) <$> Just (cSSMediaQueryExpressionValue p),
    ("unit" A..=) <$> Just (cSSMediaQueryExpressionUnit p),
    ("feature" A..=) <$> Just (cSSMediaQueryExpressionFeature p),
    ("valueRange" A..=) <$> (cSSMediaQueryExpressionValueRange p),
    ("computedLength" A..=) <$> (cSSMediaQueryExpressionComputedLength p)
    ]

-- | Type 'CSS.CSSContainerQuery'.
--   CSS container query rule descriptor.
data CSSCSSContainerQuery = CSSCSSContainerQuery
  {
    -- | Container query text.
    cSSCSSContainerQueryText :: T.Text,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSContainerQueryRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSContainerQueryStyleSheetId :: Maybe CSSStyleSheetId,
    -- | Optional name for the container.
    cSSCSSContainerQueryName :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSContainerQuery where
  parseJSON = A.withObject "CSSCSSContainerQuery" $ \o -> CSSCSSContainerQuery
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
    <*> o A..:? "name"
instance ToJSON CSSCSSContainerQuery where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSContainerQueryText p),
    ("range" A..=) <$> (cSSCSSContainerQueryRange p),
    ("styleSheetId" A..=) <$> (cSSCSSContainerQueryStyleSheetId p),
    ("name" A..=) <$> (cSSCSSContainerQueryName p)
    ]

-- | Type 'CSS.CSSSupports'.
--   CSS Supports at-rule descriptor.
data CSSCSSSupports = CSSCSSSupports
  {
    -- | Supports rule text.
    cSSCSSSupportsText :: T.Text,
    -- | Whether the supports condition is satisfied.
    cSSCSSSupportsActive :: Bool,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSSupportsRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSSupportsStyleSheetId :: Maybe CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSSupports where
  parseJSON = A.withObject "CSSCSSSupports" $ \o -> CSSCSSSupports
    <$> o A..: "text"
    <*> o A..: "active"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSSupports where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSSupportsText p),
    ("active" A..=) <$> Just (cSSCSSSupportsActive p),
    ("range" A..=) <$> (cSSCSSSupportsRange p),
    ("styleSheetId" A..=) <$> (cSSCSSSupportsStyleSheetId p)
    ]

-- | Type 'CSS.CSSScope'.
--   CSS Scope at-rule descriptor.
data CSSCSSScope = CSSCSSScope
  {
    -- | Scope rule text.
    cSSCSSScopeText :: T.Text,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSScopeRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSScopeStyleSheetId :: Maybe CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSScope where
  parseJSON = A.withObject "CSSCSSScope" $ \o -> CSSCSSScope
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSScope where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSScopeText p),
    ("range" A..=) <$> (cSSCSSScopeRange p),
    ("styleSheetId" A..=) <$> (cSSCSSScopeStyleSheetId p)
    ]

-- | Type 'CSS.CSSLayer'.
--   CSS Layer at-rule descriptor.
data CSSCSSLayer = CSSCSSLayer
  {
    -- | Layer name.
    cSSCSSLayerText :: T.Text,
    -- | The associated rule header range in the enclosing stylesheet (if
    --   available).
    cSSCSSLayerRange :: Maybe CSSSourceRange,
    -- | Identifier of the stylesheet containing this object (if exists).
    cSSCSSLayerStyleSheetId :: Maybe CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSLayer where
  parseJSON = A.withObject "CSSCSSLayer" $ \o -> CSSCSSLayer
    <$> o A..: "text"
    <*> o A..:? "range"
    <*> o A..:? "styleSheetId"
instance ToJSON CSSCSSLayer where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (cSSCSSLayerText p),
    ("range" A..=) <$> (cSSCSSLayerRange p),
    ("styleSheetId" A..=) <$> (cSSCSSLayerStyleSheetId p)
    ]

-- | Type 'CSS.CSSLayerData'.
--   CSS Layer data.
data CSSCSSLayerData = CSSCSSLayerData
  {
    -- | Layer name.
    cSSCSSLayerDataName :: T.Text,
    -- | Direct sub-layers
    cSSCSSLayerDataSubLayers :: Maybe [CSSCSSLayerData],
    -- | Layer order. The order determines the order of the layer in the cascade order.
    --   A higher number has higher priority in the cascade order.
    cSSCSSLayerDataOrder :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSLayerData where
  parseJSON = A.withObject "CSSCSSLayerData" $ \o -> CSSCSSLayerData
    <$> o A..: "name"
    <*> o A..:? "subLayers"
    <*> o A..: "order"
instance ToJSON CSSCSSLayerData where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (cSSCSSLayerDataName p),
    ("subLayers" A..=) <$> (cSSCSSLayerDataSubLayers p),
    ("order" A..=) <$> Just (cSSCSSLayerDataOrder p)
    ]

-- | Type 'CSS.PlatformFontUsage'.
--   Information about amount of glyphs that were rendered with given font.
data CSSPlatformFontUsage = CSSPlatformFontUsage
  {
    -- | Font's family name reported by platform.
    cSSPlatformFontUsageFamilyName :: T.Text,
    -- | Indicates if the font was downloaded or resolved locally.
    cSSPlatformFontUsageIsCustomFont :: Bool,
    -- | Amount of glyphs that were rendered with this font.
    cSSPlatformFontUsageGlyphCount :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSPlatformFontUsage where
  parseJSON = A.withObject "CSSPlatformFontUsage" $ \o -> CSSPlatformFontUsage
    <$> o A..: "familyName"
    <*> o A..: "isCustomFont"
    <*> o A..: "glyphCount"
instance ToJSON CSSPlatformFontUsage where
  toJSON p = A.object $ catMaybes [
    ("familyName" A..=) <$> Just (cSSPlatformFontUsageFamilyName p),
    ("isCustomFont" A..=) <$> Just (cSSPlatformFontUsageIsCustomFont p),
    ("glyphCount" A..=) <$> Just (cSSPlatformFontUsageGlyphCount p)
    ]

-- | Type 'CSS.FontVariationAxis'.
--   Information about font variation axes for variable fonts
data CSSFontVariationAxis = CSSFontVariationAxis
  {
    -- | The font-variation-setting tag (a.k.a. "axis tag").
    cSSFontVariationAxisTag :: T.Text,
    -- | Human-readable variation name in the default language (normally, "en").
    cSSFontVariationAxisName :: T.Text,
    -- | The minimum value (inclusive) the font supports for this tag.
    cSSFontVariationAxisMinValue :: Double,
    -- | The maximum value (inclusive) the font supports for this tag.
    cSSFontVariationAxisMaxValue :: Double,
    -- | The default value.
    cSSFontVariationAxisDefaultValue :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSFontVariationAxis where
  parseJSON = A.withObject "CSSFontVariationAxis" $ \o -> CSSFontVariationAxis
    <$> o A..: "tag"
    <*> o A..: "name"
    <*> o A..: "minValue"
    <*> o A..: "maxValue"
    <*> o A..: "defaultValue"
instance ToJSON CSSFontVariationAxis where
  toJSON p = A.object $ catMaybes [
    ("tag" A..=) <$> Just (cSSFontVariationAxisTag p),
    ("name" A..=) <$> Just (cSSFontVariationAxisName p),
    ("minValue" A..=) <$> Just (cSSFontVariationAxisMinValue p),
    ("maxValue" A..=) <$> Just (cSSFontVariationAxisMaxValue p),
    ("defaultValue" A..=) <$> Just (cSSFontVariationAxisDefaultValue p)
    ]

-- | Type 'CSS.FontFace'.
--   Properties of a web font: https://www.w3.org/TR/2008/REC-CSS2-20080411/fonts.html#font-descriptions
--   and additional information such as platformFontFamily and fontVariationAxes.
data CSSFontFace = CSSFontFace
  {
    -- | The font-family.
    cSSFontFaceFontFamily :: T.Text,
    -- | The font-style.
    cSSFontFaceFontStyle :: T.Text,
    -- | The font-variant.
    cSSFontFaceFontVariant :: T.Text,
    -- | The font-weight.
    cSSFontFaceFontWeight :: T.Text,
    -- | The font-stretch.
    cSSFontFaceFontStretch :: T.Text,
    -- | The font-display.
    cSSFontFaceFontDisplay :: T.Text,
    -- | The unicode-range.
    cSSFontFaceUnicodeRange :: T.Text,
    -- | The src.
    cSSFontFaceSrc :: T.Text,
    -- | The resolved platform font family
    cSSFontFacePlatformFontFamily :: T.Text,
    -- | Available variation settings (a.k.a. "axes").
    cSSFontFaceFontVariationAxes :: Maybe [CSSFontVariationAxis]
  }
  deriving (Eq, Show)
instance FromJSON CSSFontFace where
  parseJSON = A.withObject "CSSFontFace" $ \o -> CSSFontFace
    <$> o A..: "fontFamily"
    <*> o A..: "fontStyle"
    <*> o A..: "fontVariant"
    <*> o A..: "fontWeight"
    <*> o A..: "fontStretch"
    <*> o A..: "fontDisplay"
    <*> o A..: "unicodeRange"
    <*> o A..: "src"
    <*> o A..: "platformFontFamily"
    <*> o A..:? "fontVariationAxes"
instance ToJSON CSSFontFace where
  toJSON p = A.object $ catMaybes [
    ("fontFamily" A..=) <$> Just (cSSFontFaceFontFamily p),
    ("fontStyle" A..=) <$> Just (cSSFontFaceFontStyle p),
    ("fontVariant" A..=) <$> Just (cSSFontFaceFontVariant p),
    ("fontWeight" A..=) <$> Just (cSSFontFaceFontWeight p),
    ("fontStretch" A..=) <$> Just (cSSFontFaceFontStretch p),
    ("fontDisplay" A..=) <$> Just (cSSFontFaceFontDisplay p),
    ("unicodeRange" A..=) <$> Just (cSSFontFaceUnicodeRange p),
    ("src" A..=) <$> Just (cSSFontFaceSrc p),
    ("platformFontFamily" A..=) <$> Just (cSSFontFacePlatformFontFamily p),
    ("fontVariationAxes" A..=) <$> (cSSFontFaceFontVariationAxes p)
    ]

-- | Type 'CSS.CSSKeyframesRule'.
--   CSS keyframes rule representation.
data CSSCSSKeyframesRule = CSSCSSKeyframesRule
  {
    -- | Animation name.
    cSSCSSKeyframesRuleAnimationName :: CSSValue,
    -- | List of keyframes.
    cSSCSSKeyframesRuleKeyframes :: [CSSCSSKeyframeRule]
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSKeyframesRule where
  parseJSON = A.withObject "CSSCSSKeyframesRule" $ \o -> CSSCSSKeyframesRule
    <$> o A..: "animationName"
    <*> o A..: "keyframes"
instance ToJSON CSSCSSKeyframesRule where
  toJSON p = A.object $ catMaybes [
    ("animationName" A..=) <$> Just (cSSCSSKeyframesRuleAnimationName p),
    ("keyframes" A..=) <$> Just (cSSCSSKeyframesRuleKeyframes p)
    ]

-- | Type 'CSS.CSSKeyframeRule'.
--   CSS keyframe rule representation.
data CSSCSSKeyframeRule = CSSCSSKeyframeRule
  {
    -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    --   stylesheet rules) this rule came from.
    cSSCSSKeyframeRuleStyleSheetId :: Maybe CSSStyleSheetId,
    -- | Parent stylesheet's origin.
    cSSCSSKeyframeRuleOrigin :: CSSStyleSheetOrigin,
    -- | Associated key text.
    cSSCSSKeyframeRuleKeyText :: CSSValue,
    -- | Associated style declaration.
    cSSCSSKeyframeRuleStyle :: CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSCSSKeyframeRule where
  parseJSON = A.withObject "CSSCSSKeyframeRule" $ \o -> CSSCSSKeyframeRule
    <$> o A..:? "styleSheetId"
    <*> o A..: "origin"
    <*> o A..: "keyText"
    <*> o A..: "style"
instance ToJSON CSSCSSKeyframeRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> (cSSCSSKeyframeRuleStyleSheetId p),
    ("origin" A..=) <$> Just (cSSCSSKeyframeRuleOrigin p),
    ("keyText" A..=) <$> Just (cSSCSSKeyframeRuleKeyText p),
    ("style" A..=) <$> Just (cSSCSSKeyframeRuleStyle p)
    ]

-- | Type 'CSS.StyleDeclarationEdit'.
--   A descriptor of operation to mutate style declaration text.
data CSSStyleDeclarationEdit = CSSStyleDeclarationEdit
  {
    -- | The css style sheet identifier.
    cSSStyleDeclarationEditStyleSheetId :: CSSStyleSheetId,
    -- | The range of the style text in the enclosing stylesheet.
    cSSStyleDeclarationEditRange :: CSSSourceRange,
    -- | New style text.
    cSSStyleDeclarationEditText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleDeclarationEdit where
  parseJSON = A.withObject "CSSStyleDeclarationEdit" $ \o -> CSSStyleDeclarationEdit
    <$> o A..: "styleSheetId"
    <*> o A..: "range"
    <*> o A..: "text"
instance ToJSON CSSStyleDeclarationEdit where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (cSSStyleDeclarationEditStyleSheetId p),
    ("range" A..=) <$> Just (cSSStyleDeclarationEditRange p),
    ("text" A..=) <$> Just (cSSStyleDeclarationEditText p)
    ]

-- | Type of the 'CSS.fontsUpdated' event.
data CSSFontsUpdated = CSSFontsUpdated
  {
    -- | The web font that has loaded.
    cSSFontsUpdatedFont :: Maybe CSSFontFace
  }
  deriving (Eq, Show)
instance FromJSON CSSFontsUpdated where
  parseJSON = A.withObject "CSSFontsUpdated" $ \o -> CSSFontsUpdated
    <$> o A..:? "font"
instance Event CSSFontsUpdated where
  eventName _ = "CSS.fontsUpdated"

-- | Type of the 'CSS.mediaQueryResultChanged' event.
data CSSMediaQueryResultChanged = CSSMediaQueryResultChanged
  deriving (Eq, Show, Read)
instance FromJSON CSSMediaQueryResultChanged where
  parseJSON _ = pure CSSMediaQueryResultChanged
instance Event CSSMediaQueryResultChanged where
  eventName _ = "CSS.mediaQueryResultChanged"

-- | Type of the 'CSS.styleSheetAdded' event.
data CSSStyleSheetAdded = CSSStyleSheetAdded
  {
    -- | Added stylesheet metainfo.
    cSSStyleSheetAddedHeader :: CSSCSSStyleSheetHeader
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetAdded where
  parseJSON = A.withObject "CSSStyleSheetAdded" $ \o -> CSSStyleSheetAdded
    <$> o A..: "header"
instance Event CSSStyleSheetAdded where
  eventName _ = "CSS.styleSheetAdded"

-- | Type of the 'CSS.styleSheetChanged' event.
data CSSStyleSheetChanged = CSSStyleSheetChanged
  {
    cSSStyleSheetChangedStyleSheetId :: CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetChanged where
  parseJSON = A.withObject "CSSStyleSheetChanged" $ \o -> CSSStyleSheetChanged
    <$> o A..: "styleSheetId"
instance Event CSSStyleSheetChanged where
  eventName _ = "CSS.styleSheetChanged"

-- | Type of the 'CSS.styleSheetRemoved' event.
data CSSStyleSheetRemoved = CSSStyleSheetRemoved
  {
    -- | Identifier of the removed stylesheet.
    cSSStyleSheetRemovedStyleSheetId :: CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSStyleSheetRemoved where
  parseJSON = A.withObject "CSSStyleSheetRemoved" $ \o -> CSSStyleSheetRemoved
    <$> o A..: "styleSheetId"
instance Event CSSStyleSheetRemoved where
  eventName _ = "CSS.styleSheetRemoved"

-- | Inserts a new rule with the given `ruleText` in a stylesheet with given `styleSheetId`, at the
--   position specified by `location`.

-- | Parameters of the 'CSS.addRule' command.
data PCSSAddRule = PCSSAddRule
  {
    -- | The css style sheet identifier where a new rule should be inserted.
    pCSSAddRuleStyleSheetId :: CSSStyleSheetId,
    -- | The text of a new rule.
    pCSSAddRuleRuleText :: T.Text,
    -- | Text position of a new rule in the target style sheet.
    pCSSAddRuleLocation :: CSSSourceRange
  }
  deriving (Eq, Show)
pCSSAddRule
  -- | The css style sheet identifier where a new rule should be inserted.
  :: CSSStyleSheetId
  -- | The text of a new rule.
  -> T.Text
  -- | Text position of a new rule in the target style sheet.
  -> CSSSourceRange
  -> PCSSAddRule
pCSSAddRule
  arg_pCSSAddRuleStyleSheetId
  arg_pCSSAddRuleRuleText
  arg_pCSSAddRuleLocation
  = PCSSAddRule
    arg_pCSSAddRuleStyleSheetId
    arg_pCSSAddRuleRuleText
    arg_pCSSAddRuleLocation
instance ToJSON PCSSAddRule where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSAddRuleStyleSheetId p),
    ("ruleText" A..=) <$> Just (pCSSAddRuleRuleText p),
    ("location" A..=) <$> Just (pCSSAddRuleLocation p)
    ]
data CSSAddRule = CSSAddRule
  {
    -- | The newly created rule.
    cSSAddRuleRule :: CSSCSSRule
  }
  deriving (Eq, Show)
instance FromJSON CSSAddRule where
  parseJSON = A.withObject "CSSAddRule" $ \o -> CSSAddRule
    <$> o A..: "rule"
instance Command PCSSAddRule where
  type CommandResponse PCSSAddRule = CSSAddRule
  commandName _ = "CSS.addRule"

-- | Returns all class names from specified stylesheet.

-- | Parameters of the 'CSS.collectClassNames' command.
data PCSSCollectClassNames = PCSSCollectClassNames
  {
    pCSSCollectClassNamesStyleSheetId :: CSSStyleSheetId
  }
  deriving (Eq, Show)
pCSSCollectClassNames
  :: CSSStyleSheetId
  -> PCSSCollectClassNames
pCSSCollectClassNames
  arg_pCSSCollectClassNamesStyleSheetId
  = PCSSCollectClassNames
    arg_pCSSCollectClassNamesStyleSheetId
instance ToJSON PCSSCollectClassNames where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSCollectClassNamesStyleSheetId p)
    ]
data CSSCollectClassNames = CSSCollectClassNames
  {
    -- | Class name list.
    cSSCollectClassNamesClassNames :: [T.Text]
  }
  deriving (Eq, Show)
instance FromJSON CSSCollectClassNames where
  parseJSON = A.withObject "CSSCollectClassNames" $ \o -> CSSCollectClassNames
    <$> o A..: "classNames"
instance Command PCSSCollectClassNames where
  type CommandResponse PCSSCollectClassNames = CSSCollectClassNames
  commandName _ = "CSS.collectClassNames"

-- | Creates a new special "via-inspector" stylesheet in the frame with given `frameId`.

-- | Parameters of the 'CSS.createStyleSheet' command.
data PCSSCreateStyleSheet = PCSSCreateStyleSheet
  {
    -- | Identifier of the frame where "via-inspector" stylesheet should be created.
    pCSSCreateStyleSheetFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
  }
  deriving (Eq, Show)
pCSSCreateStyleSheet
  -- | Identifier of the frame where "via-inspector" stylesheet should be created.
  :: DOMPageNetworkEmulationSecurity.PageFrameId
  -> PCSSCreateStyleSheet
pCSSCreateStyleSheet
  arg_pCSSCreateStyleSheetFrameId
  = PCSSCreateStyleSheet
    arg_pCSSCreateStyleSheetFrameId
instance ToJSON PCSSCreateStyleSheet where
  toJSON p = A.object $ catMaybes [
    ("frameId" A..=) <$> Just (pCSSCreateStyleSheetFrameId p)
    ]
data CSSCreateStyleSheet = CSSCreateStyleSheet
  {
    -- | Identifier of the created "via-inspector" stylesheet.
    cSSCreateStyleSheetStyleSheetId :: CSSStyleSheetId
  }
  deriving (Eq, Show)
instance FromJSON CSSCreateStyleSheet where
  parseJSON = A.withObject "CSSCreateStyleSheet" $ \o -> CSSCreateStyleSheet
    <$> o A..: "styleSheetId"
instance Command PCSSCreateStyleSheet where
  type CommandResponse PCSSCreateStyleSheet = CSSCreateStyleSheet
  commandName _ = "CSS.createStyleSheet"

-- | Disables the CSS agent for the given page.

-- | Parameters of the 'CSS.disable' command.
data PCSSDisable = PCSSDisable
  deriving (Eq, Show)
pCSSDisable
  :: PCSSDisable
pCSSDisable
  = PCSSDisable
instance ToJSON PCSSDisable where
  toJSON _ = A.Null
instance Command PCSSDisable where
  type CommandResponse PCSSDisable = ()
  commandName _ = "CSS.disable"
  fromJSON = const . A.Success . const ()

-- | Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
--   enabled until the result of this command is received.

-- | Parameters of the 'CSS.enable' command.
data PCSSEnable = PCSSEnable
  deriving (Eq, Show)
pCSSEnable
  :: PCSSEnable
pCSSEnable
  = PCSSEnable
instance ToJSON PCSSEnable where
  toJSON _ = A.Null
instance Command PCSSEnable where
  type CommandResponse PCSSEnable = ()
  commandName _ = "CSS.enable"
  fromJSON = const . A.Success . const ()

-- | Ensures that the given node will have specified pseudo-classes whenever its style is computed by
--   the browser.

-- | Parameters of the 'CSS.forcePseudoState' command.
data PCSSForcePseudoState = PCSSForcePseudoState
  {
    -- | The element id for which to force the pseudo state.
    pCSSForcePseudoStateNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Element pseudo classes to force when computing the element's style.
    pCSSForcePseudoStateForcedPseudoClasses :: [T.Text]
  }
  deriving (Eq, Show)
pCSSForcePseudoState
  -- | The element id for which to force the pseudo state.
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -- | Element pseudo classes to force when computing the element's style.
  -> [T.Text]
  -> PCSSForcePseudoState
pCSSForcePseudoState
  arg_pCSSForcePseudoStateNodeId
  arg_pCSSForcePseudoStateForcedPseudoClasses
  = PCSSForcePseudoState
    arg_pCSSForcePseudoStateNodeId
    arg_pCSSForcePseudoStateForcedPseudoClasses
instance ToJSON PCSSForcePseudoState where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSForcePseudoStateNodeId p),
    ("forcedPseudoClasses" A..=) <$> Just (pCSSForcePseudoStateForcedPseudoClasses p)
    ]
instance Command PCSSForcePseudoState where
  type CommandResponse PCSSForcePseudoState = ()
  commandName _ = "CSS.forcePseudoState"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'CSS.getBackgroundColors' command.
data PCSSGetBackgroundColors = PCSSGetBackgroundColors
  {
    -- | Id of the node to get background colors for.
    pCSSGetBackgroundColorsNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetBackgroundColors
  -- | Id of the node to get background colors for.
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetBackgroundColors
pCSSGetBackgroundColors
  arg_pCSSGetBackgroundColorsNodeId
  = PCSSGetBackgroundColors
    arg_pCSSGetBackgroundColorsNodeId
instance ToJSON PCSSGetBackgroundColors where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetBackgroundColorsNodeId p)
    ]
data CSSGetBackgroundColors = CSSGetBackgroundColors
  {
    -- | The range of background colors behind this element, if it contains any visible text. If no
    --   visible text is present, this will be undefined. In the case of a flat background color,
    --   this will consist of simply that color. In the case of a gradient, this will consist of each
    --   of the color stops. For anything more complicated, this will be an empty array. Images will
    --   be ignored (as if the image had failed to load).
    cSSGetBackgroundColorsBackgroundColors :: Maybe [T.Text],
    -- | The computed font size for this node, as a CSS computed value string (e.g. '12px').
    cSSGetBackgroundColorsComputedFontSize :: Maybe T.Text,
    -- | The computed font weight for this node, as a CSS computed value string (e.g. 'normal' or
    --   '100').
    cSSGetBackgroundColorsComputedFontWeight :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSGetBackgroundColors where
  parseJSON = A.withObject "CSSGetBackgroundColors" $ \o -> CSSGetBackgroundColors
    <$> o A..:? "backgroundColors"
    <*> o A..:? "computedFontSize"
    <*> o A..:? "computedFontWeight"
instance Command PCSSGetBackgroundColors where
  type CommandResponse PCSSGetBackgroundColors = CSSGetBackgroundColors
  commandName _ = "CSS.getBackgroundColors"

-- | Returns the computed style for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getComputedStyleForNode' command.
data PCSSGetComputedStyleForNode = PCSSGetComputedStyleForNode
  {
    pCSSGetComputedStyleForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetComputedStyleForNode
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetComputedStyleForNode
pCSSGetComputedStyleForNode
  arg_pCSSGetComputedStyleForNodeNodeId
  = PCSSGetComputedStyleForNode
    arg_pCSSGetComputedStyleForNodeNodeId
instance ToJSON PCSSGetComputedStyleForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetComputedStyleForNodeNodeId p)
    ]
data CSSGetComputedStyleForNode = CSSGetComputedStyleForNode
  {
    -- | Computed style for the specified DOM node.
    cSSGetComputedStyleForNodeComputedStyle :: [CSSCSSComputedStyleProperty]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetComputedStyleForNode where
  parseJSON = A.withObject "CSSGetComputedStyleForNode" $ \o -> CSSGetComputedStyleForNode
    <$> o A..: "computedStyle"
instance Command PCSSGetComputedStyleForNode where
  type CommandResponse PCSSGetComputedStyleForNode = CSSGetComputedStyleForNode
  commandName _ = "CSS.getComputedStyleForNode"

-- | Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
--   attributes) for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getInlineStylesForNode' command.
data PCSSGetInlineStylesForNode = PCSSGetInlineStylesForNode
  {
    pCSSGetInlineStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetInlineStylesForNode
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetInlineStylesForNode
pCSSGetInlineStylesForNode
  arg_pCSSGetInlineStylesForNodeNodeId
  = PCSSGetInlineStylesForNode
    arg_pCSSGetInlineStylesForNodeNodeId
instance ToJSON PCSSGetInlineStylesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetInlineStylesForNodeNodeId p)
    ]
data CSSGetInlineStylesForNode = CSSGetInlineStylesForNode
  {
    -- | Inline style for the specified DOM node.
    cSSGetInlineStylesForNodeInlineStyle :: Maybe CSSCSSStyle,
    -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    cSSGetInlineStylesForNodeAttributesStyle :: Maybe CSSCSSStyle
  }
  deriving (Eq, Show)
instance FromJSON CSSGetInlineStylesForNode where
  parseJSON = A.withObject "CSSGetInlineStylesForNode" $ \o -> CSSGetInlineStylesForNode
    <$> o A..:? "inlineStyle"
    <*> o A..:? "attributesStyle"
instance Command PCSSGetInlineStylesForNode where
  type CommandResponse PCSSGetInlineStylesForNode = CSSGetInlineStylesForNode
  commandName _ = "CSS.getInlineStylesForNode"

-- | Returns requested styles for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getMatchedStylesForNode' command.
data PCSSGetMatchedStylesForNode = PCSSGetMatchedStylesForNode
  {
    pCSSGetMatchedStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetMatchedStylesForNode
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetMatchedStylesForNode
pCSSGetMatchedStylesForNode
  arg_pCSSGetMatchedStylesForNodeNodeId
  = PCSSGetMatchedStylesForNode
    arg_pCSSGetMatchedStylesForNodeNodeId
instance ToJSON PCSSGetMatchedStylesForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetMatchedStylesForNodeNodeId p)
    ]
data CSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode
  {
    -- | Inline style for the specified DOM node.
    cSSGetMatchedStylesForNodeInlineStyle :: Maybe CSSCSSStyle,
    -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
    cSSGetMatchedStylesForNodeAttributesStyle :: Maybe CSSCSSStyle,
    -- | CSS rules matching this node, from all applicable stylesheets.
    cSSGetMatchedStylesForNodeMatchedCSSRules :: Maybe [CSSRuleMatch],
    -- | Pseudo style matches for this node.
    cSSGetMatchedStylesForNodePseudoElements :: Maybe [CSSPseudoElementMatches],
    -- | A chain of inherited styles (from the immediate node parent up to the DOM tree root).
    cSSGetMatchedStylesForNodeInherited :: Maybe [CSSInheritedStyleEntry],
    -- | A chain of inherited pseudo element styles (from the immediate node parent up to the DOM tree root).
    cSSGetMatchedStylesForNodeInheritedPseudoElements :: Maybe [CSSInheritedPseudoElementMatches],
    -- | A list of CSS keyframed animations matching this node.
    cSSGetMatchedStylesForNodeCssKeyframesRules :: Maybe [CSSCSSKeyframesRule],
    -- | Id of the first parent element that does not have display: contents.
    cSSGetMatchedStylesForNodeParentLayoutNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON CSSGetMatchedStylesForNode where
  parseJSON = A.withObject "CSSGetMatchedStylesForNode" $ \o -> CSSGetMatchedStylesForNode
    <$> o A..:? "inlineStyle"
    <*> o A..:? "attributesStyle"
    <*> o A..:? "matchedCSSRules"
    <*> o A..:? "pseudoElements"
    <*> o A..:? "inherited"
    <*> o A..:? "inheritedPseudoElements"
    <*> o A..:? "cssKeyframesRules"
    <*> o A..:? "parentLayoutNodeId"
instance Command PCSSGetMatchedStylesForNode where
  type CommandResponse PCSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode
  commandName _ = "CSS.getMatchedStylesForNode"

-- | Returns all media queries parsed by the rendering engine.

-- | Parameters of the 'CSS.getMediaQueries' command.
data PCSSGetMediaQueries = PCSSGetMediaQueries
  deriving (Eq, Show)
pCSSGetMediaQueries
  :: PCSSGetMediaQueries
pCSSGetMediaQueries
  = PCSSGetMediaQueries
instance ToJSON PCSSGetMediaQueries where
  toJSON _ = A.Null
data CSSGetMediaQueries = CSSGetMediaQueries
  {
    cSSGetMediaQueriesMedias :: [CSSCSSMedia]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetMediaQueries where
  parseJSON = A.withObject "CSSGetMediaQueries" $ \o -> CSSGetMediaQueries
    <$> o A..: "medias"
instance Command PCSSGetMediaQueries where
  type CommandResponse PCSSGetMediaQueries = CSSGetMediaQueries
  commandName _ = "CSS.getMediaQueries"

-- | Requests information about platform fonts which we used to render child TextNodes in the given
--   node.

-- | Parameters of the 'CSS.getPlatformFontsForNode' command.
data PCSSGetPlatformFontsForNode = PCSSGetPlatformFontsForNode
  {
    pCSSGetPlatformFontsForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetPlatformFontsForNode
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetPlatformFontsForNode
pCSSGetPlatformFontsForNode
  arg_pCSSGetPlatformFontsForNodeNodeId
  = PCSSGetPlatformFontsForNode
    arg_pCSSGetPlatformFontsForNodeNodeId
instance ToJSON PCSSGetPlatformFontsForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetPlatformFontsForNodeNodeId p)
    ]
data CSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode
  {
    -- | Usage statistics for every employed platform font.
    cSSGetPlatformFontsForNodeFonts :: [CSSPlatformFontUsage]
  }
  deriving (Eq, Show)
instance FromJSON CSSGetPlatformFontsForNode where
  parseJSON = A.withObject "CSSGetPlatformFontsForNode" $ \o -> CSSGetPlatformFontsForNode
    <$> o A..: "fonts"
instance Command PCSSGetPlatformFontsForNode where
  type CommandResponse PCSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode
  commandName _ = "CSS.getPlatformFontsForNode"

-- | Returns the current textual content for a stylesheet.

-- | Parameters of the 'CSS.getStyleSheetText' command.
data PCSSGetStyleSheetText = PCSSGetStyleSheetText
  {
    pCSSGetStyleSheetTextStyleSheetId :: CSSStyleSheetId
  }
  deriving (Eq, Show)
pCSSGetStyleSheetText
  :: CSSStyleSheetId
  -> PCSSGetStyleSheetText
pCSSGetStyleSheetText
  arg_pCSSGetStyleSheetTextStyleSheetId
  = PCSSGetStyleSheetText
    arg_pCSSGetStyleSheetTextStyleSheetId
instance ToJSON PCSSGetStyleSheetText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSGetStyleSheetTextStyleSheetId p)
    ]
data CSSGetStyleSheetText = CSSGetStyleSheetText
  {
    -- | The stylesheet text.
    cSSGetStyleSheetTextText :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSGetStyleSheetText where
  parseJSON = A.withObject "CSSGetStyleSheetText" $ \o -> CSSGetStyleSheetText
    <$> o A..: "text"
instance Command PCSSGetStyleSheetText where
  type CommandResponse PCSSGetStyleSheetText = CSSGetStyleSheetText
  commandName _ = "CSS.getStyleSheetText"

-- | Returns all layers parsed by the rendering engine for the tree scope of a node.
--   Given a DOM element identified by nodeId, getLayersForNode returns the root
--   layer for the nearest ancestor document or shadow root. The layer root contains
--   the full layer tree for the tree scope and their ordering.

-- | Parameters of the 'CSS.getLayersForNode' command.
data PCSSGetLayersForNode = PCSSGetLayersForNode
  {
    pCSSGetLayersForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pCSSGetLayersForNode
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> PCSSGetLayersForNode
pCSSGetLayersForNode
  arg_pCSSGetLayersForNodeNodeId
  = PCSSGetLayersForNode
    arg_pCSSGetLayersForNodeNodeId
instance ToJSON PCSSGetLayersForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSGetLayersForNodeNodeId p)
    ]
data CSSGetLayersForNode = CSSGetLayersForNode
  {
    cSSGetLayersForNodeRootLayer :: CSSCSSLayerData
  }
  deriving (Eq, Show)
instance FromJSON CSSGetLayersForNode where
  parseJSON = A.withObject "CSSGetLayersForNode" $ \o -> CSSGetLayersForNode
    <$> o A..: "rootLayer"
instance Command PCSSGetLayersForNode where
  type CommandResponse PCSSGetLayersForNode = CSSGetLayersForNode
  commandName _ = "CSS.getLayersForNode"

-- | Starts tracking the given computed styles for updates. The specified array of properties
--   replaces the one previously specified. Pass empty array to disable tracking.
--   Use takeComputedStyleUpdates to retrieve the list of nodes that had properties modified.
--   The changes to computed style properties are only tracked for nodes pushed to the front-end
--   by the DOM agent. If no changes to the tracked properties occur after the node has been pushed
--   to the front-end, no updates will be issued for the node.

-- | Parameters of the 'CSS.trackComputedStyleUpdates' command.
data PCSSTrackComputedStyleUpdates = PCSSTrackComputedStyleUpdates
  {
    pCSSTrackComputedStyleUpdatesPropertiesToTrack :: [CSSCSSComputedStyleProperty]
  }
  deriving (Eq, Show)
pCSSTrackComputedStyleUpdates
  :: [CSSCSSComputedStyleProperty]
  -> PCSSTrackComputedStyleUpdates
pCSSTrackComputedStyleUpdates
  arg_pCSSTrackComputedStyleUpdatesPropertiesToTrack
  = PCSSTrackComputedStyleUpdates
    arg_pCSSTrackComputedStyleUpdatesPropertiesToTrack
instance ToJSON PCSSTrackComputedStyleUpdates where
  toJSON p = A.object $ catMaybes [
    ("propertiesToTrack" A..=) <$> Just (pCSSTrackComputedStyleUpdatesPropertiesToTrack p)
    ]
instance Command PCSSTrackComputedStyleUpdates where
  type CommandResponse PCSSTrackComputedStyleUpdates = ()
  commandName _ = "CSS.trackComputedStyleUpdates"
  fromJSON = const . A.Success . const ()

-- | Polls the next batch of computed style updates.

-- | Parameters of the 'CSS.takeComputedStyleUpdates' command.
data PCSSTakeComputedStyleUpdates = PCSSTakeComputedStyleUpdates
  deriving (Eq, Show)
pCSSTakeComputedStyleUpdates
  :: PCSSTakeComputedStyleUpdates
pCSSTakeComputedStyleUpdates
  = PCSSTakeComputedStyleUpdates
instance ToJSON PCSSTakeComputedStyleUpdates where
  toJSON _ = A.Null
data CSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates
  {
    -- | The list of node Ids that have their tracked computed styles updated
    cSSTakeComputedStyleUpdatesNodeIds :: [DOMPageNetworkEmulationSecurity.DOMNodeId]
  }
  deriving (Eq, Show)
instance FromJSON CSSTakeComputedStyleUpdates where
  parseJSON = A.withObject "CSSTakeComputedStyleUpdates" $ \o -> CSSTakeComputedStyleUpdates
    <$> o A..: "nodeIds"
instance Command PCSSTakeComputedStyleUpdates where
  type CommandResponse PCSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates
  commandName _ = "CSS.takeComputedStyleUpdates"

-- | Find a rule with the given active property for the given node and set the new value for this
--   property

-- | Parameters of the 'CSS.setEffectivePropertyValueForNode' command.
data PCSSSetEffectivePropertyValueForNode = PCSSSetEffectivePropertyValueForNode
  {
    -- | The element id for which to set property.
    pCSSSetEffectivePropertyValueForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
    pCSSSetEffectivePropertyValueForNodePropertyName :: T.Text,
    pCSSSetEffectivePropertyValueForNodeValue :: T.Text
  }
  deriving (Eq, Show)
pCSSSetEffectivePropertyValueForNode
  -- | The element id for which to set property.
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> T.Text
  -> T.Text
  -> PCSSSetEffectivePropertyValueForNode
pCSSSetEffectivePropertyValueForNode
  arg_pCSSSetEffectivePropertyValueForNodeNodeId
  arg_pCSSSetEffectivePropertyValueForNodePropertyName
  arg_pCSSSetEffectivePropertyValueForNodeValue
  = PCSSSetEffectivePropertyValueForNode
    arg_pCSSSetEffectivePropertyValueForNodeNodeId
    arg_pCSSSetEffectivePropertyValueForNodePropertyName
    arg_pCSSSetEffectivePropertyValueForNodeValue
instance ToJSON PCSSSetEffectivePropertyValueForNode where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodeNodeId p),
    ("propertyName" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodePropertyName p),
    ("value" A..=) <$> Just (pCSSSetEffectivePropertyValueForNodeValue p)
    ]
instance Command PCSSSetEffectivePropertyValueForNode where
  type CommandResponse PCSSSetEffectivePropertyValueForNode = ()
  commandName _ = "CSS.setEffectivePropertyValueForNode"
  fromJSON = const . A.Success . const ()

-- | Modifies the keyframe rule key text.

-- | Parameters of the 'CSS.setKeyframeKey' command.
data PCSSSetKeyframeKey = PCSSSetKeyframeKey
  {
    pCSSSetKeyframeKeyStyleSheetId :: CSSStyleSheetId,
    pCSSSetKeyframeKeyRange :: CSSSourceRange,
    pCSSSetKeyframeKeyKeyText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetKeyframeKey
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetKeyframeKey
pCSSSetKeyframeKey
  arg_pCSSSetKeyframeKeyStyleSheetId
  arg_pCSSSetKeyframeKeyRange
  arg_pCSSSetKeyframeKeyKeyText
  = PCSSSetKeyframeKey
    arg_pCSSSetKeyframeKeyStyleSheetId
    arg_pCSSSetKeyframeKeyRange
    arg_pCSSSetKeyframeKeyKeyText
instance ToJSON PCSSSetKeyframeKey where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetKeyframeKeyStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetKeyframeKeyRange p),
    ("keyText" A..=) <$> Just (pCSSSetKeyframeKeyKeyText p)
    ]
data CSSSetKeyframeKey = CSSSetKeyframeKey
  {
    -- | The resulting key text after modification.
    cSSSetKeyframeKeyKeyText :: CSSValue
  }
  deriving (Eq, Show)
instance FromJSON CSSSetKeyframeKey where
  parseJSON = A.withObject "CSSSetKeyframeKey" $ \o -> CSSSetKeyframeKey
    <$> o A..: "keyText"
instance Command PCSSSetKeyframeKey where
  type CommandResponse PCSSSetKeyframeKey = CSSSetKeyframeKey
  commandName _ = "CSS.setKeyframeKey"

-- | Modifies the rule selector.

-- | Parameters of the 'CSS.setMediaText' command.
data PCSSSetMediaText = PCSSSetMediaText
  {
    pCSSSetMediaTextStyleSheetId :: CSSStyleSheetId,
    pCSSSetMediaTextRange :: CSSSourceRange,
    pCSSSetMediaTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetMediaText
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetMediaText
pCSSSetMediaText
  arg_pCSSSetMediaTextStyleSheetId
  arg_pCSSSetMediaTextRange
  arg_pCSSSetMediaTextText
  = PCSSSetMediaText
    arg_pCSSSetMediaTextStyleSheetId
    arg_pCSSSetMediaTextRange
    arg_pCSSSetMediaTextText
instance ToJSON PCSSSetMediaText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetMediaTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetMediaTextRange p),
    ("text" A..=) <$> Just (pCSSSetMediaTextText p)
    ]
data CSSSetMediaText = CSSSetMediaText
  {
    -- | The resulting CSS media rule after modification.
    cSSSetMediaTextMedia :: CSSCSSMedia
  }
  deriving (Eq, Show)
instance FromJSON CSSSetMediaText where
  parseJSON = A.withObject "CSSSetMediaText" $ \o -> CSSSetMediaText
    <$> o A..: "media"
instance Command PCSSSetMediaText where
  type CommandResponse PCSSSetMediaText = CSSSetMediaText
  commandName _ = "CSS.setMediaText"

-- | Modifies the expression of a container query.

-- | Parameters of the 'CSS.setContainerQueryText' command.
data PCSSSetContainerQueryText = PCSSSetContainerQueryText
  {
    pCSSSetContainerQueryTextStyleSheetId :: CSSStyleSheetId,
    pCSSSetContainerQueryTextRange :: CSSSourceRange,
    pCSSSetContainerQueryTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetContainerQueryText
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetContainerQueryText
pCSSSetContainerQueryText
  arg_pCSSSetContainerQueryTextStyleSheetId
  arg_pCSSSetContainerQueryTextRange
  arg_pCSSSetContainerQueryTextText
  = PCSSSetContainerQueryText
    arg_pCSSSetContainerQueryTextStyleSheetId
    arg_pCSSSetContainerQueryTextRange
    arg_pCSSSetContainerQueryTextText
instance ToJSON PCSSSetContainerQueryText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetContainerQueryTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetContainerQueryTextRange p),
    ("text" A..=) <$> Just (pCSSSetContainerQueryTextText p)
    ]
data CSSSetContainerQueryText = CSSSetContainerQueryText
  {
    -- | The resulting CSS container query rule after modification.
    cSSSetContainerQueryTextContainerQuery :: CSSCSSContainerQuery
  }
  deriving (Eq, Show)
instance FromJSON CSSSetContainerQueryText where
  parseJSON = A.withObject "CSSSetContainerQueryText" $ \o -> CSSSetContainerQueryText
    <$> o A..: "containerQuery"
instance Command PCSSSetContainerQueryText where
  type CommandResponse PCSSSetContainerQueryText = CSSSetContainerQueryText
  commandName _ = "CSS.setContainerQueryText"

-- | Modifies the expression of a supports at-rule.

-- | Parameters of the 'CSS.setSupportsText' command.
data PCSSSetSupportsText = PCSSSetSupportsText
  {
    pCSSSetSupportsTextStyleSheetId :: CSSStyleSheetId,
    pCSSSetSupportsTextRange :: CSSSourceRange,
    pCSSSetSupportsTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetSupportsText
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetSupportsText
pCSSSetSupportsText
  arg_pCSSSetSupportsTextStyleSheetId
  arg_pCSSSetSupportsTextRange
  arg_pCSSSetSupportsTextText
  = PCSSSetSupportsText
    arg_pCSSSetSupportsTextStyleSheetId
    arg_pCSSSetSupportsTextRange
    arg_pCSSSetSupportsTextText
instance ToJSON PCSSSetSupportsText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetSupportsTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetSupportsTextRange p),
    ("text" A..=) <$> Just (pCSSSetSupportsTextText p)
    ]
data CSSSetSupportsText = CSSSetSupportsText
  {
    -- | The resulting CSS Supports rule after modification.
    cSSSetSupportsTextSupports :: CSSCSSSupports
  }
  deriving (Eq, Show)
instance FromJSON CSSSetSupportsText where
  parseJSON = A.withObject "CSSSetSupportsText" $ \o -> CSSSetSupportsText
    <$> o A..: "supports"
instance Command PCSSSetSupportsText where
  type CommandResponse PCSSSetSupportsText = CSSSetSupportsText
  commandName _ = "CSS.setSupportsText"

-- | Modifies the expression of a scope at-rule.

-- | Parameters of the 'CSS.setScopeText' command.
data PCSSSetScopeText = PCSSSetScopeText
  {
    pCSSSetScopeTextStyleSheetId :: CSSStyleSheetId,
    pCSSSetScopeTextRange :: CSSSourceRange,
    pCSSSetScopeTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetScopeText
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetScopeText
pCSSSetScopeText
  arg_pCSSSetScopeTextStyleSheetId
  arg_pCSSSetScopeTextRange
  arg_pCSSSetScopeTextText
  = PCSSSetScopeText
    arg_pCSSSetScopeTextStyleSheetId
    arg_pCSSSetScopeTextRange
    arg_pCSSSetScopeTextText
instance ToJSON PCSSSetScopeText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetScopeTextStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetScopeTextRange p),
    ("text" A..=) <$> Just (pCSSSetScopeTextText p)
    ]
data CSSSetScopeText = CSSSetScopeText
  {
    -- | The resulting CSS Scope rule after modification.
    cSSSetScopeTextScope :: CSSCSSScope
  }
  deriving (Eq, Show)
instance FromJSON CSSSetScopeText where
  parseJSON = A.withObject "CSSSetScopeText" $ \o -> CSSSetScopeText
    <$> o A..: "scope"
instance Command PCSSSetScopeText where
  type CommandResponse PCSSSetScopeText = CSSSetScopeText
  commandName _ = "CSS.setScopeText"

-- | Modifies the rule selector.

-- | Parameters of the 'CSS.setRuleSelector' command.
data PCSSSetRuleSelector = PCSSSetRuleSelector
  {
    pCSSSetRuleSelectorStyleSheetId :: CSSStyleSheetId,
    pCSSSetRuleSelectorRange :: CSSSourceRange,
    pCSSSetRuleSelectorSelector :: T.Text
  }
  deriving (Eq, Show)
pCSSSetRuleSelector
  :: CSSStyleSheetId
  -> CSSSourceRange
  -> T.Text
  -> PCSSSetRuleSelector
pCSSSetRuleSelector
  arg_pCSSSetRuleSelectorStyleSheetId
  arg_pCSSSetRuleSelectorRange
  arg_pCSSSetRuleSelectorSelector
  = PCSSSetRuleSelector
    arg_pCSSSetRuleSelectorStyleSheetId
    arg_pCSSSetRuleSelectorRange
    arg_pCSSSetRuleSelectorSelector
instance ToJSON PCSSSetRuleSelector where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetRuleSelectorStyleSheetId p),
    ("range" A..=) <$> Just (pCSSSetRuleSelectorRange p),
    ("selector" A..=) <$> Just (pCSSSetRuleSelectorSelector p)
    ]
data CSSSetRuleSelector = CSSSetRuleSelector
  {
    -- | The resulting selector list after modification.
    cSSSetRuleSelectorSelectorList :: CSSSelectorList
  }
  deriving (Eq, Show)
instance FromJSON CSSSetRuleSelector where
  parseJSON = A.withObject "CSSSetRuleSelector" $ \o -> CSSSetRuleSelector
    <$> o A..: "selectorList"
instance Command PCSSSetRuleSelector where
  type CommandResponse PCSSSetRuleSelector = CSSSetRuleSelector
  commandName _ = "CSS.setRuleSelector"

-- | Sets the new stylesheet text.

-- | Parameters of the 'CSS.setStyleSheetText' command.
data PCSSSetStyleSheetText = PCSSSetStyleSheetText
  {
    pCSSSetStyleSheetTextStyleSheetId :: CSSStyleSheetId,
    pCSSSetStyleSheetTextText :: T.Text
  }
  deriving (Eq, Show)
pCSSSetStyleSheetText
  :: CSSStyleSheetId
  -> T.Text
  -> PCSSSetStyleSheetText
pCSSSetStyleSheetText
  arg_pCSSSetStyleSheetTextStyleSheetId
  arg_pCSSSetStyleSheetTextText
  = PCSSSetStyleSheetText
    arg_pCSSSetStyleSheetTextStyleSheetId
    arg_pCSSSetStyleSheetTextText
instance ToJSON PCSSSetStyleSheetText where
  toJSON p = A.object $ catMaybes [
    ("styleSheetId" A..=) <$> Just (pCSSSetStyleSheetTextStyleSheetId p),
    ("text" A..=) <$> Just (pCSSSetStyleSheetTextText p)
    ]
data CSSSetStyleSheetText = CSSSetStyleSheetText
  {
    -- | URL of source map associated with script (if any).
    cSSSetStyleSheetTextSourceMapURL :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON CSSSetStyleSheetText where
  parseJSON = A.withObject "CSSSetStyleSheetText" $ \o -> CSSSetStyleSheetText
    <$> o A..:? "sourceMapURL"
instance Command PCSSSetStyleSheetText where
  type CommandResponse PCSSSetStyleSheetText = CSSSetStyleSheetText
  commandName _ = "CSS.setStyleSheetText"

-- | Applies specified style edits one after another in the given order.

-- | Parameters of the 'CSS.setStyleTexts' command.
data PCSSSetStyleTexts = PCSSSetStyleTexts
  {
    pCSSSetStyleTextsEdits :: [CSSStyleDeclarationEdit]
  }
  deriving (Eq, Show)
pCSSSetStyleTexts
  :: [CSSStyleDeclarationEdit]
  -> PCSSSetStyleTexts
pCSSSetStyleTexts
  arg_pCSSSetStyleTextsEdits
  = PCSSSetStyleTexts
    arg_pCSSSetStyleTextsEdits
instance ToJSON PCSSSetStyleTexts where
  toJSON p = A.object $ catMaybes [
    ("edits" A..=) <$> Just (pCSSSetStyleTextsEdits p)
    ]
data CSSSetStyleTexts = CSSSetStyleTexts
  {
    -- | The resulting styles after modification.
    cSSSetStyleTextsStyles :: [CSSCSSStyle]
  }
  deriving (Eq, Show)
instance FromJSON CSSSetStyleTexts where
  parseJSON = A.withObject "CSSSetStyleTexts" $ \o -> CSSSetStyleTexts
    <$> o A..: "styles"
instance Command PCSSSetStyleTexts where
  type CommandResponse PCSSSetStyleTexts = CSSSetStyleTexts
  commandName _ = "CSS.setStyleTexts"

-- | Enables the selector recording.

-- | Parameters of the 'CSS.startRuleUsageTracking' command.
data PCSSStartRuleUsageTracking = PCSSStartRuleUsageTracking
  deriving (Eq, Show)
pCSSStartRuleUsageTracking
  :: PCSSStartRuleUsageTracking
pCSSStartRuleUsageTracking
  = PCSSStartRuleUsageTracking
instance ToJSON PCSSStartRuleUsageTracking where
  toJSON _ = A.Null
instance Command PCSSStartRuleUsageTracking where
  type CommandResponse PCSSStartRuleUsageTracking = ()
  commandName _ = "CSS.startRuleUsageTracking"
  fromJSON = const . A.Success . const ()

-- | Stop tracking rule usage and return the list of rules that were used since last call to
--   `takeCoverageDelta` (or since start of coverage instrumentation)

-- | Parameters of the 'CSS.stopRuleUsageTracking' command.
data PCSSStopRuleUsageTracking = PCSSStopRuleUsageTracking
  deriving (Eq, Show)
pCSSStopRuleUsageTracking
  :: PCSSStopRuleUsageTracking
pCSSStopRuleUsageTracking
  = PCSSStopRuleUsageTracking
instance ToJSON PCSSStopRuleUsageTracking where
  toJSON _ = A.Null
data CSSStopRuleUsageTracking = CSSStopRuleUsageTracking
  {
    cSSStopRuleUsageTrackingRuleUsage :: [CSSRuleUsage]
  }
  deriving (Eq, Show)
instance FromJSON CSSStopRuleUsageTracking where
  parseJSON = A.withObject "CSSStopRuleUsageTracking" $ \o -> CSSStopRuleUsageTracking
    <$> o A..: "ruleUsage"
instance Command PCSSStopRuleUsageTracking where
  type CommandResponse PCSSStopRuleUsageTracking = CSSStopRuleUsageTracking
  commandName _ = "CSS.stopRuleUsageTracking"

-- | Obtain list of rules that became used since last call to this method (or since start of coverage
--   instrumentation)

-- | Parameters of the 'CSS.takeCoverageDelta' command.
data PCSSTakeCoverageDelta = PCSSTakeCoverageDelta
  deriving (Eq, Show)
pCSSTakeCoverageDelta
  :: PCSSTakeCoverageDelta
pCSSTakeCoverageDelta
  = PCSSTakeCoverageDelta
instance ToJSON PCSSTakeCoverageDelta where
  toJSON _ = A.Null
data CSSTakeCoverageDelta = CSSTakeCoverageDelta
  {
    cSSTakeCoverageDeltaCoverage :: [CSSRuleUsage],
    -- | Monotonically increasing time, in seconds.
    cSSTakeCoverageDeltaTimestamp :: Double
  }
  deriving (Eq, Show)
instance FromJSON CSSTakeCoverageDelta where
  parseJSON = A.withObject "CSSTakeCoverageDelta" $ \o -> CSSTakeCoverageDelta
    <$> o A..: "coverage"
    <*> o A..: "timestamp"
instance Command PCSSTakeCoverageDelta where
  type CommandResponse PCSSTakeCoverageDelta = CSSTakeCoverageDelta
  commandName _ = "CSS.takeCoverageDelta"

-- | Enables/disables rendering of local CSS fonts (enabled by default).

-- | Parameters of the 'CSS.setLocalFontsEnabled' command.
data PCSSSetLocalFontsEnabled = PCSSSetLocalFontsEnabled
  {
    -- | Whether rendering of local fonts is enabled.
    pCSSSetLocalFontsEnabledEnabled :: Bool
  }
  deriving (Eq, Show)
pCSSSetLocalFontsEnabled
  -- | Whether rendering of local fonts is enabled.
  :: Bool
  -> PCSSSetLocalFontsEnabled
pCSSSetLocalFontsEnabled
  arg_pCSSSetLocalFontsEnabledEnabled
  = PCSSSetLocalFontsEnabled
    arg_pCSSSetLocalFontsEnabledEnabled
instance ToJSON PCSSSetLocalFontsEnabled where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pCSSSetLocalFontsEnabledEnabled p)
    ]
instance Command PCSSSetLocalFontsEnabled where
  type CommandResponse PCSSSetLocalFontsEnabled = ()
  commandName _ = "CSS.setLocalFontsEnabled"
  fromJSON = const . A.Success . const ()

