{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  CSS :
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
type CSSStyleSheetId = String

-- | Type 'CSS.StyleSheetOrigin'.
--   Stylesheet type: "injected" for stylesheets injected via extension, "user-agent" for user-agent
--   stylesheets, "inspector" for stylesheets created by the inspector (i.e. those holding the "via
--   inspector" rules), "regular" for regular stylesheets.
data CSSStyleSheetOrigin = CSSStyleSheetOriginInjected | CSSStyleSheetOriginUserAgent | CSSStyleSheetOriginInspector | CSSStyleSheetOriginRegular
   deriving (Ord, Eq, Show, Read)
instance FromJSON CSSStyleSheetOrigin where
   parseJSON = A.withText  "CSSStyleSheetOrigin"  $ \v -> do
      case v of
         "injected" -> pure CSSStyleSheetOriginInjected
         "user-agent" -> pure CSSStyleSheetOriginUserAgent
         "inspector" -> pure CSSStyleSheetOriginInspector
         "regular" -> pure CSSStyleSheetOriginRegular
         _ -> fail "failed to parse CSSStyleSheetOrigin"

instance ToJSON CSSStyleSheetOrigin where
   toJSON v = A.String $
      case v of
         CSSStyleSheetOriginInjected -> "injected"
         CSSStyleSheetOriginUserAgent -> "user-agent"
         CSSStyleSheetOriginInspector -> "inspector"
         CSSStyleSheetOriginRegular -> "regular"



-- | Type 'CSS.PseudoElementMatches'.
--   CSS rule collection for a single pseudo style.
data CSSPseudoElementMatches = CSSPseudoElementMatches {
  -- | Pseudo element type.
  cSSPseudoElementMatchesPseudoType :: DOMPageNetworkEmulationSecurity.DOMPseudoType,
  -- | Pseudo element custom ident.
  cSSPseudoElementMatchesPseudoIdentifier :: Maybe String,
  -- | Matches of CSS rules applicable to the pseudo style.
  cSSPseudoElementMatchesMatches :: [CSSRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CSSPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'CSS.InheritedStyleEntry'.
--   Inherited CSS rule collection from ancestor node.
data CSSInheritedStyleEntry = CSSInheritedStyleEntry {
  -- | The ancestor node's inline style, if any, in the style inheritance chain.
  cSSInheritedStyleEntryInlineStyle :: Maybe CSSCSSStyle,
  -- | Matches of CSS rules matching the ancestor node in the style inheritance chain.
  cSSInheritedStyleEntryMatchedCSSRules :: [CSSRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSInheritedStyleEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CSSInheritedStyleEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'CSS.InheritedPseudoElementMatches'.
--   Inherited pseudo element matches from pseudos of an ancestor node.
data CSSInheritedPseudoElementMatches = CSSInheritedPseudoElementMatches {
  -- | Matches of pseudo styles from the pseudos of an ancestor node.
  cSSInheritedPseudoElementMatchesPseudoElements :: [CSSPseudoElementMatches]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSInheritedPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  CSSInheritedPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type 'CSS.RuleMatch'.
--   Match data for a CSS rule.
data CSSRuleMatch = CSSRuleMatch {
  -- | CSS rule in the match.
  cSSRuleMatchRule :: CSSCSSRule,
  -- | Matching selector indices in the rule's selectorList selectors (0-based).
  cSSRuleMatchMatchingSelectors :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSRuleMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CSSRuleMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Type 'CSS.Value'.
--   Data for a simple selector (these are delimited by commas in a selector list).
data CSSValue = CSSValue {
  -- | Value text.
  cSSValueText :: String,
  -- | Value range in the underlying resource (if available).
  cSSValueRange :: Maybe CSSSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  CSSValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }



-- | Type 'CSS.SelectorList'.
--   Selector list data.
data CSSSelectorList = CSSSelectorList {
  -- | Selectors in the list.
  cSSSelectorListSelectors :: [CSSValue],
  -- | Rule selector text.
  cSSSelectorListText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSSelectorList  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CSSSelectorList where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'CSS.CSSStyleSheetHeader'.
--   CSS stylesheet metainformation.
data CSSCSSStyleSheetHeader = CSSCSSStyleSheetHeader {
  -- | The stylesheet identifier.
  cSSCSSStyleSheetHeaderStyleSheetId :: CSSStyleSheetId,
  -- | Owner frame identifier.
  cSSCSSStyleSheetHeaderFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
  -- | Stylesheet resource URL. Empty if this is a constructed stylesheet created using
  --   new CSSStyleSheet() (but non-empty if this is a constructed sylesheet imported
  --   as a CSS module script).
  cSSCSSStyleSheetHeaderSourceURL :: String,
  -- | URL of source map associated with the stylesheet (if any).
  cSSCSSStyleSheetHeaderSourceMapURL :: Maybe String,
  -- | Stylesheet origin.
  cSSCSSStyleSheetHeaderOrigin :: CSSStyleSheetOrigin,
  -- | Stylesheet title.
  cSSCSSStyleSheetHeaderTitle :: String,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSStyleSheetHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CSSCSSStyleSheetHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'CSS.CSSRule'.
--   CSS rule representation.
data CSSCSSRule = CSSCSSRule {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  CSSCSSRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }



-- | Type 'CSS.RuleUsage'.
--   CSS coverage information.
data CSSRuleUsage = CSSRuleUsage {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
  --   stylesheet rules) this rule came from.
  cSSRuleUsageStyleSheetId :: CSSStyleSheetId,
  -- | Offset of the start of the rule (including selector) from the beginning of the stylesheet.
  cSSRuleUsageStartOffset :: Double,
  -- | Offset of the end of the rule body from the beginning of the stylesheet.
  cSSRuleUsageEndOffset :: Double,
  -- | Indicates whether the rule was actually used by some element in the page.
  cSSRuleUsageUsed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSRuleUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CSSRuleUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Type 'CSS.SourceRange'.
--   Text range within a resource. All numbers are zero-based.
data CSSSourceRange = CSSSourceRange {
  -- | Start line of range.
  cSSSourceRangeStartLine :: Int,
  -- | Start column of range (inclusive).
  cSSSourceRangeStartColumn :: Int,
  -- | End line of range
  cSSSourceRangeEndLine :: Int,
  -- | End column of range (exclusive).
  cSSSourceRangeEndColumn :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSSourceRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CSSSourceRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'CSS.ShorthandEntry'.
data CSSShorthandEntry = CSSShorthandEntry {
  -- | Shorthand name.
  cSSShorthandEntryName :: String,
  -- | Shorthand value.
  cSSShorthandEntryValue :: String,
  -- | Whether the property has "!important" annotation (implies `false` if absent).
  cSSShorthandEntryImportant :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSShorthandEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  CSSShorthandEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'CSS.CSSComputedStyleProperty'.
data CSSCSSComputedStyleProperty = CSSCSSComputedStyleProperty {
  -- | Computed style property name.
  cSSCSSComputedStylePropertyName :: String,
  -- | Computed style property value.
  cSSCSSComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  CSSCSSComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'CSS.CSSStyle'.
--   CSS style representation.
data CSSCSSStyle = CSSCSSStyle {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
  --   stylesheet rules) this rule came from.
  cSSCSSStyleStyleSheetId :: Maybe CSSStyleSheetId,
  -- | CSS properties in the style.
  cSSCSSStyleCssProperties :: [CSSCSSProperty],
  -- | Computed values for all shorthands found in the style.
  cSSCSSStyleShorthandEntries :: [CSSShorthandEntry],
  -- | Style declaration text (if available).
  cSSCSSStyleCssText :: Maybe String,
  -- | Style declaration range in the enclosing stylesheet (if available).
  cSSCSSStyleRange :: Maybe CSSSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CSSCSSStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'CSS.CSSProperty'.
--   CSS property declaration data.
data CSSCSSProperty = CSSCSSProperty {
  -- | The property name.
  cSSCSSPropertyName :: String,
  -- | The property value.
  cSSCSSPropertyValue :: String,
  -- | Whether the property has "!important" annotation (implies `false` if absent).
  cSSCSSPropertyImportant :: Maybe Bool,
  -- | Whether the property is implicit (implies `false` if absent).
  cSSCSSPropertyImplicit :: Maybe Bool,
  -- | The full property text as specified in the style.
  cSSCSSPropertyText :: Maybe String,
  -- | Whether the property is understood by the browser (implies `true` if absent).
  cSSCSSPropertyParsedOk :: Maybe Bool,
  -- | Whether the property is disabled by the user (present for source-based properties only).
  cSSCSSPropertyDisabled :: Maybe Bool,
  -- | The entire property range in the enclosing style declaration (if available).
  cSSCSSPropertyRange :: Maybe CSSSourceRange,
  -- | Parsed longhand components of this property if it is a shorthand.
  --   This field will be empty if the given property is not a shorthand.
  cSSCSSPropertyLonghandProperties :: Maybe [CSSCSSProperty]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CSSCSSProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'CSS.CSSMedia'.
--   CSS media rule descriptor.
data CSSCSSMediaSource = CSSCSSMediaSourceMediaRule | CSSCSSMediaSourceImportRule | CSSCSSMediaSourceLinkedSheet | CSSCSSMediaSourceInlineSheet
   deriving (Ord, Eq, Show, Read)
instance FromJSON CSSCSSMediaSource where
   parseJSON = A.withText  "CSSCSSMediaSource"  $ \v -> do
      case v of
         "mediaRule" -> pure CSSCSSMediaSourceMediaRule
         "importRule" -> pure CSSCSSMediaSourceImportRule
         "linkedSheet" -> pure CSSCSSMediaSourceLinkedSheet
         "inlineSheet" -> pure CSSCSSMediaSourceInlineSheet
         _ -> fail "failed to parse CSSCSSMediaSource"

instance ToJSON CSSCSSMediaSource where
   toJSON v = A.String $
      case v of
         CSSCSSMediaSourceMediaRule -> "mediaRule"
         CSSCSSMediaSourceImportRule -> "importRule"
         CSSCSSMediaSourceLinkedSheet -> "linkedSheet"
         CSSCSSMediaSourceInlineSheet -> "inlineSheet"



data CSSCSSMedia = CSSCSSMedia {
  -- | Media query text.
  cSSCSSMediaText :: String,
  -- | Source of the media query: "mediaRule" if specified by a @media rule, "importRule" if
  --   specified by an @import rule, "linkedSheet" if specified by a "media" attribute in a linked
  --   stylesheet's LINK tag, "inlineSheet" if specified by a "media" attribute in an inline
  --   stylesheet's STYLE tag.
  cSSCSSMediaSource :: CSSCSSMediaSource,
  -- | URL of the document containing the media query description.
  cSSCSSMediaSourceURL :: Maybe String,
  -- | The associated rule (@media or @import) header range in the enclosing stylesheet (if
  --   available).
  cSSCSSMediaRange :: Maybe CSSSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cSSCSSMediaStyleSheetId :: Maybe CSSStyleSheetId,
  -- | Array of media queries.
  cSSCSSMediaMediaList :: Maybe [CSSMediaQuery]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CSSCSSMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'CSS.MediaQuery'.
--   Media query descriptor.
data CSSMediaQuery = CSSMediaQuery {
  -- | Array of media query expressions.
  cSSMediaQueryExpressions :: [CSSMediaQueryExpression],
  -- | Whether the media query condition is satisfied.
  cSSMediaQueryActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSMediaQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  CSSMediaQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'CSS.MediaQueryExpression'.
--   Media query expression descriptor.
data CSSMediaQueryExpression = CSSMediaQueryExpression {
  -- | Media query expression value.
  cSSMediaQueryExpressionValue :: Double,
  -- | Media query expression units.
  cSSMediaQueryExpressionUnit :: String,
  -- | Media query expression feature.
  cSSMediaQueryExpressionFeature :: String,
  -- | The associated range of the value text in the enclosing stylesheet (if available).
  cSSMediaQueryExpressionValueRange :: Maybe CSSSourceRange,
  -- | Computed length of media query expression (if applicable).
  cSSMediaQueryExpressionComputedLength :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSMediaQueryExpression  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CSSMediaQueryExpression where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type 'CSS.CSSContainerQuery'.
--   CSS container query rule descriptor.
data CSSCSSContainerQuery = CSSCSSContainerQuery {
  -- | Container query text.
  cSSCSSContainerQueryText :: String,
  -- | The associated rule header range in the enclosing stylesheet (if
  --   available).
  cSSCSSContainerQueryRange :: Maybe CSSSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cSSCSSContainerQueryStyleSheetId :: Maybe CSSStyleSheetId,
  -- | Optional name for the container.
  cSSCSSContainerQueryName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSContainerQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CSSCSSContainerQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'CSS.CSSSupports'.
--   CSS Supports at-rule descriptor.
data CSSCSSSupports = CSSCSSSupports {
  -- | Supports rule text.
  cSSCSSSupportsText :: String,
  -- | Whether the supports condition is satisfied.
  cSSCSSSupportsActive :: Bool,
  -- | The associated rule header range in the enclosing stylesheet (if
  --   available).
  cSSCSSSupportsRange :: Maybe CSSSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cSSCSSSupportsStyleSheetId :: Maybe CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSSupports  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CSSCSSSupports where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'CSS.CSSScope'.
--   CSS Scope at-rule descriptor.
data CSSCSSScope = CSSCSSScope {
  -- | Scope rule text.
  cSSCSSScopeText :: String,
  -- | The associated rule header range in the enclosing stylesheet (if
  --   available).
  cSSCSSScopeRange :: Maybe CSSSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cSSCSSScopeStyleSheetId :: Maybe CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSScope  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CSSCSSScope where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'CSS.CSSLayer'.
--   CSS Layer at-rule descriptor.
data CSSCSSLayer = CSSCSSLayer {
  -- | Layer name.
  cSSCSSLayerText :: String,
  -- | The associated rule header range in the enclosing stylesheet (if
  --   available).
  cSSCSSLayerRange :: Maybe CSSSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cSSCSSLayerStyleSheetId :: Maybe CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CSSCSSLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'CSS.CSSLayerData'.
--   CSS Layer data.
data CSSCSSLayerData = CSSCSSLayerData {
  -- | Layer name.
  cSSCSSLayerDataName :: String,
  -- | Direct sub-layers
  cSSCSSLayerDataSubLayers :: Maybe [CSSCSSLayerData],
  -- | Layer order. The order determines the order of the layer in the cascade order.
  --   A higher number has higher priority in the cascade order.
  cSSCSSLayerDataOrder :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSLayerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CSSCSSLayerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'CSS.PlatformFontUsage'.
--   Information about amount of glyphs that were rendered with given font.
data CSSPlatformFontUsage = CSSPlatformFontUsage {
  -- | Font's family name reported by platform.
  cSSPlatformFontUsageFamilyName :: String,
  -- | Indicates if the font was downloaded or resolved locally.
  cSSPlatformFontUsageIsCustomFont :: Bool,
  -- | Amount of glyphs that were rendered with this font.
  cSSPlatformFontUsageGlyphCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSPlatformFontUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CSSPlatformFontUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'CSS.FontVariationAxis'.
--   Information about font variation axes for variable fonts
data CSSFontVariationAxis = CSSFontVariationAxis {
  -- | The font-variation-setting tag (a.k.a. "axis tag").
  cSSFontVariationAxisTag :: String,
  -- | Human-readable variation name in the default language (normally, "en").
  cSSFontVariationAxisName :: String,
  -- | The minimum value (inclusive) the font supports for this tag.
  cSSFontVariationAxisMinValue :: Double,
  -- | The maximum value (inclusive) the font supports for this tag.
  cSSFontVariationAxisMaxValue :: Double,
  -- | The default value.
  cSSFontVariationAxisDefaultValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSFontVariationAxis  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CSSFontVariationAxis where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'CSS.FontFace'.
--   Properties of a web font: https://www.w3.org/TR/2008/REC-CSS2-20080411/fonts.html#font-descriptions
--   and additional information such as platformFontFamily and fontVariationAxes.
data CSSFontFace = CSSFontFace {
  -- | The font-family.
  cSSFontFaceFontFamily :: String,
  -- | The font-style.
  cSSFontFaceFontStyle :: String,
  -- | The font-variant.
  cSSFontFaceFontVariant :: String,
  -- | The font-weight.
  cSSFontFaceFontWeight :: String,
  -- | The font-stretch.
  cSSFontFaceFontStretch :: String,
  -- | The font-display.
  cSSFontFaceFontDisplay :: String,
  -- | The unicode-range.
  cSSFontFaceUnicodeRange :: String,
  -- | The src.
  cSSFontFaceSrc :: String,
  -- | The resolved platform font family
  cSSFontFacePlatformFontFamily :: String,
  -- | Available variation settings (a.k.a. "axes").
  cSSFontFaceFontVariationAxes :: Maybe [CSSFontVariationAxis]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSFontFace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CSSFontFace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Type 'CSS.CSSKeyframesRule'.
--   CSS keyframes rule representation.
data CSSCSSKeyframesRule = CSSCSSKeyframesRule {
  -- | Animation name.
  cSSCSSKeyframesRuleAnimationName :: CSSValue,
  -- | List of keyframes.
  cSSCSSKeyframesRuleKeyframes :: [CSSCSSKeyframeRule]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  CSSCSSKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'CSS.CSSKeyframeRule'.
--   CSS keyframe rule representation.
data CSSCSSKeyframeRule = CSSCSSKeyframeRule {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
  --   stylesheet rules) this rule came from.
  cSSCSSKeyframeRuleStyleSheetId :: Maybe CSSStyleSheetId,
  -- | Parent stylesheet's origin.
  cSSCSSKeyframeRuleOrigin :: CSSStyleSheetOrigin,
  -- | Associated key text.
  cSSCSSKeyframeRuleKeyText :: CSSValue,
  -- | Associated style declaration.
  cSSCSSKeyframeRuleStyle :: CSSCSSStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSCSSKeyframeRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CSSCSSKeyframeRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'CSS.StyleDeclarationEdit'.
--   A descriptor of operation to mutate style declaration text.
data CSSStyleDeclarationEdit = CSSStyleDeclarationEdit {
  -- | The css style sheet identifier.
  cSSStyleDeclarationEditStyleSheetId :: CSSStyleSheetId,
  -- | The range of the style text in the enclosing stylesheet.
  cSSStyleDeclarationEditRange :: CSSSourceRange,
  -- | New style text.
  cSSStyleDeclarationEditText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSStyleDeclarationEdit  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CSSStyleDeclarationEdit where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





-- | Type of the 'CSS.fontsUpdated' event.
data CSSFontsUpdated = CSSFontsUpdated {
  -- | The web font that has loaded.
  cSSFontsUpdatedFont :: Maybe CSSFontFace
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSFontsUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CSSFontsUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Event CSSFontsUpdated where
    eventName _ = "CSS.fontsUpdated"

-- | Type of the 'CSS.mediaQueryResultChanged' event.
data CSSMediaQueryResultChanged = CSSMediaQueryResultChanged
   deriving (Eq, Show, Read)
instance FromJSON CSSMediaQueryResultChanged where
   parseJSON = A.withText  "CSSMediaQueryResultChanged"  $ \v -> do
      case v of
         "CSSMediaQueryResultChanged" -> pure CSSMediaQueryResultChanged
         _ -> fail "failed to parse CSSMediaQueryResultChanged"


instance Event CSSMediaQueryResultChanged where
    eventName _ = "CSS.mediaQueryResultChanged"

-- | Type of the 'CSS.styleSheetAdded' event.
data CSSStyleSheetAdded = CSSStyleSheetAdded {
  -- | Added stylesheet metainfo.
  cSSStyleSheetAddedHeader :: CSSCSSStyleSheetHeader
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSStyleSheetAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CSSStyleSheetAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


instance Event CSSStyleSheetAdded where
    eventName _ = "CSS.styleSheetAdded"

-- | Type of the 'CSS.styleSheetChanged' event.
data CSSStyleSheetChanged = CSSStyleSheetChanged {
  cSSStyleSheetChangedStyleSheetId :: CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSStyleSheetChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CSSStyleSheetChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Event CSSStyleSheetChanged where
    eventName _ = "CSS.styleSheetChanged"

-- | Type of the 'CSS.styleSheetRemoved' event.
data CSSStyleSheetRemoved = CSSStyleSheetRemoved {
  -- | Identifier of the removed stylesheet.
  cSSStyleSheetRemovedStyleSheetId :: CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CSSStyleSheetRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CSSStyleSheetRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Event CSSStyleSheetRemoved where
    eventName _ = "CSS.styleSheetRemoved"



-- | CSS.addRule
--   Inserts a new rule with the given `ruleText` in a stylesheet with given `styleSheetId`, at the
--   position specified by `location`.

-- | Parameters of the 'CSS.addRule' command.
data PCSSAddRule = PCSSAddRule {
  -- | The css style sheet identifier where a new rule should be inserted.
  pCSSAddRuleStyleSheetId :: CSSStyleSheetId,
  -- | The text of a new rule.
  pCSSAddRuleRuleText :: String,
  -- | Text position of a new rule in the target style sheet.
  pCSSAddRuleLocation :: CSSSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSAddRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PCSSAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


-- | Return type of the 'CSS.addRule' command.
data CSSAddRule = CSSAddRule {
  -- | The newly created rule.
  cSSAddRuleRule :: CSSCSSRule
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }

instance Command PCSSAddRule where
   type CommandResponse PCSSAddRule = CSSAddRule
   commandName _ = "CSS.addRule"



-- | CSS.collectClassNames
--   Returns all class names from specified stylesheet.

-- | Parameters of the 'CSS.collectClassNames' command.
data PCSSCollectClassNames = PCSSCollectClassNames {
  pCSSCollectClassNamesStyleSheetId :: CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSCollectClassNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCSSCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Return type of the 'CSS.collectClassNames' command.
data CSSCollectClassNames = CSSCollectClassNames {
  -- | Class name list.
  cSSCollectClassNamesClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PCSSCollectClassNames where
   type CommandResponse PCSSCollectClassNames = CSSCollectClassNames
   commandName _ = "CSS.collectClassNames"



-- | CSS.createStyleSheet
--   Creates a new special "via-inspector" stylesheet in the frame with given `frameId`.

-- | Parameters of the 'CSS.createStyleSheet' command.
data PCSSCreateStyleSheet = PCSSCreateStyleSheet {
  -- | Identifier of the frame where "via-inspector" stylesheet should be created.
  pCSSCreateStyleSheetFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSCreateStyleSheet  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCSSCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Return type of the 'CSS.createStyleSheet' command.
data CSSCreateStyleSheet = CSSCreateStyleSheet {
  -- | Identifier of the created "via-inspector" stylesheet.
  cSSCreateStyleSheetStyleSheetId :: CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PCSSCreateStyleSheet where
   type CommandResponse PCSSCreateStyleSheet = CSSCreateStyleSheet
   commandName _ = "CSS.createStyleSheet"



-- | CSS.disable
--   Disables the CSS agent for the given page.

-- | Parameters of the 'CSS.disable' command.
data PCSSDisable = PCSSDisable
instance ToJSON PCSSDisable where toJSON _ = A.Null

instance Command PCSSDisable where
   type CommandResponse PCSSDisable = ()
   commandName _ = "CSS.disable"
   fromJSON = const . A.Success . const ()


-- | CSS.enable
--   Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
--   enabled until the result of this command is received.

-- | Parameters of the 'CSS.enable' command.
data PCSSEnable = PCSSEnable
instance ToJSON PCSSEnable where toJSON _ = A.Null

instance Command PCSSEnable where
   type CommandResponse PCSSEnable = ()
   commandName _ = "CSS.enable"
   fromJSON = const . A.Success . const ()


-- | CSS.forcePseudoState
--   Ensures that the given node will have specified pseudo-classes whenever its style is computed by
--   the browser.

-- | Parameters of the 'CSS.forcePseudoState' command.
data PCSSForcePseudoState = PCSSForcePseudoState {
  -- | The element id for which to force the pseudo state.
  pCSSForcePseudoStateNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Element pseudo classes to force when computing the element's style.
  pCSSForcePseudoStateForcedPseudoClasses :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSForcePseudoState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCSSForcePseudoState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


instance Command PCSSForcePseudoState where
   type CommandResponse PCSSForcePseudoState = ()
   commandName _ = "CSS.forcePseudoState"
   fromJSON = const . A.Success . const ()


-- | CSS.getBackgroundColors

-- | Parameters of the 'CSS.getBackgroundColors' command.
data PCSSGetBackgroundColors = PCSSGetBackgroundColors {
  -- | Id of the node to get background colors for.
  pCSSGetBackgroundColorsNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetBackgroundColors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PCSSGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Return type of the 'CSS.getBackgroundColors' command.
data CSSGetBackgroundColors = CSSGetBackgroundColors {
  -- | The range of background colors behind this element, if it contains any visible text. If no
  --   visible text is present, this will be undefined. In the case of a flat background color,
  --   this will consist of simply that color. In the case of a gradient, this will consist of each
  --   of the color stops. For anything more complicated, this will be an empty array. Images will
  --   be ignored (as if the image had failed to load).
  cSSGetBackgroundColorsBackgroundColors :: Maybe [String],
  -- | The computed font size for this node, as a CSS computed value string (e.g. '12px').
  cSSGetBackgroundColorsComputedFontSize :: Maybe String,
  -- | The computed font weight for this node, as a CSS computed value string (e.g. 'normal' or
  --   '100').
  cSSGetBackgroundColorsComputedFontWeight :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PCSSGetBackgroundColors where
   type CommandResponse PCSSGetBackgroundColors = CSSGetBackgroundColors
   commandName _ = "CSS.getBackgroundColors"



-- | CSS.getComputedStyleForNode
--   Returns the computed style for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getComputedStyleForNode' command.
data PCSSGetComputedStyleForNode = PCSSGetComputedStyleForNode {
  pCSSGetComputedStyleForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetComputedStyleForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCSSGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Return type of the 'CSS.getComputedStyleForNode' command.
data CSSGetComputedStyleForNode = CSSGetComputedStyleForNode {
  -- | Computed style for the specified DOM node.
  cSSGetComputedStyleForNodeComputedStyle :: [CSSCSSComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command PCSSGetComputedStyleForNode where
   type CommandResponse PCSSGetComputedStyleForNode = CSSGetComputedStyleForNode
   commandName _ = "CSS.getComputedStyleForNode"



-- | CSS.getInlineStylesForNode
--   Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
--   attributes) for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getInlineStylesForNode' command.
data PCSSGetInlineStylesForNode = PCSSGetInlineStylesForNode {
  pCSSGetInlineStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetInlineStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PCSSGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Return type of the 'CSS.getInlineStylesForNode' command.
data CSSGetInlineStylesForNode = CSSGetInlineStylesForNode {
  -- | Inline style for the specified DOM node.
  cSSGetInlineStylesForNodeInlineStyle :: Maybe CSSCSSStyle,
  -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
  cSSGetInlineStylesForNodeAttributesStyle :: Maybe CSSCSSStyle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command PCSSGetInlineStylesForNode where
   type CommandResponse PCSSGetInlineStylesForNode = CSSGetInlineStylesForNode
   commandName _ = "CSS.getInlineStylesForNode"



-- | CSS.getMatchedStylesForNode
--   Returns requested styles for a DOM node identified by `nodeId`.

-- | Parameters of the 'CSS.getMatchedStylesForNode' command.
data PCSSGetMatchedStylesForNode = PCSSGetMatchedStylesForNode {
  pCSSGetMatchedStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetMatchedStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCSSGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Return type of the 'CSS.getMatchedStylesForNode' command.
data CSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode {
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
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command PCSSGetMatchedStylesForNode where
   type CommandResponse PCSSGetMatchedStylesForNode = CSSGetMatchedStylesForNode
   commandName _ = "CSS.getMatchedStylesForNode"



-- | CSS.getMediaQueries
--   Returns all media queries parsed by the rendering engine.

-- | Parameters of the 'CSS.getMediaQueries' command.
data PCSSGetMediaQueries = PCSSGetMediaQueries
instance ToJSON PCSSGetMediaQueries where toJSON _ = A.Null

-- | Return type of the 'CSS.getMediaQueries' command.
data CSSGetMediaQueries = CSSGetMediaQueries {
  cSSGetMediaQueriesMedias :: [CSSCSSMedia]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetMediaQueries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PCSSGetMediaQueries where
   type CommandResponse PCSSGetMediaQueries = CSSGetMediaQueries
   commandName _ = "CSS.getMediaQueries"



-- | CSS.getPlatformFontsForNode
--   Requests information about platform fonts which we used to render child TextNodes in the given
--   node.

-- | Parameters of the 'CSS.getPlatformFontsForNode' command.
data PCSSGetPlatformFontsForNode = PCSSGetPlatformFontsForNode {
  pCSSGetPlatformFontsForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetPlatformFontsForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCSSGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Return type of the 'CSS.getPlatformFontsForNode' command.
data CSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode {
  -- | Usage statistics for every employed platform font.
  cSSGetPlatformFontsForNodeFonts :: [CSSPlatformFontUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command PCSSGetPlatformFontsForNode where
   type CommandResponse PCSSGetPlatformFontsForNode = CSSGetPlatformFontsForNode
   commandName _ = "CSS.getPlatformFontsForNode"



-- | CSS.getStyleSheetText
--   Returns the current textual content for a stylesheet.

-- | Parameters of the 'CSS.getStyleSheetText' command.
data PCSSGetStyleSheetText = PCSSGetStyleSheetText {
  pCSSGetStyleSheetTextStyleSheetId :: CSSStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCSSGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Return type of the 'CSS.getStyleSheetText' command.
data CSSGetStyleSheetText = CSSGetStyleSheetText {
  -- | The stylesheet text.
  cSSGetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PCSSGetStyleSheetText where
   type CommandResponse PCSSGetStyleSheetText = CSSGetStyleSheetText
   commandName _ = "CSS.getStyleSheetText"



-- | CSS.getLayersForNode
--   Returns all layers parsed by the rendering engine for the tree scope of a node.
--   Given a DOM element identified by nodeId, getLayersForNode returns the root
--   layer for the nearest ancestor document or shadow root. The layer root contains
--   the full layer tree for the tree scope and their ordering.

-- | Parameters of the 'CSS.getLayersForNode' command.
data PCSSGetLayersForNode = PCSSGetLayersForNode {
  pCSSGetLayersForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSGetLayersForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCSSGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Return type of the 'CSS.getLayersForNode' command.
data CSSGetLayersForNode = CSSGetLayersForNode {
  cSSGetLayersForNodeRootLayer :: CSSCSSLayerData
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command PCSSGetLayersForNode where
   type CommandResponse PCSSGetLayersForNode = CSSGetLayersForNode
   commandName _ = "CSS.getLayersForNode"



-- | CSS.trackComputedStyleUpdates
--   Starts tracking the given computed styles for updates. The specified array of properties
--   replaces the one previously specified. Pass empty array to disable tracking.
--   Use takeComputedStyleUpdates to retrieve the list of nodes that had properties modified.
--   The changes to computed style properties are only tracked for nodes pushed to the front-end
--   by the DOM agent. If no changes to the tracked properties occur after the node has been pushed
--   to the front-end, no updates will be issued for the node.

-- | Parameters of the 'CSS.trackComputedStyleUpdates' command.
data PCSSTrackComputedStyleUpdates = PCSSTrackComputedStyleUpdates {
  pCSSTrackComputedStyleUpdatesPropertiesToTrack :: [CSSCSSComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSTrackComputedStyleUpdates  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PCSSTrackComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Command PCSSTrackComputedStyleUpdates where
   type CommandResponse PCSSTrackComputedStyleUpdates = ()
   commandName _ = "CSS.trackComputedStyleUpdates"
   fromJSON = const . A.Success . const ()


-- | CSS.takeComputedStyleUpdates
--   Polls the next batch of computed style updates.

-- | Parameters of the 'CSS.takeComputedStyleUpdates' command.
data PCSSTakeComputedStyleUpdates = PCSSTakeComputedStyleUpdates
instance ToJSON PCSSTakeComputedStyleUpdates where toJSON _ = A.Null

-- | Return type of the 'CSS.takeComputedStyleUpdates' command.
data CSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates {
  -- | The list of node Ids that have their tracked computed styles updated
  cSSTakeComputedStyleUpdatesNodeIds :: [DOMPageNetworkEmulationSecurity.DOMNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSTakeComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command PCSSTakeComputedStyleUpdates where
   type CommandResponse PCSSTakeComputedStyleUpdates = CSSTakeComputedStyleUpdates
   commandName _ = "CSS.takeComputedStyleUpdates"



-- | CSS.setEffectivePropertyValueForNode
--   Find a rule with the given active property for the given node and set the new value for this
--   property

-- | Parameters of the 'CSS.setEffectivePropertyValueForNode' command.
data PCSSSetEffectivePropertyValueForNode = PCSSSetEffectivePropertyValueForNode {
  -- | The element id for which to set property.
  pCSSSetEffectivePropertyValueForNodeNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
  pCSSSetEffectivePropertyValueForNodePropertyName :: String,
  pCSSSetEffectivePropertyValueForNodeValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetEffectivePropertyValueForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PCSSSetEffectivePropertyValueForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


instance Command PCSSSetEffectivePropertyValueForNode where
   type CommandResponse PCSSSetEffectivePropertyValueForNode = ()
   commandName _ = "CSS.setEffectivePropertyValueForNode"
   fromJSON = const . A.Success . const ()


-- | CSS.setKeyframeKey
--   Modifies the keyframe rule key text.

-- | Parameters of the 'CSS.setKeyframeKey' command.
data PCSSSetKeyframeKey = PCSSSetKeyframeKey {
  pCSSSetKeyframeKeyStyleSheetId :: CSSStyleSheetId,
  pCSSSetKeyframeKeyRange :: CSSSourceRange,
  pCSSSetKeyframeKeyKeyText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetKeyframeKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PCSSSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Return type of the 'CSS.setKeyframeKey' command.
data CSSSetKeyframeKey = CSSSetKeyframeKey {
  -- | The resulting key text after modification.
  cSSSetKeyframeKeyKeyText :: CSSValue
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command PCSSSetKeyframeKey where
   type CommandResponse PCSSSetKeyframeKey = CSSSetKeyframeKey
   commandName _ = "CSS.setKeyframeKey"



-- | CSS.setMediaText
--   Modifies the rule selector.

-- | Parameters of the 'CSS.setMediaText' command.
data PCSSSetMediaText = PCSSSetMediaText {
  pCSSSetMediaTextStyleSheetId :: CSSStyleSheetId,
  pCSSSetMediaTextRange :: CSSSourceRange,
  pCSSSetMediaTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetMediaText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCSSSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Return type of the 'CSS.setMediaText' command.
data CSSSetMediaText = CSSSetMediaText {
  -- | The resulting CSS media rule after modification.
  cSSSetMediaTextMedia :: CSSCSSMedia
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command PCSSSetMediaText where
   type CommandResponse PCSSSetMediaText = CSSSetMediaText
   commandName _ = "CSS.setMediaText"



-- | CSS.setContainerQueryText
--   Modifies the expression of a container query.

-- | Parameters of the 'CSS.setContainerQueryText' command.
data PCSSSetContainerQueryText = PCSSSetContainerQueryText {
  pCSSSetContainerQueryTextStyleSheetId :: CSSStyleSheetId,
  pCSSSetContainerQueryTextRange :: CSSSourceRange,
  pCSSSetContainerQueryTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetContainerQueryText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PCSSSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Return type of the 'CSS.setContainerQueryText' command.
data CSSSetContainerQueryText = CSSSetContainerQueryText {
  -- | The resulting CSS container query rule after modification.
  cSSSetContainerQueryTextContainerQuery :: CSSCSSContainerQuery
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PCSSSetContainerQueryText where
   type CommandResponse PCSSSetContainerQueryText = CSSSetContainerQueryText
   commandName _ = "CSS.setContainerQueryText"



-- | CSS.setSupportsText
--   Modifies the expression of a supports at-rule.

-- | Parameters of the 'CSS.setSupportsText' command.
data PCSSSetSupportsText = PCSSSetSupportsText {
  pCSSSetSupportsTextStyleSheetId :: CSSStyleSheetId,
  pCSSSetSupportsTextRange :: CSSSourceRange,
  pCSSSetSupportsTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetSupportsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCSSSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Return type of the 'CSS.setSupportsText' command.
data CSSSetSupportsText = CSSSetSupportsText {
  -- | The resulting CSS Supports rule after modification.
  cSSSetSupportsTextSupports :: CSSCSSSupports
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PCSSSetSupportsText where
   type CommandResponse PCSSSetSupportsText = CSSSetSupportsText
   commandName _ = "CSS.setSupportsText"



-- | CSS.setScopeText
--   Modifies the expression of a scope at-rule.

-- | Parameters of the 'CSS.setScopeText' command.
data PCSSSetScopeText = PCSSSetScopeText {
  pCSSSetScopeTextStyleSheetId :: CSSStyleSheetId,
  pCSSSetScopeTextRange :: CSSSourceRange,
  pCSSSetScopeTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetScopeText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCSSSetScopeText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Return type of the 'CSS.setScopeText' command.
data CSSSetScopeText = CSSSetScopeText {
  -- | The resulting CSS Scope rule after modification.
  cSSSetScopeTextScope :: CSSCSSScope
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetScopeText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command PCSSSetScopeText where
   type CommandResponse PCSSSetScopeText = CSSSetScopeText
   commandName _ = "CSS.setScopeText"



-- | CSS.setRuleSelector
--   Modifies the rule selector.

-- | Parameters of the 'CSS.setRuleSelector' command.
data PCSSSetRuleSelector = PCSSSetRuleSelector {
  pCSSSetRuleSelectorStyleSheetId :: CSSStyleSheetId,
  pCSSSetRuleSelectorRange :: CSSSourceRange,
  pCSSSetRuleSelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetRuleSelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCSSSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Return type of the 'CSS.setRuleSelector' command.
data CSSSetRuleSelector = CSSSetRuleSelector {
  -- | The resulting selector list after modification.
  cSSSetRuleSelectorSelectorList :: CSSSelectorList
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command PCSSSetRuleSelector where
   type CommandResponse PCSSSetRuleSelector = CSSSetRuleSelector
   commandName _ = "CSS.setRuleSelector"



-- | CSS.setStyleSheetText
--   Sets the new stylesheet text.

-- | Parameters of the 'CSS.setStyleSheetText' command.
data PCSSSetStyleSheetText = PCSSSetStyleSheetText {
  pCSSSetStyleSheetTextStyleSheetId :: CSSStyleSheetId,
  pCSSSetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCSSSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Return type of the 'CSS.setStyleSheetText' command.
data CSSSetStyleSheetText = CSSSetStyleSheetText {
  -- | URL of source map associated with script (if any).
  cSSSetStyleSheetTextSourceMapURL :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PCSSSetStyleSheetText where
   type CommandResponse PCSSSetStyleSheetText = CSSSetStyleSheetText
   commandName _ = "CSS.setStyleSheetText"



-- | CSS.setStyleTexts
--   Applies specified style edits one after another in the given order.

-- | Parameters of the 'CSS.setStyleTexts' command.
data PCSSSetStyleTexts = PCSSSetStyleTexts {
  pCSSSetStyleTextsEdits :: [CSSStyleDeclarationEdit]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetStyleTexts  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PCSSSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Return type of the 'CSS.setStyleTexts' command.
data CSSSetStyleTexts = CSSSetStyleTexts {
  -- | The resulting styles after modification.
  cSSSetStyleTextsStyles :: [CSSCSSStyle]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command PCSSSetStyleTexts where
   type CommandResponse PCSSSetStyleTexts = CSSSetStyleTexts
   commandName _ = "CSS.setStyleTexts"



-- | CSS.startRuleUsageTracking
--   Enables the selector recording.

-- | Parameters of the 'CSS.startRuleUsageTracking' command.
data PCSSStartRuleUsageTracking = PCSSStartRuleUsageTracking
instance ToJSON PCSSStartRuleUsageTracking where toJSON _ = A.Null

instance Command PCSSStartRuleUsageTracking where
   type CommandResponse PCSSStartRuleUsageTracking = ()
   commandName _ = "CSS.startRuleUsageTracking"
   fromJSON = const . A.Success . const ()


-- | CSS.stopRuleUsageTracking
--   Stop tracking rule usage and return the list of rules that were used since last call to
--   `takeCoverageDelta` (or since start of coverage instrumentation)

-- | Parameters of the 'CSS.stopRuleUsageTracking' command.
data PCSSStopRuleUsageTracking = PCSSStopRuleUsageTracking
instance ToJSON PCSSStopRuleUsageTracking where toJSON _ = A.Null

-- | Return type of the 'CSS.stopRuleUsageTracking' command.
data CSSStopRuleUsageTracking = CSSStopRuleUsageTracking {
  cSSStopRuleUsageTrackingRuleUsage :: [CSSRuleUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSStopRuleUsageTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command PCSSStopRuleUsageTracking where
   type CommandResponse PCSSStopRuleUsageTracking = CSSStopRuleUsageTracking
   commandName _ = "CSS.stopRuleUsageTracking"



-- | CSS.takeCoverageDelta
--   Obtain list of rules that became used since last call to this method (or since start of coverage
--   instrumentation)

-- | Parameters of the 'CSS.takeCoverageDelta' command.
data PCSSTakeCoverageDelta = PCSSTakeCoverageDelta
instance ToJSON PCSSTakeCoverageDelta where toJSON _ = A.Null

-- | Return type of the 'CSS.takeCoverageDelta' command.
data CSSTakeCoverageDelta = CSSTakeCoverageDelta {
  cSSTakeCoverageDeltaCoverage :: [CSSRuleUsage],
  -- | Monotonically increasing time, in seconds.
  cSSTakeCoverageDeltaTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CSSTakeCoverageDelta where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command PCSSTakeCoverageDelta where
   type CommandResponse PCSSTakeCoverageDelta = CSSTakeCoverageDelta
   commandName _ = "CSS.takeCoverageDelta"



-- | CSS.setLocalFontsEnabled
--   Enables/disables rendering of local CSS fonts (enabled by default).

-- | Parameters of the 'CSS.setLocalFontsEnabled' command.
data PCSSSetLocalFontsEnabled = PCSSSetLocalFontsEnabled {
  -- | Whether rendering of local fonts is enabled.
  pCSSSetLocalFontsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCSSSetLocalFontsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCSSSetLocalFontsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PCSSSetLocalFontsEnabled where
   type CommandResponse PCSSSetLocalFontsEnabled = ()
   commandName _ = "CSS.setLocalFontsEnabled"
   fromJSON = const . A.Success . const ()



