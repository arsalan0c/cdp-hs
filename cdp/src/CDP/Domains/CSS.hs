{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


-- | Type 'CSS.StyleSheetId' .
type CssStyleSheetId = String

-- | Stylesheet type: "injected" for stylesheets injected via extension, "user-agent" for user-agent
 -- stylesheets, "inspector" for stylesheets created by the inspector (i.e. those holding the "via
 -- inspector" rules), "regular" for regular stylesheets.
data CssStyleSheetOrigin = CssStyleSheetOriginInjected | CssStyleSheetOriginUserAgent | CssStyleSheetOriginInspector | CssStyleSheetOriginRegular
   deriving (Ord, Eq, Show, Read)
instance FromJSON CssStyleSheetOrigin where
   parseJSON = A.withText  "CssStyleSheetOrigin"  $ \v -> do
      case v of
         "injected" -> pure CssStyleSheetOriginInjected
         "user-agent" -> pure CssStyleSheetOriginUserAgent
         "inspector" -> pure CssStyleSheetOriginInspector
         "regular" -> pure CssStyleSheetOriginRegular
         _ -> fail "failed to parse CssStyleSheetOrigin"

instance ToJSON CssStyleSheetOrigin where
   toJSON v = A.String $
      case v of
         CssStyleSheetOriginInjected -> "injected"
         CssStyleSheetOriginUserAgent -> "user-agent"
         CssStyleSheetOriginInspector -> "inspector"
         CssStyleSheetOriginRegular -> "regular"



-- | CSS rule collection for a single pseudo style.
data CssPseudoElementMatches = CssPseudoElementMatches {
  -- | Pseudo element type.
  cssPseudoElementMatchesPseudoType :: DOMPageNetworkEmulationSecurity.DomPseudoType,
  -- | Matches of CSS rules applicable to the pseudo style.
  cssPseudoElementMatchesMatches :: [CssRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Inherited CSS rule collection from ancestor node.
data CssInheritedStyleEntry = CssInheritedStyleEntry {
  -- | The ancestor node's inline style, if any, in the style inheritance chain.
  cssInheritedStyleEntryInlineStyle :: Maybe CssCssStyle,
  -- | Matches of CSS rules matching the ancestor node in the style inheritance chain.
  cssInheritedStyleEntryMatchedCssRules :: [CssRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssInheritedStyleEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CssInheritedStyleEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Inherited pseudo element matches from pseudos of an ancestor node.
data CssInheritedPseudoElementMatches = CssInheritedPseudoElementMatches {
  -- | Matches of pseudo styles from the pseudos of an ancestor node.
  cssInheritedPseudoElementMatchesPseudoElements :: [CssPseudoElementMatches]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssInheritedPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  CssInheritedPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Match data for a CSS rule.
data CssRuleMatch = CssRuleMatch {
  -- | CSS rule in the match.
  cssRuleMatchRule :: CssCssRule,
  -- | Matching selector indices in the rule's selectorList selectors (0-based).
  cssRuleMatchMatchingSelectors :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssRuleMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CssRuleMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Data for a simple selector (these are delimited by commas in a selector list).
data CssValue = CssValue {
  -- | Value text.
  cssValueText :: String,
  -- | Value range in the underlying resource (if available).
  cssValueRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  CssValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }



-- | Selector list data.
data CssSelectorList = CssSelectorList {
  -- | Selectors in the list.
  cssSelectorListSelectors :: [CssValue],
  -- | Rule selector text.
  cssSelectorListText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssSelectorList  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssSelectorList where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | CSS stylesheet metainformation.
data CssCssStyleSheetHeader = CssCssStyleSheetHeader {
  -- | The stylesheet identifier.
  cssCssStyleSheetHeaderStyleSheetId :: CssStyleSheetId,
  -- | Owner frame identifier.
  cssCssStyleSheetHeaderFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
  -- | Stylesheet resource URL. Empty if this is a constructed stylesheet created using
    -- new CSSStyleSheet() (but non-empty if this is a constructed sylesheet imported
    -- as a CSS module script).
  cssCssStyleSheetHeaderSourceUrl :: String,
  -- | URL of source map associated with the stylesheet (if any).
  cssCssStyleSheetHeaderSourceMapUrl :: Maybe String,
  -- | Stylesheet origin.
  cssCssStyleSheetHeaderOrigin :: CssStyleSheetOrigin,
  -- | Stylesheet title.
  cssCssStyleSheetHeaderTitle :: String,
  -- | The backend id for the owner node of the stylesheet.
  cssCssStyleSheetHeaderOwnerNode :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  -- | Denotes whether the stylesheet is disabled.
  cssCssStyleSheetHeaderDisabled :: Bool,
  -- | Whether the sourceURL field value comes from the sourceURL comment.
  cssCssStyleSheetHeaderHasSourceUrl :: Maybe Bool,
  -- | Whether this stylesheet is created for STYLE tag by parser. This flag is not set for
    -- document.written STYLE tags.
  cssCssStyleSheetHeaderIsInline :: Bool,
  -- | Whether this stylesheet is mutable. Inline stylesheets become mutable
    -- after they have been modified via CSSOM API.
    -- <link> element's stylesheets become mutable only if DevTools modifies them.
    -- Constructed stylesheets (new CSSStyleSheet()) are mutable immediately after creation.
  cssCssStyleSheetHeaderIsMutable :: Bool,
  -- | True if this stylesheet is created through new CSSStyleSheet() or imported as a
    -- CSS module script.
  cssCssStyleSheetHeaderIsConstructed :: Bool,
  -- | Line offset of the stylesheet within the resource (zero based).
  cssCssStyleSheetHeaderStartLine :: Double,
  -- | Column offset of the stylesheet within the resource (zero based).
  cssCssStyleSheetHeaderStartColumn :: Double,
  -- | Size of the content (in characters).
  cssCssStyleSheetHeaderLength :: Double,
  -- | Line offset of the end of the stylesheet within the resource (zero based).
  cssCssStyleSheetHeaderEndLine :: Double,
  -- | Column offset of the end of the stylesheet within the resource (zero based).
  cssCssStyleSheetHeaderEndColumn :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssStyleSheetHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CssCssStyleSheetHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | CSS rule representation.
data CssCssRule = CssCssRule {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    -- stylesheet rules) this rule came from.
  cssCssRuleStyleSheetId :: Maybe CssStyleSheetId,
  -- | Rule selector data.
  cssCssRuleSelectorList :: CssSelectorList,
  -- | Parent stylesheet's origin.
  cssCssRuleOrigin :: CssStyleSheetOrigin,
  -- | Associated style declaration.
  cssCssRuleStyle :: CssCssStyle,
  -- | Media list array (for rules involving media queries). The array enumerates media queries
    -- starting with the innermost one, going outwards.
  cssCssRuleMedia :: Maybe [CssCssMedia],
  -- | Container query list array (for rules involving container queries).
    -- The array enumerates container queries starting with the innermost one, going outwards.
  cssCssRuleContainerQueries :: Maybe [CssCssContainerQuery],
  -- | @supports CSS at-rule array.
    -- The array enumerates @supports at-rules starting with the innermost one, going outwards.
  cssCssRuleSupports :: Maybe [CssCssSupports],
  -- | Cascade layer array. Contains the layer hierarchy that this rule belongs to starting
    -- with the innermost layer and going outwards.
  cssCssRuleLayers :: Maybe [CssCssLayer]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  CssCssRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }



-- | CSS coverage information.
data CssRuleUsage = CssRuleUsage {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    -- stylesheet rules) this rule came from.
  cssRuleUsageStyleSheetId :: CssStyleSheetId,
  -- | Offset of the start of the rule (including selector) from the beginning of the stylesheet.
  cssRuleUsageStartOffset :: Double,
  -- | Offset of the end of the rule body from the beginning of the stylesheet.
  cssRuleUsageEndOffset :: Double,
  -- | Indicates whether the rule was actually used by some element in the page.
  cssRuleUsageUsed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssRuleUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CssRuleUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



-- | Text range within a resource. All numbers are zero-based.
data CssSourceRange = CssSourceRange {
  -- | Start line of range.
  cssSourceRangeStartLine :: Int,
  -- | Start column of range (inclusive).
  cssSourceRangeStartColumn :: Int,
  -- | End line of range
  cssSourceRangeEndLine :: Int,
  -- | End column of range (exclusive).
  cssSourceRangeEndColumn :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssSourceRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssSourceRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'CSS.ShorthandEntry' .
data CssShorthandEntry = CssShorthandEntry {
  -- | Shorthand name.
  cssShorthandEntryName :: String,
  -- | Shorthand value.
  cssShorthandEntryValue :: String,
  -- | Whether the property has "!important" annotation (implies `false` if absent).
  cssShorthandEntryImportant :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssShorthandEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  CssShorthandEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'CSS.CSSComputedStyleProperty' .
data CssCssComputedStyleProperty = CssCssComputedStyleProperty {
  -- | Computed style property name.
  cssCssComputedStylePropertyName :: String,
  -- | Computed style property value.
  cssCssComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  CssCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | CSS style representation.
data CssCssStyle = CssCssStyle {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    -- stylesheet rules) this rule came from.
  cssCssStyleStyleSheetId :: Maybe CssStyleSheetId,
  -- | CSS properties in the style.
  cssCssStyleCssProperties :: [CssCssProperty],
  -- | Computed values for all shorthands found in the style.
  cssCssStyleShorthandEntries :: [CssShorthandEntry],
  -- | Style declaration text (if available).
  cssCssStyleCssText :: Maybe String,
  -- | Style declaration range in the enclosing stylesheet (if available).
  cssCssStyleRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | CSS property declaration data.
data CssCssProperty = CssCssProperty {
  -- | The property name.
  cssCssPropertyName :: String,
  -- | The property value.
  cssCssPropertyValue :: String,
  -- | Whether the property has "!important" annotation (implies `false` if absent).
  cssCssPropertyImportant :: Maybe Bool,
  -- | Whether the property is implicit (implies `false` if absent).
  cssCssPropertyImplicit :: Maybe Bool,
  -- | The full property text as specified in the style.
  cssCssPropertyText :: Maybe String,
  -- | Whether the property is understood by the browser (implies `true` if absent).
  cssCssPropertyParsedOk :: Maybe Bool,
  -- | Whether the property is disabled by the user (present for source-based properties only).
  cssCssPropertyDisabled :: Maybe Bool,
  -- | The entire property range in the enclosing style declaration (if available).
  cssCssPropertyRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssCssProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | CSS media rule descriptor.
data CssCssMediaSource = CssCssMediaSourceMediaRule | CssCssMediaSourceImportRule | CssCssMediaSourceLinkedSheet | CssCssMediaSourceInlineSheet
   deriving (Ord, Eq, Show, Read)
instance FromJSON CssCssMediaSource where
   parseJSON = A.withText  "CssCssMediaSource"  $ \v -> do
      case v of
         "mediaRule" -> pure CssCssMediaSourceMediaRule
         "importRule" -> pure CssCssMediaSourceImportRule
         "linkedSheet" -> pure CssCssMediaSourceLinkedSheet
         "inlineSheet" -> pure CssCssMediaSourceInlineSheet
         _ -> fail "failed to parse CssCssMediaSource"

instance ToJSON CssCssMediaSource where
   toJSON v = A.String $
      case v of
         CssCssMediaSourceMediaRule -> "mediaRule"
         CssCssMediaSourceImportRule -> "importRule"
         CssCssMediaSourceLinkedSheet -> "linkedSheet"
         CssCssMediaSourceInlineSheet -> "inlineSheet"



data CssCssMedia = CssCssMedia {
  -- | Media query text.
  cssCssMediaText :: String,
  -- | Source of the media query: "mediaRule" if specified by a @media rule, "importRule" if
    -- specified by an @import rule, "linkedSheet" if specified by a "media" attribute in a linked
    -- stylesheet's LINK tag, "inlineSheet" if specified by a "media" attribute in an inline
    -- stylesheet's STYLE tag.
  cssCssMediaSource :: CssCssMediaSource,
  -- | URL of the document containing the media query description.
  cssCssMediaSourceUrl :: Maybe String,
  -- | The associated rule (@media or @import) header range in the enclosing stylesheet (if
    -- available).
  cssCssMediaRange :: Maybe CssSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cssCssMediaStyleSheetId :: Maybe CssStyleSheetId,
  -- | Array of media queries.
  cssCssMediaMediaList :: Maybe [CssMediaQuery]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | Media query descriptor.
data CssMediaQuery = CssMediaQuery {
  -- | Array of media query expressions.
  cssMediaQueryExpressions :: [CssMediaQueryExpression],
  -- | Whether the media query condition is satisfied.
  cssMediaQueryActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssMediaQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  CssMediaQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Media query expression descriptor.
data CssMediaQueryExpression = CssMediaQueryExpression {
  -- | Media query expression value.
  cssMediaQueryExpressionValue :: Double,
  -- | Media query expression units.
  cssMediaQueryExpressionUnit :: String,
  -- | Media query expression feature.
  cssMediaQueryExpressionFeature :: String,
  -- | The associated range of the value text in the enclosing stylesheet (if available).
  cssMediaQueryExpressionValueRange :: Maybe CssSourceRange,
  -- | Computed length of media query expression (if applicable).
  cssMediaQueryExpressionComputedLength :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssMediaQueryExpression  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssMediaQueryExpression where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | CSS container query rule descriptor.
data CssCssContainerQuery = CssCssContainerQuery {
  -- | Container query text.
  cssCssContainerQueryText :: String,
  -- | The associated rule header range in the enclosing stylesheet (if
    -- available).
  cssCssContainerQueryRange :: Maybe CssSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cssCssContainerQueryStyleSheetId :: Maybe CssStyleSheetId,
  -- | Optional name for the container.
  cssCssContainerQueryName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssContainerQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssCssContainerQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | CSS Supports at-rule descriptor.
data CssCssSupports = CssCssSupports {
  -- | Supports rule text.
  cssCssSupportsText :: String,
  -- | Whether the supports condition is satisfied.
  cssCssSupportsActive :: Bool,
  -- | The associated rule header range in the enclosing stylesheet (if
    -- available).
  cssCssSupportsRange :: Maybe CssSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cssCssSupportsStyleSheetId :: Maybe CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssSupports  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssCssSupports where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | CSS Layer at-rule descriptor.
data CssCssLayer = CssCssLayer {
  -- | Layer name.
  cssCssLayerText :: String,
  -- | The associated rule header range in the enclosing stylesheet (if
    -- available).
  cssCssLayerRange :: Maybe CssSourceRange,
  -- | Identifier of the stylesheet containing this object (if exists).
  cssCssLayerStyleSheetId :: Maybe CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | CSS Layer data.
data CssCssLayerData = CssCssLayerData {
  -- | Layer name.
  cssCssLayerDataName :: String,
  -- | Direct sub-layers
  cssCssLayerDataSubLayers :: Maybe [CssCssLayerData],
  -- | Layer order. The order determines the order of the layer in the cascade order.
    -- A higher number has higher priority in the cascade order.
  cssCssLayerDataOrder :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssLayerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssCssLayerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Information about amount of glyphs that were rendered with given font.
data CssPlatformFontUsage = CssPlatformFontUsage {
  -- | Font's family name reported by platform.
  cssPlatformFontUsageFamilyName :: String,
  -- | Indicates if the font was downloaded or resolved locally.
  cssPlatformFontUsageIsCustomFont :: Bool,
  -- | Amount of glyphs that were rendered with this font.
  cssPlatformFontUsageGlyphCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssPlatformFontUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssPlatformFontUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Information about font variation axes for variable fonts
data CssFontVariationAxis = CssFontVariationAxis {
  -- | The font-variation-setting tag (a.k.a. "axis tag").
  cssFontVariationAxisTag :: String,
  -- | Human-readable variation name in the default language (normally, "en").
  cssFontVariationAxisName :: String,
  -- | The minimum value (inclusive) the font supports for this tag.
  cssFontVariationAxisMinValue :: Double,
  -- | The maximum value (inclusive) the font supports for this tag.
  cssFontVariationAxisMaxValue :: Double,
  -- | The default value.
  cssFontVariationAxisDefaultValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontVariationAxis  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssFontVariationAxis where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Properties of a web font: https://www.w3.org/TR/2008/REC-CSS2-20080411/fonts.html#font-descriptions
 -- and additional information such as platformFontFamily and fontVariationAxes.
data CssFontFace = CssFontFace {
  -- | The font-family.
  cssFontFaceFontFamily :: String,
  -- | The font-style.
  cssFontFaceFontStyle :: String,
  -- | The font-variant.
  cssFontFaceFontVariant :: String,
  -- | The font-weight.
  cssFontFaceFontWeight :: String,
  -- | The font-stretch.
  cssFontFaceFontStretch :: String,
  -- | The unicode-range.
  cssFontFaceUnicodeRange :: String,
  -- | The src.
  cssFontFaceSrc :: String,
  -- | The resolved platform font family
  cssFontFacePlatformFontFamily :: String,
  -- | Available variation settings (a.k.a. "axes").
  cssFontFaceFontVariationAxes :: Maybe [CssFontVariationAxis]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontFace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssFontFace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



-- | CSS keyframes rule representation.
data CssCssKeyframesRule = CssCssKeyframesRule {
  -- | Animation name.
  cssCssKeyframesRuleAnimationName :: CssValue,
  -- | List of keyframes.
  cssCssKeyframesRuleKeyframes :: [CssCssKeyframeRule]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  CssCssKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | CSS keyframe rule representation.
data CssCssKeyframeRule = CssCssKeyframeRule {
  -- | The css style sheet identifier (absent for user agent stylesheet and user-specified
    -- stylesheet rules) this rule came from.
  cssCssKeyframeRuleStyleSheetId :: Maybe CssStyleSheetId,
  -- | Parent stylesheet's origin.
  cssCssKeyframeRuleOrigin :: CssStyleSheetOrigin,
  -- | Associated key text.
  cssCssKeyframeRuleKeyText :: CssValue,
  -- | Associated style declaration.
  cssCssKeyframeRuleStyle :: CssCssStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssKeyframeRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CssCssKeyframeRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | A descriptor of operation to mutate style declaration text.
data CssStyleDeclarationEdit = CssStyleDeclarationEdit {
  -- | The css style sheet identifier.
  cssStyleDeclarationEditStyleSheetId :: CssStyleSheetId,
  -- | The range of the style text in the enclosing stylesheet.
  cssStyleDeclarationEditRange :: CssSourceRange,
  -- | New style text.
  cssStyleDeclarationEditText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleDeclarationEdit  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssStyleDeclarationEdit where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





-- | Type of the 'CSS.fontsUpdated' event.
data CssFontsUpdated = CssFontsUpdated {
  -- | The web font that has loaded.
  cssFontsUpdatedFont :: Maybe CssFontFace
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontsUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssFontsUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type of the 'CSS.mediaQueryResultChanged' event.
data CssMediaQueryResultChanged = CssMediaQueryResultChanged
   deriving (Eq, Show, Read)
instance FromJSON CssMediaQueryResultChanged where
   parseJSON = A.withText  "CssMediaQueryResultChanged"  $ \v -> do
      case v of
         "CssMediaQueryResultChanged" -> pure CssMediaQueryResultChanged
         _ -> fail "failed to parse CssMediaQueryResultChanged"



-- | Type of the 'CSS.styleSheetAdded' event.
data CssStyleSheetAdded = CssStyleSheetAdded {
  -- | Added stylesheet metainfo.
  cssStyleSheetAddedHeader :: CssCssStyleSheetHeader
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type of the 'CSS.styleSheetChanged' event.
data CssStyleSheetChanged = CssStyleSheetChanged {
  cssStyleSheetChangedStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type of the 'CSS.styleSheetRemoved' event.
data CssStyleSheetRemoved = CssStyleSheetRemoved {
  -- | Identifier of the removed stylesheet.
  cssStyleSheetRemovedStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Parameters of the 'cssAddRule' command.
data PCssAddRule = PCssAddRule {
  -- | The css style sheet identifier where a new rule should be inserted.
  pCssAddRuleStyleSheetId :: CssStyleSheetId,
  -- | The text of a new rule.
  pCssAddRuleRuleText :: String,
  -- | Text position of a new rule in the target style sheet.
  pCssAddRuleLocation :: CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssAddRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PCssAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


-- | Function for the 'CSS.addRule' command.
 -- Inserts a new rule with the given `ruleText` in a stylesheet with given `styleSheetId`, at the
 -- position specified by `location`.
-- Parameters: 'PCssAddRule'
-- Returns: 'CssAddRule'
cssAddRule :: Handle ev -> PCssAddRule -> IO CssAddRule
cssAddRule handle params = sendReceiveCommandResult handle "CSS.addRule" (Just params)

-- | Return type of the 'cssAddRule' command.
data CssAddRule = CssAddRule {
  -- | The newly created rule.
  cssAddRuleRule :: CssCssRule
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }

instance Command CssAddRule where
   commandName _ = "CSS.addRule"



-- | Parameters of the 'cssCollectClassNames' command.
data PCssCollectClassNames = PCssCollectClassNames {
  pCssCollectClassNamesStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssCollectClassNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'CSS.collectClassNames' command.
 -- Returns all class names from specified stylesheet.
-- Parameters: 'PCssCollectClassNames'
-- Returns: 'CssCollectClassNames'
cssCollectClassNames :: Handle ev -> PCssCollectClassNames -> IO CssCollectClassNames
cssCollectClassNames handle params = sendReceiveCommandResult handle "CSS.collectClassNames" (Just params)

-- | Return type of the 'cssCollectClassNames' command.
data CssCollectClassNames = CssCollectClassNames {
  -- | Class name list.
  cssCollectClassNamesClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssCollectClassNames where
   commandName _ = "CSS.collectClassNames"



-- | Parameters of the 'cssCreateStyleSheet' command.
data PCssCreateStyleSheet = PCssCreateStyleSheet {
  -- | Identifier of the frame where "via-inspector" stylesheet should be created.
  pCssCreateStyleSheetFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssCreateStyleSheet  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'CSS.createStyleSheet' command.
 -- Creates a new special "via-inspector" stylesheet in the frame with given `frameId`.
-- Parameters: 'PCssCreateStyleSheet'
-- Returns: 'CssCreateStyleSheet'
cssCreateStyleSheet :: Handle ev -> PCssCreateStyleSheet -> IO CssCreateStyleSheet
cssCreateStyleSheet handle params = sendReceiveCommandResult handle "CSS.createStyleSheet" (Just params)

-- | Return type of the 'cssCreateStyleSheet' command.
data CssCreateStyleSheet = CssCreateStyleSheet {
  -- | Identifier of the created "via-inspector" stylesheet.
  cssCreateStyleSheetStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command CssCreateStyleSheet where
   commandName _ = "CSS.createStyleSheet"



-- | Function for the 'CSS.disable' command.
 -- Disables the CSS agent for the given page.
cssDisable :: Handle ev -> IO ()
cssDisable handle = sendReceiveCommand handle "CSS.disable" (Nothing :: Maybe ())


-- | Function for the 'CSS.enable' command.
 -- Enables the CSS agent for the given page. Clients should not assume that the CSS agent has been
 -- enabled until the result of this command is received.
cssEnable :: Handle ev -> IO ()
cssEnable handle = sendReceiveCommand handle "CSS.enable" (Nothing :: Maybe ())


-- | Parameters of the 'cssForcePseudoState' command.
data PCssForcePseudoState = PCssForcePseudoState {
  -- | The element id for which to force the pseudo state.
  pCssForcePseudoStateNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Element pseudo classes to force when computing the element's style.
  pCssForcePseudoStateForcedPseudoClasses :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssForcePseudoState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssForcePseudoState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'CSS.forcePseudoState' command.
 -- Ensures that the given node will have specified pseudo-classes whenever its style is computed by
 -- the browser.
-- Parameters: 'PCssForcePseudoState'
cssForcePseudoState :: Handle ev -> PCssForcePseudoState -> IO ()
cssForcePseudoState handle params = sendReceiveCommand handle "CSS.forcePseudoState" (Just params)


-- | Parameters of the 'cssGetBackgroundColors' command.
data PCssGetBackgroundColors = PCssGetBackgroundColors {
  -- | Id of the node to get background colors for.
  pCssGetBackgroundColorsNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetBackgroundColors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PCssGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'CSS.getBackgroundColors' command.
-- Parameters: 'PCssGetBackgroundColors'
-- Returns: 'CssGetBackgroundColors'
cssGetBackgroundColors :: Handle ev -> PCssGetBackgroundColors -> IO CssGetBackgroundColors
cssGetBackgroundColors handle params = sendReceiveCommandResult handle "CSS.getBackgroundColors" (Just params)

-- | Return type of the 'cssGetBackgroundColors' command.
data CssGetBackgroundColors = CssGetBackgroundColors {
  -- | The range of background colors behind this element, if it contains any visible text. If no
    -- visible text is present, this will be undefined. In the case of a flat background color,
    -- this will consist of simply that color. In the case of a gradient, this will consist of each
    -- of the color stops. For anything more complicated, this will be an empty array. Images will
    -- be ignored (as if the image had failed to load).
  cssGetBackgroundColorsBackgroundColors :: Maybe [String],
  -- | The computed font size for this node, as a CSS computed value string (e.g. '12px').
  cssGetBackgroundColorsComputedFontSize :: Maybe String,
  -- | The computed font weight for this node, as a CSS computed value string (e.g. 'normal' or
    -- '100').
  cssGetBackgroundColorsComputedFontWeight :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command CssGetBackgroundColors where
   commandName _ = "CSS.getBackgroundColors"



-- | Parameters of the 'cssGetComputedStyleForNode' command.
data PCssGetComputedStyleForNode = PCssGetComputedStyleForNode {
  pCssGetComputedStyleForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetComputedStyleForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'CSS.getComputedStyleForNode' command.
 -- Returns the computed style for a DOM node identified by `nodeId`.
-- Parameters: 'PCssGetComputedStyleForNode'
-- Returns: 'CssGetComputedStyleForNode'
cssGetComputedStyleForNode :: Handle ev -> PCssGetComputedStyleForNode -> IO CssGetComputedStyleForNode
cssGetComputedStyleForNode handle params = sendReceiveCommandResult handle "CSS.getComputedStyleForNode" (Just params)

-- | Return type of the 'cssGetComputedStyleForNode' command.
data CssGetComputedStyleForNode = CssGetComputedStyleForNode {
  -- | Computed style for the specified DOM node.
  cssGetComputedStyleForNodeComputedStyle :: [CssCssComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetComputedStyleForNode where
   commandName _ = "CSS.getComputedStyleForNode"



-- | Parameters of the 'cssGetInlineStylesForNode' command.
data PCssGetInlineStylesForNode = PCssGetInlineStylesForNode {
  pCssGetInlineStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetInlineStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PCssGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'CSS.getInlineStylesForNode' command.
 -- Returns the styles defined inline (explicitly in the "style" attribute and implicitly, using DOM
 -- attributes) for a DOM node identified by `nodeId`.
-- Parameters: 'PCssGetInlineStylesForNode'
-- Returns: 'CssGetInlineStylesForNode'
cssGetInlineStylesForNode :: Handle ev -> PCssGetInlineStylesForNode -> IO CssGetInlineStylesForNode
cssGetInlineStylesForNode handle params = sendReceiveCommandResult handle "CSS.getInlineStylesForNode" (Just params)

-- | Return type of the 'cssGetInlineStylesForNode' command.
data CssGetInlineStylesForNode = CssGetInlineStylesForNode {
  -- | Inline style for the specified DOM node.
  cssGetInlineStylesForNodeInlineStyle :: Maybe CssCssStyle,
  -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
  cssGetInlineStylesForNodeAttributesStyle :: Maybe CssCssStyle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command CssGetInlineStylesForNode where
   commandName _ = "CSS.getInlineStylesForNode"



-- | Parameters of the 'cssGetMatchedStylesForNode' command.
data PCssGetMatchedStylesForNode = PCssGetMatchedStylesForNode {
  pCssGetMatchedStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetMatchedStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'CSS.getMatchedStylesForNode' command.
 -- Returns requested styles for a DOM node identified by `nodeId`.
-- Parameters: 'PCssGetMatchedStylesForNode'
-- Returns: 'CssGetMatchedStylesForNode'
cssGetMatchedStylesForNode :: Handle ev -> PCssGetMatchedStylesForNode -> IO CssGetMatchedStylesForNode
cssGetMatchedStylesForNode handle params = sendReceiveCommandResult handle "CSS.getMatchedStylesForNode" (Just params)

-- | Return type of the 'cssGetMatchedStylesForNode' command.
data CssGetMatchedStylesForNode = CssGetMatchedStylesForNode {
  -- | Inline style for the specified DOM node.
  cssGetMatchedStylesForNodeInlineStyle :: Maybe CssCssStyle,
  -- | Attribute-defined element style (e.g. resulting from "width=20 height=100%").
  cssGetMatchedStylesForNodeAttributesStyle :: Maybe CssCssStyle,
  -- | CSS rules matching this node, from all applicable stylesheets.
  cssGetMatchedStylesForNodeMatchedCssRules :: Maybe [CssRuleMatch],
  -- | Pseudo style matches for this node.
  cssGetMatchedStylesForNodePseudoElements :: Maybe [CssPseudoElementMatches],
  -- | A chain of inherited styles (from the immediate node parent up to the DOM tree root).
  cssGetMatchedStylesForNodeInherited :: Maybe [CssInheritedStyleEntry],
  -- | A chain of inherited pseudo element styles (from the immediate node parent up to the DOM tree root).
  cssGetMatchedStylesForNodeInheritedPseudoElements :: Maybe [CssInheritedPseudoElementMatches],
  -- | A list of CSS keyframed animations matching this node.
  cssGetMatchedStylesForNodeCssKeyframesRules :: Maybe [CssCssKeyframesRule]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetMatchedStylesForNode where
   commandName _ = "CSS.getMatchedStylesForNode"



-- | Function for the 'CSS.getMediaQueries' command.
 -- Returns all media queries parsed by the rendering engine.
-- Returns: 'CssGetMediaQueries'
cssGetMediaQueries :: Handle ev -> IO CssGetMediaQueries
cssGetMediaQueries handle = sendReceiveCommandResult handle "CSS.getMediaQueries" (Nothing :: Maybe ())

-- | Return type of the 'cssGetMediaQueries' command.
data CssGetMediaQueries = CssGetMediaQueries {
  cssGetMediaQueriesMedias :: [CssCssMedia]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetMediaQueries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssGetMediaQueries where
   commandName _ = "CSS.getMediaQueries"



-- | Parameters of the 'cssGetPlatformFontsForNode' command.
data PCssGetPlatformFontsForNode = PCssGetPlatformFontsForNode {
  pCssGetPlatformFontsForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetPlatformFontsForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'CSS.getPlatformFontsForNode' command.
 -- Requests information about platform fonts which we used to render child TextNodes in the given
 -- node.
-- Parameters: 'PCssGetPlatformFontsForNode'
-- Returns: 'CssGetPlatformFontsForNode'
cssGetPlatformFontsForNode :: Handle ev -> PCssGetPlatformFontsForNode -> IO CssGetPlatformFontsForNode
cssGetPlatformFontsForNode handle params = sendReceiveCommandResult handle "CSS.getPlatformFontsForNode" (Just params)

-- | Return type of the 'cssGetPlatformFontsForNode' command.
data CssGetPlatformFontsForNode = CssGetPlatformFontsForNode {
  -- | Usage statistics for every employed platform font.
  cssGetPlatformFontsForNodeFonts :: [CssPlatformFontUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetPlatformFontsForNode where
   commandName _ = "CSS.getPlatformFontsForNode"



-- | Parameters of the 'cssGetStyleSheetText' command.
data PCssGetStyleSheetText = PCssGetStyleSheetText {
  pCssGetStyleSheetTextStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'CSS.getStyleSheetText' command.
 -- Returns the current textual content for a stylesheet.
-- Parameters: 'PCssGetStyleSheetText'
-- Returns: 'CssGetStyleSheetText'
cssGetStyleSheetText :: Handle ev -> PCssGetStyleSheetText -> IO CssGetStyleSheetText
cssGetStyleSheetText handle params = sendReceiveCommandResult handle "CSS.getStyleSheetText" (Just params)

-- | Return type of the 'cssGetStyleSheetText' command.
data CssGetStyleSheetText = CssGetStyleSheetText {
  -- | The stylesheet text.
  cssGetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssGetStyleSheetText where
   commandName _ = "CSS.getStyleSheetText"



-- | Parameters of the 'cssGetLayersForNode' command.
data PCssGetLayersForNode = PCssGetLayersForNode {
  pCssGetLayersForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetLayersForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'CSS.getLayersForNode' command.
 -- Returns all layers parsed by the rendering engine for the tree scope of a node.
 -- Given a DOM element identified by nodeId, getLayersForNode returns the root
 -- layer for the nearest ancestor document or shadow root. The layer root contains
 -- the full layer tree for the tree scope and their ordering.
-- Parameters: 'PCssGetLayersForNode'
-- Returns: 'CssGetLayersForNode'
cssGetLayersForNode :: Handle ev -> PCssGetLayersForNode -> IO CssGetLayersForNode
cssGetLayersForNode handle params = sendReceiveCommandResult handle "CSS.getLayersForNode" (Just params)

-- | Return type of the 'cssGetLayersForNode' command.
data CssGetLayersForNode = CssGetLayersForNode {
  cssGetLayersForNodeRootLayer :: CssCssLayerData
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command CssGetLayersForNode where
   commandName _ = "CSS.getLayersForNode"



-- | Parameters of the 'cssTrackComputedStyleUpdates' command.
data PCssTrackComputedStyleUpdates = PCssTrackComputedStyleUpdates {
  pCssTrackComputedStyleUpdatesPropertiesToTrack :: [CssCssComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssTrackComputedStyleUpdates  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PCssTrackComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'CSS.trackComputedStyleUpdates' command.
 -- Starts tracking the given computed styles for updates. The specified array of properties
 -- replaces the one previously specified. Pass empty array to disable tracking.
 -- Use takeComputedStyleUpdates to retrieve the list of nodes that had properties modified.
 -- The changes to computed style properties are only tracked for nodes pushed to the front-end
 -- by the DOM agent. If no changes to the tracked properties occur after the node has been pushed
 -- to the front-end, no updates will be issued for the node.
-- Parameters: 'PCssTrackComputedStyleUpdates'
cssTrackComputedStyleUpdates :: Handle ev -> PCssTrackComputedStyleUpdates -> IO ()
cssTrackComputedStyleUpdates handle params = sendReceiveCommand handle "CSS.trackComputedStyleUpdates" (Just params)


-- | Function for the 'CSS.takeComputedStyleUpdates' command.
 -- Polls the next batch of computed style updates.
-- Returns: 'CssTakeComputedStyleUpdates'
cssTakeComputedStyleUpdates :: Handle ev -> IO CssTakeComputedStyleUpdates
cssTakeComputedStyleUpdates handle = sendReceiveCommandResult handle "CSS.takeComputedStyleUpdates" (Nothing :: Maybe ())

-- | Return type of the 'cssTakeComputedStyleUpdates' command.
data CssTakeComputedStyleUpdates = CssTakeComputedStyleUpdates {
  -- | The list of node Ids that have their tracked computed styles updated
  cssTakeComputedStyleUpdatesNodeIds :: [DOMPageNetworkEmulationSecurity.DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssTakeComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command CssTakeComputedStyleUpdates where
   commandName _ = "CSS.takeComputedStyleUpdates"



-- | Parameters of the 'cssSetEffectivePropertyValueForNode' command.
data PCssSetEffectivePropertyValueForNode = PCssSetEffectivePropertyValueForNode {
  -- | The element id for which to set property.
  pCssSetEffectivePropertyValueForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
  pCssSetEffectivePropertyValueForNodePropertyName :: String,
  pCssSetEffectivePropertyValueForNodeValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetEffectivePropertyValueForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PCssSetEffectivePropertyValueForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'CSS.setEffectivePropertyValueForNode' command.
 -- Find a rule with the given active property for the given node and set the new value for this
 -- property
-- Parameters: 'PCssSetEffectivePropertyValueForNode'
cssSetEffectivePropertyValueForNode :: Handle ev -> PCssSetEffectivePropertyValueForNode -> IO ()
cssSetEffectivePropertyValueForNode handle params = sendReceiveCommand handle "CSS.setEffectivePropertyValueForNode" (Just params)


-- | Parameters of the 'cssSetKeyframeKey' command.
data PCssSetKeyframeKey = PCssSetKeyframeKey {
  pCssSetKeyframeKeyStyleSheetId :: CssStyleSheetId,
  pCssSetKeyframeKeyRange :: CssSourceRange,
  pCssSetKeyframeKeyKeyText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetKeyframeKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PCssSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'CSS.setKeyframeKey' command.
 -- Modifies the keyframe rule key text.
-- Parameters: 'PCssSetKeyframeKey'
-- Returns: 'CssSetKeyframeKey'
cssSetKeyframeKey :: Handle ev -> PCssSetKeyframeKey -> IO CssSetKeyframeKey
cssSetKeyframeKey handle params = sendReceiveCommandResult handle "CSS.setKeyframeKey" (Just params)

-- | Return type of the 'cssSetKeyframeKey' command.
data CssSetKeyframeKey = CssSetKeyframeKey {
  -- | The resulting key text after modification.
  cssSetKeyframeKeyKeyText :: CssValue
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command CssSetKeyframeKey where
   commandName _ = "CSS.setKeyframeKey"



-- | Parameters of the 'cssSetMediaText' command.
data PCssSetMediaText = PCssSetMediaText {
  pCssSetMediaTextStyleSheetId :: CssStyleSheetId,
  pCssSetMediaTextRange :: CssSourceRange,
  pCssSetMediaTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetMediaText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCssSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'CSS.setMediaText' command.
 -- Modifies the rule selector.
-- Parameters: 'PCssSetMediaText'
-- Returns: 'CssSetMediaText'
cssSetMediaText :: Handle ev -> PCssSetMediaText -> IO CssSetMediaText
cssSetMediaText handle params = sendReceiveCommandResult handle "CSS.setMediaText" (Just params)

-- | Return type of the 'cssSetMediaText' command.
data CssSetMediaText = CssSetMediaText {
  -- | The resulting CSS media rule after modification.
  cssSetMediaTextMedia :: CssCssMedia
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command CssSetMediaText where
   commandName _ = "CSS.setMediaText"



-- | Parameters of the 'cssSetContainerQueryText' command.
data PCssSetContainerQueryText = PCssSetContainerQueryText {
  pCssSetContainerQueryTextStyleSheetId :: CssStyleSheetId,
  pCssSetContainerQueryTextRange :: CssSourceRange,
  pCssSetContainerQueryTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetContainerQueryText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PCssSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'CSS.setContainerQueryText' command.
 -- Modifies the expression of a container query.
-- Parameters: 'PCssSetContainerQueryText'
-- Returns: 'CssSetContainerQueryText'
cssSetContainerQueryText :: Handle ev -> PCssSetContainerQueryText -> IO CssSetContainerQueryText
cssSetContainerQueryText handle params = sendReceiveCommandResult handle "CSS.setContainerQueryText" (Just params)

-- | Return type of the 'cssSetContainerQueryText' command.
data CssSetContainerQueryText = CssSetContainerQueryText {
  -- | The resulting CSS container query rule after modification.
  cssSetContainerQueryTextContainerQuery :: CssCssContainerQuery
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command CssSetContainerQueryText where
   commandName _ = "CSS.setContainerQueryText"



-- | Parameters of the 'cssSetSupportsText' command.
data PCssSetSupportsText = PCssSetSupportsText {
  pCssSetSupportsTextStyleSheetId :: CssStyleSheetId,
  pCssSetSupportsTextRange :: CssSourceRange,
  pCssSetSupportsTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetSupportsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCssSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'CSS.setSupportsText' command.
 -- Modifies the expression of a supports at-rule.
-- Parameters: 'PCssSetSupportsText'
-- Returns: 'CssSetSupportsText'
cssSetSupportsText :: Handle ev -> PCssSetSupportsText -> IO CssSetSupportsText
cssSetSupportsText handle params = sendReceiveCommandResult handle "CSS.setSupportsText" (Just params)

-- | Return type of the 'cssSetSupportsText' command.
data CssSetSupportsText = CssSetSupportsText {
  -- | The resulting CSS Supports rule after modification.
  cssSetSupportsTextSupports :: CssCssSupports
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssSetSupportsText where
   commandName _ = "CSS.setSupportsText"



-- | Parameters of the 'cssSetRuleSelector' command.
data PCssSetRuleSelector = PCssSetRuleSelector {
  pCssSetRuleSelectorStyleSheetId :: CssStyleSheetId,
  pCssSetRuleSelectorRange :: CssSourceRange,
  pCssSetRuleSelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetRuleSelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCssSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'CSS.setRuleSelector' command.
 -- Modifies the rule selector.
-- Parameters: 'PCssSetRuleSelector'
-- Returns: 'CssSetRuleSelector'
cssSetRuleSelector :: Handle ev -> PCssSetRuleSelector -> IO CssSetRuleSelector
cssSetRuleSelector handle params = sendReceiveCommandResult handle "CSS.setRuleSelector" (Just params)

-- | Return type of the 'cssSetRuleSelector' command.
data CssSetRuleSelector = CssSetRuleSelector {
  -- | The resulting selector list after modification.
  cssSetRuleSelectorSelectorList :: CssSelectorList
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssSetRuleSelector where
   commandName _ = "CSS.setRuleSelector"



-- | Parameters of the 'cssSetStyleSheetText' command.
data PCssSetStyleSheetText = PCssSetStyleSheetText {
  pCssSetStyleSheetTextStyleSheetId :: CssStyleSheetId,
  pCssSetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'CSS.setStyleSheetText' command.
 -- Sets the new stylesheet text.
-- Parameters: 'PCssSetStyleSheetText'
-- Returns: 'CssSetStyleSheetText'
cssSetStyleSheetText :: Handle ev -> PCssSetStyleSheetText -> IO CssSetStyleSheetText
cssSetStyleSheetText handle params = sendReceiveCommandResult handle "CSS.setStyleSheetText" (Just params)

-- | Return type of the 'cssSetStyleSheetText' command.
data CssSetStyleSheetText = CssSetStyleSheetText {
  -- | URL of source map associated with script (if any).
  cssSetStyleSheetTextSourceMapUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssSetStyleSheetText where
   commandName _ = "CSS.setStyleSheetText"



-- | Parameters of the 'cssSetStyleTexts' command.
data PCssSetStyleTexts = PCssSetStyleTexts {
  pCssSetStyleTextsEdits :: [CssStyleDeclarationEdit]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetStyleTexts  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PCssSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'CSS.setStyleTexts' command.
 -- Applies specified style edits one after another in the given order.
-- Parameters: 'PCssSetStyleTexts'
-- Returns: 'CssSetStyleTexts'
cssSetStyleTexts :: Handle ev -> PCssSetStyleTexts -> IO CssSetStyleTexts
cssSetStyleTexts handle params = sendReceiveCommandResult handle "CSS.setStyleTexts" (Just params)

-- | Return type of the 'cssSetStyleTexts' command.
data CssSetStyleTexts = CssSetStyleTexts {
  -- | The resulting styles after modification.
  cssSetStyleTextsStyles :: [CssCssStyle]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command CssSetStyleTexts where
   commandName _ = "CSS.setStyleTexts"



-- | Function for the 'CSS.startRuleUsageTracking' command.
 -- Enables the selector recording.
cssStartRuleUsageTracking :: Handle ev -> IO ()
cssStartRuleUsageTracking handle = sendReceiveCommand handle "CSS.startRuleUsageTracking" (Nothing :: Maybe ())


-- | Function for the 'CSS.stopRuleUsageTracking' command.
 -- Stop tracking rule usage and return the list of rules that were used since last call to
 -- `takeCoverageDelta` (or since start of coverage instrumentation)
-- Returns: 'CssStopRuleUsageTracking'
cssStopRuleUsageTracking :: Handle ev -> IO CssStopRuleUsageTracking
cssStopRuleUsageTracking handle = sendReceiveCommandResult handle "CSS.stopRuleUsageTracking" (Nothing :: Maybe ())

-- | Return type of the 'cssStopRuleUsageTracking' command.
data CssStopRuleUsageTracking = CssStopRuleUsageTracking {
  cssStopRuleUsageTrackingRuleUsage :: [CssRuleUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssStopRuleUsageTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command CssStopRuleUsageTracking where
   commandName _ = "CSS.stopRuleUsageTracking"



-- | Function for the 'CSS.takeCoverageDelta' command.
 -- Obtain list of rules that became used since last call to this method (or since start of coverage
 -- instrumentation)
-- Returns: 'CssTakeCoverageDelta'
cssTakeCoverageDelta :: Handle ev -> IO CssTakeCoverageDelta
cssTakeCoverageDelta handle = sendReceiveCommandResult handle "CSS.takeCoverageDelta" (Nothing :: Maybe ())

-- | Return type of the 'cssTakeCoverageDelta' command.
data CssTakeCoverageDelta = CssTakeCoverageDelta {
  cssTakeCoverageDeltaCoverage :: [CssRuleUsage],
  -- | Monotonically increasing time, in seconds.
  cssTakeCoverageDeltaTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssTakeCoverageDelta where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssTakeCoverageDelta where
   commandName _ = "CSS.takeCoverageDelta"



-- | Parameters of the 'cssSetLocalFontsEnabled' command.
data PCssSetLocalFontsEnabled = PCssSetLocalFontsEnabled {
  -- | Whether rendering of local fonts is enabled.
  pCssSetLocalFontsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetLocalFontsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCssSetLocalFontsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'CSS.setLocalFontsEnabled' command.
 -- Enables/disables rendering of local CSS fonts (enabled by default).
-- Parameters: 'PCssSetLocalFontsEnabled'
cssSetLocalFontsEnabled :: Handle ev -> PCssSetLocalFontsEnabled -> IO ()
cssSetLocalFontsEnabled handle params = sendReceiveCommand handle "CSS.setLocalFontsEnabled" (Just params)



