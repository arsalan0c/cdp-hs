{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type CssStyleSheetId = String
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



data CssPseudoElementMatches = CssPseudoElementMatches {
   cssPseudoElementMatchesPseudoType :: DOMPageNetworkEmulationSecurity.DomPseudoType,
   cssPseudoElementMatchesMatches :: [CssRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data CssInheritedStyleEntry = CssInheritedStyleEntry {
   cssInheritedStyleEntryInlineStyle :: Maybe CssCssStyle,
   cssInheritedStyleEntryMatchedCssRules :: [CssRuleMatch]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssInheritedStyleEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CssInheritedStyleEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data CssInheritedPseudoElementMatches = CssInheritedPseudoElementMatches {
   cssInheritedPseudoElementMatchesPseudoElements :: [CssPseudoElementMatches]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssInheritedPseudoElementMatches  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  CssInheritedPseudoElementMatches where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



data CssRuleMatch = CssRuleMatch {
   cssRuleMatchRule :: CssCssRule,
   cssRuleMatchMatchingSelectors :: [Int]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssRuleMatch  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CssRuleMatch where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



data CssValue = CssValue {
   cssValueText :: String,
   cssValueRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssValue  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  CssValue where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }



data CssSelectorList = CssSelectorList {
   cssSelectorListSelectors :: [CssValue],
   cssSelectorListText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssSelectorList  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssSelectorList where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data CssCssStyleSheetHeader = CssCssStyleSheetHeader {
   cssCssStyleSheetHeaderStyleSheetId :: CssStyleSheetId,
   cssCssStyleSheetHeaderFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   cssCssStyleSheetHeaderSourceUrl :: String,
   cssCssStyleSheetHeaderSourceMapUrl :: Maybe String,
   cssCssStyleSheetHeaderOrigin :: CssStyleSheetOrigin,
   cssCssStyleSheetHeaderTitle :: String,
   cssCssStyleSheetHeaderOwnerNode :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   cssCssStyleSheetHeaderDisabled :: Bool,
   cssCssStyleSheetHeaderHasSourceUrl :: Maybe Bool,
   cssCssStyleSheetHeaderIsInline :: Bool,
   cssCssStyleSheetHeaderIsMutable :: Bool,
   cssCssStyleSheetHeaderIsConstructed :: Bool,
   cssCssStyleSheetHeaderStartLine :: Double,
   cssCssStyleSheetHeaderStartColumn :: Double,
   cssCssStyleSheetHeaderLength :: Double,
   cssCssStyleSheetHeaderEndLine :: Double,
   cssCssStyleSheetHeaderEndColumn :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssStyleSheetHeader  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  CssCssStyleSheetHeader where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data CssCssRule = CssCssRule {
   cssCssRuleStyleSheetId :: Maybe CssStyleSheetId,
   cssCssRuleSelectorList :: CssSelectorList,
   cssCssRuleOrigin :: CssStyleSheetOrigin,
   cssCssRuleStyle :: CssCssStyle,
   cssCssRuleMedia :: Maybe [CssCssMedia],
   cssCssRuleContainerQueries :: Maybe [CssCssContainerQuery],
   cssCssRuleSupports :: Maybe [CssCssSupports],
   cssCssRuleLayers :: Maybe [CssCssLayer]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 , A.omitNothingFields = True}

instance FromJSON  CssCssRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }



data CssRuleUsage = CssRuleUsage {
   cssRuleUsageStyleSheetId :: CssStyleSheetId,
   cssRuleUsageStartOffset :: Double,
   cssRuleUsageEndOffset :: Double,
   cssRuleUsageUsed :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssRuleUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 , A.omitNothingFields = True}

instance FromJSON  CssRuleUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 12 }



data CssSourceRange = CssSourceRange {
   cssSourceRangeStartLine :: Int,
   cssSourceRangeStartColumn :: Int,
   cssSourceRangeEndLine :: Int,
   cssSourceRangeEndColumn :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssSourceRange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssSourceRange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data CssShorthandEntry = CssShorthandEntry {
   cssShorthandEntryName :: String,
   cssShorthandEntryValue :: String,
   cssShorthandEntryImportant :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssShorthandEntry  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  CssShorthandEntry where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data CssCssComputedStyleProperty = CssCssComputedStyleProperty {
   cssCssComputedStylePropertyName :: String,
   cssCssComputedStylePropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssComputedStyleProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  CssCssComputedStyleProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data CssCssStyle = CssCssStyle {
   cssCssStyleStyleSheetId :: Maybe CssStyleSheetId,
   cssCssStyleCssProperties :: [CssCssProperty],
   cssCssStyleShorthandEntries :: [CssShorthandEntry],
   cssCssStyleCssText :: Maybe String,
   cssCssStyleRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data CssCssProperty = CssCssProperty {
   cssCssPropertyName :: String,
   cssCssPropertyValue :: String,
   cssCssPropertyImportant :: Maybe Bool,
   cssCssPropertyImplicit :: Maybe Bool,
   cssCssPropertyText :: Maybe String,
   cssCssPropertyParsedOk :: Maybe Bool,
   cssCssPropertyDisabled :: Maybe Bool,
   cssCssPropertyRange :: Maybe CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssCssProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


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
   cssCssMediaText :: String,
   cssCssMediaSource :: CssCssMediaSource,
   cssCssMediaSourceUrl :: Maybe String,
   cssCssMediaRange :: Maybe CssSourceRange,
   cssCssMediaStyleSheetId :: Maybe CssStyleSheetId,
   cssCssMediaMediaList :: Maybe [CssMediaQuery]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssMedia  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssMedia where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data CssMediaQuery = CssMediaQuery {
   cssMediaQueryExpressions :: [CssMediaQueryExpression],
   cssMediaQueryActive :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssMediaQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  CssMediaQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



data CssMediaQueryExpression = CssMediaQueryExpression {
   cssMediaQueryExpressionValue :: Double,
   cssMediaQueryExpressionUnit :: String,
   cssMediaQueryExpressionFeature :: String,
   cssMediaQueryExpressionValueRange :: Maybe CssSourceRange,
   cssMediaQueryExpressionComputedLength :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssMediaQueryExpression  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssMediaQueryExpression where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data CssCssContainerQuery = CssCssContainerQuery {
   cssCssContainerQueryText :: String,
   cssCssContainerQueryRange :: Maybe CssSourceRange,
   cssCssContainerQueryStyleSheetId :: Maybe CssStyleSheetId,
   cssCssContainerQueryName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssContainerQuery  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssCssContainerQuery where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data CssCssSupports = CssCssSupports {
   cssCssSupportsText :: String,
   cssCssSupportsActive :: Bool,
   cssCssSupportsRange :: Maybe CssSourceRange,
   cssCssSupportsStyleSheetId :: Maybe CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssSupports  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  CssCssSupports where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



data CssCssLayer = CssCssLayer {
   cssCssLayerText :: String,
   cssCssLayerRange :: Maybe CssSourceRange,
   cssCssLayerStyleSheetId :: Maybe CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssCssLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data CssCssLayerData = CssCssLayerData {
   cssCssLayerDataName :: String,
   cssCssLayerDataSubLayers :: Maybe [CssCssLayerData],
   cssCssLayerDataOrder :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssLayerData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssCssLayerData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



data CssPlatformFontUsage = CssPlatformFontUsage {
   cssPlatformFontUsageFamilyName :: String,
   cssPlatformFontUsageIsCustomFont :: Bool,
   cssPlatformFontUsageGlyphCount :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssPlatformFontUsage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssPlatformFontUsage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data CssFontVariationAxis = CssFontVariationAxis {
   cssFontVariationAxisTag :: String,
   cssFontVariationAxisName :: String,
   cssFontVariationAxisMinValue :: Double,
   cssFontVariationAxisMaxValue :: Double,
   cssFontVariationAxisDefaultValue :: Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontVariationAxis  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssFontVariationAxis where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data CssFontFace = CssFontFace {
   cssFontFaceFontFamily :: String,
   cssFontFaceFontStyle :: String,
   cssFontFaceFontVariant :: String,
   cssFontFaceFontWeight :: String,
   cssFontFaceFontStretch :: String,
   cssFontFaceUnicodeRange :: String,
   cssFontFaceSrc :: String,
   cssFontFacePlatformFontFamily :: String,
   cssFontFaceFontVariationAxes :: Maybe [CssFontVariationAxis]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontFace  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  CssFontFace where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }



data CssCssKeyframesRule = CssCssKeyframesRule {
   cssCssKeyframesRuleAnimationName :: CssValue,
   cssCssKeyframesRuleKeyframes :: [CssCssKeyframeRule]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssKeyframesRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  CssCssKeyframesRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data CssCssKeyframeRule = CssCssKeyframeRule {
   cssCssKeyframeRuleStyleSheetId :: Maybe CssStyleSheetId,
   cssCssKeyframeRuleOrigin :: CssStyleSheetOrigin,
   cssCssKeyframeRuleKeyText :: CssValue,
   cssCssKeyframeRuleStyle :: CssCssStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssCssKeyframeRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CssCssKeyframeRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data CssStyleDeclarationEdit = CssStyleDeclarationEdit {
   cssStyleDeclarationEditStyleSheetId :: CssStyleSheetId,
   cssStyleDeclarationEditRange :: CssSourceRange,
   cssStyleDeclarationEditText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleDeclarationEdit  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  CssStyleDeclarationEdit where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





data CssFontsUpdated = CssFontsUpdated {
   cssFontsUpdatedFont :: Maybe CssFontFace
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssFontsUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  CssFontsUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


data CssMediaQueryResultChanged = CssMediaQueryResultChanged
   deriving (Eq, Show, Read)
instance FromJSON CssMediaQueryResultChanged where
   parseJSON = A.withText  "CssMediaQueryResultChanged"  $ \v -> do
      case v of
         "CssMediaQueryResultChanged" -> pure CssMediaQueryResultChanged
         _ -> fail "failed to parse CssMediaQueryResultChanged"



data CssStyleSheetAdded = CssStyleSheetAdded {
   cssStyleSheetAddedHeader :: CssCssStyleSheetHeader
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data CssStyleSheetChanged = CssStyleSheetChanged {
   cssStyleSheetChangedStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data CssStyleSheetRemoved = CssStyleSheetRemoved {
   cssStyleSheetRemovedStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON CssStyleSheetRemoved  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  CssStyleSheetRemoved where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





data PCssAddRule = PCssAddRule {
   pCssAddRuleStyleSheetId :: CssStyleSheetId,
   pCssAddRuleRuleText :: String,
   pCssAddRuleLocation :: CssSourceRange
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssAddRule  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PCssAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


cssAddRule :: Handle ev -> PCssAddRule -> IO (Either Error CssAddRule)
cssAddRule handle params = sendReceiveCommandResult handle "CSS.addRule" (Just params)

data CssAddRule = CssAddRule {
   cssAddRuleRule :: CssCssRule
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssAddRule where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 10 }

instance Command CssAddRule where
   commandName _ = "CSS.addRule"




data PCssCollectClassNames = PCssCollectClassNames {
   pCssCollectClassNamesStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssCollectClassNames  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


cssCollectClassNames :: Handle ev -> PCssCollectClassNames -> IO (Either Error CssCollectClassNames)
cssCollectClassNames handle params = sendReceiveCommandResult handle "CSS.collectClassNames" (Just params)

data CssCollectClassNames = CssCollectClassNames {
   cssCollectClassNamesClassNames :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssCollectClassNames where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssCollectClassNames where
   commandName _ = "CSS.collectClassNames"




data PCssCreateStyleSheet = PCssCreateStyleSheet {
   pCssCreateStyleSheetFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssCreateStyleSheet  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


cssCreateStyleSheet :: Handle ev -> PCssCreateStyleSheet -> IO (Either Error CssCreateStyleSheet)
cssCreateStyleSheet handle params = sendReceiveCommandResult handle "CSS.createStyleSheet" (Just params)

data CssCreateStyleSheet = CssCreateStyleSheet {
   cssCreateStyleSheetStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssCreateStyleSheet where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command CssCreateStyleSheet where
   commandName _ = "CSS.createStyleSheet"



cssDisable :: Handle ev -> IO (Maybe Error)
cssDisable handle = sendReceiveCommand handle "CSS.disable" (Nothing :: Maybe ())


cssEnable :: Handle ev -> IO (Maybe Error)
cssEnable handle = sendReceiveCommand handle "CSS.enable" (Nothing :: Maybe ())



data PCssForcePseudoState = PCssForcePseudoState {
   pCssForcePseudoStateNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
   pCssForcePseudoStateForcedPseudoClasses :: [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssForcePseudoState  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssForcePseudoState where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


cssForcePseudoState :: Handle ev -> PCssForcePseudoState -> IO (Maybe Error)
cssForcePseudoState handle params = sendReceiveCommand handle "CSS.forcePseudoState" (Just params)



data PCssGetBackgroundColors = PCssGetBackgroundColors {
   pCssGetBackgroundColorsNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetBackgroundColors  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PCssGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


cssGetBackgroundColors :: Handle ev -> PCssGetBackgroundColors -> IO (Either Error CssGetBackgroundColors)
cssGetBackgroundColors handle params = sendReceiveCommandResult handle "CSS.getBackgroundColors" (Just params)

data CssGetBackgroundColors = CssGetBackgroundColors {
   cssGetBackgroundColorsBackgroundColors :: Maybe [String],
   cssGetBackgroundColorsComputedFontSize :: Maybe String,
   cssGetBackgroundColorsComputedFontWeight :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetBackgroundColors where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command CssGetBackgroundColors where
   commandName _ = "CSS.getBackgroundColors"




data PCssGetComputedStyleForNode = PCssGetComputedStyleForNode {
   pCssGetComputedStyleForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetComputedStyleForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


cssGetComputedStyleForNode :: Handle ev -> PCssGetComputedStyleForNode -> IO (Either Error CssGetComputedStyleForNode)
cssGetComputedStyleForNode handle params = sendReceiveCommandResult handle "CSS.getComputedStyleForNode" (Just params)

data CssGetComputedStyleForNode = CssGetComputedStyleForNode {
   cssGetComputedStyleForNodeComputedStyle :: [CssCssComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetComputedStyleForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetComputedStyleForNode where
   commandName _ = "CSS.getComputedStyleForNode"




data PCssGetInlineStylesForNode = PCssGetInlineStylesForNode {
   pCssGetInlineStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetInlineStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PCssGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


cssGetInlineStylesForNode :: Handle ev -> PCssGetInlineStylesForNode -> IO (Either Error CssGetInlineStylesForNode)
cssGetInlineStylesForNode handle params = sendReceiveCommandResult handle "CSS.getInlineStylesForNode" (Just params)

data CssGetInlineStylesForNode = CssGetInlineStylesForNode {
   cssGetInlineStylesForNodeInlineStyle :: Maybe CssCssStyle,
   cssGetInlineStylesForNodeAttributesStyle :: Maybe CssCssStyle
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetInlineStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command CssGetInlineStylesForNode where
   commandName _ = "CSS.getInlineStylesForNode"




data PCssGetMatchedStylesForNode = PCssGetMatchedStylesForNode {
   pCssGetMatchedStylesForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetMatchedStylesForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


cssGetMatchedStylesForNode :: Handle ev -> PCssGetMatchedStylesForNode -> IO (Either Error CssGetMatchedStylesForNode)
cssGetMatchedStylesForNode handle params = sendReceiveCommandResult handle "CSS.getMatchedStylesForNode" (Just params)

data CssGetMatchedStylesForNode = CssGetMatchedStylesForNode {
   cssGetMatchedStylesForNodeInlineStyle :: Maybe CssCssStyle,
   cssGetMatchedStylesForNodeAttributesStyle :: Maybe CssCssStyle,
   cssGetMatchedStylesForNodeMatchedCssRules :: Maybe [CssRuleMatch],
   cssGetMatchedStylesForNodePseudoElements :: Maybe [CssPseudoElementMatches],
   cssGetMatchedStylesForNodeInherited :: Maybe [CssInheritedStyleEntry],
   cssGetMatchedStylesForNodeInheritedPseudoElements :: Maybe [CssInheritedPseudoElementMatches],
   cssGetMatchedStylesForNodeCssKeyframesRules :: Maybe [CssCssKeyframesRule]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetMatchedStylesForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetMatchedStylesForNode where
   commandName _ = "CSS.getMatchedStylesForNode"



cssGetMediaQueries :: Handle ev -> IO (Either Error CssGetMediaQueries)
cssGetMediaQueries handle = sendReceiveCommandResult handle "CSS.getMediaQueries" (Nothing :: Maybe ())

data CssGetMediaQueries = CssGetMediaQueries {
   cssGetMediaQueriesMedias :: [CssCssMedia]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetMediaQueries where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssGetMediaQueries where
   commandName _ = "CSS.getMediaQueries"




data PCssGetPlatformFontsForNode = PCssGetPlatformFontsForNode {
   pCssGetPlatformFontsForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetPlatformFontsForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PCssGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


cssGetPlatformFontsForNode :: Handle ev -> PCssGetPlatformFontsForNode -> IO (Either Error CssGetPlatformFontsForNode)
cssGetPlatformFontsForNode handle params = sendReceiveCommandResult handle "CSS.getPlatformFontsForNode" (Just params)

data CssGetPlatformFontsForNode = CssGetPlatformFontsForNode {
   cssGetPlatformFontsForNodeFonts :: [CssPlatformFontUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetPlatformFontsForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command CssGetPlatformFontsForNode where
   commandName _ = "CSS.getPlatformFontsForNode"




data PCssGetStyleSheetText = PCssGetStyleSheetText {
   pCssGetStyleSheetTextStyleSheetId :: CssStyleSheetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


cssGetStyleSheetText :: Handle ev -> PCssGetStyleSheetText -> IO (Either Error CssGetStyleSheetText)
cssGetStyleSheetText handle params = sendReceiveCommandResult handle "CSS.getStyleSheetText" (Just params)

data CssGetStyleSheetText = CssGetStyleSheetText {
   cssGetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssGetStyleSheetText where
   commandName _ = "CSS.getStyleSheetText"




data PCssGetLayersForNode = PCssGetLayersForNode {
   pCssGetLayersForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssGetLayersForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PCssGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


cssGetLayersForNode :: Handle ev -> PCssGetLayersForNode -> IO (Either Error CssGetLayersForNode)
cssGetLayersForNode handle params = sendReceiveCommandResult handle "CSS.getLayersForNode" (Just params)

data CssGetLayersForNode = CssGetLayersForNode {
   cssGetLayersForNodeRootLayer :: CssCssLayerData
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssGetLayersForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command CssGetLayersForNode where
   commandName _ = "CSS.getLayersForNode"




data PCssTrackComputedStyleUpdates = PCssTrackComputedStyleUpdates {
   pCssTrackComputedStyleUpdatesPropertiesToTrack :: [CssCssComputedStyleProperty]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssTrackComputedStyleUpdates  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PCssTrackComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


cssTrackComputedStyleUpdates :: Handle ev -> PCssTrackComputedStyleUpdates -> IO (Maybe Error)
cssTrackComputedStyleUpdates handle params = sendReceiveCommand handle "CSS.trackComputedStyleUpdates" (Just params)


cssTakeComputedStyleUpdates :: Handle ev -> IO (Either Error CssTakeComputedStyleUpdates)
cssTakeComputedStyleUpdates handle = sendReceiveCommandResult handle "CSS.takeComputedStyleUpdates" (Nothing :: Maybe ())

data CssTakeComputedStyleUpdates = CssTakeComputedStyleUpdates {
   cssTakeComputedStyleUpdatesNodeIds :: [DOMPageNetworkEmulationSecurity.DomNodeId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssTakeComputedStyleUpdates where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command CssTakeComputedStyleUpdates where
   commandName _ = "CSS.takeComputedStyleUpdates"




data PCssSetEffectivePropertyValueForNode = PCssSetEffectivePropertyValueForNode {
   pCssSetEffectivePropertyValueForNodeNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
   pCssSetEffectivePropertyValueForNodePropertyName :: String,
   pCssSetEffectivePropertyValueForNodeValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetEffectivePropertyValueForNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  PCssSetEffectivePropertyValueForNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


cssSetEffectivePropertyValueForNode :: Handle ev -> PCssSetEffectivePropertyValueForNode -> IO (Maybe Error)
cssSetEffectivePropertyValueForNode handle params = sendReceiveCommand handle "CSS.setEffectivePropertyValueForNode" (Just params)



data PCssSetKeyframeKey = PCssSetKeyframeKey {
   pCssSetKeyframeKeyStyleSheetId :: CssStyleSheetId,
   pCssSetKeyframeKeyRange :: CssSourceRange,
   pCssSetKeyframeKeyKeyText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetKeyframeKey  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PCssSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


cssSetKeyframeKey :: Handle ev -> PCssSetKeyframeKey -> IO (Either Error CssSetKeyframeKey)
cssSetKeyframeKey handle params = sendReceiveCommandResult handle "CSS.setKeyframeKey" (Just params)

data CssSetKeyframeKey = CssSetKeyframeKey {
   cssSetKeyframeKeyKeyText :: CssValue
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetKeyframeKey where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command CssSetKeyframeKey where
   commandName _ = "CSS.setKeyframeKey"




data PCssSetMediaText = PCssSetMediaText {
   pCssSetMediaTextStyleSheetId :: CssStyleSheetId,
   pCssSetMediaTextRange :: CssSourceRange,
   pCssSetMediaTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetMediaText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCssSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


cssSetMediaText :: Handle ev -> PCssSetMediaText -> IO (Either Error CssSetMediaText)
cssSetMediaText handle params = sendReceiveCommandResult handle "CSS.setMediaText" (Just params)

data CssSetMediaText = CssSetMediaText {
   cssSetMediaTextMedia :: CssCssMedia
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetMediaText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }

instance Command CssSetMediaText where
   commandName _ = "CSS.setMediaText"




data PCssSetContainerQueryText = PCssSetContainerQueryText {
   pCssSetContainerQueryTextStyleSheetId :: CssStyleSheetId,
   pCssSetContainerQueryTextRange :: CssSourceRange,
   pCssSetContainerQueryTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetContainerQueryText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PCssSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


cssSetContainerQueryText :: Handle ev -> PCssSetContainerQueryText -> IO (Either Error CssSetContainerQueryText)
cssSetContainerQueryText handle params = sendReceiveCommandResult handle "CSS.setContainerQueryText" (Just params)

data CssSetContainerQueryText = CssSetContainerQueryText {
   cssSetContainerQueryTextContainerQuery :: CssCssContainerQuery
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetContainerQueryText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command CssSetContainerQueryText where
   commandName _ = "CSS.setContainerQueryText"




data PCssSetSupportsText = PCssSetSupportsText {
   pCssSetSupportsTextStyleSheetId :: CssStyleSheetId,
   pCssSetSupportsTextRange :: CssSourceRange,
   pCssSetSupportsTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetSupportsText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCssSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


cssSetSupportsText :: Handle ev -> PCssSetSupportsText -> IO (Either Error CssSetSupportsText)
cssSetSupportsText handle params = sendReceiveCommandResult handle "CSS.setSupportsText" (Just params)

data CssSetSupportsText = CssSetSupportsText {
   cssSetSupportsTextSupports :: CssCssSupports
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetSupportsText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssSetSupportsText where
   commandName _ = "CSS.setSupportsText"




data PCssSetRuleSelector = PCssSetRuleSelector {
   pCssSetRuleSelectorStyleSheetId :: CssStyleSheetId,
   pCssSetRuleSelectorRange :: CssSourceRange,
   pCssSetRuleSelectorSelector :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetRuleSelector  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PCssSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


cssSetRuleSelector :: Handle ev -> PCssSetRuleSelector -> IO (Either Error CssSetRuleSelector)
cssSetRuleSelector handle params = sendReceiveCommandResult handle "CSS.setRuleSelector" (Just params)

data CssSetRuleSelector = CssSetRuleSelector {
   cssSetRuleSelectorSelectorList :: CssSelectorList
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetRuleSelector where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command CssSetRuleSelector where
   commandName _ = "CSS.setRuleSelector"




data PCssSetStyleSheetText = PCssSetStyleSheetText {
   pCssSetStyleSheetTextStyleSheetId :: CssStyleSheetId,
   pCssSetStyleSheetTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetStyleSheetText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PCssSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


cssSetStyleSheetText :: Handle ev -> PCssSetStyleSheetText -> IO (Either Error CssSetStyleSheetText)
cssSetStyleSheetText handle params = sendReceiveCommandResult handle "CSS.setStyleSheetText" (Just params)

data CssSetStyleSheetText = CssSetStyleSheetText {
   cssSetStyleSheetTextSourceMapUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetStyleSheetText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssSetStyleSheetText where
   commandName _ = "CSS.setStyleSheetText"




data PCssSetStyleTexts = PCssSetStyleTexts {
   pCssSetStyleTextsEdits :: [CssStyleDeclarationEdit]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetStyleTexts  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PCssSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


cssSetStyleTexts :: Handle ev -> PCssSetStyleTexts -> IO (Either Error CssSetStyleTexts)
cssSetStyleTexts handle params = sendReceiveCommandResult handle "CSS.setStyleTexts" (Just params)

data CssSetStyleTexts = CssSetStyleTexts {
   cssSetStyleTextsStyles :: [CssCssStyle]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssSetStyleTexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command CssSetStyleTexts where
   commandName _ = "CSS.setStyleTexts"



cssStartRuleUsageTracking :: Handle ev -> IO (Maybe Error)
cssStartRuleUsageTracking handle = sendReceiveCommand handle "CSS.startRuleUsageTracking" (Nothing :: Maybe ())


cssStopRuleUsageTracking :: Handle ev -> IO (Either Error CssStopRuleUsageTracking)
cssStopRuleUsageTracking handle = sendReceiveCommandResult handle "CSS.stopRuleUsageTracking" (Nothing :: Maybe ())

data CssStopRuleUsageTracking = CssStopRuleUsageTracking {
   cssStopRuleUsageTrackingRuleUsage :: [CssRuleUsage]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssStopRuleUsageTracking where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command CssStopRuleUsageTracking where
   commandName _ = "CSS.stopRuleUsageTracking"



cssTakeCoverageDelta :: Handle ev -> IO (Either Error CssTakeCoverageDelta)
cssTakeCoverageDelta handle = sendReceiveCommandResult handle "CSS.takeCoverageDelta" (Nothing :: Maybe ())

data CssTakeCoverageDelta = CssTakeCoverageDelta {
   cssTakeCoverageDeltaCoverage :: [CssRuleUsage],
   cssTakeCoverageDeltaTimestamp :: Double
} deriving (Generic, Eq, Show, Read)

instance FromJSON  CssTakeCoverageDelta where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command CssTakeCoverageDelta where
   commandName _ = "CSS.takeCoverageDelta"




data PCssSetLocalFontsEnabled = PCssSetLocalFontsEnabled {
   pCssSetLocalFontsEnabledEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCssSetLocalFontsEnabled  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PCssSetLocalFontsEnabled where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


cssSetLocalFontsEnabled :: Handle ev -> PCssSetLocalFontsEnabled -> IO (Maybe Error)
cssSetLocalFontsEnabled handle params = sendReceiveCommand handle "CSS.setLocalFontsEnabled" (Just params)



