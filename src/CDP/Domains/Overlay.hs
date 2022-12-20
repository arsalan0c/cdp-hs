{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Overlay

This domain provides various functionality related to drawing atop the inspected page.
-}


module CDP.Domains.Overlay (module CDP.Domains.Overlay) where

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


-- | Type 'Overlay.SourceOrderConfig'.
--   Configuration data for drawing the source order of an elements children.
data OverlaySourceOrderConfig = OverlaySourceOrderConfig
  {
    -- | the color to outline the givent element in.
    overlaySourceOrderConfigParentOutlineColor :: DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | the color to outline the child elements in.
    overlaySourceOrderConfigChildOutlineColor :: DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlaySourceOrderConfig where
  parseJSON = A.withObject "OverlaySourceOrderConfig" $ \o -> OverlaySourceOrderConfig
    <$> o A..: "parentOutlineColor"
    <*> o A..: "childOutlineColor"
instance ToJSON OverlaySourceOrderConfig where
  toJSON p = A.object $ catMaybes [
    ("parentOutlineColor" A..=) <$> Just (overlaySourceOrderConfigParentOutlineColor p),
    ("childOutlineColor" A..=) <$> Just (overlaySourceOrderConfigChildOutlineColor p)
    ]

-- | Type 'Overlay.GridHighlightConfig'.
--   Configuration data for the highlighting of Grid elements.
data OverlayGridHighlightConfig = OverlayGridHighlightConfig
  {
    -- | Whether the extension lines from grid cells to the rulers should be shown (default: false).
    overlayGridHighlightConfigShowGridExtensionLines :: Maybe Bool,
    -- | Show Positive line number labels (default: false).
    overlayGridHighlightConfigShowPositiveLineNumbers :: Maybe Bool,
    -- | Show Negative line number labels (default: false).
    overlayGridHighlightConfigShowNegativeLineNumbers :: Maybe Bool,
    -- | Show area name labels (default: false).
    overlayGridHighlightConfigShowAreaNames :: Maybe Bool,
    -- | Show line name labels (default: false).
    overlayGridHighlightConfigShowLineNames :: Maybe Bool,
    -- | Show track size labels (default: false).
    overlayGridHighlightConfigShowTrackSizes :: Maybe Bool,
    -- | The grid container border highlight color (default: transparent).
    overlayGridHighlightConfigGridBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The row line color (default: transparent).
    overlayGridHighlightConfigRowLineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The column line color (default: transparent).
    overlayGridHighlightConfigColumnLineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | Whether the grid border is dashed (default: false).
    overlayGridHighlightConfigGridBorderDash :: Maybe Bool,
    -- | Whether row lines are dashed (default: false).
    overlayGridHighlightConfigRowLineDash :: Maybe Bool,
    -- | Whether column lines are dashed (default: false).
    overlayGridHighlightConfigColumnLineDash :: Maybe Bool,
    -- | The row gap highlight fill color (default: transparent).
    overlayGridHighlightConfigRowGapColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The row gap hatching fill color (default: transparent).
    overlayGridHighlightConfigRowHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The column gap highlight fill color (default: transparent).
    overlayGridHighlightConfigColumnGapColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The column gap hatching fill color (default: transparent).
    overlayGridHighlightConfigColumnHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The named grid areas border color (Default: transparent).
    overlayGridHighlightConfigAreaBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The grid container background color (Default: transparent).
    overlayGridHighlightConfigGridBackgroundColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlayGridHighlightConfig where
  parseJSON = A.withObject "OverlayGridHighlightConfig" $ \o -> OverlayGridHighlightConfig
    <$> o A..:? "showGridExtensionLines"
    <*> o A..:? "showPositiveLineNumbers"
    <*> o A..:? "showNegativeLineNumbers"
    <*> o A..:? "showAreaNames"
    <*> o A..:? "showLineNames"
    <*> o A..:? "showTrackSizes"
    <*> o A..:? "gridBorderColor"
    <*> o A..:? "rowLineColor"
    <*> o A..:? "columnLineColor"
    <*> o A..:? "gridBorderDash"
    <*> o A..:? "rowLineDash"
    <*> o A..:? "columnLineDash"
    <*> o A..:? "rowGapColor"
    <*> o A..:? "rowHatchColor"
    <*> o A..:? "columnGapColor"
    <*> o A..:? "columnHatchColor"
    <*> o A..:? "areaBorderColor"
    <*> o A..:? "gridBackgroundColor"
instance ToJSON OverlayGridHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("showGridExtensionLines" A..=) <$> (overlayGridHighlightConfigShowGridExtensionLines p),
    ("showPositiveLineNumbers" A..=) <$> (overlayGridHighlightConfigShowPositiveLineNumbers p),
    ("showNegativeLineNumbers" A..=) <$> (overlayGridHighlightConfigShowNegativeLineNumbers p),
    ("showAreaNames" A..=) <$> (overlayGridHighlightConfigShowAreaNames p),
    ("showLineNames" A..=) <$> (overlayGridHighlightConfigShowLineNames p),
    ("showTrackSizes" A..=) <$> (overlayGridHighlightConfigShowTrackSizes p),
    ("gridBorderColor" A..=) <$> (overlayGridHighlightConfigGridBorderColor p),
    ("rowLineColor" A..=) <$> (overlayGridHighlightConfigRowLineColor p),
    ("columnLineColor" A..=) <$> (overlayGridHighlightConfigColumnLineColor p),
    ("gridBorderDash" A..=) <$> (overlayGridHighlightConfigGridBorderDash p),
    ("rowLineDash" A..=) <$> (overlayGridHighlightConfigRowLineDash p),
    ("columnLineDash" A..=) <$> (overlayGridHighlightConfigColumnLineDash p),
    ("rowGapColor" A..=) <$> (overlayGridHighlightConfigRowGapColor p),
    ("rowHatchColor" A..=) <$> (overlayGridHighlightConfigRowHatchColor p),
    ("columnGapColor" A..=) <$> (overlayGridHighlightConfigColumnGapColor p),
    ("columnHatchColor" A..=) <$> (overlayGridHighlightConfigColumnHatchColor p),
    ("areaBorderColor" A..=) <$> (overlayGridHighlightConfigAreaBorderColor p),
    ("gridBackgroundColor" A..=) <$> (overlayGridHighlightConfigGridBackgroundColor p)
    ]

-- | Type 'Overlay.FlexContainerHighlightConfig'.
--   Configuration data for the highlighting of Flex container elements.
data OverlayFlexContainerHighlightConfig = OverlayFlexContainerHighlightConfig
  {
    -- | The style of the container border
    overlayFlexContainerHighlightConfigContainerBorder :: Maybe OverlayLineStyle,
    -- | The style of the separator between lines
    overlayFlexContainerHighlightConfigLineSeparator :: Maybe OverlayLineStyle,
    -- | The style of the separator between items
    overlayFlexContainerHighlightConfigItemSeparator :: Maybe OverlayLineStyle,
    -- | Style of content-distribution space on the main axis (justify-content).
    overlayFlexContainerHighlightConfigMainDistributedSpace :: Maybe OverlayBoxStyle,
    -- | Style of content-distribution space on the cross axis (align-content).
    overlayFlexContainerHighlightConfigCrossDistributedSpace :: Maybe OverlayBoxStyle,
    -- | Style of empty space caused by row gaps (gap/row-gap).
    overlayFlexContainerHighlightConfigRowGapSpace :: Maybe OverlayBoxStyle,
    -- | Style of empty space caused by columns gaps (gap/column-gap).
    overlayFlexContainerHighlightConfigColumnGapSpace :: Maybe OverlayBoxStyle,
    -- | Style of the self-alignment line (align-items).
    overlayFlexContainerHighlightConfigCrossAlignment :: Maybe OverlayLineStyle
  }
  deriving (Eq, Show)
instance FromJSON OverlayFlexContainerHighlightConfig where
  parseJSON = A.withObject "OverlayFlexContainerHighlightConfig" $ \o -> OverlayFlexContainerHighlightConfig
    <$> o A..:? "containerBorder"
    <*> o A..:? "lineSeparator"
    <*> o A..:? "itemSeparator"
    <*> o A..:? "mainDistributedSpace"
    <*> o A..:? "crossDistributedSpace"
    <*> o A..:? "rowGapSpace"
    <*> o A..:? "columnGapSpace"
    <*> o A..:? "crossAlignment"
instance ToJSON OverlayFlexContainerHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("containerBorder" A..=) <$> (overlayFlexContainerHighlightConfigContainerBorder p),
    ("lineSeparator" A..=) <$> (overlayFlexContainerHighlightConfigLineSeparator p),
    ("itemSeparator" A..=) <$> (overlayFlexContainerHighlightConfigItemSeparator p),
    ("mainDistributedSpace" A..=) <$> (overlayFlexContainerHighlightConfigMainDistributedSpace p),
    ("crossDistributedSpace" A..=) <$> (overlayFlexContainerHighlightConfigCrossDistributedSpace p),
    ("rowGapSpace" A..=) <$> (overlayFlexContainerHighlightConfigRowGapSpace p),
    ("columnGapSpace" A..=) <$> (overlayFlexContainerHighlightConfigColumnGapSpace p),
    ("crossAlignment" A..=) <$> (overlayFlexContainerHighlightConfigCrossAlignment p)
    ]

-- | Type 'Overlay.FlexItemHighlightConfig'.
--   Configuration data for the highlighting of Flex item elements.
data OverlayFlexItemHighlightConfig = OverlayFlexItemHighlightConfig
  {
    -- | Style of the box representing the item's base size
    overlayFlexItemHighlightConfigBaseSizeBox :: Maybe OverlayBoxStyle,
    -- | Style of the border around the box representing the item's base size
    overlayFlexItemHighlightConfigBaseSizeBorder :: Maybe OverlayLineStyle,
    -- | Style of the arrow representing if the item grew or shrank
    overlayFlexItemHighlightConfigFlexibilityArrow :: Maybe OverlayLineStyle
  }
  deriving (Eq, Show)
instance FromJSON OverlayFlexItemHighlightConfig where
  parseJSON = A.withObject "OverlayFlexItemHighlightConfig" $ \o -> OverlayFlexItemHighlightConfig
    <$> o A..:? "baseSizeBox"
    <*> o A..:? "baseSizeBorder"
    <*> o A..:? "flexibilityArrow"
instance ToJSON OverlayFlexItemHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("baseSizeBox" A..=) <$> (overlayFlexItemHighlightConfigBaseSizeBox p),
    ("baseSizeBorder" A..=) <$> (overlayFlexItemHighlightConfigBaseSizeBorder p),
    ("flexibilityArrow" A..=) <$> (overlayFlexItemHighlightConfigFlexibilityArrow p)
    ]

-- | Type 'Overlay.LineStyle'.
--   Style information for drawing a line.
data OverlayLineStylePattern = OverlayLineStylePatternDashed | OverlayLineStylePatternDotted
  deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayLineStylePattern where
  parseJSON = A.withText "OverlayLineStylePattern" $ \v -> case v of
    "dashed" -> pure OverlayLineStylePatternDashed
    "dotted" -> pure OverlayLineStylePatternDotted
    "_" -> fail "failed to parse OverlayLineStylePattern"
instance ToJSON OverlayLineStylePattern where
  toJSON v = A.String $ case v of
    OverlayLineStylePatternDashed -> "dashed"
    OverlayLineStylePatternDotted -> "dotted"
data OverlayLineStyle = OverlayLineStyle
  {
    -- | The color of the line (default: transparent)
    overlayLineStyleColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The line pattern (default: solid)
    overlayLineStylePattern :: Maybe OverlayLineStylePattern
  }
  deriving (Eq, Show)
instance FromJSON OverlayLineStyle where
  parseJSON = A.withObject "OverlayLineStyle" $ \o -> OverlayLineStyle
    <$> o A..:? "color"
    <*> o A..:? "pattern"
instance ToJSON OverlayLineStyle where
  toJSON p = A.object $ catMaybes [
    ("color" A..=) <$> (overlayLineStyleColor p),
    ("pattern" A..=) <$> (overlayLineStylePattern p)
    ]

-- | Type 'Overlay.BoxStyle'.
--   Style information for drawing a box.
data OverlayBoxStyle = OverlayBoxStyle
  {
    -- | The background color for the box (default: transparent)
    overlayBoxStyleFillColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The hatching color for the box (default: transparent)
    overlayBoxStyleHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlayBoxStyle where
  parseJSON = A.withObject "OverlayBoxStyle" $ \o -> OverlayBoxStyle
    <$> o A..:? "fillColor"
    <*> o A..:? "hatchColor"
instance ToJSON OverlayBoxStyle where
  toJSON p = A.object $ catMaybes [
    ("fillColor" A..=) <$> (overlayBoxStyleFillColor p),
    ("hatchColor" A..=) <$> (overlayBoxStyleHatchColor p)
    ]

-- | Type 'Overlay.ContrastAlgorithm'.
data OverlayContrastAlgorithm = OverlayContrastAlgorithmAa | OverlayContrastAlgorithmAaa | OverlayContrastAlgorithmApca
  deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayContrastAlgorithm where
  parseJSON = A.withText "OverlayContrastAlgorithm" $ \v -> case v of
    "aa" -> pure OverlayContrastAlgorithmAa
    "aaa" -> pure OverlayContrastAlgorithmAaa
    "apca" -> pure OverlayContrastAlgorithmApca
    "_" -> fail "failed to parse OverlayContrastAlgorithm"
instance ToJSON OverlayContrastAlgorithm where
  toJSON v = A.String $ case v of
    OverlayContrastAlgorithmAa -> "aa"
    OverlayContrastAlgorithmAaa -> "aaa"
    OverlayContrastAlgorithmApca -> "apca"

-- | Type 'Overlay.HighlightConfig'.
--   Configuration data for the highlighting of page elements.
data OverlayHighlightConfig = OverlayHighlightConfig
  {
    -- | Whether the node info tooltip should be shown (default: false).
    overlayHighlightConfigShowInfo :: Maybe Bool,
    -- | Whether the node styles in the tooltip (default: false).
    overlayHighlightConfigShowStyles :: Maybe Bool,
    -- | Whether the rulers should be shown (default: false).
    overlayHighlightConfigShowRulers :: Maybe Bool,
    -- | Whether the a11y info should be shown (default: true).
    overlayHighlightConfigShowAccessibilityInfo :: Maybe Bool,
    -- | Whether the extension lines from node to the rulers should be shown (default: false).
    overlayHighlightConfigShowExtensionLines :: Maybe Bool,
    -- | The content box highlight fill color (default: transparent).
    overlayHighlightConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The padding highlight fill color (default: transparent).
    overlayHighlightConfigPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The border highlight fill color (default: transparent).
    overlayHighlightConfigBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The margin highlight fill color (default: transparent).
    overlayHighlightConfigMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The event target element highlight fill color (default: transparent).
    overlayHighlightConfigEventTargetColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The shape outside fill color (default: transparent).
    overlayHighlightConfigShapeColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The shape margin fill color (default: transparent).
    overlayHighlightConfigShapeMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The grid layout color (default: transparent).
    overlayHighlightConfigCssGridColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The color format used to format color styles (default: hex).
    overlayHighlightConfigColorFormat :: Maybe OverlayColorFormat,
    -- | The grid layout highlight configuration (default: all transparent).
    overlayHighlightConfigGridHighlightConfig :: Maybe OverlayGridHighlightConfig,
    -- | The flex container highlight configuration (default: all transparent).
    overlayHighlightConfigFlexContainerHighlightConfig :: Maybe OverlayFlexContainerHighlightConfig,
    -- | The flex item highlight configuration (default: all transparent).
    overlayHighlightConfigFlexItemHighlightConfig :: Maybe OverlayFlexItemHighlightConfig,
    -- | The contrast algorithm to use for the contrast ratio (default: aa).
    overlayHighlightConfigContrastAlgorithm :: Maybe OverlayContrastAlgorithm,
    -- | The container query container highlight configuration (default: all transparent).
    overlayHighlightConfigContainerQueryContainerHighlightConfig :: Maybe OverlayContainerQueryContainerHighlightConfig
  }
  deriving (Eq, Show)
instance FromJSON OverlayHighlightConfig where
  parseJSON = A.withObject "OverlayHighlightConfig" $ \o -> OverlayHighlightConfig
    <$> o A..:? "showInfo"
    <*> o A..:? "showStyles"
    <*> o A..:? "showRulers"
    <*> o A..:? "showAccessibilityInfo"
    <*> o A..:? "showExtensionLines"
    <*> o A..:? "contentColor"
    <*> o A..:? "paddingColor"
    <*> o A..:? "borderColor"
    <*> o A..:? "marginColor"
    <*> o A..:? "eventTargetColor"
    <*> o A..:? "shapeColor"
    <*> o A..:? "shapeMarginColor"
    <*> o A..:? "cssGridColor"
    <*> o A..:? "colorFormat"
    <*> o A..:? "gridHighlightConfig"
    <*> o A..:? "flexContainerHighlightConfig"
    <*> o A..:? "flexItemHighlightConfig"
    <*> o A..:? "contrastAlgorithm"
    <*> o A..:? "containerQueryContainerHighlightConfig"
instance ToJSON OverlayHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("showInfo" A..=) <$> (overlayHighlightConfigShowInfo p),
    ("showStyles" A..=) <$> (overlayHighlightConfigShowStyles p),
    ("showRulers" A..=) <$> (overlayHighlightConfigShowRulers p),
    ("showAccessibilityInfo" A..=) <$> (overlayHighlightConfigShowAccessibilityInfo p),
    ("showExtensionLines" A..=) <$> (overlayHighlightConfigShowExtensionLines p),
    ("contentColor" A..=) <$> (overlayHighlightConfigContentColor p),
    ("paddingColor" A..=) <$> (overlayHighlightConfigPaddingColor p),
    ("borderColor" A..=) <$> (overlayHighlightConfigBorderColor p),
    ("marginColor" A..=) <$> (overlayHighlightConfigMarginColor p),
    ("eventTargetColor" A..=) <$> (overlayHighlightConfigEventTargetColor p),
    ("shapeColor" A..=) <$> (overlayHighlightConfigShapeColor p),
    ("shapeMarginColor" A..=) <$> (overlayHighlightConfigShapeMarginColor p),
    ("cssGridColor" A..=) <$> (overlayHighlightConfigCssGridColor p),
    ("colorFormat" A..=) <$> (overlayHighlightConfigColorFormat p),
    ("gridHighlightConfig" A..=) <$> (overlayHighlightConfigGridHighlightConfig p),
    ("flexContainerHighlightConfig" A..=) <$> (overlayHighlightConfigFlexContainerHighlightConfig p),
    ("flexItemHighlightConfig" A..=) <$> (overlayHighlightConfigFlexItemHighlightConfig p),
    ("contrastAlgorithm" A..=) <$> (overlayHighlightConfigContrastAlgorithm p),
    ("containerQueryContainerHighlightConfig" A..=) <$> (overlayHighlightConfigContainerQueryContainerHighlightConfig p)
    ]

-- | Type 'Overlay.ColorFormat'.
data OverlayColorFormat = OverlayColorFormatRgb | OverlayColorFormatHsl | OverlayColorFormatHwb | OverlayColorFormatHex
  deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayColorFormat where
  parseJSON = A.withText "OverlayColorFormat" $ \v -> case v of
    "rgb" -> pure OverlayColorFormatRgb
    "hsl" -> pure OverlayColorFormatHsl
    "hwb" -> pure OverlayColorFormatHwb
    "hex" -> pure OverlayColorFormatHex
    "_" -> fail "failed to parse OverlayColorFormat"
instance ToJSON OverlayColorFormat where
  toJSON v = A.String $ case v of
    OverlayColorFormatRgb -> "rgb"
    OverlayColorFormatHsl -> "hsl"
    OverlayColorFormatHwb -> "hwb"
    OverlayColorFormatHex -> "hex"

-- | Type 'Overlay.GridNodeHighlightConfig'.
--   Configurations for Persistent Grid Highlight
data OverlayGridNodeHighlightConfig = OverlayGridNodeHighlightConfig
  {
    -- | A descriptor for the highlight appearance.
    overlayGridNodeHighlightConfigGridHighlightConfig :: OverlayGridHighlightConfig,
    -- | Identifier of the node to highlight.
    overlayGridNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayGridNodeHighlightConfig where
  parseJSON = A.withObject "OverlayGridNodeHighlightConfig" $ \o -> OverlayGridNodeHighlightConfig
    <$> o A..: "gridHighlightConfig"
    <*> o A..: "nodeId"
instance ToJSON OverlayGridNodeHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("gridHighlightConfig" A..=) <$> Just (overlayGridNodeHighlightConfigGridHighlightConfig p),
    ("nodeId" A..=) <$> Just (overlayGridNodeHighlightConfigNodeId p)
    ]

-- | Type 'Overlay.FlexNodeHighlightConfig'.
data OverlayFlexNodeHighlightConfig = OverlayFlexNodeHighlightConfig
  {
    -- | A descriptor for the highlight appearance of flex containers.
    overlayFlexNodeHighlightConfigFlexContainerHighlightConfig :: OverlayFlexContainerHighlightConfig,
    -- | Identifier of the node to highlight.
    overlayFlexNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayFlexNodeHighlightConfig where
  parseJSON = A.withObject "OverlayFlexNodeHighlightConfig" $ \o -> OverlayFlexNodeHighlightConfig
    <$> o A..: "flexContainerHighlightConfig"
    <*> o A..: "nodeId"
instance ToJSON OverlayFlexNodeHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("flexContainerHighlightConfig" A..=) <$> Just (overlayFlexNodeHighlightConfigFlexContainerHighlightConfig p),
    ("nodeId" A..=) <$> Just (overlayFlexNodeHighlightConfigNodeId p)
    ]

-- | Type 'Overlay.ScrollSnapContainerHighlightConfig'.
data OverlayScrollSnapContainerHighlightConfig = OverlayScrollSnapContainerHighlightConfig
  {
    -- | The style of the snapport border (default: transparent)
    overlayScrollSnapContainerHighlightConfigSnapportBorder :: Maybe OverlayLineStyle,
    -- | The style of the snap area border (default: transparent)
    overlayScrollSnapContainerHighlightConfigSnapAreaBorder :: Maybe OverlayLineStyle,
    -- | The margin highlight fill color (default: transparent).
    overlayScrollSnapContainerHighlightConfigScrollMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The padding highlight fill color (default: transparent).
    overlayScrollSnapContainerHighlightConfigScrollPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlayScrollSnapContainerHighlightConfig where
  parseJSON = A.withObject "OverlayScrollSnapContainerHighlightConfig" $ \o -> OverlayScrollSnapContainerHighlightConfig
    <$> o A..:? "snapportBorder"
    <*> o A..:? "snapAreaBorder"
    <*> o A..:? "scrollMarginColor"
    <*> o A..:? "scrollPaddingColor"
instance ToJSON OverlayScrollSnapContainerHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("snapportBorder" A..=) <$> (overlayScrollSnapContainerHighlightConfigSnapportBorder p),
    ("snapAreaBorder" A..=) <$> (overlayScrollSnapContainerHighlightConfigSnapAreaBorder p),
    ("scrollMarginColor" A..=) <$> (overlayScrollSnapContainerHighlightConfigScrollMarginColor p),
    ("scrollPaddingColor" A..=) <$> (overlayScrollSnapContainerHighlightConfigScrollPaddingColor p)
    ]

-- | Type 'Overlay.ScrollSnapHighlightConfig'.
data OverlayScrollSnapHighlightConfig = OverlayScrollSnapHighlightConfig
  {
    -- | A descriptor for the highlight appearance of scroll snap containers.
    overlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig :: OverlayScrollSnapContainerHighlightConfig,
    -- | Identifier of the node to highlight.
    overlayScrollSnapHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayScrollSnapHighlightConfig where
  parseJSON = A.withObject "OverlayScrollSnapHighlightConfig" $ \o -> OverlayScrollSnapHighlightConfig
    <$> o A..: "scrollSnapContainerHighlightConfig"
    <*> o A..: "nodeId"
instance ToJSON OverlayScrollSnapHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("scrollSnapContainerHighlightConfig" A..=) <$> Just (overlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig p),
    ("nodeId" A..=) <$> Just (overlayScrollSnapHighlightConfigNodeId p)
    ]

-- | Type 'Overlay.HingeConfig'.
--   Configuration for dual screen hinge
data OverlayHingeConfig = OverlayHingeConfig
  {
    -- | A rectangle represent hinge
    overlayHingeConfigRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | The content box highlight fill color (default: a dark color).
    overlayHingeConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The content box highlight outline color (default: transparent).
    overlayHingeConfigOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlayHingeConfig where
  parseJSON = A.withObject "OverlayHingeConfig" $ \o -> OverlayHingeConfig
    <$> o A..: "rect"
    <*> o A..:? "contentColor"
    <*> o A..:? "outlineColor"
instance ToJSON OverlayHingeConfig where
  toJSON p = A.object $ catMaybes [
    ("rect" A..=) <$> Just (overlayHingeConfigRect p),
    ("contentColor" A..=) <$> (overlayHingeConfigContentColor p),
    ("outlineColor" A..=) <$> (overlayHingeConfigOutlineColor p)
    ]

-- | Type 'Overlay.ContainerQueryHighlightConfig'.
data OverlayContainerQueryHighlightConfig = OverlayContainerQueryHighlightConfig
  {
    -- | A descriptor for the highlight appearance of container query containers.
    overlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig :: OverlayContainerQueryContainerHighlightConfig,
    -- | Identifier of the container node to highlight.
    overlayContainerQueryHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayContainerQueryHighlightConfig where
  parseJSON = A.withObject "OverlayContainerQueryHighlightConfig" $ \o -> OverlayContainerQueryHighlightConfig
    <$> o A..: "containerQueryContainerHighlightConfig"
    <*> o A..: "nodeId"
instance ToJSON OverlayContainerQueryHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("containerQueryContainerHighlightConfig" A..=) <$> Just (overlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig p),
    ("nodeId" A..=) <$> Just (overlayContainerQueryHighlightConfigNodeId p)
    ]

-- | Type 'Overlay.ContainerQueryContainerHighlightConfig'.
data OverlayContainerQueryContainerHighlightConfig = OverlayContainerQueryContainerHighlightConfig
  {
    -- | The style of the container border.
    overlayContainerQueryContainerHighlightConfigContainerBorder :: Maybe OverlayLineStyle,
    -- | The style of the descendants' borders.
    overlayContainerQueryContainerHighlightConfigDescendantBorder :: Maybe OverlayLineStyle
  }
  deriving (Eq, Show)
instance FromJSON OverlayContainerQueryContainerHighlightConfig where
  parseJSON = A.withObject "OverlayContainerQueryContainerHighlightConfig" $ \o -> OverlayContainerQueryContainerHighlightConfig
    <$> o A..:? "containerBorder"
    <*> o A..:? "descendantBorder"
instance ToJSON OverlayContainerQueryContainerHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("containerBorder" A..=) <$> (overlayContainerQueryContainerHighlightConfigContainerBorder p),
    ("descendantBorder" A..=) <$> (overlayContainerQueryContainerHighlightConfigDescendantBorder p)
    ]

-- | Type 'Overlay.IsolatedElementHighlightConfig'.
data OverlayIsolatedElementHighlightConfig = OverlayIsolatedElementHighlightConfig
  {
    -- | A descriptor for the highlight appearance of an element in isolation mode.
    overlayIsolatedElementHighlightConfigIsolationModeHighlightConfig :: OverlayIsolationModeHighlightConfig,
    -- | Identifier of the isolated element to highlight.
    overlayIsolatedElementHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayIsolatedElementHighlightConfig where
  parseJSON = A.withObject "OverlayIsolatedElementHighlightConfig" $ \o -> OverlayIsolatedElementHighlightConfig
    <$> o A..: "isolationModeHighlightConfig"
    <*> o A..: "nodeId"
instance ToJSON OverlayIsolatedElementHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("isolationModeHighlightConfig" A..=) <$> Just (overlayIsolatedElementHighlightConfigIsolationModeHighlightConfig p),
    ("nodeId" A..=) <$> Just (overlayIsolatedElementHighlightConfigNodeId p)
    ]

-- | Type 'Overlay.IsolationModeHighlightConfig'.
data OverlayIsolationModeHighlightConfig = OverlayIsolationModeHighlightConfig
  {
    -- | The fill color of the resizers (default: transparent).
    overlayIsolationModeHighlightConfigResizerColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The fill color for resizer handles (default: transparent).
    overlayIsolationModeHighlightConfigResizerHandleColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The fill color for the mask covering non-isolated elements (default: transparent).
    overlayIsolationModeHighlightConfigMaskColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
instance FromJSON OverlayIsolationModeHighlightConfig where
  parseJSON = A.withObject "OverlayIsolationModeHighlightConfig" $ \o -> OverlayIsolationModeHighlightConfig
    <$> o A..:? "resizerColor"
    <*> o A..:? "resizerHandleColor"
    <*> o A..:? "maskColor"
instance ToJSON OverlayIsolationModeHighlightConfig where
  toJSON p = A.object $ catMaybes [
    ("resizerColor" A..=) <$> (overlayIsolationModeHighlightConfigResizerColor p),
    ("resizerHandleColor" A..=) <$> (overlayIsolationModeHighlightConfigResizerHandleColor p),
    ("maskColor" A..=) <$> (overlayIsolationModeHighlightConfigMaskColor p)
    ]

-- | Type 'Overlay.InspectMode'.
data OverlayInspectMode = OverlayInspectModeSearchForNode | OverlayInspectModeSearchForUAShadowDOM | OverlayInspectModeCaptureAreaScreenshot | OverlayInspectModeShowDistances | OverlayInspectModeNone
  deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayInspectMode where
  parseJSON = A.withText "OverlayInspectMode" $ \v -> case v of
    "searchForNode" -> pure OverlayInspectModeSearchForNode
    "searchForUAShadowDOM" -> pure OverlayInspectModeSearchForUAShadowDOM
    "captureAreaScreenshot" -> pure OverlayInspectModeCaptureAreaScreenshot
    "showDistances" -> pure OverlayInspectModeShowDistances
    "none" -> pure OverlayInspectModeNone
    "_" -> fail "failed to parse OverlayInspectMode"
instance ToJSON OverlayInspectMode where
  toJSON v = A.String $ case v of
    OverlayInspectModeSearchForNode -> "searchForNode"
    OverlayInspectModeSearchForUAShadowDOM -> "searchForUAShadowDOM"
    OverlayInspectModeCaptureAreaScreenshot -> "captureAreaScreenshot"
    OverlayInspectModeShowDistances -> "showDistances"
    OverlayInspectModeNone -> "none"

-- | Type of the 'Overlay.inspectNodeRequested' event.
data OverlayInspectNodeRequested = OverlayInspectNodeRequested
  {
    -- | Id of the node to inspect.
    overlayInspectNodeRequestedBackendNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayInspectNodeRequested where
  parseJSON = A.withObject "OverlayInspectNodeRequested" $ \o -> OverlayInspectNodeRequested
    <$> o A..: "backendNodeId"
instance Event OverlayInspectNodeRequested where
  eventName _ = "Overlay.inspectNodeRequested"

-- | Type of the 'Overlay.nodeHighlightRequested' event.
data OverlayNodeHighlightRequested = OverlayNodeHighlightRequested
  {
    overlayNodeHighlightRequestedNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
instance FromJSON OverlayNodeHighlightRequested where
  parseJSON = A.withObject "OverlayNodeHighlightRequested" $ \o -> OverlayNodeHighlightRequested
    <$> o A..: "nodeId"
instance Event OverlayNodeHighlightRequested where
  eventName _ = "Overlay.nodeHighlightRequested"

-- | Type of the 'Overlay.screenshotRequested' event.
data OverlayScreenshotRequested = OverlayScreenshotRequested
  {
    -- | Viewport to capture, in device independent pixels (dip).
    overlayScreenshotRequestedViewport :: DOMPageNetworkEmulationSecurity.PageViewport
  }
  deriving (Eq, Show)
instance FromJSON OverlayScreenshotRequested where
  parseJSON = A.withObject "OverlayScreenshotRequested" $ \o -> OverlayScreenshotRequested
    <$> o A..: "viewport"
instance Event OverlayScreenshotRequested where
  eventName _ = "Overlay.screenshotRequested"

-- | Type of the 'Overlay.inspectModeCanceled' event.
data OverlayInspectModeCanceled = OverlayInspectModeCanceled
  deriving (Eq, Show, Read)
instance FromJSON OverlayInspectModeCanceled where
  parseJSON _ = pure OverlayInspectModeCanceled
instance Event OverlayInspectModeCanceled where
  eventName _ = "Overlay.inspectModeCanceled"

-- | Disables domain notifications.

-- | Parameters of the 'Overlay.disable' command.
data POverlayDisable = POverlayDisable
  deriving (Eq, Show)
pOverlayDisable
  :: POverlayDisable
pOverlayDisable
  = POverlayDisable
instance ToJSON POverlayDisable where
  toJSON _ = A.Null
instance Command POverlayDisable where
  type CommandResponse POverlayDisable = ()
  commandName _ = "Overlay.disable"
  fromJSON = const . A.Success . const ()

-- | Enables domain notifications.

-- | Parameters of the 'Overlay.enable' command.
data POverlayEnable = POverlayEnable
  deriving (Eq, Show)
pOverlayEnable
  :: POverlayEnable
pOverlayEnable
  = POverlayEnable
instance ToJSON POverlayEnable where
  toJSON _ = A.Null
instance Command POverlayEnable where
  type CommandResponse POverlayEnable = ()
  commandName _ = "Overlay.enable"
  fromJSON = const . A.Success . const ()

-- | For testing.

-- | Parameters of the 'Overlay.getHighlightObjectForTest' command.
data POverlayGetHighlightObjectForTest = POverlayGetHighlightObjectForTest
  {
    -- | Id of the node to get highlight object for.
    pOverlayGetHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Whether to include distance info.
    pOverlayGetHighlightObjectForTestIncludeDistance :: Maybe Bool,
    -- | Whether to include style info.
    pOverlayGetHighlightObjectForTestIncludeStyle :: Maybe Bool,
    -- | The color format to get config with (default: hex).
    pOverlayGetHighlightObjectForTestColorFormat :: Maybe OverlayColorFormat,
    -- | Whether to show accessibility info (default: true).
    pOverlayGetHighlightObjectForTestShowAccessibilityInfo :: Maybe Bool
  }
  deriving (Eq, Show)
pOverlayGetHighlightObjectForTest
  {-
  -- | Id of the node to get highlight object for.
  -}
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> POverlayGetHighlightObjectForTest
pOverlayGetHighlightObjectForTest
  arg_pOverlayGetHighlightObjectForTestNodeId
  = POverlayGetHighlightObjectForTest
    arg_pOverlayGetHighlightObjectForTestNodeId
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON POverlayGetHighlightObjectForTest where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pOverlayGetHighlightObjectForTestNodeId p),
    ("includeDistance" A..=) <$> (pOverlayGetHighlightObjectForTestIncludeDistance p),
    ("includeStyle" A..=) <$> (pOverlayGetHighlightObjectForTestIncludeStyle p),
    ("colorFormat" A..=) <$> (pOverlayGetHighlightObjectForTestColorFormat p),
    ("showAccessibilityInfo" A..=) <$> (pOverlayGetHighlightObjectForTestShowAccessibilityInfo p)
    ]
data OverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest
  {
    -- | Highlight data for the node.
    overlayGetHighlightObjectForTestHighlight :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON OverlayGetHighlightObjectForTest where
  parseJSON = A.withObject "OverlayGetHighlightObjectForTest" $ \o -> OverlayGetHighlightObjectForTest
    <$> o A..: "highlight"
instance Command POverlayGetHighlightObjectForTest where
  type CommandResponse POverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest
  commandName _ = "Overlay.getHighlightObjectForTest"

-- | For Persistent Grid testing.

-- | Parameters of the 'Overlay.getGridHighlightObjectsForTest' command.
data POverlayGetGridHighlightObjectsForTest = POverlayGetGridHighlightObjectsForTest
  {
    -- | Ids of the node to get highlight object for.
    pOverlayGetGridHighlightObjectsForTestNodeIds :: [DOMPageNetworkEmulationSecurity.DOMNodeId]
  }
  deriving (Eq, Show)
pOverlayGetGridHighlightObjectsForTest
  {-
  -- | Ids of the node to get highlight object for.
  -}
  :: [DOMPageNetworkEmulationSecurity.DOMNodeId]
  -> POverlayGetGridHighlightObjectsForTest
pOverlayGetGridHighlightObjectsForTest
  arg_pOverlayGetGridHighlightObjectsForTestNodeIds
  = POverlayGetGridHighlightObjectsForTest
    arg_pOverlayGetGridHighlightObjectsForTestNodeIds
instance ToJSON POverlayGetGridHighlightObjectsForTest where
  toJSON p = A.object $ catMaybes [
    ("nodeIds" A..=) <$> Just (pOverlayGetGridHighlightObjectsForTestNodeIds p)
    ]
data OverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest
  {
    -- | Grid Highlight data for the node ids provided.
    overlayGetGridHighlightObjectsForTestHighlights :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON OverlayGetGridHighlightObjectsForTest where
  parseJSON = A.withObject "OverlayGetGridHighlightObjectsForTest" $ \o -> OverlayGetGridHighlightObjectsForTest
    <$> o A..: "highlights"
instance Command POverlayGetGridHighlightObjectsForTest where
  type CommandResponse POverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest
  commandName _ = "Overlay.getGridHighlightObjectsForTest"

-- | For Source Order Viewer testing.

-- | Parameters of the 'Overlay.getSourceOrderHighlightObjectForTest' command.
data POverlayGetSourceOrderHighlightObjectForTest = POverlayGetSourceOrderHighlightObjectForTest
  {
    -- | Id of the node to highlight.
    pOverlayGetSourceOrderHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
  }
  deriving (Eq, Show)
pOverlayGetSourceOrderHighlightObjectForTest
  {-
  -- | Id of the node to highlight.
  -}
  :: DOMPageNetworkEmulationSecurity.DOMNodeId
  -> POverlayGetSourceOrderHighlightObjectForTest
pOverlayGetSourceOrderHighlightObjectForTest
  arg_pOverlayGetSourceOrderHighlightObjectForTestNodeId
  = POverlayGetSourceOrderHighlightObjectForTest
    arg_pOverlayGetSourceOrderHighlightObjectForTestNodeId
instance ToJSON POverlayGetSourceOrderHighlightObjectForTest where
  toJSON p = A.object $ catMaybes [
    ("nodeId" A..=) <$> Just (pOverlayGetSourceOrderHighlightObjectForTestNodeId p)
    ]
data OverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest
  {
    -- | Source order highlight data for the node id provided.
    overlayGetSourceOrderHighlightObjectForTestHighlight :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON OverlayGetSourceOrderHighlightObjectForTest where
  parseJSON = A.withObject "OverlayGetSourceOrderHighlightObjectForTest" $ \o -> OverlayGetSourceOrderHighlightObjectForTest
    <$> o A..: "highlight"
instance Command POverlayGetSourceOrderHighlightObjectForTest where
  type CommandResponse POverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest
  commandName _ = "Overlay.getSourceOrderHighlightObjectForTest"

-- | Hides any highlight.

-- | Parameters of the 'Overlay.hideHighlight' command.
data POverlayHideHighlight = POverlayHideHighlight
  deriving (Eq, Show)
pOverlayHideHighlight
  :: POverlayHideHighlight
pOverlayHideHighlight
  = POverlayHideHighlight
instance ToJSON POverlayHideHighlight where
  toJSON _ = A.Null
instance Command POverlayHideHighlight where
  type CommandResponse POverlayHideHighlight = ()
  commandName _ = "Overlay.hideHighlight"
  fromJSON = const . A.Success . const ()

-- | Highlights DOM node with given id or with the given JavaScript object wrapper. Either nodeId or
--   objectId must be specified.

-- | Parameters of the 'Overlay.highlightNode' command.
data POverlayHighlightNode = POverlayHighlightNode
  {
    -- | A descriptor for the highlight appearance.
    pOverlayHighlightNodeHighlightConfig :: OverlayHighlightConfig,
    -- | Identifier of the node to highlight.
    pOverlayHighlightNodeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Identifier of the backend node to highlight.
    pOverlayHighlightNodeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | JavaScript object id of the node to be highlighted.
    pOverlayHighlightNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
    -- | Selectors to highlight relevant nodes.
    pOverlayHighlightNodeSelector :: Maybe T.Text
  }
  deriving (Eq, Show)
pOverlayHighlightNode
  {-
  -- | A descriptor for the highlight appearance.
  -}
  :: OverlayHighlightConfig
  -> POverlayHighlightNode
pOverlayHighlightNode
  arg_pOverlayHighlightNodeHighlightConfig
  = POverlayHighlightNode
    arg_pOverlayHighlightNodeHighlightConfig
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON POverlayHighlightNode where
  toJSON p = A.object $ catMaybes [
    ("highlightConfig" A..=) <$> Just (pOverlayHighlightNodeHighlightConfig p),
    ("nodeId" A..=) <$> (pOverlayHighlightNodeNodeId p),
    ("backendNodeId" A..=) <$> (pOverlayHighlightNodeBackendNodeId p),
    ("objectId" A..=) <$> (pOverlayHighlightNodeObjectId p),
    ("selector" A..=) <$> (pOverlayHighlightNodeSelector p)
    ]
instance Command POverlayHighlightNode where
  type CommandResponse POverlayHighlightNode = ()
  commandName _ = "Overlay.highlightNode"
  fromJSON = const . A.Success . const ()

-- | Highlights given quad. Coordinates are absolute with respect to the main frame viewport.

-- | Parameters of the 'Overlay.highlightQuad' command.
data POverlayHighlightQuad = POverlayHighlightQuad
  {
    -- | Quad to highlight
    pOverlayHighlightQuadQuad :: DOMPageNetworkEmulationSecurity.DOMQuad,
    -- | The highlight fill color (default: transparent).
    pOverlayHighlightQuadColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The highlight outline color (default: transparent).
    pOverlayHighlightQuadOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
pOverlayHighlightQuad
  {-
  -- | Quad to highlight
  -}
  :: DOMPageNetworkEmulationSecurity.DOMQuad
  -> POverlayHighlightQuad
pOverlayHighlightQuad
  arg_pOverlayHighlightQuadQuad
  = POverlayHighlightQuad
    arg_pOverlayHighlightQuadQuad
    Nothing
    Nothing
instance ToJSON POverlayHighlightQuad where
  toJSON p = A.object $ catMaybes [
    ("quad" A..=) <$> Just (pOverlayHighlightQuadQuad p),
    ("color" A..=) <$> (pOverlayHighlightQuadColor p),
    ("outlineColor" A..=) <$> (pOverlayHighlightQuadOutlineColor p)
    ]
instance Command POverlayHighlightQuad where
  type CommandResponse POverlayHighlightQuad = ()
  commandName _ = "Overlay.highlightQuad"
  fromJSON = const . A.Success . const ()

-- | Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.

-- | Parameters of the 'Overlay.highlightRect' command.
data POverlayHighlightRect = POverlayHighlightRect
  {
    -- | X coordinate
    pOverlayHighlightRectX :: Int,
    -- | Y coordinate
    pOverlayHighlightRectY :: Int,
    -- | Rectangle width
    pOverlayHighlightRectWidth :: Int,
    -- | Rectangle height
    pOverlayHighlightRectHeight :: Int,
    -- | The highlight fill color (default: transparent).
    pOverlayHighlightRectColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
    -- | The highlight outline color (default: transparent).
    pOverlayHighlightRectOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
  }
  deriving (Eq, Show)
pOverlayHighlightRect
  {-
  -- | X coordinate
  -}
  :: Int
  {-
  -- | Y coordinate
  -}
  -> Int
  {-
  -- | Rectangle width
  -}
  -> Int
  {-
  -- | Rectangle height
  -}
  -> Int
  -> POverlayHighlightRect
pOverlayHighlightRect
  arg_pOverlayHighlightRectX
  arg_pOverlayHighlightRectY
  arg_pOverlayHighlightRectWidth
  arg_pOverlayHighlightRectHeight
  = POverlayHighlightRect
    arg_pOverlayHighlightRectX
    arg_pOverlayHighlightRectY
    arg_pOverlayHighlightRectWidth
    arg_pOverlayHighlightRectHeight
    Nothing
    Nothing
instance ToJSON POverlayHighlightRect where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pOverlayHighlightRectX p),
    ("y" A..=) <$> Just (pOverlayHighlightRectY p),
    ("width" A..=) <$> Just (pOverlayHighlightRectWidth p),
    ("height" A..=) <$> Just (pOverlayHighlightRectHeight p),
    ("color" A..=) <$> (pOverlayHighlightRectColor p),
    ("outlineColor" A..=) <$> (pOverlayHighlightRectOutlineColor p)
    ]
instance Command POverlayHighlightRect where
  type CommandResponse POverlayHighlightRect = ()
  commandName _ = "Overlay.highlightRect"
  fromJSON = const . A.Success . const ()

-- | Highlights the source order of the children of the DOM node with given id or with the given
--   JavaScript object wrapper. Either nodeId or objectId must be specified.

-- | Parameters of the 'Overlay.highlightSourceOrder' command.
data POverlayHighlightSourceOrder = POverlayHighlightSourceOrder
  {
    -- | A descriptor for the appearance of the overlay drawing.
    pOverlayHighlightSourceOrderSourceOrderConfig :: OverlaySourceOrderConfig,
    -- | Identifier of the node to highlight.
    pOverlayHighlightSourceOrderNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
    -- | Identifier of the backend node to highlight.
    pOverlayHighlightSourceOrderBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | JavaScript object id of the node to be highlighted.
    pOverlayHighlightSourceOrderObjectId :: Maybe Runtime.RuntimeRemoteObjectId
  }
  deriving (Eq, Show)
pOverlayHighlightSourceOrder
  {-
  -- | A descriptor for the appearance of the overlay drawing.
  -}
  :: OverlaySourceOrderConfig
  -> POverlayHighlightSourceOrder
pOverlayHighlightSourceOrder
  arg_pOverlayHighlightSourceOrderSourceOrderConfig
  = POverlayHighlightSourceOrder
    arg_pOverlayHighlightSourceOrderSourceOrderConfig
    Nothing
    Nothing
    Nothing
instance ToJSON POverlayHighlightSourceOrder where
  toJSON p = A.object $ catMaybes [
    ("sourceOrderConfig" A..=) <$> Just (pOverlayHighlightSourceOrderSourceOrderConfig p),
    ("nodeId" A..=) <$> (pOverlayHighlightSourceOrderNodeId p),
    ("backendNodeId" A..=) <$> (pOverlayHighlightSourceOrderBackendNodeId p),
    ("objectId" A..=) <$> (pOverlayHighlightSourceOrderObjectId p)
    ]
instance Command POverlayHighlightSourceOrder where
  type CommandResponse POverlayHighlightSourceOrder = ()
  commandName _ = "Overlay.highlightSourceOrder"
  fromJSON = const . A.Success . const ()

-- | Enters the 'inspect' mode. In this mode, elements that user is hovering over are highlighted.
--   Backend then generates 'inspectNodeRequested' event upon element selection.

-- | Parameters of the 'Overlay.setInspectMode' command.
data POverlaySetInspectMode = POverlaySetInspectMode
  {
    -- | Set an inspection mode.
    pOverlaySetInspectModeMode :: OverlayInspectMode,
    -- | A descriptor for the highlight appearance of hovered-over nodes. May be omitted if `enabled
    --   == false`.
    pOverlaySetInspectModeHighlightConfig :: Maybe OverlayHighlightConfig
  }
  deriving (Eq, Show)
pOverlaySetInspectMode
  {-
  -- | Set an inspection mode.
  -}
  :: OverlayInspectMode
  -> POverlaySetInspectMode
pOverlaySetInspectMode
  arg_pOverlaySetInspectModeMode
  = POverlaySetInspectMode
    arg_pOverlaySetInspectModeMode
    Nothing
instance ToJSON POverlaySetInspectMode where
  toJSON p = A.object $ catMaybes [
    ("mode" A..=) <$> Just (pOverlaySetInspectModeMode p),
    ("highlightConfig" A..=) <$> (pOverlaySetInspectModeHighlightConfig p)
    ]
instance Command POverlaySetInspectMode where
  type CommandResponse POverlaySetInspectMode = ()
  commandName _ = "Overlay.setInspectMode"
  fromJSON = const . A.Success . const ()

-- | Highlights owner element of all frames detected to be ads.

-- | Parameters of the 'Overlay.setShowAdHighlights' command.
data POverlaySetShowAdHighlights = POverlaySetShowAdHighlights
  {
    -- | True for showing ad highlights
    pOverlaySetShowAdHighlightsShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowAdHighlights
  {-
  -- | True for showing ad highlights
  -}
  :: Bool
  -> POverlaySetShowAdHighlights
pOverlaySetShowAdHighlights
  arg_pOverlaySetShowAdHighlightsShow
  = POverlaySetShowAdHighlights
    arg_pOverlaySetShowAdHighlightsShow
instance ToJSON POverlaySetShowAdHighlights where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowAdHighlightsShow p)
    ]
instance Command POverlaySetShowAdHighlights where
  type CommandResponse POverlaySetShowAdHighlights = ()
  commandName _ = "Overlay.setShowAdHighlights"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Overlay.setPausedInDebuggerMessage' command.
data POverlaySetPausedInDebuggerMessage = POverlaySetPausedInDebuggerMessage
  {
    -- | The message to display, also triggers resume and step over controls.
    pOverlaySetPausedInDebuggerMessageMessage :: Maybe T.Text
  }
  deriving (Eq, Show)
pOverlaySetPausedInDebuggerMessage
  :: POverlaySetPausedInDebuggerMessage
pOverlaySetPausedInDebuggerMessage
  = POverlaySetPausedInDebuggerMessage
    Nothing
instance ToJSON POverlaySetPausedInDebuggerMessage where
  toJSON p = A.object $ catMaybes [
    ("message" A..=) <$> (pOverlaySetPausedInDebuggerMessageMessage p)
    ]
instance Command POverlaySetPausedInDebuggerMessage where
  type CommandResponse POverlaySetPausedInDebuggerMessage = ()
  commandName _ = "Overlay.setPausedInDebuggerMessage"
  fromJSON = const . A.Success . const ()

-- | Requests that backend shows debug borders on layers

-- | Parameters of the 'Overlay.setShowDebugBorders' command.
data POverlaySetShowDebugBorders = POverlaySetShowDebugBorders
  {
    -- | True for showing debug borders
    pOverlaySetShowDebugBordersShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowDebugBorders
  {-
  -- | True for showing debug borders
  -}
  :: Bool
  -> POverlaySetShowDebugBorders
pOverlaySetShowDebugBorders
  arg_pOverlaySetShowDebugBordersShow
  = POverlaySetShowDebugBorders
    arg_pOverlaySetShowDebugBordersShow
instance ToJSON POverlaySetShowDebugBorders where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowDebugBordersShow p)
    ]
instance Command POverlaySetShowDebugBorders where
  type CommandResponse POverlaySetShowDebugBorders = ()
  commandName _ = "Overlay.setShowDebugBorders"
  fromJSON = const . A.Success . const ()

-- | Requests that backend shows the FPS counter

-- | Parameters of the 'Overlay.setShowFPSCounter' command.
data POverlaySetShowFPSCounter = POverlaySetShowFPSCounter
  {
    -- | True for showing the FPS counter
    pOverlaySetShowFPSCounterShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowFPSCounter
  {-
  -- | True for showing the FPS counter
  -}
  :: Bool
  -> POverlaySetShowFPSCounter
pOverlaySetShowFPSCounter
  arg_pOverlaySetShowFPSCounterShow
  = POverlaySetShowFPSCounter
    arg_pOverlaySetShowFPSCounterShow
instance ToJSON POverlaySetShowFPSCounter where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowFPSCounterShow p)
    ]
instance Command POverlaySetShowFPSCounter where
  type CommandResponse POverlaySetShowFPSCounter = ()
  commandName _ = "Overlay.setShowFPSCounter"
  fromJSON = const . A.Success . const ()

-- | Highlight multiple elements with the CSS Grid overlay.

-- | Parameters of the 'Overlay.setShowGridOverlays' command.
data POverlaySetShowGridOverlays = POverlaySetShowGridOverlays
  {
    -- | An array of node identifiers and descriptors for the highlight appearance.
    pOverlaySetShowGridOverlaysGridNodeHighlightConfigs :: [OverlayGridNodeHighlightConfig]
  }
  deriving (Eq, Show)
pOverlaySetShowGridOverlays
  {-
  -- | An array of node identifiers and descriptors for the highlight appearance.
  -}
  :: [OverlayGridNodeHighlightConfig]
  -> POverlaySetShowGridOverlays
pOverlaySetShowGridOverlays
  arg_pOverlaySetShowGridOverlaysGridNodeHighlightConfigs
  = POverlaySetShowGridOverlays
    arg_pOverlaySetShowGridOverlaysGridNodeHighlightConfigs
instance ToJSON POverlaySetShowGridOverlays where
  toJSON p = A.object $ catMaybes [
    ("gridNodeHighlightConfigs" A..=) <$> Just (pOverlaySetShowGridOverlaysGridNodeHighlightConfigs p)
    ]
instance Command POverlaySetShowGridOverlays where
  type CommandResponse POverlaySetShowGridOverlays = ()
  commandName _ = "Overlay.setShowGridOverlays"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Overlay.setShowFlexOverlays' command.
data POverlaySetShowFlexOverlays = POverlaySetShowFlexOverlays
  {
    -- | An array of node identifiers and descriptors for the highlight appearance.
    pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs :: [OverlayFlexNodeHighlightConfig]
  }
  deriving (Eq, Show)
pOverlaySetShowFlexOverlays
  {-
  -- | An array of node identifiers and descriptors for the highlight appearance.
  -}
  :: [OverlayFlexNodeHighlightConfig]
  -> POverlaySetShowFlexOverlays
pOverlaySetShowFlexOverlays
  arg_pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs
  = POverlaySetShowFlexOverlays
    arg_pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs
instance ToJSON POverlaySetShowFlexOverlays where
  toJSON p = A.object $ catMaybes [
    ("flexNodeHighlightConfigs" A..=) <$> Just (pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs p)
    ]
instance Command POverlaySetShowFlexOverlays where
  type CommandResponse POverlaySetShowFlexOverlays = ()
  commandName _ = "Overlay.setShowFlexOverlays"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Overlay.setShowScrollSnapOverlays' command.
data POverlaySetShowScrollSnapOverlays = POverlaySetShowScrollSnapOverlays
  {
    -- | An array of node identifiers and descriptors for the highlight appearance.
    pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs :: [OverlayScrollSnapHighlightConfig]
  }
  deriving (Eq, Show)
pOverlaySetShowScrollSnapOverlays
  {-
  -- | An array of node identifiers and descriptors for the highlight appearance.
  -}
  :: [OverlayScrollSnapHighlightConfig]
  -> POverlaySetShowScrollSnapOverlays
pOverlaySetShowScrollSnapOverlays
  arg_pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs
  = POverlaySetShowScrollSnapOverlays
    arg_pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs
instance ToJSON POverlaySetShowScrollSnapOverlays where
  toJSON p = A.object $ catMaybes [
    ("scrollSnapHighlightConfigs" A..=) <$> Just (pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs p)
    ]
instance Command POverlaySetShowScrollSnapOverlays where
  type CommandResponse POverlaySetShowScrollSnapOverlays = ()
  commandName _ = "Overlay.setShowScrollSnapOverlays"
  fromJSON = const . A.Success . const ()


-- | Parameters of the 'Overlay.setShowContainerQueryOverlays' command.
data POverlaySetShowContainerQueryOverlays = POverlaySetShowContainerQueryOverlays
  {
    -- | An array of node identifiers and descriptors for the highlight appearance.
    pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs :: [OverlayContainerQueryHighlightConfig]
  }
  deriving (Eq, Show)
pOverlaySetShowContainerQueryOverlays
  {-
  -- | An array of node identifiers and descriptors for the highlight appearance.
  -}
  :: [OverlayContainerQueryHighlightConfig]
  -> POverlaySetShowContainerQueryOverlays
pOverlaySetShowContainerQueryOverlays
  arg_pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs
  = POverlaySetShowContainerQueryOverlays
    arg_pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs
instance ToJSON POverlaySetShowContainerQueryOverlays where
  toJSON p = A.object $ catMaybes [
    ("containerQueryHighlightConfigs" A..=) <$> Just (pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs p)
    ]
instance Command POverlaySetShowContainerQueryOverlays where
  type CommandResponse POverlaySetShowContainerQueryOverlays = ()
  commandName _ = "Overlay.setShowContainerQueryOverlays"
  fromJSON = const . A.Success . const ()

-- | Requests that backend shows paint rectangles

-- | Parameters of the 'Overlay.setShowPaintRects' command.
data POverlaySetShowPaintRects = POverlaySetShowPaintRects
  {
    -- | True for showing paint rectangles
    pOverlaySetShowPaintRectsResult :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowPaintRects
  {-
  -- | True for showing paint rectangles
  -}
  :: Bool
  -> POverlaySetShowPaintRects
pOverlaySetShowPaintRects
  arg_pOverlaySetShowPaintRectsResult
  = POverlaySetShowPaintRects
    arg_pOverlaySetShowPaintRectsResult
instance ToJSON POverlaySetShowPaintRects where
  toJSON p = A.object $ catMaybes [
    ("result" A..=) <$> Just (pOverlaySetShowPaintRectsResult p)
    ]
instance Command POverlaySetShowPaintRects where
  type CommandResponse POverlaySetShowPaintRects = ()
  commandName _ = "Overlay.setShowPaintRects"
  fromJSON = const . A.Success . const ()

-- | Requests that backend shows layout shift regions

-- | Parameters of the 'Overlay.setShowLayoutShiftRegions' command.
data POverlaySetShowLayoutShiftRegions = POverlaySetShowLayoutShiftRegions
  {
    -- | True for showing layout shift regions
    pOverlaySetShowLayoutShiftRegionsResult :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowLayoutShiftRegions
  {-
  -- | True for showing layout shift regions
  -}
  :: Bool
  -> POverlaySetShowLayoutShiftRegions
pOverlaySetShowLayoutShiftRegions
  arg_pOverlaySetShowLayoutShiftRegionsResult
  = POverlaySetShowLayoutShiftRegions
    arg_pOverlaySetShowLayoutShiftRegionsResult
instance ToJSON POverlaySetShowLayoutShiftRegions where
  toJSON p = A.object $ catMaybes [
    ("result" A..=) <$> Just (pOverlaySetShowLayoutShiftRegionsResult p)
    ]
instance Command POverlaySetShowLayoutShiftRegions where
  type CommandResponse POverlaySetShowLayoutShiftRegions = ()
  commandName _ = "Overlay.setShowLayoutShiftRegions"
  fromJSON = const . A.Success . const ()

-- | Requests that backend shows scroll bottleneck rects

-- | Parameters of the 'Overlay.setShowScrollBottleneckRects' command.
data POverlaySetShowScrollBottleneckRects = POverlaySetShowScrollBottleneckRects
  {
    -- | True for showing scroll bottleneck rects
    pOverlaySetShowScrollBottleneckRectsShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowScrollBottleneckRects
  {-
  -- | True for showing scroll bottleneck rects
  -}
  :: Bool
  -> POverlaySetShowScrollBottleneckRects
pOverlaySetShowScrollBottleneckRects
  arg_pOverlaySetShowScrollBottleneckRectsShow
  = POverlaySetShowScrollBottleneckRects
    arg_pOverlaySetShowScrollBottleneckRectsShow
instance ToJSON POverlaySetShowScrollBottleneckRects where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowScrollBottleneckRectsShow p)
    ]
instance Command POverlaySetShowScrollBottleneckRects where
  type CommandResponse POverlaySetShowScrollBottleneckRects = ()
  commandName _ = "Overlay.setShowScrollBottleneckRects"
  fromJSON = const . A.Success . const ()

-- | Request that backend shows an overlay with web vital metrics.

-- | Parameters of the 'Overlay.setShowWebVitals' command.
data POverlaySetShowWebVitals = POverlaySetShowWebVitals
  {
    pOverlaySetShowWebVitalsShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowWebVitals
  :: Bool
  -> POverlaySetShowWebVitals
pOverlaySetShowWebVitals
  arg_pOverlaySetShowWebVitalsShow
  = POverlaySetShowWebVitals
    arg_pOverlaySetShowWebVitalsShow
instance ToJSON POverlaySetShowWebVitals where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowWebVitalsShow p)
    ]
instance Command POverlaySetShowWebVitals where
  type CommandResponse POverlaySetShowWebVitals = ()
  commandName _ = "Overlay.setShowWebVitals"
  fromJSON = const . A.Success . const ()

-- | Paints viewport size upon main frame resize.

-- | Parameters of the 'Overlay.setShowViewportSizeOnResize' command.
data POverlaySetShowViewportSizeOnResize = POverlaySetShowViewportSizeOnResize
  {
    -- | Whether to paint size or not.
    pOverlaySetShowViewportSizeOnResizeShow :: Bool
  }
  deriving (Eq, Show)
pOverlaySetShowViewportSizeOnResize
  {-
  -- | Whether to paint size or not.
  -}
  :: Bool
  -> POverlaySetShowViewportSizeOnResize
pOverlaySetShowViewportSizeOnResize
  arg_pOverlaySetShowViewportSizeOnResizeShow
  = POverlaySetShowViewportSizeOnResize
    arg_pOverlaySetShowViewportSizeOnResizeShow
instance ToJSON POverlaySetShowViewportSizeOnResize where
  toJSON p = A.object $ catMaybes [
    ("show" A..=) <$> Just (pOverlaySetShowViewportSizeOnResizeShow p)
    ]
instance Command POverlaySetShowViewportSizeOnResize where
  type CommandResponse POverlaySetShowViewportSizeOnResize = ()
  commandName _ = "Overlay.setShowViewportSizeOnResize"
  fromJSON = const . A.Success . const ()

-- | Add a dual screen device hinge

-- | Parameters of the 'Overlay.setShowHinge' command.
data POverlaySetShowHinge = POverlaySetShowHinge
  {
    -- | hinge data, null means hideHinge
    pOverlaySetShowHingeHingeConfig :: Maybe OverlayHingeConfig
  }
  deriving (Eq, Show)
pOverlaySetShowHinge
  :: POverlaySetShowHinge
pOverlaySetShowHinge
  = POverlaySetShowHinge
    Nothing
instance ToJSON POverlaySetShowHinge where
  toJSON p = A.object $ catMaybes [
    ("hingeConfig" A..=) <$> (pOverlaySetShowHingeHingeConfig p)
    ]
instance Command POverlaySetShowHinge where
  type CommandResponse POverlaySetShowHinge = ()
  commandName _ = "Overlay.setShowHinge"
  fromJSON = const . A.Success . const ()

-- | Show elements in isolation mode with overlays.

-- | Parameters of the 'Overlay.setShowIsolatedElements' command.
data POverlaySetShowIsolatedElements = POverlaySetShowIsolatedElements
  {
    -- | An array of node identifiers and descriptors for the highlight appearance.
    pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs :: [OverlayIsolatedElementHighlightConfig]
  }
  deriving (Eq, Show)
pOverlaySetShowIsolatedElements
  {-
  -- | An array of node identifiers and descriptors for the highlight appearance.
  -}
  :: [OverlayIsolatedElementHighlightConfig]
  -> POverlaySetShowIsolatedElements
pOverlaySetShowIsolatedElements
  arg_pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs
  = POverlaySetShowIsolatedElements
    arg_pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs
instance ToJSON POverlaySetShowIsolatedElements where
  toJSON p = A.object $ catMaybes [
    ("isolatedElementHighlightConfigs" A..=) <$> Just (pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs p)
    ]
instance Command POverlaySetShowIsolatedElements where
  type CommandResponse POverlaySetShowIsolatedElements = ()
  commandName _ = "Overlay.setShowIsolatedElements"
  fromJSON = const . A.Success . const ()

