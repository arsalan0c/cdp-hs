{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Overlay :
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


-- | Configuration data for drawing the source order of an elements children.
data OverlaySourceOrderConfig = OverlaySourceOrderConfig {
   overlaySourceOrderConfigParentOutlineColor :: OverlaySourceOrderConfigParentOutlineColor, -- ^ the color to outline the givent element in.
   overlaySourceOrderConfigChildOutlineColor :: OverlaySourceOrderConfigChildOutlineColor -- ^ the color to outline the child elements in.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlaySourceOrderConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  OverlaySourceOrderConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Configuration data for the highlighting of Grid elements.
data OverlayGridHighlightConfig = OverlayGridHighlightConfig {
   overlayGridHighlightConfigShowGridExtensionLines :: OverlayGridHighlightConfigShowGridExtensionLines, -- ^ Whether the extension lines from grid cells to the rulers should be shown (default: false).
   overlayGridHighlightConfigShowPositiveLineNumbers :: OverlayGridHighlightConfigShowPositiveLineNumbers, -- ^ Show Positive line number labels (default: false).
   overlayGridHighlightConfigShowNegativeLineNumbers :: OverlayGridHighlightConfigShowNegativeLineNumbers, -- ^ Show Negative line number labels (default: false).
   overlayGridHighlightConfigShowAreaNames :: OverlayGridHighlightConfigShowAreaNames, -- ^ Show area name labels (default: false).
   overlayGridHighlightConfigShowLineNames :: OverlayGridHighlightConfigShowLineNames, -- ^ Show line name labels (default: false).
   overlayGridHighlightConfigShowTrackSizes :: OverlayGridHighlightConfigShowTrackSizes, -- ^ Show track size labels (default: false).
   overlayGridHighlightConfigGridBorderColor :: OverlayGridHighlightConfigGridBorderColor, -- ^ The grid container border highlight color (default: transparent).
   overlayGridHighlightConfigRowLineColor :: OverlayGridHighlightConfigRowLineColor, -- ^ The row line color (default: transparent).
   overlayGridHighlightConfigColumnLineColor :: OverlayGridHighlightConfigColumnLineColor, -- ^ The column line color (default: transparent).
   overlayGridHighlightConfigGridBorderDash :: OverlayGridHighlightConfigGridBorderDash, -- ^ Whether the grid border is dashed (default: false).
   overlayGridHighlightConfigRowLineDash :: OverlayGridHighlightConfigRowLineDash, -- ^ Whether row lines are dashed (default: false).
   overlayGridHighlightConfigColumnLineDash :: OverlayGridHighlightConfigColumnLineDash, -- ^ Whether column lines are dashed (default: false).
   overlayGridHighlightConfigRowGapColor :: OverlayGridHighlightConfigRowGapColor, -- ^ The row gap highlight fill color (default: transparent).
   overlayGridHighlightConfigRowHatchColor :: OverlayGridHighlightConfigRowHatchColor, -- ^ The row gap hatching fill color (default: transparent).
   overlayGridHighlightConfigColumnGapColor :: OverlayGridHighlightConfigColumnGapColor, -- ^ The column gap highlight fill color (default: transparent).
   overlayGridHighlightConfigColumnHatchColor :: OverlayGridHighlightConfigColumnHatchColor, -- ^ The column gap hatching fill color (default: transparent).
   overlayGridHighlightConfigAreaBorderColor :: OverlayGridHighlightConfigAreaBorderColor, -- ^ The named grid areas border color (Default: transparent).
   overlayGridHighlightConfigGridBackgroundColor :: OverlayGridHighlightConfigGridBackgroundColor -- ^ The grid container background color (Default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayGridHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Configuration data for the highlighting of Flex container elements.
data OverlayFlexContainerHighlightConfig = OverlayFlexContainerHighlightConfig {
   overlayFlexContainerHighlightConfigContainerBorder :: OverlayFlexContainerHighlightConfigContainerBorder, -- ^ The style of the container border
   overlayFlexContainerHighlightConfigLineSeparator :: OverlayFlexContainerHighlightConfigLineSeparator, -- ^ The style of the separator between lines
   overlayFlexContainerHighlightConfigItemSeparator :: OverlayFlexContainerHighlightConfigItemSeparator, -- ^ The style of the separator between items
   overlayFlexContainerHighlightConfigMainDistributedSpace :: OverlayFlexContainerHighlightConfigMainDistributedSpace, -- ^ Style of content-distribution space on the main axis (justify-content).
   overlayFlexContainerHighlightConfigCrossDistributedSpace :: OverlayFlexContainerHighlightConfigCrossDistributedSpace, -- ^ Style of content-distribution space on the cross axis (align-content).
   overlayFlexContainerHighlightConfigRowGapSpace :: OverlayFlexContainerHighlightConfigRowGapSpace, -- ^ Style of empty space caused by row gaps (gap/row-gap).
   overlayFlexContainerHighlightConfigColumnGapSpace :: OverlayFlexContainerHighlightConfigColumnGapSpace, -- ^ Style of empty space caused by columns gaps (gap/column-gap).
   overlayFlexContainerHighlightConfigCrossAlignment :: OverlayFlexContainerHighlightConfigCrossAlignment -- ^ Style of the self-alignment line (align-items).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Configuration data for the highlighting of Flex item elements.
data OverlayFlexItemHighlightConfig = OverlayFlexItemHighlightConfig {
   overlayFlexItemHighlightConfigBaseSizeBox :: OverlayFlexItemHighlightConfigBaseSizeBox, -- ^ Style of the box representing the item's base size
   overlayFlexItemHighlightConfigBaseSizeBorder :: OverlayFlexItemHighlightConfigBaseSizeBorder, -- ^ Style of the border around the box representing the item's base size
   overlayFlexItemHighlightConfigFlexibilityArrow :: OverlayFlexItemHighlightConfigFlexibilityArrow -- ^ Style of the arrow representing if the item grew or shrank
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexItemHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexItemHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Style information for drawing a line.
data OverlayLineStylePattern = OverlayLineStylePatternDashed | OverlayLineStylePatternDotted
   deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayLineStylePattern where
   parseJSON = A.withText  "OverlayLineStylePattern"  $ \v -> do
      case v of
         "dashed" -> pure OverlayLineStylePatternDashed
         "dotted" -> pure OverlayLineStylePatternDotted
         _ -> fail "failed to parse OverlayLineStylePattern"

instance ToJSON OverlayLineStylePattern where
   toJSON v = A.String $
      case v of
         OverlayLineStylePatternDashed -> "dashed"
         OverlayLineStylePatternDotted -> "dotted"



data OverlayLineStyle = OverlayLineStyle {
   overlayLineStyleColor :: OverlayLineStyleColor, -- ^ The color of the line (default: transparent)
   overlayLineStylePattern :: OverlayLineStylePattern -- ^ The line pattern (default: solid)
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayLineStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  OverlayLineStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Style information for drawing a box.
data OverlayBoxStyle = OverlayBoxStyle {
   overlayBoxStyleFillColor :: OverlayBoxStyleFillColor, -- ^ The background color for the box (default: transparent)
   overlayBoxStyleHatchColor :: OverlayBoxStyleHatchColor -- ^ The hatching color for the box (default: transparent)
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayBoxStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  OverlayBoxStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'Overlay.ContrastAlgorithm' .
data OverlayContrastAlgorithm = OverlayContrastAlgorithmAa | OverlayContrastAlgorithmAaa | OverlayContrastAlgorithmApca
   deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayContrastAlgorithm where
   parseJSON = A.withText  "OverlayContrastAlgorithm"  $ \v -> do
      case v of
         "aa" -> pure OverlayContrastAlgorithmAa
         "aaa" -> pure OverlayContrastAlgorithmAaa
         "apca" -> pure OverlayContrastAlgorithmApca
         _ -> fail "failed to parse OverlayContrastAlgorithm"

instance ToJSON OverlayContrastAlgorithm where
   toJSON v = A.String $
      case v of
         OverlayContrastAlgorithmAa -> "aa"
         OverlayContrastAlgorithmAaa -> "aaa"
         OverlayContrastAlgorithmApca -> "apca"



-- | Configuration data for the highlighting of page elements.
data OverlayHighlightConfig = OverlayHighlightConfig {
   overlayHighlightConfigShowInfo :: OverlayHighlightConfigShowInfo, -- ^ Whether the node info tooltip should be shown (default: false).
   overlayHighlightConfigShowStyles :: OverlayHighlightConfigShowStyles, -- ^ Whether the node styles in the tooltip (default: false).
   overlayHighlightConfigShowRulers :: OverlayHighlightConfigShowRulers, -- ^ Whether the rulers should be shown (default: false).
   overlayHighlightConfigShowAccessibilityInfo :: OverlayHighlightConfigShowAccessibilityInfo, -- ^ Whether the a11y info should be shown (default: true).
   overlayHighlightConfigShowExtensionLines :: OverlayHighlightConfigShowExtensionLines, -- ^ Whether the extension lines from node to the rulers should be shown (default: false).
   overlayHighlightConfigContentColor :: OverlayHighlightConfigContentColor, -- ^ The content box highlight fill color (default: transparent).
   overlayHighlightConfigPaddingColor :: OverlayHighlightConfigPaddingColor, -- ^ The padding highlight fill color (default: transparent).
   overlayHighlightConfigBorderColor :: OverlayHighlightConfigBorderColor, -- ^ The border highlight fill color (default: transparent).
   overlayHighlightConfigMarginColor :: OverlayHighlightConfigMarginColor, -- ^ The margin highlight fill color (default: transparent).
   overlayHighlightConfigEventTargetColor :: OverlayHighlightConfigEventTargetColor, -- ^ The event target element highlight fill color (default: transparent).
   overlayHighlightConfigShapeColor :: OverlayHighlightConfigShapeColor, -- ^ The shape outside fill color (default: transparent).
   overlayHighlightConfigShapeMarginColor :: OverlayHighlightConfigShapeMarginColor, -- ^ The shape margin fill color (default: transparent).
   overlayHighlightConfigCssGridColor :: OverlayHighlightConfigCssGridColor, -- ^ The grid layout color (default: transparent).
   overlayHighlightConfigColorFormat :: OverlayHighlightConfigColorFormat, -- ^ The color format used to format color styles (default: hex).
   overlayHighlightConfigGridHighlightConfig :: OverlayHighlightConfigGridHighlightConfig, -- ^ The grid layout highlight configuration (default: all transparent).
   overlayHighlightConfigFlexContainerHighlightConfig :: OverlayHighlightConfigFlexContainerHighlightConfig, -- ^ The flex container highlight configuration (default: all transparent).
   overlayHighlightConfigFlexItemHighlightConfig :: OverlayHighlightConfigFlexItemHighlightConfig, -- ^ The flex item highlight configuration (default: all transparent).
   overlayHighlightConfigContrastAlgorithm :: OverlayHighlightConfigContrastAlgorithm, -- ^ The contrast algorithm to use for the contrast ratio (default: aa).
   overlayHighlightConfigContainerQueryContainerHighlightConfig :: OverlayHighlightConfigContainerQueryContainerHighlightConfig -- ^ The container query container highlight configuration (default: all transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  OverlayHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Overlay.ColorFormat' .
data OverlayColorFormat = OverlayColorFormatRgb | OverlayColorFormatHsl | OverlayColorFormatHwb | OverlayColorFormatHex
   deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayColorFormat where
   parseJSON = A.withText  "OverlayColorFormat"  $ \v -> do
      case v of
         "rgb" -> pure OverlayColorFormatRgb
         "hsl" -> pure OverlayColorFormatHsl
         "hwb" -> pure OverlayColorFormatHwb
         "hex" -> pure OverlayColorFormatHex
         _ -> fail "failed to parse OverlayColorFormat"

instance ToJSON OverlayColorFormat where
   toJSON v = A.String $
      case v of
         OverlayColorFormatRgb -> "rgb"
         OverlayColorFormatHsl -> "hsl"
         OverlayColorFormatHwb -> "hwb"
         OverlayColorFormatHex -> "hex"



-- | Configurations for Persistent Grid Highlight
data OverlayGridNodeHighlightConfig = OverlayGridNodeHighlightConfig {
   overlayGridNodeHighlightConfigGridHighlightConfig :: OverlayGridNodeHighlightConfigGridHighlightConfig, -- ^ A descriptor for the highlight appearance.
   overlayGridNodeHighlightConfigNodeId :: OverlayGridNodeHighlightConfigNodeId -- ^ Identifier of the node to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayGridNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Overlay.FlexNodeHighlightConfig' .
data OverlayFlexNodeHighlightConfig = OverlayFlexNodeHighlightConfig {
   overlayFlexNodeHighlightConfigFlexContainerHighlightConfig :: OverlayFlexNodeHighlightConfigFlexContainerHighlightConfig, -- ^ A descriptor for the highlight appearance of flex containers.
   overlayFlexNodeHighlightConfigNodeId :: OverlayFlexNodeHighlightConfigNodeId -- ^ Identifier of the node to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Overlay.ScrollSnapContainerHighlightConfig' .
data OverlayScrollSnapContainerHighlightConfig = OverlayScrollSnapContainerHighlightConfig {
   overlayScrollSnapContainerHighlightConfigSnapportBorder :: OverlayScrollSnapContainerHighlightConfigSnapportBorder, -- ^ The style of the snapport border (default: transparent)
   overlayScrollSnapContainerHighlightConfigSnapAreaBorder :: OverlayScrollSnapContainerHighlightConfigSnapAreaBorder, -- ^ The style of the snap area border (default: transparent)
   overlayScrollSnapContainerHighlightConfigScrollMarginColor :: OverlayScrollSnapContainerHighlightConfigScrollMarginColor, -- ^ The margin highlight fill color (default: transparent).
   overlayScrollSnapContainerHighlightConfigScrollPaddingColor :: OverlayScrollSnapContainerHighlightConfigScrollPaddingColor -- ^ The padding highlight fill color (default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | Type 'Overlay.ScrollSnapHighlightConfig' .
data OverlayScrollSnapHighlightConfig = OverlayScrollSnapHighlightConfig {
   overlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig :: OverlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig, -- ^ A descriptor for the highlight appearance of scroll snap containers.
   overlayScrollSnapHighlightConfigNodeId :: OverlayScrollSnapHighlightConfigNodeId -- ^ Identifier of the node to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Configuration for dual screen hinge
data OverlayHingeConfig = OverlayHingeConfig {
   overlayHingeConfigRect :: OverlayHingeConfigRect, -- ^ A rectangle represent hinge
   overlayHingeConfigContentColor :: OverlayHingeConfigContentColor, -- ^ The content box highlight fill color (default: a dark color).
   overlayHingeConfigOutlineColor :: OverlayHingeConfigOutlineColor -- ^ The content box highlight outline color (default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHingeConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  OverlayHingeConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Overlay.ContainerQueryHighlightConfig' .
data OverlayContainerQueryHighlightConfig = OverlayContainerQueryHighlightConfig {
   overlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig :: OverlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig, -- ^ A descriptor for the highlight appearance of container query containers.
   overlayContainerQueryHighlightConfigNodeId :: OverlayContainerQueryHighlightConfigNodeId -- ^ Identifier of the container node to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Overlay.ContainerQueryContainerHighlightConfig' .
data OverlayContainerQueryContainerHighlightConfig = OverlayContainerQueryContainerHighlightConfig {
   overlayContainerQueryContainerHighlightConfigContainerBorder :: OverlayContainerQueryContainerHighlightConfigContainerBorder, -- ^ The style of the container border.
   overlayContainerQueryContainerHighlightConfigDescendantBorder :: OverlayContainerQueryContainerHighlightConfigDescendantBorder -- ^ The style of the descendants' borders.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



-- | Type 'Overlay.IsolatedElementHighlightConfig' .
data OverlayIsolatedElementHighlightConfig = OverlayIsolatedElementHighlightConfig {
   overlayIsolatedElementHighlightConfigIsolationModeHighlightConfig :: OverlayIsolatedElementHighlightConfigIsolationModeHighlightConfig, -- ^ A descriptor for the highlight appearance of an element in isolation mode.
   overlayIsolatedElementHighlightConfigNodeId :: OverlayIsolatedElementHighlightConfigNodeId -- ^ Identifier of the isolated element to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolatedElementHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolatedElementHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



-- | Type 'Overlay.IsolationModeHighlightConfig' .
data OverlayIsolationModeHighlightConfig = OverlayIsolationModeHighlightConfig {
   overlayIsolationModeHighlightConfigResizerColor :: OverlayIsolationModeHighlightConfigResizerColor, -- ^ The fill color of the resizers (default: transparent).
   overlayIsolationModeHighlightConfigResizerHandleColor :: OverlayIsolationModeHighlightConfigResizerHandleColor, -- ^ The fill color for resizer handles (default: transparent).
   overlayIsolationModeHighlightConfigMaskColor :: OverlayIsolationModeHighlightConfigMaskColor -- ^ The fill color for the mask covering non-isolated elements (default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolationModeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolationModeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Overlay.InspectMode' .
data OverlayInspectMode = OverlayInspectModeSearchForNode | OverlayInspectModeSearchForUaShadowDom | OverlayInspectModeCaptureAreaScreenshot | OverlayInspectModeShowDistances | OverlayInspectModeNone
   deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayInspectMode where
   parseJSON = A.withText  "OverlayInspectMode"  $ \v -> do
      case v of
         "searchForNode" -> pure OverlayInspectModeSearchForNode
         "searchForUAShadowDOM" -> pure OverlayInspectModeSearchForUaShadowDom
         "captureAreaScreenshot" -> pure OverlayInspectModeCaptureAreaScreenshot
         "showDistances" -> pure OverlayInspectModeShowDistances
         "none" -> pure OverlayInspectModeNone
         _ -> fail "failed to parse OverlayInspectMode"

instance ToJSON OverlayInspectMode where
   toJSON v = A.String $
      case v of
         OverlayInspectModeSearchForNode -> "searchForNode"
         OverlayInspectModeSearchForUaShadowDom -> "searchForUAShadowDOM"
         OverlayInspectModeCaptureAreaScreenshot -> "captureAreaScreenshot"
         OverlayInspectModeShowDistances -> "showDistances"
         OverlayInspectModeNone -> "none"





-- | Type of the 'Overlay.inspectNodeRequested' event.
data OverlayInspectNodeRequested = OverlayInspectNodeRequested {
   overlayInspectNodeRequestedBackendNodeId :: OverlayInspectNodeRequestedBackendNodeId -- ^ Id of the node to inspect.
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayInspectNodeRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  OverlayInspectNodeRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Overlay.nodeHighlightRequested' event.
data OverlayNodeHighlightRequested = OverlayNodeHighlightRequested {
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayNodeHighlightRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  OverlayNodeHighlightRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Overlay.screenshotRequested' event.
data OverlayScreenshotRequested = OverlayScreenshotRequested {
   overlayScreenshotRequestedViewport :: OverlayScreenshotRequestedViewport -- ^ Viewport to capture, in device independent pixels (dip).
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScreenshotRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayScreenshotRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type of the 'Overlay.inspectModeCanceled' event.
data OverlayInspectModeCanceled = OverlayInspectModeCanceled
   deriving (Eq, Show, Read)
instance FromJSON OverlayInspectModeCanceled where
   parseJSON = A.withText  "OverlayInspectModeCanceled"  $ \v -> do
      case v of
         "OverlayInspectModeCanceled" -> pure OverlayInspectModeCanceled
         _ -> fail "failed to parse OverlayInspectModeCanceled"





-- | Function for the command 'Overlay.disable'.
-- Disables domain notifications.
overlayDisable :: Handle ev -> IO (Maybe Error)
overlayDisable handle = sendReceiveCommand handle "Overlay.disable" (Nothing :: Maybe ())


-- | Function for the command 'Overlay.enable'.
-- Enables domain notifications.
overlayEnable :: Handle ev -> IO (Maybe Error)
overlayEnable handle = sendReceiveCommand handle "Overlay.enable" (Nothing :: Maybe ())


-- | Parameters of the 'overlayGetHighlightObjectForTest' command.
data POverlayGetHighlightObjectForTest = POverlayGetHighlightObjectForTest {
   pOverlayGetHighlightObjectForTestNodeId :: POverlayGetHighlightObjectForTestNodeId, -- ^ Id of the node to get highlight object for.
   pOverlayGetHighlightObjectForTestIncludeDistance :: POverlayGetHighlightObjectForTestIncludeDistance, -- ^ Whether to include distance info.
   pOverlayGetHighlightObjectForTestIncludeStyle :: POverlayGetHighlightObjectForTestIncludeStyle, -- ^ Whether to include style info.
   pOverlayGetHighlightObjectForTestColorFormat :: POverlayGetHighlightObjectForTestColorFormat, -- ^ The color format to get config with (default: hex).
   pOverlayGetHighlightObjectForTestShowAccessibilityInfo :: POverlayGetHighlightObjectForTestShowAccessibilityInfo -- ^ Whether to show accessibility info (default: true).
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the command 'Overlay.getHighlightObjectForTest'.
-- For testing.
-- Parameters: 'POverlayGetHighlightObjectForTest'
-- Returns: 'OverlayGetHighlightObjectForTest'
overlayGetHighlightObjectForTest :: Handle ev -> POverlayGetHighlightObjectForTest -> IO (Either Error OverlayGetHighlightObjectForTest)
overlayGetHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getHighlightObjectForTest" (Just params)

-- | Return type of the 'overlayGetHighlightObjectForTest' command.
data OverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest {
   overlayGetHighlightObjectForTestHighlight :: [(String, String)] -- ^ Highlight data for the node.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance Command OverlayGetHighlightObjectForTest where
   commandName _ = "Overlay.getHighlightObjectForTest"



-- | Parameters of the 'overlayGetGridHighlightObjectsForTest' command.
data POverlayGetGridHighlightObjectsForTest = POverlayGetGridHighlightObjectsForTest {
   pOverlayGetGridHighlightObjectsForTestNodeIds :: POverlayGetGridHighlightObjectsForTestNodeIds -- ^ Ids of the node to get highlight object for.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetGridHighlightObjectsForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  POverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the command 'Overlay.getGridHighlightObjectsForTest'.
-- For Persistent Grid testing.
-- Parameters: 'POverlayGetGridHighlightObjectsForTest'
-- Returns: 'OverlayGetGridHighlightObjectsForTest'
overlayGetGridHighlightObjectsForTest :: Handle ev -> POverlayGetGridHighlightObjectsForTest -> IO (Either Error OverlayGetGridHighlightObjectsForTest)
overlayGetGridHighlightObjectsForTest handle params = sendReceiveCommandResult handle "Overlay.getGridHighlightObjectsForTest" (Just params)

-- | Return type of the 'overlayGetGridHighlightObjectsForTest' command.
data OverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest {
   overlayGetGridHighlightObjectsForTestHighlights :: [(String, String)] -- ^ Grid Highlight data for the node ids provided.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command OverlayGetGridHighlightObjectsForTest where
   commandName _ = "Overlay.getGridHighlightObjectsForTest"



-- | Parameters of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data POverlayGetSourceOrderHighlightObjectForTest = POverlayGetSourceOrderHighlightObjectForTest {
   pOverlayGetSourceOrderHighlightObjectForTestNodeId :: POverlayGetSourceOrderHighlightObjectForTestNodeId -- ^ Id of the node to highlight.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetSourceOrderHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  POverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }


-- | Function for the command 'Overlay.getSourceOrderHighlightObjectForTest'.
-- For Source Order Viewer testing.
-- Parameters: 'POverlayGetSourceOrderHighlightObjectForTest'
-- Returns: 'OverlayGetSourceOrderHighlightObjectForTest'
overlayGetSourceOrderHighlightObjectForTest :: Handle ev -> POverlayGetSourceOrderHighlightObjectForTest -> IO (Either Error OverlayGetSourceOrderHighlightObjectForTest)
overlayGetSourceOrderHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getSourceOrderHighlightObjectForTest" (Just params)

-- | Return type of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data OverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest {
   overlayGetSourceOrderHighlightObjectForTestHighlight :: [(String, String)] -- ^ Source order highlight data for the node id provided.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }

instance Command OverlayGetSourceOrderHighlightObjectForTest where
   commandName _ = "Overlay.getSourceOrderHighlightObjectForTest"



-- | Function for the command 'Overlay.hideHighlight'.
-- Hides any highlight.
overlayHideHighlight :: Handle ev -> IO (Maybe Error)
overlayHideHighlight handle = sendReceiveCommand handle "Overlay.hideHighlight" (Nothing :: Maybe ())


-- | Parameters of the 'overlayHighlightNode' command.
data POverlayHighlightNode = POverlayHighlightNode {
   pOverlayHighlightNodeHighlightConfig :: POverlayHighlightNodeHighlightConfig, -- ^ A descriptor for the highlight appearance.
   pOverlayHighlightNodeNodeId :: POverlayHighlightNodeNodeId, -- ^ Identifier of the node to highlight.
   pOverlayHighlightNodeBackendNodeId :: POverlayHighlightNodeBackendNodeId, -- ^ Identifier of the backend node to highlight.
   pOverlayHighlightNodeObjectId :: POverlayHighlightNodeObjectId, -- ^ JavaScript object id of the node to be highlighted.
   pOverlayHighlightNodeSelector :: POverlayHighlightNodeSelector -- ^ Selectors to highlight relevant nodes.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Overlay.highlightNode'.
-- Highlights DOM node with given id or with the given JavaScript object wrapper. Either nodeId or
-- objectId must be specified.
-- Parameters: 'POverlayHighlightNode'
overlayHighlightNode :: Handle ev -> POverlayHighlightNode -> IO (Maybe Error)
overlayHighlightNode handle params = sendReceiveCommand handle "Overlay.highlightNode" (Just params)


-- | Parameters of the 'overlayHighlightQuad' command.
data POverlayHighlightQuad = POverlayHighlightQuad {
   pOverlayHighlightQuadQuad :: POverlayHighlightQuadQuad, -- ^ Quad to highlight
   pOverlayHighlightQuadColor :: POverlayHighlightQuadColor, -- ^ The highlight fill color (default: transparent).
   pOverlayHighlightQuadOutlineColor :: POverlayHighlightQuadOutlineColor -- ^ The highlight outline color (default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightQuad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightQuad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Overlay.highlightQuad'.
-- Highlights given quad. Coordinates are absolute with respect to the main frame viewport.
-- Parameters: 'POverlayHighlightQuad'
overlayHighlightQuad :: Handle ev -> POverlayHighlightQuad -> IO (Maybe Error)
overlayHighlightQuad handle params = sendReceiveCommand handle "Overlay.highlightQuad" (Just params)


-- | Parameters of the 'overlayHighlightRect' command.
data POverlayHighlightRect = POverlayHighlightRect {
   pOverlayHighlightRectX :: POverlayHighlightRectX, -- ^ X coordinate
   pOverlayHighlightRectY :: POverlayHighlightRectY, -- ^ Y coordinate
   pOverlayHighlightRectWidth :: POverlayHighlightRectWidth, -- ^ Rectangle width
   pOverlayHighlightRectHeight :: POverlayHighlightRectHeight, -- ^ Rectangle height
   pOverlayHighlightRectColor :: POverlayHighlightRectColor, -- ^ The highlight fill color (default: transparent).
   pOverlayHighlightRectOutlineColor :: POverlayHighlightRectOutlineColor -- ^ The highlight outline color (default: transparent).
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Overlay.highlightRect'.
-- Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.
-- Parameters: 'POverlayHighlightRect'
overlayHighlightRect :: Handle ev -> POverlayHighlightRect -> IO (Maybe Error)
overlayHighlightRect handle params = sendReceiveCommand handle "Overlay.highlightRect" (Just params)


-- | Parameters of the 'overlayHighlightSourceOrder' command.
data POverlayHighlightSourceOrder = POverlayHighlightSourceOrder {
   pOverlayHighlightSourceOrderSourceOrderConfig :: POverlayHighlightSourceOrderSourceOrderConfig, -- ^ A descriptor for the appearance of the overlay drawing.
   pOverlayHighlightSourceOrderNodeId :: POverlayHighlightSourceOrderNodeId, -- ^ Identifier of the node to highlight.
   pOverlayHighlightSourceOrderBackendNodeId :: POverlayHighlightSourceOrderBackendNodeId, -- ^ Identifier of the backend node to highlight.
   pOverlayHighlightSourceOrderObjectId :: POverlayHighlightSourceOrderObjectId -- ^ JavaScript object id of the node to be highlighted.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightSourceOrder  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightSourceOrder where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Overlay.highlightSourceOrder'.
-- Highlights the source order of the children of the DOM node with given id or with the given
-- JavaScript object wrapper. Either nodeId or objectId must be specified.
-- Parameters: 'POverlayHighlightSourceOrder'
overlayHighlightSourceOrder :: Handle ev -> POverlayHighlightSourceOrder -> IO (Maybe Error)
overlayHighlightSourceOrder handle params = sendReceiveCommand handle "Overlay.highlightSourceOrder" (Just params)


-- | Parameters of the 'overlaySetInspectMode' command.
data POverlaySetInspectMode = POverlaySetInspectMode {
   pOverlaySetInspectModeMode :: POverlaySetInspectModeMode, -- ^ Set an inspection mode.
   pOverlaySetInspectModeHighlightConfig :: POverlaySetInspectModeHighlightConfig -- ^ A descriptor for the highlight appearance of hovered-over nodes. May be omitted if `enabled
== false`.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetInspectMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  POverlaySetInspectMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Overlay.setInspectMode'.
-- Enters the 'inspect' mode. In this mode, elements that user is hovering over are highlighted.
-- Backend then generates 'inspectNodeRequested' event upon element selection.
-- Parameters: 'POverlaySetInspectMode'
overlaySetInspectMode :: Handle ev -> POverlaySetInspectMode -> IO (Maybe Error)
overlaySetInspectMode handle params = sendReceiveCommand handle "Overlay.setInspectMode" (Just params)


-- | Parameters of the 'overlaySetShowAdHighlights' command.
data POverlaySetShowAdHighlights = POverlaySetShowAdHighlights {
   pOverlaySetShowAdHighlightsShow :: POverlaySetShowAdHighlightsShow -- ^ True for showing ad highlights
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowAdHighlights  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowAdHighlights where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Overlay.setShowAdHighlights'.
-- Highlights owner element of all frames detected to be ads.
-- Parameters: 'POverlaySetShowAdHighlights'
overlaySetShowAdHighlights :: Handle ev -> POverlaySetShowAdHighlights -> IO (Maybe Error)
overlaySetShowAdHighlights handle params = sendReceiveCommand handle "Overlay.setShowAdHighlights" (Just params)


-- | Parameters of the 'overlaySetPausedInDebuggerMessage' command.
data POverlaySetPausedInDebuggerMessage = POverlaySetPausedInDebuggerMessage {
   pOverlaySetPausedInDebuggerMessageMessage :: POverlaySetPausedInDebuggerMessageMessage -- ^ The message to display, also triggers resume and step over controls.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetPausedInDebuggerMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  POverlaySetPausedInDebuggerMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the command 'Overlay.setPausedInDebuggerMessage'.
-- Parameters: 'POverlaySetPausedInDebuggerMessage'
overlaySetPausedInDebuggerMessage :: Handle ev -> POverlaySetPausedInDebuggerMessage -> IO (Maybe Error)
overlaySetPausedInDebuggerMessage handle params = sendReceiveCommand handle "Overlay.setPausedInDebuggerMessage" (Just params)


-- | Parameters of the 'overlaySetShowDebugBorders' command.
data POverlaySetShowDebugBorders = POverlaySetShowDebugBorders {
   pOverlaySetShowDebugBordersShow :: POverlaySetShowDebugBordersShow -- ^ True for showing debug borders
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowDebugBorders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowDebugBorders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Overlay.setShowDebugBorders'.
-- Requests that backend shows debug borders on layers
-- Parameters: 'POverlaySetShowDebugBorders'
overlaySetShowDebugBorders :: Handle ev -> POverlaySetShowDebugBorders -> IO (Maybe Error)
overlaySetShowDebugBorders handle params = sendReceiveCommand handle "Overlay.setShowDebugBorders" (Just params)


-- | Parameters of the 'overlaySetShowFpsCounter' command.
data POverlaySetShowFpsCounter = POverlaySetShowFpsCounter {
   pOverlaySetShowFpsCounterShow :: POverlaySetShowFpsCounterShow -- ^ True for showing the FPS counter
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFpsCounter  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFpsCounter where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Overlay.setShowFPSCounter'.
-- Requests that backend shows the FPS counter
-- Parameters: 'POverlaySetShowFpsCounter'
overlaySetShowFpsCounter :: Handle ev -> POverlaySetShowFpsCounter -> IO (Maybe Error)
overlaySetShowFpsCounter handle params = sendReceiveCommand handle "Overlay.setShowFPSCounter" (Just params)


-- | Parameters of the 'overlaySetShowGridOverlays' command.
data POverlaySetShowGridOverlays = POverlaySetShowGridOverlays {
   pOverlaySetShowGridOverlaysGridNodeHighlightConfigs :: POverlaySetShowGridOverlaysGridNodeHighlightConfigs -- ^ An array of node identifiers and descriptors for the highlight appearance.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowGridOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowGridOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Overlay.setShowGridOverlays'.
-- Highlight multiple elements with the CSS Grid overlay.
-- Parameters: 'POverlaySetShowGridOverlays'
overlaySetShowGridOverlays :: Handle ev -> POverlaySetShowGridOverlays -> IO (Maybe Error)
overlaySetShowGridOverlays handle params = sendReceiveCommand handle "Overlay.setShowGridOverlays" (Just params)


-- | Parameters of the 'overlaySetShowFlexOverlays' command.
data POverlaySetShowFlexOverlays = POverlaySetShowFlexOverlays {
   pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs :: POverlaySetShowFlexOverlaysFlexNodeHighlightConfigs -- ^ An array of node identifiers and descriptors for the highlight appearance.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFlexOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFlexOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Overlay.setShowFlexOverlays'.
-- Parameters: 'POverlaySetShowFlexOverlays'
overlaySetShowFlexOverlays :: Handle ev -> POverlaySetShowFlexOverlays -> IO (Maybe Error)
overlaySetShowFlexOverlays handle params = sendReceiveCommand handle "Overlay.setShowFlexOverlays" (Just params)


-- | Parameters of the 'overlaySetShowScrollSnapOverlays' command.
data POverlaySetShowScrollSnapOverlays = POverlaySetShowScrollSnapOverlays {
   pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs :: POverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs -- ^ An array of node identifiers and descriptors for the highlight appearance.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollSnapOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollSnapOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the command 'Overlay.setShowScrollSnapOverlays'.
-- Parameters: 'POverlaySetShowScrollSnapOverlays'
overlaySetShowScrollSnapOverlays :: Handle ev -> POverlaySetShowScrollSnapOverlays -> IO (Maybe Error)
overlaySetShowScrollSnapOverlays handle params = sendReceiveCommand handle "Overlay.setShowScrollSnapOverlays" (Just params)


-- | Parameters of the 'overlaySetShowContainerQueryOverlays' command.
data POverlaySetShowContainerQueryOverlays = POverlaySetShowContainerQueryOverlays {
   pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs :: POverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs -- ^ An array of node identifiers and descriptors for the highlight appearance.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowContainerQueryOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowContainerQueryOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the command 'Overlay.setShowContainerQueryOverlays'.
-- Parameters: 'POverlaySetShowContainerQueryOverlays'
overlaySetShowContainerQueryOverlays :: Handle ev -> POverlaySetShowContainerQueryOverlays -> IO (Maybe Error)
overlaySetShowContainerQueryOverlays handle params = sendReceiveCommand handle "Overlay.setShowContainerQueryOverlays" (Just params)


-- | Parameters of the 'overlaySetShowPaintRects' command.
data POverlaySetShowPaintRects = POverlaySetShowPaintRects {
   pOverlaySetShowPaintRectsResult :: POverlaySetShowPaintRectsResult -- ^ True for showing paint rectangles
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowPaintRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowPaintRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Overlay.setShowPaintRects'.
-- Requests that backend shows paint rectangles
-- Parameters: 'POverlaySetShowPaintRects'
overlaySetShowPaintRects :: Handle ev -> POverlaySetShowPaintRects -> IO (Maybe Error)
overlaySetShowPaintRects handle params = sendReceiveCommand handle "Overlay.setShowPaintRects" (Just params)


-- | Parameters of the 'overlaySetShowLayoutShiftRegions' command.
data POverlaySetShowLayoutShiftRegions = POverlaySetShowLayoutShiftRegions {
   pOverlaySetShowLayoutShiftRegionsResult :: POverlaySetShowLayoutShiftRegionsResult -- ^ True for showing layout shift regions
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowLayoutShiftRegions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowLayoutShiftRegions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the command 'Overlay.setShowLayoutShiftRegions'.
-- Requests that backend shows layout shift regions
-- Parameters: 'POverlaySetShowLayoutShiftRegions'
overlaySetShowLayoutShiftRegions :: Handle ev -> POverlaySetShowLayoutShiftRegions -> IO (Maybe Error)
overlaySetShowLayoutShiftRegions handle params = sendReceiveCommand handle "Overlay.setShowLayoutShiftRegions" (Just params)


-- | Parameters of the 'overlaySetShowScrollBottleneckRects' command.
data POverlaySetShowScrollBottleneckRects = POverlaySetShowScrollBottleneckRects {
   pOverlaySetShowScrollBottleneckRectsShow :: POverlaySetShowScrollBottleneckRectsShow -- ^ True for showing scroll bottleneck rects
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollBottleneckRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollBottleneckRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the command 'Overlay.setShowScrollBottleneckRects'.
-- Requests that backend shows scroll bottleneck rects
-- Parameters: 'POverlaySetShowScrollBottleneckRects'
overlaySetShowScrollBottleneckRects :: Handle ev -> POverlaySetShowScrollBottleneckRects -> IO (Maybe Error)
overlaySetShowScrollBottleneckRects handle params = sendReceiveCommand handle "Overlay.setShowScrollBottleneckRects" (Just params)


-- | Parameters of the 'overlaySetShowWebVitals' command.
data POverlaySetShowWebVitals = POverlaySetShowWebVitals {
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowWebVitals  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowWebVitals where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Overlay.setShowWebVitals'.
-- Request that backend shows an overlay with web vital metrics.
-- Parameters: 'POverlaySetShowWebVitals'
overlaySetShowWebVitals :: Handle ev -> POverlaySetShowWebVitals -> IO (Maybe Error)
overlaySetShowWebVitals handle params = sendReceiveCommand handle "Overlay.setShowWebVitals" (Just params)


-- | Parameters of the 'overlaySetShowViewportSizeOnResize' command.
data POverlaySetShowViewportSizeOnResize = POverlaySetShowViewportSizeOnResize {
   pOverlaySetShowViewportSizeOnResizeShow :: POverlaySetShowViewportSizeOnResizeShow -- ^ Whether to paint size or not.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowViewportSizeOnResize  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowViewportSizeOnResize where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'Overlay.setShowViewportSizeOnResize'.
-- Paints viewport size upon main frame resize.
-- Parameters: 'POverlaySetShowViewportSizeOnResize'
overlaySetShowViewportSizeOnResize :: Handle ev -> POverlaySetShowViewportSizeOnResize -> IO (Maybe Error)
overlaySetShowViewportSizeOnResize handle params = sendReceiveCommand handle "Overlay.setShowViewportSizeOnResize" (Just params)


-- | Parameters of the 'overlaySetShowHinge' command.
data POverlaySetShowHinge = POverlaySetShowHinge {
   pOverlaySetShowHingeHingeConfig :: POverlaySetShowHingeHingeConfig -- ^ hinge data, null means hideHinge
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowHinge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowHinge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Overlay.setShowHinge'.
-- Add a dual screen device hinge
-- Parameters: 'POverlaySetShowHinge'
overlaySetShowHinge :: Handle ev -> POverlaySetShowHinge -> IO (Maybe Error)
overlaySetShowHinge handle params = sendReceiveCommand handle "Overlay.setShowHinge" (Just params)


-- | Parameters of the 'overlaySetShowIsolatedElements' command.
data POverlaySetShowIsolatedElements = POverlaySetShowIsolatedElements {
   pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs :: POverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs -- ^ An array of node identifiers and descriptors for the highlight appearance.
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowIsolatedElements  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowIsolatedElements where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the command 'Overlay.setShowIsolatedElements'.
-- Show elements in isolation mode with overlays.
-- Parameters: 'POverlaySetShowIsolatedElements'
overlaySetShowIsolatedElements :: Handle ev -> POverlaySetShowIsolatedElements -> IO (Maybe Error)
overlaySetShowIsolatedElements handle params = sendReceiveCommand handle "Overlay.setShowIsolatedElements" (Just params)



